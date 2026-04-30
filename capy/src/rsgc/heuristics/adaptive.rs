//! Adaptive GC trigger heuristics.
//!
//! This mode follows the same broad shape as Shenandoah's adaptive trigger:
//! start with a learning phase, wait until enough allocation has happened to
//! make the next collection useful, then predict whether allocation will
//! consume the remaining headroom before a GC can complete.  It uses three
//! signals: average allocation pressure, short-term allocation spikes, and
//! accelerating allocation rates.

use std::{collections::VecDeque, time::Duration};

use crate::utils::FormattedSize;

use super::{
    HeapSnapshot, HeuristicState, elapsed_seconds, format_allocation_pressure_reason, percent_of,
    rate_to_size,
};

pub(super) const DEFAULT_CONFIDENCE: f64 = 1.8;
pub(super) const DEFAULT_SPIKE_THRESHOLD: f64 = 1.8;
pub(super) const DEFAULT_ACCELERATION_SAMPLE_PERIOD: Duration = Duration::from_millis(15);
pub(super) const DEFAULT_ACCELERATION_SAMPLE_COUNT: usize = 8;
pub(super) const DEFAULT_MOMENTARY_SPIKE_SAMPLE_COUNT: usize = 3;
pub(super) const MINIMUM_CONFIDENCE: f64 = 0.319;
pub(super) const MAXIMUM_CONFIDENCE: f64 = 3.291;
const DEGENERATE_PENALTY_SD: f64 = 0.1;
const FULL_PENALTY_SD: f64 = 0.2;
const DEGENERATE_PENALTY_PERCENT: isize = 10;
const FULL_PENALTY_PERCENT: isize = 20;
const LOWEST_EXPECTED_FREE_AT_END: f64 = -0.5;
const HIGHEST_EXPECTED_FREE_AT_END: f64 = 0.5;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum TriggerType {
    /// A non-adaptive trigger, such as the learning threshold or minimum free threshold.
    Other,
    /// A trigger caused by average-rate or accelerating-rate allocation pressure.
    Rate,
    /// A trigger caused by short-term allocation above the normal rate distribution.
    Spike,
}

#[derive(Clone, Debug)]
pub(super) struct LinearPredictor {
    samples: VecDeque<(f64, f64)>,
    max_samples: usize,
}

impl LinearPredictor {
    pub(super) fn new(max_samples: usize) -> Self {
        Self {
            samples: VecDeque::with_capacity(max_samples.max(1)),
            max_samples: max_samples.max(1),
        }
    }

    pub(super) fn add(&mut self, timestamp: f64, value: f64) {
        if self.samples.len() == self.max_samples {
            self.samples.pop_front();
        }
        self.samples.push_back((timestamp, value));
    }

    fn predict(&self, timestamp: f64, confidence: f64) -> f64 {
        match self.samples.len() {
            0 => 0.0,
            1 => self.samples[0].1,
            count => {
                // Fit a line through recent cycle times.  If GC time is trending
                // upward, this predicts the future cycle more conservatively than
                // a plain decaying average.
                let n = count as f64;
                let sum_x: f64 = self.samples.iter().map(|(x, _)| *x).sum();
                let sum_y: f64 = self.samples.iter().map(|(_, y)| *y).sum();
                let sum_xx: f64 = self.samples.iter().map(|(x, _)| x * x).sum();
                let sum_xy: f64 = self.samples.iter().map(|(x, y)| x * y).sum();
                let denominator = n * sum_xx - sum_x * sum_x;
                let (slope, intercept) = if denominator == 0.0 {
                    (0.0, sum_y / n)
                } else {
                    let slope = (n * sum_xy - sum_x * sum_y) / denominator;
                    let intercept = (sum_y - slope * sum_x) / n;
                    (slope, intercept)
                };
                let variance = self
                    .samples
                    .iter()
                    .map(|(x, y)| {
                        let predicted = slope * x + intercept;
                        let deviation = predicted - y;
                        deviation * deviation
                    })
                    .sum::<f64>()
                    / n;
                // The confidence term mirrors Shenandoah's use of standard
                // deviations as a configurable safety margin.
                (slope * timestamp + intercept + confidence * variance.sqrt()).max(0.0)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct RateSampleHistory {
    samples: VecDeque<(std::time::Instant, f64)>,
    max_samples: usize,
}

impl RateSampleHistory {
    pub(super) fn new(max_samples: usize) -> Self {
        Self {
            samples: VecDeque::with_capacity(max_samples.max(1)),
            max_samples: max_samples.max(1),
        }
    }

    pub(super) fn add(&mut self, timestamp: std::time::Instant, rate: f64) {
        if self.samples.len() == self.max_samples {
            self.samples.pop_front();
        }
        self.samples.push_back((timestamp, rate.max(0.0)));
    }

    fn accelerated_consumption(&self, elapsed: Duration) -> Option<(usize, f64, f64)> {
        if self.samples.len() < 2 || elapsed.is_zero() {
            return None;
        }

        let first = self.samples.front()?.0;
        let mut monotonic = true;
        let mut previous_rate = None;
        let mut sum_x = 0.0;
        let mut sum_y = 0.0;
        let mut sum_xx = 0.0;
        let mut sum_xy = 0.0;
        for (timestamp, rate) in &self.samples {
            // Treat a falling sample as the end of an acceleration run.  We still
            // use the current rate, but do not extrapolate an upward slope.
            if let Some(previous_rate) = previous_rate
                && *rate < previous_rate
            {
                monotonic = false;
            }
            previous_rate = Some(*rate);

            let x = timestamp.saturating_duration_since(first).as_secs_f64();
            sum_x += x;
            sum_y += *rate;
            sum_xx += x * x;
            sum_xy += x * rate;
        }

        let n = self.samples.len() as f64;
        let denominator = n * sum_xx - sum_x * sum_x;
        let acceleration = if monotonic && denominator > 0.0 {
            ((n * sum_xy - sum_x * sum_y) / denominator).max(0.0)
        } else {
            0.0
        };
        let current_rate = self.samples.back().map(|(_, rate)| *rate).unwrap_or(0.0);
        let seconds = elapsed.as_secs_f64();
        // Constant-acceleration approximation: bytes = rate * t + 1/2 * accel * t^2.
        // This lets a ramping allocator trigger before the average-rate signal catches up.
        let consumption = current_rate * seconds + 0.5 * acceleration * seconds * seconds;
        Some((rate_to_size(consumption), current_rate, acceleration))
    }

    fn momentary_average(&self, sample_count: usize) -> Option<f64> {
        let count = sample_count.min(self.samples.len());
        if count == 0 {
            return None;
        }
        Some(
            self.samples
                .iter()
                .rev()
                .take(count)
                .map(|(_, rate)| *rate)
                .sum::<f64>()
                / count as f64,
        )
    }
}

impl HeuristicState {
    pub(super) fn adaptive_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: std::time::Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        // Absolute low-memory pressure wins over adaptive timing.
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        // Early cycles are deliberately conservative: collect before the heap
        // gets tight so the history has enough cycle-time samples to be useful.
        if self.gc_times_learned < self.config.learning_steps {
            let init_threshold = percent_of(
                snapshot.total_bytes,
                self.config.init_free_threshold_percent,
            );
            if snapshot.effective_free_bytes() < init_threshold {
                let learned_cycles = self.gc_times_learned;
                let learning_steps = self.config.learning_steps;
                self.accept_trigger(
                    format_args!(
                        "learning threshold crossed (learned_cycles={}, learning_steps={}, free={}, threshold={})",
                        learned_cycles,
                        learning_steps,
                        FormattedSize(snapshot.effective_free_bytes()),
                        FormattedSize(init_threshold),
                    ),
                    Some(snapshot),
                );
                return true;
            }
        }

        // Delay adaptive triggers until at least a quarter of the current
        // allocation runway has been consumed.  Triggering too early often finds
        // little reclaimable garbage and can make the next cycle less useful.
        if snapshot.allocated_since_gc.saturating_mul(3) <= snapshot.effective_free_bytes() {
            return self.base_should_start(snapshot, now);
        }

        let planned_sleep = planned_sleep_interval;
        let average_cycle_time = self
            .gc_cycle_time_history
            .upper_bound(self.adaptive_confidence);
        let predicted_cycle_time = self.gc_cycle_time_predictor.predict(
            elapsed_seconds(self.time_origin, now) + planned_sleep.as_secs_f64(),
            self.adaptive_confidence,
        );
        let cycle_time = average_cycle_time.max(predicted_cycle_time) + planned_sleep.as_secs_f64();
        let allocation_rate = self
            .allocation_rate_history
            .upper_bound(self.adaptive_confidence);
        let headroom = self.headroom_adjustment(snapshot.total_bytes);
        // Headroom is reserved for allocation spikes and previous GC-time
        // penalties.  Adaptive triggers compare predicted consumption against
        // what remains after that reserve, not against all free memory.
        let allocatable_free = snapshot.effective_free_bytes().saturating_sub(headroom);

        // Fast path for phase changes: use recent allocation-rate samples to
        // estimate consumption over the next sample period plus the GC time.
        let acceleration_horizon =
            cycle_time + self.config.acceleration_sample_period.as_secs_f64();
        if let Some((accelerated_consumption, current_rate, acceleration)) = self
            .acceleration_rate_history
            .accelerated_consumption(Duration::from_secs_f64(acceleration_horizon))
            && accelerated_consumption > allocatable_free
        {
            self.accept_adaptive_trigger(
                TriggerType::Rate,
                format_args!(
                    "accelerated allocation pressure exceeded threshold (consumption={}, headroom={}, current_rate={}/s, acceleration={}/s/s, planned_gc_time={:.2}ms)",
                    FormattedSize(accelerated_consumption),
                    FormattedSize(allocatable_free),
                    FormattedSize(rate_to_size(current_rate)),
                    FormattedSize(rate_to_size(acceleration)),
                    cycle_time * 1000.0,
                ),
                Some(snapshot),
            );
            return true;
        }

        let expected_consumption = (cycle_time * allocation_rate) as usize;

        // Steady-state trigger: if the upper-bound average allocation rate can
        // deplete usable headroom before GC finishes, start now.
        if expected_consumption > allocatable_free {
            self.accept_adaptive_trigger(
                TriggerType::Rate,
                format_allocation_pressure_reason(
                    allocatable_free,
                    expected_consumption,
                    headroom,
                    allocation_rate,
                ),
                Some(snapshot),
            );
            return true;
        }

        let current_rate = self
            .acceleration_rate_history
            .momentary_average(self.config.momentary_spike_sample_count)
            .unwrap_or(0.0);
        let average_rate = self.allocation_rate_history.average();
        let rate_sd = self.allocation_rate_history.standard_deviation();
        let is_spiking =
            rate_sd > 0.0 && current_rate > average_rate + self.adaptive_spike_threshold * rate_sd;
        let spike_consumption = (cycle_time * current_rate) as usize;
        // Momentary-spike trigger: distinguish bursty allocation from a sustained
        // rate increase so post-GC feedback can tune the spike threshold separately.
        if is_spiking && spike_consumption > allocatable_free {
            let spike_threshold = self.adaptive_spike_threshold;
            self.accept_adaptive_trigger(
                TriggerType::Spike,
                format_args!(
                    "instantaneous allocation spike exceeded threshold (consumption={}, headroom={}, current_rate={}/s, average_rate={}/s, spike_threshold={:.2}, planned_gc_time={:.2}ms)",
                    FormattedSize(spike_consumption),
                    FormattedSize(allocatable_free),
                    FormattedSize(rate_to_size(current_rate)),
                    FormattedSize(rate_to_size(average_rate)),
                    spike_threshold,
                    cycle_time * 1000.0,
                ),
                Some(snapshot),
            );
            return true;
        }

        self.base_should_start(snapshot, now)
    }

    pub(super) fn accept_adaptive_trigger(
        &mut self,
        trigger_type: TriggerType,
        reason: impl std::fmt::Display,
        snapshot: Option<HeapSnapshot>,
    ) {
        // Remember which adaptive signal fired; successful cycles use this to
        // adjust the matching sensitivity rather than all knobs at once.
        self.last_trigger_type = trigger_type;
        self.most_recent_declined_trigger_count = self.declined_trigger_count;
        self.declined_trigger_count = 0;
        self.start_gc_is_pending = true;
        self.log_decision(super::TriggerDecision::Accepted, reason, snapshot);
    }

    pub(super) fn adjust_adaptive_trigger_parameters(&mut self, free_at_end: f64) {
        // Compare post-GC free memory to recent outcomes.  Very low free memory
        // means the trigger was too late; very high free memory means it was too early.
        let sd = self.free_at_end_history.standard_deviation();
        let z_score = if sd > 0.0 {
            (free_at_end - self.free_at_end_history.average()) / sd
        } else {
            0.0
        };
        self.free_at_end_history.add(free_at_end);

        if !(LOWEST_EXPECTED_FREE_AT_END..=HIGHEST_EXPECTED_FREE_AT_END).contains(&z_score) {
            self.adjust_last_trigger_parameters(z_score / -100.0);
        }
    }

    fn adjust_last_trigger_parameters(&mut self, amount: f64) {
        // A negative z-score produces a positive amount, making future triggers
        // more conservative.  A positive z-score relaxes the last trigger type.
        match self.last_trigger_type {
            TriggerType::Rate => self.adjust_adaptive_confidence(amount),
            TriggerType::Spike => self.adjust_spike_threshold(amount),
            TriggerType::Other => {}
        }
    }

    fn adjust_adaptive_confidence(&mut self, amount: f64) {
        self.adaptive_confidence =
            (self.adaptive_confidence + amount).clamp(MINIMUM_CONFIDENCE, MAXIMUM_CONFIDENCE);
    }

    fn adjust_spike_threshold(&mut self, amount: f64) {
        self.adaptive_spike_threshold =
            (self.adaptive_spike_threshold - amount).clamp(MINIMUM_CONFIDENCE, MAXIMUM_CONFIDENCE);
    }

    #[allow(dead_code)]
    fn record_degenerated(&mut self) {
        self.adjust_penalty(DEGENERATE_PENALTY_PERCENT);
        self.adjust_adaptive_confidence(DEGENERATE_PENALTY_SD);
        self.adjust_spike_threshold(DEGENERATE_PENALTY_SD);
    }

    #[allow(dead_code)]
    fn record_success_full(&mut self) {
        self.adjust_penalty(FULL_PENALTY_PERCENT);
        self.adjust_adaptive_confidence(FULL_PENALTY_SD);
        self.adjust_spike_threshold(FULL_PENALTY_SD);
    }
}
