//! `runway` GC trigger.
//!
//! Port of the ZGC director's allocation-rate rule. Allocation volume is
//! sampled on a fixed period, and a collection starts when the free space
//! would be exhausted, at a 1-in-1000 rate spike (mean + 3.29 sigma, scaled by
//! a spike tolerance), before a GC of the slowest plausible duration (mean +
//! 3.29 sigma of recent cycles) plus one sample period could finish.
//!
//! The first cycles only exist to learn how long a GC takes, so they fire at
//! 10 / 20 / 30 percent of the heap allocated since the previous GC. Allocation since
//! the last GC is used instead so a large boot heap does not cause back-to-back warmup collections.

use std::{collections::VecDeque, time::Duration, time::Instant};

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState, percent_of};

pub(super) const MAX_SIGMA: f64 = 3.291;
pub(super) const DEFAULT_SPIKE_TOLERANCE: f64 = 2.0;
pub(super) const DEFAULT_SAMPLE_PERIOD: Duration = Duration::from_millis(100);
const SAMPLE_WINDOW: usize = 10;
const MIN_SAMPLES: usize = 3;

/// Allocation-rate sampler for the `runway` rule.
#[derive(Clone, Debug)]
pub(super) struct RunwayState {
    samples: VecDeque<f64>,
    last: Option<(Instant, usize)>,
}

impl RunwayState {
    pub(super) fn new() -> Self {
        Self {
            samples: VecDeque::with_capacity(SAMPLE_WINDOW),
            last: None,
        }
    }

    pub(super) fn reset_baseline(&mut self) {
        self.last = None;
    }

    /// Adds a rate sample when `period` has elapsed since the previous one.
    pub(super) fn observe(&mut self, now: Instant, allocated_since_gc: usize, period: Duration) {
        let Some((last_time, last_allocated)) = self.last else {
            self.last = Some((now, allocated_since_gc));
            return;
        };
        let elapsed = now.saturating_duration_since(last_time);
        if elapsed < period || elapsed.is_zero() {
            return;
        }
        let allocated = allocated_since_gc.saturating_sub(last_allocated);
        if self.samples.len() == SAMPLE_WINDOW {
            self.samples.pop_front();
        }
        self.samples
            .push_back(allocated as f64 / elapsed.as_secs_f64());
        self.last = Some((now, allocated_since_gc));
    }

    /// Mean and population standard deviation of the sampled rates.
    pub(super) fn mean_and_sd(&self) -> Option<(f64, f64)> {
        if self.samples.len() < MIN_SAMPLES {
            return None;
        }
        let n = self.samples.len() as f64;
        let mean = self.samples.iter().sum::<f64>() / n;
        let variance = self.samples.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / n;
        Some((mean, variance.sqrt()))
    }
}

/// Percent of the heap that must be allocated to start warmup cycle `learned`.
pub(super) fn warmup_threshold_percent(learned: usize) -> Option<usize> {
    (learned < 3).then(|| 10 * (learned + 1))
}

/// Whether the free space runs out before a worst-case GC can finish.
pub(super) fn exhaustion_before_gc_completes(
    free_bytes: usize,
    mean_rate: f64,
    rate_sd: f64,
    spike_tolerance: f64,
    max_gc_seconds: f64,
    sample_period: Duration,
) -> bool {
    let max_rate = mean_rate * spike_tolerance + MAX_SIGMA * rate_sd;
    if !max_rate.is_finite() || max_rate <= 0.0 {
        return false;
    }
    let time_until_exhaustion = free_bytes as f64 / max_rate;
    time_until_exhaustion - max_gc_seconds - sample_period.as_secs_f64() <= 0.0
}

impl HeuristicState {
    pub(super) fn runway_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }
        self.runway.observe(
            now,
            snapshot.allocated_since_gc,
            self.config.runway_sample_period,
        );

        if let Some(percent) = warmup_threshold_percent(self.gc_times_learned) {
            let threshold = percent_of(snapshot.total_bytes, percent);
            if snapshot.allocated_since_gc >= threshold {
                let cycle = self.gc_times_learned;
                self.accept_trigger(
                    format_args!(
                        "warmup threshold crossed (cycle={}, allocated={}, threshold={})",
                        cycle,
                        FormattedSize(snapshot.allocated_since_gc),
                        FormattedSize(threshold),
                    ),
                    Some(snapshot),
                );
                return true;
            }
            return self.base_should_start(snapshot, now);
        }

        if let Some((mean, sd)) = self.runway.mean_and_sd() {
            let penalty = 1.0 + self.gc_time_penalties as f64 / 100.0;
            let max_gc = self.gc_cycle_time_history.upper_bound(MAX_SIGMA) * penalty;
            let free = snapshot.effective_free_bytes();
            if exhaustion_before_gc_completes(
                free,
                mean,
                sd,
                self.config.runway_spike_tolerance,
                max_gc,
                self.config.runway_sample_period,
            ) {
                self.accept_trigger(
                    format_args!(
                        "free space runs out before a worst-case GC finishes (free={}, mean_rate={}/s, rate_sd={}/s, max_gc={:.2}ms)",
                        FormattedSize(free),
                        FormattedSize(super::rate_to_size(mean)),
                        FormattedSize(super::rate_to_size(sd)),
                        max_gc * 1000.0,
                    ),
                    Some(snapshot),
                );
                return true;
            }
        }

        self.base_should_start(snapshot, now)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MIB: f64 = 1024.0 * 1024.0;

    #[test]
    fn warmup_thresholds_step_up_then_stop() {
        assert_eq!(warmup_threshold_percent(0), Some(10));
        assert_eq!(warmup_threshold_percent(1), Some(20));
        assert_eq!(warmup_threshold_percent(2), Some(30));
        assert_eq!(warmup_threshold_percent(3), None);
    }

    #[test]
    fn steady_rate_with_ample_free_does_not_trigger() {
        // 10 MiB/s, 1 GiB free, 50 ms GC: ~50 s of runway.
        assert!(!exhaustion_before_gc_completes(
            1024 * 1024 * 1024,
            10.0 * MIB,
            0.0,
            2.0,
            0.05,
            DEFAULT_SAMPLE_PERIOD,
        ));
    }

    #[test]
    fn spike_margin_triggers_earlier_than_the_mean_would() {
        // A 0.2 s GC plus one 0.1 s sample period needs more than 0.3 s of runway.
        let free = (40.0 * MIB) as usize;
        // Mean alone: 100 MiB/s leaves 0.4 s.
        assert!(!exhaustion_before_gc_completes(
            free,
            100.0 * MIB,
            0.0,
            1.0,
            0.2,
            DEFAULT_SAMPLE_PERIOD
        ));
        // A 2x spike tolerance halves that to 0.2 s.
        assert!(exhaustion_before_gc_completes(
            free,
            100.0 * MIB,
            0.0,
            2.0,
            0.2,
            DEFAULT_SAMPLE_PERIOD
        ));
        // Variance alone can push it over: 100 + 3.291 * 20 = 165.8 MiB/s -> 0.24 s.
        assert!(exhaustion_before_gc_completes(
            free,
            100.0 * MIB,
            20.0 * MIB,
            1.0,
            0.2,
            DEFAULT_SAMPLE_PERIOD
        ));
    }

    #[test]
    fn idle_allocator_never_triggers() {
        assert!(!exhaustion_before_gc_completes(
            1,
            0.0,
            0.0,
            2.0,
            10.0,
            DEFAULT_SAMPLE_PERIOD
        ));
    }

    #[test]
    fn sampler_needs_a_baseline_and_a_full_period() {
        let t0 = Instant::now();
        let period = Duration::from_millis(100);
        let mut state = RunwayState::new();
        state.observe(t0, 0, period);
        state.observe(t0 + Duration::from_millis(50), 1000, period);
        assert!(state.mean_and_sd().is_none());
        for i in 1..=3u64 {
            state.observe(t0 + period * i as u32, 1000 * i as usize, period);
        }
        let (mean, sd) = state.mean_and_sd().expect("three samples");
        assert!((mean - 10_000.0).abs() < 1.0);
        assert!(sd < 1.0);
    }

    #[test]
    fn sampler_survives_allocation_counter_reset() {
        let t0 = Instant::now();
        let period = Duration::from_millis(10);
        let mut state = RunwayState::new();
        state.observe(t0, 5000, period);
        // A GC reset allocated_since_gc below the old baseline.
        state.observe(t0 + period, 100, period);
        assert_eq!(state.samples.back().copied(), Some(0.0));
    }
}
