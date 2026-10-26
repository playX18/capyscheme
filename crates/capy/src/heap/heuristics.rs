//! Capy GC trigger policy and shared heuristic state.
//!
//! The MMTk trigger callback enters this module with plan-level heap accounting.
//! Shared code builds a small heap snapshot, records allocation/GC history, and
//! dispatches to the selected heuristic mode.  Mode-specific behavior lives in
//! sibling modules when it has enough logic to stand alone.

use std::{
    fmt,
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
    },
    time::{Duration, Instant},
};

use mmtk::{
    MMTK,
    plan::Plan,
    util::{
        conversions::bytes_to_pages_up,
        heap::{GCTriggerPolicy, SpaceStats},
        options::GCTriggerSelector,
    },
};

use crate::{
    heap::{logging::GC_LOG_TARGET, mm::MemoryManager},
    utils::FormattedSize,
    utils::flags::{self, FlagId},
};

mod adaptive;
mod aggressive;
mod compact;
mod generational;

const DEFAULT_MAX_HEAP_BYTES: usize = 2 * 1024 * 1024 * 1024;
const DEFAULT_MIN_FREE_THRESHOLD_PERCENT: usize = 10;
const DEFAULT_INIT_FREE_THRESHOLD_PERCENT: usize = 50;
const DEFAULT_HEAP_GROWTH_FREE_PERCENT: usize = 50;
const HEAP_GROWTH_MIN_STEP_PERCENT: usize = 25;
const HEAP_SHRINK_AFTER_CYCLES: usize = 3;
const HEADROOM_FLOOR_BYTES: usize = 1024 * 1024;
const DEFAULT_ALLOCATION_THRESHOLD_PERCENT: usize = 0;
const DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT: usize = 5;
const DEFAULT_LEARNING_STEPS: usize = 5;
const DEFAULT_ADAPTIVE_DECAY_FACTOR: f64 = 0.5;
const DEFAULT_GUARANTEED_GC_INTERVAL: Duration = Duration::from_secs(5 * 60);

static DEFAULT_MAX_HEAP_OVERRIDE: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum HeuristicMode {
    /// Shenandoah-style predictor using GC-time and allocation-rate history.
    Adaptive,
    /// Generational Shenandoah-style scheduling: adaptive young triggers with
    /// expedited promotion/mixed work plus idle-gated old triggers.
    Generational,
    /// Minimum-free plus guaranteed-interval trigger only.
    Static,
    /// Small-heap mode with a fixed allocation-since-GC threshold.
    Compact,
    /// Trigger at every opportunity.
    Aggressive,
    /// Never trigger proactively, only MMTk plan fallbacks can require GC.
    Passive,
}

impl HeuristicMode {
    fn from_word(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "adaptive" => Some(Self::Adaptive),
            "generational" => Some(Self::Generational),
            "static" => Some(Self::Static),
            "compact" => Some(Self::Compact),
            "aggressive" => Some(Self::Aggressive),
            "passive" => Some(Self::Passive),
            _ => None,
        }
    }
}

impl fmt::Display for HeuristicMode {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Adaptive => "adaptive",
            Self::Generational => "generational",
            Self::Static => "static",
            Self::Compact => "compact",
            Self::Aggressive => "aggressive",
            Self::Passive => "passive",
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum TriggerDecision {
    Accepted,
    #[allow(dead_code)]
    Suppressed,
}

impl fmt::Display for TriggerDecision {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Accepted => "accepted",
            Self::Suppressed => "suppressed",
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct HeuristicConfig {
    pub(crate) mode: HeuristicMode,
    pub(crate) max_heap_bytes: usize,
    pub(crate) initial_heap_bytes: usize,
    pub(crate) heap_growth_free_percent: usize,
    pub(crate) min_free_threshold_percent: usize,
    pub(crate) init_free_threshold_percent: usize,
    pub(crate) allocation_threshold_percent: usize,
    pub(crate) alloc_spike_factor_percent: usize,
    pub(crate) learning_steps: usize,
    pub(crate) adaptive_decay_factor: f64,
    pub(crate) adaptive_confidence: f64,
    pub(crate) adaptive_spike_threshold: f64,
    pub(crate) acceleration_sample_period: Duration,
    pub(crate) acceleration_sample_count: usize,
    pub(crate) momentary_spike_sample_count: usize,
    pub(crate) guaranteed_gc_interval: Option<Duration>,
    pub(crate) expedite_promotion_threshold_percent: usize,
    pub(crate) expedite_mixed_threshold: usize,
    pub(crate) old_garbage_threshold_percent: usize,
    pub(crate) minimum_old_time: Duration,
    pub(crate) old_growth_allowance_percent: usize,
    pub(crate) old_growth_ignore_percent: usize,
    pub(crate) old_growth_ignore_young_cycles: usize,
}

impl HeuristicConfig {
    fn defaults_for_mode(mode: HeuristicMode) -> Self {
        let mut config = Self {
            mode,
            max_heap_bytes: configured_max_heap_bytes(),
            initial_heap_bytes: usize::MAX,
            heap_growth_free_percent: DEFAULT_HEAP_GROWTH_FREE_PERCENT,
            min_free_threshold_percent: DEFAULT_MIN_FREE_THRESHOLD_PERCENT,
            init_free_threshold_percent: DEFAULT_INIT_FREE_THRESHOLD_PERCENT,
            allocation_threshold_percent: DEFAULT_ALLOCATION_THRESHOLD_PERCENT,
            alloc_spike_factor_percent: DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT,
            learning_steps: DEFAULT_LEARNING_STEPS,
            adaptive_decay_factor: DEFAULT_ADAPTIVE_DECAY_FACTOR,
            adaptive_confidence: adaptive::DEFAULT_CONFIDENCE,
            adaptive_spike_threshold: adaptive::DEFAULT_SPIKE_THRESHOLD,
            acceleration_sample_period: adaptive::DEFAULT_ACCELERATION_SAMPLE_PERIOD,
            acceleration_sample_count: adaptive::DEFAULT_ACCELERATION_SAMPLE_COUNT,
            momentary_spike_sample_count: adaptive::DEFAULT_MOMENTARY_SPIKE_SAMPLE_COUNT,
            guaranteed_gc_interval: Some(DEFAULT_GUARANTEED_GC_INTERVAL),
            expedite_promotion_threshold_percent:
                generational::EXPEDITE_PROMOTION_THRESHOLD_PERCENT,
            expedite_mixed_threshold: generational::EXPEDITE_MIXED_THRESHOLD,
            old_garbage_threshold_percent: generational::OLD_GARBAGE_THRESHOLD_PERCENT,
            minimum_old_time: generational::MINIMUM_OLD_TIME,
            old_growth_allowance_percent: generational::OLD_GROWTH_ALLOWANCE_PERCENT,
            old_growth_ignore_percent: generational::OLD_GROWTH_IGNORE_PERCENT,
            old_growth_ignore_young_cycles: generational::OLD_GROWTH_IGNORE_YOUNG_CYCLES,
        };

        if mode == HeuristicMode::Compact {
            config.allocation_threshold_percent = compact::ALLOCATION_THRESHOLD_PERCENT;
            config.guaranteed_gc_interval = Some(compact::GUARANTEED_GC_INTERVAL);
        }

        config
    }

    pub(crate) fn from_env() -> Self {
        let mode =
            HeuristicMode::from_word(flags::gc_heuristic()).unwrap_or(HeuristicMode::Adaptive);
        let mut config = Self::defaults_for_mode(mode);

        if flags::is_set(FlagId::gc_max_heap) {
            config.max_heap_bytes = flags::gc_max_heap();
        }
        // A zero initial size means "fixed at max"; the policy clamps to max.
        let initial = flags::gc_init_heap();
        config.initial_heap_bytes = if initial == 0 { usize::MAX } else { initial };
        if flags::is_set(FlagId::gc_heap_growth_free_percent) {
            config.heap_growth_free_percent = flags::gc_heap_growth_free_percent();
        }
        if flags::is_set(FlagId::gc_min_free_percent) {
            config.min_free_threshold_percent = flags::gc_min_free_percent();
        }
        if flags::is_set(FlagId::gc_init_free_percent) {
            config.init_free_threshold_percent = flags::gc_init_free_percent();
        }
        if flags::is_set(FlagId::gc_allocation_threshold_percent) {
            config.allocation_threshold_percent = flags::gc_allocation_threshold_percent();
        }
        if flags::is_set(FlagId::gc_alloc_spike_percent) {
            config.alloc_spike_factor_percent = flags::gc_alloc_spike_percent();
        }
        if flags::is_set(FlagId::gc_learning_steps) {
            config.learning_steps = flags::gc_learning_steps();
        }
        if flags::is_set(FlagId::gc_adaptive_confidence) {
            config.adaptive_confidence = flags::gc_adaptive_confidence();
        }
        if flags::is_set(FlagId::gc_adaptive_spike_threshold) {
            config.adaptive_spike_threshold = flags::gc_adaptive_spike_threshold();
        }
        if flags::is_set(FlagId::gc_acceleration_sample_count) {
            let samples = flags::gc_acceleration_sample_count();
            if samples > 0 {
                config.acceleration_sample_count = samples;
            }
        }
        if flags::is_set(FlagId::gc_momentary_spike_sample_count) {
            let samples = flags::gc_momentary_spike_sample_count();
            if samples > 0 {
                config.momentary_spike_sample_count = samples;
            }
        }
        if flags::is_set(FlagId::gc_acceleration_sample_period_ms) {
            config.acceleration_sample_period =
                Duration::from_millis(flags::gc_acceleration_sample_period_ms() as u64);
        }
        if flags::is_set(FlagId::gc_guaranteed_interval_ms) {
            let ms = flags::gc_guaranteed_interval_ms();
            config.guaranteed_gc_interval = if ms == 0 {
                None
            } else {
                Some(Duration::from_millis(ms as u64))
            };
        }

        config
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct HeapSnapshot {
    /// Current (growable) heap size.
    pub(crate) total_bytes: usize,
    /// Upper bound the heap may grow to.
    pub(crate) max_bytes: usize,
    pub(crate) available_bytes: usize,
    pub(crate) free_bytes: usize,
    pub(crate) allocated_since_gc: usize,
}

impl HeapSnapshot {
    pub(super) fn effective_free_bytes(self) -> usize {
        // MMTk exposes both free and available pages.  Available includes
        // collection reserves for copying plans, while free is closer to raw
        // unused capacity.  Use the larger value so a plan-specific accounting
        // choice does not make the heuristic trigger earlier than intended.
        self.free_bytes.max(self.available_bytes)
    }
}

fn format_trigger_decision_message(
    decision: TriggerDecision,
    mode: HeuristicMode,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) -> String {
    let mut message = format!("GC trigger {decision} ({mode}): {reason}");
    if let Some(snapshot) = snapshot {
        message.push_str(&format!(
            "; heap total={}, available={}, free={}, allocated_since_gc={}",
            FormattedSize(snapshot.total_bytes),
            FormattedSize(snapshot.available_bytes),
            FormattedSize(snapshot.free_bytes),
            FormattedSize(snapshot.allocated_since_gc),
        ));
    }
    message
}

fn log_trigger_decision(
    decision: TriggerDecision,
    mode: HeuristicMode,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) {
    {
        log::info!(
            target: GC_LOG_TARGET,
            "{}",
            format_trigger_decision_message(decision, mode, reason, snapshot)
        );
    }
}

pub(super) fn format_allocation_pressure_reason(
    free: usize,
    expected_consumption: usize,
    headroom: usize,
    allocation_rate: f64,
) -> String {
    format!(
        "allocation rate pressure exceeded threshold (free={}, expected={}, headroom={}, allocation_rate={}/s)",
        FormattedSize(free),
        FormattedSize(expected_consumption),
        FormattedSize(headroom),
        FormattedSize(rate_to_size(allocation_rate)),
    )
}

pub(super) fn rate_to_size(rate: f64) -> usize {
    if !rate.is_finite() || rate <= 0.0 {
        0
    } else if rate >= usize::MAX as f64 {
        usize::MAX
    } else {
        rate.round() as usize
    }
}

#[derive(Clone, Debug)]
pub(super) struct DecayingSequence {
    samples: usize,
    average: f64,
    variance: f64,
    alpha: f64,
}

impl DecayingSequence {
    pub(super) fn new(alpha: f64) -> Self {
        Self {
            samples: 0,
            average: 0.0,
            variance: 0.0,
            alpha,
        }
    }

    pub(super) fn add(&mut self, value: f64) {
        if self.samples == 0 {
            self.average = value;
            self.variance = 0.0;
            self.samples = 1;
            return;
        }

        let old_average = self.average;
        let delta = value - old_average;
        // Exponential moving average plus a matching decayed variance.  This is
        // cheap enough to update on every trigger evaluation and gives adaptive
        // mode both a central estimate and a safety margin.
        self.average = self.alpha * value + (1.0 - self.alpha) * old_average;
        self.variance = (1.0 - self.alpha) * (self.variance + self.alpha * delta * delta);
        self.samples += 1;
    }

    pub(super) fn upper_bound(&self, confidence: f64) -> f64 {
        if self.samples == 0 {
            0.0
        } else {
            self.average + confidence * self.standard_deviation()
        }
    }

    pub(super) fn average(&self) -> f64 {
        self.average
    }

    pub(super) fn standard_deviation(&self) -> f64 {
        self.variance.sqrt()
    }
}

#[derive(Clone, Debug)]
struct ActiveCycle {
    started_at: Instant,
    depth: usize,
    kind: Option<generational::GenerationalCycleKind>,
}

#[derive(Clone, Debug)]
pub(crate) struct HeuristicState {
    config: HeuristicConfig,
    start_gc_is_pending: bool,
    declined_trigger_count: usize,
    most_recent_declined_trigger_count: usize,
    last_trigger_type: adaptive::TriggerType,
    last_cycle_end: Option<Instant>,
    active_cycle: Option<ActiveCycle>,
    time_origin: Instant,
    gc_times_learned: usize,
    gc_time_penalties: usize,
    adaptive_confidence: f64,
    adaptive_spike_threshold: f64,
    free_at_end_history: DecayingSequence,
    gc_cycle_time_history: DecayingSequence,
    gc_cycle_time_predictor: adaptive::LinearPredictor,
    allocation_rate_history: DecayingSequence,
    acceleration_rate_history: adaptive::RateSampleHistory,
    last_allocation_sample: Option<(Instant, usize)>,
    last_acceleration_sample: Option<Instant>,
    generational: generational::GenerationalState,
}

impl HeuristicState {
    pub(crate) fn new(config: HeuristicConfig) -> Self {
        let now = Instant::now();
        let acceleration_capacity = config
            .acceleration_sample_count
            .max(config.momentary_spike_sample_count.saturating_add(1));
        Self {
            gc_cycle_time_history: DecayingSequence::new(config.adaptive_decay_factor),
            gc_cycle_time_predictor: adaptive::LinearPredictor::new(3),
            allocation_rate_history: DecayingSequence::new(config.adaptive_decay_factor),
            acceleration_rate_history: adaptive::RateSampleHistory::new(acceleration_capacity),
            free_at_end_history: DecayingSequence::new(config.adaptive_decay_factor),
            adaptive_confidence: config.adaptive_confidence,
            adaptive_spike_threshold: config.adaptive_spike_threshold,
            generational: generational::GenerationalState::new(
                config.old_garbage_threshold_percent,
            ),
            config,
            start_gc_is_pending: false,
            declined_trigger_count: 0,
            most_recent_declined_trigger_count: 0,
            last_trigger_type: adaptive::TriggerType::Other,
            last_cycle_end: None,
            active_cycle: None,
            time_origin: now,
            gc_times_learned: 0,
            gc_time_penalties: 0,
            last_allocation_sample: None,
            last_acceleration_sample: None,
        }
    }

    pub(crate) fn should_start_gc(
        &mut self,
        snapshot: HeapSnapshot,
        now: Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        if self.active_cycle.is_some() {
            /*self.log_decision(
                TriggerDecision::Suppressed,
                format_args!("GC cycle already in progress"),
                Some(snapshot),
            );*/
            return false;
        }

        // Allocation samples are collected before mode dispatch so all
        // heuristics can share the same history if the mode changes at startup.
        self.record_allocation_sample(now, snapshot.allocated_since_gc);

        if self.start_gc_is_pending {
            return true;
        }

        match self.config.mode {
            HeuristicMode::Aggressive => self.aggressive_should_start(snapshot),
            HeuristicMode::Passive => {
                // Passive mode intentionally declines proactive triggers.  The
                // MMTk plan fallback at the call site can still force GC for
                // allocation failure or other plan-specific requirements.
                self.decline_trigger();
                false
            }
            HeuristicMode::Static => self.static_should_start(snapshot, now),
            HeuristicMode::Compact => self.compact_should_start(snapshot, now),
            HeuristicMode::Adaptive => {
                self.adaptive_should_start(snapshot, now, planned_sleep_interval)
            }
            HeuristicMode::Generational => {
                self.generational_should_start(snapshot, now, planned_sleep_interval)
            }
        }
    }

    pub(crate) fn record_cycle_start(&mut self, now: Instant) {
        self.start_gc_is_pending = false;
        if let Some(active_cycle) = self.active_cycle.as_mut() {
            active_cycle.depth = active_cycle.depth.saturating_add(1);
        } else {
            let kind = self.generational.pending_cycle_kind.take();
            if self.config.mode == HeuristicMode::Generational
                && kind == Some(generational::GenerationalCycleKind::Old)
            {
                self.generational.old_cycle_in_progress = true;
                self.generational.old_cycle_started_at = Some(now);
            }
            self.active_cycle = Some(ActiveCycle {
                started_at: now,
                depth: 1,
                kind,
            });
        }
    }

    fn record_cycle_end_with_duration(
        &mut self,
        now: Instant,
        cycle_time: Duration,
        snapshot: Option<HeapSnapshot>,
    ) -> Option<Duration> {
        self.last_cycle_end = Some(now);
        let active_cycle = self.active_cycle.as_mut()?;
        if active_cycle.depth > 1 {
            active_cycle.depth -= 1;
            return None;
        }
        let cycle_started_at = active_cycle.started_at;
        let cycle_kind = active_cycle.kind;

        self.active_cycle = None;
        let cycle_time_seconds = cycle_time.as_secs_f64();
        self.gc_cycle_time_history.add(cycle_time_seconds);
        self.gc_cycle_time_predictor.add(
            elapsed_seconds(self.time_origin, cycle_started_at),
            cycle_time_seconds,
        );
        self.gc_times_learned = self.gc_times_learned.saturating_add(1);
        self.adjust_penalty(-1);
        if self.config.mode == HeuristicMode::Generational {
            if let Some(snapshot) = snapshot {
                self.record_generational_cycle_end(cycle_kind, snapshot);
            }
        }
        if let Some(snapshot) = snapshot {
            // Post-GC free memory tells adaptive mode whether the previous
            // trigger was too early or too late.
            self.adjust_adaptive_trigger_parameters(snapshot.effective_free_bytes() as f64);
        }
        Some(cycle_time)
    }

    pub(crate) fn record_allocation_sample(&mut self, now: Instant, allocated_since_gc: usize) {
        let Some((last_time, last_allocated)) = self.last_allocation_sample else {
            self.last_allocation_sample = Some((now, allocated_since_gc));
            return;
        };

        let elapsed = now.saturating_duration_since(last_time);
        if elapsed.is_zero() {
            return;
        }

        let allocated = allocated_since_gc.saturating_sub(last_allocated);
        let rate = allocated as f64 / elapsed.as_secs_f64();
        self.allocation_rate_history.add(rate);
        // The acceleration detector intentionally samples less often than
        // trigger evaluation, tiny intervals produce noisy rates and poor slopes.
        if self.last_acceleration_sample.is_none_or(|last_sample| {
            now.saturating_duration_since(last_sample) >= self.config.acceleration_sample_period
        }) {
            self.acceleration_rate_history.add(now, rate);
            self.last_acceleration_sample = Some(now);
        }
        self.last_allocation_sample = Some((now, allocated_since_gc));
    }

    fn static_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        // Static mode is the baseline: trigger only when free memory crosses the
        // configured floor, otherwise rely on shared interval and plan fallback.
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        self.base_should_start(snapshot, now)
    }

    pub(super) fn base_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        // Shared low-frequency trigger.  This prevents indefinitely idle heaps
        // from avoiding GC forever, which matters for finalizers/weak processing
        // and for returning memory in compact mode.
        if let (Some(interval), Some(last_cycle_end)) =
            (self.config.guaranteed_gc_interval, self.last_cycle_end)
        {
            let elapsed = now.saturating_duration_since(last_cycle_end);
            if elapsed > interval {
                self.accept_trigger(
                    format_args!(
                        "guaranteed interval elapsed (elapsed={elapsed:?}, interval={interval:?})"
                    ),
                    Some(snapshot),
                );
                return true;
            }
        }

        self.decline_trigger();
        false
    }

    fn min_free_threshold(&self, snapshot: HeapSnapshot) -> usize {
        percent_of(snapshot.total_bytes, self.config.min_free_threshold_percent)
    }

    pub(super) fn trigger_if_below_min_free(&mut self, snapshot: HeapSnapshot) -> bool {
        let min_threshold = self.min_free_threshold(snapshot);
        let free_bytes = snapshot.effective_free_bytes();
        if free_bytes >= min_threshold {
            return false;
        }

        self.accept_trigger(
            format_args!(
                "free below minimum threshold (free={}, threshold={})",
                FormattedSize(free_bytes),
                FormattedSize(min_threshold),
            ),
            Some(snapshot),
        );
        true
    }

    pub(super) fn headroom_adjustment(&self, capacity: usize) -> usize {
        // The floor keeps the spike reserve meaningful while the heap is small.
        percent_of(capacity, self.config.alloc_spike_factor_percent)
            .saturating_add(percent_of(capacity, self.gc_time_penalties))
            .max(HEADROOM_FLOOR_BYTES.min(capacity / 4))
    }

    pub(super) fn accept_trigger(
        &mut self,
        reason: impl fmt::Display,
        snapshot: Option<HeapSnapshot>,
    ) {
        // Non-adaptive reasons reset the adaptive trigger classification.  That
        // prevents later feedback from tuning rate/spike flags for a min-free or
        // guaranteed-interval cycle.
        self.last_trigger_type = adaptive::TriggerType::Other;
        self.most_recent_declined_trigger_count = self.declined_trigger_count;
        self.declined_trigger_count = 0;
        self.start_gc_is_pending = true;
        self.log_decision(TriggerDecision::Accepted, reason, snapshot);
    }

    fn decline_trigger(&mut self) {
        self.declined_trigger_count = self.declined_trigger_count.saturating_add(1);
    }

    pub(super) fn log_decision(
        &self,
        decision: TriggerDecision,
        reason: impl fmt::Display,
        snapshot: Option<HeapSnapshot>,
    ) {
        log_trigger_decision(decision, self.config.mode, reason, snapshot);
    }

    pub(super) fn adjust_penalty(&mut self, step: isize) {
        self.gc_time_penalties = self.gc_time_penalties.saturating_add_signed(step).min(100);
    }
}

pub(crate) struct CapyTriggerPolicy {
    state: Mutex<HeuristicState>,
    max_heap_pages: usize,
    initial_heap_pages: usize,
    current_heap_pages: AtomicUsize,
    shrink_streak: AtomicUsize,
    pending_pages: AtomicUsize,
    used_at_last_gc_end: AtomicUsize,
}

impl CapyTriggerPolicy {
    pub(crate) fn new() -> Self {
        Self::with_config(HeuristicConfig::from_env())
    }

    fn with_config(config: HeuristicConfig) -> Self {
        let max_heap_pages = bytes_to_pages_up(config.max_heap_bytes);
        let initial_heap_pages = bytes_to_pages_up(config.initial_heap_bytes)
            .min(max_heap_pages)
            .max(1);
        Self {
            state: Mutex::new(HeuristicState::new(config)),
            max_heap_pages,
            initial_heap_pages,
            current_heap_pages: AtomicUsize::new(initial_heap_pages),
            shrink_streak: AtomicUsize::new(0),
            pending_pages: AtomicUsize::new(0),
            used_at_last_gc_end: AtomicUsize::new(0),
        }
    }

    fn current_heap_bytes(&self) -> usize {
        pages_to_bytes(self.current_heap_pages.load(Ordering::Relaxed))
    }

    fn snapshot_with(
        &self,
        available_bytes: usize,
        free_bytes: usize,
        allocated_since_gc: usize,
    ) -> HeapSnapshot {
        HeapSnapshot {
            total_bytes: self.current_heap_bytes(),
            max_bytes: pages_to_bytes(self.max_heap_pages),
            available_bytes,
            free_bytes,
            allocated_since_gc,
        }
    }

    /// Resizes the heap from post-GC live data; returns `(old, new)` pages on change.
    fn resize_after_gc(
        &self,
        used_pages: usize,
        free_percent: usize,
        min_free_percent: usize,
    ) -> Option<(usize, usize)> {
        let current = self.current_heap_pages.load(Ordering::Relaxed);
        let streak = self.shrink_streak.load(Ordering::Relaxed);
        let (next, streak) = next_heap_pages(
            used_pages,
            current,
            self.initial_heap_pages,
            self.max_heap_pages,
            free_percent,
            min_free_percent,
            streak,
        );
        self.shrink_streak.store(streak, Ordering::Relaxed);
        self.current_heap_pages.store(next, Ordering::Relaxed);
        (next != current).then_some((current, next))
    }

    fn lock_state(&self) -> std::sync::MutexGuard<'_, HeuristicState> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

impl GCTriggerPolicy<MemoryManager> for CapyTriggerPolicy {
    fn on_pending_allocation(&self, pages: usize) {
        self.pending_pages.fetch_add(pages, Ordering::Relaxed);
    }

    fn on_gc_start(&self, _mmtk: &'static MMTK<MemoryManager>) {
        let now = Instant::now();
        self.pending_pages.store(0, Ordering::Relaxed);
        {
            let mut global_stats = crate::runtime::GLOBAL_STATS.lock();
            global_stats.start_gc_at(now);
        }
        self.lock_state().record_cycle_start(now);
    }

    fn on_pause_start(&self, _mmtk: &'static MMTK<MemoryManager>) {
        crate::runtime::gc_stats::mark_pause_start(
            crate::runtime::vm::base::real_time_ms(),
            crate::runtime::vm::base::cpu_time_ms(),
        );
    }

    fn on_pause_end(&self, _mmtk: &'static MMTK<MemoryManager>) {
        crate::runtime::gc_stats::mark_pause_end(
            crate::runtime::vm::base::real_time_ms(),
            crate::runtime::vm::base::cpu_time_ms(),
        );
    }

    fn on_gc_release(&self, mmtk: &'static MMTK<MemoryManager>) {
        crate::runtime::gc_stats::mark_gc_release(mmtk::memory_manager::used_bytes(mmtk));
    }

    fn on_gc_end(&self, mmtk: &'static MMTK<MemoryManager>) {
        let now = Instant::now();
        {
            let mut global_stats = crate::runtime::GLOBAL_STATS.lock();
            global_stats.end_gc_at(now);
        }
        let plan = mmtk.get_plan();
        let used_bytes = pages_to_bytes(plan.get_used_pages());
        crate::runtime::gc_stats::mark_gc_end(used_bytes);
        self.used_at_last_gc_end
            .store(used_bytes, Ordering::Relaxed);
        let (growth_free, min_free) = {
            let state = self.lock_state();
            (
                state.config.heap_growth_free_percent,
                state.config.min_free_threshold_percent,
            )
        };
        if let Some((old, new)) = self.resize_after_gc(plan.get_used_pages(), growth_free, min_free)
        {
            log::info!(
                target: GC_LOG_TARGET,
                "heap resized {} -> {} (live={}, max={})",
                FormattedSize(pages_to_bytes(old)),
                FormattedSize(pages_to_bytes(new)),
                FormattedSize(used_bytes),
                FormattedSize(pages_to_bytes(self.max_heap_pages)),
            );
        }
        // Built after the resize so available/free reflect the new size.
        let snapshot = self.snapshot_with(
            pages_to_bytes(plan.get_available_pages()),
            pages_to_bytes(plan.get_free_pages()),
            0,
        );

        // Feed post-GC plan accounting back into the heuristic.  Adaptive mode
        // uses this to adjust how early rate/spike triggers should fire.
        let cycle_time = {
            let mut state = self.lock_state();
            let cycle_time = state
                .active_cycle
                .as_ref()
                .map(|cycle| now.saturating_duration_since(cycle.started_at))
                .unwrap_or_default();
            state.record_cycle_end_with_duration(now, cycle_time, Some(snapshot))
        };

        if let Some(cycle_time) = cycle_time {
            log::trace!(
                target: GC_LOG_TARGET,
                "Capy-style trigger recorded GC cycle time {:?}",
                cycle_time
            );
        }
    }

    fn is_gc_required(
        &self,
        space_full: bool,
        space: Option<SpaceStats<MemoryManager>>,
        plan: &dyn Plan<VM = MemoryManager>,
    ) -> bool {
        // Include failed/pending allocation pages in the snapshot.  MMTk has not
        // reserved them yet, but the trigger decision must make room for them.
        let pending_bytes = pages_to_bytes(self.pending_pages.load(Ordering::Relaxed));
        let used_bytes = pages_to_bytes(plan.get_used_pages()).saturating_add(pending_bytes);
        let available_bytes =
            pages_to_bytes(plan.get_available_pages()).saturating_sub(pending_bytes);
        let free_bytes = pages_to_bytes(plan.get_free_pages()).saturating_sub(pending_bytes);
        let allocated_since_gc =
            used_bytes.saturating_sub(self.used_at_last_gc_end.load(Ordering::Relaxed));
        let snapshot = self.snapshot_with(available_bytes, free_bytes, allocated_since_gc);

        self.lock_state()
            .should_start_gc(snapshot, Instant::now(), Duration::ZERO)
            || plan.collection_required(space_full, space)
    }

    fn is_heap_full(&self, plan: &dyn Plan<VM = MemoryManager>) -> bool {
        let pending_pages = self.pending_pages.load(Ordering::Relaxed);
        let current = self.current_heap_pages.load(Ordering::Relaxed);
        plan.get_reserved_pages().saturating_add(pending_pages) > current
    }

    fn get_current_heap_size_in_pages(&self) -> usize {
        self.current_heap_pages.load(Ordering::Relaxed)
    }

    fn get_max_heap_size_in_pages(&self) -> usize {
        self.max_heap_pages
    }

    fn can_heap_size_grow(&self) -> bool {
        self.current_heap_pages.load(Ordering::Relaxed) < self.max_heap_pages
    }
}

pub(crate) fn create_gc_trigger() -> Box<dyn GCTriggerPolicy<MemoryManager>> {
    Box::new(CapyTriggerPolicy::new())
}

pub(crate) fn set_default_max_heap_bytes(bytes: usize) {
    DEFAULT_MAX_HEAP_OVERRIDE.store(bytes, Ordering::Relaxed);
}

pub(crate) fn configured_max_heap_bytes() -> usize {
    if flags::is_set(FlagId::gc_max_heap) {
        return flags::gc_max_heap();
    }

    let overridden = DEFAULT_MAX_HEAP_OVERRIDE.load(Ordering::Relaxed);
    if overridden != 0 {
        return overridden;
    }

    match flags::mmtk_gc_trigger().and_then(|value| value.parse::<GCTriggerSelector>().ok()) {
        Some(GCTriggerSelector::FixedHeapSize(bytes)) => bytes,
        Some(GCTriggerSelector::DynamicHeapSize(_, bytes)) => bytes,
        _ => DEFAULT_MAX_HEAP_BYTES,
    }
}

/// Computes the heap size for the next cycle from post-GC live pages.
///
/// Grows immediately toward `used / (1 - free%)`, steps by at least 25% when the
/// post-GC free share is under the minimum, and shrinks only after the target
/// has stayed under half the current size for `HEAP_SHRINK_AFTER_CYCLES`
/// cycles.  Returns the new size and the updated shrink streak.
fn next_heap_pages(
    used: usize,
    current: usize,
    initial: usize,
    max: usize,
    free_percent: usize,
    min_free_percent: usize,
    shrink_streak: usize,
) -> (usize, usize) {
    let live_percent = 100 - free_percent.min(99);
    let target = used.saturating_mul(100) / live_percent;
    let free_pages = current.saturating_sub(used);

    let mut next = current;
    let mut streak = 0;
    if target > current {
        next = target;
    }
    if free_pages < current / 100 * min_free_percent {
        next = next.max(current + current / 100 * HEAP_GROWTH_MIN_STEP_PERCENT);
    } else if target < current / 2 {
        streak = shrink_streak + 1;
        if streak >= HEAP_SHRINK_AFTER_CYCLES {
            next = target;
            streak = 0;
        }
    }
    (next.clamp(initial.min(max), max), streak)
}

fn percent_of(total: usize, percent: usize) -> usize {
    total / 100 * percent
}

fn pages_to_bytes(pages: usize) -> usize {
    pages.saturating_mul(mmtk::util::constants::BYTES_IN_PAGE)
}

fn elapsed_seconds(start: Instant, end: Instant) -> f64 {
    end.saturating_duration_since(start).as_secs_f64()
}

#[cfg(test)]
mod tests {
    use super::next_heap_pages;

    #[test]
    fn grows_to_target_and_caps_at_max() {
        assert_eq!(next_heap_pages(60, 100, 10, 1000, 50, 10, 0).0, 120);
        assert_eq!(next_heap_pages(900, 100, 10, 1000, 50, 10, 0).0, 1000);
    }

    #[test]
    fn near_full_steps_by_at_least_a_quarter() {
        assert_eq!(next_heap_pages(95, 100, 10, 1000, 10, 10, 0).0, 125);
    }

    #[test]
    fn shrinks_only_after_streak_and_not_below_initial() {
        let (n, s) = next_heap_pages(10, 400, 100, 1000, 50, 10, 0);
        assert_eq!((n, s), (400, 1));
        let (n, s) = next_heap_pages(10, 400, 100, 1000, 50, 10, 2);
        assert_eq!((n, s), (100, 0));
    }

    #[test]
    fn initial_at_or_above_max_is_fixed() {
        assert_eq!(next_heap_pages(10, 1000, 1000, 1000, 50, 10, 2).0, 1000);
    }
}
