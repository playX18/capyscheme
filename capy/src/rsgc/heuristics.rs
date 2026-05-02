//! Capy GC trigger policy and shared heuristic state.
//!
//! The MMTk trigger callback enters this module with plan-level heap accounting.
//! Shared code builds a small heap snapshot, records allocation/GC history, and
//! dispatches to the selected heuristic mode.  Mode-specific behavior lives in
//! sibling modules when it has enough logic to stand alone.

use std::{
    env, fmt,
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

use crate::{rsgc::mm::MemoryManager, utils::FormattedSize};

mod adaptive;
mod aggressive;
mod compact;

const DEFAULT_MAX_HEAP_BYTES: usize = 2 * 1024 * 1024 * 1024;
const DEFAULT_MIN_FREE_THRESHOLD_PERCENT: usize = 10;
const DEFAULT_INIT_FREE_THRESHOLD_PERCENT: usize = 70;
const DEFAULT_ALLOCATION_THRESHOLD_PERCENT: usize = 0;
const DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT: usize = 5;
const DEFAULT_LEARNING_STEPS: usize = 5;
const DEFAULT_ADAPTIVE_DECAY_FACTOR: f64 = 0.5;
const DEFAULT_GUARANTEED_GC_INTERVAL: Duration = Duration::from_secs(5 * 60);

static DEFAULT_MAX_HEAP_OVERRIDE: AtomicUsize = AtomicUsize::new(0);

const GC_LOG_TARGET: &str = "capy::gc";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum HeuristicMode {
    /// Shenandoah-style predictor using GC-time and allocation-rate history.
    Adaptive,
    /// Minimum-free plus guaranteed-interval trigger only.
    Static,
    /// Small-heap mode with a fixed allocation-since-GC threshold.
    Compact,
    /// Trigger at every opportunity.
    Aggressive,
    /// Never trigger proactively; only MMTk plan fallbacks can require GC.
    Passive,
}

impl HeuristicMode {
    fn from_env_value(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "adaptive" => Some(Self::Adaptive),
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
}

impl HeuristicConfig {
    fn defaults_for_mode(mode: HeuristicMode) -> Self {
        let mut config = Self {
            mode,
            max_heap_bytes: configured_max_heap_bytes(),
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
        };

        if mode == HeuristicMode::Compact {
            config.allocation_threshold_percent = compact::ALLOCATION_THRESHOLD_PERCENT;
            config.guaranteed_gc_interval = Some(compact::GUARANTEED_GC_INTERVAL);
        }

        config
    }

    pub(crate) fn from_env() -> Self {
        let mode = env::var("CAPY_GC_HEURISTIC")
            .ok()
            .and_then(|value| HeuristicMode::from_env_value(&value))
            .unwrap_or(HeuristicMode::Adaptive);
        let mut config = Self::defaults_for_mode(mode);

        config.max_heap_bytes = env_size("CAPY_GC_MAX_HEAP").unwrap_or(config.max_heap_bytes);
        config.min_free_threshold_percent = env_percent(
            "CAPY_GC_MIN_FREE_PERCENT",
            config.min_free_threshold_percent,
        );
        config.init_free_threshold_percent = env_percent(
            "CAPY_GC_INIT_FREE_PERCENT",
            config.init_free_threshold_percent,
        );
        config.allocation_threshold_percent = env_percent(
            "CAPY_GC_ALLOCATION_THRESHOLD_PERCENT",
            config.allocation_threshold_percent,
        );
        config.alloc_spike_factor_percent = env_percent(
            "CAPY_GC_ALLOC_SPIKE_PERCENT",
            config.alloc_spike_factor_percent,
        );
        config.learning_steps =
            env_usize("CAPY_GC_LEARNING_STEPS").unwrap_or(config.learning_steps);
        config.adaptive_confidence =
            env_f64("CAPY_GC_ADAPTIVE_CONFIDENCE").unwrap_or(config.adaptive_confidence);
        config.adaptive_spike_threshold =
            env_f64("CAPY_GC_ADAPTIVE_SPIKE_THRESHOLD").unwrap_or(config.adaptive_spike_threshold);
        config.acceleration_sample_count = env_usize("CAPY_GC_ACCELERATION_SAMPLE_COUNT")
            .filter(|samples| *samples > 0)
            .unwrap_or(config.acceleration_sample_count);
        config.momentary_spike_sample_count = env_usize("CAPY_GC_MOMENTARY_SPIKE_SAMPLE_COUNT")
            .filter(|samples| *samples > 0)
            .unwrap_or(config.momentary_spike_sample_count);

        if let Some(ms) = env_usize("CAPY_GC_GUARANTEED_INTERVAL_MS") {
            config.guaranteed_gc_interval = if ms == 0 {
                None
            } else {
                Some(Duration::from_millis(ms as u64))
            };
        }
        if let Some(ms) = env_usize("CAPY_GC_ACCELERATION_SAMPLE_PERIOD_MS") {
            config.acceleration_sample_period = Duration::from_millis(ms as u64);
        }

        config
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct HeapSnapshot {
    pub(crate) total_bytes: usize,
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

fn format_trigger_decision_log_line(
    decision: TriggerDecision,
    mode: HeuristicMode,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) -> String {
    crate::rsgc::logging::format_log_line(
        log::Level::Info,
        GC_LOG_TARGET,
        format_trigger_decision_message(decision, mode, reason, snapshot),
    )
}

fn log_trigger_decision(
    decision: TriggerDecision,
    mode: HeuristicMode,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) {
    if crate::rsgc::logging::gc_logging_enabled() {
        eprintln!(
            "{}",
            format_trigger_decision_log_line(decision, mode, reason, snapshot)
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
        }
    }

    pub(crate) fn record_cycle_start(&mut self, now: Instant) {
        self.start_gc_is_pending = false;
        if let Some(active_cycle) = self.active_cycle.as_mut() {
            active_cycle.depth = active_cycle.depth.saturating_add(1);
        } else {
            self.active_cycle = Some(ActiveCycle {
                started_at: now,
                depth: 1,
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

        self.active_cycle = None;
        let cycle_time_seconds = cycle_time.as_secs_f64();
        // Successful cycles feed both the average and the trend predictor used
        // by adaptive mode.  The timestamp is the cycle start, matching the
        // question "how long would a cycle starting at future time T take?".
        self.gc_cycle_time_history.add(cycle_time_seconds);
        self.gc_cycle_time_predictor.add(
            elapsed_seconds(self.time_origin, cycle_started_at),
            cycle_time_seconds,
        );
        self.gc_times_learned = self.gc_times_learned.saturating_add(1);
        self.adjust_penalty(-1);
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
        // trigger evaluation; tiny intervals produce noisy rates and poor slopes.
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
        percent_of(capacity, self.config.alloc_spike_factor_percent)
            .saturating_add(percent_of(capacity, self.gc_time_penalties))
    }

    pub(super) fn accept_trigger(
        &mut self,
        reason: impl fmt::Display,
        snapshot: Option<HeapSnapshot>,
    ) {
        // Non-adaptive reasons reset the adaptive trigger classification.  That
        // prevents later feedback from tuning rate/spike knobs for a min-free or
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
    pending_pages: AtomicUsize,
    used_at_last_gc_end: AtomicUsize,
}

impl CapyTriggerPolicy {
    pub(crate) fn new() -> Self {
        Self::with_config(HeuristicConfig::from_env())
    }

    fn with_config(config: HeuristicConfig) -> Self {
        let max_heap_pages = bytes_to_pages_up(config.max_heap_bytes);
        Self {
            state: Mutex::new(HeuristicState::new(config)),
            max_heap_pages,
            pending_pages: AtomicUsize::new(0),
            used_at_last_gc_end: AtomicUsize::new(0),
        }
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

    fn on_gc_end(&self, mmtk: &'static MMTK<MemoryManager>) {
        let now = Instant::now();
        {
            let mut global_stats = crate::runtime::GLOBAL_STATS.lock();
            global_stats.end_gc_at(now);
        }
        let plan = mmtk.get_plan();
        let used_bytes = pages_to_bytes(plan.get_used_pages());
        self.used_at_last_gc_end
            .store(used_bytes, Ordering::Relaxed);
        let snapshot = HeapSnapshot {
            total_bytes: pages_to_bytes(self.max_heap_pages),
            available_bytes: pages_to_bytes(plan.get_available_pages()),
            free_bytes: pages_to_bytes(plan.get_free_pages()),
            allocated_since_gc: 0,
        };

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
        let total_bytes = pages_to_bytes(self.max_heap_pages);
        let available_bytes =
            pages_to_bytes(plan.get_available_pages()).saturating_sub(pending_bytes);
        let free_bytes = pages_to_bytes(plan.get_free_pages()).saturating_sub(pending_bytes);
        let allocated_since_gc =
            used_bytes.saturating_sub(self.used_at_last_gc_end.load(Ordering::Relaxed));
        let snapshot = HeapSnapshot {
            total_bytes,
            available_bytes,
            free_bytes,
            allocated_since_gc,
        };

        self.lock_state()
            .should_start_gc(snapshot, Instant::now(), Duration::ZERO)
            || plan.collection_required(space_full, space)
    }

    fn is_heap_full(&self, plan: &dyn Plan<VM = MemoryManager>) -> bool {
        let pending_pages = self.pending_pages.load(Ordering::Relaxed);
        plan.get_reserved_pages().saturating_add(pending_pages) > self.max_heap_pages
    }

    fn get_current_heap_size_in_pages(&self) -> usize {
        self.max_heap_pages
    }

    fn get_max_heap_size_in_pages(&self) -> usize {
        self.max_heap_pages
    }

    fn can_heap_size_grow(&self) -> bool {
        false
    }
}

pub(crate) fn create_gc_trigger() -> Box<dyn GCTriggerPolicy<MemoryManager>> {
    Box::new(CapyTriggerPolicy::new())
}

pub(crate) fn set_default_max_heap_bytes(bytes: usize) {
    DEFAULT_MAX_HEAP_OVERRIDE.store(bytes, Ordering::Relaxed);
}

pub(crate) fn configured_max_heap_bytes() -> usize {
    if let Some(bytes) = env_size("CAPY_GC_MAX_HEAP") {
        return bytes;
    }

    let overridden = DEFAULT_MAX_HEAP_OVERRIDE.load(Ordering::Relaxed);
    if overridden != 0 {
        return overridden;
    }

    match env::var("MMTK_GC_TRIGGER")
        .ok()
        .and_then(|value| value.parse::<GCTriggerSelector>().ok())
    {
        Some(GCTriggerSelector::FixedHeapSize(bytes)) => bytes,
        Some(GCTriggerSelector::DynamicHeapSize(_, bytes)) => bytes,
        _ => DEFAULT_MAX_HEAP_BYTES,
    }
}

fn percent_of(total: usize, percent: usize) -> usize {
    total / 100 * percent
}

fn pages_to_bytes(pages: usize) -> usize {
    pages.saturating_mul(mmtk::util::constants::BYTES_IN_PAGE)
}

fn env_percent(name: &str, default: usize) -> usize {
    env_usize(name)
        .filter(|value| *value <= 100)
        .unwrap_or(default)
}

fn env_usize(name: &str) -> Option<usize> {
    env::var(name).ok()?.parse().ok()
}

fn env_f64(name: &str) -> Option<f64> {
    env::var(name).ok()?.parse().ok()
}

fn env_size(name: &str) -> Option<usize> {
    parse_size(&env::var(name).ok()?)
}

fn elapsed_seconds(start: Instant, end: Instant) -> f64 {
    end.saturating_duration_since(start).as_secs_f64()
}

fn parse_size(value: &str) -> Option<usize> {
    let value = value.trim();
    if value.is_empty() {
        return None;
    }

    let split_at = value
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(value.len());
    let (digits, suffix) = value.split_at(split_at);
    let number = digits.parse::<usize>().ok()?;
    let multiplier = match suffix.trim().to_ascii_lowercase().as_str() {
        "" | "b" => 1,
        "k" | "kb" => 1024,
        "m" | "mb" => 1024 * 1024,
        "g" | "gb" => 1024 * 1024 * 1024,
        _ => return None,
    };

    number.checked_mul(multiplier)
}
