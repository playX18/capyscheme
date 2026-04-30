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

const DEFAULT_MAX_HEAP_BYTES: usize = 2 * 1024 * 1024 * 1024;
const DEFAULT_MIN_FREE_THRESHOLD_PERCENT: usize = 10;
const DEFAULT_INIT_FREE_THRESHOLD_PERCENT: usize = 70;
const DEFAULT_ALLOCATION_THRESHOLD_PERCENT: usize = 0;
const DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT: usize = 5;
const DEFAULT_LEARNING_STEPS: usize = 5;
const DEFAULT_ADAPTIVE_DECAY_FACTOR: f64 = 0.5;
const DEFAULT_ADAPTIVE_CONFIDENCE: f64 = 1.8;
const DEFAULT_GUARANTEED_GC_INTERVAL: Duration = Duration::from_secs(5 * 60);
const COMPACT_ALLOCATION_THRESHOLD_PERCENT: usize = 10;
const COMPACT_GUARANTEED_GC_INTERVAL: Duration = Duration::from_secs(30);

static DEFAULT_MAX_HEAP_OVERRIDE: AtomicUsize = AtomicUsize::new(0);

const GC_LOG_TARGET: &str = "capy::gc";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum HeuristicMode {
    Adaptive,
    Static,
    Compact,
    Aggressive,
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
            adaptive_confidence: DEFAULT_ADAPTIVE_CONFIDENCE,
            guaranteed_gc_interval: Some(DEFAULT_GUARANTEED_GC_INTERVAL),
        };

        if mode == HeuristicMode::Compact {
            config.allocation_threshold_percent = COMPACT_ALLOCATION_THRESHOLD_PERCENT;
            config.guaranteed_gc_interval = Some(COMPACT_GUARANTEED_GC_INTERVAL);
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

        if let Some(ms) = env_usize("CAPY_GC_GUARANTEED_INTERVAL_MS") {
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
    pub(crate) total_bytes: usize,
    pub(crate) available_bytes: usize,
    pub(crate) free_bytes: usize,
    pub(crate) allocated_since_gc: usize,
}

impl HeapSnapshot {
    fn effective_free_bytes(self) -> usize {
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

fn format_allocation_pressure_reason(
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

fn rate_to_size(rate: f64) -> usize {
    if !rate.is_finite() || rate <= 0.0 {
        0
    } else if rate >= usize::MAX as f64 {
        usize::MAX
    } else {
        rate.round() as usize
    }
}

#[derive(Clone, Debug)]
struct DecayingSequence {
    samples: usize,
    average: f64,
    variance: f64,
    alpha: f64,
}

impl DecayingSequence {
    fn new(alpha: f64) -> Self {
        Self {
            samples: 0,
            average: 0.0,
            variance: 0.0,
            alpha,
        }
    }

    fn add(&mut self, value: f64) {
        if self.samples == 0 {
            self.average = value;
            self.variance = 0.0;
            self.samples = 1;
            return;
        }

        let old_average = self.average;
        let delta = value - old_average;
        self.average = self.alpha * value + (1.0 - self.alpha) * old_average;
        self.variance = (1.0 - self.alpha) * (self.variance + self.alpha * delta * delta);
        self.samples += 1;
    }

    fn upper_bound(&self, confidence: f64) -> f64 {
        if self.samples == 0 {
            0.0
        } else {
            self.average + confidence * self.variance.sqrt()
        }
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
    last_cycle_end: Option<Instant>,
    active_cycle: Option<ActiveCycle>,
    gc_times_learned: usize,
    gc_time_penalties: usize,
    gc_cycle_time_history: DecayingSequence,
    allocation_rate_history: DecayingSequence,
    last_allocation_sample: Option<(Instant, usize)>,
}

impl HeuristicState {
    pub(crate) fn new(config: HeuristicConfig) -> Self {
        Self {
            gc_cycle_time_history: DecayingSequence::new(config.adaptive_decay_factor),
            allocation_rate_history: DecayingSequence::new(config.adaptive_decay_factor),
            config,
            start_gc_is_pending: false,
            declined_trigger_count: 0,
            most_recent_declined_trigger_count: 0,
            last_cycle_end: None,
            active_cycle: None,
            gc_times_learned: 0,
            gc_time_penalties: 0,
            last_allocation_sample: None,
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

        self.record_allocation_sample(now, snapshot.allocated_since_gc);

        if self.start_gc_is_pending {
            return true;
        }

        match self.config.mode {
            HeuristicMode::Aggressive => {
                self.accept_trigger(format_args!("aggressive mode"), Some(snapshot));
                true
            }
            HeuristicMode::Passive => {
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

    fn record_cycle_end(&mut self, now: Instant) -> Option<Duration> {
        let cycle_time = self
            .active_cycle
            .as_ref()
            .map(|cycle| now.saturating_duration_since(cycle.started_at))
            .unwrap_or_default();

        self.record_cycle_end_with_duration(now, cycle_time)
    }

    fn record_cycle_end_with_duration(
        &mut self,
        now: Instant,
        cycle_time: Duration,
    ) -> Option<Duration> {
        self.last_cycle_end = Some(now);
        let active_cycle = self.active_cycle.as_mut()?;
        if active_cycle.depth > 1 {
            active_cycle.depth -= 1;
            return None;
        }

        self.active_cycle = None;
        self.gc_cycle_time_history.add(cycle_time.as_secs_f64());
        self.gc_times_learned = self.gc_times_learned.saturating_add(1);
        self.adjust_penalty(-1);
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
        self.last_allocation_sample = Some((now, allocated_since_gc));
    }

    fn static_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        self.base_should_start(snapshot, now)
    }

    fn compact_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        let threshold = percent_of(
            snapshot.total_bytes,
            self.config.allocation_threshold_percent,
        );
        if threshold > 0 && snapshot.allocated_since_gc > threshold {
            self.accept_trigger(
                format_args!(
                    "allocated since last GC above threshold (allocated={}, threshold={})",
                    FormattedSize(snapshot.allocated_since_gc),
                    FormattedSize(threshold),
                ),
                Some(snapshot),
            );
            return true;
        }

        self.base_should_start(snapshot, now)
    }

    fn adaptive_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

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

        if snapshot.allocated_since_gc.saturating_mul(3) <= snapshot.effective_free_bytes() {
            return self.base_should_start(snapshot, now);
        }

        let cycle_time = self
            .gc_cycle_time_history
            .upper_bound(self.config.adaptive_confidence)
            + planned_sleep_interval.as_secs_f64();
        let allocation_rate = self
            .allocation_rate_history
            .upper_bound(self.config.adaptive_confidence);
        let headroom = self.headroom_adjustment(snapshot.total_bytes);
        let expected_consumption = (cycle_time * allocation_rate) as usize;

        if snapshot.effective_free_bytes() <= expected_consumption.saturating_add(headroom) {
            self.accept_trigger(
                format_allocation_pressure_reason(
                    snapshot.effective_free_bytes(),
                    expected_consumption,
                    headroom,
                    allocation_rate,
                ),
                Some(snapshot),
            );
            return true;
        }

        self.base_should_start(snapshot, now)
    }

    fn base_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
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

    fn trigger_if_below_min_free(&mut self, snapshot: HeapSnapshot) -> bool {
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

    fn headroom_adjustment(&self, capacity: usize) -> usize {
        percent_of(capacity, self.config.alloc_spike_factor_percent)
            .saturating_add(percent_of(capacity, self.gc_time_penalties))
    }

    fn accept_trigger(&mut self, reason: impl fmt::Display, snapshot: Option<HeapSnapshot>) {
        self.most_recent_declined_trigger_count = self.declined_trigger_count;
        self.declined_trigger_count = 0;
        self.start_gc_is_pending = true;
        self.log_decision(TriggerDecision::Accepted, reason, snapshot);
    }

    fn decline_trigger(&mut self) {
        self.declined_trigger_count = self.declined_trigger_count.saturating_add(1);
    }

    fn log_decision(
        &self,
        decision: TriggerDecision,
        reason: impl fmt::Display,
        snapshot: Option<HeapSnapshot>,
    ) {
        log_trigger_decision(decision, self.config.mode, reason, snapshot);
    }

    fn adjust_penalty(&mut self, step: isize) {
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
        self.pending_pages.store(0, Ordering::Relaxed);
        self.lock_state().record_cycle_start(Instant::now());
    }

    fn on_gc_end(&self, mmtk: &'static MMTK<MemoryManager>) {
        let now = Instant::now();
        let used_bytes = pages_to_bytes(mmtk.get_plan().get_used_pages());
        self.used_at_last_gc_end
            .store(used_bytes, Ordering::Relaxed);

        let cycle_time = {
            let mut state = self.lock_state();
            state.record_cycle_end(now)
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

fn env_size(name: &str) -> Option<usize> {
    parse_size(&env::var(name).ok()?)
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
