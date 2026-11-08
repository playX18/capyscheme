//! Capy GC trigger policy and shared heuristic state.

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
    plan::{Pause, Plan},
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

mod eager;
mod generational;
mod goal;
pub(crate) mod pacer;
mod predictive;
mod proportional;
mod runway;
mod sizing;
mod sysmem;

const DEFAULT_MAX_HEAP_BYTES: usize = 2 * 1024 * 1024 * 1024;
/// Auto starting heap is `available / divisor`, never below the floor.
const AUTO_INIT_HEAP_AVAILABLE_DIVISOR: usize = 32;
const AUTO_INIT_HEAP_MIN_BYTES: usize = 32 * 1024 * 1024;
const DEFAULT_MIN_FREE_THRESHOLD_PERCENT: usize = 10;
const DEFAULT_INIT_FREE_THRESHOLD_PERCENT: usize = 50;
const DEFAULT_HEAP_GROWTH_FREE_PERCENT: usize = 75;
const HEAP_GROWTH_MIN_STEP_PERCENT: usize = 25;
const HEAP_SHRINK_AFTER_CYCLES: usize = 3;
const HEADROOM_FLOOR_BYTES: usize = 1024 * 1024;
const PACING_FAILURE_PENALTY_PERCENT: isize = 10;
const DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT: usize = 5;
const DEFAULT_LEARNING_STEPS: usize = 5;
const DEFAULT_PREDICTIVE_DECAY_FACTOR: f64 = 0.5;
const DEFAULT_GUARANTEED_GC_INTERVAL: Duration = Duration::from_secs(5 * 60);

static DEFAULT_MAX_HEAP_OVERRIDE: AtomicUsize = AtomicUsize::new(0);

/// When to start a collection.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum TriggerPolicy {
    /// Shenandoah-style predictor from GC-time and allocation-rate history.
    Predictive,
    /// Predictive young triggers plus an old-growth gate (generational plans).
    Generational,
    /// ZGC-style: free space would run out before a worst-case GC finishes.
    Runway,
    /// Go-style: heap reaches `goal - marking runway`.
    Goal,
    /// BDWGC/Chez-style: allocation since the last GC reaches a share of live data.
    Proportional,
    /// Minimum-free floor plus a guaranteed interval only.
    Floor,
    /// Every opportunity.
    Eager,
    /// Never proactively; only the MMTk plan can require a GC.
    Deferred,
}

impl TriggerPolicy {
    const WORDS: &'static [(&'static str, Self)] = &[
        ("predictive", Self::Predictive),
        ("generational", Self::Generational),
        ("runway", Self::Runway),
        ("goal", Self::Goal),
        ("proportional", Self::Proportional),
        ("floor", Self::Floor),
        ("eager", Self::Eager),
        ("deferred", Self::Deferred),
    ];

    fn from_word(value: &str) -> Option<Self> {
        Self::WORDS
            .iter()
            .find(|(word, _)| word.eq_ignore_ascii_case(value))
            .map(|(_, policy)| *policy)
    }

    /// Whether this policy, not the plan's fixed allocation threshold, decides
    /// when concurrent marking starts.
    fn owns_initial_mark(self) -> bool {
        matches!(self, Self::Runway | Self::Goal | Self::Proportional)
    }
}

impl fmt::Display for TriggerPolicy {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(
            Self::WORDS
                .iter()
                .find(|(_, policy)| policy == self)
                .map_or("unknown", |(word, _)| word),
        )
    }
}

/// How large the heap should be after a collection.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SizingPolicy {
    /// Size so a fixed share of the heap is free after the GC; shrink with hysteresis.
    FreeTarget,
    /// Keep the heap inside a ratio band over live data (Ruby's MMTk heap).
    Banded,
    /// Steer the heap size to hold GC CPU overhead at a target (MPLR '23).
    Overhead,
    /// MemBalancer: headroom grows with the square root of allocation over collection rate.
    Balanced,
}

impl SizingPolicy {
    const WORDS: &'static [(&'static str, Self)] = &[
        ("free-target", Self::FreeTarget),
        ("banded", Self::Banded),
        ("overhead", Self::Overhead),
        ("balanced", Self::Balanced),
    ];

    fn from_word(value: &str) -> Option<Self> {
        Self::WORDS
            .iter()
            .find(|(word, _)| word.eq_ignore_ascii_case(value))
            .map(|(_, policy)| *policy)
    }
}

impl fmt::Display for SizingPolicy {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(
            Self::WORDS
                .iter()
                .find(|(_, policy)| policy == self)
                .map_or("unknown", |(word, _)| word),
        )
    }
}

/// Whether mutators are slowed down while a concurrent cycle runs.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PacingPolicy {
    Off,
    /// Shenandoah-style tax-and-spend pacing, see [`pacer`].
    Tax,
}

impl PacingPolicy {
    fn from_word(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "off" => Some(Self::Off),
            "tax" => Some(Self::Tax),
            _ => None,
        }
    }
}

impl fmt::Display for PacingPolicy {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Off => "off",
            Self::Tax => "tax",
        })
    }
}

/// The three independent policy axes a preset or explicit flags resolve to.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct PolicySet {
    pub(crate) trigger: TriggerPolicy,
    pub(crate) sizing: SizingPolicy,
    pub(crate) pacing: PacingPolicy,
}

impl PolicySet {
    /// The default: Shenandoah-style prediction with free-share heap sizing.
    const PREDICTIVE: Self = Self::new(
        TriggerPolicy::Predictive,
        SizingPolicy::FreeTarget,
        PacingPolicy::Off,
    );

    const fn new(trigger: TriggerPolicy, sizing: SizingPolicy, pacing: PacingPolicy) -> Self {
        Self {
            trigger,
            sizing,
            pacing,
        }
    }

    /// Resolves a `--gc-heuristic` preset.
    fn preset(word: &str) -> Option<Self> {
        use PacingPolicy::{Off, Tax};
        use SizingPolicy::{Balanced, Banded, FreeTarget, Overhead};
        use TriggerPolicy::*;
        Some(match word.to_ascii_lowercase().as_str() {
            "predictive" => Self::new(Predictive, FreeTarget, Off),
            "generational" => Self::new(Generational, FreeTarget, Off),
            "runway" => Self::new(Runway, FreeTarget, Off),
            "goal" => Self::new(Goal, FreeTarget, Off),
            "proportional" => Self::new(Proportional, FreeTarget, Off),
            "floor" => Self::new(Floor, FreeTarget, Off),
            "eager" => Self::new(Eager, FreeTarget, Off),
            "deferred" => Self::new(Deferred, FreeTarget, Off),
            "banded" => Self::new(Deferred, Banded, Off),
            "overhead" => Self::new(Deferred, Overhead, Off),
            "balanced" => Self::new(Deferred, Balanced, Off),
            "concurrent" => Self::new(Goal, Balanced, Tax),
            _ => return None,
        })
    }
}

impl fmt::Display for PolicySet {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}/{}", self.trigger, self.sizing)?;
        if self.pacing != PacingPolicy::Off {
            write!(formatter, "/{}-pacing", self.pacing)?;
        }
        Ok(())
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
    pub(crate) trigger: TriggerPolicy,
    pub(crate) sizing: SizingPolicy,
    pub(crate) pacing: PacingPolicy,
    pub(crate) max_heap_bytes: usize,
    pub(crate) initial_heap_bytes: usize,
    pub(crate) heap_growth_free_percent: usize,
    pub(crate) min_free_threshold_percent: usize,
    pub(crate) init_free_threshold_percent: usize,
    pub(crate) alloc_spike_factor_percent: usize,
    pub(crate) learning_steps: usize,
    pub(crate) predictive_decay_factor: f64,
    pub(crate) predictive_confidence: f64,
    pub(crate) predictive_spike_threshold: f64,
    pub(crate) acceleration_sample_period: Duration,
    pub(crate) acceleration_sample_count: usize,
    pub(crate) momentary_spike_sample_count: usize,
    pub(crate) guaranteed_gc_interval: Option<Duration>,
    pub(crate) minimum_old_time: Duration,
    pub(crate) old_growth_percent: usize,
    pub(crate) old_growth_remaining_heap_percent: usize,
    pub(crate) old_growth_ignore_percent: usize,
    pub(crate) old_growth_ignore_young_cycles: usize,
    pub(crate) proportional_divisor: usize,
    pub(crate) proportional_min_trip: usize,
    /// Zero means half of the heap.
    pub(crate) proportional_max_trip: usize,
    pub(crate) runway_spike_tolerance: f64,
    pub(crate) runway_sample_period: Duration,
    pub(crate) goal_growth_percent: usize,
    pub(crate) pacer: pacer::PacerConfig,
    pub(crate) banded_sizing: sizing::BandedSizing,
    pub(crate) overhead_target: f64,
    pub(crate) overhead_window: usize,
}

impl HeuristicConfig {
    fn defaults_for(policies: PolicySet) -> Self {
        let mut config = Self {
            trigger: policies.trigger,
            sizing: policies.sizing,
            pacing: policies.pacing,
            max_heap_bytes: configured_max_heap_bytes(),
            initial_heap_bytes: usize::MAX,
            heap_growth_free_percent: DEFAULT_HEAP_GROWTH_FREE_PERCENT,
            min_free_threshold_percent: DEFAULT_MIN_FREE_THRESHOLD_PERCENT,
            init_free_threshold_percent: DEFAULT_INIT_FREE_THRESHOLD_PERCENT,
            alloc_spike_factor_percent: DEFAULT_ALLOC_SPIKE_FACTOR_PERCENT,
            learning_steps: DEFAULT_LEARNING_STEPS,
            predictive_decay_factor: DEFAULT_PREDICTIVE_DECAY_FACTOR,
            predictive_confidence: predictive::DEFAULT_CONFIDENCE,
            predictive_spike_threshold: predictive::DEFAULT_SPIKE_THRESHOLD,
            acceleration_sample_period: predictive::DEFAULT_ACCELERATION_SAMPLE_PERIOD,
            acceleration_sample_count: predictive::DEFAULT_ACCELERATION_SAMPLE_COUNT,
            momentary_spike_sample_count: predictive::DEFAULT_MOMENTARY_SPIKE_SAMPLE_COUNT,
            guaranteed_gc_interval: Some(DEFAULT_GUARANTEED_GC_INTERVAL),
            minimum_old_time: generational::MINIMUM_OLD_TIME,
            old_growth_percent: generational::OLD_GROWTH_PERCENT,
            old_growth_remaining_heap_percent: generational::OLD_GROWTH_REMAINING_HEAP_PERCENT,
            old_growth_ignore_percent: generational::OLD_GROWTH_IGNORE_PERCENT,
            old_growth_ignore_young_cycles: generational::OLD_GROWTH_IGNORE_YOUNG_CYCLES,
            proportional_divisor: proportional::DEFAULT_DIVISOR,
            proportional_min_trip: proportional::DEFAULT_MIN_TRIP_BYTES,
            proportional_max_trip: 0,
            runway_spike_tolerance: runway::DEFAULT_SPIKE_TOLERANCE,
            runway_sample_period: runway::DEFAULT_SAMPLE_PERIOD,
            goal_growth_percent: goal::DEFAULT_GROWTH_PERCENT,
            pacer: pacer::PacerConfig {
                max_delay: Duration::from_millis(pacer::DEFAULT_MAX_DELAY_MS as u64),
                cycle_slack_percent: pacer::DEFAULT_CYCLE_SLACK_PERCENT,
                surcharge: pacer::DEFAULT_SURCHARGE,
            },
            banded_sizing: sizing::BandedSizing {
                min_ratio: sizing::DEFAULT_MIN_RATIO,
                goal_ratio: sizing::DEFAULT_GOAL_RATIO,
                max_ratio: sizing::DEFAULT_MAX_RATIO,
            },
            overhead_target: sizing::DEFAULT_TARGET_GC_CPU,
            overhead_window: sizing::DEFAULT_CPU_WINDOW,
        };

        if policies.trigger == TriggerPolicy::Proportional {
            config.guaranteed_gc_interval = Some(proportional::GUARANTEED_GC_INTERVAL);
        }

        config
    }

    pub(crate) fn policies(&self) -> PolicySet {
        PolicySet::new(self.trigger, self.sizing, self.pacing)
    }

    /// Resolves the preset (`--gc-heuristic`) and then any explicit per-axis flags.
    fn resolve_policies() -> PolicySet {
        let mut policies =
            PolicySet::preset(flags::gc_heuristic()).unwrap_or(PolicySet::PREDICTIVE);
        if flags::is_set(FlagId::gc_trigger_policy)
            && let Some(trigger) = TriggerPolicy::from_word(flags::gc_trigger_policy())
        {
            policies.trigger = trigger;
        }
        if flags::is_set(FlagId::gc_sizing)
            && let Some(sizing) = SizingPolicy::from_word(flags::gc_sizing())
        {
            policies.sizing = sizing;
        }
        if flags::is_set(FlagId::gc_pacing)
            && let Some(pacing) = PacingPolicy::from_word(flags::gc_pacing())
        {
            policies.pacing = pacing;
        }
        if policies.trigger == TriggerPolicy::Generational && !plan_is_generational() {
            log::warn!(
                target: GC_LOG_TARGET,
                "generational trigger policy needs the GenImmix, GenCopy or StickyImmix plan; using predictive"
            );
            policies.trigger = TriggerPolicy::Predictive;
        }
        policies
    }

    pub(crate) fn from_env() -> Self {
        let mut config = Self::defaults_for(Self::resolve_policies());

        if flags::is_set(FlagId::gc_max_heap) {
            config.max_heap_bytes = flags::gc_max_heap();
        }
        config.initial_heap_bytes = if flags::is_set(FlagId::gc_init_heap) {
            // An explicit zero still means "fixed at max"; the policy clamps to max.
            match flags::gc_init_heap() {
                0 => usize::MAX,
                initial => initial,
            }
        } else {
            auto_initial_heap_bytes(config.max_heap_bytes)
        };
        if flags::is_set(FlagId::gc_heap_growth_free_percent) {
            config.heap_growth_free_percent = flags::gc_heap_growth_free_percent();
        }
        if flags::is_set(FlagId::gc_min_free_percent) {
            config.min_free_threshold_percent = flags::gc_min_free_percent();
        }
        if flags::is_set(FlagId::gc_init_free_percent) {
            config.init_free_threshold_percent = flags::gc_init_free_percent();
        }
        if flags::is_set(FlagId::gc_alloc_spike_percent) {
            config.alloc_spike_factor_percent = flags::gc_alloc_spike_percent();
        }
        if flags::is_set(FlagId::gc_learning_steps) {
            config.learning_steps = flags::gc_learning_steps();
        }
        if flags::is_set(FlagId::gc_predictive_confidence) {
            config.predictive_confidence = flags::gc_predictive_confidence();
        }
        if flags::is_set(FlagId::gc_predictive_spike_threshold) {
            config.predictive_spike_threshold = flags::gc_predictive_spike_threshold();
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
        if flags::is_set(FlagId::gc_proportional_divisor) {
            config.proportional_divisor = flags::gc_proportional_divisor().max(1);
        }
        if flags::is_set(FlagId::gc_proportional_min_trip) {
            config.proportional_min_trip = flags::gc_proportional_min_trip();
        }
        if flags::is_set(FlagId::gc_proportional_max_trip) {
            config.proportional_max_trip = flags::gc_proportional_max_trip();
        }
        if flags::is_set(FlagId::gc_runway_spike_tolerance) {
            config.runway_spike_tolerance = flags::gc_runway_spike_tolerance().max(1.0);
        }
        if flags::is_set(FlagId::gc_runway_sample_ms) {
            config.runway_sample_period =
                Duration::from_millis(flags::gc_runway_sample_ms().max(1) as u64);
        }
        if flags::is_set(FlagId::gc_goal_growth_percent) {
            config.goal_growth_percent = flags::gc_goal_growth_percent().max(1);
        }
        if flags::is_set(FlagId::gc_pacing_max_delay_ms) {
            config.pacer.max_delay = Duration::from_millis(flags::gc_pacing_max_delay_ms() as u64);
        }
        if flags::is_set(FlagId::gc_pacing_cycle_slack_percent) {
            config.pacer.cycle_slack_percent = flags::gc_pacing_cycle_slack_percent();
        }
        if flags::is_set(FlagId::gc_pacing_surcharge) {
            config.pacer.surcharge = flags::gc_pacing_surcharge().max(1.0);
        }
        if flags::is_set(FlagId::gc_banded_min_ratio) {
            config.banded_sizing.min_ratio = flags::gc_banded_min_ratio();
        }
        if flags::is_set(FlagId::gc_banded_goal_ratio) {
            config.banded_sizing.goal_ratio = flags::gc_banded_goal_ratio();
        }
        if flags::is_set(FlagId::gc_banded_max_ratio) {
            config.banded_sizing.max_ratio = flags::gc_banded_max_ratio();
        }
        if flags::is_set(FlagId::gc_overhead_target) {
            config.overhead_target = flags::gc_overhead_target().clamp(0.001, 0.999);
        }
        if flags::is_set(FlagId::gc_overhead_window) {
            config.overhead_window = flags::gc_overhead_window().max(1);
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
    pub(crate) total_bytes: usize,
    /// Current heap limit, which may sit below `total_bytes` while the heap grows.
    pub(crate) committed_bytes: usize,
    pub(crate) available_bytes: usize,
    pub(crate) free_bytes: usize,
    pub(crate) allocated_since_gc: usize,
    /// Heap in use when the previous GC ended; zero before the first GC.
    pub(crate) live_bytes: usize,
}

impl HeapSnapshot {
    pub(super) fn used_bytes(self) -> usize {
        self.total_bytes.saturating_sub(self.effective_free_bytes())
    }

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
    policies: PolicySet,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) -> String {
    let mut message = format!("GC trigger {decision} ({policies}): {reason}");
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
    policies: PolicySet,
    reason: impl fmt::Display,
    snapshot: Option<HeapSnapshot>,
) {
    {
        log::info!(
            target: GC_LOG_TARGET,
            "{}",
            format_trigger_decision_message(decision, policies, reason, snapshot)
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

/// What the end-of-cycle log line reports.
pub(crate) struct CycleReport {
    policies: PolicySet,
    free_at_start: usize,
    alloc_rate: usize,
    alloc_rate_sd: usize,
    cons_mark: f64,
    penalties: usize,
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
    last_trigger_type: predictive::TriggerType,
    last_cycle_end: Option<Instant>,
    active_cycle: Option<ActiveCycle>,
    time_origin: Instant,
    gc_times_learned: usize,
    gc_time_penalties: usize,
    predictive_confidence: f64,
    predictive_spike_threshold: f64,
    free_at_end_history: DecayingSequence,
    gc_cycle_time_history: DecayingSequence,
    gc_cycle_time_predictor: predictive::LinearPredictor,
    allocation_rate_history: DecayingSequence,
    acceleration_rate_history: predictive::RateSampleHistory,
    last_allocation_sample: Option<(Instant, usize)>,
    last_acceleration_sample: Option<Instant>,
    generational: generational::GenerationalState,
    runway: runway::RunwayState,
    goal: goal::GoalState,
    /// Free bytes when the current cycle started, for the cycle report.
    cycle_free_at_start: usize,
}

impl HeuristicState {
    pub(crate) fn new(config: HeuristicConfig) -> Self {
        let now = Instant::now();
        let acceleration_capacity = config
            .acceleration_sample_count
            .max(config.momentary_spike_sample_count.saturating_add(1));
        Self {
            gc_cycle_time_history: DecayingSequence::new(config.predictive_decay_factor),
            gc_cycle_time_predictor: predictive::LinearPredictor::new(3),
            allocation_rate_history: DecayingSequence::new(config.predictive_decay_factor),
            acceleration_rate_history: predictive::RateSampleHistory::new(acceleration_capacity),
            free_at_end_history: DecayingSequence::new(config.predictive_decay_factor),
            predictive_confidence: config.predictive_confidence,
            predictive_spike_threshold: config.predictive_spike_threshold,
            generational: generational::GenerationalState::new(),
            runway: runway::RunwayState::new(),
            goal: goal::GoalState::default(),
            cycle_free_at_start: 0,
            config,
            start_gc_is_pending: false,
            declined_trigger_count: 0,
            most_recent_declined_trigger_count: 0,
            last_trigger_type: predictive::TriggerType::Other,
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

        match self.config.trigger {
            TriggerPolicy::Eager => self.eager_should_start(snapshot),
            TriggerPolicy::Deferred => {
                // The MMTk plan fallback at the call site can still force GC
                // for allocation failure or other plan-specific requirements.
                self.decline_trigger();
                false
            }
            TriggerPolicy::Floor => self.floor_should_start(snapshot, now),
            TriggerPolicy::Proportional => self.proportional_should_start(snapshot, now),
            TriggerPolicy::Runway => self.runway_should_start(snapshot, now),
            TriggerPolicy::Goal => self.goal_should_start(snapshot, now),
            TriggerPolicy::Predictive => {
                self.predictive_should_start(snapshot, now, planned_sleep_interval)
            }
            TriggerPolicy::Generational => {
                self.generational_should_start(snapshot, now, planned_sleep_interval)
            }
        }
    }

    pub(crate) fn record_cycle_start(&mut self, now: Instant, free_at_start: usize) {
        self.start_gc_is_pending = false;
        if let Some(active_cycle) = self.active_cycle.as_mut() {
            active_cycle.depth = active_cycle.depth.saturating_add(1);
        } else {
            let kind = self.generational.pending_cycle_kind.take();
            self.cycle_free_at_start = free_at_start;
            if self.config.trigger == TriggerPolicy::Generational
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
        self.runway.reset_baseline();
        if self.config.trigger == TriggerPolicy::Generational {
            if let Some(snapshot) = snapshot {
                self.record_generational_cycle_end(cycle_kind, snapshot);
            }
        }
        if let Some(snapshot) = snapshot {
            // Post-GC free memory tells adaptive mode whether the previous
            // trigger was too early or too late.
            self.adjust_predictive_trigger_parameters(snapshot.effective_free_bytes() as f64);
        }
        Some(cycle_time)
    }

    /// Figures for the end-of-cycle log line.
    pub(crate) fn cycle_report(&self) -> CycleReport {
        CycleReport {
            policies: self.config.policies(),
            free_at_start: self.cycle_free_at_start,
            alloc_rate: rate_to_size(self.allocation_rate_history.average()),
            alloc_rate_sd: rate_to_size(self.allocation_rate_history.standard_deviation()),
            cons_mark: self.goal.cons_mark(),
            penalties: self.gc_time_penalties,
        }
    }

    /// Concurrent marking begins: remember the occupancy to measure cons/mark from.
    pub(crate) fn on_initial_mark(&mut self, used_bytes: usize, live_bytes: usize) {
        self.goal.record_initial_mark(used_bytes, live_bytes);
    }

    /// Concurrent marking is over; returns the measured cons/mark.
    pub(crate) fn on_final_mark(&mut self, used_bytes: usize) -> Option<f64> {
        self.goal.record_final_mark(used_bytes)
    }

    /// A pause that marks with the world stopped.
    pub(crate) fn on_stw_pause(&mut self) {
        self.goal.record_stw_cycle();
    }

    /// The pacer ran out of allowance and allocation hit a full space.  Make the
    /// next cycles start earlier, like Shenandoah does after a degenerated GC.
    pub(crate) fn record_pacing_failure(&mut self) {
        self.adjust_penalty(PACING_FAILURE_PENALTY_PERCENT);
    }

    /// Duration the pacer should expect the current concurrent cycle to take.
    pub(crate) fn expected_cycle_time(&self) -> Duration {
        if self.gc_times_learned == 0 {
            pacer::BOOTSTRAP_EXPECTED_CYCLE
        } else {
            Duration::from_secs_f64(self.gc_cycle_time_history.upper_bound(1.0).max(0.0))
        }
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

    fn floor_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        // Floor is the baseline: trigger only when free memory crosses the
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
        self.last_trigger_type = predictive::TriggerType::Other;
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
        log_trigger_decision(decision, self.config.policies(), reason, snapshot);
    }

    pub(super) fn adjust_penalty(&mut self, step: isize) {
        self.gc_time_penalties = self.gc_time_penalties.saturating_add_signed(step).min(100);
    }
}

pub(crate) struct CapyTriggerPolicy {
    state: Mutex<HeuristicState>,
    trigger: TriggerPolicy,
    pacing: PacingPolicy,
    pacer_config: pacer::PacerConfig,
    min_free_percent: usize,
    max_heap_pages: usize,
    initial_heap_pages: usize,
    current_heap_pages: AtomicUsize,
    shrink_streak: AtomicUsize,
    pending_pages: AtomicUsize,
    used_at_last_gc_end: AtomicUsize,
    sizing: Mutex<SizingState>,
}

/// Per-policy heap sizing state.
enum SizingState {
    /// Resize from post-GC used pages in `on_gc_end`.
    FreeTarget,
    Banded(sizing::BandedSizing),
    Overhead(sizing::OverheadSizing),
    Balanced(sizing::BalancedSizing),
}

/// Whether the trigger policy must hand a poll to the MMTk plan.
///
/// A policy that owns the InitialMark decision keeps the plan's own allocation
/// threshold out of it while no concurrent cycle runs.  Full spaces, a full
/// heap and an in-flight concurrent cycle (whose FinalMark the plan schedules)
/// always go to the plan.
fn delegate_to_plan(
    owns_initial_mark: bool,
    concurrent_plan: bool,
    space_full: bool,
    marking: bool,
    heap_full: bool,
) -> bool {
    !(owns_initial_mark && concurrent_plan && !space_full && !marking && !heap_full)
}

/// Smallest `balanced` heap target for `live_pages`.
///
/// MemBalancer can ask for a heap barely above the live set when collection is
/// fast relative to allocation.  The trigger policies fire whenever free space
/// drops under the minimum-free share, so a heap that leaves less than twice
/// that share free would be collected again right after every GC.
fn balanced_floor_pages(live_pages: usize, min_free_percent: usize) -> usize {
    let free_percent = (2 * min_free_percent).min(90);
    live_pages.saturating_mul(100) / (100 - free_percent)
}

/// The concurrent pause MMTk is running, if the plan is concurrent.
fn current_pause(mmtk: &'static MMTK<MemoryManager>) -> Option<Pause> {
    mmtk.get_plan()
        .concurrent()
        .and_then(|plan| plan.current_pause())
}

impl CapyTriggerPolicy {
    pub(crate) fn new() -> Self {
        Self::with_config(HeuristicConfig::from_env())
    }

    fn with_config(config: HeuristicConfig) -> Self {
        let max_heap_pages = bytes_to_pages_up(config.max_heap_bytes);
        let initial_heap_pages =
            bytes_to_pages_up(config.initial_heap_bytes.min(config.max_heap_bytes))
                .min(max_heap_pages)
                .max(1);
        let sizing = match config.sizing {
            SizingPolicy::FreeTarget => SizingState::FreeTarget,
            SizingPolicy::Banded => SizingState::Banded(config.banded_sizing),
            SizingPolicy::Overhead => SizingState::Overhead(sizing::OverheadSizing::new(
                config.overhead_target,
                config.overhead_window,
            )),
            SizingPolicy::Balanced => SizingState::Balanced(sizing::BalancedSizing::new()),
        };
        // Never plan for more than half of the machine's memory.
        let max_heap_pages = match (&sizing, sysmem::system_memory()) {
            (SizingState::FreeTarget, _) | (_, None) => max_heap_pages,
            (_, Some(memory)) => max_heap_pages
                .min(bytes_to_pages_up(memory.total / 2))
                .max(initial_heap_pages),
        };
        Self {
            sizing: Mutex::new(sizing),
            trigger: config.trigger,
            pacing: config.pacing,
            pacer_config: config.pacer,
            min_free_percent: config.min_free_threshold_percent,
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

    /// Builds the heuristic view of the heap against the *current* limit.
    ///
    /// MMTk's free/available counts are relative to the max heap it was built
    /// with, so using them directly makes a small heap look nearly empty.  The
    /// limit is what the heuristics must pace against, so free space is derived
    /// from it and the reserved bytes instead.
    fn snapshot_with(&self, reserved_bytes: usize, allocated_since_gc: usize) -> HeapSnapshot {
        let committed_bytes = self.current_heap_bytes();
        let free_bytes = committed_bytes.saturating_sub(reserved_bytes);
        HeapSnapshot {
            total_bytes: committed_bytes,
            committed_bytes,
            available_bytes: free_bytes,
            free_bytes,
            allocated_since_gc,
            live_bytes: self.used_at_last_gc_end.load(Ordering::Relaxed),
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

    fn lock_sizing(&self) -> std::sync::MutexGuard<'_, SizingState> {
        self.sizing
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn store_heap_pages(&self, current: usize, next: usize, used_pages: usize) {
        if next == current {
            return;
        }
        self.current_heap_pages.store(next, Ordering::Relaxed);
        log::info!(
            target: GC_LOG_TARGET,
            "heap resized {} -> {} (live={}, max={})",
            FormattedSize(pages_to_bytes(current)),
            FormattedSize(pages_to_bytes(next)),
            FormattedSize(pages_to_bytes(used_pages)),
            FormattedSize(pages_to_bytes(self.max_heap_pages)),
        );
    }

    /// Banded/overhead sizing: retarget the heap after full-heap pauses only, so
    /// nursery survivors never inflate it.
    fn resize_plan_driven(&self, mmtk: &'static MMTK<MemoryManager>) {
        let mut sizing = self.lock_sizing();
        if matches!(*sizing, SizingState::FreeTarget | SizingState::Balanced(_)) {
            return;
        }
        let pending = self.pending_pages.swap(0, Ordering::Relaxed);
        let plan = mmtk.get_plan();
        if pending == 0
            && plan
                .generational()
                .is_some_and(|plan| plan.is_current_gc_nursery())
        {
            return;
        }
        let used = plan.get_used_pages();
        let current = self.current_heap_pages.load(Ordering::Relaxed);
        let min = self.initial_heap_pages;
        let max = self.max_heap_pages;
        let next = match &mut *sizing {
            SizingState::FreeTarget | SizingState::Balanced(_) => None,
            SizingState::Banded(banded) => banded.retarget(used, current, pending, min, max),
            SizingState::Overhead(overhead) => {
                overhead.pause_end(used, current, min, max).map(|step| {
                    log::info!(
                        target: GC_LOG_TARGET,
                        "overhead sizing: gc_cpu={:.4} factor={:.3}",
                        step.gc_cpu,
                        step.factor,
                    );
                    step.new_pages
                })
            }
        };
        if let Some(next) = next {
            self.store_heap_pages(current, next, used);
        }
    }

    /// Balanced sizing: compute the new target once a collection has finished.
    fn resize_balanced(&self, mmtk: &'static MMTK<MemoryManager>, now: Instant) {
        let mut sizing = self.lock_sizing();
        let SizingState::Balanced(balanced) = &mut *sizing else {
            return;
        };
        let plan = mmtk.get_plan();
        let (live, generational, nursery) = match plan.generational() {
            Some(plan) => (
                plan.get_mature_reserved_pages(),
                true,
                plan.is_current_gc_nursery(),
            ),
            None => (plan.get_reserved_pages(), false, false),
        };
        // Pending allocation is accounted for by this GC whether or not it resizes.
        let pending = self.pending_pages.swap(0, Ordering::Relaxed);
        if !balanced.gc_end(now, live, generational, nursery) {
            return;
        }
        let current = self.current_heap_pages.load(Ordering::Relaxed);
        let reserved = plan.get_reserved_pages();
        let next = balanced.retarget(
            reserved,
            plan.get_collection_reserved_pages(),
            pending,
            self.initial_heap_pages
                .max(balanced_floor_pages(reserved, self.min_free_percent)),
            self.max_heap_pages,
        );
        self.store_heap_pages(current, next, plan.get_used_pages());
    }

    fn lock_state(&self) -> std::sync::MutexGuard<'_, HeuristicState> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Stops pacing and logs what it cost.
    fn disarm_pacer(&self) {
        if let Some(summary) = pacer::PACER.disarm() {
            log::info!(
                target: GC_LOG_TARGET,
                "pacing ended: {} stalls, {:?} total stall time",
                summary.stalls,
                summary.stalled,
            );
        }
    }
}

impl GCTriggerPolicy<MemoryManager> for CapyTriggerPolicy {
    fn on_pending_allocation(&self, pages: usize) {
        self.pending_pages.fetch_add(pages, Ordering::Relaxed);
    }

    fn on_gc_start(&self, mmtk: &'static MMTK<MemoryManager>) {
        let now = Instant::now();
        let plan = mmtk.get_plan();
        match &mut *self.lock_sizing() {
            SizingState::FreeTarget => self.pending_pages.store(0, Ordering::Relaxed),
            SizingState::Balanced(balanced) => {
                balanced.gc_start(
                    now,
                    plan.get_reserved_pages(),
                    plan.generational().is_some(),
                );
            }
            SizingState::Banded(_) | SizingState::Overhead(_) => {}
        }
        {
            let mut global_stats = crate::runtime::GLOBAL_STATS.lock();
            global_stats.start_gc_at(now);
        }
        let free_at_start = self
            .snapshot_with(pages_to_bytes(plan.get_reserved_pages()), 0)
            .effective_free_bytes();
        self.lock_state().record_cycle_start(now, free_at_start);
    }

    fn on_pause_start(&self, mmtk: &'static MMTK<MemoryManager>) {
        if let SizingState::Overhead(overhead) = &mut *self.lock_sizing() {
            overhead.pause_start();
        }
        let used_bytes = pages_to_bytes(mmtk.get_plan().get_used_pages());
        match current_pause(mmtk) {
            Some(Pause::InitialMark) => {
                let live = self.used_at_last_gc_end.load(Ordering::Relaxed);
                self.lock_state().on_initial_mark(used_bytes, live);
            }
            Some(Pause::FinalMark) => {
                self.disarm_pacer();
                if let Some(cons_mark) = self.lock_state().on_final_mark(used_bytes) {
                    log::info!(
                        target: GC_LOG_TARGET,
                        "concurrent mark finished: cons/mark={cons_mark:.3}"
                    );
                }
            }
            _ => {
                self.disarm_pacer();
                self.lock_state().on_stw_pause();
            }
        }
        crate::runtime::gc_stats::mark_pause_start(
            crate::runtime::vm::base::real_time_ms(),
            crate::runtime::vm::base::cpu_time_ms(),
        );
    }

    fn on_pause_end(&self, mmtk: &'static MMTK<MemoryManager>) {
        self.resize_plan_driven(mmtk);
        if self.pacing == PacingPolicy::Tax && current_pause(mmtk) == Some(Pause::InitialMark) {
            // Concurrent marking starts now; tax allocation until FinalMark.
            let plan = mmtk.get_plan();
            let free = self
                .snapshot_with(pages_to_bytes(plan.get_reserved_pages()), 0)
                .effective_free_bytes();
            let expected = self.lock_state().expected_cycle_time();
            pacer::PACER.arm(free, expected, self.pacer_config);
            log::info!(
                target: GC_LOG_TARGET,
                "pacing armed: free={}, expected_cycle={:?}",
                FormattedSize(free),
                expected,
            );
        }
        crate::runtime::gc_stats::mark_pause_end(
            crate::runtime::vm::base::real_time_ms(),
            crate::runtime::vm::base::cpu_time_ms(),
        );
    }

    fn on_gc_release(&self, mmtk: &'static MMTK<MemoryManager>) {
        if let SizingState::Balanced(balanced) = &mut *self.lock_sizing() {
            let plan = mmtk.get_plan();
            match plan.generational() {
                Some(plan) => balanced.gc_release(
                    plan.get_mature_reserved_pages(),
                    true,
                    plan.is_current_gc_nursery(),
                ),
                None => balanced.gc_release(plan.get_reserved_pages(), false, false),
            }
        }
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
        let free_target = matches!(*self.lock_sizing(), SizingState::FreeTarget);
        if let Some((old, new)) = free_target
            .then(|| self.resize_after_gc(plan.get_used_pages(), growth_free, min_free))
            .flatten()
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
        self.resize_balanced(mmtk, now);
        // Built after the resize so available/free reflect the new size.
        let snapshot = self.snapshot_with(pages_to_bytes(plan.get_reserved_pages()), 0);

        // Feed post-GC plan accounting back into the heuristic.  Predictive
        // uses this to adjust how early rate/spike triggers should fire.
        let (cycle_time, report) = {
            let mut state = self.lock_state();
            let cycle_time = state
                .active_cycle
                .as_ref()
                .map(|cycle| now.saturating_duration_since(cycle.started_at))
                .unwrap_or_default();
            let cycle_time = state.record_cycle_end_with_duration(now, cycle_time, Some(snapshot));
            let report = cycle_time.map(|_| state.cycle_report());
            (cycle_time, report)
        };

        if let (Some(cycle_time), Some(report)) = (cycle_time, report) {
            log::info!(
                target: GC_LOG_TARGET,
                "GC cycle finished ({}): cycle_time={:?}, free_at_start={}, live={}, heap={}, \
                 alloc_rate={}/s (sd {}/s), cons_mark={:.3}, penalties={}%",
                report.policies,
                cycle_time,
                FormattedSize(report.free_at_start),
                FormattedSize(used_bytes),
                FormattedSize(self.current_heap_bytes()),
                FormattedSize(report.alloc_rate),
                FormattedSize(report.alloc_rate_sd),
                report.cons_mark,
                report.penalties,
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
        let reserved_bytes =
            pages_to_bytes(plan.get_reserved_pages()).saturating_add(pending_bytes);
        let allocated_since_gc =
            used_bytes.saturating_sub(self.used_at_last_gc_end.load(Ordering::Relaxed));
        let snapshot = self.snapshot_with(reserved_bytes, allocated_since_gc);

        if self
            .lock_state()
            .should_start_gc(snapshot, Instant::now(), Duration::ZERO)
        {
            return true;
        }

        // Allocation hit a full space while the pacer was holding mutators back:
        // pacing was not enough, so start earlier next time.
        if space_full && pacer::PACER.note_exhaustion() {
            self.lock_state().record_pacing_failure();
        }

        let concurrent = plan.concurrent();
        let marking = concurrent.is_some_and(|plan| plan.concurrent_work_in_progress());
        if delegate_to_plan(
            self.trigger.owns_initial_mark(),
            concurrent.is_some(),
            space_full,
            marking,
            self.is_heap_full(plan),
        ) {
            plan.collection_required(space_full, space)
        } else {
            false
        }
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

/// Starting heap used when `--gc-init-heap` is not given.
///
/// A slice of the memory the machine has free right now, bounded to a quarter
/// of max so the heap still has room to grow.  Falls back to a quarter of max
/// when the machine figures cannot be read.
fn auto_initial_heap_bytes(max: usize) -> usize {
    match sysmem::system_memory() {
        Some(memory) => initial_heap_from_available(memory.available, max),
        None => max / 4,
    }
}

/// Pure sizing rule behind [`auto_initial_heap_bytes`].
fn initial_heap_from_available(available: usize, max: usize) -> usize {
    let floor = AUTO_INIT_HEAP_MIN_BYTES.min(max);
    (available / AUTO_INIT_HEAP_AVAILABLE_DIVISOR).clamp(floor, (max / 4).max(floor))
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

/// Whether the configured MMTk plan is one of the generational ones.
fn plan_is_generational() -> bool {
    flags::gc_plan().is_some_and(|plan| {
        ["GenImmix", "GenCopy", "StickyImmix"]
            .iter()
            .any(|name| plan.eq_ignore_ascii_case(name))
    })
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
    use super::*;

    const MIB: usize = 1024 * 1024;

    #[test]
    fn balanced_floor_keeps_twice_the_minimum_free_share() {
        // 10% minimum free -> at least 20% of the heap free -> heap >= live / 0.8.
        assert_eq!(balanced_floor_pages(800, 10), 1000);
        assert_eq!(balanced_floor_pages(0, 10), 0);
        // A huge minimum-free setting cannot make the floor unbounded.
        assert_eq!(balanced_floor_pages(100, 100), 1000);
    }

    #[test]
    fn presets_resolve_to_trigger_sizing_and_pacing() {
        use PacingPolicy::{Off, Tax};
        use SizingPolicy::{Balanced, Banded, FreeTarget, Overhead};
        use TriggerPolicy::{Deferred, Goal, Predictive, Runway};

        let preset = |word| PolicySet::preset(word).expect(word);
        assert_eq!(preset("predictive"), PolicySet::PREDICTIVE);
        assert_eq!(preset("PREDICTIVE"), PolicySet::PREDICTIVE);
        assert_eq!(preset("runway"), PolicySet::new(Runway, FreeTarget, Off));
        assert_eq!(preset("banded"), PolicySet::new(Deferred, Banded, Off));
        assert_eq!(preset("overhead"), PolicySet::new(Deferred, Overhead, Off));
        assert_eq!(preset("balanced"), PolicySet::new(Deferred, Balanced, Off));
        assert_eq!(preset("concurrent"), PolicySet::new(Goal, Balanced, Tax));
        assert_eq!(preset("goal").trigger, Goal);
        assert_eq!(PolicySet::preset("adaptive"), None);
        assert_eq!(PolicySet::preset("ruby"), None);
        assert_ne!(preset("generational").trigger, Predictive);
    }

    #[test]
    fn policy_names_round_trip_and_render_kebab_case() {
        for (word, policy) in TriggerPolicy::WORDS {
            assert_eq!(TriggerPolicy::from_word(word), Some(*policy));
            assert_eq!(&policy.to_string(), word);
        }
        for (word, policy) in SizingPolicy::WORDS {
            assert_eq!(SizingPolicy::from_word(word), Some(*policy));
            assert_eq!(&policy.to_string(), word);
        }
        assert_eq!(PacingPolicy::from_word("tax"), Some(PacingPolicy::Tax));
        assert_eq!(PacingPolicy::from_word("nope"), None);
        assert_eq!(
            PolicySet::preset("concurrent").unwrap().to_string(),
            "goal/balanced/tax-pacing"
        );
        assert_eq!(PolicySet::PREDICTIVE.to_string(), "predictive/free-target");
    }

    #[test]
    fn only_runway_goal_and_proportional_own_initial_mark() {
        for (_, policy) in TriggerPolicy::WORDS {
            let owns = matches!(
                policy,
                TriggerPolicy::Runway | TriggerPolicy::Goal | TriggerPolicy::Proportional
            );
            assert_eq!(policy.owns_initial_mark(), owns, "{policy}");
        }
    }

    #[test]
    fn plan_is_consulted_unless_an_owning_policy_has_nothing_pressing() {
        // owns, concurrent, space_full, marking, heap_full
        assert!(!delegate_to_plan(true, true, false, false, false));
        // Anything pressing goes to the plan.
        assert!(delegate_to_plan(true, true, true, false, false));
        assert!(delegate_to_plan(true, true, false, true, false));
        assert!(delegate_to_plan(true, true, false, false, true));
        // Policies that do not own InitialMark, and stop-the-world plans, never skip the plan.
        assert!(delegate_to_plan(false, true, false, false, false));
        assert!(delegate_to_plan(true, false, false, false, false));
    }

    #[test]
    fn auto_initial_heap_follows_available_memory_within_bounds() {
        let max = 2048 * MIB;
        // 1/32 of 3.2 GiB free lands between the floor and max/4.
        assert_eq!(initial_heap_from_available(3200 * MIB, max), 100 * MIB);
        // Almost nothing free still starts at the floor.
        assert_eq!(initial_heap_from_available(MIB, max), 32 * MIB);
        // Plenty free is capped at a quarter of max.
        assert_eq!(initial_heap_from_available(64 * 1024 * MIB, max), 512 * MIB);
    }

    #[test]
    fn auto_initial_heap_never_exceeds_small_max() {
        let max = 16 * MIB;
        assert_eq!(initial_heap_from_available(0, max), 16 * MIB);
        assert_eq!(initial_heap_from_available(usize::MAX, max), 16 * MIB);
    }

    #[test]
    fn growth_steps_toward_live_target_and_shrinks_with_hysteresis() {
        let max = 4096;
        // Live 100 at 75% free means a 400-page target, so growth is immediate.
        let (next, streak) = next_heap_pages(100, 128, 64, max, 75, 10, 0);
        assert_eq!((next, streak), (400, 0));

        // Target under half of current counts toward shrink only after the streak.
        let (next, streak) = next_heap_pages(10, 1000, 64, max, 75, 10, 0);
        assert_eq!((next, streak), (1000, 1));
        // Third consecutive low cycle shrinks; the target 40 is clamped up to the initial 64.
        let (next, streak) = next_heap_pages(10, 1000, 64, max, 75, 10, 2);
        assert_eq!((next, streak), (64, 0));
    }

    #[test]
    fn growth_never_leaves_initial_and_max_bounds() {
        // Shrink target 4 pages is clamped up to the initial size.
        let (next, _) = next_heap_pages(1, 1000, 64, 4096, 75, 10, 2);
        assert_eq!(next, 64);
        // Growth past max is clamped to max.
        let (next, _) = next_heap_pages(3000, 1000, 64, 4096, 75, 10, 0);
        assert_eq!(next, 4096);
    }
}
