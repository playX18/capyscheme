//! Generational GC trigger heuristics, modelled on Shenandoah's generational
//! mode and meant for MMTk's `GenImmix`, `GenCopy` and `StickyImmix` plans.

use std::time::{Duration, Instant};

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState, percent_of};

/// Time an old cycle gets before a young cycle may interrupt
pub(super) const MINIMUM_OLD_TIME: Duration = Duration::from_millis(100);
/// Old usage may grow this percent past its live size at the last old mark
/// before an old cycle starts
pub(super) const OLD_GROWTH_PERCENT: usize = 50;
/// ... or into this percent of the capacity not live at the last old mark,
/// whichever bound is lower
pub(super) const OLD_GROWTH_REMAINING_HEAP_PERCENT: usize = 35;
/// Old usage below this percent of capacity is ignored while young cycles
/// are still recent
pub(super) const OLD_GROWTH_IGNORE_PERCENT: usize = 40;
/// Consecutive young cycles after which old growth is never ignored
pub(super) const OLD_GROWTH_IGNORE_YOUNG_CYCLES: usize = 100;

/// Which generational cycle the pending/active GC runs.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum GenerationalCycleKind {
    Young,
    Old,
}

/// Scheduling state for generational mode.
#[derive(Clone, Copy, Debug)]
pub(super) struct GenerationalState {
    pub(super) consecutive_young_cycles: usize,
    pub(super) old_cycle_in_progress: bool,
    pub(super) old_cycle_started_at: Option<Instant>,
    /// Heap occupancy at the end of the last old cycle.
    pub(super) live_at_last_old_mark: Option<usize>,
    /// Set once per cycle end, read by the poll path
    pub(super) growth_trigger: bool,
    pub(super) pending_cycle_kind: Option<GenerationalCycleKind>,
}

impl GenerationalState {
    pub(super) fn new() -> Self {
        Self {
            consecutive_young_cycles: 0,
            old_cycle_in_progress: false,
            old_cycle_started_at: None,
            live_at_last_old_mark: None,
            growth_trigger: false,
            pending_cycle_kind: None,
        }
    }
}

impl HeuristicState {
    pub(super) fn generational_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        // The old generation gets first claim on a trigger, then young.
        if self.generational_old_should_start(snapshot) {
            return true;
        }
        self.generational_young_should_start(snapshot, now, planned_sleep_interval)
    }

    /// Old trigger: expansion failure, then the growth flag set at cycle end.
    fn generational_old_should_start(&mut self, snapshot: HeapSnapshot) -> bool {
        if self.generational.old_cycle_in_progress {
            return false;
        }

        // Full heap that cannot grow further.
        if snapshot.total_bytes > 0
            && snapshot.effective_free_bytes() == 0
            && snapshot.committed_bytes >= snapshot.total_bytes
        {
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Old);
            self.accept_trigger(
                format_args!("old generation cannot expand (no free heap)"),
                Some(snapshot),
            );
            return true;
        }

        if !self.generational.growth_trigger {
            return false;
        }

        // The flag can be stale by the time it is polled, so recheck it.
        let current = snapshot.used_bytes();
        let Some(live) = self.generational.live_at_last_old_mark else {
            self.generational.growth_trigger = false;
            return false;
        };
        let threshold = self.old_usage_trigger_threshold(live, snapshot.total_bytes);
        let ignore_threshold =
            percent_of(snapshot.total_bytes, self.config.old_growth_ignore_percent);
        if current < ignore_threshold
            && self.generational.consecutive_young_cycles
                < self.config.old_growth_ignore_young_cycles
        {
            self.generational.growth_trigger = false;
            return false;
        }
        if current > threshold {
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Old);
            self.accept_trigger(
                format_args!(
                    "old generation overgrown (used={}, live_at_mark={}, threshold={})",
                    FormattedSize(current),
                    FormattedSize(live),
                    FormattedSize(threshold),
                ),
                Some(snapshot),
            );
            return true;
        }
        self.generational.growth_trigger = false;
        false
    }

    /// the lower of relative growth over the last mark and growth into a share of the remaining heap.
    fn old_usage_trigger_threshold(&self, live: usize, capacity: usize) -> usize {
        let by_growth = live.saturating_add(percent_of(live, self.config.old_growth_percent));
        if live >= capacity {
            return capacity;
        }
        let by_remaining = live.saturating_add(percent_of(
            capacity - live,
            self.config.old_growth_remaining_heap_percent,
        ));
        by_growth.min(by_remaining)
    }

    /// Young trigger: yield to a fresh old cycle, then run the adaptive triggers.
    fn generational_young_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        // Let a fresh old cycle run before a young cycle may start.
        if self.generational.old_cycle_in_progress
            && let Some(started) = self.generational.old_cycle_started_at
            && now.saturating_duration_since(started) < self.config.minimum_old_time
        {
            return false;
        }

        if self.adaptive_should_start(snapshot, now, planned_sleep_interval) {
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Young);
            return true;
        }
        false
    }

    pub(super) fn record_generational_cycle_end(
        &mut self,
        kind: Option<GenerationalCycleKind>,
        snapshot: HeapSnapshot,
    ) {
        let used = snapshot.used_bytes();
        match kind {
            Some(GenerationalCycleKind::Old) => {
                self.generational.live_at_last_old_mark = Some(used);
                self.generational.consecutive_young_cycles = 0;
                self.generational.old_cycle_in_progress = false;
                self.generational.old_cycle_started_at = None;
                self.generational.growth_trigger = false;
            }
            _ => {
                self.generational.consecutive_young_cycles =
                    self.generational.consecutive_young_cycles.saturating_add(1);
                // Evaluate growth once here, not on every poll.
                if let Some(live) = self.generational.live_at_last_old_mark {
                    let threshold = self.old_usage_trigger_threshold(live, snapshot.total_bytes);
                    self.generational.growth_trigger = used > threshold;
                }
            }
        }
    }
}
