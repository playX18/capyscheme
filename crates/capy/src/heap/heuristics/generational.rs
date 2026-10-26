//! Generational GC trigger heuristics, based on Shenandoah.

use std::time::{Duration, Instant};

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState, percent_of};

/// Percent of the heap whose post-GC allocation expedites a young cycle to
/// promote (default 5).
pub(super) const EXPEDITE_PROMOTION_THRESHOLD_PERCENT: usize = 5;
/// Deferred old-growth observations that expedite a mixed (young) cycle (default 10 regions).
pub(super) const EXPEDITE_MIXED_THRESHOLD: usize = 10;
/// Garbage share an old region must hold to be worth collecting; lowered (default 25).
pub(super) const OLD_GARBAGE_THRESHOLD_PERCENT: usize = 25;
/// Floor for the dynamic old garbage threshold.
pub(super) const MINIMUM_OLD_GARBAGE_THRESHOLD_PERCENT: usize = 10;
/// Time an old cycle gets before a young cycle may interrupt.
pub(super) const MINIMUM_OLD_TIME: Duration = Duration::from_millis(100);
/// Old usage growth allowance as a percent of the non-live heap.
pub(super) const OLD_GROWTH_ALLOWANCE_PERCENT: usize = 35;
/// Old usage below this percent of the heap is ignored while young cycles
/// are still recent (default 40).
pub(super) const OLD_GROWTH_IGNORE_PERCENT: usize = 40;
/// Consecutive young cycles after which old growth is never ignored (default 100).
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
    pub(super) live_at_last_old_mark: Option<usize>,
    pub(super) old_used_estimate: Option<usize>,
    pub(super) old_garbage_threshold_percent: usize,
    pub(super) unprocessed_old_candidates: usize,
    pub(super) pending_cycle_kind: Option<GenerationalCycleKind>,
}

impl GenerationalState {
    pub(super) fn new(old_garbage_threshold_percent: usize) -> Self {
        Self {
            consecutive_young_cycles: 0,
            old_cycle_in_progress: false,
            old_cycle_started_at: None,
            live_at_last_old_mark: None,
            old_used_estimate: None,
            old_garbage_threshold_percent,
            unprocessed_old_candidates: 0,
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
        if self.generational_old_should_start(snapshot, now) {
            return true;
        }
        self.generational_young_should_start(snapshot, now, planned_sleep_interval)
    }

    /// Old trigger: idle gate, cannot-expand, fragmentation, overgrowth.
    fn generational_old_should_start(&mut self, snapshot: HeapSnapshot, now: Instant) -> bool {
        let _ = now;
        if self.generational.old_cycle_in_progress {
            return false;
        }

        // Full heap that cannot grow further.
        if snapshot.total_bytes > 0
            && snapshot.effective_free_bytes() == 0
            && snapshot.total_bytes >= snapshot.max_bytes
        {
            self.generational_accept_old(
                format_args!("old generation cannot expand (no free heap)"),
                snapshot,
            );
            return true;
        }

        // Fragmentation, using byte estimates in place of region span/density.
        if let (Some(live), Some(span)) = (
            self.generational.live_at_last_old_mark,
            self.generational.old_used_estimate,
        ) {
            let total = snapshot.total_bytes;
            if total > 0 && span > 0 {
                let span_fraction = span as f64 / total as f64;
                if span_fraction > 0.50 {
                    let density = (live.min(span)) as f64 / span as f64;
                    let bar = 1.0 - self.generational.old_garbage_threshold_percent as f64 / 100.0;
                    if density / span_fraction.powi(2) < bar {
                        self.generational_accept_old(
                            format_args!(
                                "old generation fragmented (density={:.1}%, span={:.1}% of heap)",
                                density * 100.0,
                                span_fraction * 100.0,
                            ),
                            snapshot,
                        );
                        return true;
                    }
                }
            }
        }

        // Overgrowth: usage past live-at-mark plus an allowance, ignored
        // while young cycles are recent (deferred, feeds expedite-mixed).
        if let Some(live) = self.generational.live_at_last_old_mark {
            let total = snapshot.total_bytes;
            let current_old = total.saturating_sub(snapshot.effective_free_bytes());
            let allowance = percent_of(
                total.saturating_sub(live),
                self.config.old_growth_allowance_percent,
            );
            let trigger_threshold = live.saturating_add(allowance);
            let ignore_threshold = percent_of(total, self.config.old_growth_ignore_percent);
            if current_old > trigger_threshold {
                if current_old < ignore_threshold
                    && self.generational.consecutive_young_cycles
                        < self.config.old_growth_ignore_young_cycles
                {
                    self.generational.unprocessed_old_candidates = self
                        .generational
                        .unprocessed_old_candidates
                        .saturating_add(1);
                } else {
                    self.generational_accept_old(
                        format_args!(
                            "old generation overgrown (used={}, live_at_mark={}, threshold={})",
                            FormattedSize(current_old),
                            FormattedSize(live),
                            FormattedSize(trigger_threshold),
                        ),
                        snapshot,
                    );
                    return true;
                }
            }
        }

        false
    }

    /// Young trigger: yield to a fresh old cycle, run adaptive triggers, then
    /// expedite promotions and mixed work.
    fn generational_young_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: Instant,
        planned_sleep_interval: Duration,
    ) -> bool {
        // Let a fresh old cycle run before a young cycle may start.
        if self.generational.old_cycle_in_progress {
            if let Some(started) = self.generational.old_cycle_started_at {
                if now.saturating_duration_since(started) < self.config.minimum_old_time {
                    return false;
                }
            }
        }

        if self.adaptive_should_start(snapshot, now, planned_sleep_interval) {
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Young);
            return true;
        }

        // Allocation since the last GC stands in for promotion potential.
        let promo_threshold = percent_of(
            snapshot.total_bytes,
            self.config.expedite_promotion_threshold_percent,
        );
        if promo_threshold > 0 && snapshot.allocated_since_gc > promo_threshold {
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Young);
            self.accept_trigger(
                format_args!(
                    "expedite promotion (allocated_since_gc={}, threshold={})",
                    FormattedSize(snapshot.allocated_since_gc),
                    FormattedSize(promo_threshold),
                ),
                Some(snapshot),
            );
            return true;
        }

        // Deferred old candidates wait for a young cycle.
        if self.generational.unprocessed_old_candidates > self.config.expedite_mixed_threshold {
            let candidates = self.generational.unprocessed_old_candidates;
            let threshold = self.config.expedite_mixed_threshold;
            self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Young);
            self.accept_trigger(
                format_args!(
                    "expedite mixed (unprocessed_old_candidates={candidates}, threshold={threshold})",
                ),
                Some(snapshot),
            );
            return true;
        }

        false
    }

    fn generational_accept_old(&mut self, reason: impl std::fmt::Display, snapshot: HeapSnapshot) {
        self.generational.pending_cycle_kind = Some(GenerationalCycleKind::Old);
        self.adjust_old_garbage_threshold(snapshot.total_bytes);
        self.accept_trigger(reason, Some(snapshot));
    }

    fn adjust_old_garbage_threshold(&mut self, total_bytes: usize) {
        const INTERVENTION_PERCENT: usize = 50;
        let base = self.config.old_garbage_threshold_percent;
        let mut threshold = base;
        let span = self.generational.old_used_estimate.unwrap_or(0);
        let percent_used = if total_bytes > 0 {
            span.saturating_mul(100) / total_bytes
        } else {
            0
        };
        if percent_used > INTERVENTION_PERCENT && MINIMUM_OLD_GARBAGE_THRESHOLD_PERCENT < base {
            let potential = base - MINIMUM_OLD_GARBAGE_THRESHOLD_PERCENT;
            let severity = percent_used - INTERVENTION_PERCENT;
            threshold = if severity > 30 {
                base - potential
            } else if severity > 15 {
                base - 2 * potential / 3
            } else {
                base - potential / 3
            };
        }
        self.generational.old_garbage_threshold_percent = threshold;
    }

    pub(super) fn record_generational_cycle_end(
        &mut self,
        kind: Option<GenerationalCycleKind>,
        snapshot: HeapSnapshot,
    ) {
        let live = snapshot
            .total_bytes
            .saturating_sub(snapshot.effective_free_bytes());
        self.generational.old_used_estimate = Some(live);
        match kind {
            Some(GenerationalCycleKind::Old) => {
                self.generational.live_at_last_old_mark = Some(live);
                self.generational.consecutive_young_cycles = 0;
                self.generational.unprocessed_old_candidates = 0;
                self.generational.old_cycle_in_progress = false;
                self.generational.old_cycle_started_at = None;
            }
            _ => {
                self.generational.consecutive_young_cycles =
                    self.generational.consecutive_young_cycles.saturating_add(1);
            }
        }
    }
}
