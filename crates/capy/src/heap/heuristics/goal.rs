//! `goal` GC trigger.
//!
//! Port of the trigger math in Go's pacer.  After each
//! collection the heap goal is `live * (1 + growth)`.  Marking has to finish
//! exactly when the heap reaches the goal, so the trigger is the goal minus the
//! allocation expected while marking (`cons/mark * live`), kept between 70% and
//! 95% of the way from live to goal.
//!
//! `cons/mark` is the bytes allocated during a concurrent mark per byte of live
//! data scanned.  It is measured from InitialMark to FinalMark and the maximum
//! of the last four cycles is used, which biases early like Go does.  Stop the
//! world plans never run a concurrent phase, so their samples are zero and the
//! trigger sits at the 70% point.

use std::collections::VecDeque;

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState, percent_of};

pub(super) const MIN_TRIGGER_FRACTION: f64 = 0.70;
pub(super) const MAX_TRIGGER_FRACTION: f64 = 0.95;
pub(super) const DEFAULT_GROWTH_PERCENT: usize = 100;
/// Go's 4 MiB heap minimum, scaled by the growth percentage.
const MIN_GOAL_BYTES: usize = 4 * 1024 * 1024;
const CONS_MARK_WINDOW: usize = 4;

#[derive(Clone, Debug, Default)]
pub(super) struct GoalState {
    cons_mark: VecDeque<f64>,
    used_at_initial_mark: Option<usize>,
    live_at_initial_mark: usize,
}

impl GoalState {
    pub(super) fn record_initial_mark(&mut self, used_bytes: usize, live_bytes: usize) {
        self.used_at_initial_mark = Some(used_bytes);
        self.live_at_initial_mark = live_bytes;
    }

    /// Records a finished concurrent mark; returns the measured cons/mark.
    pub(super) fn record_final_mark(&mut self, used_bytes: usize) -> Option<f64> {
        let at_initial = self.used_at_initial_mark.take()?;
        let allocated = used_bytes.saturating_sub(at_initial) as f64;
        let ratio = allocated / self.live_at_initial_mark.max(1) as f64;
        self.push(ratio);
        Some(ratio)
    }

    /// A stop-the-world cycle allocates nothing while marking.
    pub(super) fn record_stw_cycle(&mut self) {
        self.used_at_initial_mark = None;
        self.push(0.0);
    }

    fn push(&mut self, ratio: f64) {
        if self.cons_mark.len() == CONS_MARK_WINDOW {
            self.cons_mark.pop_front();
        }
        self.cons_mark.push_back(ratio.max(0.0));
    }

    pub(super) fn cons_mark(&self) -> f64 {
        self.cons_mark.iter().copied().fold(0.0, f64::max)
    }
}

/// Heap occupancy at which marking should start.
///
/// `penalties_percent` inflates the runway after late cycles, `min_free_percent`
/// keeps the goal off the hard backstop.
pub(super) fn trigger_point(
    live: usize,
    growth_percent: usize,
    cons_mark: f64,
    penalties_percent: usize,
    committed: usize,
    min_free_percent: usize,
) -> usize {
    let growth = growth_percent as f64 / 100.0;
    let ceiling = committed.saturating_sub(percent_of(committed, min_free_percent));
    let goal = (live as f64 * (1.0 + growth))
        .max(MIN_GOAL_BYTES as f64 * growth)
        .min(ceiling as f64)
        .max(live as f64);
    let span = goal - live as f64;
    let runway = cons_mark * live as f64 * (1.0 + penalties_percent as f64 / 100.0);
    let lowest = live as f64 + MIN_TRIGGER_FRACTION * span;
    let highest = live as f64 + MAX_TRIGGER_FRACTION * span;
    (goal - runway).clamp(lowest, highest) as usize
}

impl HeuristicState {
    pub(super) fn goal_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: std::time::Instant,
    ) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        // Nothing has been measured before the first GC, so there is no live set to size a goal from.
        if snapshot.live_bytes == 0 {
            let threshold = percent_of(
                snapshot.total_bytes,
                self.config.init_free_threshold_percent,
            );
            if snapshot.effective_free_bytes() < threshold {
                self.accept_trigger(
                    format_args!(
                        "bootstrap threshold crossed (free={}, threshold={})",
                        FormattedSize(snapshot.effective_free_bytes()),
                        FormattedSize(threshold),
                    ),
                    Some(snapshot),
                );
                return true;
            }
            return self.base_should_start(snapshot, now);
        }

        let trigger = trigger_point(
            snapshot.live_bytes,
            self.config.goal_growth_percent,
            self.goal.cons_mark(),
            self.gc_time_penalties,
            snapshot.committed_bytes,
            self.config.min_free_threshold_percent,
        );
        let used = snapshot.used_bytes();
        if used >= trigger {
            self.accept_trigger(
                format_args!(
                    "heap reached marking trigger (used={}, trigger={}, live={}, cons_mark={:.3})",
                    FormattedSize(used),
                    FormattedSize(trigger),
                    FormattedSize(snapshot.live_bytes),
                    self.goal.cons_mark(),
                ),
                Some(snapshot),
            );
            return true;
        }

        self.base_should_start(snapshot, now)
    }
}
