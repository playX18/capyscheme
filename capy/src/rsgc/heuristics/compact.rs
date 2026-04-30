//! Compact GC trigger heuristics.
//!
//! Compact mode is biased toward keeping the heap small and clean.  It keeps
//! the normal minimum-free safeguard, but also starts a cycle after a fixed
//! percentage of the heap has been allocated since the previous GC.  That makes
//! it more eager than static mode without the adaptive predictor machinery.

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState, percent_of};

pub(super) const ALLOCATION_THRESHOLD_PERCENT: usize = 10;
pub(super) const GUARANTEED_GC_INTERVAL: std::time::Duration = std::time::Duration::from_secs(30);

impl HeuristicState {
    pub(super) fn compact_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: std::time::Instant,
    ) -> bool {
        // Do not wait for the allocation threshold if the heap is already below
        // the hard free-space floor.
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        // Compact mode's distinctive trigger: collect once post-GC allocation
        // exceeds a configured share of the fixed heap.
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

        // Fall back to shared triggers such as the guaranteed GC interval.
        self.base_should_start(snapshot, now)
    }
}
