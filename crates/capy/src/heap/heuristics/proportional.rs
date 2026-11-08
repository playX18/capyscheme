//! `proportional` GC trigger.
//!
//! Collect once the mutator has allocated a fixed fraction of the live data
//! that survived the previous GC.  This is similar to BDWGC/Chez Scheme:
//! (`min_bytes_allocd = scan_size / GC_free_space_divisor`, divisor 3) with
//! 8 MiB as the floor, so a tiny live set does not turn into a collection every few pages.

use crate::utils::FormattedSize;

use super::{HeapSnapshot, HeuristicState};

pub(super) const DEFAULT_DIVISOR: usize = 3;
pub(super) const DEFAULT_MIN_TRIP_BYTES: usize = 8 * 1024 * 1024;
pub(super) const GUARANTEED_GC_INTERVAL: std::time::Duration = std::time::Duration::from_secs(30);

/// Allocation volume that triggers the next GC.
///
/// `max_trip == 0` means "half of the heap".  Before the first GC `live` is
/// zero, which yields `min_trip`.
pub(super) fn trip_bytes(
    live: usize,
    divisor: usize,
    min_trip: usize,
    max_trip: usize,
    total: usize,
) -> usize {
    let max_trip = if max_trip == 0 { total / 2 } else { max_trip };
    (live / divisor.max(1)).clamp(min_trip, max_trip.max(min_trip))
}

impl HeuristicState {
    pub(super) fn proportional_should_start(
        &mut self,
        snapshot: HeapSnapshot,
        now: std::time::Instant,
    ) -> bool {
        if self.trigger_if_below_min_free(snapshot) {
            return true;
        }

        let trip = trip_bytes(
            snapshot.live_bytes,
            self.config.proportional_divisor,
            self.config.proportional_min_trip,
            self.config.proportional_max_trip,
            snapshot.total_bytes,
        );
        if snapshot.allocated_since_gc >= trip {
            self.accept_trigger(
                format_args!(
                    "allocated since last GC reached live-proportional trip (allocated={}, trip={}, live={})",
                    FormattedSize(snapshot.allocated_since_gc),
                    FormattedSize(trip),
                    FormattedSize(snapshot.live_bytes),
                ),
                Some(snapshot),
            );
            return true;
        }

        self.base_should_start(snapshot, now)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MIB: usize = 1024 * 1024;

    #[test]
    fn trip_is_a_third_of_live_inside_bounds() {
        assert_eq!(trip_bytes(300 * MIB, 3, 8 * MIB, 0, 2048 * MIB), 100 * MIB);
    }

    #[test]
    fn trip_never_drops_below_the_floor() {
        assert_eq!(trip_bytes(0, 3, 8 * MIB, 0, 2048 * MIB), 8 * MIB);
        assert_eq!(trip_bytes(3 * MIB, 3, 8 * MIB, 0, 2048 * MIB), 8 * MIB);
    }

    #[test]
    fn trip_is_capped_by_half_the_heap_or_explicit_max() {
        assert_eq!(trip_bytes(3000 * MIB, 3, 8 * MIB, 0, 1024 * MIB), 512 * MIB);
        assert_eq!(
            trip_bytes(3000 * MIB, 3, 8 * MIB, 64 * MIB, 1024 * MIB),
            64 * MIB
        );
        // A cap below the floor cannot invert the clamp.
        assert_eq!(
            trip_bytes(300 * MIB, 3, 8 * MIB, 4 * MIB, 1024 * MIB),
            8 * MIB
        );
    }
}
