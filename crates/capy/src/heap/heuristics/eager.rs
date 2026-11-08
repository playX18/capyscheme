//! `eager` GC trigger: collect at every opportunity.
//!
//! Eager is intentionally simple: every trigger evaluation requests a
//! GC cycle unless one is already active.  It is useful for stress testing GC
//! paths and for workloads where minimizing heap growth matters more than GC
//! frequency.

use super::{HeapSnapshot, HeuristicState};

impl HeuristicState {
    pub(super) fn eager_should_start(&mut self, snapshot: HeapSnapshot) -> bool {
        // The shared state machine has already filtered out active cycles, so
        // accepting here means "start the next cycle as soon as the scheduler can".
        self.accept_trigger(format_args!("eager policy"), Some(snapshot));
        true
    }
}
