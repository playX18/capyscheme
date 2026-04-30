//! Aggressive GC trigger heuristics.
//!
//! Aggressive mode is intentionally simple: every trigger evaluation requests a
//! GC cycle unless one is already active.  It is useful for stress testing GC
//! paths and for workloads where minimizing heap growth matters more than GC
//! frequency.

use super::{HeapSnapshot, HeuristicState};

impl HeuristicState {
    pub(super) fn aggressive_should_start(&mut self, snapshot: HeapSnapshot) -> bool {
        // The shared state machine has already filtered out active cycles, so
        // accepting here means "start the next cycle as soon as the scheduler can".
        self.accept_trigger(format_args!("aggressive mode"), Some(snapshot));
        true
    }
}
