//! HotSpot-style OopStorage for off-heap precise Value roots.
//!
//! Clients allocate slots, store [`Value`]s, and hold `*mut Value` handles.
//! Unused / released slots are always empty (`Value::empty()`). GC iterates
//! allocated slots as relocatable [`ObjectSlot`] roots (via [`Value::trace`]).
//!
//! Ported from OpenJDK `gc/shared/oopStorage.cpp` (+ inline / ParState / Set).

mod block;
mod handle;
mod par_state;
mod set;
mod storage;
mod synchronizer;

pub use block::{BLOCK_ALIGNMENT, BULK_ALLOCATE_LIMIT, SLOTS_PER_BLOCK};
pub use handle::{HandleScope, OopHandle, StrongOopStorage};
pub use par_state::{BasicParState, ParState};
pub use set::OopStorageSet;
pub use storage::{EntryStatus, NumDeadCallback, OopStorage};
pub use synchronizer::{CriticalSection, SingleWriterSynchronizer};
