//! Heap objects for threads and synchronization.

use super::*;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::cell::Lock;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, AtomicUsize};

pub struct Condition {
    pub(crate) _unused: u64,
}

pub(crate) fn condition_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONDITION).expect("builtin class id is nonzero"),
    )
}

// SAFETY: GC trace for `Condition`, all reachable heap fields are visited
unsafe impl Trace for Condition {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}


pub struct Mutex {
    pub(crate) reentrant: bool,
    pub(crate) state: parking_lot::Mutex<MutexState>,
}

#[derive(Default)]
pub(crate) struct MutexState {
    /// Owner token, `0` when unowned.
    pub(crate) owner: u64,
    pub(crate) count: usize,
    /// Set when the owner died holding the lock. The next acquirer takes it
    /// and reports it once (SRFI-18 abandoned-mutex).
    pub(crate) abandoned: bool,
}

pub(crate) fn mutex_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::MUTEX).expect("builtin class id is nonzero"))
}


unsafe impl Trace for Mutex {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Mutex`
unsafe impl ClassTagged for Mutex {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::MUTEX];

    const TYPE_NAME: &'static str = "mutex";
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Condition`
unsafe impl ClassTagged for Condition {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONDITION];

    const TYPE_NAME: &'static str = "condition";
}

#[repr(C)]

pub struct ThreadObject<'gc> {
    pub(crate) entrypoint: Lock<Option<Value<'gc>>>,
    /// Allocated by the first `enqueue_interrupt`: most threads never get one,
    /// and the box is a malloc on every spawn otherwise.
    pub(crate) pending_interrupts: AtomicPtr<PendingInterruptQueue>,
    /// The native thread this object interrupts, for threads that are not
    /// threads. Zero when unused.
    pub(crate) runtime_thread: AtomicUsize,
    /// The thread this object interrupts. Zero when unbound. VirtualThread ids
    /// start at 1, so zero is never a live thread.
    pub(crate) virtual_thread: AtomicU64,
    pub(crate) interrupt_level: AtomicUsize,
    /// Set when the thread's thread has finished. Joiners wait on it.
    pub(crate) done: parking_lot::Mutex<bool>,
    /// The escape continuation of the thread's body, which a kill invokes to
    /// unwind the Scheme stack through `dynamic-wind` after-thunks.
    pub(crate) unwind_k: Lock<Option<Value<'gc>>>,
    /// Set when the thread was killed (`thread-terminate!`).
    pub(crate) killed: AtomicBool,
    /// VirtualThread a kill is sent to before the thread has bound itself. Set at
    /// spawn, so a kill can reach a thread that has not run yet.
    pub(crate) kill_target: AtomicU64,
    /// Mutexes this thread holds, by address, rooted here. A thread that
    /// finishes while holding one has it force-released and abandoned.
    pub(crate) held: parking_lot::Mutex<Vec<(usize, Value<'static>)>>,
    /// Key of this thread's join queue in the wait-queue table. A counter
    /// value with the top bit set, so it is stable when the collector moves
    /// the object and cannot equal the address of a mutex or condition.
    pub(crate) join_key: usize,
}

/// Source of [`ThreadObject::join_key`] values.
pub(crate) static NEXT_JOIN_KEY: AtomicUsize = AtomicUsize::new(0);

pub(crate) struct PendingInterruptQueue {
    pub(crate) queue: parking_lot::Mutex<VecDeque<Value<'static>>>,
}

pub(crate) fn thread_object_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::THREAD).expect("builtin class id is nonzero"))
}


unsafe impl<'gc> Trace for ThreadObject<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.entrypoint.trace(visitor);
            self.unwind_k.trace(visitor);
            for (_, value) in self.held.get_mut().iter_mut() {
                visitor.trace(value);
            }
            if let Some(queue) = self.pending_interrupts.get_mut().as_mut() {
                for value in queue.queue.get_mut().iter_mut() {
                    visitor.trace(value);
                }
            }
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.entrypoint.process_weak_refs(weak_processor);
            self.unwind_k.process_weak_refs(weak_processor);
            if let Some(queue) = self.pending_interrupts.get_mut().as_mut() {
                for value in queue.queue.get_mut().iter_mut() {
                    value.process_weak_refs(weak_processor);
                }
            }
        }
    }
}

// SAFETY: `gc` for `ThreadObject` upholds all trait invariants
unsafe impl<'gc> ClassTagged for ThreadObject<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::THREAD];

    const TYPE_NAME: &'static str = "thread";
}


