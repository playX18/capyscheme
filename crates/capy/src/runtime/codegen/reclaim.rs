//! Deferred release of dead machine code.
//!
//! The collector finds an image's owner object unreachable inside the GC
//! pause. Releasing the image there would take the code-memory locks, the
//! registries and the profiler and debugger maps on a collector thread, so
//! the finalizer only records the owner, resurrected. A mutator releases it
//! later, from a point that holds none of those locks ([`drain`]).
//!
//! Release takes two drains with a collection between them. Resurrection
//! keeps weak references to the owner intact, so a watchpoint dependent or a
//! cache can still reach the image after the collector found it dead. The
//! first drain retires the image ([`CodeImage::retire`]): from then on it
//! answers that it has no code. The image stays rooted, and the next
//! collection marks it as seen. A thread that read the image as live before
//! the retire has passed that collection's safepoint, so the drain after it
//! can drop the native part ([`CodeImage::release`]) with no reader left.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock};

use mmtk::util::{Address, ObjectReference};
use parking_lot::Mutex;

use crate::heap::finalizer::FinalizerQueue;
use crate::heap::{Gc, ObjectSlot};
use crate::runtime::objectmodel::CodeImage;

/// A collection queued dead owners since the last drain. The collector sets
/// this from its finalization hook; a mutator or the service thread drains.
static PENDING: AtomicBool = AtomicBool::new(false);

/// The finalizer queue of every [`CodeImage`].
pub(crate) struct ImageFinalizers {
    /// Owners the collector found dead and resurrected, awaiting [`drain`].
    /// They are in the large object space, so they never move.
    ready: Mutex<Vec<ObjectReference>>,
    /// Owners retired by a drain, still rooted, whose native part is dropped
    /// by the first drain after a collection has seen them.
    retiring: Mutex<Vec<ObjectReference>>,
    /// A collection rooted every owner in `retiring` since the last drain.
    retiring_seen: AtomicBool,
}

impl ImageFinalizers {
    fn take(&self) -> Vec<ObjectReference> {
        std::mem::take(&mut *self.ready.lock())
    }
}

unsafe impl FinalizerQueue for ImageFinalizers {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        self.ready.lock().push(object);
    }

    fn schedule(&self) {
        // Runs on the collector's finalization thread. Taking the code-memory
        // and map locks there would be unsafe, so only raise the flag; a
        // mutator or the service thread drains.
        PENDING.store(true, Ordering::Release);
    }

    fn ready_slots(&self, slots: &mut Vec<ObjectSlot>) {
        let ready = self.ready.lock();
        slots.extend(
            ready
                .iter()
                .map(|object| ObjectSlot::from_address(Address::from_ref(object))),
        );
        let retiring = self.retiring.lock();
        slots.extend(
            retiring
                .iter()
                .map(|object| ObjectSlot::from_address(Address::from_ref(object))),
        );
        if !retiring.is_empty() {
            self.retiring_seen.store(true, Ordering::Release);
        }
    }
}

pub(crate) static FINALIZERS: LazyLock<Arc<ImageFinalizers>> = LazyLock::new(|| {
    Arc::new(ImageFinalizers {
        ready: Mutex::new(Vec::new()),
        retiring: Mutex::new(Vec::new()),
        retiring_seen: AtomicBool::new(false),
    })
});

pub(crate) fn pending() -> bool {
    PENDING.load(Ordering::Acquire)
        || !FINALIZERS.ready.lock().is_empty()
        || FINALIZERS.retiring_seen.load(Ordering::Acquire)
}

/// Drain if a collection queued dead owners or the queue is non-empty. Safe
/// to call from a mutator or the service thread; it takes the code-memory
/// and map locks, never the GC lock.
pub(crate) fn drain_if_needed() {
    if pending() {
        drain();
    }
}

/// Releases everything the collector found dead
pub(crate) fn drain() {
    PENDING.store(false, Ordering::Release);
    // The owners a collection has seen since they were retired, and the
    // newly dead ones, which retire now and wait for the next collection.
    // The lock is held only to move the lists, never while code is released.
    let aged = {
        let mut retiring = FINALIZERS.retiring.lock();
        let aged = if FINALIZERS.retiring_seen.swap(false, Ordering::AcqRel) {
            std::mem::take(&mut *retiring)
        } else {
            Vec::new()
        };
        for owner in FINALIZERS.take() {
            // SAFETY: a resurrected owner from the finalizer queue.
            let image = unsafe { Gc::<CodeImage>::from_object_reference(owner) };
            image.as_gc_ref().retire();
            retiring.push(owner);
        }
        aged
    };
    for owner in aged {
        // SAFETY: retired by an earlier drain, and rooted through a
        // collection since, so no thread still reads it as live.
        let owner = unsafe { Gc::<CodeImage>::from_object_reference(owner) };
        super::cache::note_reclaim();
        unsafe { owner.as_gc_ref().release() };
    }
}
