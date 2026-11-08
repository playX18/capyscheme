//! Deferred release of dead machine code.
//!
//! The collector finds an image's owner object unreachable inside the GC
//! pause. Releasing the image there would take the code-memory locks, the
//! registries and the profiler and debugger maps on a collector thread, so
//! the finalizer only records the owner, resurrected. A mutator releases it
//! later, from a point that holds none of those locks ([`drain`]).

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
    }
}

pub(crate) static FINALIZERS: LazyLock<Arc<ImageFinalizers>> = LazyLock::new(|| {
    Arc::new(ImageFinalizers {
        ready: Mutex::new(Vec::new()),
    })
});

pub(crate) fn pending() -> bool {
    PENDING.load(Ordering::Acquire) || !FINALIZERS.ready.lock().is_empty()
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
    for owner in FINALIZERS.take() {
        let owner = unsafe { Gc::<CodeImage>::from_object_reference(owner) };
        super::cache::note_reclaim();
        unsafe { owner.as_gc_ref().release() };
    }
}
