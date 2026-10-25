//! Deferred release of dead machine code.
//!
//! The collector finds an image's owner object unreachable inside the GC
//! pause. Releasing the image there would take the code-memory locks, the
//! registries and the profiler and debugger maps on a collector thread, so
//! the finalizer only records the owner, resurrected. A mutator releases it
//! later, from a point that holds none of those locks ([`drain`]).

use std::sync::{Arc, LazyLock};

use mmtk::util::{Address, ObjectReference};
use parking_lot::Mutex;

use crate::heap::finalizer::FinalizerQueue;
use crate::heap::{Gc, ObjectSlot};
use crate::runtime::value::CodeImage;

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

    fn schedule(&self) {}

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
    !FINALIZERS.ready.lock().is_empty()
}

/// Releases everything the collector found dead
pub(crate) fn drain() {
    for owner in FINALIZERS.take() {
        let owner = unsafe { Gc::<CodeImage>::from_object_reference(owner) };
        unsafe { owner.as_gc_ref().release() };
    }
}
