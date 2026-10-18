//! Deferred release of dead machine code.
//!
//! The collector finds an image's owner object unreachable inside the GC
//! pause. Releasing the image there would take the code-memory locks, the
//! registries and the profiler and debugger maps on a collector thread, so
//! the finalizer only records the owner, resurrected. A mutator releases it
//! later, from a point that holds none of those locks ([`drain`]).
//!
//! The native part of the code (a [`CodeUnit`](super::unit::CodeUnit) or a
//! call IC stub) lives inside the owner, a [`CodeImage`]. Between the
//! finalizer and the drain the owner is a root ([`scan_roots`]), so a
//! collection in between does not free it: dropping the native part in
//! place is the whole release.

use std::sync::{Arc, LazyLock};

use mmtk::util::ObjectReference;
use mmtk::vm::RootsWorkFactory;
use parking_lot::Mutex;

use crate::heap::Gc;
use crate::heap::ObjectSlot;
use crate::heap::finalizer::FinalizerQueue;
use crate::runtime::value::CodeImage;

/// A pending owner, in a slot of its own: the collector gets the slot's
/// address as a root, so it must not move while the owner waits.
type PendingSlot = Box<Gc<'static, CodeImage<'static>>>;

/// Owners the collector found dead and resurrected, awaiting [`drain`].
static DEAD: Mutex<Vec<PendingSlot>> = Mutex::new(Vec::new());

/// Records `owner` for the next [`drain`]. Takes only the queue lock, for a
/// push, so it is safe inside the GC pause.
fn defer(owner: ObjectReference) {
    // SAFETY: only `CodeImage`s are registered with this queue. The slot is a
    // root until `drain`, so the lifetime is the list's.
    let owner = unsafe { Gc::<CodeImage>::from_object_reference(owner) };
    DEAD.lock().push(Box::new(owner));
}

/// Whether anything awaits release.
pub(crate) fn pending() -> bool {
    !DEAD.lock().is_empty()
}

/// Reports the owners that await release as roots, so they survive until
/// [`drain`]. They are in the large object space and never move.
pub(crate) fn scan_roots(factory: &mut impl RootsWorkFactory<ObjectSlot>) {
    let mut dead = DEAD.lock();
    if !dead.is_empty() {
        factory.create_process_roots_work(
            dead.iter_mut()
                .map(|slot| ObjectSlot::from(&mut **slot))
                .collect(),
        );
    }
}

/// Releases everything the collector found dead: dropping the native part of
/// an owner returns its executable memory.
pub(crate) fn drain() {
    let dead = std::mem::take(&mut *DEAD.lock());
    for owner in dead {
        // SAFETY: the collector found the owner dead, so nothing else refers
        // to its code, and the list held the only reference.
        unsafe { owner.as_gc_ref().release() };
    }
}

/// The finalizer queue of every [`CodeImage`].
pub(crate) struct ImageFinalizers;

// SAFETY: the queue keeps the resurrected owner reachable through `DEAD`
// (see `scan_roots`) until `drain` releases it.
unsafe impl FinalizerQueue for ImageFinalizers {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        defer(object);
    }

    fn schedule(&self) {}
}

pub(crate) static FINALIZERS: LazyLock<Arc<ImageFinalizers>> =
    LazyLock::new(|| Arc::new(ImageFinalizers));
