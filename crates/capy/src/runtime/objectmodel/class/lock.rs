//! The metaobject mutation lock.
//!
//! Adding or removing a method, changing a combination or fallback, sealing, and defining or
//! redefining a class are each a read, a decision and a write over shared descriptors. The
//! scheduler runs Scheme threads in parallel, so without one lock two of them lose each other's
//! update. The lock is process wide and reentrant: a mutator that calls another one keeps it.
//!
//! The section under the lock allocates, so it can reach a safepoint while another thread waits
//! for the lock. A waiter therefore blocks in native state ([`MutationContext::call_in_native`]),
//! where a stop-the-world request does not wait for it. A thread blocked here holds no Scheme
//! values across the wait.

use parking_lot::ReentrantMutexGuard;

use crate::heap::sync::monitor::ReentrantMonitor;
use crate::runtime::MutationContext;

static MOP_LOCK: ReentrantMonitor = ReentrantMonitor::new(());

/// Proof that the calling thread holds the metaobject mutation lock.
#[must_use = "the lock is released when the guard is dropped"]
pub(crate) struct MopGuard(#[allow(dead_code)] ReentrantMutexGuard<'static, ()>);

/// Take the metaobject mutation lock, waiting outside the collector's mutating world if another
/// thread holds it.
pub(crate) fn mop_lock<'gc>(ctx: MutationContext<'gc>) -> MopGuard {
    if let Some(guard) = MOP_LOCK.try_lock() {
        return MopGuard(guard);
    }
    MopGuard(ctx.call_in_native(|| MOP_LOCK.lock()))
}
