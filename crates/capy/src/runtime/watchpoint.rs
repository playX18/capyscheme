//! # Watchpoint sets
//!
//! A `WatchpointSet` holds the dependents of one assumption: sites or caches
//! whose correctness relies on it. The set is in one of three states:
//!
//! - clear: no dependent has registered, and firing only changes the state
//! - watched: at least one dependent is registered
//! - invalidated: the assumption is false for good. Nothing registers again,
//!   and the set is never reset
//!
//! [`WatchpointSet::add`] registers a dependent and refuses once the set is
//! invalidated. [`WatchpointSet::fire`] invalidates the set and clears every
//! live dependent. Dependents whose owner has been collected are skipped, and
//! they are dropped on a later `add` once more than half of the list is dead.
//!
//! Firing leaves the thread owing a grace period: a reader may still be
//! running with an entry it loaded before the clear. The invalidating
//! operation calls [`grace`] last, once it holds no unrooted `Gc`.
//!
//! ## Adding an assumption
//!
//! The set is not tied to what it guards. To watch a new assumption:
//!
//! 1. add a [`Dependent`] variant for the kind of cache that depends on it,
//!    with its arms in `same`, `is_dead` and `clear`
//! 2. store a `Gc<WatchpointSet>` on the object that owns the assumption
//! 3. have fillers read the set before their lookup, [`WatchpointSet::add`]
//!    before publishing, and recheck [`WatchpointSet::is_invalid`] after.
//! 4. have writers mutate first, then [`WatchpointSet::fire`] (renewing the
//!    set if the assumption can hold again), then call [`grace`].
//!
//! See [`ClassTable::fire_all_slot_protocol`](crate::runtime::class::table::ClassTable::fire_all_slot_protocol) for an example.

use std::cell::Cell;
use std::sync::atomic::{AtomicU8, Ordering};

use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::{FILL_RETRIES, Lock, cas_cache};
use crate::heap::weak::Weak;
use crate::heap::{Gc, Trace};
use crate::runtime::MutationContext;
use crate::runtime::class::GenericDescriptor;
use crate::runtime::interp::slow::clear_slot_site;
use crate::runtime::value::{CodeBuffer, Value};

/// Total dependents cleared by fires so far, for tests and `CAPY_IC_LOG`.
pub static DEPENDENTS_CLEARED: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Not yet depended on: firing is a state change only.
const CLEAR: u8 = 0;
/// At least one dependent registered.
const WATCHED: u8 = 1;
/// The assumption is false for good, nothing may register.
const INVALIDATED: u8 = 2;

/// Which slot operation a [`Dependent::BytecodeSite`] caches.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(require_static)]
pub enum SiteOp {
    GetSlot,
    PutSlot,
}

/// A site or cache that depends on a set. A dead owner (collected buffer or
/// generic) is skipped.
#[derive(Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum Dependent<'gc> {
    /// The inline cache in metadata entry `metadata_id` of `buffer`.
    BytecodeSite {
        buffer: Weak<'gc, CodeBuffer<'gc>>,
        metadata_id: u32,
        op: SiteOp,
    },
    /// The slot-access cache of a generic (`GenericDescriptor::slot_cache`).
    GenericSlotCache(Weak<'gc, GenericDescriptor<'gc>>),
}

impl<'gc> Dependent<'gc> {
    /// The `get_slot`/`put_slot` site `metadata_id` of `buffer`.
    pub fn site(buffer: Gc<'gc, CodeBuffer<'gc>>, metadata_id: u32, op: SiteOp) -> Self {
        Self::BytecodeSite {
            buffer: Gc::downgrade(buffer),
            metadata_id,
            op,
        }
    }

    /// The slot-access cache of `generic`.
    pub fn generic_slot_cache(generic: Gc<'gc, GenericDescriptor<'gc>>) -> Self {
        Self::GenericSlotCache(Gc::downgrade(generic))
    }

    /// Whether both name the same site or cache.
    fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::BytecodeSite {
                    buffer: a,
                    metadata_id: ia,
                    op: oa,
                },
                Self::BytecodeSite {
                    buffer: b,
                    metadata_id: ib,
                    op: ob,
                },
            ) => a.ptr == b.ptr && ia == ib && oa == ob,
            (Self::GenericSlotCache(a), Self::GenericSlotCache(b)) => a.ptr == b.ptr,
            _ => false,
        }
    }

    fn is_dead(&self) -> bool {
        match self {
            Self::BytecodeSite { buffer, .. } => buffer.is_broken(),
            Self::GenericSlotCache(generic) => generic.is_broken(),
        }
    }

    /// Empty the cache this dependent names.
    fn clear(&self, ctx: MutationContext<'gc>) {
        match self {
            Self::BytecodeSite {
                buffer,
                metadata_id,
                op,
            } => {
                if let Some(buffer) = buffer.upgrade(ctx) {
                    clear_slot_site(buffer, *metadata_id, *op);
                }
            }
            Self::GenericSlotCache(generic) => {
                if let Some(generic) = generic.upgrade(ctx) {
                    GenericDescriptor::clear_slot_cache(ctx, generic);
                }
            }
        }
    }
}

thread_local! {
    /// This thread fired a set and has not yet waited out the grace period.
    static GRACE_OWED: Cell<bool> = const { Cell::new(false) };
}

/// Wait until every thread has left any cache hit that started before the
/// fires so far (a no-op when this thread fired nothing). Call last in an
/// invalidating operation. A collection may run while this waits, so
/// `result` is rooted across the wait and returned reloaded; hold no other
/// unrooted `Gc`.
pub fn grace<'gc>(ctx: MutationContext<'gc>, result: Value<'gc>) -> Value<'gc> {
    if !GRACE_OWED.with(|owed| owed.replace(false)) {
        return result;
    }
    let scope = crate::runtime::root::RootScope::new(ctx);
    let rooted = scope.root(result);
    crate::heap::sync::handshake::grace_period();
    rooted.get()
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct WatchpointSet<'gc> {
    state: Lock<u8>,
    dependents: Lock<ArrayRef<'gc, Dependent<'gc>>>,
}

impl<'gc> WatchpointSet<'gc> {
    pub fn new(ctx: MutationContext<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            ctx,
            Self {
                state: Lock::new(CLEAR),
                dependents: Lock::new(Array::with(ctx, 0, |_, _| {
                    unreachable!("zero-length dependent arrays are never initialized")
                })),
            },
        )
    }

    #[inline]
    fn state_atomic(&self) -> &AtomicU8 {
        // SAFETY: `Lock<u8>` is `repr(transparent)` over `Cell<u8>`, which
        // has the layout of `u8`, and the state is only accessed through
        // this view.
        unsafe { &*(self.state.as_ptr() as *const AtomicU8) }
    }

    /// Whether the assumption has been broken. Acquire, pairing with the
    /// Release of [`Self::fire`].
    #[inline]
    pub fn is_invalid(&self) -> bool {
        self.state_atomic().load(Ordering::Acquire) == INVALIDATED
    }

    /// Number of registered dependents (including dead ones not yet
    /// compacted).
    pub fn dependent_count(&self) -> usize {
        self.dependents.get().len()
    }

    /// Register `dependent`. Returns `false`, registering nothing, when the
    /// set is (or becomes, while registering) invalidated: the caller drops
    /// its fill, or clears it if already published.
    pub fn add(this: Gc<'gc, Self>, ctx: MutationContext<'gc>, dependent: Dependent<'gc>) -> bool {
        for _ in 0..=FILL_RETRIES {
            if this.is_invalid() {
                return false;
            }
            let current = this.dependents.get();
            // A site that refills (a polymorphic one thrashing) registers
            // once.
            if current.iter().any(|d| d.same(&dependent)) {
                return !this.is_invalid();
            }
            // Drop dead dependents when more than half are, so a long-lived
            // set does not keep every buffer's tombstone.
            let dead = current.iter().filter(|d| d.is_dead()).count();
            let keep: Vec<Dependent<'gc>> = if dead * 2 > current.len() {
                current.iter().copied().filter(|d| !d.is_dead()).collect()
            } else {
                current.iter().copied().collect()
            };
            let appended = Array::with(ctx, keep.len() + 1, |_, index| {
                if index < keep.len() {
                    keep[index]
                } else {
                    dependent
                }
            });
            let owner = Gc::write(ctx, this);
            let _ = owner;
            if cas_cache(&this.dependents, current, appended) {
                // Fire reads the dependents after setting the state, so a
                // set invalidated before this check may have missed us.
                let _ = this.state_atomic().compare_exchange(
                    CLEAR,
                    WATCHED,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                );
                return !this.is_invalid();
            }
        }
        false
    }

    /// Break the assumption: invalidate the set for good and clear every
    /// registered dependent. Returns how many were cleared. The caller runs
    /// [`grace`] (one call covers any number of fires).
    pub fn fire(this: Gc<'gc, Self>, ctx: MutationContext<'gc>) -> usize {
        GRACE_OWED.with(|owed| owed.set(true));
        // The state is set before the dependents are read, so an `add` that
        // registered before this point is in the list, and one that checks
        // after it sees the invalidation.
        this.state_atomic().swap(INVALIDATED, Ordering::AcqRel);
        let dependents = this.dependents.get();
        let mut cleared = 0;
        for dependent in dependents.iter() {
            if !dependent.is_dead() {
                dependent.clear(ctx);
                cleared += 1;
            }
        }
        DEPENDENTS_CLEARED.fetch_add(cleared as u64, Ordering::Relaxed);
        cleared
    }
}
