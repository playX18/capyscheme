//! POD record views over movable Scheme bytevectors.
//!
//! Interior pointers are only valid while the BV is rooted and until the next
//! allocation / nest / GC-triggering call. The [`PodRecord::from_scm`] /
//! [`PodRecord::from_scm_mut`] signatures borrow the [`Env`] for the lifetime
//! of the returned view, so safe callers cannot perform `&mut Env` calls
//! (allocation, nesting) that could move the bytevector while the view is
//! live. Reload via [`PodRecord::from_scm`] after any nesting call.

use std::mem::{align_of, size_of};

use bytemuck::{Pod, Zeroable};

use crate::{Env, Ref};

pub trait PodRecord: Pod + Zeroable {
    const TYPE_NAME: &'static str;

    fn scheme_size() -> usize {
        size_of::<Self>()
    }

    fn scheme_align() -> usize {
        align_of::<Self>()
    }

    /// Allocate a movable BV and write `value` into it.
    fn new_scm<'env>(env: &mut Env<'env>, value: Self) -> Ref<'env> {
        let bv = env.make_pod_bytevector(Self::scheme_size());
        if let Some(slot) = Self::from_scm_mut(env, bv) {
            *slot = value;
        }
        bv
    }

    /// View BV contents as `&Self`.
    ///
    /// Returns `None` unless `value` is a bytevector of exactly
    /// `size_of::<Self>()` bytes whose data pointer is aligned for `Self`.
    /// The borrow on `env` is the point of the signature: it keeps safe
    /// callers from allocating or nesting while the view is live.
    fn from_scm<'a, 'env>(env: &'a Env<'env>, value: Ref<'env>) -> Option<&'a Self> {
        if !env.is_bytevector(value) {
            return None;
        }
        let len = env.bytevector_len(value).ok()?;
        if len != Self::scheme_size() {
            return None;
        }
        let ptr = env.bytevector_data(value).ok()?;
        if ptr.is_null() || !(ptr as usize).is_multiple_of(align_of::<Self>()) {
            return None;
        }
        // SAFETY: value is a bytevector of exactly size_of::<Self>() bytes
        // whose data pointer is aligned for Self, and Self: Pod so any bit
        // pattern is a valid Self. The &'a Env borrow prevents safe callers
        // from GC-triggering calls while the view is live.
        Some(unsafe { &*(ptr as *const Self) })
    }

    /// View BV contents as `&mut Self`. Same checks and same env-borrow
    /// discipline as [`PodRecord::from_scm`].
    fn from_scm_mut<'a, 'env>(env: &'a mut Env<'env>, value: Ref<'env>) -> Option<&'a mut Self> {
        if !env.is_bytevector(value) {
            return None;
        }
        let len = env.bytevector_len(value).ok()?;
        if len != Self::scheme_size() {
            return None;
        }
        let ptr = env.bytevector_data(value).ok()?;
        if ptr.is_null() || !(ptr as usize).is_multiple_of(align_of::<Self>()) {
            return None;
        }
        // SAFETY: same as from_scm; the &'a mut Env borrow additionally
        // guarantees exclusive access to the view.
        Some(unsafe { &mut *(ptr as *mut Self) })
    }
}
