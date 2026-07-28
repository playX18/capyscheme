//! SNI (Scheme Native Interface) — nest API for calling Scheme from native code.
//!
//! Safe re-entry into Scheme (increasing [`State::nest_level`](super::State::nest_level))
//! must go through this module. Raw [`call_scheme`](super::vm::call_scheme) is `unsafe`
//! and requires the caller to keep all live [`Value`]s rooted across the nest.
//!
//! Local references live in process-wide OopStorage (`sni_locals`) and are released
//! when a [`LocalFrame`] drops. Global references live in `sni_globals` until
//! explicitly deleted. Pack into `call_data` / runstack happens after roots
//! are established; prefer reloading from [`OopHandle`] after any nest.

use std::cell::RefCell;

use crate::heap::oop_storage::{HandleScope, OopHandle, OopStorage, OopStorageSet};
use crate::runtime::{
    Context,
    value::Value,
    vm::{ExecutionResult, call_scheme},
};

/// Strong storage for SNI local references.
pub fn sni_locals() -> &'static OopStorage {
    OopStorageSet::get().sni_locals
}

/// Strong storage for SNI global references.
pub fn sni_globals() -> &'static OopStorage {
    OopStorageSet::get().sni_globals
}

thread_local! {
    /// Pending exception for the current mutator thread.
    static PENDING_EXCEPTION: RefCell<Option<OopHandle>> = const { RefCell::new(None) };
}

/// A local-reference frame; local refs in the frame are released on drop.
pub struct LocalFrame<'a> {
    scope: HandleScope<'a>,
}

impl<'a> LocalFrame<'a> {
    pub fn push() -> Self {
        Self {
            scope: HandleScope::new(sni_locals()),
        }
    }

    pub fn new_local_ref<'gc>(&mut self, value: Value<'gc>) -> &OopHandle {
        self.scope.root(value)
    }

    /// Number of local refs currently held in this frame.
    pub fn len(&self) -> usize {
        self.scope.len()
    }

    pub fn ensure_capacity(&mut self, capacity: usize) {
        self.scope.ensure_capacity(capacity);
    }
}

/// A global reference that survives local-frame pop.
pub struct GlobalRef {
    handle: OopHandle,
}

impl GlobalRef {
    pub fn new<'gc>(value: Value<'gc>) -> Self {
        Self {
            handle: OopHandle::new(sni_globals(), value),
        }
    }

    pub fn get<'gc>(&self) -> Value<'gc> {
        self.handle.get()
    }

    pub fn set<'gc>(&self, value: Value<'gc>) {
        self.handle.set(value);
    }

    pub fn as_ptr(&self) -> *mut Value<'static> {
        self.handle.as_ptr()
    }

    /// Release the global ref.
    pub fn delete(self) {
        self.handle.release(sni_globals());
    }
}

/// Pending-exception helpers backing the SNI error channel.
pub mod exception {
    use super::*;

    /// Set the pending exception (takes ownership of a rooted value via global storage).
    pub fn throw<'gc>(value: Value<'gc>) {
        PENDING_EXCEPTION.with(|cell| {
            let mut slot = cell.borrow_mut();
            if let Some(old) = slot.take() {
                old.release(sni_globals());
            }
            *slot = Some(OopHandle::new(sni_globals(), value));
        });
    }

    /// True if an exception is pending.
    pub fn check() -> bool {
        PENDING_EXCEPTION.with(|cell| cell.borrow().is_some())
    }

    /// Return the pending exception value without clearing it.
    pub fn occurred<'gc>() -> Option<Value<'gc>> {
        PENDING_EXCEPTION.with(|cell| cell.borrow().as_ref().map(|h| h.get()))
    }

    /// Clear and return the pending exception, if any.
    pub fn clear<'gc>() -> Option<Value<'gc>> {
        PENDING_EXCEPTION.with(|cell| {
            cell.borrow_mut().take().map(|h| {
                let v = h.get();
                h.release(sni_globals());
                v
            })
        })
    }

    /// Describe the pending exception to stderr (best-effort print).
    pub fn describe() {
        if let Some(v) = occurred::<'_>() {
            eprintln!("SNI pending exception: {v}");
        }
    }
}

/// Environment for safe nested Scheme calls with local roots.
pub struct SniEnv<'gc> {
    pub ctx: Context<'gc>,
}

impl<'gc> SniEnv<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self { ctx }
    }

    /// Call a Scheme procedure, rooting `rator` and `args` for the duration of the nest.
    ///
    /// On `ExecutionResult::Err`, the error value is also installed as the pending
    /// exception so C callers can inspect it via the SNI exception API.
    pub fn call_function(
        &self,
        rator: Value<'gc>,
        args: impl IntoIterator<Item = Value<'gc>>,
    ) -> ExecutionResult<'gc> {
        let mut frame = LocalFrame::push();
        let rator_slot = frame.new_local_ref(rator).as_ptr();
        let mut arg_slots: Vec<*mut Value<'static>> = Vec::new();
        for arg in args {
            arg_slots.push(frame.new_local_ref(arg).as_ptr());
        }
        // Reload from OopStorage immediately before nest packing.
        // SAFETY: slots live until `frame` drops.
        let result = unsafe {
            let rator = std::mem::transmute::<Value<'static>, Value<'gc>>(*rator_slot);
            let rooted_args: Vec<Value<'gc>> = arg_slots
                .iter()
                .map(|slot| std::mem::transmute::<Value<'static>, Value<'gc>>(**slot))
                .collect();
            call_scheme(self.ctx, rator, rooted_args)
        };
        if let ExecutionResult::Err(err) = &result {
            exception::throw(*err);
        }
        result
    }
}

/// Convenience wrapper around [`SniEnv::call_function`].
pub fn call_function<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    SniEnv::new(ctx).call_function(rator, args)
}
