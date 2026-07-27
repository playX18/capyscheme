//! JNI-like nest API for calling Scheme from native code with precise local roots.
//!
//! Safe re-entry into Scheme (increasing [`State::nest_level`](super::State::nest_level))
//! must go through this module. Raw [`call_scheme`](super::vm::call_scheme) is `unsafe`
//! and requires the caller to keep all live [`Value`]s rooted across the nest.
//!
//! Local references live in process-wide OopStorage (`jni_locals`) and are released
//! when a [`LocalFrame`] drops. Pack into `call_data` / runstack happens after roots
//! are established; prefer reloading from [`OopHandle`] after any nest.

use crate::rsgc::oop_storage::{HandleScope, OopHandle, OopStorage, OopStorageSet};
use crate::runtime::{
    Context,
    value::Value,
    vm::{ExecutionResult, call_scheme},
};

/// Strong storage for JNI-style local references.
pub fn jni_locals() -> &'static OopStorage {
    OopStorageSet::get().jni_locals
}

/// A local-reference frame (JNI `PushLocalFrame` / `PopLocalFrame`).
pub struct LocalFrame<'a> {
    scope: HandleScope<'a>,
}

impl<'a> LocalFrame<'a> {
    pub fn push() -> Self {
        Self {
            scope: HandleScope::new(jni_locals()),
        }
    }

    pub fn new_local_ref<'gc>(&mut self, value: Value<'gc>) -> &OopHandle {
        self.scope.root(value)
    }
}

/// Environment for safe nested Scheme calls with local roots.
pub struct JniEnv<'gc> {
    pub ctx: Context<'gc>,
}

impl<'gc> JniEnv<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self { ctx }
    }

    /// Call a Scheme procedure, rooting `rator` and `args` for the duration of the nest.
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
        unsafe {
            let rator = std::mem::transmute::<Value<'static>, Value<'gc>>(*rator_slot);
            let rooted_args: Vec<Value<'gc>> = arg_slots
                .iter()
                .map(|slot| std::mem::transmute::<Value<'static>, Value<'gc>>(**slot))
                .collect();
            call_scheme(self.ctx, rator, rooted_args)
        }
    }
}

/// Convenience wrapper around [`JniEnv::call_function`].
pub fn call_function<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    JniEnv::new(ctx).call_function(rator, args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;
    use crate::runtime::value::{NativeReturn, PROCEDURES, ReturnCode};

    extern "C-unwind" fn test_ok<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> NativeReturn<'gc> {
        NativeReturn {
            code: ReturnCode::ReturnOk,
            value: Value::new(true),
        }
    }

    #[test]
    fn jni_call_function_bumps_and_restores_nest_level() {
        Scheme::new_uninit().enter(|ctx| {
            let proc = PROCEDURES
                .fetch(*ctx)
                .register_static_closure(ctx, test_ok as _, Value::null());
            assert_eq!(ctx.nest_level(), 0);
            let result = call_function(ctx, proc.into(), []);
            assert!(matches!(result, ExecutionResult::Ok(v) if v == Value::new(true)));
            assert_eq!(ctx.nest_level(), 0);
        });
    }
}
