//! Shared helpers for SNI API modules.

use crate::prelude::*;
use crate::runtime::modules::{Module, convert_module_name, resolve_module};
use crate::runtime::sni;
use crate::runtime::vm::ExecutionResult;
use crate::runtime::vm::ffi::{Pointer, pointer_header_word};

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, ref_value};

pub(crate) fn call_public<'gc>(
    env: &mut SniEnv<'gc>,
    name: &str,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ScmRef {
    let Some(proc) = env.ctx.public_ref("capy", name) else {
        return SCM_REF_NULL;
    };
    match sni::call_function(env.ctx, proc, args) {
        ExecutionResult::Ok(v) => env.make_local(v),
        ExecutionResult::Err(_) => SCM_REF_NULL,
    }
}

pub(crate) fn call_root<'gc>(
    env: &mut SniEnv<'gc>,
    name: &str,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ScmRef {
    let Some(proc) = env.ctx.globals().root_module().get_str(env.ctx, name) else {
        // Fall back to public capy binding.
        return call_public(env, name, args);
    };
    match sni::call_function(env.ctx, proc, args) {
        ExecutionResult::Ok(v) => env.make_local(v),
        ExecutionResult::Err(_) => SCM_REF_NULL,
    }
}

pub(crate) fn values_from_refs<'gc>(argv: *const ScmRef, n: usize) -> Vec<Value<'gc>> {
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let r = unsafe { *argv.add(i) };
        out.push(unsafe { ref_value(r) });
    }
    out
}

pub(crate) fn make_pointer_value<'gc>(ctx: Context<'gc>, ptr: *mut std::ffi::c_void) -> Value<'gc> {
    let p = Pointer::new(ptr);
    Gc::new_with_header_word(ctx, p, pointer_header_word()).into()
}

pub(crate) fn ensure_module<'gc>(ctx: Context<'gc>, module: &str) -> Option<Gc<'gc, Module<'gc>>> {
    let name = convert_module_name(ctx, module);
    let m = resolve_module(ctx, name, false, true)?;
    m.beautify_user_module(ctx);
    Some(m)
}
