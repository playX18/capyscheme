//! Shared helpers for SNI API modules.

use crate::prelude::*;
use crate::runtime::hooks::Hook;
use crate::runtime::sni;
use crate::runtime::vm::ExecutionResult;
use crate::runtime::vm::ffi::{Pointer, pointer_header_word};

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, ref_value};

/// Calls the native `(#%capy)` procedure `name`.
pub(crate) fn call_public<'gc>(
    env: &mut SniEnv<'gc>,
    name: &str,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ScmRef {
    call_library(env, "capy", name, args)
}

/// Calls the procedure `name` exported by the library `library` (space separated, as in
/// `"boot records"`).
pub(crate) fn call_library<'gc>(
    env: &mut SniEnv<'gc>,
    library: &str,
    name: &str,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ScmRef {
    let Some(proc) = env.ctx.public_ref(library, name) else {
        return SCM_REF_NULL;
    };
    match sni::call_function(env.ctx, proc, args) {
        ExecutionResult::Ok(v) => env.make_local(v),
        ExecutionResult::Err(_) => SCM_REF_NULL,
    }
}

/// Calls the procedure Scheme installed for `hook`. Returns null before boot installed it.
pub(crate) fn call_hook<'gc>(
    env: &mut SniEnv<'gc>,
    hook: Hook,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ScmRef {
    let Some(proc) = env.ctx.try_hook(hook) else {
        return SCM_REF_NULL;
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

pub(crate) fn make_pointer_value<'gc>(
    ctx: MutationContext<'gc>,
    ptr: *mut std::ffi::c_void,
) -> Value<'gc> {
    let p = Pointer::new(ptr);
    ctx.allocate_with(p, pointer_header_word()).into()
}
