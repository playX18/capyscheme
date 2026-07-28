//! Call Scheme procedures and look up module bindings.

use std::ffi::c_void;

use libc::c_char;

use crate::runtime::{
    sni,
    value::{Closure, Value},
    vm::ExecutionResult,
};

use super::env::{SCM_REF_NULL, ScmPtr, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::exceptions::set_type_error;

/// Look up a public binding; returns a new local ref or `default_value`.
///
/// # Safety
/// `module_name` / `name` must be valid C strings when non-null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_public_ref(
    env: *mut SniEnv<'static>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: ScmRef,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let (Some(m), Some(n)) = (unsafe { (safe_cstr(module_name), safe_cstr(name)) }) else {
        return if default_value.is_null() {
            SCM_REF_NULL
        } else {
            unsafe { sni_new_local_ref_helper(e, default_value) }
        };
    };
    match e.ctx.public_ref(m, n) {
        Some(v) => e.make_local(v),
        None => {
            if default_value.is_null() {
                SCM_REF_NULL
            } else {
                unsafe { sni_new_local_ref_helper(e, default_value) }
            }
        }
    }
}

/// Look up a private binding; returns a new local ref or `default_value`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_private_ref(
    env: *mut SniEnv<'static>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: ScmRef,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let (Some(m), Some(n)) = (unsafe { (safe_cstr(module_name), safe_cstr(name)) }) else {
        return if default_value.is_null() {
            SCM_REF_NULL
        } else {
            unsafe { sni_new_local_ref_helper(e, default_value) }
        };
    };
    match e.ctx.private_ref(m, n) {
        Some(v) => e.make_local(v),
        None => {
            if default_value.is_null() {
                SCM_REF_NULL
            } else {
                unsafe { sni_new_local_ref_helper(e, default_value) }
            }
        }
    }
}

unsafe fn sni_new_local_ref_helper<'gc>(e: &mut SniEnv<'gc>, obj: ScmRef) -> ScmRef {
    let v = unsafe { ref_value(obj) };
    e.make_local(v)
}

/// Call procedure `proc` with `argc` arguments in `argv`.
///
/// Returns a new local ref on success. On error, sets pending exception and
/// returns null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_call_n(
    env: *mut SniEnv<'static>,
    proc: ScmRef,
    argc: i32,
    argv: *const ScmRef,
) -> ScmRef {
    if env.is_null() || proc.is_null() || argc < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rator = unsafe { ref_value(proc) };
    if !rator.is::<Closure>() {
        set_type_error(e, "sni_call_n", "procedure", rator);
        return SCM_REF_NULL;
    }
    let mut args = Vec::with_capacity(argc as usize);
    for i in 0..argc as usize {
        let r = unsafe { *argv.add(i) };
        args.push(unsafe { ref_value(r) });
    }
    match sni::call_function(e.ctx, rator, args) {
        ExecutionResult::Ok(v) => e.make_local(v),
        ExecutionResult::Err(_) => SCM_REF_NULL,
    }
}

/// Convenience: call with 0–3 args without an argv array.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_call(
    env: *mut SniEnv<'static>,
    proc: ScmRef,
    a0: ScmRef,
    a1: ScmRef,
    a2: ScmRef,
    nargs: i32,
) -> ScmRef {
    let mut buf = [SCM_REF_NULL; 3];
    let n = nargs.clamp(0, 3) as usize;
    if n > 0 {
        buf[0] = a0;
    }
    if n > 1 {
        buf[1] = a1;
    }
    if n > 2 {
        buf[2] = a2;
    }
    unsafe { sni_call_n(env, proc, n as i32, buf.as_ptr()) }
}

/// Call a named public procedure from outside enter (boots a temporary env).
///
/// `prepare` builds an argument list (consed into `*args` as a Scheme list).
/// `finish` receives success/failure and the result local-ref equivalent value.
pub type PrepareCallFn =
    unsafe extern "C" fn(env: *mut SniEnv<'static>, args: *mut ScmRef, data: *mut c_void);

pub type FinishCallFn = unsafe extern "C" fn(
    env: *mut SniEnv<'static>,
    success: bool,
    result: ScmRef,
    data: *mut c_void,
) -> libc::c_int;

/// # Safety
/// Module/func names must be valid C strings; callbacks must be valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_call(
    scm: ScmPtr,
    mod_name: *const c_char,
    func_name: *const c_char,
    prepare: PrepareCallFn,
    data1: *mut c_void,
    finish: FinishCallFn,
    data2: *mut c_void,
) -> libc::c_int {
    if scm.0.is_null() {
        return -1;
    }
    let Some(mod_name) = (unsafe { safe_cstr(mod_name) }) else {
        return -1;
    };
    let Some(func_name) = (unsafe { safe_cstr(func_name) }) else {
        return -1;
    };
    scm.scheme().call(
        mod_name,
        func_name,
        |ctx, args| {
            let mut env = SniEnv::new(ctx);
            let env_ptr = env.as_ptr();
            let mut list_ref = SCM_REF_NULL;
            // SAFETY: prepare from caller.
            unsafe { prepare(env_ptr, &mut list_ref, data1) };
            let mut ls = if list_ref.is_null() {
                Value::null()
            } else {
                unsafe { ref_value(list_ref) }
            };
            while !ls.is_null() {
                args.push(ls.car());
                ls = ls.cdr();
            }
        },
        |ctx, result| {
            let mut env = SniEnv::new(ctx);
            let env_ptr = env.as_ptr();
            let (succ, val) = match result {
                Ok(v) => (true, v),
                Err(e) => (false, e),
            };
            let result_ref = env.make_local(val);
            unsafe { finish(env_ptr, succ, result_ref, data2) }
        },
    )
}

/// Leave the mutator to run a blocking native callback.
///
/// Values needed across the call must already be in global refs or root storage.
pub type NativeCallback = unsafe extern "C" fn(data: *mut c_void);

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_call_in_native(
    env: *mut SniEnv<'static>,
    callback: NativeCallback,
    data: *mut c_void,
) {
    if env.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    e.ctx.call_in_native(|| {
        // SAFETY: callback from caller.
        unsafe { callback(data) }
    });
}
