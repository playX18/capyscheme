//! Native procedure registration and module `define`.

use libc::c_char;

use crate::prelude::*;
use crate::runtime::vm::ffi::Pointer;

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::util::{ensure_module, make_pointer_value};

/// C callback for SNI-registered native procedures.
pub type SniNativeFn =
    unsafe extern "C" fn(env: *mut SniEnv<'static>, argc: i32, argv: *const ScmRef) -> ScmRef;

/// Trampoline: Scheme NativeFn -> SniNativeFn stored in free[1] as Pointer.
///
/// Scheme code that the callback runs goes through `call_scheme`, which catches
/// its raises. No raise unwinds this frame. The error is left as the pending
/// exception, and it is checked once `fn_ptr` returns.
extern "C" fn sni_native_trampoline<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        let closure = call.rator.downcast::<Closure>();
        // free[0] = NativeProc wrapper, free[1] = Pointer to SniNativeFn
        if closure.nfree < 2 {
            call.err(Value::null());
            return call.finish_flagged();
        }
        let ptr_val = closure[1].get();
        let Some(ptr) = ptr_val.try_as::<Pointer>() else {
            call.err(Value::null());
            return call.finish_flagged();
        };
        let raw = ptr.value();
        if raw.is_null() {
            call.err(Value::null());
            return call.finish_flagged();
        }
        let fn_ptr: SniNativeFn = unsafe { std::mem::transmute(raw) };

        let mut env = SniEnv::new(ctx);
        let env_ptr = env.as_ptr();
        let mut arg_refs: Vec<ScmRef> = Vec::with_capacity(call.args.len());
        for &v in call.args {
            arg_refs.push(env.make_local(v));
        }

        let result_ref = unsafe { fn_ptr(env_ptr, call.args.len() as i32, arg_refs.as_ptr()) };

        if crate::runtime::sni::exception::check() {
            let err = crate::runtime::sni::exception::occurred().unwrap_or(Value::null());
            call.err(err);
            return call.finish_flagged();
        }

        let result = if result_ref.is_null() {
            Value::unspecified()
        } else {
            unsafe { ref_value(result_ref) }
        };
        call.run(|_| result)
    })
}

/// Create a Scheme procedure that invokes `fn` with an SNI environment.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_new_native_procedure(
    env: *mut SniEnv<'static>,
    f: Option<SniNativeFn>,
) -> ScmRef {
    let Some(f) = f else {
        return SCM_REF_NULL;
    };
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let ptr_val = make_pointer_value(e.ctx, f as *mut std::ffi::c_void);
    let closure = e.ctx.make_native_closure(
        sni_native_trampoline as NativeFn<'_>,
        [ptr_val],
        Value::null(),
    );
    e.make_local(closure.into())
}

/// Define and export `name` in `module` (space-separated module path).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_define(
    env: *mut SniEnv<'static>,
    module: *const c_char,
    name: *const c_char,
    value: ScmRef,
) -> libc::c_int {
    if env.is_null() || value.is_null() {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    let Some(module) = (unsafe { safe_cstr(module) }) else {
        return -1;
    };
    let Some(name) = (unsafe { safe_cstr(name) }) else {
        return -1;
    };
    let Some(m) = ensure_module(e.ctx, module) else {
        return -1;
    };
    let sym = Symbol::from_str(e.ctx, name);
    let val = unsafe { ref_value(value) };
    m.define(e.ctx, sym.into(), val);
    m.export_one(e.ctx, sym.into());
    0
}
