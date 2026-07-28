//! R6RS raise / with-exception-handler / condition helpers.

use libc::c_char;

use crate::prelude::*;

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::native::{SniNativeFn, sni_new_native_procedure};
use super::util::{call_public, call_root, values_from_refs};

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_raise(env: *mut SniEnv<'static>, obj: ScmRef) -> ScmRef {
    if env.is_null() || obj.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    call_root(e, "raise", [unsafe { ref_value(obj) }])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_raise_continuable(env: *mut SniEnv<'static>, obj: ScmRef) -> ScmRef {
    if env.is_null() || obj.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    call_root(e, "raise-continuable", [unsafe { ref_value(obj) }])
}

pub type SniThunk =
    unsafe extern "C" fn(env: *mut SniEnv<'static>, data: *mut libc::c_void) -> ScmRef;
pub type SniExceptionHandler =
    unsafe extern "C" fn(env: *mut SniEnv<'static>, exn: ScmRef, data: *mut libc::c_void) -> ScmRef;

#[derive(Clone, Copy)]
struct HandlerData {
    handler: SniExceptionHandler,
    data: *mut libc::c_void,
}

#[derive(Clone, Copy)]
struct ThunkData {
    thunk: SniThunk,
    data: *mut libc::c_void,
}

unsafe extern "C" fn handler_trampoline(
    env: *mut SniEnv<'static>,
    argc: i32,
    argv: *const ScmRef,
) -> ScmRef {
    // argv[0] = exception; userdata is not available this way.
    // We store HandlerData pointer in a thread-local set by with_exception_handler.
    HANDLER.with(|cell| {
        let Some(hd) = *cell.borrow() else {
            return SCM_REF_NULL;
        };
        let exn = if argc > 0 {
            unsafe { *argv }
        } else {
            SCM_REF_NULL
        };
        unsafe { (hd.handler)(env, exn, hd.data) }
    })
}

unsafe extern "C" fn thunk_trampoline(
    env: *mut SniEnv<'static>,
    _argc: i32,
    _argv: *const ScmRef,
) -> ScmRef {
    THUNK.with(|cell| {
        let Some(td) = *cell.borrow() else {
            return SCM_REF_NULL;
        };
        unsafe { (td.thunk)(env, td.data) }
    })
}

thread_local! {
    static HANDLER: std::cell::RefCell<Option<HandlerData>> = const { std::cell::RefCell::new(None) };
    static THUNK: std::cell::RefCell<Option<ThunkData>> = const { std::cell::RefCell::new(None) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_with_exception_handler(
    env: *mut SniEnv<'static>,
    handler: Option<SniExceptionHandler>,
    handler_data: *mut libc::c_void,
    body: Option<SniThunk>,
    body_data: *mut libc::c_void,
) -> ScmRef {
    if env.is_null() || handler.is_none() || body.is_none() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let handler = handler.unwrap();
    let body = body.unwrap();

    HANDLER.with(|cell| {
        *cell.borrow_mut() = Some(HandlerData {
            handler,
            data: handler_data,
        });
    });
    THUNK.with(|cell| {
        *cell.borrow_mut() = Some(ThunkData {
            thunk: body,
            data: body_data,
        });
    });

    let handler_proc =
        unsafe { sni_new_native_procedure(env, Some(handler_trampoline as SniNativeFn)) };
    let body_proc = unsafe { sni_new_native_procedure(env, Some(thunk_trampoline as SniNativeFn)) };
    if handler_proc.is_null() || body_proc.is_null() {
        return SCM_REF_NULL;
    }

    let result = call_root(
        e,
        "with-exception-handler",
        [unsafe { ref_value(handler_proc) }, unsafe {
            ref_value(body_proc)
        }],
    );

    HANDLER.with(|cell| *cell.borrow_mut() = None);
    THUNK.with(|cell| *cell.borrow_mut() = None);
    result
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_condition(
    env: *mut SniEnv<'static>,
    n: i32,
    components: *const ScmRef,
) -> ScmRef {
    if env.is_null() || n < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let args = values_from_refs(components, n as usize);
    call_public(e, "condition", args)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_message_condition(
    env: *mut SniEnv<'static>,
    msg: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let Some(msg) = (unsafe { safe_cstr(msg) }) else {
        return SCM_REF_NULL;
    };
    let s = e.ctx.str(msg);
    call_public(e, "make-message-condition", [s])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_who_condition(
    env: *mut SniEnv<'static>,
    who: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let Some(who) = (unsafe { safe_cstr(who) }) else {
        return SCM_REF_NULL;
    };
    let sym = Symbol::from_str(e.ctx, who);
    call_public(e, "make-who-condition", [sym.into()])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_irritants_condition(
    env: *mut SniEnv<'static>,
    n: i32,
    irritants: *const ScmRef,
) -> ScmRef {
    if env.is_null() || n < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let mut list = Value::null();
    for i in (0..n as usize).rev() {
        let r = unsafe { *irritants.add(i) };
        list = Value::cons(e.ctx, unsafe { ref_value(r) }, list);
    }
    call_public(e, "make-irritants-condition", [list])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_assertion_violation(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    call_public(e, "make-assertion-violation", [])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_error(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    call_public(e, "make-error", [])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_assertion_violation(
    env: *mut SniEnv<'static>,
    who: *const c_char,
    message: *const c_char,
    n: i32,
    irritants: *const ScmRef,
) -> () {
    if env.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let who_s = unsafe { safe_cstr(who) }.unwrap_or("native");
    let msg_s = unsafe { safe_cstr(message) }.unwrap_or("assertion violation");
    let who_v = Symbol::from_str(e.ctx, who_s).into();
    let msg_v = e.ctx.str(msg_s);
    let mut args = vec![who_v, msg_v];
    if n > 0 && !irritants.is_null() {
        for i in 0..n as usize {
            args.push(unsafe { ref_value(*irritants.add(i)) });
        }
    }
    let _ = call_root(e, "assertion-violation", args);
}
