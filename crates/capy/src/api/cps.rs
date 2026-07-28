//! Continuation trampoline for SNI.
//!
//! `SniContinuation.result` is a **continuation-owned** strong root in
//! `sni_globals` OopStorage (allocated in [`sni_cont_alloc`], released in
//! [`sni_cont_free`]). Callers must not replace the `result` pointer; store
//! values with [`sni_cont_set_result`] / [`sni_call_k`] / [`sni_raise_k`].
//! [`sni_trampoline`] returns a **new local ref** of the final value.

use std::ptr;

use crate::runtime::sni::sni_globals;
use crate::runtime::value::Value;

use super::call::sni_call_n;
use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value};
use super::raise::{sni_raise, sni_with_exception_handler};

/// Resume function: return updated continuation (same pointer OK).
pub type SniContFn = unsafe extern "C" fn(
    env: *mut SniEnv<'static>,
    c: *mut SniContinuation,
) -> *mut SniContinuation;

pub type SniDropDataFn = unsafe extern "C" fn(data: *mut libc::c_void);

#[repr(C)]
pub struct SniContinuation {
    pub next: Option<SniContFn>,
    /// Continuation-owned OopStorage slot (`sni_globals`). Never null after alloc.
    pub result: ScmRef,
    pub data: *mut libc::c_void,
    pub drop_data: Option<SniDropDataFn>,
}

fn alloc_result_slot() -> ScmRef {
    let storage = sni_globals();
    let ptr = storage
        .allocate()
        .expect("SNI continuation result root allocation failed");
    // SAFETY: freshly allocated empty slot.
    unsafe {
        *ptr = Value::null();
    }
    ptr
}

unsafe fn release_result_slot(slot: ScmRef) {
    if slot.is_null() {
        return;
    }
    // SAFETY: slot owned by the continuation; allocated from sni_globals.
    unsafe {
        *slot = Value::empty();
        sni_globals().release(slot);
    }
}

/// Write `value` into the continuation's owned result root.
///
/// # Safety
/// `c` must be a live continuation from [`sni_cont_alloc`].
pub(crate) unsafe fn cont_store_value<'gc>(c: *mut SniContinuation, value: Value<'gc>) {
    if c.is_null() {
        return;
    }
    let slot = unsafe { (*c).result };
    if slot.is_null() {
        return;
    }
    // SAFETY: `result` is a live sni_globals slot for this cont.
    unsafe {
        *slot = std::mem::transmute::<Value<'gc>, Value<'static>>(value);
    }
}

/// Copy the Scheme object from `src` into the continuation's owned result root.
///
/// # Safety
/// `c` live; `src` null or a live ScmRef.
pub(crate) unsafe fn cont_store_ref(c: *mut SniContinuation, src: ScmRef) {
    let v = unsafe { ref_value(src) };
    unsafe { cont_store_value(c, v) };
}

/// Store into `c->result` (owned global root). Does not replace the slot pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_cont_set_result(
    _env: *mut SniEnv<'static>,
    c: *mut SniContinuation,
    value: ScmRef,
) {
    if c.is_null() {
        return;
    }
    unsafe { cont_store_ref(c, value) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_cont_alloc(
    _env: *mut SniEnv<'static>,
    start: Option<SniContFn>,
    data: *mut libc::c_void,
    drop_data: Option<SniDropDataFn>,
) -> *mut SniContinuation {
    let c = Box::new(SniContinuation {
        next: start,
        result: alloc_result_slot(),
        data,
        drop_data,
    });
    Box::into_raw(c)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_cont_free(_env: *mut SniEnv<'static>, c: *mut SniContinuation) {
    if c.is_null() {
        return;
    }
    // SAFETY: allocated by sni_cont_alloc.
    unsafe {
        let mut boxed = Box::from_raw(c);
        if let Some(drop_fn) = boxed.drop_data {
            if !boxed.data.is_null() {
                drop_fn(boxed.data);
                boxed.data = ptr::null_mut();
            }
        }
        release_result_slot(boxed.result);
        boxed.result = SCM_REF_NULL;
        drop(boxed);
    }
}

/// Run `c` until `next` is null. Returns a **new local ref** of the final result.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_trampoline(
    env: *mut SniEnv<'static>,
    c: *mut SniContinuation,
) -> ScmRef {
    if env.is_null() || c.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let mut cur = c;
    // SAFETY: cont graph owned by caller for the duration of the trampoline.
    unsafe {
        loop {
            let Some(next) = (*cur).next else {
                let v = ref_value((*cur).result);
                return e.make_local(v);
            };
            let resumed = next(env, cur);
            if resumed.is_null() {
                return SCM_REF_NULL;
            }
            cur = resumed;
        }
    }
}

/// Call `proc`; store outcome in `c`'s owned result root. Does not advance `c->next`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_call_k(
    env: *mut SniEnv<'static>,
    proc: ScmRef,
    argc: i32,
    argv: *const ScmRef,
    c: *mut SniContinuation,
) {
    if env.is_null() || c.is_null() {
        return;
    }
    let r = unsafe { sni_call_n(env, proc, argc, argv) };
    // Copy into cont-owned root so the value survives local-frame pop.
    unsafe { cont_store_ref(c, r) };
}

/// Raise `obj` into Scheme; store outcome in `c`'s owned result root.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_raise_k(
    env: *mut SniEnv<'static>,
    obj: ScmRef,
    c: *mut SniContinuation,
) {
    if env.is_null() || c.is_null() {
        return;
    }
    let r = unsafe { sni_raise(env, obj) };
    unsafe { cont_store_ref(c, r) };
}

struct GuardCtx {
    handler: *mut SniContinuation,
    body: *mut SniContinuation,
}

unsafe extern "C" fn guard_handler_cb(
    env: *mut SniEnv<'static>,
    exn: ScmRef,
    data: *mut libc::c_void,
) -> ScmRef {
    let ctx = unsafe { &*(data as *const GuardCtx) };
    if ctx.handler.is_null() {
        return SCM_REF_NULL;
    }
    unsafe {
        cont_store_ref(ctx.handler, exn);
        sni_trampoline(env, ctx.handler)
    }
}

unsafe extern "C" fn guard_body_cb(env: *mut SniEnv<'static>, data: *mut libc::c_void) -> ScmRef {
    let ctx = unsafe { &*(data as *const GuardCtx) };
    if ctx.body.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { sni_trampoline(env, ctx.body) }
}

/// Run `body` under `handler` via `with-exception-handler`.
/// Returns `body` (finished) with its owned result filled.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_guard_k(
    env: *mut SniEnv<'static>,
    handler: *mut SniContinuation,
    body: *mut SniContinuation,
) -> *mut SniContinuation {
    if env.is_null() || body.is_null() {
        return ptr::null_mut();
    }
    let mut ctx = GuardCtx { handler, body };
    let result = unsafe {
        sni_with_exception_handler(
            env,
            Some(guard_handler_cb),
            &mut ctx as *mut _ as *mut libc::c_void,
            Some(guard_body_cb),
            &mut ctx as *mut _ as *mut libc::c_void,
        )
    };
    unsafe {
        cont_store_ref(body, result);
        (*body).next = None;
    }
    body
}
