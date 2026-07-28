//! Lifecycle: scm_new / free / enter / fork / attach.

use std::{marker::PhantomData, sync::Arc};

use libc::c_void;

use crate::runtime::{Context, Scheme, value::Value, vm::threading::ThreadObject};

use super::env::{SCM_REF_NULL, Scm, ScmPtr, ScmRef, SniEnv, env_mut};

/// Create a new Scheme runtime instance (boots the VM).
#[unsafe(no_mangle)]
pub extern "C" fn scm_new() -> *mut Scm {
    let scm = Scheme::new();
    Box::into_raw(Box::new(Scm { scheme: scm }))
}

/// Unimplemented heap-image resume; returns a null handle.
#[unsafe(no_mangle)]
pub extern "C" fn scm_from_image(_image_data: *const u8, _image_size: usize) -> *mut Scm {
    std::ptr::null_mut()
}

/// Free a Scheme instance from [`scm_new`].
///
/// # Safety
/// `scm` must be a non-null handle from `scm_new` that has not been freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_free(scm: *mut Scm) {
    if scm.is_null() {
        return;
    }
    // SAFETY: allocated by scm_new.
    unsafe {
        let _ = Box::from_raw(scm);
    }
}

pub type ScmEnterFn =
    unsafe extern "C" fn(env: *mut SniEnv<'static>, arg: *mut c_void) -> libc::c_int;

/// Enter the mutator and invoke `enter` with a fresh [`SniEnv`] (initial local frame).
///
/// # Safety
/// `scm` must be live; `enter` must not retain `env` after return.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_enter(
    scm: ScmPtr,
    enter: ScmEnterFn,
    arg: *mut c_void,
) -> libc::c_int {
    if scm.0.is_null() {
        return -1;
    }
    let scheme = scm.scheme();
    scheme.enter(|ctx| {
        let mut env = SniEnv::new(ctx);
        let env_ptr = env.as_ptr();
        // SAFETY: env lives for this callback.
        unsafe { enter(env_ptr, arg) }
    })
}

/// Alias for attach: same as enter for the creating thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_attach_current_thread(
    scm: ScmPtr,
    enter: ScmEnterFn,
    arg: *mut c_void,
) -> libc::c_int {
    unsafe { scm_enter(scm, enter, arg) }
}

/// No-op detach placeholder (threads are scoped to enter).
#[unsafe(no_mangle)]
pub extern "C" fn scm_detach_current_thread(_scm: ScmPtr) {}

pub type ThreadFn = unsafe extern "C" fn(scm: ScmPtr, arg: *mut c_void) -> *mut c_void;

/// Fork a new Scheme thread from the parent env's dynamic state.
///
/// Returns a local ref to the thread object, or null on failure.
///
/// # Safety
/// `env` must be live; `init` must be a valid function pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_fork(
    env: *mut SniEnv<'static>,
    init: ThreadFn,
    arg: *mut c_void,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    // SAFETY: live env.
    let e = unsafe { env_mut(env) };
    let thread_object = ThreadObject::new(*e.ctx, None);
    let dynamic_state = e.ctx.dynamic_state().bits();
    let thread_object_bits = thread_object.as_ptr() as u64;
    let thread_spawned = Arc::new(std::sync::Barrier::new(2));
    let thread_spawned_clone = thread_spawned.clone();
    let arg = arg as usize;
    std::thread::spawn(move || {
        let scm = Scheme::forked(thread_object_bits, dynamic_state);
        thread_spawned_clone.wait();
        let parent_ptr = ScmPtr(Box::into_raw(Box::new(Scm { scheme: scm })));
        // SAFETY: init provided by caller.
        unsafe { init(parent_ptr, arg as _) };
        unsafe {
            let _ = Box::from_raw(parent_ptr.0);
        }
    });
    e.make_local(thread_object.into())
}

/// Get the underlying Context pointer (advanced / internal).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_get_context(env: *mut SniEnv<'static>) -> *const c_void {
    if env.is_null() {
        return std::ptr::null();
    }
    let e = unsafe { env_mut(env) };
    e.ctx.as_ptr() as *const c_void
}

// Silence unused import in case Context helpers shift.
const _: PhantomData<Context<'static>> = PhantomData;
const _: PhantomData<Value<'static>> = PhantomData;
