//! C API for Capy runtime.
//!
//! Exports most of the runtime functionality to C-compatible interface.

use std::{
    ffi::{CStr, c_void},
    marker::PhantomData,
    sync::Arc,
};

use libc::c_char;

use crate::{
    prelude::Value,
    runtime::{Context, Scheme, vm::threading::ThreadObject},
};

pub struct Scm {
    #[allow(dead_code)]
    scheme: Scheme,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ScmRef(pub *mut Scm);

impl std::ops::Deref for ScmRef {
    type Target = Scm;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref().unwrap() }
    }
}

impl std::ops::DerefMut for ScmRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut().unwrap() }
    }
}
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ContextRef<'gc>(pub *mut c_void, PhantomData<&'gc Context<'gc>>);

impl<'gc> std::ops::Deref for ContextRef<'gc> {
    type Target = crate::runtime::Context<'gc>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.cast::<Context<'gc>>().as_ref().unwrap() }
    }
}

/// Create a new Scheme thread instance.
///
/// The thread instance will be attached to currently active VM or
/// will initialize a new VM if there isn't any.    
///
/// For thread creation look at [`scm_fork()`](scm_fork).
#[unsafe(no_mangle)]
pub extern "C" fn scm_new() -> ScmRef {
    let scm = Scheme::new();
    let capy = Scm { scheme: scm };
    ScmRef(Box::into_raw(Box::new(capy)))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_free(scm: ScmRef) {
    unsafe {
        let _ = Box::from_raw(scm.0);
    }
}

pub type ThreadFn =
    unsafe extern "C" fn(parent: ScmRef, arg: *mut std::ffi::c_void) -> *mut std::ffi::c_void;

/// Fork a new Scheme thread from the given parent thread.
///
/// New thread is created with a copy of the parent's dynamic state.
///
/// Returns thread-object for the newly created thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_fork<'gc>(
    parent: ContextRef<'gc>,
    init: ThreadFn,
    arg: *mut std::ffi::c_void,
) -> Value<'gc> {
    let thread_object = ThreadObject::new(&parent, None);
    let dynamic_state = parent.dynamic_state().bits();
    let thread_object_bits = thread_object.as_ptr() as u64;
    let thread_spawned = Arc::new(std::sync::Barrier::new(2));
    let thread_spawned_clone = thread_spawned.clone();
    let arg = arg as usize;
    std::thread::spawn(move || {
        let scm = Scheme::forked(thread_object_bits, dynamic_state);
        thread_spawned_clone.wait();
        let parent_ptr = ScmRef(Box::into_raw(Box::new(Scm { scheme: scm })));
        unsafe { init(parent_ptr, arg as _) };
        let _ = unsafe { Box::from_raw(parent_ptr.0) };
    });

    thread_object.into()
}

pub type ScmEnterFn<'gc> =
    unsafe extern "C" fn(ctx: ContextRef<'gc>, arg: *mut std::ffi::c_void) -> libc::c_int;

/// Enter the Scheme runtime with the given Scheme instance.
///
/// This function will set up the necessary runtime state and call the provided
/// `enter` function with a context reference and the provided argument.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_enter<'gc>(
    scm: ScmRef,
    enter: ScmEnterFn<'gc>,
    arg: *mut std::ffi::c_void,
) -> libc::c_int {
    let scheme = &scm.scheme;
    let res = scheme.enter(|ctx| unsafe {
        let ctxref = ContextRef(&ctx as *const _ as *mut _, PhantomData);
        enter(ctxref, arg)
    });

    res
}

pub extern "C" fn scm_public_ref<'gc>(
    ctx: &Context<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    let module_name = unsafe { CStr::from_ptr(module_name).to_str().unwrap() };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };

    ctx.public_ref(module_name, name).unwrap_or(default_value)
}

pub extern "C" fn scm_private_ref<'gc>(
    ctx: &Context<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    let module_name = unsafe { CStr::from_ptr(module_name).to_str().unwrap() };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };

    ctx.private_ref(module_name, name).unwrap_or(default_value)
}
