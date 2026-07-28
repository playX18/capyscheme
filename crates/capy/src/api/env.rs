//! SNI environment and opaque reference helpers.

use std::{
    ffi::{CStr, c_void},
    marker::PhantomData,
};

use libc::c_char;

use crate::runtime::{
    Context, Scheme,
    sni::{sni_globals, sni_locals},
    value::Value,
};

/// Opaque Scheme value handle. Points into OopStorage.
pub type ScmRef = *mut Value<'static>;

/// Null reference (no object).
pub const SCM_REF_NULL: ScmRef = std::ptr::null_mut();

pub struct Scm {
    pub(crate) scheme: Scheme,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ScmPtr(pub *mut Scm);

impl ScmPtr {
    pub(crate) fn from_raw(ptr: *mut Scm) -> Self {
        Self(ptr)
    }

    pub(crate) fn scheme(&self) -> &Scheme {
        // SAFETY: caller must pass a live scm_new handle.
        unsafe { &(*self.0).scheme }
    }
}

/// One local-reference frame's slots.
pub(crate) struct LocalFrameState {
    slots: Vec<ScmRef>,
}

impl LocalFrameState {
    pub(crate) fn new() -> Self {
        Self { slots: Vec::new() }
    }

    pub(crate) fn ensure_capacity(&mut self, n: usize) {
        self.slots.reserve(n);
    }

    pub(crate) fn push_value<'gc>(&mut self, value: Value<'gc>) -> ScmRef {
        let storage = sni_locals();
        let ptr = storage.allocate().expect("SNI local allocation failed");
        // SAFETY: freshly allocated empty slot.
        unsafe {
            *ptr = std::mem::transmute::<Value<'gc>, Value<'static>>(value);
        }
        self.slots.push(ptr);
        ptr
    }

    pub(crate) fn delete(&mut self, r: ScmRef) -> bool {
        if r.is_null() {
            return false;
        }
        if let Some(i) = self.slots.iter().position(|&p| p == r) {
            self.slots.swap_remove(i);
            // SAFETY: slot was owned by this frame.
            unsafe {
                *r = Value::empty();
                sni_locals().release(r);
            }
            true
        } else {
            false
        }
    }
}

impl Drop for LocalFrameState {
    fn drop(&mut self) {
        let storage = sni_locals();
        for ptr in self.slots.drain(..) {
            // SAFETY: slots owned by this frame.
            unsafe {
                *ptr = Value::empty();
                storage.release(ptr);
            }
        }
    }
}

/// Per-enter SNI environment.
pub struct SniEnv<'gc> {
    /// Must be first for C `(*env)->Call` / `env->functions` layout.
    pub(crate) functions: *const crate::api::vtable::SNINativeInterface,
    pub(crate) ctx: Context<'gc>,
    pub(crate) frames: Vec<LocalFrameState>,
    _phantom: PhantomData<&'gc ()>,
}

impl<'gc> SniEnv<'gc> {
    pub(crate) fn new(ctx: Context<'gc>) -> Box<Self> {
        Box::new(Self {
            functions: crate::api::vtable::SNI_NATIVE_INTERFACE,
            ctx,
            frames: vec![LocalFrameState::new()],
            _phantom: PhantomData,
        })
    }

    pub(crate) fn as_ptr(&self) -> *mut SniEnv<'static> {
        // SAFETY: SniEnv is only used within scm_enter; lifetime erased for C.
        unsafe { std::mem::transmute::<*const SniEnv<'gc>, *mut SniEnv<'static>>(self as *const _) }
    }

    pub(crate) fn current_frame(&mut self) -> &mut LocalFrameState {
        if self.frames.is_empty() {
            self.frames.push(LocalFrameState::new());
        }
        self.frames.last_mut().expect("frame stack non-empty")
    }

    pub(crate) fn make_local<'a>(&'a mut self, value: Value<'gc>) -> ScmRef {
        self.current_frame().push_value(value)
    }
}

/// # Safety
/// `env` must be a live pointer from `scm_enter` / attach.
pub(crate) unsafe fn env_mut<'gc>(env: *mut SniEnv<'static>) -> &'gc mut SniEnv<'gc> {
    // SAFETY: caller guarantees live env for current enter.
    unsafe { &mut *(env as *mut SniEnv<'gc>) }
}

/// # Safety
/// `r` must be null or a live ScmRef slot.
pub(crate) unsafe fn ref_value<'gc>(r: ScmRef) -> Value<'gc> {
    if r.is_null() {
        return Value::null();
    }
    // SAFETY: slot is a rooted OopStorage entry.
    unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*r) }
}

pub(crate) fn global_new<'gc>(value: Value<'gc>) -> ScmRef {
    let storage = sni_globals();
    let ptr = storage.allocate().expect("SNI global allocation failed");
    // SAFETY: freshly allocated empty slot.
    unsafe {
        *ptr = std::mem::transmute::<Value<'gc>, Value<'static>>(value);
    }
    ptr
}

pub(crate) unsafe fn global_delete(r: ScmRef) {
    if r.is_null() {
        return;
    }
    // SAFETY: caller must pass a global ref from sni_new_global_ref.
    unsafe {
        *r = Value::empty();
        sni_globals().release(r);
    }
}

/// Safely convert a C string pointer to a Rust `&str`.
///
/// # Safety
/// `ptr` (if non-null) must point to a valid NUL-terminated C string.
pub(crate) unsafe fn safe_cstr<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

/// Re-export for modules that need opaque void* casting.
pub(crate) type VoidPtr = *mut c_void;
