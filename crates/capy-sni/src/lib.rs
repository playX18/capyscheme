//! Safe Scheme Native Interface over [`capy_sni_sys`].

mod continuation;
mod conversions;
mod pod;

use std::any::Any;
use std::ffi::{CString, OsString};
use std::marker::PhantomData;
use std::os::unix::ffi::OsStrExt;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::ptr;

use capy_sni_sys as sys;

pub use continuation::{__cps, Continuation, cps_ops};
pub use conversions::{Arity, ConversionError, FromScmArgs, IntoScm, TryFromScm};
pub use pod::PodRecord;
pub use sys::ScmRef;

/// Colorless stubs (`cps::call` / `raise` / `guard`); rewritten inside `#[cps]`.
pub use continuation::cps_ops as cps;

#[cfg(feature = "derive")]
pub use capy_sni_derive::cps;
#[cfg(feature = "derive")]
pub use capy_sni_derive::onload;
#[cfg(feature = "derive")]
pub use capy_sni_derive::scheme;

/// Owned Scheme runtime (`scm_new` / `scm_free`).
pub struct Scm {
    ptr: *mut sys::Scm,
}

impl Scm {
    pub fn new() -> Self {
        // SAFETY: ffi constructor; null checked below.
        let ptr = unsafe { sys::scm_new() };
        assert!(!ptr.is_null(), "scm_new failed");
        Self { ptr }
    }

    pub fn as_ptr(&self) -> *mut sys::Scm {
        self.ptr
    }

    /// Enter the mutator and run `f` with an [`Env`].
    ///
    /// The env is branded with an anonymous lifetime so neither it nor any
    /// [`Ref`] obtained from it can escape the callback. A panic in `f` is
    /// caught at the FFI boundary and resumed once `scm_enter` has returned
    /// to Rust.
    pub fn enter<F, R>(&self, f: F) -> R
    where
        F: for<'env> FnOnce(&mut Env<'env>) -> R,
    {
        struct Data<F, R> {
            f: Option<F>,
            result: Option<R>,
            panic: Option<Box<dyn Any + Send + 'static>>,
        }
        let mut data = Data {
            f: Some(f),
            result: None,
            panic: None,
        };
        unsafe extern "C" fn trampoline<F, R>(
            env: *mut sys::SniEnv,
            arg: *mut libc::c_void,
        ) -> libc::c_int
        where
            F: for<'env> FnOnce(&mut Env<'env>) -> R,
        {
            // SAFETY: arg is the `&mut Data` passed to scm_enter below, alive
            // for the whole call; env is live for the duration of this callback.
            let data = unsafe { &mut *(arg as *mut Data<F, R>) };
            if let Some(f) = data.f.take() {
                let outcome = catch_unwind(AssertUnwindSafe(|| {
                    // SAFETY: env is a live SniEnv for this enter callback.
                    let mut env = unsafe { Env::from_raw(env) };
                    f(&mut env)
                }));
                match outcome {
                    Ok(r) => data.result = Some(r),
                    Err(p) => data.panic = Some(p),
                }
            }
            0
        }
        // SAFETY: data is stack-local and scm_enter runs the callback
        // synchronously before returning.
        unsafe {
            sys::scm_enter(
                self.ptr,
                Some(trampoline::<F, R>),
                &mut data as *mut _ as *mut libc::c_void,
            );
        }
        if let Some(p) = data.panic {
            resume_unwind(p);
        }
        data.result.expect("scm_enter did not run callback")
    }

    /// Call a named public procedure with prepare/finish hooks.
    ///
    /// Returns -1 without calling when `module` or `name` contains an
    /// interior NUL byte. A panic in either hook is caught at the FFI
    /// boundary and resumed once `scm_call` has returned to Rust.
    pub fn call_named<P, Fin>(&self, module: &str, name: &str, prepare: P, finish: Fin) -> i32
    where
        P: for<'env> FnOnce(&mut Env<'env>, &mut Ref<'env>),
        Fin: for<'env> FnOnce(&mut Env<'env>, bool, Ref<'env>) -> i32,
    {
        struct Data<P, Fin> {
            prepare: Option<P>,
            finish: Option<Fin>,
            code: i32,
            panic: Option<Box<dyn Any + Send + 'static>>,
        }
        let mut data = Data {
            prepare: Some(prepare),
            finish: Some(finish),
            code: -1,
            panic: None,
        };
        let (mod_c, name_c) = match (CString::new(module), CString::new(name)) {
            (Ok(m), Ok(n)) => (m, n),
            _ => return -1,
        };

        unsafe extern "C" fn prepare_cb<P, Fin>(
            env: *mut sys::SniEnv,
            args: *mut ScmRef,
            data: *mut libc::c_void,
        ) where
            P: for<'env> FnOnce(&mut Env<'env>, &mut Ref<'env>),
            Fin: for<'env> FnOnce(&mut Env<'env>, bool, Ref<'env>) -> i32,
        {
            // SAFETY: data is the `&mut Data` passed to scm_call below; env is
            // live for this callback; args points to one rooted ref slot
            // provided by scm_call (Ref is repr(transparent) over ScmRef).
            let data = unsafe { &mut *(data as *mut Data<P, Fin>) };
            if let Some(p) = data.prepare.take() {
                let outcome = catch_unwind(AssertUnwindSafe(|| {
                    let mut env = unsafe { Env::from_raw(env) };
                    let args = unsafe { &mut *(args as *mut Ref<'_>) };
                    p(&mut env, args)
                }));
                if let Err(panicked) = outcome {
                    data.panic = Some(panicked);
                }
            }
        }
        unsafe extern "C" fn finish_cb<P, Fin>(
            env: *mut sys::SniEnv,
            success: bool,
            result: ScmRef,
            data: *mut libc::c_void,
        ) -> libc::c_int
        where
            P: for<'env> FnOnce(&mut Env<'env>, &mut Ref<'env>),
            Fin: for<'env> FnOnce(&mut Env<'env>, bool, Ref<'env>) -> i32,
        {
            // SAFETY: same as prepare_cb; result is a rooted ref owned by scm_call.
            let data = unsafe { &mut *(data as *mut Data<P, Fin>) };
            if let Some(f) = data.finish.take() {
                let outcome = catch_unwind(AssertUnwindSafe(|| {
                    let mut env = unsafe { Env::from_raw(env) };
                    let result = unsafe { Ref::from_raw(result) };
                    f(&mut env, success, result)
                }));
                match outcome {
                    Ok(code) => data.code = code,
                    Err(p) => data.panic = Some(p),
                }
            }
            data.code
        }

        // SAFETY: data is stack-local and scm_call runs the callbacks
        // synchronously before returning.
        unsafe {
            sys::scm_call(
                self.ptr,
                mod_c.as_ptr(),
                name_c.as_ptr(),
                Some(prepare_cb::<P, Fin>),
                &mut data as *mut _ as *mut libc::c_void,
                Some(finish_cb::<P, Fin>),
                &mut data as *mut _ as *mut libc::c_void,
            );
        }
        if let Some(p) = data.panic {
            resume_unwind(p);
        }
        data.code
    }

    pub fn load_file(&self, path: &str) -> Result<(), i32> {
        let c = CString::new(path).map_err(|_| -1)?;
        // SAFETY: c is a valid NUL-terminated string for the duration of the call.
        let rc = unsafe { sys::sni_load_file(self.ptr, c.as_ptr()) };
        if rc == 0 { Ok(()) } else { Err(rc) }
    }
}

impl Default for Scm {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Scm {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            // SAFETY: ptr came from scm_new and is freed once.
            unsafe { sys::scm_free(self.ptr) };
            self.ptr = ptr::null_mut();
        }
    }
}

/// Rooted handle to a Scheme value, branded by the [`Env`] lifetime that
/// produced it.
///
/// `Copy` and covariant over `'env`; not `Send`/`Sync` (raw handle).
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Ref<'env>(ScmRef, PhantomData<&'env ()>);

impl<'env> Ref<'env> {
    /// The null handle (no value).
    pub const fn null() -> Self {
        Ref(ptr::null_mut(), PhantomData)
    }

    pub fn as_raw(self) -> ScmRef {
        self.0
    }

    /// Brand a raw handle.
    ///
    /// # Safety
    /// `raw` must be a rooted handle (local or global ref) that stays valid
    /// for the whole `'env` scope.
    pub unsafe fn from_raw(raw: ScmRef) -> Self {
        Ref(raw, PhantomData)
    }

    pub fn is_null(self) -> bool {
        self.0.is_null()
    }
}

impl std::fmt::Debug for Ref<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ref({:?})", self.0)
    }
}

/// Owned global ref created by [`Env::new_global_ref`].
///
/// There is no `Drop` impl: releasing a global ref requires an env, so a
/// `Global` dropped (or forgotten) without [`Global::delete`] leaks the root.
pub struct Global(ScmRef);

impl Global {
    pub fn as_raw(&self) -> ScmRef {
        self.0
    }

    /// Borrow as an env-branded ref. The global root keeps the object alive;
    /// the brand ties its use to `env`'s scope.
    pub fn get<'env>(&self, _env: &Env<'env>) -> Ref<'env> {
        Ref(self.0, PhantomData)
    }

    /// Release the global ref.
    pub fn delete(self, env: &Env<'_>) {
        // SAFETY: env is live by branding; self.0 is a global ref created by
        // sni_new_global_ref and released exactly once (self is consumed).
        unsafe { sys::sni_delete_global_ref(env.as_ptr(), self.0) };
    }

    pub fn into_raw(self) -> ScmRef {
        self.0
    }
}

/// Borrowed SNI environment for the duration of an enter / call callback.
///
/// The `'env` brand is invariant and ties the env (and every [`Ref<'env>`]
/// obtained from it) to the callback scope that produced it, so neither can
/// escape `Scm::enter` or a native trampoline.
///
/// # Safety invariants
///
/// Every `unsafe` sys call in `impl Env` relies on two standing invariants,
/// established by `Scm::enter`, the trampolines in this crate, or the caller
/// of [`Env::from_raw`]:
///
/// - `ptr` is a live `SniEnv*` for the whole branded lifetime `'env`;
/// - every `Ref<'env>` is a rooted handle (local or global) valid in that scope.
///
/// Pending-exception state lives on the C side and is accessed through the
/// raw env handle, so exception queries take `&self`. Individual `SAFETY`
/// comments appear only where a call needs more than the above.
pub struct Env<'env> {
    ptr: *mut sys::SniEnv,
    _marker: PhantomData<&'env mut &'env ()>,
}

impl<'env> Env<'env> {
    /// Wrap a raw `SniEnv*` from `SNI_OnLoad` / a native trampoline.
    ///
    /// # Safety
    /// `ptr` must be a live environment for the current mutator enter, and
    /// must remain live for the whole chosen `'env` lifetime.
    pub unsafe fn from_raw(ptr: *mut sys::SniEnv) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *mut sys::SniEnv {
        self.ptr
    }

    /// Err payload for a failed C-side operation: the pending exception
    /// (retrieved and cleared), or a fresh `&assertion` when none was set.
    fn failure(&mut self) -> Ref<'env> {
        if let Some(e) = self.exception_clear() {
            e
        } else {
            self.make_assertion_violation()
        }
    }

    /// Wrap a sys-call result: null with a pending exception is an error
    /// (the exception is retrieved and cleared); anything else is Ok.
    fn check_pending(&self, raw: ScmRef) -> Result<Ref<'env>, Ref<'env>> {
        if raw.is_null()
            && let Some(e) = self.exception_clear()
        {
            return Err(e);
        }
        // SAFETY: non-failure results of sys accessors are rooted handles.
        Ok(unsafe { Ref::from_raw(raw) })
    }

    pub fn push_local_frame(&mut self, capacity: i32) -> Result<(), i32> {
        let rc = unsafe { sys::sni_push_local_frame(self.ptr, capacity) };
        if rc == 0 { Ok(()) } else { Err(rc) }
    }

    pub fn ensure_local_capacity(&mut self, capacity: i32) -> Result<(), i32> {
        let rc = unsafe { sys::sni_ensure_local_capacity(self.ptr, capacity) };
        if rc == 0 { Ok(()) } else { Err(rc) }
    }

    pub fn pop_local_frame(&mut self, result: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_pop_local_frame(self.ptr, result.as_raw())) }
    }

    pub fn new_local_ref(&mut self, obj: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_new_local_ref(self.ptr, obj.as_raw())) }
    }

    pub fn delete_local_ref(&mut self, obj: Ref<'_>) {
        unsafe { sys::sni_delete_local_ref(self.ptr, obj.as_raw()) }
    }

    pub fn new_global_ref(&mut self, obj: Ref<'_>) -> Global {
        Global(unsafe { sys::sni_new_global_ref(self.ptr, obj.as_raw()) })
    }

    pub fn public_ref(&mut self, module: &str, name: &str) -> Option<Ref<'env>> {
        let m = CString::new(module).ok()?;
        let n = CString::new(name).ok()?;
        let r = unsafe { sys::sni_public_ref(self.ptr, m.as_ptr(), n.as_ptr(), ptr::null_mut()) };
        if r.is_null() {
            None
        } else {
            Some(unsafe { Ref::from_raw(r) })
        }
    }

    pub fn private_ref(&mut self, module: &str, name: &str) -> Option<Ref<'env>> {
        let m = CString::new(module).ok()?;
        let n = CString::new(name).ok()?;
        let r = unsafe { sys::sni_private_ref(self.ptr, m.as_ptr(), n.as_ptr(), ptr::null_mut()) };
        if r.is_null() {
            None
        } else {
            Some(unsafe { Ref::from_raw(r) })
        }
    }

    pub fn call_n(&mut self, proc: Ref<'_>, args: &[Ref<'_>]) -> Result<Ref<'env>, Ref<'env>> {
        // SAFETY: args is a &[Ref]; Ref is repr(transparent) over ScmRef.
        let r = unsafe {
            sys::sni_call_n(
                self.ptr,
                proc.as_raw(),
                args.len() as i32,
                args.as_ptr() as *const ScmRef,
            )
        };
        self.check_pending(r)
    }

    /// Leave the mutator to run a blocking native callback (no GC / no Scheme).
    ///
    /// Values needed across the call must already be in [`Global`] (or cont
    /// roots); reload with [`Global::get`] after return.
    pub fn call_in_native<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        struct Data<F, R> {
            f: Option<F>,
            result: Option<R>,
            panic: Option<Box<dyn Any + Send + 'static>>,
        }
        let mut data = Data {
            f: Some(f),
            result: None,
            panic: None,
        };
        unsafe extern "C" fn trampoline<F, R>(arg: *mut libc::c_void)
        where
            F: FnOnce() -> R,
        {
            let data = unsafe { &mut *(arg as *mut Data<F, R>) };
            if let Some(f) = data.f.take() {
                match catch_unwind(AssertUnwindSafe(f)) {
                    Ok(r) => data.result = Some(r),
                    Err(p) => data.panic = Some(p),
                }
            }
        }
        unsafe {
            sys::sni_call_in_native(
                self.ptr,
                Some(trampoline::<F, R>),
                &mut data as *mut _ as *mut libc::c_void,
            );
        }
        if let Some(p) = data.panic {
            resume_unwind(p);
        }
        data.result.expect("call_in_native produced no result")
    }

    pub fn exception_check(&self) -> bool {
        unsafe { sys::sni_exception_check(self.ptr) }
    }

    /// The pending exception without clearing it (`sni_exception_occurred`).
    pub fn exception_occurred(&self) -> Option<Ref<'env>> {
        let r = unsafe { sys::sni_exception_occurred(self.ptr) };
        if r.is_null() {
            None
        } else {
            Some(unsafe { Ref::from_raw(r) })
        }
    }

    /// Clear and return the pending exception (`sni_exception_clear`).
    pub fn exception_clear(&self) -> Option<Ref<'env>> {
        let r = unsafe { sys::sni_exception_clear(self.ptr) };
        if r.is_null() {
            None
        } else {
            Some(unsafe { Ref::from_raw(r) })
        }
    }

    pub fn null(&mut self) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_null(self.ptr)) }
    }

    pub fn unspecified(&mut self) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_unspecified(self.ptr)) }
    }

    pub fn bool(&mut self, value: bool) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_bool(self.ptr, value)) }
    }

    pub fn fixnum(&mut self, value: i32) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_fixnum(self.ptr, value)) }
    }

    pub fn flonum(&mut self, value: f64) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_flonum(self.ptr, value)) }
    }

    pub fn uint32(&mut self, value: u32) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_uint32(self.ptr, value)) }
    }

    pub fn uint64(&mut self, value: u64) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_uint64(self.ptr, value)) }
    }

    pub fn int64(&mut self, value: i64) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_int64(self.ptr, value)) }
    }

    /// New Scheme string. Interior NUL is an assertion violation, surfaced
    /// as `Err` with the exception condition.
    pub fn string(&mut self, s: &str) -> Result<Ref<'env>, Ref<'env>> {
        let c = match CString::new(s) {
            Ok(c) => c,
            Err(_) => {
                self.assertion_violation("capy-sni", "string contains an interior NUL byte", &[]);
                return Err(self.failure());
            }
        };
        Ok(unsafe { Ref::from_raw(sys::sni_string(self.ptr, c.as_ptr())) })
    }

    /// Intern a symbol. Interior NUL is an assertion violation, surfaced as
    /// `Err` with the exception condition.
    pub fn intern_symbol(&mut self, name: &str) -> Result<Ref<'env>, Ref<'env>> {
        let c = match CString::new(name) {
            Ok(c) => c,
            Err(_) => {
                self.assertion_violation(
                    "capy-sni",
                    "symbol name contains an interior NUL byte",
                    &[],
                );
                return Err(self.failure());
            }
        };
        Ok(unsafe { Ref::from_raw(sys::sni_intern_symbol(self.ptr, c.as_ptr())) })
    }

    pub fn cons(&mut self, car: Ref<'_>, cdr: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_cons(self.ptr, car.as_raw(), cdr.as_raw())) }
    }

    pub fn car(&self, pair: Ref<'_>) -> Result<Ref<'env>, Ref<'env>> {
        let r = unsafe { sys::sni_car(self.ptr, pair.as_raw()) };
        self.check_pending(r)
    }

    pub fn cdr(&self, pair: Ref<'_>) -> Result<Ref<'env>, Ref<'env>> {
        let r = unsafe { sys::sni_cdr(self.ptr, pair.as_raw()) };
        self.check_pending(r)
    }

    pub fn is_string(&self, obj: Ref<'_>) -> bool {
        unsafe { sys::sni_is_string(self.ptr, obj.as_raw()) }
    }

    pub fn is_bytevector(&self, obj: Ref<'_>) -> bool {
        unsafe { sys::sni_is_bytevector(self.ptr, obj.as_raw()) }
    }

    pub fn is_procedure(&self, obj: Ref<'_>) -> bool {
        unsafe { sys::sni_is_procedure(self.ptr, obj.as_raw()) }
    }

    pub fn string_to_utf8(&mut self, obj: Ref<'_>) -> Result<String, Ref<'env>> {
        let mut written = 0usize;
        // First call only queries the length (null buf, capacity 0).
        let _ = unsafe {
            sys::sni_string_to_utf8(self.ptr, obj.as_raw(), ptr::null_mut(), 0, &mut written)
        };
        let mut buf = vec![0u8; written + 1];
        // SAFETY: buf has written + 1 bytes; the call updates written.
        let ok = unsafe {
            sys::sni_string_to_utf8(
                self.ptr,
                obj.as_raw(),
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut written,
            )
        };
        if !ok {
            return Err(self.failure());
        }
        buf.truncate(written);
        match String::from_utf8(buf) {
            Ok(s) => Ok(s),
            Err(_) => Err(self.failure()),
        }
    }

    pub fn value_to_utf8(&mut self, obj: Ref<'_>) -> Result<String, Ref<'env>> {
        let mut written = 0usize;
        let _ = unsafe {
            sys::sni_value_to_utf8(self.ptr, obj.as_raw(), ptr::null_mut(), 0, &mut written)
        };
        let mut buf = vec![0u8; written + 1];
        // SAFETY: buf has written + 1 bytes; the call updates written.
        let ok = unsafe {
            sys::sni_value_to_utf8(
                self.ptr,
                obj.as_raw(),
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut written,
            )
        };
        if !ok {
            return Err(self.failure());
        }
        buf.truncate(written);
        match String::from_utf8(buf) {
            Ok(s) => Ok(s),
            Err(_) => Err(self.failure()),
        }
    }

    /// Install program arguments. An argument with an interior NUL byte is an
    /// assertion violation, surfaced as `Err` with the exception condition.
    pub fn init_program_arguments(&mut self, args: &[OsString]) -> Result<(), Ref<'env>> {
        let mut c_strings = Vec::with_capacity(args.len());
        for a in args {
            match CString::new(a.as_bytes()) {
                Ok(c) => c_strings.push(c),
                Err(_) => {
                    self.assertion_violation(
                        "capy-sni",
                        "program argument contains an interior NUL byte",
                        &[],
                    );
                    return Err(self.failure());
                }
            }
        }
        let ptrs: Vec<*const libc::c_char> = c_strings.iter().map(|c| c.as_ptr()).collect();
        // SAFETY: ptrs points to c_strings' NUL-terminated strings, alive for the call.
        unsafe {
            sys::sni_program_arguments_init(self.ptr, ptrs.len() as i32, ptrs.as_ptr());
        }
        Ok(())
    }

    pub fn program_arguments(&mut self) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_program_arguments(self.ptr)) }
    }

    /// Define `name` in `module`. Interior NUL in either is an assertion
    /// violation, surfaced as `Err` with the exception condition.
    pub fn define(&mut self, module: &str, name: &str, value: Ref<'_>) -> Result<(), Ref<'env>> {
        let (m, n) = match (CString::new(module), CString::new(name)) {
            (Ok(m), Ok(n)) => (m, n),
            _ => {
                self.assertion_violation(
                    "capy-sni",
                    "module or name contains an interior NUL byte",
                    &[],
                );
                return Err(self.failure());
            }
        };
        let rc = unsafe { sys::sni_define(self.ptr, m.as_ptr(), n.as_ptr(), value.as_raw()) };
        if rc == 0 && !self.exception_check() {
            Ok(())
        } else {
            Err(self.failure())
        }
    }

    pub fn new_native_procedure(
        &mut self,
        f: unsafe extern "C" fn(*mut sys::SniEnv, i32, *const ScmRef) -> ScmRef,
    ) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_new_native_procedure(self.ptr, Some(f))) }
    }

    pub fn make_rtd(
        &mut self,
        name: Ref<'_>,
        parent: Ref<'_>,
        uid: Ref<'_>,
        sealed: bool,
        opaque: bool,
        fields: Ref<'_>,
    ) -> Ref<'env> {
        unsafe {
            Ref::from_raw(sys::sni_make_rtd(
                self.ptr,
                name.as_raw(),
                parent.as_raw(),
                uid.as_raw(),
                sealed,
                opaque,
                fields.as_raw(),
            ))
        }
    }

    pub fn make_rcd(&mut self, rtd: Ref<'_>, parent_rcd: Ref<'_>, protocol: Ref<'_>) -> Ref<'env> {
        unsafe {
            Ref::from_raw(sys::sni_make_rcd(
                self.ptr,
                rtd.as_raw(),
                parent_rcd.as_raw(),
                protocol.as_raw(),
            ))
        }
    }

    pub fn record_constructor(&mut self, rcd: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_record_constructor(self.ptr, rcd.as_raw())) }
    }

    pub fn make_class(&mut self, name: Ref<'_>, slots: Ref<'_>, supers: Ref<'_>) -> Ref<'env> {
        unsafe {
            Ref::from_raw(sys::sni_make_class(
                self.ptr,
                name.as_raw(),
                slots.as_raw(),
                supers.as_raw(),
            ))
        }
    }

    pub fn make_abstract_class(&mut self, name: Ref<'_>, supers: Ref<'_>) -> Ref<'env> {
        unsafe {
            Ref::from_raw(sys::sni_make_abstract_class(
                self.ptr,
                name.as_raw(),
                supers.as_raw(),
            ))
        }
    }

    pub fn make_instance(&mut self, class: Ref<'_>, initargs: &[Ref<'_>]) -> Ref<'env> {
        // SAFETY: initargs is a &[Ref]; Ref is repr(transparent) over ScmRef.
        unsafe {
            Ref::from_raw(sys::sni_make_instance(
                self.ptr,
                class.as_raw(),
                initargs.len() as i32,
                initargs.as_ptr() as *const ScmRef,
            ))
        }
    }

    pub fn slot_ref(
        &self,
        obj: Ref<'_>,
        slot: Ref<'_>,
        unbound: Ref<'_>,
    ) -> Result<Ref<'env>, Ref<'env>> {
        let r =
            unsafe { sys::sni_slot_ref(self.ptr, obj.as_raw(), slot.as_raw(), unbound.as_raw()) };
        self.check_pending(r)
    }

    pub fn slot_set(
        &mut self,
        obj: Ref<'_>,
        slot: Ref<'_>,
        value: Ref<'_>,
    ) -> Result<(), Ref<'env>> {
        let rc =
            unsafe { sys::sni_slot_set(self.ptr, obj.as_raw(), slot.as_raw(), value.as_raw()) };
        if rc == 0 && !self.exception_check() {
            Ok(())
        } else {
            Err(self.failure())
        }
    }

    pub fn class_of(&mut self, obj: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_class_of(self.ptr, obj.as_raw())) }
    }

    /// Built-in class by name. Interior NUL is an assertion violation,
    /// surfaced as `Err` with the exception condition.
    pub fn builtin_class(&mut self, name: &str) -> Result<Ref<'env>, Ref<'env>> {
        let c = match CString::new(name) {
            Ok(c) => c,
            Err(_) => {
                self.assertion_violation(
                    "capy-sni",
                    "class name contains an interior NUL byte",
                    &[],
                );
                return Err(self.failure());
            }
        };
        Ok(unsafe { Ref::from_raw(sys::sni_builtin_class(self.ptr, c.as_ptr())) })
    }

    pub fn make_generic(&mut self, name: Ref<'_>, max_dispatch_args: i32) -> Ref<'env> {
        unsafe {
            Ref::from_raw(sys::sni_make_generic(
                self.ptr,
                name.as_raw(),
                max_dispatch_args,
            ))
        }
    }

    pub fn add_method(
        &mut self,
        generic: Ref<'_>,
        specializers: Ref<'_>,
        required_argc: i32,
        body: Ref<'_>,
        locked: bool,
    ) -> Result<(), Ref<'env>> {
        let rc = unsafe {
            sys::sni_add_method(
                self.ptr,
                generic.as_raw(),
                specializers.as_raw(),
                required_argc,
                body.as_raw(),
                locked,
            )
        };
        if rc == 0 && !self.exception_check() {
            Ok(())
        } else {
            Err(self.failure())
        }
    }

    pub fn make_vector(&mut self, len: usize, fill: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_make_vector(self.ptr, len, fill.as_raw())) }
    }

    pub fn vector_ref(&self, vector: Ref<'_>, index: usize) -> Result<Ref<'env>, Ref<'env>> {
        let r = unsafe { sys::sni_vector_ref(self.ptr, vector.as_raw(), index) };
        self.check_pending(r)
    }

    pub fn vector_set(
        &mut self,
        vector: Ref<'_>,
        index: usize,
        value: Ref<'_>,
    ) -> Result<(), Ref<'env>> {
        unsafe { sys::sni_vector_set(self.ptr, vector.as_raw(), index, value.as_raw()) };
        if self.exception_check() {
            Err(self.failure())
        } else {
            Ok(())
        }
    }

    pub fn make_pod_bytevector(&mut self, len: usize) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_make_pod_bytevector(self.ptr, len)) }
    }

    /// Interior pointer of a bytevector. Valid only until the next
    /// allocation / nest / GC-triggering call on this env.
    pub fn bytevector_data(&self, bv: Ref<'_>) -> Result<*mut u8, Ref<'env>> {
        let p = unsafe { sys::sni_bytevector_data(self.ptr, bv.as_raw()) };
        if p.is_null()
            && let Some(e) = self.exception_clear()
        {
            return Err(e);
        }
        Ok(p)
    }

    pub fn bytevector_len(&self, bv: Ref<'_>) -> Result<usize, Ref<'env>> {
        let n = unsafe { sys::sni_bytevector_len(self.ptr, bv.as_raw()) };
        if n == 0
            && let Some(e) = self.exception_clear()
        {
            return Err(e);
        }
        Ok(n)
    }

    /// Register a POD type. Interior NUL in `name` or `uid` is an assertion
    /// violation, surfaced as `Err` with the exception condition.
    pub fn register_pod_type(
        &mut self,
        name: &str,
        size: usize,
        align: usize,
        uid: Option<&str>,
    ) -> Result<Ref<'env>, Ref<'env>> {
        let n = match CString::new(name) {
            Ok(c) => c,
            Err(_) => {
                self.assertion_violation(
                    "capy-sni",
                    "pod type name contains an interior NUL byte",
                    &[],
                );
                return Err(self.failure());
            }
        };
        let uid_c = match uid.map(CString::new) {
            Some(Ok(c)) => Some(c),
            None => None,
            Some(Err(_)) => {
                self.assertion_violation("capy-sni", "uid contains an interior NUL byte", &[]);
                return Err(self.failure());
            }
        };
        Ok(unsafe {
            Ref::from_raw(sys::sni_register_pod_type(
                self.ptr,
                n.as_ptr(),
                size,
                align,
                uid_c.as_ref().map(|c| c.as_ptr()).unwrap_or(ptr::null()),
            ))
        })
    }

    pub fn pod_type_check(&mut self, pod_type: Ref<'_>, bv: Ref<'_>) -> bool {
        unsafe { sys::sni_pod_type_check(self.ptr, pod_type.as_raw(), bv.as_raw()) }
    }

    pub fn raise(&mut self, obj: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_raise(self.ptr, obj.as_raw())) }
    }

    pub fn raise_continuable(&mut self, obj: Ref<'_>) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_raise_continuable(self.ptr, obj.as_raw())) }
    }

    /// Install `obj` as the pending exception without unwinding (`sni_throw`).
    pub fn throw(&mut self, obj: Ref<'_>) -> i32 {
        unsafe { sys::sni_throw(self.ptr, obj.as_raw()) }
    }

    pub fn assertion_violation(&mut self, who: &str, message: &str, irritants: &[Ref<'_>]) {
        let w = CString::new(who).unwrap_or_default();
        let m = CString::new(message).unwrap_or_default();
        // SAFETY: irritants is a &[Ref]; Ref is repr(transparent) over ScmRef.
        unsafe {
            sys::sni_assertion_violation(
                self.ptr,
                w.as_ptr(),
                m.as_ptr(),
                irritants.len() as i32,
                irritants.as_ptr() as *const ScmRef,
            );
        }
    }

    pub fn make_message_condition(&mut self, msg: &str) -> Ref<'env> {
        let c = CString::new(msg).unwrap_or_default();
        unsafe { Ref::from_raw(sys::sni_make_message_condition(self.ptr, c.as_ptr())) }
    }

    pub fn make_assertion_violation(&mut self) -> Ref<'env> {
        unsafe { Ref::from_raw(sys::sni_make_assertion_violation(self.ptr)) }
    }

    pub fn condition(&mut self, components: &[Ref<'_>]) -> Ref<'env> {
        // SAFETY: components is a &[Ref]; Ref is repr(transparent) over ScmRef.
        unsafe {
            Ref::from_raw(sys::sni_condition(
                self.ptr,
                components.len() as i32,
                components.as_ptr() as *const ScmRef,
            ))
        }
    }

    /// Run `body` under `handler`. A panic in either is caught at the FFI
    /// boundary and resumed once `sni_with_exception_handler` returns to Rust.
    pub fn with_exception_handler<H, B, R>(&mut self, handler: H, body: B) -> Ref<'env>
    where
        H: FnMut(&mut Env<'env>, Ref<'env>) -> Ref<'env>,
        B: FnMut(&mut Env<'env>) -> R,
        R: IntoScm<'env>,
    {
        struct Data<H, B> {
            handler: H,
            body: B,
            panic: Option<Box<dyn Any + Send + 'static>>,
        }
        let mut data = Data {
            handler,
            body,
            panic: None,
        };
        unsafe extern "C" fn handler_cb<'env, H, B, R>(
            env: *mut sys::SniEnv,
            exn: ScmRef,
            data: *mut libc::c_void,
        ) -> ScmRef
        where
            H: FnMut(&mut Env<'env>, Ref<'env>) -> Ref<'env>,
            B: FnMut(&mut Env<'env>) -> R,
            R: IntoScm<'env>,
        {
            // SAFETY: data is the `&mut Data` passed below, alive for the
            // whole sni_with_exception_handler call; env and exn are live for
            // this callback.
            let data = unsafe { &mut *(data as *mut Data<H, B>) };
            let outcome = catch_unwind(AssertUnwindSafe(|| {
                let mut env = unsafe { Env::<'env>::from_raw(env) };
                let exn = unsafe { Ref::from_raw(exn) };
                (data.handler)(&mut env, exn).as_raw()
            }));
            match outcome {
                Ok(r) => r,
                Err(p) => {
                    data.panic = Some(p);
                    ptr::null_mut()
                }
            }
        }
        unsafe extern "C" fn body_cb<'env, H, B, R>(
            env: *mut sys::SniEnv,
            data: *mut libc::c_void,
        ) -> ScmRef
        where
            H: FnMut(&mut Env<'env>, Ref<'env>) -> Ref<'env>,
            B: FnMut(&mut Env<'env>) -> R,
            R: IntoScm<'env>,
        {
            // SAFETY: same as handler_cb.
            let data = unsafe { &mut *(data as *mut Data<H, B>) };
            let outcome = catch_unwind(AssertUnwindSafe(|| {
                let mut env = unsafe { Env::<'env>::from_raw(env) };
                let r = (data.body)(&mut env);
                r.into_scm(&mut env).as_raw()
            }));
            match outcome {
                Ok(r) => r,
                Err(p) => {
                    data.panic = Some(p);
                    ptr::null_mut()
                }
            }
        }
        let raw = unsafe {
            sys::sni_with_exception_handler(
                self.ptr,
                Some(handler_cb::<'env, H, B, R>),
                &mut data as *mut _ as *mut libc::c_void,
                Some(body_cb::<'env, H, B, R>),
                &mut data as *mut _ as *mut libc::c_void,
            )
        };
        if let Some(p) = data.panic {
            resume_unwind(p);
        }
        // SAFETY: result of sni_with_exception_handler is a rooted handle.
        unsafe { Ref::from_raw(raw) }
    }

    /// List helper: build a proper list from refs.
    pub fn list(&mut self, items: &[Ref<'_>]) -> Ref<'env> {
        let mut out = self.null();
        for &item in items.iter().rev() {
            out = self.cons(item, out);
        }
        out
    }

    /// Vector of symbols for record field specs: `#((mutable name) ...)`.
    pub fn mutable_field_vector(&mut self, names: &[&str]) -> Result<Ref<'env>, Ref<'env>> {
        let fill = self.null();
        let vec = self.make_vector(names.len(), fill);
        for (i, name) in names.iter().enumerate() {
            let mut_sym = self.intern_symbol("mutable")?;
            let field = self.intern_symbol(name)?;
            let nil = self.null();
            let inner = self.cons(field, nil);
            let spec = self.cons(mut_sym, inner);
            self.vector_set(vec, i, spec)?;
        }
        Ok(vec)
    }
}

/// Reconstruct an [`Env`] from a raw SNI pointer and run `f`.
///
/// Used by `capy-sni-derive` trampolines. There is no Rust return path past
/// this point, so a panic in `f` aborts the process (unwinding across the
/// FFI boundary would be UB).
///
/// # Safety
/// `env_ptr` must be a live `SniEnv*` for the duration of this call, and
/// `argv` must point to `argc` rooted refs (the SNI native-procedure
/// calling convention).
#[doc(hidden)]
pub unsafe fn __scheme_trampoline<F>(
    env_ptr: *mut sys::SniEnv,
    argc: i32,
    argv: *const ScmRef,
    f: F,
) -> ScmRef
where
    F: for<'env> FnOnce(&mut Env<'env>, &[Ref<'env>]) -> Ref<'env>,
{
    let outcome = catch_unwind(AssertUnwindSafe(|| {
        // SAFETY: env_ptr is a live env for the duration of this native call
        // (SNI native-procedure calling convention).
        let mut env = unsafe { Env::from_raw(env_ptr) };
        let args: &[Ref<'_>] = if argc <= 0 || argv.is_null() {
            &[]
        } else {
            // SAFETY: argv points to argc rooted refs; Ref is
            // repr(transparent) over ScmRef.
            unsafe { std::slice::from_raw_parts(argv as *const Ref<'_>, argc as usize) }
        };
        f(&mut env, args)
    }));
    match outcome {
        Ok(r) => r.as_raw(),
        Err(p) => {
            eprintln!(
                "capy-sni: panic in Scheme native procedure trampoline: {}; \
                 aborting (cannot unwind across the FFI boundary)",
                panic_payload_message(&*p)
            );
            std::process::abort()
        }
    }
}

/// RAII local frame (PushLocalFrame / PopLocalFrame).
pub struct LocalFrame<'a, 'env> {
    env: &'a mut Env<'env>,
    active: bool,
}

impl<'a, 'env> LocalFrame<'a, 'env> {
    pub fn push(env: &'a mut Env<'env>, capacity: i32) -> Result<Self, i32> {
        env.push_local_frame(capacity)?;
        Ok(Self { env, active: true })
    }

    pub fn pop(mut self, result: Ref<'env>) -> Ref<'env> {
        self.active = false;
        self.env.pop_local_frame(result)
    }
}

impl Drop for LocalFrame<'_, '_> {
    fn drop(&mut self) {
        if self.active {
            let _ = self.env.pop_local_frame(Ref::null());
        }
    }
}

pub(crate) fn panic_payload_message(payload: &(dyn Any + Send + 'static)) -> String {
    if let Some(s) = payload.downcast_ref::<&'static str>() {
        (*s).to_owned()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic payload".to_owned()
    }
}
