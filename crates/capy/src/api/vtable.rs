//! Function table (`SNINativeInterface`) hung off [`SniEnv`].

use super::cps::{SniContinuation, sni_call_k, sni_guard_k, sni_raise_k, sni_trampoline};
use super::env::{ScmRef, SniEnv};
use super::native::SniNativeFn;
use super::raise::{SniExceptionHandler, SniThunk};
use super::{call, exceptions, load, native, oop, raise, refs, values};

/// Flat vtable. Prefer the flat `sni_*` symbols from Rust/bindgen;
/// C code may use `(*env)->Call(env, …)` style via this table.
#[repr(C)]
#[allow(non_snake_case)]
pub struct SNINativeInterface {
    pub PushLocalFrame: unsafe extern "C" fn(*mut SniEnv<'static>, i32) -> libc::c_int,
    pub PopLocalFrame: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub EnsureLocalCapacity: unsafe extern "C" fn(*mut SniEnv<'static>, i32) -> libc::c_int,
    pub NewLocalRef: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub DeleteLocalRef: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef),
    pub NewGlobalRef: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub DeleteGlobalRef: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef),
    pub IsSameObject: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef) -> bool,
    pub CallN: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, i32, *const ScmRef) -> ScmRef,
    pub ExceptionCheck: unsafe extern "C" fn(*mut SniEnv<'static>) -> bool,
    pub ExceptionOccurred: unsafe extern "C" fn(*mut SniEnv<'static>) -> ScmRef,
    pub ExceptionClear: unsafe extern "C" fn(*mut SniEnv<'static>) -> ScmRef,
    pub ExceptionDescribe: unsafe extern "C" fn(*mut SniEnv<'static>),
    pub Throw: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> libc::c_int,
    pub PublicRef: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        *const libc::c_char,
        *const libc::c_char,
        ScmRef,
    ) -> ScmRef,
    pub Cons: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef) -> ScmRef,
    pub Car: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub Cdr: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub NewString: unsafe extern "C" fn(*mut SniEnv<'static>, *const libc::c_char) -> ScmRef,
    pub Null: unsafe extern "C" fn(*mut SniEnv<'static>) -> ScmRef,
    pub NewNativeProcedure:
        unsafe extern "C" fn(*mut SniEnv<'static>, Option<SniNativeFn>) -> ScmRef,
    pub Define: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        *const libc::c_char,
        *const libc::c_char,
        ScmRef,
    ) -> libc::c_int,
    pub MakeClass: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef, ScmRef) -> ScmRef,
    pub MakeAbstractClass: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef) -> ScmRef,
    pub MakeInstance:
        unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, i32, *const ScmRef) -> ScmRef,
    pub SlotRef: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef, ScmRef) -> ScmRef,
    pub SlotSet: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, ScmRef, ScmRef) -> libc::c_int,
    pub MakeGeneric: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, i32) -> ScmRef,
    pub AddMethod: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        ScmRef,
        ScmRef,
        i32,
        ScmRef,
        bool,
    ) -> libc::c_int,
    pub Raise: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub RaiseContinuable: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef) -> ScmRef,
    pub WithExceptionHandler: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        Option<SniExceptionHandler>,
        *mut libc::c_void,
        Option<SniThunk>,
        *mut libc::c_void,
    ) -> ScmRef,
    pub AssertionViolation: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        *const libc::c_char,
        *const libc::c_char,
        i32,
        *const ScmRef,
    ),
    pub Trampoline: unsafe extern "C" fn(*mut SniEnv<'static>, *mut SniContinuation) -> ScmRef,
    pub CallK: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        ScmRef,
        i32,
        *const ScmRef,
        *mut SniContinuation,
    ),
    pub RaiseK: unsafe extern "C" fn(*mut SniEnv<'static>, ScmRef, *mut SniContinuation),
    pub GuardK: unsafe extern "C" fn(
        *mut SniEnv<'static>,
        *mut SniContinuation,
        *mut SniContinuation,
    ) -> *mut SniContinuation,
}

pub static SNI_NATIVE_INTERFACE: &SNINativeInterface = &SNINativeInterface {
    PushLocalFrame: refs::sni_push_local_frame,
    PopLocalFrame: refs::sni_pop_local_frame,
    EnsureLocalCapacity: refs::sni_ensure_local_capacity,
    NewLocalRef: refs::sni_new_local_ref,
    DeleteLocalRef: refs::sni_delete_local_ref,
    NewGlobalRef: refs::sni_new_global_ref,
    DeleteGlobalRef: refs::sni_delete_global_ref,
    IsSameObject: refs::sni_is_same,
    CallN: call::sni_call_n,
    ExceptionCheck: exceptions::sni_exception_check,
    ExceptionOccurred: exceptions::sni_exception_occurred,
    ExceptionClear: exceptions::sni_exception_clear,
    ExceptionDescribe: exceptions::sni_exception_describe,
    Throw: exceptions::sni_throw,
    PublicRef: call::sni_public_ref,
    Cons: values::sni_cons,
    Car: values::sni_car,
    Cdr: values::sni_cdr,
    NewString: values::sni_string,
    Null: values::sni_null,
    NewNativeProcedure: native::sni_new_native_procedure,
    Define: native::sni_define,
    MakeClass: oop::sni_make_class,
    MakeAbstractClass: oop::sni_make_abstract_class,
    MakeInstance: oop::sni_make_instance,
    SlotRef: oop::sni_slot_ref,
    SlotSet: oop::sni_slot_set,
    MakeGeneric: oop::sni_make_generic,
    AddMethod: oop::sni_add_method,
    Raise: raise::sni_raise,
    RaiseContinuable: raise::sni_raise_continuable,
    WithExceptionHandler: raise::sni_with_exception_handler,
    AssertionViolation: raise::sni_assertion_violation,
    Trampoline: sni_trampoline,
    CallK: sni_call_k,
    RaiseK: sni_raise_k,
    GuardK: sni_guard_k,
};

/// Return the function table pointer for `env` (may be null if env is null).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_get_functions(env: *mut SniEnv<'static>) -> *const SNINativeInterface {
    if env.is_null() {
        return std::ptr::null();
    }
    // SAFETY: env is live.
    unsafe { (*env).functions }
}

// Keep load/pod/cps symbols linked into the cdylib.
#[allow(dead_code)]
fn _link_extras() {
    let _ = load::sni_load_file as *const ();
    let _ = super::pod::sni_make_pod_bytevector as *const ();
    let _ = super::cps::sni_cont_alloc as *const ();
    let _ = super::cps::sni_cont_set_result as *const ();
}
