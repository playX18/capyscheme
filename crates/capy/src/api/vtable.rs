//! Function table (`SniNativeInterface`) hung off [`SniEnv`].

use super::cps::{SniContinuation, sni_call_k, sni_guard_k, sni_raise_k, sni_trampoline};
use super::env::{ScmRef, SniEnv};
use super::native::SniNativeFn;
use super::raise::{SniExceptionHandler, SniThunk};
use super::{call, exceptions, load, native, oop, raise, refs, values};

/// Flat vtable. Prefer the flat `sni_*` symbols from Rust/bindgen.
/// C code may use `(*env)->Call(env, …)` style via this table.
///
/// The first two fields identify the layout: an extension built against another
/// `SNI_ABI_VERSION` is refused when it is loaded (`SNI_ABIVersion`, see `runtime/vm/dl.rs`).
#[repr(C)]
#[allow(non_snake_case)]
pub struct SniNativeInterface {
    pub abi_version: u32,
    pub struct_size: u32,
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

/// Bumped whenever `SniNativeInterface` or a function it holds changes incompatibly. Mirrored by
/// `SNI_ABI_VERSION` in `c/capy.h` and in `capy-sni-sys`.
pub const SNI_ABI_VERSION: u32 = 1;

pub static SNI_NATIVE_INTERFACE: &SniNativeInterface = &SniNativeInterface {
    abi_version: SNI_ABI_VERSION,
    struct_size: size_of::<SniNativeInterface>() as u32,
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
pub unsafe extern "C" fn sni_get_functions(env: *mut SniEnv<'static>) -> *const SniNativeInterface {
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

#[cfg(test)]
mod tests {
    use super::*;
    use core::mem::size_of;

    /// The function-pointer entries of `struct SNINativeInterface` in c/capy.h, in order.
    fn header_functions() -> Vec<String> {
        let header = include_str!("../../../../c/capy.h");
        let start = header
            .find("struct SNINativeInterface {")
            .expect("the table in capy.h");
        let body = &header[start..];
        let end = body.find("\n};").expect("the end of the table");
        let mut names = Vec::new();
        let mut text = String::new();
        // a declaration can span lines: collect up to the `;`
        for line in body[..end].lines().skip(1) {
            text.push_str(line.trim());
            text.push(' ');
            if line.trim_end().ends_with(';') {
                if let Some(at) = text.find("(*") {
                    let rest = &text[at + 2..];
                    names.push(rest[..rest.find(')').unwrap()].to_string());
                }
                text.clear();
            }
        }
        names
    }

    #[test]
    fn the_table_of_capy_h_has_the_functions_of_the_vtable() {
        let functions = header_functions();
        // two u32 header fields, then one pointer-sized entry per function
        let header_bytes = 2 * size_of::<u32>();
        let entries = (size_of::<SniNativeInterface>() - header_bytes) / size_of::<usize>();
        assert_eq!(
            functions.len(),
            entries,
            "capy.h declares {} functions, the runtime {entries}: {functions:?}",
            functions.len()
        );
        // the Rust field names are the C names; check the order of the first and the last
        assert_eq!(
            functions.first().map(String::as_str),
            Some("PushLocalFrame")
        );
        assert_eq!(functions.last().map(String::as_str), Some("GuardK"));
        // the header lists them in the order of the fields: spot-check the middle
        assert_eq!(functions.iter().position(|f| f == "Define"), Some(21));
    }

    #[test]
    fn the_header_carries_the_abi_version_of_the_crate() {
        let header = include_str!("../../../../c/capy.h");
        assert!(
            header.contains(&format!("#define SNI_ABI_VERSION {SNI_ABI_VERSION}u")),
            "SNI_ABI_VERSION of capy.h and of the runtime differ"
        );
    }

    #[test]
    fn the_table_starts_with_the_version_fields() {
        use core::mem::offset_of;
        assert_eq!(offset_of!(SniNativeInterface, abi_version), 0);
        assert_eq!(
            offset_of!(SniNativeInterface, struct_size),
            size_of::<u32>()
        );
    }
}
