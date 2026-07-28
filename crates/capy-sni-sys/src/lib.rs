//! Raw FFI bindings to `libcapy` SNI. Prefer the safe [`capy-sni`] crate.

#![allow(non_camel_case_types, non_snake_case, clippy::all)]

use libc::{c_char, c_int, c_void, size_t};

pub type Scm = c_void;
pub type SniEnv = c_void;
pub type ScmRef = *mut c_void;

pub type ScmEnterFn = Option<unsafe extern "C" fn(env: *mut SniEnv, arg: *mut c_void) -> c_int>;
pub type ThreadFn = Option<unsafe extern "C" fn(scm: *mut Scm, arg: *mut c_void) -> *mut c_void>;
pub type PrepareCallFn =
    Option<unsafe extern "C" fn(env: *mut SniEnv, args: *mut ScmRef, data: *mut c_void)>;
pub type FinishCallFn = Option<
    unsafe extern "C" fn(
        env: *mut SniEnv,
        success: bool,
        result: ScmRef,
        data: *mut c_void,
    ) -> c_int,
>;
pub type NativeCallback = Option<unsafe extern "C" fn(data: *mut c_void)>;
pub type SNI_OnLoadFn =
    Option<unsafe extern "C" fn(env: *mut SniEnv, reserved: *mut c_void) -> c_int>;

/// Must mirror `struct SNINativeInterface` in c/capy.h exactly (order and types).
#[repr(C)]
pub struct SNINativeInterface {
    pub PushLocalFrame: Option<unsafe extern "C" fn(*mut SniEnv, c_int) -> c_int>,
    pub PopLocalFrame: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub EnsureLocalCapacity: Option<unsafe extern "C" fn(*mut SniEnv, c_int) -> c_int>,
    pub NewLocalRef: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub DeleteLocalRef: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef)>,
    pub NewGlobalRef: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub DeleteGlobalRef: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef)>,
    pub IsSameObject: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef) -> bool>,
    pub CallN: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, c_int, *const ScmRef) -> ScmRef>,
    pub ExceptionCheck: Option<unsafe extern "C" fn(*mut SniEnv) -> bool>,
    pub ExceptionOccurred: Option<unsafe extern "C" fn(*mut SniEnv) -> ScmRef>,
    pub ExceptionClear: Option<unsafe extern "C" fn(*mut SniEnv) -> ScmRef>,
    pub ExceptionDescribe: Option<unsafe extern "C" fn(*mut SniEnv)>,
    pub Throw: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> c_int>,
    pub PublicRef:
        Option<unsafe extern "C" fn(*mut SniEnv, *const c_char, *const c_char, ScmRef) -> ScmRef>,
    pub Cons: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef) -> ScmRef>,
    pub Car: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub Cdr: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub NewString: Option<unsafe extern "C" fn(*mut SniEnv, *const c_char) -> ScmRef>,
    pub Null: Option<unsafe extern "C" fn(*mut SniEnv) -> ScmRef>,
    pub NewNativeProcedure: Option<unsafe extern "C" fn(*mut SniEnv, SniNativeFn) -> ScmRef>,
    pub Define:
        Option<unsafe extern "C" fn(*mut SniEnv, *const c_char, *const c_char, ScmRef) -> c_int>,
    pub MakeClass: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef, ScmRef) -> ScmRef>,
    pub MakeAbstractClass: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef) -> ScmRef>,
    pub MakeInstance:
        Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, c_int, *const ScmRef) -> ScmRef>,
    pub SlotRef: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef, ScmRef) -> ScmRef>,
    pub SlotSet: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef, ScmRef) -> c_int>,
    pub MakeGeneric: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, c_int) -> ScmRef>,
    pub AddMethod:
        Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, ScmRef, c_int, ScmRef, bool) -> c_int>,
    pub Raise: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub RaiseContinuable: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef) -> ScmRef>,
    pub WithExceptionHandler: Option<
        unsafe extern "C" fn(
            *mut SniEnv,
            SniExceptionHandler,
            *mut c_void,
            SniThunk,
            *mut c_void,
        ) -> ScmRef,
    >,
    pub AssertionViolation: Option<
        unsafe extern "C" fn(*mut SniEnv, *const c_char, *const c_char, c_int, *const ScmRef),
    >,
    pub Trampoline: Option<unsafe extern "C" fn(*mut SniEnv, *mut SniContinuation) -> ScmRef>,
    pub CallK: Option<
        unsafe extern "C" fn(*mut SniEnv, ScmRef, c_int, *const ScmRef, *mut SniContinuation),
    >,
    pub RaiseK: Option<unsafe extern "C" fn(*mut SniEnv, ScmRef, *mut SniContinuation)>,
    pub GuardK: Option<
        unsafe extern "C" fn(
            *mut SniEnv,
            *mut SniContinuation,
            *mut SniContinuation,
        ) -> *mut SniContinuation,
    >,
}

pub type SniNativeFn =
    Option<unsafe extern "C" fn(env: *mut SniEnv, argc: c_int, argv: *const ScmRef) -> ScmRef>;
pub type SniThunk = Option<unsafe extern "C" fn(env: *mut SniEnv, data: *mut c_void) -> ScmRef>;
pub type SniExceptionHandler =
    Option<unsafe extern "C" fn(env: *mut SniEnv, exn: ScmRef, data: *mut c_void) -> ScmRef>;

pub type SniContFn =
    Option<unsafe extern "C" fn(env: *mut SniEnv, c: *mut SniContinuation) -> *mut SniContinuation>;
pub type SniDropDataFn = Option<unsafe extern "C" fn(data: *mut c_void)>;

/// Must mirror `struct SniContinuation` in c/capy.h exactly.
#[repr(C)]
pub struct SniContinuation {
    pub next: SniContFn,
    pub result: ScmRef,
    pub data: *mut c_void,
    pub drop_data: SniDropDataFn,
}

const _: () = {
    use core::mem::{offset_of, size_of};
    assert!(size_of::<SniContinuation>() == 4 * size_of::<*mut c_void>());
    assert!(offset_of!(SniContinuation, next) == 0);
    assert!(offset_of!(SniContinuation, result) == size_of::<*mut c_void>());
    assert!(offset_of!(SniContinuation, data) == 2 * size_of::<*mut c_void>());
    assert!(offset_of!(SniContinuation, drop_data) == 3 * size_of::<*mut c_void>());
};

unsafe extern "C" {
    pub fn scm_new() -> *mut Scm;
    pub fn scm_from_image(image_data: *const u8, image_size: size_t) -> *mut Scm;
    pub fn scm_free(scm: *mut Scm);
    pub fn scm_enter(scm: *mut Scm, enter: ScmEnterFn, arg: *mut c_void) -> c_int;
    pub fn scm_attach_current_thread(scm: *mut Scm, enter: ScmEnterFn, arg: *mut c_void) -> c_int;
    pub fn scm_detach_current_thread(scm: *mut Scm);
    pub fn scm_fork(env: *mut SniEnv, init: ThreadFn, arg: *mut c_void) -> ScmRef;

    pub fn sni_push_local_frame(env: *mut SniEnv, capacity: c_int) -> c_int;
    pub fn sni_pop_local_frame(env: *mut SniEnv, result: ScmRef) -> ScmRef;
    pub fn sni_ensure_local_capacity(env: *mut SniEnv, capacity: c_int) -> c_int;
    pub fn sni_new_local_ref(env: *mut SniEnv, obj: ScmRef) -> ScmRef;
    pub fn sni_delete_local_ref(env: *mut SniEnv, obj: ScmRef);
    pub fn sni_new_global_ref(env: *mut SniEnv, obj: ScmRef) -> ScmRef;
    pub fn sni_delete_global_ref(env: *mut SniEnv, obj: ScmRef);
    pub fn sni_is_same(env: *mut SniEnv, a: ScmRef, b: ScmRef) -> bool;
    pub fn sni_ref_is_null(obj: ScmRef) -> bool;

    pub fn sni_public_ref(
        env: *mut SniEnv,
        module_name: *const c_char,
        name: *const c_char,
        default_value: ScmRef,
    ) -> ScmRef;
    pub fn sni_private_ref(
        env: *mut SniEnv,
        module_name: *const c_char,
        name: *const c_char,
        default_value: ScmRef,
    ) -> ScmRef;
    pub fn sni_call_n(env: *mut SniEnv, proc: ScmRef, argc: c_int, argv: *const ScmRef) -> ScmRef;
    pub fn sni_call(
        env: *mut SniEnv,
        proc: ScmRef,
        a0: ScmRef,
        a1: ScmRef,
        a2: ScmRef,
        nargs: c_int,
    ) -> ScmRef;
    pub fn scm_call(
        scm: *mut Scm,
        mod_name: *const c_char,
        func_name: *const c_char,
        prepare: PrepareCallFn,
        data1: *mut c_void,
        finish: FinishCallFn,
        data2: *mut c_void,
    ) -> c_int;
    pub fn sni_call_in_native(env: *mut SniEnv, callback: NativeCallback, data: *mut c_void);

    pub fn sni_throw(env: *mut SniEnv, obj: ScmRef) -> c_int;
    pub fn sni_exception_check(env: *mut SniEnv) -> bool;
    pub fn sni_exception_occurred(env: *mut SniEnv) -> ScmRef;
    pub fn sni_exception_clear(env: *mut SniEnv) -> ScmRef;
    pub fn sni_exception_describe(env: *mut SniEnv);

    pub fn sni_is_null(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_eof(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_unspecified(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_bool(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_true(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_fixnum(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_flonum(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_char(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_pair(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_string(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_symbol(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_vector(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_bytevector(env: *mut SniEnv, obj: ScmRef) -> bool;
    pub fn sni_is_procedure(env: *mut SniEnv, obj: ScmRef) -> bool;

    pub fn sni_null(env: *mut SniEnv) -> ScmRef;
    pub fn sni_eof(env: *mut SniEnv) -> ScmRef;
    pub fn sni_unspecified(env: *mut SniEnv) -> ScmRef;
    pub fn sni_bool(env: *mut SniEnv, value: bool) -> ScmRef;
    pub fn sni_fixnum(env: *mut SniEnv, value: i32) -> ScmRef;
    pub fn sni_flonum(env: *mut SniEnv, value: f64) -> ScmRef;
    pub fn sni_char(env: *mut SniEnv, ch: u32) -> ScmRef;
    pub fn sni_intern_symbol(env: *mut SniEnv, name: *const c_char) -> ScmRef;
    pub fn sni_string(env: *mut SniEnv, data: *const c_char) -> ScmRef;
    pub fn sni_string_utf8(env: *mut SniEnv, data: *const u8, len: size_t) -> ScmRef;
    pub fn sni_cons(env: *mut SniEnv, car: ScmRef, cdr: ScmRef) -> ScmRef;
    pub fn sni_car(env: *mut SniEnv, pair: ScmRef) -> ScmRef;
    pub fn sni_cdr(env: *mut SniEnv, pair: ScmRef) -> ScmRef;
    pub fn sni_set_car(env: *mut SniEnv, pair: ScmRef, car: ScmRef);
    pub fn sni_set_cdr(env: *mut SniEnv, pair: ScmRef, cdr: ScmRef);

    pub fn sni_make_vector(env: *mut SniEnv, len: size_t, fill: ScmRef) -> ScmRef;
    pub fn sni_vector_length(env: *mut SniEnv, vector: ScmRef) -> size_t;
    pub fn sni_vector_ref(env: *mut SniEnv, vector: ScmRef, index: size_t) -> ScmRef;
    pub fn sni_vector_set(env: *mut SniEnv, vector: ScmRef, index: size_t, value: ScmRef);

    pub fn sni_make_bytevector(env: *mut SniEnv, len: size_t, fill: u8) -> ScmRef;
    pub fn sni_bytevector_length(env: *mut SniEnv, bv: ScmRef) -> size_t;
    pub fn sni_bytevector_ref(env: *mut SniEnv, bv: ScmRef, index: size_t) -> i32;
    pub fn sni_bytevector_set(env: *mut SniEnv, bv: ScmRef, index: size_t, byte: u8);

    pub fn sni_string_length(env: *mut SniEnv, s: ScmRef) -> size_t;
    pub fn sni_string_ref(env: *mut SniEnv, s: ScmRef, index: size_t) -> u32;
    pub fn sni_string_set(env: *mut SniEnv, s: ScmRef, index: size_t, ch: u32);
    pub fn sni_string_to_utf8(
        env: *mut SniEnv,
        value: ScmRef,
        buf: *mut c_char,
        capacity: size_t,
        written: *mut size_t,
    ) -> bool;
    pub fn sni_value_to_utf8(
        env: *mut SniEnv,
        value: ScmRef,
        buf: *mut c_char,
        capacity: size_t,
        written: *mut size_t,
    ) -> bool;

    pub fn sni_uint32(env: *mut SniEnv, value: u32) -> ScmRef;
    pub fn sni_uint64(env: *mut SniEnv, value: u64) -> ScmRef;
    pub fn sni_int64(env: *mut SniEnv, value: i64) -> ScmRef;
    pub fn sni_to_u8(env: *mut SniEnv, value: ScmRef, res: *mut u8) -> bool;
    pub fn sni_to_u16(env: *mut SniEnv, value: ScmRef, res: *mut u16) -> bool;
    pub fn sni_to_u32(env: *mut SniEnv, value: ScmRef, res: *mut u32) -> bool;
    pub fn sni_to_u64(env: *mut SniEnv, value: ScmRef, res: *mut u64) -> bool;
    pub fn sni_to_i8(env: *mut SniEnv, value: ScmRef, res: *mut i8) -> bool;
    pub fn sni_to_i16(env: *mut SniEnv, value: ScmRef, res: *mut i16) -> bool;
    pub fn sni_to_i32(env: *mut SniEnv, value: ScmRef, res: *mut i32) -> bool;
    pub fn sni_to_i64(env: *mut SniEnv, value: ScmRef, res: *mut i64) -> bool;
    pub fn sni_to_f32(env: *mut SniEnv, value: ScmRef, res: *mut f32) -> bool;
    pub fn sni_to_f64(env: *mut SniEnv, value: ScmRef, res: *mut f64) -> bool;
    pub fn sni_real_to_f64(env: *mut SniEnv, value: ScmRef, res: *mut f64) -> bool;
    pub fn sni_to_bool(env: *mut SniEnv, value: ScmRef) -> bool;

    pub fn sni_load_file(scm: *mut Scm, name: *const c_char) -> c_int;
    pub fn scm_load_file(scm: *mut Scm, name: *const c_char) -> c_int;
    pub fn sni_eval_string(env: *mut SniEnv, source: *const c_char) -> ScmRef;
    pub fn sni_program_arguments(env: *mut SniEnv) -> ScmRef;
    pub fn sni_program_arguments_init(env: *mut SniEnv, argc: c_int, argv: *const *const c_char);
    pub fn sni_set_program_arguments(env: *mut SniEnv, args: ScmRef);
    pub fn scm_program_arguments_init(env: *mut SniEnv, argc: size_t, argv: *const *const c_char);

    pub fn sni_get_functions(env: *mut SniEnv) -> *const SNINativeInterface;

    pub fn sni_new_native_procedure(env: *mut SniEnv, f: SniNativeFn) -> ScmRef;
    pub fn sni_define(
        env: *mut SniEnv,
        module: *const c_char,
        name: *const c_char,
        value: ScmRef,
    ) -> c_int;

    pub fn sni_make_rtd(
        env: *mut SniEnv,
        name: ScmRef,
        parent: ScmRef,
        uid: ScmRef,
        sealed: bool,
        opaque: bool,
        fields: ScmRef,
    ) -> ScmRef;
    pub fn sni_make_rcd(
        env: *mut SniEnv,
        rtd: ScmRef,
        parent_rcd: ScmRef,
        protocol: ScmRef,
    ) -> ScmRef;
    pub fn sni_record_constructor(env: *mut SniEnv, rcd: ScmRef) -> ScmRef;
    pub fn sni_record_predicate(env: *mut SniEnv, rtd: ScmRef) -> ScmRef;
    pub fn sni_record_accessor(env: *mut SniEnv, rtd: ScmRef, field: c_int) -> ScmRef;
    pub fn sni_record_mutator(env: *mut SniEnv, rtd: ScmRef, field: c_int) -> ScmRef;
    pub fn sni_is_record(env: *mut SniEnv, obj: ScmRef) -> bool;

    pub fn sni_make_class(env: *mut SniEnv, name: ScmRef, slots: ScmRef, supers: ScmRef) -> ScmRef;
    pub fn sni_make_abstract_class(env: *mut SniEnv, name: ScmRef, supers: ScmRef) -> ScmRef;
    pub fn sni_make_instance(
        env: *mut SniEnv,
        class_: ScmRef,
        n: c_int,
        initargs: *const ScmRef,
    ) -> ScmRef;
    pub fn sni_slot_ref(env: *mut SniEnv, obj: ScmRef, slot: ScmRef, unbound: ScmRef) -> ScmRef;
    pub fn sni_slot_set(env: *mut SniEnv, obj: ScmRef, slot: ScmRef, value: ScmRef) -> c_int;
    pub fn sni_class_of(env: *mut SniEnv, obj: ScmRef) -> ScmRef;
    pub fn sni_builtin_class(env: *mut SniEnv, name: *const c_char) -> ScmRef;
    pub fn sni_make_generic(env: *mut SniEnv, name: ScmRef, max_dispatch_args: c_int) -> ScmRef;
    pub fn sni_add_method(
        env: *mut SniEnv,
        generic: ScmRef,
        specializers: ScmRef,
        required_argc: c_int,
        body: ScmRef,
        locked: bool,
    ) -> c_int;

    pub fn sni_make_pod_bytevector(env: *mut SniEnv, len: size_t) -> ScmRef;
    pub fn sni_bytevector_data(env: *mut SniEnv, bv: ScmRef) -> *mut u8;
    pub fn sni_bytevector_len(env: *mut SniEnv, bv: ScmRef) -> size_t;
    pub fn sni_register_pod_type(
        env: *mut SniEnv,
        name: *const c_char,
        size: size_t,
        align: size_t,
        uid: *const c_char,
    ) -> ScmRef;
    pub fn sni_pod_type_check(env: *mut SniEnv, pod_type: ScmRef, bv: ScmRef) -> bool;

    pub fn sni_raise(env: *mut SniEnv, obj: ScmRef) -> ScmRef;
    pub fn sni_raise_continuable(env: *mut SniEnv, obj: ScmRef) -> ScmRef;
    pub fn sni_with_exception_handler(
        env: *mut SniEnv,
        handler: SniExceptionHandler,
        handler_data: *mut c_void,
        body: SniThunk,
        body_data: *mut c_void,
    ) -> ScmRef;
    pub fn sni_condition(env: *mut SniEnv, n: c_int, components: *const ScmRef) -> ScmRef;
    pub fn sni_make_message_condition(env: *mut SniEnv, msg: *const c_char) -> ScmRef;
    pub fn sni_make_who_condition(env: *mut SniEnv, who: *const c_char) -> ScmRef;
    pub fn sni_make_irritants_condition(
        env: *mut SniEnv,
        n: c_int,
        irritants: *const ScmRef,
    ) -> ScmRef;
    pub fn sni_make_assertion_violation(env: *mut SniEnv) -> ScmRef;
    pub fn sni_make_error(env: *mut SniEnv) -> ScmRef;
    pub fn sni_assertion_violation(
        env: *mut SniEnv,
        who: *const c_char,
        message: *const c_char,
        n: c_int,
        irritants: *const ScmRef,
    );

    pub fn sni_cont_alloc(
        env: *mut SniEnv,
        start: SniContFn,
        data: *mut c_void,
        drop_data: SniDropDataFn,
    ) -> *mut SniContinuation;
    pub fn sni_cont_free(env: *mut SniEnv, c: *mut SniContinuation);
    pub fn sni_cont_set_result(env: *mut SniEnv, c: *mut SniContinuation, value: ScmRef);
    pub fn sni_trampoline(env: *mut SniEnv, c: *mut SniContinuation) -> ScmRef;
    pub fn sni_call_k(
        env: *mut SniEnv,
        proc: ScmRef,
        argc: c_int,
        argv: *const ScmRef,
        c: *mut SniContinuation,
    );
    pub fn sni_raise_k(env: *mut SniEnv, obj: ScmRef, c: *mut SniContinuation);
    pub fn sni_guard_k(
        env: *mut SniEnv,
        handler: *mut SniContinuation,
        body: *mut SniContinuation,
    ) -> *mut SniContinuation;
}
