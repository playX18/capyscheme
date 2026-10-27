use std::{
    collections::{BTreeMap, VecDeque},
    sync::{Arc, LazyLock, Mutex},
};

use crate::heap::{
    Trace,
    finalizer::FinalizerQueue,
    mmtk::util::{Address, ObjectReference},
    object::{ClassId, builtin_class_ids, class_header_word},
};
use crate::{
    global,
    prelude::*,
    runtime::{
        modules::define,
        prelude::*,
        vm::ExecutionResult,
        vm::thunks::make_assertion_violation,
        vmthread::{Command, VM_THREAD},
    },
    static_symbols,
};
use libffi::{
    low::{self, CodePtr, ffi_cif, type_tag, types},
    middle::Type,
    raw::{ffi_arg, ffi_type},
};

// Not supported: `long double` and `__int128`. Neither has a `ForeignType` code, and
// libffi's `longdouble` needs a 16-byte scalar path that the Scheme number conversions
// do not provide. Callers can still move those values as byte vectors through
// `pointer-ref`/`pointer-set!` and an opaque struct type such as `#(uint8 16)`.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum ForeignType {
    Void = 0,
    Float,
    Double,
    Uint8,
    Int8,
    Uint16,
    Int16,
    Uint32,
    Int32,
    Uint64,
    Int64,
    ComplexFloat,
    ComplexDouble,
}

impl<'gc> FromValue<'gc> for ForeignType {
    fn try_from_value(
        _ctx: MutationContext<'gc>,
        value: Value<'gc>,
    ) -> Result<Self, ConversionError<'gc>> {
        if !value.is_int32() {
            return Err(ConversionError::type_mismatch(0, "fixnum", value));
        }

        let n = value.as_int32();
        if n < 0 || n > ForeignType::ComplexDouble as i32 {
            return Err(ConversionError::type_mismatch(
                0,
                "fixnum in range 0..=12",
                value,
            ));
        }

        match n {
            0 => Ok(ForeignType::Void),
            1 => Ok(ForeignType::Float),
            2 => Ok(ForeignType::Double),
            3 => Ok(ForeignType::Uint8),
            4 => Ok(ForeignType::Int8),
            5 => Ok(ForeignType::Uint16),
            6 => Ok(ForeignType::Int16),
            7 => Ok(ForeignType::Uint32),
            8 => Ok(ForeignType::Int32),
            9 => Ok(ForeignType::Uint64),
            10 => Ok(ForeignType::Int64),
            11 => Ok(ForeignType::ComplexFloat),
            12 => Ok(ForeignType::ComplexDouble),
            _ => unreachable!(),
        }
    }
}

impl<'gc> IntoValue<'gc> for ForeignType {
    fn into_value(self, _ctx: MutationContext<'gc>) -> Value<'gc> {
        Value::new(self as i32)
    }
}

impl<'gc> From<ForeignType> for Value<'gc> {
    fn from(value: ForeignType) -> Self {
        Value::new(value as i32)
    }
}

static_symbols!(
    SYM_VOID = "void"
    SYM_FLOAT = "float"
    SYM_DOUBLE = "double"
    SYM_COMPLEX_FLOAT = "complex-float"
    SYM_COMPLEX_DOUBLE = "complex-double"
    SYM_UINT8 = "uint8"
    SYM_INT8 = "int8"
    SYM_UINT16 = "uint16"
    SYM_INT16 = "int16"
    SYM_UINT32 = "uint32"
    SYM_INT32 = "int32"
    SYM_UINT64 = "uint64"
    SYM_INT64 = "int64"
    SYM_SHORT = "short"
    SYM_INT = "int"
    SYM_LONG = "long"
    SYM_UNSIGNED_SHORT = "unsigned-short"
    SYM_UNSIGNED_INT = "unsigned-int"
    SYM_UNSIGNED_LONG = "unsigned-long"
    SYM_SIZE_T = "size_t"
    SYM_SSIZE_T = "ssize_t"
    SYM_PTRDIFF_T = "ptrdiff_t"
    SYM_INTPTR_T = "intptr_t"
    SYM_UINTPTR_T = "uintptr_t"
    SYM_ASTERISK = "*"
    SYM_NULL = "%null-pointer"
    SYM_NULL_POINTER_ERROR = "null-pointer-error"
    SYM_ARRAY = "array"
    SYM_TYPED_ARGUMENT = "%typed-argument"
);

/// Rounds `len` up to a multiple of `align` (a power of two, or 0 for "no alignment").
/// Wraps instead of panicking; callers that size user-controlled types use
/// `checked_round_up`.
const fn round_up(len: usize, align: usize) -> usize {
    if align != 0 {
        len.wrapping_add(align - 1) & !(align - 1)
    } else {
        len
    }
}

/// `round_up` that reports overflow instead of wrapping.
fn checked_round_up(len: usize, align: usize) -> Option<usize> {
    if align == 0 {
        return Some(len);
    }
    len.checked_add(align - 1).map(|v| v & !(align - 1))
}

/// Largest element count accepted for an array or `#(type count)` aggregate. Anything
/// bigger would need an enormous libffi type description.
const MAX_AGGREGATE_ELEMENTS: usize = 1 << 20;

/// Largest aggregate passed or returned by value. Keeps nested arrays from building
/// libffi descriptions of gigabytes.
const MAX_AGGREGATE_BYTES: usize = 1 << 30;

fn size_overflow<'gc>(ftype: Value<'gc>) -> ConversionError<'gc> {
    ConversionError::type_mismatch(0, "foreign type with a representable size", ftype)
}

fn check_aggregate_size<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
) -> Result<(), ConversionError<'gc>> {
    if sizeof(ctx, ftype)? > MAX_AGGREGATE_BYTES {
        return Err(ConversionError::type_mismatch(
            0,
            "foreign type no larger than 1 GiB",
            ftype,
        ));
    }
    Ok(())
}

fn null_pointer_error<'gc>(call: &Call<'_, 'gc>, who: &str) -> Value<'gc> {
    call.raise_undefined_violation(who, "null-pointer dereference", &[])
}

/// `pointer + offset`, or `None` when the sum would wrap around the address space.
fn address_plus(pointer: Gc<'_, Pointer>, offset: usize) -> Option<usize> {
    (pointer.value() as usize).checked_add(offset)
}

fn offset_overflow<'gc>(call: &Call<'_, 'gc>, who: &str) -> Value<'gc> {
    call.wrong_argument_violation(
        who,
        "offset runs past the end of the address space",
        None,
        None,
        2,
        &[],
    )
}

/// A wrapped C pointer. This allows us to protect
/// from accidental UBs, but we won't take away
/// the ability to do unsafe things.
#[repr(C)]
pub struct Pointer {
    pub(crate) value: Address,
}

pub(crate) fn pointer_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::POINTER).expect("builtin class id is nonzero"),
    )
}

pub struct PointerWithFinalizers {
    queue: Mutex<VecDeque<ObjectReference>>,
    finalizers: Mutex<BTreeMap<ObjectReference, extern "C" fn(*mut std::ffi::c_void)>>,
}

impl PointerWithFinalizers {
    pub fn new() -> Self {
        Self {
            queue: Mutex::new(VecDeque::new()),
            finalizers: Mutex::new(BTreeMap::new()),
        }
    }
}

impl Default for PointerWithFinalizers {
    fn default() -> Self {
        Self::new()
    }
}
pub(crate) static POINTERS_WITH_FINALIZERS: LazyLock<Arc<PointerWithFinalizers>> =
    LazyLock::new(|| Arc::new(PointerWithFinalizers::new()));

type RootedCallbackProc = crate::Rootable!(Value<'_>);

struct CallbackData {
    ctx: *const (),
    proc: crate::heap::global::Global<RootedCallbackProc>,
}

struct CallbackOwner {
    cif: libffi::middle::Cif,
    closure: *mut low::ffi_closure,
    code: CodePtr,
    data: CallbackData,
}

impl Drop for CallbackOwner {
    fn drop(&mut self) {
        // SAFETY: `closure` was allocated by libffi::low::closure_alloc.
        unsafe {
            low::closure_free(self.closure);
        }
    }
}

// SAFETY: Correct `pop` semantics for the finalization queue
unsafe impl FinalizerQueue for PointerWithFinalizers {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        self.queue
            .lock()
            .expect("lock should not be poisoned")
            .push_back(object);
    }

    fn schedule(&self) {
        VM_THREAD.schedule_task(Command::FinalizePointers);
    }
}

impl PointerWithFinalizers {
    pub fn pop(&self) -> Option<(ObjectReference, extern "C" fn(*mut std::ffi::c_void))> {
        let mut queue = self.queue.lock().expect("lock should not be poisoned");
        if let Some(obj) = queue.pop_front() {
            let finalizer = {
                let mut finalizers = self.finalizers.lock().expect("lock should not be poisoned");
                finalizers.remove(&obj)
            };
            if let Some(finalizer) = finalizer {
                Some((obj, finalizer))
            } else {
                unreachable!("finalizer not found for object {:?}", obj)
            }
        } else {
            None
        }
    }
}

global!(
    PTRS<'gc>: Gc<'gc, WeakTable<'gc>> = (ctx) WeakTable::new(ctx, 8, 0.75);
);

// SAFETY: GC trace for `Pointer`, all reachable heap fields are visited
unsafe impl Trace for Pointer {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl Pointer {
    pub fn new(value: *mut std::ffi::c_void) -> Self {
        Self {
            value: Address::from_ptr(value as *mut u8),
        }
    }

    pub fn value(&self) -> *mut std::ffi::c_void {
        self.value.to_mut_ptr()
    }
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Pointer`
unsafe impl ClassTagged for Pointer {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::POINTER];
    const TYPE_NAME: &'static str = "pointer";
}

#[scheme(path=capy)]
pub mod ffi_ops {
    #[scheme(name = "pointer?")]
    pub fn pointerp(x: Value<'gc>) -> bool {
        x.is::<Pointer>()
    }

    #[scheme(name = "make-pointer")]
    pub fn make_pointer(addr: usize) -> Value<'gc> {
        let ptr = Pointer::new(addr as *mut std::ffi::c_void);
        let ptr = ctx.allocate_with(ptr, pointer_header_word());
        ptr.into()
    }

    #[scheme(name = "pointer-address")]
    pub fn pointer_address(p: Gc<'gc, Pointer>) -> usize {
        p.value() as usize
    }

    /// unsafely convert a pointer to a Scheme value.
    #[scheme(name = "pointer->scm")]
    pub fn pointer_to_value(p: Gc<'gc, Pointer>) -> Value<'gc> {
        Value::from_raw(p.value() as u64)
    }

    #[scheme(name = "scm->pointer")]
    pub fn value_to_pointer(v: Value<'gc>) -> Gc<'gc, Pointer> {
        let p = Pointer::new(v.bits() as *mut std::ffi::c_void);
        let p = ctx.allocate_with(p, pointer_header_word());
        p
    }

    #[scheme(name = "pointer->bytevector")]
    pub fn pointer_to_bytevector(
        pointer: Gc<'gc, Pointer>,
        length: usize,
        offset: Option<usize>,
    ) -> Value<'gc> {
        let value = pointer.value();
        if value.is_null() {
            return call.wrong_argument_violation(
                "pointer->bytevector",
                "null pointer",
                Some(pointer.into()),
                None,
                1,
                &[pointer.into()],
            );
        }

        let Some(at) = address_plus(pointer, offset.unwrap_or(0)) else {
            return offset_overflow(call, "pointer->bytevector");
        };
        // SAFETY: mapping an arbitrary address is the caller's responsibility, as with
        // `pointer-ref`.
        let bvec = ByteVector::new_mapping(ctx, unsafe { Address::from_usize(at) }, length);
        (bvec).into()
    }

    #[scheme(name = "bytevector->pointer")]
    pub fn bytevector_to_pointer(bv: Gc<'gc, ByteVector>, offset: Option<usize>) -> Value<'gc> {
        let offset = offset.unwrap_or(0);
        if offset >= bv.len() {
            return call.wrong_argument_violation(
                "bytevector->pointer",
                "offset out of bounds",
                Some(bv.into()),
                None,
                1,
                &[bv.into()],
            );
        }

        // Expose a stable address: copy movable owned BVs into NonMoving.
        // Mapped BVs already reference external memory.
        let bv = if bv.is_mapping() || bv.is_nonmoving(&ctx) {
            bv
        } else {
            let copy = ByteVector::new::<false>(ctx, bv.len(), false);
            copy.copy_from(bv.as_slice());
            copy
        };

        let ptr = bv.contents() + offset;
        let p = Pointer::new(ptr.to_mut_ptr());
        let p = ctx.allocate_with(p, pointer_header_word());

        // Ephemeron: keep the (NonMoving) bytevector alive while `p` is live.
        ptrs(ctx).put(ctx, p, bv);

        (p).into()
    }

    /// If `len` is -1 or None then string is null-terminated
    #[scheme(name = "pointer->string")]
    pub fn pointer_to_string(pointer: Gc<'gc, Pointer>, length: Option<isize>) -> Value<'gc> {
        let value = pointer.value();
        if value.is_null() {
            return null_pointer_error(call, "pointer->string");
        }

        let length = length.unwrap_or(-1);

        if length < 0 {
            // null-terminated string
            // SAFETY: Preconditions verified by the surrounding code
            let cstr = unsafe { std::ffi::CStr::from_ptr(value.cast::<std::ffi::c_char>()) };
            let string = match cstr.to_str() {
                Ok(s) => s,
                Err(_) => {
                    return call.wrong_argument_violation(
                        "pointer->string",
                        "invalid UTF-8 sequence",
                        Some(pointer.into()),
                        None,
                        1,
                        &[pointer.into()],
                    );
                }
            };
            let str_obj = Str::from_str(ctx, string);
            str_obj.into()
        } else {
            // fixed length string
            if length < 0 {
                return call.wrong_argument_violation(
                    "pointer->string",
                    "negative length",
                    Some(Value::new(length as i32)),
                    None,
                    2,
                    &[pointer.into(), Value::new(length as i32)],
                );
            }
            // SAFETY: Pointer is valid for the given element count
            let slice = unsafe { std::slice::from_raw_parts(value.cast::<u8>(), length as usize) };
            let string = match std::str::from_utf8(slice) {
                Ok(s) => s,
                Err(_) => {
                    return call.wrong_argument_violation(
                        "pointer->string",
                        "invalid UTF-8 sequence",
                        Some(pointer.into()),
                        None,
                        1,
                        &[pointer.into()],
                    );
                }
            };
            let str_obj = Str::from_str(ctx, string);
            str_obj.into()
        }
    }

    #[scheme(name = "set-pointer-finalizer!")]
    pub fn set_pointer_finalizer(p: Gc<'gc, Pointer>, finalizer: Gc<'gc, Pointer>) -> Value<'gc> {
        if p.value().is_null() {
            return null_pointer_error(call, "set-pointer-finalizer!");
        }
        let mc = ctx;
        mc.finalizers()
            .register_finalizer(&POINTERS_WITH_FINALIZERS, p);
        POINTERS_WITH_FINALIZERS
            .finalizers
            .lock()
            .expect("lock should not be poisoned")
            .insert(
                p.as_gc_object()
                    .to_object_reference()
                    .expect("gc object has object reference"),
                // SAFETY: Source and destination types have compatible layouts and sizes
                unsafe {
                    std::mem::transmute::<*mut libc::c_void, extern "C" fn(*mut libc::c_void)>(
                        finalizer.value(),
                    )
                },
            );
        Value::undefined()
    }

    #[scheme(name = "dereference-pointer")]
    pub fn dereference_pointer(p: Gc<'gc, Pointer>) -> Value<'gc> {
        if p.value().is_null() {
            return null_pointer_error(call, "dereference-pointer");
        }
        // SAFETY: Preconditions verified by the surrounding code
        let ptr = unsafe { *(p.value() as *const *mut std::ffi::c_void) };
        let new_ptr = Pointer::new(ptr);
        let new_ptr = ctx.allocate_with(new_ptr, pointer_header_word());
        new_ptr.into()
    }

    #[scheme(name = "alignof")]
    pub fn alignof_scm(ftype: Value<'gc>) -> Value<'gc> {
        match alignof(ctx, ftype) {
            Ok(align) => align.into_value(ctx),
            Err(e) => call.conversion_error("alignof", e),
        }
    }

    #[scheme(name = "sizeof")]
    pub fn sizeof_scm(ftype: Value<'gc>) -> Value<'gc> {
        match sizeof(ctx, ftype) {
            Ok(size) => size.into_value(ctx),
            Err(e) => call.conversion_error("sizeof", e),
        }
    }

    #[scheme(name = "pointer->procedure")]
    pub fn pointer_to_procedure(
        return_type: Value<'gc>,
        func_ptr: Gc<'gc, Pointer>,
        arg_types: Value<'gc>,
        variadic: Option<bool>,
        blocking: Option<bool>,
    ) -> Value<'gc> {
        let variadic = variadic.unwrap_or(false);
        // Assume foreign calls can block so they never stall the mutator.
        let blocking = blocking.unwrap_or(true);
        if func_ptr.value().is_null() {
            return call.wrong_argument_violation(
                "pointer->procedure",
                "null function pointer",
                Some(func_ptr.into()),
                None,
                2,
                &[func_ptr.into()],
            );
        }
        let cif = match make_cif(ctx, return_type, arg_types, variadic, blocking) {
            Ok(cif) => cif,
            Err(e) => return call.conversion_error("pointer->procedure", e),
        };

        let procs = PROCEDURES.fetch(ctx);
        let clos = procs.make_closure(
            ctx,
            c_foreign_call,
            [cif.into(), func_ptr.into()],
            Value::null(),
        );

        clos.into()
    }

    #[scheme(name = "procedure->callback-pointer")]
    pub fn procedure_to_callback_pointer(
        return_type: Value<'gc>,
        arg_types: Value<'gc>,
        proc: Gc<'gc, Closure<'gc>>,
    ) -> Value<'gc> {
        if !parse_ffi_type(ctx, return_type, true, &mut 0, &mut 0) {
            return call.conversion_error(
                "procedure->callback-pointer",
                ConversionError::type_mismatch(0, "foreign type", return_type),
            );
        }
        let rtype = match make_ffi_type(ctx, return_type) {
            Ok(rtype) => rtype,
            Err(e) => return call.conversion_error("procedure->callback-pointer", e),
        };
        let args = match make_ffi_arg_types(ctx, arg_types) {
            Ok(args) => args,
            Err(e) => return call.conversion_error("procedure->callback-pointer", e),
        };
        let cif = match libffi::middle::Cif::try_new(args, rtype) {
            Ok(cif) => cif,
            Err(_) => {
                return call.conversion_error(
                    "procedure->callback-pointer",
                    ConversionError::type_mismatch(
                        1,
                        "foreign function signature libffi accepts",
                        arg_types,
                    ),
                );
            }
        };
        let (closure, code) = low::closure_alloc();
        let mut owner = Box::new(CallbackOwner {
            cif,
            closure,
            code,
            data: CallbackData {
                ctx: ctx.as_ptr(),
                proc: crate::heap::global::Global::new(proc.into()),
            },
        });

        // SAFETY: The boxed owner keeps both the CIF and userdata alive until
        // `free-callback-pointer` drops it.
        let prepared = unsafe {
            low::prep_closure(
                owner.closure,
                owner.cif.as_raw_ptr(),
                scheme_callback,
                &owner.data,
                owner.code,
            )
        };
        if prepared.is_err() {
            // Dropping the owner frees the closure allocation.
            drop(owner);
            return call.conversion_error(
                "procedure->callback-pointer",
                ConversionError::type_mismatch(
                    0,
                    "callback closure libffi can prepare",
                    proc.into(),
                ),
            );
        }

        let code = owner.code.as_mut_ptr();
        let owner = Box::into_raw(owner);
        let ptr = ctx.allocate_with(Pointer::new(code), pointer_header_word());
        let owner_ptr = ctx.allocate_with(Pointer::new(owner.cast()), pointer_header_word());
        ptrs(ctx).put(ctx, ptr, owner_ptr);
        ptr.into()
    }

    #[scheme(name = "free-callback-pointer")]
    pub fn free_callback_pointer(callback: Gc<'gc, Pointer>) -> bool {
        let owner = ptrs(ctx).get(ctx, callback);
        if let Some(owner) = owner {
            ptrs(ctx).remove(ctx, callback.into());
            let owner = owner.downcast::<Pointer>().value().cast::<CallbackOwner>();
            // SAFETY: This pointer came from Box::into_raw in procedure->callback-pointer.
            unsafe {
                drop(Box::from_raw(owner));
            }
        }
        true
    }

    #[scheme(name = "ioctl/pointer")]
    pub fn ioctl(fd: i32, request: u64, argp: Gc<'gc, Pointer>) -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let res = libc::ioctl(fd, request as _, argp.value());
            Value::new(res)
        }
    }

    #[scheme(name = "errno")]
    pub fn errno() -> i32 {
        let err = ::errno::errno();
        err.0
    }

    #[scheme(name = "errno-set!")]
    pub fn errno_set(value: i32) -> Value<'gc> {
        ::errno::set_errno(::errno::Errno(value));
        Value::undefined()
    }

    /// Reads a scalar of `ftype` at `pointer + offset`.
    #[scheme(name = "pointer-ref")]
    pub fn pointer_ref(
        ftype: ForeignType,
        pointer: Gc<'gc, Pointer>,
        offset: Option<usize>,
    ) -> Value<'gc> {
        if pointer.value().is_null() {
            return null_pointer_error(call, "pointer-ref");
        }
        let Some(at) = address_plus(pointer, offset.unwrap_or(0)) else {
            return offset_overflow(call, "pointer-ref");
        };
        // SAFETY: the caller vouches that `pointer + offset` addresses a live
        // value of `ftype`; reads are unaligned-safe.
        unsafe {
            match ftype {
                ForeignType::Uint8 => Value::new((at as *const u8).read_unaligned() as i32),
                ForeignType::Int8 => Value::new((at as *const i8).read_unaligned() as i32),
                ForeignType::Uint16 => Value::new((at as *const u16).read_unaligned() as i32),
                ForeignType::Int16 => Value::new((at as *const i16).read_unaligned() as i32),
                ForeignType::Uint32 => (at as *const u32).read_unaligned().into_value(ctx),
                ForeignType::Int32 => Value::new((at as *const i32).read_unaligned()),
                ForeignType::Uint64 => (at as *const u64).read_unaligned().into_value(ctx),
                ForeignType::Int64 => (at as *const i64).read_unaligned().into_value(ctx),
                ForeignType::Float => Value::new((at as *const f32).read_unaligned() as f64),
                ForeignType::Double => Value::new((at as *const f64).read_unaligned()),
                ForeignType::Void | ForeignType::ComplexFloat | ForeignType::ComplexDouble => call
                    .wrong_argument_violation(
                        "pointer-ref",
                        "unsupported type",
                        Some(Value::new(ftype as i32)),
                        None,
                        1,
                        &[],
                    ),
            }
        }
    }

    /// Writes `value` as a scalar of `ftype` at `pointer + offset`.
    #[scheme(name = "pointer-set!")]
    pub fn pointer_set(
        ftype: ForeignType,
        pointer: Gc<'gc, Pointer>,
        offset: usize,
        value: Value<'gc>,
    ) -> Value<'gc> {
        if pointer.value().is_null() {
            return null_pointer_error(call, "pointer-set!");
        }
        let Some(at) = address_plus(pointer, offset) else {
            return offset_overflow(call, "pointer-set!");
        };
        let bad = |why: &str| {
            call.wrong_argument_violation("pointer-set!", why, Some(value), None, 4, &[])
        };

        if matches!(ftype, ForeignType::Float | ForeignType::Double) {
            let Some(n) = real_argument(ctx, value) else {
                return bad("expected a real number");
            };
            // SAFETY: see `pointer-ref`.
            unsafe {
                if ftype == ForeignType::Float {
                    (at as *mut f32).write_unaligned(n as f32);
                } else {
                    (at as *mut f64).write_unaligned(n);
                }
            }
            return Value::undefined();
        }

        let (min, max) = match ftype {
            ForeignType::Uint8 => (0, u8::MAX as i128),
            ForeignType::Int8 => (i8::MIN as i128, i8::MAX as i128),
            ForeignType::Uint16 => (0, u16::MAX as i128),
            ForeignType::Int16 => (i16::MIN as i128, i16::MAX as i128),
            ForeignType::Uint32 => (0, u32::MAX as i128),
            ForeignType::Int32 => (i32::MIN as i128, i32::MAX as i128),
            ForeignType::Uint64 => (0, u64::MAX as i128),
            ForeignType::Int64 => (i64::MIN as i128, i64::MAX as i128),
            _ => return bad("unsupported type"),
        };
        let Some(n) = value
            .number()
            .and_then(exact_integer_to_i128)
            .filter(|n| (min..=max).contains(n))
        else {
            return bad("integer out of range for type");
        };
        // SAFETY: see `pointer-ref`; `n` was range-checked for `ftype`.
        unsafe {
            match ftype {
                ForeignType::Uint8 | ForeignType::Int8 => (at as *mut u8).write_unaligned(n as u8),
                ForeignType::Uint16 | ForeignType::Int16 => {
                    (at as *mut u16).write_unaligned(n as u16)
                }
                ForeignType::Uint32 | ForeignType::Int32 => {
                    (at as *mut u32).write_unaligned(n as u32)
                }
                _ => (at as *mut u64).write_unaligned(n as u64),
            }
        }
        Value::undefined()
    }

    /// Reads a pointer-sized value at `pointer + offset` as a new pointer.
    #[scheme(name = "pointer-ref/pointer")]
    pub fn pointer_ref_pointer(pointer: Gc<'gc, Pointer>, offset: Option<usize>) -> Value<'gc> {
        if pointer.value().is_null() {
            return null_pointer_error(call, "pointer-ref/pointer");
        }
        let Some(at) = address_plus(pointer, offset.unwrap_or(0)) else {
            return offset_overflow(call, "pointer-ref/pointer");
        };
        // SAFETY: see `pointer-ref`.
        let target = unsafe { (at as *const *mut std::ffi::c_void).read_unaligned() };
        ctx.allocate_with(Pointer::new(target), pointer_header_word())
            .into()
    }

    /// Writes `value` (a pointer) at `pointer + offset`.
    #[scheme(name = "pointer-set/pointer!")]
    pub fn pointer_set_pointer(
        pointer: Gc<'gc, Pointer>,
        offset: usize,
        value: Gc<'gc, Pointer>,
    ) -> Value<'gc> {
        if pointer.value().is_null() {
            return null_pointer_error(call, "pointer-set/pointer!");
        }
        let Some(at) = address_plus(pointer, offset) else {
            return offset_overflow(call, "pointer-set/pointer!");
        };
        // SAFETY: see `pointer-ref`.
        unsafe { (at as *mut *mut std::ffi::c_void).write_unaligned(value.value()) };
        Value::undefined()
    }

    /// Allocates `size` zeroed bytes with `calloc`. Release with `foreign-free`.
    #[scheme(name = "foreign-alloc")]
    pub fn foreign_alloc(size: usize) -> Value<'gc> {
        // SAFETY: calloc has no preconditions; a null result is surfaced as #f.
        let mem = unsafe { libc::calloc(1, size.max(1)) };
        if mem.is_null() {
            return Value::new(false);
        }
        ctx.allocate_with(Pointer::new(mem), pointer_header_word())
            .into()
    }

    /// Frees memory from `foreign-alloc` (or any C `malloc`-family allocation).
    #[scheme(name = "foreign-free")]
    pub fn foreign_free(pointer: Gc<'gc, Pointer>) -> Value<'gc> {
        // SAFETY: the caller owns `pointer` and does not use it afterwards.
        unsafe { libc::free(pointer.value()) };
        Value::undefined()
    }
    #[scheme(name = "pointer-ref/u8")]
    pub fn ptr_u8_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/u8", crate::runtime::prims::memory::Elem::U8, pointer, offset)
    }

    #[scheme(name = "pointer-set!/u8")]
    pub fn ptr_u8_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/u8", crate::runtime::prims::memory::Elem::U8, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/s8")]
    pub fn ptr_s8_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/s8", crate::runtime::prims::memory::Elem::S8, pointer, offset)
    }

    #[scheme(name = "pointer-set!/s8")]
    pub fn ptr_s8_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/s8", crate::runtime::prims::memory::Elem::S8, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/u16")]
    pub fn ptr_u16_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/u16", crate::runtime::prims::memory::Elem::U16, pointer, offset)
    }

    #[scheme(name = "pointer-set!/u16")]
    pub fn ptr_u16_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/u16", crate::runtime::prims::memory::Elem::U16, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/s16")]
    pub fn ptr_s16_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/s16", crate::runtime::prims::memory::Elem::S16, pointer, offset)
    }

    #[scheme(name = "pointer-set!/s16")]
    pub fn ptr_s16_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/s16", crate::runtime::prims::memory::Elem::S16, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/u32")]
    pub fn ptr_u32_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/u32", crate::runtime::prims::memory::Elem::U32, pointer, offset)
    }

    #[scheme(name = "pointer-set!/u32")]
    pub fn ptr_u32_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/u32", crate::runtime::prims::memory::Elem::U32, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/s32")]
    pub fn ptr_s32_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/s32", crate::runtime::prims::memory::Elem::S32, pointer, offset)
    }

    #[scheme(name = "pointer-set!/s32")]
    pub fn ptr_s32_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/s32", crate::runtime::prims::memory::Elem::S32, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/u64")]
    pub fn ptr_u64_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/u64", crate::runtime::prims::memory::Elem::U64, pointer, offset)
    }

    #[scheme(name = "pointer-set!/u64")]
    pub fn ptr_u64_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/u64", crate::runtime::prims::memory::Elem::U64, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/s64")]
    pub fn ptr_s64_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/s64", crate::runtime::prims::memory::Elem::S64, pointer, offset)
    }

    #[scheme(name = "pointer-set!/s64")]
    pub fn ptr_s64_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/s64", crate::runtime::prims::memory::Elem::S64, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/f32")]
    pub fn ptr_f32_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/f32", crate::runtime::prims::memory::Elem::F32, pointer, offset)
    }

    #[scheme(name = "pointer-set!/f32")]
    pub fn ptr_f32_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/f32", crate::runtime::prims::memory::Elem::F32, pointer, offset, value)
    }

    #[scheme(name = "pointer-ref/f64")]
    pub fn ptr_f64_ref(pointer: Value<'gc>, offset: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_ref(ctx, "pointer-ref/f64", crate::runtime::prims::memory::Elem::F64, pointer, offset)
    }

    #[scheme(name = "pointer-set!/f64")]
    pub fn ptr_f64_set(pointer: Value<'gc>, offset: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        crate::runtime::prims::memory::pointer_set(ctx, "pointer-set!/f64", crate::runtime::prims::memory::Elem::F64, pointer, offset, value)
    }

}

pub fn alignof<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
) -> Result<usize, ConversionError<'gc>> {
    if ftype.is_int32() {
        let ftype = ForeignType::try_from_value(ctx, ftype)?;

        let align = match ftype {
            ForeignType::Float => std::mem::align_of::<f32>(),
            ForeignType::Double => std::mem::align_of::<f64>(),
            ForeignType::ComplexFloat => std::mem::align_of::<f32>(),
            ForeignType::ComplexDouble => std::mem::align_of::<f64>(),
            ForeignType::Uint8 | ForeignType::Int8 => std::mem::align_of::<u8>(),
            ForeignType::Uint16 | ForeignType::Int16 => std::mem::align_of::<u16>(),
            ForeignType::Uint32 | ForeignType::Int32 => std::mem::align_of::<u32>(),
            ForeignType::Uint64 | ForeignType::Int64 => std::mem::align_of::<u64>(),
            _ => {
                return Err(ConversionError::type_mismatch(
                    0,
                    "valid foreign type",
                    ftype.into(),
                ));
            }
        };
        Ok(align)
    } else if ftype == Value::new(sym_asterisk(ctx)) {
        Ok(std::mem::align_of::<*mut ()>())
    } else if ftype.is_list() {
        if ftype.list_length() == 3 && ftype.car() == sym_array(ctx).into() {
            let typ = ftype.cadr();
            return alignof(ctx, typ);
        }

        let mut max = 0;
        let mut current = ftype;
        while current.is_pair() {
            let car = current.car();
            let align = alignof(ctx, car)?;
            if align > max {
                max = align;
            }
            current = current.cdr();
        }

        Ok(max)
    } else {
        Err(ConversionError::type_mismatch(0, "foreign type", ftype))
    }
}

pub fn sizeof<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
) -> Result<usize, ConversionError<'gc>> {
    if ftype.is_int32() {
        let ftype = ForeignType::try_from_value(ctx, ftype)?;

        let size = match ftype {
            ForeignType::Void => 0,
            ForeignType::Float => std::mem::size_of::<f32>(),
            ForeignType::Double => std::mem::size_of::<f64>(),
            ForeignType::ComplexFloat => std::mem::size_of::<f32>() * 2,
            ForeignType::ComplexDouble => std::mem::size_of::<f64>() * 2,
            ForeignType::Uint8 | ForeignType::Int8 => std::mem::size_of::<u8>(),
            ForeignType::Uint16 | ForeignType::Int16 => std::mem::size_of::<u16>(),
            ForeignType::Uint32 | ForeignType::Int32 => std::mem::size_of::<u32>(),
            ForeignType::Uint64 | ForeignType::Int64 => std::mem::size_of::<u64>(),
        };
        Ok(size)
    } else if ftype == Value::new(sym_asterisk(ctx)) {
        Ok(std::mem::size_of::<*mut ()>())
    } else if ftype.is_list() {
        if ftype.list_length() == 3 && ftype.car() == sym_array(ctx).into() {
            let typ = ftype.cadr();
            let count = ftype.caddr();

            let Some(n) = count.number().and_then(|x| x.exact_integer_to_usize()) else {
                return Err(ConversionError::type_mismatch(2, "fixnum", count));
            };
            let size = sizeof(ctx, typ)?;
            let align = alignof(ctx, typ)?;
            let total = size.checked_mul(n).ok_or_else(|| size_overflow(ftype))?;
            return checked_round_up(total, align).ok_or_else(|| size_overflow(ftype));
        }
        let mut off = 0;
        let align = alignof(ctx, ftype)?;
        let mut current = ftype;
        while current.is_pair() {
            off = checked_round_up(off, alignof(ctx, current.car())?)
                .ok_or_else(|| size_overflow(ftype))?;
            off = off
                .checked_add(sizeof(ctx, current.car())?)
                .ok_or_else(|| size_overflow(ftype))?;
            current = current.cdr();
        }

        checked_round_up(off, align).ok_or_else(|| size_overflow(ftype))
    } else if ftype.is::<Vector>() {
        let v = ftype.downcast::<Vector>();
        if v.len() != 2 {
            return Err(ConversionError::type_mismatch(
                0,
                "vector of length 2",
                ftype,
            ));
        }

        let typ = v[0].get();
        let count = v[1].get();

        if !count.is_int32() {
            return Err(ConversionError::type_mismatch(1, "fixnum", count));
        }
        let count = count.as_int32();

        if count <= 0 {
            return Err(ConversionError::type_mismatch(
                1,
                "positive fixnum",
                count.into(),
            ));
        }

        let size = sizeof(ctx, typ)?;
        let total = size
            .checked_mul(count as usize)
            .ok_or_else(|| size_overflow(ftype))?;
        checked_round_up(total, alignof(ctx, typ)?).ok_or_else(|| size_overflow(ftype))
    } else {
        Err(ConversionError::type_mismatch(0, "foreign type", ftype))
    }
}

fn parse_ffi_type<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
    returnp: bool,
    n_structs: &mut usize,
    n_struct_elems: &mut usize,
) -> bool {
    if ftype.is_int32() {
        let t = ftype.as_int32();
        if t < 0 || t > ForeignType::ComplexDouble as i32 {
            false
        } else {
            !(t == ForeignType::Void as i32 && !returnp)
        }
    } else if ftype == Value::new(sym_asterisk(ctx)) {
        true
    } else if ftype.is_pair() {
        let len = ftype.list_length();
        if len < 1 {
            return false;
        }
        let mut typ = ftype;
        for _ in 0..len {
            if !parse_ffi_type(ctx, typ.car(), false, n_structs, n_struct_elems) {
                return false;
            }
            *n_struct_elems += 1;
            typ = typ.cdr();
        }
        *n_structs += 1;
        true
    } else if ftype.is::<Vector>() {
        let v = ftype.downcast::<Vector>();
        if v.len() != 2 {
            return false;
        }

        let typ = v[0].get();
        let count = v[1].get();
        if !count.is_int32() {
            return false;
        }

        let count = count.as_int32();
        if count <= 0 {
            return false;
        }

        parse_ffi_type(ctx, typ, false, n_structs, n_struct_elems)
    } else {
        false
    }
}

fn make_ffi_type<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
) -> Result<Type, ConversionError<'gc>> {
    if ftype.is_int32() {
        let typ = ForeignType::try_from_value(ctx, ftype)?;
        match typ {
            ForeignType::Void => Ok(Type::void()),
            ForeignType::Float => Ok(Type::f32()),
            ForeignType::Double => Ok(Type::f64()),
            ForeignType::ComplexFloat => Ok(Type::structure([Type::f32(), Type::f32()])),
            ForeignType::ComplexDouble => Ok(Type::structure([Type::f64(), Type::f64()])),
            ForeignType::Uint8 => Ok(Type::u8()),
            ForeignType::Int8 => Ok(Type::i8()),
            ForeignType::Uint16 => Ok(Type::u16()),
            ForeignType::Int16 => Ok(Type::i16()),
            ForeignType::Uint32 => Ok(Type::u32()),
            ForeignType::Int32 => Ok(Type::i32()),
            ForeignType::Uint64 => Ok(Type::u64()),
            ForeignType::Int64 => Ok(Type::i64()),
        }
    } else if ftype == Value::new(sym_asterisk(ctx)) {
        Ok(Type::pointer())
    } else if ftype.is_pair() {
        check_aggregate_size(ctx, ftype)?;
        let len = ftype.list_length();
        let mut elts = Vec::with_capacity(len);
        let mut typ = ftype;
        for _ in 0..len {
            elts.push(make_ffi_type(ctx, typ.car())?);
            typ = typ.cdr();
        }
        Ok(Type::structure(elts))
    } else if ftype.is::<Vector>() {
        let v = ftype.downcast::<Vector>();
        if v.len() != 2 {
            return Err(ConversionError::type_mismatch(
                0,
                "vector of length 2",
                ftype,
            ));
        }

        let elem_typ = v[0].get();
        let count = v[1].get();
        if !count.is_int32() {
            return Err(ConversionError::type_mismatch(1, "fixnum", count));
        }

        let count = count.as_int32();
        if count <= 0 {
            return Err(ConversionError::type_mismatch(
                1,
                "positive fixnum",
                count.into(),
            ));
        }
        if count as usize > MAX_AGGREGATE_ELEMENTS {
            return Err(ConversionError::type_mismatch(
                1,
                "fixnum no larger than 1048576",
                count.into(),
            ));
        }

        check_aggregate_size(ctx, ftype)?;
        let elem = make_ffi_type(ctx, elem_typ)?;
        Ok(Type::structure(vec![elem; count as usize]))
    } else {
        Err(ConversionError::type_mismatch(0, "foreign type", ftype))
    }
}

fn make_ffi_arg_types<'gc>(
    ctx: MutationContext<'gc>,
    arg_types: Value<'gc>,
) -> Result<Vec<Type>, ConversionError<'gc>> {
    if !arg_types.is_list() {
        return Err(ConversionError::type_mismatch(1, "list", arg_types));
    }

    let mut walk = arg_types;
    let mut args = Vec::with_capacity(arg_types.list_length());
    while walk.is_pair() {
        let atype = walk.car();
        if !parse_ffi_type(ctx, atype, false, &mut 0, &mut 0) {
            return Err(ConversionError::type_mismatch(1, "foreign type", atype));
        }
        args.push(make_ffi_type(ctx, atype)?);
        walk = walk.cdr();
    }
    Ok(args)
}

#[repr(C)]
pub struct Cif<'gc> {
    pub(crate) cif: libffi::middle::Cif,
    pub(crate) nargs: u32,
    pub(crate) variadic: bool,
    /// Whether the foreign call can block. Blocking calls first yield to native
    /// before using the slower calling convention.
    ///
    /// Anything that can block the mutator thread should be marked as blocking. Example:
    /// - `epoll`, `poll`, `read` etc can block
    pub(crate) blocking: bool,
    pub(crate) args: Value<'gc>,
    pub(crate) return_type: Value<'gc>,
}

fn cif_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::CIF).expect("builtin class id is nonzero"))
}

// SAFETY: `gc` for `Cif` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Cif<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CIF];
    const TYPE_NAME: &'static str = "cif";
}

// SAFETY: `gc` for `Cif` upholds all trait invariants
unsafe impl<'gc> Trace for Cif<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        visitor.trace(&mut self.args);
        visitor.trace(&mut self.return_type);
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

fn make_cif<'gc>(
    ctx: MutationContext<'gc>,
    return_type: Value<'gc>,
    arg_types: Value<'gc>,
    variadic: bool,
    blocking: bool,
) -> Result<Gc<'gc, Cif<'gc>>, ConversionError<'gc>> {
    if !arg_types.is_list() {
        return Err(ConversionError::type_mismatch(1, "list", arg_types));
    }

    if !parse_ffi_type(ctx, return_type, true, &mut 0, &mut 0) {
        return Err(ConversionError::type_mismatch(
            0,
            "foreign type",
            return_type,
        ));
    }

    let rtype = make_ffi_type(ctx, return_type)?;

    let mut walk = arg_types;
    let mut args = Vec::with_capacity(arg_types.list_length());
    while walk.is_pair() {
        let atype = walk.car();
        if !parse_ffi_type(ctx, atype, false, &mut 0, &mut 0) {
            return Err(ConversionError::type_mismatch(1, "foreign type", atype));
        }
        args.push(make_ffi_type(ctx, atype)?);
        walk = walk.cdr();
    }

    // `try_new` reports libffi's preparation failures; `new` would panic on them.
    let prepared = if !variadic {
        libffi::middle::Cif::try_new(args, rtype)
    } else {
        let nargs = args.len();
        libffi::middle::Cif::try_new_variadic(args, nargs, rtype)
    };
    let cif = prepared.map_err(|_| {
        ConversionError::type_mismatch(1, "foreign function signature libffi accepts", arg_types)
    })?;

    let cif = Cif {
        cif,
        blocking,
        nargs: arg_types.list_length() as u32,
        variadic,
        args: arg_types,
        return_type,
    };

    Ok(ctx.allocate_with(cif, cif_header_word()))
}

/// Raise `condition` as a Scheme exception so `guard` and handlers see it.
fn raise_condition<'gc>(
    call: &Call<'_, 'gc>,
    ctx: MutationContext<'gc>,
    condition: Value<'gc>,
) -> Value<'gc> {
    match ctx.globals().root_module().get_str(ctx, "raise") {
        Some(raise) => call.tail(raise, &[condition]),
        None => call.err(condition),
    }
}

fn foreign_call<'gc>(
    call: &Call<'_, 'gc>,
    ctx: MutationContext<'gc>,
    cif: Value<'gc>,
    pointer: Value<'gc>,
    rands: &[Value<'gc>],
) -> Value<'gc> {
    let cif = cif.downcast::<Cif>();
    let pointer = pointer.downcast::<Pointer>().value();

    let mut args: Vec<usize> = Vec::with_capacity(cif.nargs as usize);
    let mut arg_size = 0;

    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let raw = cif.cif.as_raw_ptr();

        for i in 0..(*raw).nargs as usize {
            arg_size += (*(*raw).arg_types.add(i).read()).size
                + (*(*raw).arg_types.add(i).read()).alignment as usize
                - 1;
        }
        // Types of the variadic arguments: explicit for `(typed TYPE VALUE)`
        // wrappers, guessed from the value otherwise.
        let mut guessed: Vec<*mut ffi_type> = Vec::new();
        let mut variadic_values: Vec<Value<'gc>> = Vec::new();
        if cif.variadic {
            for rand in rands[cif.nargs as usize..].iter() {
                let (ftype, value) = match variadic_argument(ctx, *rand) {
                    Ok(t) => t,
                    Err(e) => return raise_condition(call, ctx, e),
                };
                arg_size += (*ftype).size + (*ftype).alignment as usize - 1;
                guessed.push(ftype);
                variadic_values.push(value);
            }
        }

        arg_size +=
            (*(*raw).rtype).size + size_of::<usize>().max((*(*raw).rtype).alignment as usize);

        let mut data = Vec::<u8>::with_capacity(arg_size);
        let data_ptr = data.as_mut_ptr();
        let mut off = 0;
        for i in 0..cif.nargs as usize {
            args.push(round_up(
                data_ptr.add(off) as usize,
                (*(*raw).arg_types.add(i).read()).alignment as usize,
            ));
            match unpack(
                ctx,
                (*raw).arg_types.add(i).read(),
                rands[i],
                args[i] as *mut (),
                false,
            ) {
                Ok(()) => (),
                Err(e) => return raise_condition(call, ctx, e),
            }
            off = args[i] - data_ptr as usize + (*(*raw).arg_types.add(i).read()).size;
        }

        if cif.variadic {
            for (value, ftype) in variadic_values.iter().copied().zip(guessed.iter().copied()) {
                args.push(round_up(
                    data_ptr.add(off) as usize,
                    (*ftype).alignment as usize,
                ));
                match unpack(
                    ctx,
                    ftype,
                    value,
                    args.last().copied().expect("args non-empty") as *mut (),
                    false,
                ) {
                    Ok(()) => (),
                    Err(e) => return raise_condition(call, ctx, e),
                }
                off = args.last().copied().expect("args non-empty") - data_ptr as usize
                    + (*ftype).size;
            }
        }

        let rvalue = round_up(
            data_ptr.wrapping_add(off) as usize,
            size_of::<usize>().max((*(*raw).rtype).alignment as usize),
        ) as *mut ();

        // The prepared CIF only describes the fixed parameters. A variadic call needs one
        // describing every argument actually passed, prepared with `ffi_prep_cif_var` so the
        // platform's variadic convention (stack slots on Apple arm64) is used.
        let mut var_types: Vec<*mut ffi_type> = Vec::new();
        let mut var_cif: libffi::low::ffi_cif = std::mem::zeroed();
        let raw = if cif.variadic {
            for i in 0..cif.nargs as usize {
                var_types.push((*raw).arg_types.add(i).read());
            }
            var_types.extend_from_slice(&guessed);
            if libffi::low::prep_cif_var(
                &mut var_cif,
                libffi::low::ffi_abi_FFI_DEFAULT_ABI,
                cif.nargs as usize,
                var_types.len(),
                (*raw).rtype,
                var_types.as_mut_ptr(),
            )
            .is_err()
            {
                return call.err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "cannot prepare variadic foreign call").into(),
                    &[],
                ));
            }
            &mut var_cif as *mut libffi::low::ffi_cif
        } else {
            raw
        };

        if cif.blocking {
            let target: *mut () = pointer.cast();
            let mut args = args;
            // Capture errno on this thread right after the foreign call; leaving native
            // mode and packing the result may overwrite it.
            let errno_after_call = ctx.call_in_native(|| {
                libffi::raw::ffi_call(
                    raw,
                    // SAFETY: Invariants are upheld at this call site
                    Some(std::mem::transmute::<*mut (), unsafe extern "C" fn()>(
                        target,
                    )),
                    rvalue.cast(),
                    args.as_mut_ptr().cast(),
                );
                ::errno::errno()
            });
            let result = pack(ctx, (*raw).rtype, rvalue, true);
            ::errno::set_errno(errno_after_call);
            return match result {
                Ok(value) => value,
                Err(e) => raise_condition(call, ctx, e),
            };
        }

        libffi::raw::ffi_call(
            raw,
            Some(std::mem::transmute::<
                *mut libc::c_void,
                // SAFETY: Invariants are upheld at this call site
                unsafe extern "C" fn(),
            >(pointer)),
            rvalue.cast(),
            args.as_mut_ptr().cast(),
        );

        let errno_after_call = ::errno::errno();
        let result = pack(ctx, (*raw).rtype, rvalue, true);
        ::errno::set_errno(errno_after_call);
        match result {
            Ok(value) => value,
            Err(e) => raise_condition(call, ctx, e),
        }
    }
}

/// Foreign type and value of one variadic argument.
///
/// `(%typed-argument TYPE VALUE)`, as built by `typed` in `(core foreign)`, names the
/// C type explicitly. Any other value has its type guessed by `guess_ffi_type`.
fn variadic_argument<'gc>(
    ctx: MutationContext<'gc>,
    val: Value<'gc>,
) -> Result<(*mut ffi_type, Value<'gc>), Value<'gc>> {
    if val.is_pair() && val.car() == Value::new(sym_typed_argument(ctx)) {
        let rest = val.cdr();
        if rest.is_pair() && rest.cdr().is_pair() && rest.cdr().cdr().is_null() {
            let ftype = typed_argument_ffi_type(ctx, rest.car())?;
            return Ok((ftype, rest.cdr().car()));
        }
        return Err(make_assertion_violation(
            ctx,
            Value::new(false),
            Str::from_str(ctx, "malformed typed foreign argument").into(),
            &[val],
        ));
    }

    Ok((guess_ffi_type(ctx, val)?, val))
}

/// libffi type for an explicitly typed variadic argument. Only scalar codes and `'*`
/// are accepted; the returned types are libffi's static descriptors.
fn typed_argument_ffi_type<'gc>(
    ctx: MutationContext<'gc>,
    ftype: Value<'gc>,
) -> Result<*mut ffi_type, Value<'gc>> {
    let typ = if ftype == Value::new(sym_asterisk(ctx)) {
        Type::pointer()
    } else {
        match ForeignType::try_from_value(ctx, ftype).ok() {
            // C default argument promotions: a variadic callee reads
            // `double` and `int`, never `float` or a narrower integer.
            Some(ForeignType::Float | ForeignType::Double) => Type::f64(),
            Some(
                ForeignType::Uint8 | ForeignType::Int8 | ForeignType::Uint16 | ForeignType::Int16,
            ) => Type::i32(),
            Some(ForeignType::Uint32) => Type::u32(),
            Some(ForeignType::Int32) => Type::i32(),
            Some(ForeignType::Uint64) => Type::u64(),
            Some(ForeignType::Int64) => Type::i64(),
            Some(ForeignType::Void | ForeignType::ComplexFloat | ForeignType::ComplexDouble)
            | None => {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "unsupported foreign type for typed variadic argument")
                        .into(),
                    &[ftype],
                ));
            }
        }
    };
    Ok(typ.as_raw_ptr())
}

/// Given a value, try to guess its foreign type. Used for variadic functions.
fn guess_ffi_type<'gc>(
    ctx: MutationContext<'gc>,
    val: Value<'gc>,
) -> Result<*mut ffi_type, Value<'gc>> {
    if let Some(number) = val.number() {
        match number {
            Number::Rational(_) | Number::Flonum(_) | Number::Complex(_) => {
                return Ok(libffi::middle::Type::f64().as_raw_ptr());
            }

            Number::Fixnum(_) => {
                return Ok(libffi::middle::Type::i32().as_raw_ptr());
            }
            Number::BigInt(_) => {
                return Ok(libffi::middle::Type::i64().as_raw_ptr());
            }
        }
    }

    if val.is::<Pointer>() || val.is::<ByteVector>() || val == Value::new(false) {
        return Ok(libffi::middle::Type::pointer().as_raw_ptr());
    }

    Err(make_assertion_violation(
        ctx,
        Value::new(false),
        Str::from_str(ctx, "cannot guess foreign type for variadic argument").into(),
        &[val],
    ))
}

/// Exact integer value of `n`, or `None` when it is not an exact integer
/// or does not fit in 64 bits of magnitude.
pub(crate) fn exact_integer_to_i128(n: Number<'_>) -> Option<i128> {
    match n {
        Number::Fixnum(i) => Some(i as i128),
        Number::BigInt(b) => {
            if b.count() == 0 {
                return Some(0);
            }
            if b.count() > 1 {
                return None;
            }
            let magnitude = b[0] as i128;
            Some(if b.negative() { -magnitude } else { magnitude })
        }
        _ => None,
    }
}

/// Inclusive range of values representable by the integer `ffi_type` `t`.
fn integer_range(t: u16) -> Option<(&'static str, i128, i128)> {
    // SAFETY: the libffi `types` statics are initialized descriptors that are only read
    unsafe {
        if t == types::uint8.type_ {
            Some(("uint8", 0, u8::MAX as i128))
        } else if t == types::sint8.type_ {
            Some(("int8", i8::MIN as i128, i8::MAX as i128))
        } else if t == types::uint16.type_ {
            Some(("uint16", 0, u16::MAX as i128))
        } else if t == types::sint16.type_ {
            Some(("int16", i16::MIN as i128, i16::MAX as i128))
        } else if t == types::uint32.type_ {
            Some(("uint32", 0, u32::MAX as i128))
        } else if t == types::sint32.type_ {
            Some(("int32", i32::MIN as i128, i32::MAX as i128))
        } else if t == types::uint64.type_ {
            Some(("uint64", 0, u64::MAX as i128))
        } else if t == types::sint64.type_ {
            Some(("int64", i64::MIN as i128, i64::MAX as i128))
        } else {
            None
        }
    }
}

/// `val` as an f64 when it is a real number. Complex numbers with a nonzero imaginary
/// part are rejected here; `Number::real_to_f64` would panic on them.
pub(crate) fn real_argument<'gc>(ctx: MutationContext<'gc>, val: Value<'gc>) -> Option<f64> {
    let n = val.number()?;
    n.is_real_valued().then(|| n.real_to_f64(ctx))
}

/// Address passed for a pointer-typed argument. Accepts a Pointer (NULL included),
/// `#f` for NULL, and a bytevector whose storage cannot move while the call runs
/// (mapped, or allocated NonMoving).
fn pointer_argument<'gc>(
    ctx: MutationContext<'gc>,
    val: Value<'gc>,
) -> Result<*mut std::ffi::c_void, Value<'gc>> {
    if val == Value::new(false) {
        return Ok(std::ptr::null_mut());
    }
    if val.is::<Pointer>() {
        return Ok(val.downcast::<Pointer>().value());
    }
    if val.is::<ByteVector>() {
        let bv = val.downcast::<ByteVector>();
        if bv.is_mapping() || bv.is_nonmoving(&ctx) {
            return Ok(bv.contents().to_mut_ptr());
        }
        return Err(make_assertion_violation(
            ctx,
            Value::new(false),
            Str::from_str(
                ctx,
                "movable bytevector passed to FFI pointer; use make-bytevector/nonmoving or bytevector->pointer",
            )
            .into(),
            &[val],
        ));
    }
    Err(make_assertion_violation(
        ctx,
        Value::new(false),
        Str::from_str(ctx, "wrong type of argument for FFI pointer").into(),
        &[val],
    ))
}

/// Source bytes for a struct passed by value of `size` bytes: a non-NULL Pointer, or a
/// bytevector at least `size` bytes long.
fn struct_argument<'gc>(
    ctx: MutationContext<'gc>,
    val: Value<'gc>,
    size: usize,
) -> Result<*const u8, Value<'gc>> {
    if val.is::<Pointer>() {
        let p = val.downcast::<Pointer>().value();
        if p.is_null() {
            return Err(make_assertion_violation(
                ctx,
                Value::new(false),
                Str::from_str(ctx, "null pointer passed as FFI struct").into(),
                &[val],
            ));
        }
        return Ok(p.cast::<u8>());
    }
    if val.is::<ByteVector>() {
        let bv = val.downcast::<ByteVector>();
        if bv.len() < size {
            return Err(make_assertion_violation(
                ctx,
                Value::new(false),
                Str::from_str(
                    ctx,
                    &format!(
                        "bytevector of {} bytes is too short for FFI struct of {size} bytes",
                        bv.len()
                    ),
                )
                .into(),
                &[val],
            ));
        }
        return Ok(bv.contents().to_ptr::<u8>());
    }
    Err(make_assertion_violation(
        ctx,
        Value::new(false),
        Str::from_str(ctx, "wrong type of argument for FFI struct").into(),
        &[val],
    ))
}

// SAFETY: Caller must ensure preconditions are met (see fn docs)
unsafe fn unpack<'gc>(
    ctx: MutationContext<'gc>,
    typ: *const ffi_type,
    val: Value<'gc>,
    loc: *mut (),
    return_value: bool,
) -> Result<(), Value<'gc>> {
    let orig = typ;
    // SAFETY: Preconditions verified by the surrounding code
    let t = unsafe { (*typ).type_ };

    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        if t == types::void.type_ {
            return Ok(());
        }

        // Reject out-of-range integers up front so the per-type branches
        // below can truncate safely.
        if let Some((name, min, max)) = integer_range(t) {
            let in_range = val
                .number()
                .and_then(exact_integer_to_i128)
                .is_some_and(|v| (min..=max).contains(&v));
            if !in_range {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(
                        ctx,
                        &format!("argument out of range or wrong type for FFI {name}"),
                    )
                    .into(),
                    &[val],
                ));
            }
        }

        if t == types::float.type_ {
            let Some(n) = real_argument(ctx, val) else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI float").into(),
                    &[val],
                ));
            };

            loc.cast::<f32>().write(n as f32);
        }

        if t == types::double.type_ {
            let Some(n) = real_argument(ctx, val) else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI double").into(),
                    &[val],
                ));
            };

            loc.cast::<f64>().write(n);
        }
        /* For integer return values smaller than `int', libffi expects the
        result in an `ffi_arg'-long buffer.  */
        if t == types::uint8.type_ {
            if !val.is_int32() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint8").into(),
                    &[val],
                ));
            }

            if return_value {
                loc.cast::<ffi_arg>().write(val.as_int32() as u8 as ffi_arg);
            } else {
                loc.cast::<u8>().write(val.as_int32() as u8);
            }
        }

        if t == types::sint8.type_ {
            if !val.is_int32() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int8").into(),
                    &[val],
                ));
            }

            if return_value {
                loc.cast::<ffi_arg>().write(val.as_int32() as i8 as ffi_arg);
            } else {
                loc.cast::<i8>().write(val.as_int32() as i8);
            }
        }

        if t == types::uint16.type_ {
            if !val.is_int32() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint16").into(),
                    &[val],
                ));
            }

            if return_value {
                loc.cast::<ffi_arg>()
                    .write(val.as_int32() as u16 as ffi_arg);
            } else {
                loc.cast::<u16>().write(val.as_int32() as u16);
            }
        }

        if t == types::sint16.type_ {
            if !val.is_int32() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int16").into(),
                    &[val],
                ));
            }

            if return_value {
                loc.cast::<ffi_arg>()
                    .write(val.as_int32() as i16 as ffi_arg);
            } else {
                loc.cast::<i16>().write(val.as_int32() as i16);
            }
        }

        if t == types::uint32.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint32").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint32").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_u32();

            if return_value {
                loc.cast::<ffi_arg>().write(n as ffi_arg);
            } else {
                loc.cast::<u32>().write(n);
            }
        }

        if t == types::sint32.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int32").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int32").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_i32();

            if return_value {
                loc.cast::<ffi_arg>().write(n as ffi_arg);
            } else {
                loc.cast::<i32>().write(n);
            }
        }

        if t == types::uint64.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint64").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI uint64").into(),
                    &[val],
                ));
            }

            let n = n.coerce_exact_integer_to_u64();

            if return_value {
                loc.cast::<ffi_arg>().write(n as ffi_arg);
            } else {
                loc.cast::<u64>().write(n);
            }
        }

        if t == types::sint64.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int64").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(ctx, "wrong type of argument for FFI int64").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_i64();

            if return_value {
                loc.cast::<ffi_arg>().write(n as ffi_arg);
            } else {
                loc.cast::<i64>().write(n);
            }
        }

        if t == types::pointer.type_ {
            let p = pointer_argument(ctx, val)?;
            loc.cast::<*mut std::ffi::c_void>().write(p);
        }

        if t == type_tag::STRUCT {
            let size: usize = (*orig).size;
            let src = struct_argument(ctx, val, size)?;
            std::ptr::copy_nonoverlapping(src, loc.cast::<u8>(), size);
        }
    }

    Ok(())
}

// SAFETY: Caller must ensure preconditions are met (see fn docs)
unsafe fn pack<'gc>(
    ctx: MutationContext<'gc>,
    typ: *mut ffi_type,
    loc: *mut (),
    return_value: bool,
) -> Result<Value<'gc>, Value<'gc>> {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let t = (*typ).type_;

        if t == types::void.type_ {
            return Ok(Value::undefined());
        }

        if t == types::uint8.type_ {
            if return_value {
                return Ok(Value::new(loc.cast::<ffi_arg>().read() as u8 as u32 as i32));
            } else {
                return Ok(Value::new(loc.cast::<u8>().read() as u32 as i32));
            }
        }

        if t == types::sint8.type_ {
            if return_value {
                return Ok(Value::new(loc.cast::<ffi_arg>().read() as i8 as i32));
            } else {
                return Ok(Value::new(loc.cast::<i8>().read() as i32));
            }
        }

        if t == types::uint16.type_ {
            if return_value {
                return Ok(Value::new(loc.cast::<ffi_arg>().read() as u16 as u32 as i32));
            } else {
                return Ok(Value::new(loc.cast::<u16>().read() as u32 as i32));
            }
        }

        if t == types::sint16.type_ {
            if return_value {
                return Ok(Value::new(loc.cast::<ffi_arg>().read() as i16 as i32));
            } else {
                return Ok(Value::new(loc.cast::<i16>().read() as i32));
            }
        }

        if t == types::uint32.type_ {
            if return_value {
                return Ok((loc.cast::<ffi_arg>().read() as u32 as u64).into_value(ctx));
            } else {
                return Ok((loc.cast::<u32>().read() as u64).into_value(ctx));
            }
        }

        if t == types::sint32.type_ {
            if return_value {
                return Ok(Value::new(loc.cast::<ffi_arg>().read() as i32));
            } else {
                return Ok(Value::new(loc.cast::<i32>().read()));
            }
        }

        if t == types::uint64.type_ {
            return Ok(loc.cast::<u64>().read().into_value(ctx));
        }

        if t == types::sint64.type_ {
            return Ok(loc.cast::<i64>().read().into_value(ctx));
        }

        if t == types::double.type_ {
            let n = loc.cast::<f64>().read();
            return Ok(Value::new(n));
        }

        if t == types::float.type_ {
            let n = loc.cast::<f32>().read();
            return Ok(Value::new(n as f64));
        }

        if t == types::pointer.type_ {
            return Ok(ctx
                .allocate_with(
                    Pointer::new(loc.cast::<*mut std::ffi::c_void>().read()),
                    pointer_header_word(),
                )
                .into());
        }

        if t == type_tag::STRUCT {
            let arr = ByteVector::new::<false>(ctx, (*typ).size, false);
            std::ptr::copy_nonoverlapping(
                loc.cast::<u8>(),
                arr.contents().to_mut_ptr(),
                (*typ).size,
            );
            let ptr = Pointer::new(arr.contents().to_mut_ptr());
            let ptr = ctx.allocate_with(ptr, pointer_header_word());
            // keep bytevector alive as long as pointer is alive.
            ptrs(ctx).put(ctx, ptr, arr);
            return Ok(ptr.into());
        }

        Err(make_assertion_violation(
            ctx,
            Value::new(false),
            Str::from_str(ctx, &format!("unsupported FFI type tag: {t}")).into(),
            &[],
        ))
    }
}

unsafe extern "C" fn scheme_callback(
    cif: &ffi_cif,
    result: &mut ffi_arg,
    args: *const *const libc::c_void,
    userdata: &CallbackData,
) {
    use crate::heap::sync::thread::{Thread, ThreadState, current_thread};
    use crate::runtime::sni::LocalFrame;

    // Use the mutator of whichever thread the callback fires on: it can differ
    // from the one that created the callback (workers migrate, qsort may run on a
    // blocking thread). A thread Capy never registered has no mutator at all.
    if !crate::heap::sync::thread::is_current_thread_registed() {
        eprintln!("capy: FFI callback invoked from a thread unknown to the runtime");
        // SAFETY: libffi gives us the return slot for this CIF.
        unsafe { *result = 0 };
        return;
    }
    // SAFETY: the registered thread's thread outlives this callback.
    let ctx = unsafe {
        MutationContext::from_ptr(
            crate::heap::sync::thread::current_virtual_thread() as *const _ as *const (),
        )
    };

    let was_native = matches!(
        current_thread().get_exec_status(),
        ThreadState::InNative | ThreadState::BlockedInNative
    );
    struct NativeReenterGuard {
        was_native: bool,
    }
    impl Drop for NativeReenterGuard {
        fn drop(&mut self) {
            if self.was_native {
                Thread::enter_native();
            }
        }
    }
    if was_native {
        Thread::leave_native();
    }
    let _reenter = NativeReenterGuard { was_native };

    let proc = *userdata.proc.fetch(ctx);
    let mut frame = LocalFrame::push();
    let proc_slot = frame.new_local_ref(proc).as_ptr();
    let mut arg_slots: Vec<*mut Value<'static>> = Vec::with_capacity(cif.nargs as usize);

    for i in 0..cif.nargs as usize {
        // SAFETY: libffi supplies one argument slot per CIF argument.
        let arg_loc = unsafe { *args.add(i) as *mut () };
        // SAFETY: `arg_types` belongs to the live CIF passed by libffi.
        let arg_type = unsafe { *cif.arg_types.add(i) };
        // SAFETY: libffi argument locations match the CIF argument types.
        let arg = match unsafe { pack(ctx, arg_type, arg_loc, false) } {
            Ok(arg) => arg,
            Err(err) => {
                // An exception cannot unwind through C frames, and the callback has no
                // way to return one. Report it and return zeroes to the caller.
                eprintln!("capy: FFI callback argument could not be converted: {err}");
                // SAFETY: libffi gives us the return slot for this CIF.
                unsafe { *result = 0 };
                return;
            }
        };
        arg_slots.push(frame.new_local_ref(arg).as_ptr());
    }

    // SAFETY: OopStorage slots live until `frame` drops.
    let value = unsafe {
        let proc = std::mem::transmute::<Value<'static>, Value<'_>>(*proc_slot);
        let scheme_args: Vec<Value<'_>> = arg_slots
            .iter()
            .map(|slot| std::mem::transmute::<Value<'static>, Value<'_>>(**slot))
            .collect();
        match crate::runtime::sni::call_function(ctx, proc, scheme_args) {
            ExecutionResult::Ok(value) => value,
            ExecutionResult::Err(err) => {
                // An exception cannot unwind through C frames. Report it rather than
                // silently returning 0 to the foreign caller.
                eprintln!("capy: uncaught exception in FFI callback: {err}");
                *result = 0;
                return;
            }
        }
    };
    let value_slot = frame.new_local_ref(value).as_ptr();
    let value = unsafe { std::mem::transmute::<Value<'static>, Value<'_>>(*value_slot) };

    // SAFETY: libffi provided `result` as the return storage for this CIF.
    let converted = unsafe {
        unpack(
            ctx,
            cif.rtype,
            value,
            (result as *mut ffi_arg).cast::<()>(),
            true,
        )
    };
    if let Err(err) = converted {
        eprintln!("capy: FFI callback returned a value of the wrong type: {err}");
        // Return zeroes rather than whatever libffi left in the slot. libffi sizes the slot
        // to at least `ffi_arg`, and to the aggregate's size for struct returns.
        // SAFETY: see above; the slot covers `len` bytes.
        unsafe {
            let len = (*cif.rtype).size.max(size_of::<ffi_arg>());
            std::ptr::write_bytes(result as *mut ffi_arg as *mut u8, 0, len);
        }
    }
}

pub(crate) extern "C" fn c_foreign_call<'gc>(
    ctx: MutationContext<'gc>,
    sfp: Slot<'gc>,
) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        let closure = call.rator.downcast::<Closure>();

        let cif = closure[1].get().downcast::<Cif>();
        let pointer = closure[2].get().downcast::<Pointer>();

        let fits = if cif.variadic {
            call.args.len() >= cif.nargs as usize
        } else {
            call.args.len() == cif.nargs as usize
        };

        if !fits {
            let err = make_assertion_violation(
                ctx,
                Value::new(false),
                Str::from_str(
                    ctx,
                    format!(
                        "wrong number of arguments to foreign function, expected {}{}, got {}",
                        if cif.variadic { "at least " } else { "" },
                        cif.nargs,
                        call.args.len(),
                    ),
                )
                .into(),
                &[],
            );

            return call.run(|call| raise_condition(call, ctx, err));
        }

        call.run(|call| foreign_call(call, ctx, cif.into(), pointer.into(), call.args))
    })
}

pub(crate) fn init_ffi<'gc>(ctx: MutationContext<'gc>) {
    ffi_ops::register(ctx);

    define(ctx, "%void", ForeignType::Void);
    define(ctx, "%float", ForeignType::Float);
    define(ctx, "%double", ForeignType::Double);
    define(ctx, "%complex-float", ForeignType::ComplexFloat);
    define(ctx, "%complex-double", ForeignType::ComplexDouble);
    define(ctx, "%uint8", ForeignType::Uint8);
    define(ctx, "%int8", ForeignType::Int8);
    define(ctx, "%uint16", ForeignType::Uint16);
    define(ctx, "%int16", ForeignType::Int16);
    define(ctx, "%uint32", ForeignType::Uint32);
    define(ctx, "%int32", ForeignType::Int32);
    define(ctx, "%uint64", ForeignType::Uint64);
    define(ctx, "%int64", ForeignType::Int64);

    if size_of::<std::ffi::c_short>() == 2 {
        define(ctx, "%short", ForeignType::Int16);
        define(ctx, "%unsigned-short", ForeignType::Uint16);
    } else if size_of::<std::ffi::c_short>() == 4 {
        define(ctx, "%short", ForeignType::Int32);
        define(ctx, "%unsigned-short", ForeignType::Uint32);
    } else if size_of::<std::ffi::c_short>() == 8 {
        define(ctx, "%short", ForeignType::Int64);
        define(ctx, "%unsigned-short", ForeignType::Uint64);
    }

    if size_of::<std::ffi::c_int>() == 2 {
        define(ctx, "%int", ForeignType::Int16);
        define(ctx, "%unsigned-int", ForeignType::Uint16);
    } else if size_of::<std::ffi::c_int>() == 4 {
        define(ctx, "%int", ForeignType::Int32);
        define(ctx, "%unsigned-int", ForeignType::Uint32);
    } else if size_of::<std::ffi::c_int>() == 8 {
        define(ctx, "%int", ForeignType::Int64);
        define(ctx, "%unsigned-int", ForeignType::Uint64);
    }

    if size_of::<std::ffi::c_long>() == 2 {
        define(ctx, "%long", ForeignType::Int16);
        define(ctx, "%unsigned-long", ForeignType::Uint16);
    } else if size_of::<std::ffi::c_long>() == 4 {
        define(ctx, "%long", ForeignType::Int32);
        define(ctx, "%unsigned-long", ForeignType::Uint32);
    } else if size_of::<std::ffi::c_long>() == 8 {
        define(ctx, "%long", ForeignType::Int64);
        define(ctx, "%unsigned-long", ForeignType::Uint64);
    }

    if size_of::<std::ffi::c_longlong>() == 2 {
        define(ctx, "%long-long", ForeignType::Int16);
        define(ctx, "%unsigned-long-long", ForeignType::Uint16);
    } else if size_of::<std::ffi::c_longlong>() == 4 {
        define(ctx, "%long-long", ForeignType::Int32);
        define(ctx, "%unsigned-long-long", ForeignType::Uint32);
    } else if size_of::<std::ffi::c_longlong>() == 8 {
        define(ctx, "%long-long", ForeignType::Int64);
        define(ctx, "%unsigned-long-long", ForeignType::Uint64);
    }

    if size_of::<libc::ssize_t>() == 2 {
        define(ctx, "%ssize_t", ForeignType::Int16);
        define(ctx, "%size_t", ForeignType::Uint16);
    } else if size_of::<libc::ssize_t>() == 4 {
        define(ctx, "%ssize_t", ForeignType::Int32);
        define(ctx, "%size_t", ForeignType::Uint32);
    } else if size_of::<libc::ssize_t>() == 8 {
        define(ctx, "%ssize_t", ForeignType::Int64);
        define(ctx, "%size_t", ForeignType::Uint64);
    }

    if size_of::<libc::intptr_t>() == 4 {
        define(ctx, "%intptr_t", ForeignType::Int32);
        define(ctx, "%uintptr_t", ForeignType::Uint32);
    } else if size_of::<libc::intptr_t>() == 8 {
        define(ctx, "%intptr_t", ForeignType::Int64);
        define(ctx, "%uintptr_t", ForeignType::Uint64);
    }

    if size_of::<libc::ptrdiff_t>() == 4 {
        define(ctx, "%ptrdiff_t", ForeignType::Int32);
    } else if size_of::<libc::ptrdiff_t>() == 8 {
        define(ctx, "%ptrdiff_t", ForeignType::Int64);
    }

    let nullp = ctx.allocate_with(Pointer::new(std::ptr::null_mut()), pointer_header_word());

    define(ctx, "%null-pointer", nullp);

    define(ctx, "RTLD_LAZY", Value::new(libc::RTLD_LAZY));
    define(ctx, "RTLD_NOW", Value::new(libc::RTLD_NOW));
    define(ctx, "RTLD_GLOBAL", Value::new(libc::RTLD_GLOBAL));
    define(ctx, "RTLD_LOCAL", Value::new(libc::RTLD_LOCAL));
}
