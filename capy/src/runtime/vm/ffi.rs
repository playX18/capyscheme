use std::{
    cell::Cell,
    collections::{BTreeMap, VecDeque},
    sync::{Arc, LazyLock, Mutex, atomic::Ordering},
};

use crate::{
    CAN_PIN_OBJECTS,
    rsgc::{
        Trace,
        finalizer::FinalizerQueue,
        mmtk::util::{Address, ObjectReference},
        object::GCObject,
    },
    runtime::BlockingOperationWithReturn,
};
use crate::{
    global,
    prelude::*,
    runtime::{
        modules::define,
        prelude::*,
        vm::thunks::make_assertion_violation,
        vmthread::{VM_THREAD, VMThreadTask},
    },
    static_symbols,
};
use libffi::{
    low::{type_tag, types},
    middle::Type,
    raw::{ffi_arg, ffi_type},
};

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
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
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
    fn into_value(self, _ctx: Context<'gc>) -> Value<'gc> {
        Value::new(self as i32)
    }
}

impl<'gc> Into<Value<'gc>> for ForeignType {
    fn into(self) -> Value<'gc> {
        Value::new(self as i32)
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
);

const fn round_up(len: usize, align: usize) -> usize {
    if align != 0 {
        ((len - 1) | (align - 1)) + 1
    } else {
        len
    }
}

fn null_pointer_error<'a, 'gc, R: TryIntoValues<'gc>>(
    nctx: NativeCallContext<'a, 'gc, R>,
    who: &str,
) -> NativeCallReturn<'gc> {
    nctx.raise_undefined_violation(who, "null-pointer dereference", &[])
}

/// A wrapped C pointer. This allows us to protect
/// from accidental UBs, but we won't take away
/// the ability to do unsafe things.
#[repr(C)]
pub struct Pointer {
    header: ScmHeader,
    pub(crate) value: Address,
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
pub(crate) static POINTERS_WITH_FINALIZERS: LazyLock<Arc<PointerWithFinalizers>> =
    LazyLock::new(|| Arc::new(PointerWithFinalizers::new()));

unsafe impl FinalizerQueue for PointerWithFinalizers {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        self.queue.lock().unwrap().push_back(object);
    }

    fn schedule(&self) {
        VM_THREAD.schedule_task(VMThreadTask::FinalizePointers);
    }
}

impl PointerWithFinalizers {
    pub fn pop(&self) -> Option<(ObjectReference, extern "C" fn(*mut std::ffi::c_void))> {
        let mut queue = self.queue.lock().unwrap();
        if let Some(obj) = queue.pop_front() {
            let finalizer = {
                let mut finalizers = self.finalizers.lock().unwrap();
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
    PTRS<'gc>: Gc<'gc, WeakTable<'gc>> = (ctx) WeakTable::new(*ctx, 8, 0.75);
);

unsafe impl Trace for Pointer {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl Pointer {
    pub fn new(value: *mut std::ffi::c_void) -> Self {
        Self {
            header: ScmHeader::with_type_bits(TypeCode8::POINTER.bits() as _),
            value: Address::from_ptr(value as *mut u8),
        }
    }

    pub fn value(&self) -> *mut std::ffi::c_void {
        self.value.to_mut_ptr()
    }
}

unsafe impl Tagged for Pointer {
    const TC8: TypeCode8 = TypeCode8::POINTER;
    const TYPE_NAME: &'static str = "pointer";
}

#[scheme(path=capy)]
pub mod ffi_ops {
    #[scheme(name = "pointer?")]
    pub fn pointerp(x: Value<'gc>) -> bool {
        nctx.return_(x.is::<Pointer>())
    }

    #[scheme(name = "make-pointer")]
    pub fn make_pointer(addr: usize) -> Value<'gc> {
        let ptr = Pointer::new(addr as *mut std::ffi::c_void);
        let ptr = Gc::new(*nctx.ctx, ptr);
        nctx.return_(ptr.into())
    }

    #[scheme(name = "pointer-address")]
    pub fn pointer_address(p: Gc<'gc, Pointer>) -> usize {
        nctx.return_(p.value() as usize)
    }

    /// unsafely convert a pointer to a Scheme value.
    #[scheme(name = "pointer->scm")]
    pub fn pointer_to_value(p: Gc<'gc, Pointer>) -> Value<'gc> {
        nctx.return_(Value::from_raw(p.value() as u64))
    }

    #[scheme(name = "scm->pointer")]
    pub fn value_to_pointer(v: Value<'gc>) -> Gc<'gc, Pointer> {
        let p = Pointer::new(v.bits() as *mut std::ffi::c_void);
        let p = Gc::new(*nctx.ctx, p);
        nctx.return_(p)
    }

    #[scheme(name = "pointer->bytevector")]
    pub fn pointer_to_bytevector(
        pointer: Gc<'gc, Pointer>,
        length: usize,
        offset: Option<usize>,
    ) -> Gc<'gc, ByteVector> {
        let value = pointer.value();
        if value.is_null() {
            return nctx.wrong_argument_violation(
                "pointer->bytevector",
                "null pointer",
                Some(pointer.into()),
                None,
                1,
                &[pointer.into()],
            );
        }

        let offset = offset.unwrap_or(0);
        let ptr = pointer.value + offset;
        let bvec = ByteVector::new_mapping(*nctx.ctx, ptr, length);
        nctx.return_(bvec)
    }

    #[scheme(name = "bytevector->pointer")]
    pub fn bytevector_to_pointer(
        bv: Gc<'gc, ByteVector>,
        offset: Option<usize>,
    ) -> Gc<'gc, Pointer> {
        let offset = offset.unwrap_or(0);
        if offset >= bv.len() {
            return nctx.wrong_argument_violation(
                "bytevector->pointer",
                "offset out of bounds",
                Some(bv.into()),
                None,
                1,
                &[bv.into()],
            );
        }

        let ptr = bv.contents() + offset;
        let p = Pointer::new(ptr.to_mut_ptr());
        let p = Gc::new(*nctx.ctx, p);
        // TODO(Adel): pin bytevector in memory OR move contents to non-moving space.

        // create ephemeron to keep the bytevector alive if `p` is alive.
        ptrs(nctx.ctx).put(nctx.ctx, p, bv);

        nctx.return_(p)
    }

    /// If `len` is -1 or None then string is null-terminated
    #[scheme(name = "pointer->string")]
    pub fn pointer_to_string(
        pointer: Gc<'gc, Pointer>,
        length: Option<isize>,
    ) -> Gc<'gc, Str<'gc>> {
        let value = pointer.value();
        if value.is_null() {
            return null_pointer_error(nctx, "pointer->string");
        }

        let length = length.unwrap_or(-1);

        if length < 0 {
            // null-terminated string
            let cstr = unsafe { std::ffi::CStr::from_ptr(value.cast::<std::ffi::c_char>()) };
            let string = match cstr.to_str() {
                Ok(s) => s,
                Err(_) => {
                    return nctx.wrong_argument_violation(
                        "pointer->string",
                        "invalid UTF-8 sequence",
                        Some(pointer.into()),
                        None,
                        1,
                        &[pointer.into()],
                    );
                }
            };
            let str_obj = Str::from_str(*nctx.ctx, string);
            nctx.return_(str_obj)
        } else {
            // fixed length string
            if length < 0 {
                return nctx.wrong_argument_violation(
                    "pointer->string",
                    "negative length",
                    Some(Value::new(length as i32)),
                    None,
                    2,
                    &[pointer.into(), Value::new(length as i32)],
                );
            }
            let slice = unsafe { std::slice::from_raw_parts(value.cast::<u8>(), length as usize) };
            let string = match std::str::from_utf8(slice) {
                Ok(s) => s,
                Err(_) => {
                    return nctx.wrong_argument_violation(
                        "pointer->string",
                        "invalid UTF-8 sequence",
                        Some(pointer.into()),
                        None,
                        1,
                        &[pointer.into()],
                    );
                }
            };
            let str_obj = Str::from_str(*nctx.ctx, string);
            nctx.return_(str_obj)
        }
    }

    #[scheme(name = "set-pointer-finalizer!")]
    pub fn set_pointer_finalizer(p: Gc<'gc, Pointer>, finalizer: Gc<'gc, Pointer>) -> Value<'gc> {
        if p.value().is_null() {
            return null_pointer_error(nctx, "set-pointer-finalizer!");
        }
        let mc = *nctx.ctx;
        mc.finalizers()
            .register_finalizer(&POINTERS_WITH_FINALIZERS, p);
        POINTERS_WITH_FINALIZERS
            .finalizers
            .lock()
            .unwrap()
            .insert(p.as_gcobj().to_objref().unwrap(), unsafe {
                std::mem::transmute(finalizer.value())
            });
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "dereference-pointer")]
    pub fn dereference_pointer(p: Gc<'gc, Pointer>) -> Gc<'gc, Pointer> {
        if p.value().is_null() {
            return null_pointer_error(nctx, "dereference-pointer");
        }
        let ptr = unsafe { *(p.value() as *const *mut std::ffi::c_void) };
        let new_ptr = Pointer::new(ptr);
        let new_ptr = Gc::new(*nctx.ctx, new_ptr);
        nctx.return_(new_ptr)
    }

    #[scheme(name = "alignof")]
    pub fn alignof_scm(ftype: Value<'gc>) -> usize {
        let ctx = nctx.ctx;
        match alignof(ctx, ftype) {
            Ok(align) => nctx.return_(align),
            Err(e) => nctx.conversion_error("alignof", e),
        }
    }

    #[scheme(name = "sizeof")]
    pub fn sizeof_scm(ftype: Value<'gc>) -> usize {
        let ctx = nctx.ctx;
        match sizeof(ctx, ftype) {
            Ok(size) => nctx.return_(size),
            Err(e) => nctx.conversion_error("sizeof", e),
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
        let blocking = blocking.unwrap_or(true); // default to true: we can't risk CIF blocking mutator thread
        let cif = match make_cif(nctx.ctx, return_type, arg_types, variadic, blocking) {
            Ok(cif) => cif,
            Err(e) => return nctx.conversion_error("pointer->procedure", e),
        };

        let procs = PROCEDURES.fetch(*nctx.ctx);
        let clos = procs.make_closure(
            nctx.ctx,
            c_foreign_call,
            [cif.into(), func_ptr.into()],
            Value::null(),
        );

        nctx.return_(clos.into())
    }

    #[scheme(name = "ioctl/pointer")]
    pub fn ioctl(fd: i32, request: u64, argp: Gc<'gc, Pointer>) -> Value<'gc> {
        unsafe {
            let res = libc::ioctl(fd, request, argp.value());
            nctx.return_(Value::new(res))
        }
    }

    #[scheme(name = "errno")]
    pub fn errno() -> i32 {
        let err = ::errno::errno();
        nctx.return_(err.0)
    }
}

pub fn alignof<'gc>(ctx: Context<'gc>, ftype: Value<'gc>) -> Result<usize, ConversionError<'gc>> {
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

pub fn sizeof<'gc>(ctx: Context<'gc>, ftype: Value<'gc>) -> Result<usize, ConversionError<'gc>> {
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
            return Ok(round_up(size * n, align));
        }
        let mut off = 0;
        let align = alignof(ctx, ftype)?;
        let mut current = ftype;
        while current.is_pair() {
            off = round_up(off, alignof(ctx, current.car())?);
            off += sizeof(ctx, current.car())?;
            current = current.cdr();
        }

        Ok(round_up(off, align))
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
        Ok(round_up(size * (count as usize), alignof(ctx, typ)?))
    } else {
        Err(ConversionError::type_mismatch(0, "foreign type", ftype))
    }
}

fn parse_ffi_type<'gc>(
    ctx: Context<'gc>,
    ftype: Value<'gc>,
    returnp: bool,
    n_structs: &mut usize,
    n_struct_elems: &mut usize,
) -> bool {
    if ftype.is_int32() {
        let t = ftype.as_int32();
        if t < 0 || t > ForeignType::ComplexDouble as i32 {
            return false;
        } else if t == ForeignType::Void as i32 && !returnp {
            return false;
        } else {
            return true;
        }
    } else if ftype == Value::new(sym_asterisk(ctx)) {
        return true;
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
        return true;
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

        return parse_ffi_type(ctx, typ, false, n_structs, n_struct_elems);
    } else {
        return false;
    }
}

fn make_ffi_type<'gc>(ctx: Context<'gc>, ftype: Value<'gc>) -> Result<Type, ConversionError<'gc>> {
    if ftype.is_int32() {
        let typ = ForeignType::try_from_value(ctx, ftype)?;
        match typ {
            ForeignType::Void => Ok(Type::void()),
            ForeignType::Float => Ok(Type::f32()),
            ForeignType::Double => Ok(Type::f64()),
            ForeignType::ComplexFloat => todo!(), // Ok(Type::c32()),
            ForeignType::ComplexDouble => todo!(), //Ok(Type::c64()),
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

        let _typ = v[0].get();
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

        todo!()
    } else {
        Err(ConversionError::type_mismatch(0, "foreign type", ftype))
    }
}

#[repr(C)]
pub struct CIF<'gc> {
    pub(crate) header: ScmHeader,
    pub(crate) cif: libffi::middle::Cif,
    pub(crate) nargs: u32,
    pub(crate) variadic: bool,
    /// Is this CIF blocking? If so we use slightly slower
    /// calling convention where we first yield to native and then perform a call.
    ///
    /// Anything that can block the mutator thread should be marked as blocking. Example:
    /// - `epoll`, `poll`, `read` etc can block
    pub(crate) blocking: bool,
    pub(crate) args: Value<'gc>,
    pub(crate) return_type: Value<'gc>,
}

unsafe impl<'gc> Tagged for CIF<'gc> {
    const TC8: TypeCode8 = TypeCode8::CIF;
    const TYPE_NAME: &'static str = "cif";
}

unsafe impl<'gc> Trace for CIF<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        visitor.trace(&mut self.args);
        visitor.trace(&mut self.return_type);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[allow(dead_code)]
pub(crate) fn make_cif_at<'gc>(
    ctx: Context<'gc>,
    obj: GCObject,
    arg_types: Value<'gc>,
    return_type: Value<'gc>,
    variadic: bool,
    blocking: bool,
) -> Result<(), ConversionError<'gc>> {
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

    let cif = if !variadic {
        libffi::middle::Cif::new(args, rtype)
    } else {
        let nargs = args.len();
        libffi::middle::Cif::new_variadic(args, nargs, rtype)
    };

    let cif = CIF {
        header: unsafe { obj.to_address().to_mut_ptr::<ScmHeader>().read() },
        cif,
        nargs: arg_types.list_length() as u32,
        variadic,
        blocking,
        args: arg_types,
        return_type,
    };

    unsafe {
        obj.to_address().to_mut_ptr::<CIF>().write(cif);
    }

    Ok(())
}

fn make_cif<'gc>(
    ctx: Context<'gc>,
    return_type: Value<'gc>,
    arg_types: Value<'gc>,
    variadic: bool,
    blocking: bool,
) -> Result<Gc<'gc, CIF<'gc>>, ConversionError<'gc>> {
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

    let cif = if !variadic {
        libffi::middle::Cif::new(args, rtype)
    } else {
        let nargs = args.len();
        libffi::middle::Cif::new_variadic(args, nargs, rtype)
    };

    let cif = CIF {
        header: ScmHeader::with_type_bits(TypeCode8::CIF.bits() as _),
        cif,
        blocking,
        nargs: arg_types.list_length() as u32,
        variadic,
        args: arg_types,
        return_type,
    };

    Ok(Gc::new(*ctx, cif))
}

unsafe fn foreign_call<'a, 'gc>(
    nctx: NativeCallContext<'a, 'gc>,
    ctx: Context<'gc>,
    cif: Value<'gc>,
    pointer: Value<'gc>,
    num_rands: usize,
    rands: *const Value<'gc>,
) -> NativeCallReturn<'gc> {
    let rands = unsafe { std::slice::from_raw_parts(rands, num_rands) };
    let cif = cif.downcast::<CIF>();
    let pointer = pointer.downcast::<Pointer>().value();

    let mut args: Vec<usize> = Vec::with_capacity(cif.nargs as usize);
    let mut arg_size = 0;

    unsafe {
        let raw = cif.cif.as_raw_ptr();

        for i in 0..(*raw).nargs as usize {
            arg_size += (*(*raw).arg_types.add(i).read()).size
                + (*(*raw).arg_types.add(i).read()).alignment as usize
                - 1;
        }
        if cif.variadic {
            for rand in rands[cif.nargs as usize..].iter() {
                let ftype = match guess_ffi_type(ctx, *rand) {
                    Ok(t) => t,
                    Err(e) => return nctx.return_error(e),
                };
                arg_size += (*ftype).size + (*ftype).alignment as usize - 1;
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
                Err(e) => return nctx.return_error(e),
            }
            off = args[i] - data_ptr as usize + (*(*raw).arg_types.add(i).read()).size;
        }

        if cif.variadic {
            for rand in rands[cif.nargs as usize..].iter() {
                let ftype = match guess_ffi_type(ctx, *rand) {
                    Ok(t) => t,
                    Err(e) => {
                        return nctx.return_error(e);
                    }
                };
                args.push(round_up(
                    data_ptr.add(off) as usize,
                    (*ftype).alignment as usize,
                ));
                match unpack(
                    ctx,
                    ftype,
                    *rand,
                    args.last().copied().unwrap() as *mut (),
                    false,
                ) {
                    Ok(()) => (),
                    Err(e) => return nctx.return_error(e),
                }
                off = args.last().copied().unwrap() - data_ptr as usize + (*ftype).size;
            }
        }

        let rvalue = round_up(
            data_ptr.wrapping_add(off) as usize,
            size_of::<usize>().max((*(*raw).rtype).alignment as usize),
        ) as *mut ();

        if cif.blocking {
            let ffi_op = FFICallOperation {
                arg_size,
                cif,
                arguments: rands.to_vec(),
                args: Cell::new(args),
                data: Cell::new(data),
                rvalue,
                target: pointer.cast(),
            };
            let retk = nctx.retk;
            // yield to native, then run ffi_op and only then return to retk.
            return nctx.perform_returning_to(retk, ffi_op);
        }

        libffi::raw::ffi_call(
            raw,
            Some(std::mem::transmute(pointer)),
            rvalue.cast(),
            args.as_mut_ptr().cast(),
        );

        /*ThunkResult {
            code: 0,
            value: pack(ctx, (*raw).rtype, rvalue, true),
        }*/
        nctx.return_(pack(ctx, (*raw).rtype, rvalue, true))
    }
}

/// Given a value, try to guess its foreign type. Used for variadic functions.
fn guess_ffi_type<'gc>(ctx: Context<'gc>, val: Value<'gc>) -> Result<*mut ffi_type, Value<'gc>> {
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

    if val.is::<Pointer>() || val.is::<ByteVector>() {
        return Ok(libffi::middle::Type::pointer().as_raw_ptr());
    }

    Err(make_assertion_violation(
        ctx,
        Value::new(false),
        Str::from_str(*ctx, "cannot guess foreign type for variadic argument").into(),
        &[val],
    ))
}

unsafe fn unpack<'gc>(
    ctx: Context<'gc>,
    typ: *const ffi_type,
    val: Value<'gc>,
    loc: *mut (),
    return_value: bool,
) -> Result<(), Value<'gc>> {
    let orig = typ;
    let t = unsafe { (*typ).type_ };

    unsafe {
        if t == types::void.type_ {
            return Ok(());
        }

        if t == types::float.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI float").into(),
                    &[val],
                ));
            };

            let n = n.real_to_f64(ctx) as f32;
            loc.cast::<f32>().write(n);
        }

        if t == types::double.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI double").into(),
                    &[val],
                ));
            };

            let n = n.real_to_f64(ctx);
            loc.cast::<f64>().write(n);
        }
        /* For integer return values smaller than `int', libffi expects the
        result in an `ffi_arg'-long buffer.  */
        if t == types::uint8.type_ {
            if !val.is_int32() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI uint8").into(),
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
                    Str::from_str(*ctx, "wrong type of argument for FFI int8").into(),
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
                    Str::from_str(*ctx, "wrong type of argument for FFI uint16").into(),
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
                    Str::from_str(*ctx, "wrong type of argument for FFI int16").into(),
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
                    Str::from_str(*ctx, "wrong type of argument for FFI uint32").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI uint32").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_u32();

            if return_value {
                loc.cast::<ffi_arg>().write(n as u32 as ffi_arg);
            } else {
                loc.cast::<u32>().write(val.as_int32() as u32);
            }
        }

        if t == types::sint32.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI int32").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI int32").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_i32();

            if return_value {
                loc.cast::<ffi_arg>().write(n as i32 as ffi_arg);
            } else {
                loc.cast::<i32>().write(n as i32);
            }
        }

        if t == types::uint64.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI uint64").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI uint64").into(),
                    &[val],
                ));
            }

            let n = n.coerce_exact_integer_to_u64();

            if return_value {
                loc.cast::<ffi_arg>().write(n as u64 as ffi_arg);
            } else {
                loc.cast::<u64>().write(n as u64);
            }
        }

        if t == types::sint64.type_ {
            let Some(n) = val.number() else {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI int64").into(),
                    &[val],
                ));
            };
            if !n.is_exact_integer() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI int64").into(),
                    &[val],
                ));
            }
            let n = n.coerce_exact_integer_to_i64();

            if return_value {
                loc.cast::<ffi_arg>().write(n as i64 as ffi_arg);
            } else {
                loc.cast::<i64>().write(n as i64);
            }
        }

        if t == types::pointer.type_ {
            if !val.is::<Pointer>() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI pointer").into(),
                    &[val],
                ));
            }
            let p = val.downcast::<Pointer>().value();
            loc.cast::<*mut std::ffi::c_void>()
                .write(p as *mut std::ffi::c_void);
        }

        if t == type_tag::STRUCT {
            if !val.is::<Pointer>() {
                return Err(make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::from_str(*ctx, "wrong type of argument for FFI struct").into(),
                    &[val],
                ));
            }

            let ptr = val.downcast::<Pointer>().value();
            let size: usize = (*orig).size;
            std::ptr::copy_nonoverlapping(ptr.cast::<u8>(), loc.cast::<u8>(), size);
        }
    }

    Ok(())
}

unsafe fn pack<'gc>(
    ctx: Context<'gc>,
    typ: *mut ffi_type,
    loc: *mut (),
    return_value: bool,
) -> Value<'gc> {
    unsafe {
        let t = (*typ).type_;

        if t == types::void.type_ {
            return Value::undefined();
        }

        if t == types::uint8.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as u8 as u32 as i32);
            } else {
                return Value::new(loc.cast::<u8>().read() as u32 as i32);
            }
        }

        if t == types::sint8.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as i8 as i32);
            } else {
                return Value::new(loc.cast::<i8>().read() as i32);
            }
        }

        if t == types::uint16.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as u16 as u32 as i32);
            } else {
                return Value::new(loc.cast::<u16>().read() as u32 as i32);
            }
        }

        if t == types::sint16.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as i16 as i32);
            } else {
                return Value::new(loc.cast::<i16>().read() as i32);
            }
        }

        if t == types::uint32.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as u32 as i32);
            } else {
                return Value::new(loc.cast::<u32>().read() as i32);
            }
        }

        if t == types::sint32.type_ {
            if return_value {
                return Value::new(loc.cast::<ffi_arg>().read() as i32);
            } else {
                return Value::new(loc.cast::<i32>().read() as i32);
            }
        }

        if t == types::uint64.type_ {
            return loc.cast::<u64>().read().into_value(ctx);
        }

        if t == types::sint64.type_ {
            return loc.cast::<i64>().read().into_value(ctx);
        }

        if t == types::double.type_ {
            let n = loc.cast::<f64>().read();
            return Value::new(n);
        }

        if t == types::float.type_ {
            let n = loc.cast::<f32>().read();
            return Value::new(n as f64);
        }

        if t == types::pointer.type_ {
            return Gc::new(
                *ctx,
                Pointer::new(loc.cast::<*mut std::ffi::c_void>().read()),
            )
            .into();
        }

        if t == type_tag::STRUCT {
            let arr = ByteVector::new::<false>(*ctx, (*typ).size, false);
            std::ptr::copy_nonoverlapping(
                loc.cast::<u8>(),
                arr.contents().to_mut_ptr(),
                (*typ).size,
            );
            let ptr = Pointer::new(arr.contents().to_mut_ptr() as *mut std::ffi::c_void);
            let ptr = Gc::new(*ctx, ptr);
            // keep bytevector alive as long as pointer is alive.
            ptrs(ctx).put(ctx, ptr, arr);
            return ptr.into();
        }

        todo!()
    }
}

pub(crate) extern "C-unwind" fn c_foreign_call<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
    reth: Value<'gc>,
) -> NativeReturn<'gc> {
    unsafe {
        let rands = std::slice::from_raw_parts(rands, num_rands);
        let closure = rator.downcast::<Closure>();
        let free = closure.free.downcast::<Vector>();
        let cif = free[1].get().downcast::<CIF>();
        let pointer = free[2].get().downcast::<Pointer>();

        let fits = if cif.variadic {
            num_rands >= cif.nargs as usize
        } else {
            num_rands == cif.nargs as usize
        };

        if !fits {
            let err = make_assertion_violation(
                ctx,
                Value::new(false),
                Str::from_str(
                    *ctx,
                    &format!(
                        "wrong number of arguments to foreign function, expected {}{}, got {}",
                        if cif.variadic { "at least " } else { "" },
                        cif.nargs,
                        num_rands,
                    ),
                )
                .into(),
                &[],
            );

            return ctx.return_call(reth, [err], None);
        }

        let nctx = NativeCallContext::from_raw(ctx, rator, rands.as_ptr(), num_rands, retk, reth);

        foreign_call(
            nctx,
            ctx,
            cif.into(),
            pointer.into(),
            num_rands,
            rands.as_ptr(),
        )
        .into_inner()
    }
}

pub(crate) fn init_ffi<'gc>(ctx: Context<'gc>) {
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
        define(ctx, "ssize_t", ForeignType::Int16);
        define(ctx, "size_t", ForeignType::Uint16);
    } else if size_of::<libc::ssize_t>() == 4 {
        define(ctx, "ssize_t", ForeignType::Int32);
        define(ctx, "size_t", ForeignType::Uint32);
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

    let nullp = Gc::new(*ctx, Pointer::new(std::ptr::null_mut()));

    define(ctx, "%null-pointer", nullp);

    define(ctx, "RTLD_LAZY", Value::new(libc::RTLD_LAZY as i32));
    define(ctx, "RTLD_NOW", Value::new(libc::RTLD_NOW as i32));
    define(ctx, "RTLD_GLOBAL", Value::new(libc::RTLD_GLOBAL as i32));
    define(ctx, "RTLD_LOCAL", Value::new(libc::RTLD_LOCAL as i32));
}

// TODO: Shrink this type?

/// A FFI call operation performed by the VM outside of Scheme context.
pub struct FFICallOperation<'gc> {
    pub cif: Gc<'gc, CIF<'gc>>,
    pub arguments: Vec<Value<'gc>>,
    pub args: Cell<Vec<usize>>,
    pub arg_size: usize,
    pub data: Cell<Vec<u8>>,

    pub rvalue: *mut (),
    pub target: *mut (),
}

unsafe impl<'gc> Trace for FFICallOperation<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        println!("TRACE FFI");
        let can_pin = CAN_PIN_OBJECTS.load(Ordering::Relaxed);
        for argument in self.arguments.iter_mut().filter(|arg| arg.is_cell()) {
            if can_pin {
                visitor.pin_root(argument.as_cell_raw().to_objref().unwrap());
            } else {
                unsafe { argument.trace(visitor) }
            }
        }

        if can_pin {
            visitor.pin_root(self.cif.to_object_reference());
        } else {
            visitor.trace(&mut self.cif);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> BlockingOperationWithReturn<'gc> for FFICallOperation<'gc> {
    fn prepare(
        &self,
        _ctx: Context<'gc>,
    ) -> Box<dyn FnOnce() -> crate::runtime::AfterBlockingOperationCallback> {
        let mut args = self.args.replace(Vec::new());
        let data = self.data.replace(Vec::new());
        let rvalue = self.rvalue;
        let target = self.target;
        let raw_cif = self.cif.cif.as_raw_ptr();
        let rtype = unsafe { (*raw_cif).rtype };
        Box::new(move || unsafe {
            let data = data;
            libffi::raw::ffi_call(
                raw_cif,
                Some(std::mem::transmute(target)),
                rvalue.cast(),
                args.as_mut_ptr().cast(),
            );

            Box::new(move |ctx, args| {
                let _ = data;

                let val = pack(ctx, rtype, rvalue, true);

                args.push(val)
            })
        })
    }
}
