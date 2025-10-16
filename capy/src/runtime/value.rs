use std::{fmt, hash::Hash, marker::PhantomData};

use rsgc::{
    Gc, Mutation, ObjectSlot, Trace,
    barrier::Write,
    mmtk::{util::Address, vm::SlotVisitor},
    object::GCObject,
};

/// A Scheme value.
///
/// Uses NaN boxing layout based on JavaScriptCore NaN boxing, we formally prove
/// the correctness of this encoding in tests/z3_value_encoding_proof.py.
///
/// Heap objects store their tags in the header provided by RSGC crate. Some objects
/// such as vectors require first word to be an additional header to store the length
/// or any other information. This header is always 64 bits in size and in case of
/// vectors can be loaded as a valid fixnum value.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Value<'gc> {
    desc: EncodedValueDescriptor,
    pd: PhantomData<&'gc ()>,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union EncodedValueDescriptor {
    pub as_i64: i64,
    pub as_u64: u64,
    pub as_f64: f64,
    pub ptr: GCObject,
}

impl<'gc> Eq for Value<'gc> {}
impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_i64() == other.raw_i64()
    }
}

impl<'gc> Hash for Value<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.is_cell() {
            unsafe {
                let obj = self.desc.ptr;
                state.write_u64(obj.hashcode());
            }
        } else {
            state.write_i64(self.raw_i64());
        }
    }
}

impl<'gc> Value<'gc> {
    pub const fn from_raw_i64(raw: i64) -> Self {
        Self {
            desc: EncodedValueDescriptor { as_i64: raw },
            pd: PhantomData,
        }
    }

    pub const fn from_raw(bits: u64) -> Self {
        Self {
            desc: EncodedValueDescriptor { as_u64: bits },
            pd: PhantomData,
        }
    }

    pub const fn raw_i64(self) -> i64 {
        unsafe { self.desc.as_i64 }
    }

    pub const fn bits(self) -> u64 {
        unsafe { self.desc.as_u64 }
    }
}

unsafe impl<'gc> Trace for Value<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        unsafe {
            if self.is_cell() && !self.is_empty() {
                visitor.visit_slot(ObjectSlot::from_address(Address::from_mut_ptr(
                    &mut self.desc.ptr,
                )));
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

// Design Assumption for Flonum Encoding:
// - All flonums are created from valid f64 bit patterns.
// - If an f64 NaN is passed to `from_f64`, it is effectively converted to,
//   or results in an encoding equivalent to, `pure_nan::PURE_NAN_BITS`.
//   The Z3 SMT proofs (see tests/z3_value_encoding_proof.py) are intended to
//   verify that any `Value` which `is_flonum()` and represents a NaN
//   will decode to `pure_nan::PURE_NAN_BITS`.

pub mod pure_nan {
    pub const PURE_NAN_BITS: u64 = 0x7ff8000000000000;
    pub const PURE_NAN: f64 = f64::from_bits(PURE_NAN_BITS);

    pub const IMPURE_NAN_BITS: u64 = 0xffff000000000000;
    pub const IMPURE_NAN: f64 = f64::from_bits(IMPURE_NAN_BITS);

    pub const VALUE_DOUBLE_ENCODE_OFFSET_BIT: u32 = 49;
    pub const VALUE_DOUBLE_ENCODE_OFFSET: u64 = 1 << VALUE_DOUBLE_ENCODE_OFFSET_BIT;
    pub const VALUE_NUMBER_TAG: u64 = 0xfffe000000000000;
}

#[cfg(target_pointer_width = "64")]
impl<'gc> Value<'gc> {
    pub const DOUBLE_ENCODE_OFFSET_BIT: u32 = pure_nan::VALUE_DOUBLE_ENCODE_OFFSET_BIT;
    pub const DOUBLE_ENCODE_OFFSET: u64 = pure_nan::VALUE_DOUBLE_ENCODE_OFFSET;

    pub const NUMBER_TAG: i64 = 0xfffe000000000000u64 as i64;
    pub const LOWEST_OF_HIGH_BITS: i64 = 1 << 49;

    pub const OTHER_TAG: i64 = 0x2;
    pub const BOOL_TAG: i64 = 0x4;
    pub const UNDEFINED_TAG: i64 = 0x8;

    pub const CHAR_TAG: i64 = 0x82; // Was 0x12, changed to avoid clash with MISC_TAG values
    pub const CHAR_MASK: i64 = Self::NUMBER_TAG | Self::CHAR_TAG;

    pub const VALUE_FALSE: i64 = Self::OTHER_TAG | Self::BOOL_TAG | 0x0;
    pub const VALUE_TRUE: i64 = Self::OTHER_TAG | Self::BOOL_TAG | 0x1;
    pub const VALUE_UNDEFINED: i64 = Self::OTHER_TAG | Self::UNDEFINED_TAG;
    pub const VALUE_NULL: i64 = Self::OTHER_TAG;

    pub const MISC_TAG: i64 = Self::OTHER_TAG | Self::BOOL_TAG | Self::UNDEFINED_TAG;

    pub const NOT_CELL_MASK: i64 = Self::NUMBER_TAG | Self::OTHER_TAG; //| Self::BOOL_TAG; // Added BOOL_TAG

    pub const VALUE_EMPTY: i64 = 0x0;
    pub const VALUE_DELETED: i64 = 0x4;

    pub const VALUE_VOID: i64 = Self::MISC_TAG | 0x10;
    pub const VALUE_UNSPECIFIED: i64 = Self::MISC_TAG | 0x20;
    pub const VALUE_EOF: i64 = Self::MISC_TAG | 0x30;
    pub const VALUE_BWP: i64 = Self::MISC_TAG | 0x40;

    pub fn from_char(c: char) -> Self {
        let raw = (c as u32 as i64) << 16 | Self::CHAR_TAG;
        Self::from_raw_i64(raw)
    }

    pub fn from_i32(i: i32) -> Self {
        let raw = (i as i64) | Self::NUMBER_TAG;
        Self::from_raw_i64(raw)
    }

    pub fn from_f64(f: f64) -> Self {
        Self::from_raw_i64((f.to_bits() as i64).wrapping_add(Self::DOUBLE_ENCODE_OFFSET as i64))
    }

    pub fn from_bool(b: bool) -> Self {
        if b {
            Self::from_raw_i64(Self::VALUE_TRUE)
        } else {
            Self::from_raw_i64(Self::VALUE_FALSE)
        }
    }

    pub fn undefined() -> Self {
        Self::from_raw_i64(Self::VALUE_UNDEFINED)
    }

    pub fn void() -> Self {
        Self::from_raw_i64(Self::VALUE_VOID)
    }

    pub fn unspecified() -> Self {
        Self::from_raw_i64(Self::VALUE_UNSPECIFIED)
    }

    pub fn eof() -> Self {
        Self::from_raw_i64(Self::VALUE_EOF)
    }

    pub fn bwp() -> Self {
        Self::from_raw_i64(Self::VALUE_BWP)
    }

    pub const fn empty() -> Self {
        Self::from_raw_i64(Self::VALUE_EMPTY)
    }

    pub fn deleted() -> Self {
        Self::from_raw_i64(Self::VALUE_DELETED)
    }

    pub fn is_void(self) -> bool {
        self.raw_i64() == Self::VALUE_VOID
    }

    pub fn is_unspecified(self) -> bool {
        self.raw_i64() == Self::VALUE_UNSPECIFIED
    }

    pub fn is_eof(self) -> bool {
        self.raw_i64() == Self::VALUE_EOF
    }

    pub fn is_bwp(self) -> bool {
        self.raw_i64() == Self::VALUE_BWP
    }

    pub fn is_deleted(self) -> bool {
        self.raw_i64() == Self::VALUE_DELETED
    }

    pub fn is_empty(self) -> bool {
        self.raw_i64() == Self::VALUE_EMPTY
    }

    pub fn is_cell(self) -> bool {
        self.raw_i64() & Self::NOT_CELL_MASK == 0 && self.raw_i64() != Self::VALUE_EMPTY
    }

    pub fn is_int32(self) -> bool {
        self.raw_i64() & Self::NUMBER_TAG == Self::NUMBER_TAG
    }

    pub fn is_inline_number(self) -> bool {
        self.raw_i64() & Self::NUMBER_TAG != 0
    }

    pub fn is_flonum(self) -> bool {
        self.is_inline_number() && !self.is_int32()
    }

    pub fn is_char(self) -> bool {
        self.raw_i64() & Self::CHAR_MASK == Self::CHAR_TAG
    }

    pub fn char(self) -> char {
        assert!(self.is_char());
        unsafe { char::from_u32_unchecked((self.raw_i64() >> 16) as i32 as u32) }
    }

    pub fn is_null(self) -> bool {
        self.raw_i64() == Self::VALUE_NULL
    }

    pub fn null() -> Self {
        Self::from_raw_i64(Self::VALUE_NULL)
    }

    pub fn is_undefined_or_null(self) -> bool {
        self.raw_i64() & !Self::UNDEFINED_TAG == Self::VALUE_NULL
    }

    pub fn as_int32(self) -> i32 {
        assert!(self.is_int32(), "not an int32: {}", self);
        self.raw_i64() as i32
    }

    pub fn int32(self) -> Option<i32> {
        if self.is_int32() {
            Some(self.as_int32())
        } else {
            None
        }
    }

    pub fn as_flonum(self) -> f64 {
        assert!(self.is_flonum());
        let bits = (self.raw_i64() as u64).wrapping_sub(Self::DOUBLE_ENCODE_OFFSET);
        f64::from_bits(bits)
    }

    pub fn flonum(self) -> Option<f64> {
        if self.is_flonum() {
            Some(self.as_flonum())
        } else {
            None
        }
    }

    pub fn as_bool(self) -> bool {
        assert!(self.is_bool());
        self.raw_i64() != Value::VALUE_FALSE
    }

    pub fn is_bool(self) -> bool {
        self.raw_i64() & Self::BOOL_TAG == Self::BOOL_TAG
    }

    pub fn as_cell_raw(self) -> GCObject {
        assert!(self.is_cell());
        unsafe { self.desc.ptr }
    }
}

#[derive(Copy, Clone)]
pub struct WeakValue<'gc> {
    desc: EncodedValueDescriptor,
    pd: PhantomData<&'gc ()>,
}

impl<'gc> From<Value<'gc>> for WeakValue<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self {
            desc: value.desc,
            pd: PhantomData,
        }
    }
}

impl<'gc> WeakValue<'gc> {
    pub fn from_value(value: Value<'gc>) -> Self {
        Self {
            desc: value.desc,
            pd: PhantomData,
        }
    }

    pub fn as_value(self) -> Value<'gc> {
        Value {
            desc: self.desc,
            pd: PhantomData,
        }
    }

    pub fn is_broken(&self) -> bool {
        self.as_value().raw_i64() == Value::bwp().raw_i64()
    }
}

unsafe impl<'gc> Trace for WeakValue<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        unsafe {
            let value = self.as_value();

            if value.is_cell() {
                let ptr = weak_processor.is_live_object(value.desc.ptr);
                if ptr.is_null() {
                    self.desc = Value::bwp().desc;
                } else {
                    self.desc.ptr = ptr;
                }
            } else {
                self.desc = Value::bwp().desc;
            }
        }
    }
}

impl<'gc> Eq for WeakValue<'gc> {}
impl<'gc> PartialEq for WeakValue<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.as_value() == other.as_value()
    }
}

impl<'gc> PartialEq<Value<'gc>> for WeakValue<'gc> {
    fn eq(&self, other: &Value<'gc>) -> bool {
        self.as_value() == *other
    }
}

/// 8 bit type codes.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct TypeCode8(u8);

impl TypeCode8 {
    pub const fn bits(self) -> u8 {
        self.0
    }

    /// Create a new type-code.
    ///
    /// # Safety
    ///
    /// Type-codes must be unique and not overlap with other type-codes.
    pub const unsafe fn new(bits: u8) -> Self {
        Self(bits)
    }

    pub const PAIR: Self = Self(0);
    pub const RECORD: Self = Self(1);
    pub const SYMBOL: Self = Self(2);
    pub const VARIABLE: Self = Self(3);
    pub const VECTOR: Self = Self(4);
    pub const WVECT: Self = Self(5);
    pub const STRING: Self = Self(6);
    /// Extends into the 16 bit range, see [`TypeCode16`].
    pub const NUMBER: Self = Self(7);
    pub const HASHTABLE: Self = Self(8);
    pub const POINTER: Self = Self(9);
    pub const FLUID: Self = Self(10);
    pub const STRINGBUF: Self = Self(11);
    pub const DYNAMICSTATE: Self = Self(12);
    pub const FRAME: Self = Self(13);
    pub const KEYWORD: Self = Self(14);
    pub const ATOMICBOX: Self = Self(15);
    pub const SYNTAX: Self = Self(16);
    pub const VALUES: Self = Self(17);
    pub const CLOSURE: Self = Self(18);
    pub const VMCONT: Self = Self(19);
    pub const BYTEVECTOR: Self = Self(20);
    pub const WEAKSET: Self = Self(21);
    pub const WEAKTABLE: Self = Self(22);
    pub const ARRAY: Self = Self(23);
    pub const BITVECTOR: Self = Self(24);
    pub const SMOB: Self = Self(25);
    pub const PORT: Self = Self(26);
    pub const RECORD_TYPE_DESCRIPTOR: Self = Self(27);
    pub const RECORD_TYPE: Self = Self(28);
    pub const RECORD_CONSTRUCTOR_DESCRIPTOR: Self = Self(29);
    pub const ANNOTATION: Self = Self(30);
    pub const TUPLE: Self = Self(31);

    pub const ENVIRONMENT: Self = Self(32);
    pub const WEAK_MAPPING: Self = Self(33);

    pub const DYNAMIC_STATE: Self = Self(34);

    pub const MODULE: Self = Self(35);
    pub const BOX: Self = Self(36);

    pub const IDENTIFIER: Self = Self(37);
    pub const LVAR: Self = Self(38);
    pub const SYNCLO: Self = Self(39);
    pub const EXPANDER: Self = Self(40);
    pub const NATIVE_PROCEDURE: Self = Self(41);
    pub const CACHE_CELL: Self = Self(42);
    pub const CIF: Self = Self(43);

    pub const UNKNOWN: Self = Self(0xFF);
}

impl From<u8> for TypeCode8 {
    fn from(x: u8) -> Self {
        Self(x)
    }
}

impl From<TypeCode8> for u8 {
    fn from(tc: TypeCode8) -> u8 {
        tc.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct TypeCode16(pub u16);

impl Into<u32> for TypeCode16 {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

impl Into<u32> for TypeCode8 {
    fn into(self) -> u32 {
        self.0 as u32
    }
}

#[allow(clippy::identity_op)]
impl TypeCode16 {
    pub const MUTABLE_PAIR: Self = Self(TypeCode8::PAIR.0 as u16);
    pub const IMMUTABLE_PAIR: Self = Self(TypeCode8::PAIR.0 as u16 + 1 * 256);

    pub const STRING: Self = Self(TypeCode8::STRING.0 as u16);
    pub const IMMUTABLE_STRING: Self = Self(TypeCode8::STRING.0 as u16 + 1 * 256);
    pub const SHARED_STRING: Self = Self(TypeCode8::STRING.0 as u16 + 2 * 256);

    pub const MUTABLE_VECTOR: Self = Self(TypeCode8::VECTOR.0 as u16);
    pub const IMMUTABLE_VECTOR: Self = Self(TypeCode8::VECTOR.0 as u16 + 1 * 256);

    pub const MUTABLE_BYTEVECTOR: Self = Self(TypeCode8::BYTEVECTOR.0 as u16);
    pub const IMMUTABLE_BYTEVECTOR: Self = Self(TypeCode8::BYTEVECTOR.0 as u16 + 1 * 256);

    pub const BIG: Self = Self(TypeCode8::NUMBER.0 as u16 + 1 * 256);
    pub const COMPLEX: Self = Self(TypeCode8::NUMBER.0 as u16 + 2 * 256);
    pub const RATIONAL: Self = Self(TypeCode8::NUMBER.0 as u16 + 3 * 256);

    pub const UNKNOWN: Self = Self(0xFFFF);

    pub const VALUES: Self = Self(TypeCode8::VALUES.0 as u16);

    pub const CLOSURE_PROC: Self = Self(TypeCode8::CLOSURE.0 as u16);
    pub const CLOSURE_K: Self = Self(TypeCode8::CLOSURE.0 as u16 + 1 * 256);
    pub const CLOSURE_FOREIGN: Self = Self(TypeCode8::CLOSURE.0 as u16 + 2 * 256);

    pub const NATIVE_PROC: Self = Self(TypeCode8::NATIVE_PROCEDURE.0 as u16);
    pub const NATIVE_K: Self = Self(TypeCode8::NATIVE_PROCEDURE.0 as u16 + 1 * 256);

    pub const MUTABLE_HASHTABLE: Self = Self(TypeCode8::HASHTABLE.0 as u16);
    pub const IMMUTABLE_HASHTABLE: Self = Self(TypeCode8::HASHTABLE.0 as u16 + 1 * 256);

    pub const fn tc8(self) -> TypeCode8 {
        TypeCode8(self.0 as u8)
    }

    pub const fn bits(self) -> u16 {
        self.0
    }
}

impl From<u16> for TypeCode16 {
    fn from(x: u16) -> Self {
        Self(x)
    }
}

impl From<TypeCode16> for u16 {
    fn from(tc: TypeCode16) -> u16 {
        tc.0
    }
}

fn typ8(x: GCObject) -> TypeCode8 {
    unsafe {
        let hdr = x.to_address().as_ref::<ScmHeader>();
        TypeCode8::from(hdr.type_bits() as u8)
    }
}

fn typ16(x: GCObject) -> TypeCode16 {
    unsafe {
        let hdr = x.to_address().as_ref::<ScmHeader>();
        TypeCode16::from(hdr.type_bits())
    }
}

impl<'gc> Value<'gc> {
    pub fn new(x: impl Into<Self>) -> Self {
        x.into()
    }

    pub fn header(&self) -> &'gc ScmHeader {
        unsafe {
            assert!(self.is_cell(), "Value must be a cell to have a header");
            let obj = self.as_cell_raw();
            obj.to_address().as_ref::<ScmHeader>()
        }
    }

    pub fn is_immediate(&self) -> bool {
        !self.is_cell()
    }

    pub fn has_typ8(&self, tc: TypeCode8) -> bool {
        !self.is_immediate() && typ8(self.as_cell_raw()) == tc
    }

    pub fn has_typ16(&self, tc: TypeCode16) -> bool {
        !self.is_immediate() && typ16(self.as_cell_raw()) == tc
    }

    pub fn typ8(&self) -> TypeCode8 {
        if self.is_immediate() {
            TypeCode8::UNKNOWN
        } else {
            typ8(self.as_cell_raw())
        }
    }

    pub fn typ16(&self) -> TypeCode16 {
        if self.is_immediate() {
            TypeCode16::UNKNOWN
        } else {
            typ16(self.as_cell_raw())
        }
    }

    pub fn from_gc<T: Tagged>(gc: Gc<'gc, T>) -> Self {
        Self {
            desc: EncodedValueDescriptor { ptr: gc.as_gcobj() },
            pd: PhantomData,
        }
    }

    pub fn is<T: Tagged>(&self) -> bool {
        if T::ONLY_TC16 {
            return self.is_cell() && T::TC16.iter().any(|&tc| self.has_typ16(tc));
        }

        self.is_cell() && self.has_typ8(T::TC8)
    }

    pub fn downcast<T: Tagged>(self) -> Gc<'gc, T> {
        assert!(
            self.is::<T>(),
            "Value is not of type {}: {}",
            std::any::type_name::<T>(),
            self
        );
        unsafe { Gc::from_gcobj(self.as_cell_raw()) }
    }

    pub fn try_as<T: Tagged>(self) -> Option<Gc<'gc, T>> {
        if self.is::<T>() {
            Some(self.downcast())
        } else {
            None
        }
    }
}

impl<'gc> std::ops::Not for Value<'gc> {
    type Output = bool;

    fn not(self) -> Self::Output {
        self.raw_i64() == Self::VALUE_FALSE
    }
}

pub mod conversions;
pub mod environment;
pub mod eq;
pub mod hash;
pub mod header;
pub mod list;
pub mod number;
pub mod port;
pub mod proc;
pub mod string;
pub mod symbols;
pub mod vector;
pub mod weak_set;
pub mod weak_table;

pub use conversions::*;
pub use hash::*;
pub use header::*;
pub use list::*;
pub use number::*;

pub use proc::*;
pub use string::*;
pub use symbols::*;
pub use vector::*;
pub use weak_set::*;
pub use weak_table::*;

use crate::{
    frontend::reader::Annotation,
    runtime::{
        Context,
        modules::Module,
        vm::syntax::{Syntax, SyntaxTransformer},
    },
};

impl<'gc> fmt::Pointer for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({:x})", self.raw_i64())
    }
}

impl<'gc, T: Tagged> From<Gc<'gc, T>> for Value<'gc> {
    fn from(gc: Gc<'gc, T>) -> Self {
        Value::from_gc(gc)
    }
}

unsafe impl<'gc> bytemuck::Zeroable for Value<'gc> {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalValue(u64);

impl GlobalValue {
    pub const fn new<'gc>(value: Value<'gc>) -> Self {
        Self(value.bits())
    }

    pub fn get<'gc>(self, _ctx: &Mutation<'gc>) -> Value<'gc> {
        Value::from_raw_i64(self.0 as i64)
    }
}

unsafe impl bytemuck::Zeroable for GlobalValue {}
unsafe impl bytemuck::Pod for GlobalValue {}

unsafe impl Trace for GlobalValue {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        let value = Value::from_raw_i64(self.0 as i64);
        if value.is_cell() {
            visitor.visit_slot(ObjectSlot::from_address(Address::from_mut_ptr(&mut self.0)));
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

impl<'gc> std::fmt::Display for Value<'gc> {
    #[allow(clippy::collapsible_else_if)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(number) = self.number() {
            write!(f, "{number}")?;
            return Ok(());
        }
        if self.is_immediate() {
            if self.is_null() {
                write!(f, "'()")
            } else if self.is_void() {
                write!(f, "#<void>")
            } else if *self == Value::undefined() {
                write!(f, "#<undefined>")
            } else if self.is_unspecified() {
                write!(f, "#<unspecified>")
            } else if self.is_eof() {
                write!(f, "#<eof-object>")
            } else if self.is_bwp() {
                write!(f, "#<bwp>")
            } else if self.is_char() {
                write!(f, "'{}'", self.char())
            } else if self.is_bool() {
                write!(f, "{}", self.as_bool())
            } else if self.is_int32() {
                write!(f, "{}", self.as_int32())
            } else if self.is_flonum() {
                write!(f, "{}", self.as_flonum())
            } else {
                write!(f, "{:x}", self.raw_i64())
            }
        } else {
            if self.is::<Module>() {
                write!(f, "#<module {}>", self.downcast::<Module>().name.get())
            } else if self.is::<Pair>() {
                write!(f, "(")?;
                format_list_contents(f, *self)?;
                write!(f, ")")
            } else if self.is::<Str>() {
                write!(f, "\"{}\"", self.downcast::<Str>())
            } else if self.is::<Symbol>() {
                write!(f, "{}", self.downcast::<Symbol>())
            } else if self.is::<Vector>() {
                let v = self.downcast::<Vector>();

                write!(f, "#(")?;

                for i in 0..v.len() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v[i].get())?;
                }
                write!(f, ")")
            } else if self.is::<Tuple>() {
                let v = self.downcast::<Tuple>();

                write!(f, "#t(")?;

                for i in 0..v.len() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v[i].get())?;
                }
                write!(f, ")")
            } else if self.is::<ByteVector>() {
                let bv = self.downcast::<ByteVector>();

                write!(f, "#u8(")?;

                for i in 0..bv.len() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", bv[i])?;
                }
                write!(f, ")")
            } else if self.is::<Annotation>() {
                let an = self.downcast::<Annotation>();

                write!(
                    f,
                    "{} @ {}:{}:{}",
                    an.expression,
                    an.source,
                    an.start_point.0 + 1,
                    an.start_point.1
                )
            } else if self.is::<Closure>() {
                let clo = self.downcast::<Closure>();
                if clo.is_continuation() {
                    write!(f, "#<continuation {:p}>", clo)
                } else {
                    write!(f, "#<closure {:p}>", clo)
                }
            } else if self.is::<Syntax>() {
                let syn = self.downcast::<Syntax>();
                write!(
                    f,
                    "#<syntax {} at {}, wrap={}>",
                    syn.expr(),
                    syn.source(),
                    syn.wrap()
                )
            } else if self.is::<SyntaxTransformer>() {
                let st = self.downcast::<SyntaxTransformer>();
                write!(f, "#<syntax-transformer {}>", st.name())
            } else {
                write!(f, "{:p} with tc={}", self, self.typ16().bits())
            }
        }
    }
}

impl<'gc> fmt::Debug for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

fn format_list_contents<'gc>(f: &mut fmt::Formatter<'_>, list: Value<'gc>) -> fmt::Result {
    if !list.is_pair() {
        return Ok(());
    }

    let car = list.car();
    let cdr = list.cdr();

    write!(f, "{car}")?;

    if cdr.is_pair() {
        write!(f, " ")?;
        format_list_contents(f, cdr)
    } else if cdr.is_null() {
        Ok(())
    } else {
        write!(f, " . {cdr}")
    }
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct ValueEqual<'gc>(pub Value<'gc>);

impl<'gc> PartialEq for ValueEqual<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.r5rs_equal(other.0)
    }
}

impl<'gc> Eq for ValueEqual<'gc> {}

impl<'gc> std::hash::Hash for ValueEqual<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let val = self.0;
        if val.is_pair() {
            ValueEqual(val.car()).hash(state);
            ValueEqual(val.cdr()).hash(state);
        } else if val.is::<Vector>() {
            for val in val.downcast::<Vector>().iter() {
                ValueEqual(val.get()).hash(state);
            }
        } else if val.is::<Str>() {
            let v = val.downcast::<Str>();

            v.hash(state);
        } else if val.is::<ByteVector>() {
            let v = val.downcast::<ByteVector>();

            v.hash(state);
        } else if val.is::<Boxed>() {
            ValueEqual(val.downcast::<Boxed>().val).hash(state);
        } else {
            val.hash(state);
        }
    }
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct Boxed<'gc> {
    pub header: ScmHeader,
    pub val: Value<'gc>,
}

impl<'gc> Boxed<'gc> {
    pub fn new(ctx: Context<'gc>, val: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::BOX.bits() as _),
                val,
            },
        )
    }
}

unsafe impl<'gc> Tagged for Boxed<'gc> {
    const TC8: TypeCode8 = TypeCode8::BOX;
    const TYPE_NAME: &'static str = "box";
}
