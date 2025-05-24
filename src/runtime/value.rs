// Encoding invariants for Value are formally checked in tests/z3_value_encoding_proof.py using the Z3 SMT solver.
// This script proves that the tag bits for each Value variant are disjoint and that decoding is always possible.
// It also proves that the encodings do not break each other.
// To rerun the proof, see tests/z3_value_encoding_proof.py.

use std::{convert::Infallible, hash::Hash, marker::PhantomData, sync::atomic::AtomicI64};

use rsgc::{
    Gc, GcWeak, RSGC, Trace,
    barrier::Write,

    define_namespace,
    vmkit::{
        mm::MemoryManager,
        mmtk::util::ObjectReference,
        object_model::header::HeapObjectHeader,
        prelude::{ObjectTracer, VMKitObject},
    },
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
pub struct Value<'gc> {
    desc: EncodedValueDescriptor,
    pd: PhantomData<&'gc ()>,
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_i64() == other.raw_i64()
    }
}

impl<'gc> Eq for Value<'gc> {}

impl<'gc> Hash for Value<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.is_cell() {
            // heap objects need to be hashed differently.
            // VMKit will mark them as hashed and if GC decides
            // to move the object, hashcode will remain the same
            // and be stored behind object header.
            unsafe {
                let vmkit_obj = VMKitObject::from(self.desc.ptr);
                state.write_usize(vmkit_obj.hashcode::<RSGC>());
            }
        } else {
            self.raw_i64().hash(state);
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

    pub fn raw_i64(self) -> i64 {
        unsafe { self.desc.as_i64 }
    }

    pub fn is<T: Tagged>(self) -> bool {
        if T::ONLY_TC16 {
            return self.is_cell() && T::TC16.iter().any(|&t| self.has_typ16(t));
        }
        self.is_cell() && self.has_typ8(T::TC8)
    }

    pub fn downcast<T: Tagged>(self) -> Gc<'gc, T> {
        assert!(self.is::<T>());
        unsafe { Gc::from_mmtk_object(self.desc.ptr) }
    }
}

impl<T: Tagged> From<Gc<'_, T>> for Value<'_> {
    fn from(gc: Gc<'_, T>) -> Self {
        Self {
            desc: EncodedValueDescriptor {
                ptr: gc.as_mmtk_object(),
            },
            pd: PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
pub union EncodedValueDescriptor {
    pub as_i64: i64,
    pub as_u64: u64,
    pub as_f64: f64,
    pub ptr: ObjectReference,
}

unsafe impl<'gc> Trace for Value<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        if self.is_cell() && !self.is_empty() {
            visitor.trace_object(unsafe { self.desc.ptr });
        }
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        if self.is_cell() && !self.is_empty() {
            let mut weak: GcWeak<'static, ()> = unsafe { std::mem::transmute(self.desc.ptr) };
            weak_processor.process_weak(&mut weak);
            if weak.is_broken() {
                *self = Value::from_raw_i64(Value::VALUE_BWP);
            }
        }
    }
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
        assert!(self.is_int32());
        self.raw_i64() as i32
    }

    pub fn as_flonum(self) -> f64 {
        assert!(self.is_flonum());
        let bits = (self.raw_i64() as u64).wrapping_sub(Self::DOUBLE_ENCODE_OFFSET);
        f64::from_bits(bits)
    }

    pub fn as_bool(self) -> bool {
        assert!(self.is_bool());
        self.raw_i64() != Value::VALUE_FALSE
    }

    pub fn is_bool(self) -> bool {
        self.raw_i64() & Self::BOOL_TAG == Self::BOOL_TAG
    }
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

impl From<i32> for Value<'_> {
    fn from(i: i32) -> Self {
        Value::from_i32(i)
    }
}

impl From<f64> for Value<'_> {
    fn from(f: f64) -> Self {
        Value::from_f64(f)
    }
}

impl From<bool> for Value<'_> {
    fn from(b: bool) -> Self {
        Value::from_bool(b)
    }
}

impl From<char> for Value<'_> {
    fn from(c: char) -> Self {
        Value::from_char(c)
    }
}

impl<'gc> Value<'gc> {
    pub fn new(x: impl Into<Self>) -> Self {
        x.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encoding() {
        let c = Value::from_char('📦');
        assert!(!c.is_cell());
        assert!(!c.is_inline_number());
        assert!(!c.is_int32());
        assert!(!c.is_flonum());
        assert!(!c.is_undefined_or_null());

        let immediates = [
            Value::from_i32(42),
            Value::from_f64(42.424224),
            Value::from_bool(true),
            Value::from_bool(false),
            Value::void(),
            Value::unspecified(),
            Value::eof(),
            Value::bwp(),
            Value::from_char('📦'),
        ];

        for i in immediates {
            assert!(!i.is_cell());
        }

        assert!(!Value::void().is_undefined_or_null());
        assert!(!Value::unspecified().is_undefined_or_null());
        assert!(!Value::eof().is_undefined_or_null());
        assert!(!Value::bwp().is_undefined_or_null());
    }
}

define_namespace!(pub ValuesNamespace);

/// 8 bit type codes.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
    pub const PROGRAM: Self = Self(18);
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

    pub fn tc8(self) -> TypeCode8 {
        TypeCode8(self.0 as u8)
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



fn typ8(x: ObjectReference) -> TypeCode8 {
    unsafe {
        let header = x
            .to_header::<MemoryManager<RSGC>>()
            .as_ref::<HeapObjectHeader<RSGC>>();
        let meta = header.metadata();
        TypeCode8::try_from(meta.user_header().bits as u8).unwrap_or(TypeCode8::UNKNOWN)
    }
}

fn typ16(x: ObjectReference) -> TypeCode16 {
    unsafe {
        let header = x
            .to_header::<MemoryManager<RSGC>>()
            .as_ref::<HeapObjectHeader<RSGC>>();
        let meta = header.metadata();
        TypeCode16::try_from(meta.user_header().bits as u16).unwrap_or(TypeCode16::UNKNOWN)
    }
}

impl<'gc> Value<'gc> {
    pub fn is_immediate(self) -> bool {
        !self.is_cell()
    }

    pub fn has_typ8(self, typ: TypeCode8) -> bool {
        !self.is_immediate() && typ8(unsafe { self.desc.ptr }) == typ
    }

    pub fn has_typ16(self, typ: TypeCode16) -> bool {
        !self.is_immediate() && typ16(unsafe { self.desc.ptr }) == typ
    }

    pub fn typ16(self) -> TypeCode16 {
        if self.is_immediate() {
            return TypeCode16::UNKNOWN;
        }
        typ16(unsafe { self.desc.ptr })
    }

    pub fn write(self: &Write<Self>, value: Value<'gc>) {
        // SAFETY: `Value` is repr(transparent) so is `Write`.
        unsafe {
            let atomic: &'gc AtomicI64 = std::mem::transmute(self);
            atomic.store(value.raw_i64(), std::sync::atomic::Ordering::SeqCst);
        }
    }

    pub fn not(self) -> bool {
        self.raw_i64() == Value::VALUE_FALSE
    }

}

#[macro_use]
pub mod lists;
#[macro_use]
pub mod vectors;
pub mod proc;
pub mod number;
pub mod strings;
pub mod structs;
pub mod symbols;
pub mod variable;
pub mod conversions;
pub mod weak_set;
pub mod values;
pub mod hash;
pub mod equiv;
pub mod port;
pub mod map;
pub mod eq;
pub mod tuple;

pub use port::*;
pub use number::*;
pub use hash::*;
pub use conversions::*;
pub use lists::*;
pub use proc::*;
pub use strings::*;
pub use structs::*;
pub use symbols::*;
pub use variable::*;
pub use vectors::*;
pub use weak_set::*;
pub use map::*;
pub use tuple::*;

impl<'gc> std::fmt::Pointer for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value({:x})", self.raw_i64())
    }
}
