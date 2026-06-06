//! Runtime type tags stored in GC object headers.

use crate::rsgc::object::GCObject;

/// 8 bit type codes.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct TypeCode8(pub(crate) u8);

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
    pub const THREAD: Self = Self(44);
    pub const THREAD_MUTEX: Self = Self(45);
    pub const THREAD_CONDITION: Self = Self(46);

    /// Native object that has a custom VTable and is provided by foreign code.
    pub const NATIVE_OBJECT: Self = Self(47);
    pub const CMARKS: Self = Self(48);
    pub const EPHEMERON: Self = Self(49);
    pub const POLLER: Self = Self(72);
    pub const POLL_EVENT: Self = Self(73);
    pub const SOCKET: Self = Self(74);
    pub const CODE_BLOCK: Self = Self(75);
    pub const CLOSURE2: Self = Self(76);
    pub const UNLINKED_CODEBLOCK: Self = Self(77);
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

impl From<TypeCode8> for u32 {
    fn from(tc: TypeCode8) -> Self {
        tc.0 as u32
    }
}

/// Extended 16-bit type code used when one 8-bit family has multiple variants.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct TypeCode16(pub u16);

impl From<TypeCode16> for u32 {
    fn from(tc: TypeCode16) -> Self {
        tc.0 as u32
    }
}

impl TypeCode16 {
    pub const MUTABLE_PAIR: Self = Self(TypeCode8::PAIR.0 as u16);
    pub const IMMUTABLE_PAIR: Self = Self(TypeCode8::PAIR.0 as u16 + 256);
    pub const STRING: Self = Self(TypeCode8::STRING.0 as u16);
    pub const IMMUTABLE_STRING: Self = Self(TypeCode8::STRING.0 as u16 + 256);
    pub const SHARED_STRING: Self = Self(TypeCode8::STRING.0 as u16 + 2 * 256);
    pub const MUTABLE_VECTOR: Self = Self(TypeCode8::VECTOR.0 as u16);
    pub const IMMUTABLE_VECTOR: Self = Self(TypeCode8::VECTOR.0 as u16 + 256);
    pub const MUTABLE_BYTEVECTOR: Self = Self(TypeCode8::BYTEVECTOR.0 as u16);
    pub const IMMUTABLE_BYTEVECTOR: Self = Self(TypeCode8::BYTEVECTOR.0 as u16 + 256);
    pub const BIG: Self = Self(TypeCode8::NUMBER.0 as u16 + 256);
    pub const COMPLEX: Self = Self(TypeCode8::NUMBER.0 as u16 + 2 * 256);
    pub const RATIONAL: Self = Self(TypeCode8::NUMBER.0 as u16 + 3 * 256);
    pub const UNKNOWN: Self = Self(0xFFFF);
    pub const VALUES: Self = Self(TypeCode8::VALUES.0 as u16);
    pub const CLOSURE_PROC: Self = Self(TypeCode8::CLOSURE.0 as u16);

    /// A continuation, real one. Does not represent a regular procedure.
    pub const CLOSURE_K: Self = Self(TypeCode8::CLOSURE.0 as u16 + 256);
    pub const CLOSURE_FOREIGN: Self = Self(TypeCode8::CLOSURE.0 as u16 + 2 * 256);
    /// Procedure marked as a continuation. Differs from `CLOSURE_K`
    /// in that it represents a continuation closure.
    pub const CLOSURE_CONTINUATION: Self = Self(TypeCode8::CLOSURE.0 as u16 + 3 * 256);
    pub const NATIVE_PROC: Self = Self(TypeCode8::NATIVE_PROCEDURE.0 as u16);
    pub const NATIVE_K: Self = Self(TypeCode8::NATIVE_PROCEDURE.0 as u16 + 256);
    pub const MUTABLE_HASHTABLE: Self = Self(TypeCode8::HASHTABLE.0 as u16);
    pub const IMMUTABLE_HASHTABLE: Self = Self(TypeCode8::HASHTABLE.0 as u16 + 256);

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

pub(crate) fn typ8(x: GCObject) -> TypeCode8 {
    TypeCode8::from(x.header().type_bits() as u8)
}

pub(crate) fn typ16(x: GCObject) -> TypeCode16 {
    TypeCode16::from(x.header().type_bits())
}
