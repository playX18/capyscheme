//! FASL encoding/decoding format
//!
//! FASL is an ancient Lisp jargon for "Fast Loading". We implement a simple format which encodes Scheme
//! values as a sequence of bytes. The format is portable and allows for efficient serialization and deserialization of Scheme values.

use rsgc::Gc;

use super::value::HashTable;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FaslTag {
    Eof = 0,
    Lookup = 1,
    Fixnum32 = 2,
    Plist = 3,
    Dlist = 4,
    Vector = 5,
    Rational = 6,
    Complex = 7,
    Flonum = 8,
    Bignum = 9,
    Bvector = 10,
    Char = 11,
    Nil = 12,
    T = 13,
    F = 14,
    Symbol = 15,
    String = 16,
    UninternedSymbol = 17,
    Fixnum64 = 18,
    Int0 = 19,
    Int1 = 20,
    Int2 = 21,
    Int3 = 22,
}

pub struct FaslWriter<'gc> {
    pub lites: Gc<'gc, HashTable<'gc>>,
}
