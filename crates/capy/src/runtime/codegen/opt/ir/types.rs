//! The static type lattice of IR values.

use cranelift_entity::entity_impl;

use crate::heap::object::builtin_class_ids;

/// A code block of the unit, named by its index in [`Func::codes`](super::func::Func).
/// The IR never holds the code block itself, since it is a movable heap object.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodeRef(u32);
entity_impl!(CodeRef, "code");

/// Where a [`CodeRef`]'s code block comes from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeSource<'gc> {
    /// The procedure being compiled.
    Root,
    /// Constant `index` of the root's bytecode pool, which holds a code block
    /// in the non-moving buffer.
    Pool(u32),
    /// A code block held as a rooted value from the build to the install
    /// An inlined callee is named this way, and the
    /// unit's constant table keeps it alive.
    Heap(crate::runtime::objectmodel::Value<'gc>),
}

/// A heap class. The payload is the builtin class id the object header
/// carries, so a `CheckClass` tests the header directly.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassRef(u32);
entity_impl!(ClassRef, "class");

impl ClassRef {
    pub const PAIR: Self = Self(builtin_class_ids::PAIR);
    pub const VARIABLE: Self = Self(builtin_class_ids::VARIABLE);
    pub const CLOSURE: Self = Self(builtin_class_ids::CLOSURE);
    pub const ENV_RECORD: Self = Self(builtin_class_ids::ENV_RECORD);
    pub const VECTOR: Self = Self(builtin_class_ids::VECTOR);
    pub const SYMBOL: Self = Self(builtin_class_ids::SYMBOL);

    pub const STRING: Self = Self(builtin_class_ids::STRING);
    pub const TUPLE: Self = Self(builtin_class_ids::TUPLE);
    /// The two bytevector classes. The bytevector primitives accept either.
    pub const MUTABLE_BYTEVECTOR: Self = Self(builtin_class_ids::MUTABLE_BYTEVECTOR);
    pub const IMMUTABLE_BYTEVECTOR: Self = Self(builtin_class_ids::IMMUTABLE_BYTEVECTOR);
    pub const BIGINT: Self = Self(builtin_class_ids::BIGINT);
    /// A record type descriptor object.
    pub const RTD: Self = Self(builtin_class_ids::RTD);
    /// The exact rational class, id 23. Its payload is two inline `Number`s:
    /// the numerator at payload offset 0 and the denominator at offset 16
    /// (see `value/number/mod.rs` `layout`).
    pub const RATIONAL: Self = Self(builtin_class_ids::RATIONAL);

    /// The builtin class id this class names.
    pub const fn class_id(self) -> u32 {
        self.0
    }
}

/// The type of an SSA value.
///
/// `Bottom` (no value, as in unreachable code) lies below every type and
/// `Any` above every type. `Closure(Some(code))` lies below `Closure(None)`.
/// The other kinds are incomparable with each other.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Bottom,
    Int32,
    /// An unboxed 64-bit integer. Unlike `Int32` it is not a Scheme value:
    /// it only exists inside compiled code, and a `box.int64` decides whether
    /// the result is a fixnum or (in a later milestone) a bignum.
    Int64,
    /// An unboxed 128-bit integer, held as two 64-bit limbs (low, high). It
    /// only exists inside compiled code: the wide rational plan computes
    /// cross-products in it and boxes a result with `box.int128`. An `Int128`
    /// value is lowered to a fixed pair of LIR registers; it is never a block
    /// parameter or a frame value.
    Int128,
    Double,
    Bool,
    Null,
    Pair,
    /// A closure, optionally known to run the given code block.
    Closure(Option<CodeRef>),
    Vector,
    Cell,
    /// A heap object of a known class.
    Heap(ClassRef),
    /// An exact rational number (`RATIONAL`, class 23). It is a Scheme value
    /// like the other heap kinds; the type only marks that the object was
    /// built by `alloc.rational` and has the two-`Number` payload.
    Rational,
    Any,
}

impl Type {
    pub fn is_subtype_of(self, other: Type) -> bool {
        match (self, other) {
            (a, b) if a == b => true,
            (Type::Bottom, _) | (_, Type::Any) => true,
            (Type::Closure(Some(_)), Type::Closure(None)) => true,
            _ => false,
        }
    }

    /// The least type above both `self` and `other`.
    pub fn join(self, other: Type) -> Type {
        if self.is_subtype_of(other) {
            other
        } else if other.is_subtype_of(self) {
            self
        } else if let (Type::Closure(_), Type::Closure(_)) = (self, other) {
            Type::Closure(None)
        } else {
            Type::Any
        }
    }

    /// The greatest type below both `self` and `other`.
    pub fn meet(self, other: Type) -> Type {
        if self.is_subtype_of(other) {
            self
        } else if other.is_subtype_of(self) {
            other
        } else {
            Type::Bottom
        }
    }
}
