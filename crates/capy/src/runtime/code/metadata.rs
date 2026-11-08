//! Metadata shared by the object model and the compilers.

use std::fmt::{self, Display, Formatter};

/// The check that guards a deopt exit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GuardKind {
    /// A fixnum check (`CheckInt32`, or the fixnum test in a bounds check).
    Fixnum,
    /// A flonum check.
    Flonum,
    /// A character check.
    Char,
    /// A class check.
    Class,
    /// A code-block check.
    Code,
    /// The post-call invalidation check.
    Invalidated,
    /// An int32 add/sub/multiply overflow.
    IntOverflow,
    /// An int64 value that leaves the fixnum range when it is boxed.
    Int64Range,
    /// A `BIGINT` guard: the wrong class, more than one limb, or a magnitude
    /// that does not fit an i64.
    BigInt,
    /// A rational component or operand guard: not a `RATIONAL`, a component
    /// that is not an `i32` fixnum, or an integer operand that is not a
    /// fixnum.
    Rational,
    /// A quotient/remainder/modulo zero-divisor or overflow.
    Division,
    /// A vector length or index range check.
    Range,
    /// A `get_slot`/`put_slot` cache miss.
    Slot,
    /// A primitive or runtime-helper error.
    Call,
    /// A multiple-value return point that exits.
    Return,
    /// An explicit `Terminator::Deopt` in the IR.
    Explicit,
}

impl GuardKind {
    pub fn as_str(self) -> &'static str {
        match self {
            GuardKind::Fixnum => "fixnum",
            GuardKind::Flonum => "flonum",
            GuardKind::Char => "char",
            GuardKind::Class => "class",
            GuardKind::Code => "code",
            GuardKind::Invalidated => "invalidated",
            GuardKind::IntOverflow => "int-overflow",
            GuardKind::Int64Range => "int64-range",
            GuardKind::BigInt => "bigint",
            GuardKind::Rational => "rational",
            GuardKind::Division => "division",
            GuardKind::Range => "range",
            GuardKind::Slot => "slot",
            GuardKind::Call => "call",
            GuardKind::Return => "return",
            GuardKind::Explicit => "explicit",
        }
    }
}

impl Display for GuardKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
