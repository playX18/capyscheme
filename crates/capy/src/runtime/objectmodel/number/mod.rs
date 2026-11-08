//! Scheme numeric tower values.
//!
//! This module represents fixnums, flonums, bignums, rationals, and complex
//! numbers. Big integers and number formatting helpers live in the `bigint`
//! submodule.

mod arith;
mod bigint;
mod coerce;
mod compare;
mod complex_ops;
mod construct;
mod bitwise;
mod predicates;
mod rational;
mod rounding;
mod transcendental;
mod convert;
mod display;
mod exact;
pub mod layout;
mod parse;
mod print;
pub use bigint::*;
pub use convert::*;
pub use exact::*;
pub use parse::*;

use core::f64;
use std::{
    fmt,
    hash::{Hash, Hasher},
    iter::Peekable,
};

use crate::heap::{Gc, Trace, collection::Visitor};

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::runtime::{MutationContext, objectmodel::ConversionError};

use crate::runtime::objectmodel::{ClassTagged, FromValue, IntoValue, Value};

const IEXPT_2N53: i64 = 0x20000000000000;
const IEXPT_2N52: i64 = 0x10000000000000;

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
/// Heap-allocated complex number.
pub struct Complex<'gc> {
    pub real: Number<'gc>,
    pub imag: Number<'gc>,
}

fn complex_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::COMPLEX).expect("builtin class id is nonzero"),
    )
}

impl<'gc> Complex<'gc> {
    /// Allocates a complex number from real and imaginary parts.
    pub fn new(ctx: MutationContext<'gc>, real: Number<'gc>, imag: Number<'gc>) -> Gc<'gc, Self> {
        let complex = Complex { real, imag };
        ctx.allocate_with(complex, complex_header_word())
    }
}
// SAFETY: `gc` for `Complex` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Complex<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::COMPLEX];
    const TYPE_NAME: &'static str = "complex";
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
/// Heap-allocated rational number.
pub struct Rational<'gc> {
    pub numerator: Number<'gc>,
    pub denominator: Number<'gc>,
}

fn rational_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::RATIONAL).expect("builtin class id is nonzero"),
    )
}

impl<'gc> Rational<'gc> {
    /// Allocates a rational number from numerator and denominator.
    pub fn new(
        ctx: MutationContext<'gc>,
        numerator: Number<'gc>,
        denominator: Number<'gc>,
    ) -> Gc<'gc, Self> {
        let rational = Rational {
            numerator,
            denominator,
        };
        ctx.allocate_with(rational, rational_header_word())
    }
}

// SAFETY: `gc` for `Rational` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Rational<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::RATIONAL];
    const TYPE_NAME: &'static str = "rational";
}

/// A number type which represents one of the types from numerical tower of Scheme:
/// - `integer`: represented by `Fixnum` or `BigInt`, `fixnum` is 32-bit integer
///   used for optimization of value sizes in VM and is a VM detail that might change in the future.
///   while bigint provides virtually unlimited precision for integer values.
/// - `flonum`: double precision IEEE 754 floating point number.
/// - `rational`: a pair of integers representing a rational number.
/// - `complex`: a pair of numbers representing a complex number.
///
/// The representation is part of the runtime ABI: the optimizing JIT loads
/// rational and complex components with field loads, so the layout is pinned
/// by `repr(C, u64)` and checked by the [`layout`] module's compile-time
/// assertions. Do not reorder the variants or add one before these five.
#[derive(Clone, Copy)]
#[repr(C, u64)]
pub enum Number<'gc> {
    Fixnum(i32),
    Flonum(f64),
    BigInt(Gc<'gc, BigInt<'gc>>),
    Rational(Gc<'gc, Rational<'gc>>),
    Complex(Gc<'gc, Complex<'gc>>),
}

/// Stable in-memory layout of [`Number`], and the contract the optimizing JIT
/// relies on when it loads a rational or complex component directly.
///
/// `Number` is a `#[repr(C, u64)]` enum: a `u64` discriminant (the tag) at
/// offset 0, then the active variant's payload in a C union after it. Every
/// payload is at most 8 bytes and needs 8-byte alignment, so the payload
/// always starts at offset 8 and a `Number` is 16 bytes with 8-byte alignment
/// on 64-bit targets. The assertions at the bottom of this module enforce all
/// of this at compile time; if the toolchain layout ever stops matching, the
/// build fails instead of the JIT reading the wrong word.
///
/// Tags (variant declaration order):
///
/// | tag | variant | payload |
/// |-----|---------|---------|
/// | 0 | `Fixnum` | `i32` in the low 4 bytes; sign-extend it |
/// | 1 | `Flonum` | `f64` bits: read as `u64`, then `f64::from_bits` |
/// | 2 | `BigInt` | `Gc` pointer (non-null, 8 bytes) |

// SAFETY: `gc` for `Number` upholds all trait invariants
unsafe impl<'gc> Trace for Number<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, tracer: &mut Visitor) {
        match self {
            Number::Fixnum(_) => {}
            Number::Flonum(_) => {}
            Number::BigInt(b) => tracer.trace(b),
            Number::Rational(r) => tracer.trace(r),
            Number::Complex(c) => tracer.trace(c),
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> IntoValue<'gc> for Number<'gc> {
    fn into_value(self, _ctx: MutationContext<'gc>) -> Value<'gc> {
        match self {
            Number::Fixnum(i) => Value::new(i),
            Number::Flonum(f) => Value::new(f),
            Number::BigInt(b) => b
                .try_as_i64()
                .and_then(|v| i32::try_from(v).ok())
                .map(Value::new)
                .unwrap_or_else(|| Value::new(b)),
            Number::Rational(r) => Value::new(r),
            Number::Complex(c) => Value::new(c),
        }
    }
}

impl<'gc> FromValue<'gc> for Number<'gc> {
    fn try_from_value(
        _ctx: MutationContext<'gc>,
        value: Value<'gc>,
    ) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            Ok(Number::Fixnum(value.as_int32()))
        } else if value.is::<BigInt>() {
            Ok(Number::BigInt(value.downcast::<BigInt>()))
        } else if value.is::<Rational>() {
            Ok(Number::Rational(value.downcast::<Rational>()))
        } else if value.is::<Complex>() {
            Ok(Number::Complex(value.downcast::<Complex>()))
        } else if value.is_flonum() {
            Ok(Number::Flonum(value.as_flonum()))
        } else {
            Err(ConversionError::TypeMismatch {
                pos: 0,
                expected: "number",
                found: value,
            })
        }
    }
}







impl<'gc> Value<'gc> {
    pub fn is_number(self) -> bool {
        self.is_inline_number()
            || self.is::<BigInt>()
            || self.is::<Rational>()
            || self.is::<Complex>()
    }

    pub fn number(self) -> Option<Number<'gc>> {
        if self.is_inline_number() {
            if self.is_int32() {
                return Some(Number::Fixnum(self.as_int32()));
            } else {
                return Some(Number::Flonum(self.as_flonum()));
            }
        }

        if self.is::<BigInt>() {
            return Some(Number::BigInt(self.downcast()));
        }

        if self.is::<Rational>() {
            return Some(Number::Rational(self.downcast()));
        }

        if self.is::<Complex>() {
            return Some(Number::Complex(self.downcast()));
        }

        None
    }
}

impl<'gc> Hash for Number<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Fixnum(n) => n.hash(state),
            Number::Flonum(n) => n.to_bits().hash(state),
            Number::BigInt(b) => b.hash(state),
            Number::Rational(r) => {
                r.numerator.hash(state);
                r.denominator.hash(state);
            }
            Number::Complex(c) => {
                c.real.hash(state);
                c.imag.hash(state);
            }
        }
    }
}




const fn nbits32(mut x: u32) -> u32 {
    x = x - ((x >> 1) & 0x55555555);
    let t = (x >> 2) & 0x33333333;
    x = (x & 0x33333333) + t;
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    // The top byte sums the four byte counts; the product wraps by design.
    x = x.wrapping_mul(0x01010101);
    x >> 24
}
