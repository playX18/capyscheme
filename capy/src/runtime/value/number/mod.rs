#![allow(unused_variables)]

//! Scheme numeric tower values.
//!
//! This module represents fixnums, flonums, bignums, rationals, and complex
//! numbers. Big integers and number formatting helpers live in the `bigint`
//! submodule.

mod bigint;
pub use bigint::*;

use core::f64;
use std::{
    fmt,
    hash::{Hash, Hasher},
    iter::Peekable,
};

use crate::rsgc::{Gc, Trace, collection::Visitor};

use crate::rsgc::object::{
    ClassId, builtin_class_ids, class_header_word_with_primitive_layout_tag, primitive_layout_tags,
};
use crate::runtime::{Context, value::ConversionError};

use crate::runtime::value::{ClassTagged, FromValue, IntoValue, Value};

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
    class_header_word_with_primitive_layout_tag(
        ClassId::new(builtin_class_ids::COMPLEX).unwrap(),
        primitive_layout_tags::NUMBER,
    )
}

impl<'gc> Complex<'gc> {
    /// Allocates a complex number from real and imaginary parts.
    pub fn new(ctx: Context<'gc>, real: Number<'gc>, imag: Number<'gc>) -> Gc<'gc, Self> {
        let complex = Complex { real, imag };
        Gc::new_with_header_word(*ctx, complex, complex_header_word())
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
    class_header_word_with_primitive_layout_tag(
        ClassId::new(builtin_class_ids::RATIONAL).unwrap(),
        primitive_layout_tags::NUMBER,
    )
}

impl<'gc> Rational<'gc> {
    /// Allocates a rational number from numerator and denominator.
    pub fn new(
        ctx: Context<'gc>,
        numerator: Number<'gc>,
        denominator: Number<'gc>,
    ) -> Gc<'gc, Self> {
        let rational = Rational {
            numerator,
            denominator,
        };
        Gc::new_with_header_word(*ctx, rational, rational_header_word())
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
#[derive(Clone, Copy)]
pub enum Number<'gc> {
    Fixnum(i32),
    Flonum(f64),
    BigInt(Gc<'gc, BigInt<'gc>>),
    Rational(Gc<'gc, Rational<'gc>>),
    Complex(Gc<'gc, Complex<'gc>>),
}

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
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> IntoValue<'gc> for Number<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
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
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
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

impl<'gc> Number<'gc> {
    pub fn exact_integer_to_i32(self) -> Option<i32> {
        if let Number::Fixnum(i) = self {
            Some(i)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| v as i32)
        } else {
            None
        }
    }

    pub fn exact_integer_to_u32(self) -> Option<u32> {
        if let Number::Fixnum(i) = self {
            Some(i as u32)
        } else if let Number::BigInt(b) = self {
            b.try_as_u64()
                .filter(|&v| v <= u32::MAX as u64)
                .map(|v| v as u32)
        } else {
            None
        }
    }

    pub fn exact_integer_to_i64(self) -> Option<i64> {
        if let Number::Fixnum(i) = self {
            Some(i as i64)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64()
        } else {
            None
        }
    }

    pub fn exact_integer_to_u64(self) -> Option<u64> {
        if let Number::Fixnum(i) = self {
            Some(i as u64)
        } else if let Number::BigInt(b) = self {
            b.try_as_u64()
        } else {
            None
        }
    }

    pub fn exact_integer_to_usize(self) -> Option<usize> {
        if let Number::Fixnum(i) = self {
            Some(i as usize)
        } else if let Number::BigInt(b) = self {
            b.try_as_u64().map(|v| v as usize)
        } else {
            None
        }
    }

    pub fn exact_integer_to_isize(self) -> Option<isize> {
        if let Number::Fixnum(i) = self {
            Some(i as isize)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64().map(|v| v as isize)
        } else {
            None
        }
    }

    pub fn exact_integer_to_u128(self) -> Option<u128> {
        if let Number::Fixnum(i) = self {
            Some(i as u128)
        } else if let Number::BigInt(b) = self {
            b.try_as_u128()
        } else {
            None
        }
    }

    pub fn exact_integer_to_i128(self) -> Option<i128> {
        if let Number::Fixnum(i) = self {
            Some(i as i128)
        } else if let Number::BigInt(b) = self {
            b.try_as_i128()
        } else {
            None
        }
    }

    pub fn exact_integer_to_f64(self) -> Option<f64> {
        if let Number::Fixnum(i) = self {
            Some(i as f64)
        } else if let Number::BigInt(b) = self {
            Some(b.as_f64())
        } else {
            None
        }
    }

    pub fn exact_integer_to_u16(self) -> Option<u16> {
        self.exact_integer_to_u32()
            .filter(|&v| v <= u16::MAX as u32)
            .map(|v| v as u16)
    }

    pub fn exact_integer_to_u8(self) -> Option<u8> {
        self.exact_integer_to_u32()
            .filter(|&v| v <= u8::MAX as u32)
            .map(|v| v as u8)
    }

    pub fn exact_integer_to_i16(self) -> Option<i16> {
        self.exact_integer_to_i32()
            .filter(|&v| v >= i16::MIN as i32 && v <= i16::MAX as i32)
            .map(|v| v as i16)
    }

    pub fn exact_integer_to_i8(self) -> Option<i8> {
        self.exact_integer_to_i32()
            .filter(|&v| v >= i8::MIN as i32 && v <= i8::MAX as i32)
            .map(|v| v as i8)
    }

    pub fn coerce_exact_integer_to_i32(self) -> i32 {
        match self {
            Number::Fixnum(i) => i,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u32;
                if b.negative() { -(v as i32) } else { v as i32 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i32")
            }
        }
    }

    pub fn coerce_exact_integer_to_u32(self) -> u32 {
        match self {
            Number::Fixnum(i) => i as u32,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u32
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u32")
            }
        }
    }

    pub fn coerce_exact_integer_to_i64(self) -> i64 {
        match self {
            Number::Fixnum(i) => i as i64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() { -(v as i64) } else { v as i64 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i64")
            }
        }
    }

    pub fn coerce_exact_integer_to_u64(self) -> u64 {
        match self {
            Number::Fixnum(i) => i as u64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0]
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u64")
            }
        }
    }

    pub fn coerce_exact_integer_to_usize(self) -> usize {
        match self {
            Number::Fixnum(i) => i as usize,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as usize
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to usize")
            }
        }
    }

    pub fn coerce_exact_integer_to_isize(self) -> isize {
        match self {
            Number::Fixnum(i) => i as isize,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() {
                    -(v as isize)
                } else {
                    v as isize
                }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to isize")
            }
        }
    }

    pub fn coerce_exact_integer_to_f64(self) -> f64 {
        match self {
            Number::Fixnum(i) => i as f64,
            Number::BigInt(b) => b.as_f64(),
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to f64")
            }
        }
    }

    pub fn coerce_exact_integer_to_u8(self) -> u8 {
        match self {
            Number::Fixnum(i) => i as u8,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u8
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u8")
            }
        }
    }

    pub fn coerce_exact_integer_to_i8(self) -> i8 {
        match self {
            Number::Fixnum(i) => i as i8,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() { -(v as i8) } else { v as i8 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i8")
            }
        }
    }

    pub fn coerce_exact_integer_to_u16(self) -> u16 {
        match self {
            Number::Fixnum(i) => i as u16,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u16
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u16")
            }
        }
    }

    pub fn coerce_exact_integer_to_i16(self) -> i16 {
        match self {
            Number::Fixnum(i) => i as i16,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() { -(v as i16) } else { v as i16 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i16")
            }
        }
    }

    pub fn real_to_f64(&self, ctx: Context<'gc>) -> f64 {
        match self {
            Number::Fixnum(i) => *i as f64,
            Number::Flonum(f) => *f,
            Number::BigInt(b) => b.as_f64(),
            Number::Rational(r) => r.to_f64(ctx),
            Number::Complex(c) => {
                if c.imag.is_zero() {
                    c.real.real_to_f64(ctx)
                } else {
                    panic!("Cannot convert complex number to f64");
                }
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Number::Fixnum(i) => *i == 0,
            Number::Flonum(f) => *f == 0.0,
            Number::BigInt(b) => b.is_zero(),
            Number::Rational(_) => false,
            Number::Complex(c) => c.imag.is_zero() && c.real.is_zero(),
        }
    }

    pub fn is_fixnum(&self) -> bool {
        matches!(self, Number::Fixnum(_))
    }

    pub fn is_flonum(&self) -> bool {
        matches!(self, Number::Flonum(_))
    }

    pub fn is_bigint(&self) -> bool {
        matches!(self, Number::BigInt(_))
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, Number::Complex(_))
    }

    pub fn inexact_negate(&self, ctx: Context<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::Flonum(-(*i as f64)),
            Number::Flonum(f) => Number::Flonum(-f),
            Number::BigInt(b) => Number::Flonum(-b.as_f64()),
            Number::Rational(r) => Number::Flonum(-r.to_f64(ctx)),
            Number::Complex(c) => {
                let imag = c.imag.negate(ctx).to_inexact(ctx);
                let real = c.real.negate(ctx).to_inexact(ctx);
                Self::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn normalize_integer(&self) -> Self {
        match self {
            Self::BigInt(n) => n.try_as_i64().map_or_else(
                || *self,
                |v| {
                    if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
                        Number::Fixnum(v as i32)
                    } else {
                        *self
                    }
                },
            ),
            _ => *self,
        }
    }

    pub fn normalize_complex(ctx: Context<'gc>, real: Self, imag: Self) -> Self {
        assert!(!imag.is_complex());
        assert!(!real.is_complex());

        if let Number::Fixnum(0) = imag {
            return real;
        }

        if let Number::BigInt(b) = imag
            && b.is_zero()
        {
            return real;
        }

        if real.is_flonum() || imag.is_flonum() {
            return Number::Complex(Complex::new(
                ctx,
                real.to_inexact(ctx),
                imag.to_inexact(ctx),
            ));
        }

        Number::Complex(Complex::new(ctx, real, imag))
    }

    pub fn reduce_fix_fix(ctx: Context<'gc>, nume: i32, deno: i32) -> Self {
        let mut nume = nume as IDigit;
        let mut deno = deno as IDigit;

        if deno == 1 {
            return Number::Fixnum(nume as _);
        }

        if deno == -1 {
            return Number::Fixnum(nume as _).negate(ctx);
        }

        if nume == 0 {
            return Number::Fixnum(0);
        }

        if nume == 1 {
            if deno < 0 {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    (-deno).into_number(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                1i32.into_number(ctx),
                deno.into_number(ctx),
            ));
        }

        let mut ans_sign = 1;

        if nume < 0 {
            ans_sign = -ans_sign;
            nume = -nume;
        }

        if deno < 0 {
            ans_sign = -ans_sign;
            deno = -deno;
        }

        let mut n1 = nume;
        let mut n2 = deno;

        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }

        let gcd = n1;

        if deno == gcd {
            return (ans_sign * nume / gcd).into_number(ctx);
        }

        Self::Rational(Rational::new(
            ctx,
            (ans_sign * nume / gcd).into_number(ctx),
            (deno / gcd).into_number(ctx),
        ))
    }

    pub fn reduce_fix_big(ctx: Context<'gc>, nume: i32, deno: Gc<'gc, BigInt<'gc>>) -> Self {
        let mut nume = nume as IDigit;
        if nume == 0 {
            return Number::Fixnum(0);
        }

        if nume == 1 {
            if deno.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    Self::BigInt(deno).negate(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                1i32.into_number(ctx),
                Self::BigInt(deno),
            ));
        }

        if nume == -1 {
            if deno.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    Self::BigInt(deno).negate(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                (-1i32).into_number(ctx),
                Self::BigInt(deno),
            ));
        }

        let mut ans_sign = 1;
        if nume < 0 {
            ans_sign = -ans_sign;
            nume = -nume;
        }

        if deno.is_negative() {
            ans_sign = -ans_sign;
        }

        let mut n1 = deno.remainder_digit(nume as _) as IDigit;
        let mut n2 = nume;

        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }

        let gcd = n1;
        nume /= gcd;
        if ans_sign < 0 {
            nume = -nume;
        }

        let (mut quo, _) = BigInt::div_digit(deno, ctx, gcd as _);
        if quo.is_negative() {
            quo = BigInt::negate(quo, ctx);
        }

        let ans_nume = nume.into_number(ctx);
        let ans_deno = quo.into_number(ctx);

        if matches!(ans_deno, Number::Fixnum(1)) {
            return ans_nume;
        }

        Self::Rational(Rational::new(ctx, ans_nume, ans_deno))
    }

    pub fn reduce_big_fix(ctx: Context<'gc>, nume: Gc<'gc, BigInt<'gc>>, deno: i32) -> Self {
        let mut nume = nume;
        let mut deno = deno as IDigit;

        if nume.is_zero() {
            return Number::Fixnum(0);
        }
        if deno == 1 {
            return Number::BigInt(nume);
        }
        if deno == -1 {
            return Number::BigInt(BigInt::negate(nume, ctx));
        }

        let mut ans_sign = 1;
        if deno < 0 {
            ans_sign = -ans_sign;
            deno = -deno;
        }
        if nume.is_negative() {
            ans_sign = -ans_sign;
            nume = BigInt::negate(nume, ctx);
        }

        let mut n1 = nume.remainder_digit(deno as Digit) as IDigit;
        let mut n2 = deno;
        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }
        let gcd = n1;
        deno /= gcd;

        let (mut quo, _) = BigInt::div_digit(nume, ctx, gcd as Digit);
        if ans_sign < 0 {
            quo = BigInt::negate(quo, ctx);
        }
        let ans_nume = Number::BigInt(quo);
        let ans_deno = deno.into_number(ctx);

        if let Number::Fixnum(1) = ans_deno {
            return ans_nume;
        }
        Self::Rational(Rational::new(ctx, ans_nume, ans_deno))
    }

    pub fn reduce(ctx: Context<'gc>, numerator: Self, denominator: Self) -> Self {
        assert!(matches!(numerator, Number::Fixnum(_) | Number::BigInt(_)));
        assert!(matches!(denominator, Number::Fixnum(_) | Number::BigInt(_)));
        if let Number::Fixnum(nume) = numerator {
            if let Number::Fixnum(deno) = denominator {
                return Self::reduce_fix_fix(ctx, nume, deno);
            }

            return Self::reduce_fix_big(ctx, nume, denominator.as_bigint().unwrap());
        }

        if let Number::Fixnum(deno) = denominator {
            return Self::reduce_big_fix(ctx, numerator.as_bigint().unwrap(), deno);
        }

        if matches!(denominator, Number::Fixnum(1)) {
            return numerator;
        }

        if matches!(denominator, Number::Fixnum(-1)) {
            return numerator.negate(ctx);
        }

        if matches!(numerator, Number::Fixnum(0)) {
            return Number::Fixnum(0);
        }

        if matches!(numerator, Number::Fixnum(1)) {
            if denominator.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    denominator.negate(ctx),
                ));
            }
            return Number::Rational(Rational::new(ctx, 1i32.into_number(ctx), denominator));
        }

        if matches!(numerator, Number::Fixnum(-1)) {
            if denominator.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    denominator.negate(ctx),
                ));
            }
            return Number::Rational(Rational::new(ctx, (-1i32).into_number(ctx), denominator));
        }

        let mut n1 = match numerator {
            Number::Fixnum(deno) => BigInt::from_i64(ctx, deno as i64),
            Number::BigInt(b) => b,
            _ => unreachable!(),
        };
        let mut n2 = match denominator {
            Number::Fixnum(nume) => BigInt::from_i64(ctx, nume as i64),
            Number::BigInt(b) => b,
            _ => unreachable!(),
        };
        let nume = n1;
        let deno = n2;
        let mut ans_sign = 1;

        if n1.is_negative() {
            ans_sign = -ans_sign;
            n1 = BigInt::negate(n1, ctx)
        }

        if n2.is_negative() {
            ans_sign = -ans_sign;
            n2 = BigInt::negate(n2, ctx);
        }

        let gcd = BigInt::gcd(n1, ctx, n2);

        if deno == gcd {
            let mut res = BigInt::div(nume, ctx, gcd);
            if ans_sign < 0 {
                res = BigInt::negate(res, ctx);
            }

            return res.into_number(ctx);
        }

        let mut res = BigInt::div(nume, ctx, gcd);
        if ans_sign < 0 {
            res = BigInt::negate(res, ctx);
        }
        Self::Rational(Rational::new(
            ctx,
            res.into_number(ctx),
            BigInt::div(n2, ctx, gcd).into_number(ctx),
        ))
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Number::Fixnum(i) => *i < 0,
            Number::Flonum(f) => *f < 0.0,
            Number::BigInt(b) => b.is_negative(),
            Number::Rational(r) => r.numerator.is_negative(),
            Number::Complex(c) => c.real.is_negative() || c.imag.is_negative(),
        }
    }

    pub fn negate(&self, ctx: Context<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::from_i64(ctx, -(*i as i64)),
            Number::Flonum(f) => Number::Flonum(-f),
            Number::BigInt(b) => Self::BigInt(BigInt::negate(*b, ctx)),
            Number::Rational(r) => {
                let nume = r.numerator.negate(ctx);
                let deno = r.denominator;

                Self::Rational(Rational::new(ctx, nume, deno))
            }
            Number::Complex(c) => {
                let imag = c.imag.negate(ctx);
                let real = c.real.negate(ctx);
                Self::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn to_inexact(&self, ctx: Context<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::Flonum(*i as f64),
            Number::Flonum(f) => Number::Flonum(*f),
            Number::BigInt(b) => Number::Flonum(b.as_f64()),
            Number::Rational(r) => Number::Flonum(r.to_f64(ctx)),
            Number::Complex(c) => {
                let real = c.real.to_inexact(ctx);
                let imag = c.imag.to_inexact(ctx);
                Number::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn as_bigint(&self) -> Option<Gc<'gc, BigInt<'gc>>> {
        if let Number::BigInt(b) = self {
            Some(*b)
        } else {
            None
        }
    }

    pub fn add(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        if let Number::Fixnum(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                match lhs.checked_add(rhs) {
                    Some(res) => return Number::Fixnum(res),
                    None => {
                        return BigInt::from_i64(ctx, lhs as i64 + rhs as i64).into_number(ctx);
                    }
                }
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs as f64 + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return BigInt::plus(BigInt::from_i64(ctx, lhs as i64), ctx, rhs).into_number(ctx);
            } else if let Number::Rational(rn) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        rn.numerator,
                        Self::mul(ctx, rn.denominator, Self::Fixnum(lhs)),
                    ),
                    rn.denominator,
                );
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Fixnum(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Flonum(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return Number::Flonum(lhs + rhs as f64);
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return Number::Flonum(lhs + rhs.as_f64());
            } else if let Number::Rational(rn) = rhs {
                return Number::Flonum(lhs + rn.to_f64(ctx));
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Flonum(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag.to_inexact(ctx)));
            }
        }

        if let Number::BigInt(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return BigInt::plus(lhs, ctx, BigInt::from_i64(ctx, rhs as i64)).into_number(ctx);
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs.as_f64() + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return BigInt::plus(lhs, ctx, rhs).into_number(ctx);
            } else if let Number::Rational(rn) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        rn.numerator,
                        Self::mul(ctx, rn.denominator, Self::BigInt(lhs)),
                    ),
                    rn.denominator,
                );
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::BigInt(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Rational(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                    ),
                    lhs.denominator,
                );
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs.to_f64(ctx) + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                    ),
                    lhs.denominator,
                );
            } else if let Number::Rational(rn) = rhs {
                return Rational::add(ctx, lhs, rn);
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Rational(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Complex(lhs) = lhs {
            let real = lhs.real;
            let imag = lhs.imag;

            if let Number::Fixnum(rhs) = rhs {
                let real = Self::add(ctx, real, Self::Fixnum(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Flonum(rhs) = rhs {
                let real = Self::add(ctx, real, Self::Flonum(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::BigInt(rhs) = rhs {
                let real = Self::add(ctx, real, Self::BigInt(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Rational(rn) = rhs {
                let real = Self::add(ctx, real, Self::Rational(rn));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, real, cn.real);
                let imag = Self::add(ctx, imag, cn.imag);
                return Self::Complex(Complex::new(ctx, real, imag));
            }
        }

        unreachable!()
    }

    pub fn sub(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if let Some(res) = lhs.checked_sub(rhs) {
                        Number::Fixnum(res)
                    } else {
                        BigInt::from_i64(ctx, lhs as i64 - rhs as i64).into_number(ctx)
                    }
                }

                Number::Flonum(rhs) => Number::Flonum(lhs as f64 - rhs),
                Number::BigInt(rhs) => {
                    BigInt::minus(BigInt::from_i64(ctx, lhs as i64), ctx, rhs).into_number(ctx)
                }
                Number::Rational(rn) => Self::reduce(
                    ctx,
                    Self::sub(
                        ctx,
                        Self::mul(ctx, rn.denominator, Self::Fixnum(lhs)),
                        rn.numerator,
                    ),
                    rn.denominator,
                ),

                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::Fixnum(lhs), cn.real);
                    Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)))
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => Number::Flonum(lhs - rhs as f64),
                Number::Flonum(rhs) => Number::Flonum(lhs - rhs),
                Number::BigInt(rhs) => Number::Flonum(lhs - rhs.as_f64()),
                Number::Rational(rn) => Number::Flonum(lhs - rn.to_f64(ctx)),
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::Flonum(lhs), cn.real);
                    Self::Complex(Complex::new(ctx, real, cn.imag.inexact_negate(ctx)))
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    BigInt::minus(lhs, ctx, BigInt::from_i64(ctx, rhs as i64)).into_number(ctx)
                }
                Number::Flonum(rhs) => Number::Flonum(lhs.as_f64() - rhs),
                Number::BigInt(rhs) => BigInt::minus(lhs, ctx, rhs).into_number(ctx),
                Number::Rational(rn) => Self::reduce(
                    ctx,
                    Self::sub(
                        ctx,
                        rn.numerator,
                        Self::mul(ctx, rn.denominator, Self::BigInt(lhs)),
                    ),
                    rn.denominator,
                ),
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::BigInt(lhs), cn.real);
                    Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)))
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => Self::reduce(
                    ctx,
                    Self::sub(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                    ),
                    lhs.denominator,
                ),
                Number::Flonum(rhs) => Number::Flonum(lhs.to_f64(ctx) - rhs),
                Number::BigInt(rhs) => Self::reduce(
                    ctx,
                    Self::sub(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                    ),
                    lhs.denominator,
                ),
                Number::Rational(rn) => Rational::sub(ctx, lhs, rn),
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, cn.real, Self::Rational(lhs));
                    Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)))
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => {
                        let real = Self::sub(ctx, real, Self::Fixnum(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Flonum(rhs) => {
                        let real = Self::sub(ctx, real, Self::Flonum(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::BigInt(rhs) => {
                        let real = Self::sub(ctx, real, Self::BigInt(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Rational(rn) => {
                        let real = Self::sub(ctx, real, Self::Rational(rn));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Complex(cn) => {
                        let real = Self::sub(ctx, cn.real, lhs.real);
                        let imag = Self::sub(ctx, cn.imag, lhs.imag);
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                }
            }
        }
    }

    pub fn mul(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if let Some(res) = lhs.checked_mul(rhs) {
                        Number::Fixnum(res)
                    } else {
                        BigInt::from_i64(ctx, lhs as i64 * rhs as i64).into_number(ctx)
                    }
                }
                Number::Flonum(rhs) => Number::Flonum(lhs as f64 * rhs),
                Number::BigInt(rhs) => {
                    probe::probe!(mul_bigint, start);
                    let res =
                        BigInt::times(BigInt::from_i64(ctx, lhs as i64), ctx, rhs).into_number(ctx);
                    probe::probe!(mul_bigint, end);

                    res
                }
                Number::Rational(rn) => {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::Fixnum(lhs), rn.denominator);
                    }

                    if matches!(rn.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::Fixnum(lhs), rn.denominator).negate(ctx);
                    }

                    Self::reduce(
                        ctx,
                        Self::mul(ctx, rn.numerator, Self::Fixnum(lhs)),
                        rn.denominator,
                    )
                }
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Fixnum(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Fixnum(lhs));
                    Self::Complex(Complex::new(ctx, real, imag))
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => Number::Flonum(lhs * rhs as f64),
                Number::Flonum(rhs) => Number::Flonum(lhs * rhs),
                Number::BigInt(rhs) => Number::Flonum(lhs * rhs.as_f64()),
                Number::Rational(rn) => Number::Flonum(lhs * rn.to_f64(ctx)),
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Flonum(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Flonum(lhs));
                    Self::Complex(Complex::new(ctx, real, imag))
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    BigInt::times(lhs, ctx, BigInt::from_i64(ctx, rhs as i64)).into_number(ctx)
                }
                Number::Flonum(rhs) => Number::Flonum(lhs.as_f64() * rhs),
                Number::BigInt(rhs) => BigInt::times(lhs, ctx, rhs).into_number(ctx),
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::BigInt(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::BigInt(lhs));
                    Self::Complex(Complex::new(ctx, real, imag))
                }

                Number::Rational(rn) => {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::BigInt(lhs), rn.denominator);
                    }

                    if matches!(rn.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::BigInt(lhs), rn.denominator).negate(ctx);
                    }

                    Self::reduce(
                        ctx,
                        Self::mul(ctx, rn.numerator, Self::BigInt(lhs)),
                        rn.denominator,
                    )
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if matches!(lhs.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::Fixnum(rhs), lhs.denominator);
                    }

                    if matches!(lhs.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::Fixnum(rhs), lhs.denominator).negate(ctx);
                    }

                    Self::reduce(
                        ctx,
                        Self::mul(ctx, lhs.numerator, Self::Fixnum(rhs)),
                        lhs.denominator,
                    )
                }
                Number::Flonum(rhs) => Number::Flonum(lhs.to_f64(ctx) * rhs),
                Number::BigInt(rhs) => {
                    if matches!(lhs.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::BigInt(rhs), lhs.denominator);
                    }

                    if matches!(lhs.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::BigInt(rhs), lhs.denominator).negate(ctx);
                    }

                    Self::reduce(
                        ctx,
                        Self::mul(ctx, lhs.numerator, Self::BigInt(rhs)),
                        lhs.denominator,
                    )
                }
                Number::Rational(rn) => Rational::mul(ctx, lhs, rn),
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Rational(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Rational(lhs));
                    Self::Complex(Complex::new(ctx, real, imag))
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => {
                        let real = Self::mul(ctx, real, Self::Fixnum(rhs));
                        let imag = Self::mul(ctx, imag, Self::Fixnum(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Flonum(rhs) => {
                        let real = Self::mul(ctx, real, Self::Flonum(rhs));
                        let imag = Self::mul(ctx, imag, Self::Flonum(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::BigInt(rhs) => {
                        let real = Self::mul(ctx, real, Self::BigInt(rhs));
                        let imag = Self::mul(ctx, imag, Self::BigInt(rhs));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Rational(rn) => {
                        let real = Self::mul(ctx, real, Self::Rational(rn));
                        let imag = Self::mul(ctx, imag, Self::Rational(rn));
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                    Number::Complex(cn) => {
                        let real = Self::sub(
                            ctx,
                            Self::mul(ctx, cn.real, lhs.real),
                            Self::mul(ctx, cn.imag, lhs.imag),
                        );
                        let imag = Self::add(
                            ctx,
                            Self::mul(ctx, cn.real, lhs.imag),
                            Self::mul(ctx, cn.imag, lhs.real),
                        );
                        Self::Complex(Complex::new(ctx, real, imag))
                    }
                }
            }
        }
    }

    pub fn integer_div(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        if let Self::Fixnum(x) = lhs
            && let Number::Fixnum(y) = rhs
        {
            let x = x as i64;
            let y = y as i64;
            let div = if x == 0 {
                0
            } else if x > 0 {
                x / y
            } else if y > 0 {
                (x - y + 1) / y
            } else {
                (x + y + 1) / y
            };

            return div.into_number(ctx);
        }

        if matches!(lhs, Number::Flonum(_)) || matches!(rhs, Number::Flonum(_)) {
            let x = lhs.real_to_f64(ctx);
            let y = rhs.real_to_f64(ctx);
            return Self::Flonum(if y > 0.0 {
                (x / y).floor()
            } else {
                -((x / -y).floor())
            });
        }

        if rhs.is_positive() {
            return Self::div(ctx, lhs, rhs).floor(ctx);
        }

        Self::div(ctx, lhs, rhs.negate(ctx)).floor(ctx).negate(ctx)
    }

    pub fn integer_div0(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        let div = Self::integer_div(ctx, lhs, rhs);
        let mod_ = Self::sub(ctx, lhs, Self::mul(ctx, div, rhs));
        if Self::compare(
            ctx,
            mod_,
            Self::magnitude(ctx, Self::div(ctx, rhs, Self::Fixnum(2))),
        ) == Some(std::cmp::Ordering::Less)
        {
            return div;
        }
        if rhs.is_positive() {
            return Self::add(ctx, div, Number::Fixnum(1));
        }

        Self::sub(ctx, div, Number::Fixnum(1))
    }

    pub fn div(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => {
                if lhs == 0 {
                    if rhs.is_exact() {
                        return Number::Fixnum(0);
                    }

                    return Self::div(ctx, Self::Flonum(0.0), rhs);
                }

                match rhs {
                    Number::Fixnum(rhs) => Self::reduce_fix_fix(ctx, lhs, rhs),
                    Number::Flonum(rhs) => Number::Flonum(lhs as f64 / rhs),
                    Number::BigInt(rhs) => Self::reduce_fix_big(ctx, lhs, rhs),

                    Number::Rational(rhs) => Self::reduce(
                        ctx,
                        Self::mul(ctx, rhs.denominator, Self::Fixnum(lhs)),
                        rhs.numerator,
                    ),

                    Number::Complex(rhs) => {
                        let real = rhs.real;
                        let imag = rhs.imag;
                        let r2 =
                            Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));
                        Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, real, Self::Fixnum(lhs)), r2),
                            Self::div(ctx, Self::mul(ctx, imag, Self::Fixnum(lhs)), r2).negate(ctx),
                        ))
                    }
                }
            }

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => Number::Flonum(lhs / rhs as f64),
                Number::Flonum(rhs) => Number::Flonum(lhs / rhs),
                Number::BigInt(rhs) => Number::Flonum(lhs / rhs.as_f64()),
                Number::Rational(rhs) => Number::Flonum(lhs / rhs.to_f64(ctx)),
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, Self::Flonum(lhs)), r2),
                        Self::div(ctx, Self::mul(ctx, imag, Self::Flonum(lhs)), r2).negate(ctx),
                    ))
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => Self::reduce_big_fix(ctx, lhs, rhs),
                Number::Flonum(rhs) => Number::Flonum(lhs.as_f64() / rhs),
                Number::BigInt(rhs) => Self::reduce(ctx, Self::BigInt(lhs), Self::BigInt(rhs)),
                Number::Rational(rhs) => Self::reduce(
                    ctx,
                    Self::mul(ctx, Self::BigInt(lhs), rhs.denominator),
                    rhs.numerator,
                ),
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, Self::BigInt(lhs)), r2),
                        Self::div(ctx, Self::mul(ctx, imag, Self::BigInt(lhs)), r2).negate(ctx),
                    ))
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => Self::reduce(
                    ctx,
                    lhs.numerator,
                    Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                ),
                Number::Flonum(rhs) => Number::Flonum(lhs.to_f64(ctx) / rhs),
                Number::BigInt(rhs) => Self::reduce(
                    ctx,
                    lhs.numerator,
                    Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                ),
                Number::Rational(rhs) => Rational::div(ctx, lhs, rhs),
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, lhs.numerator), r2),
                        Self::mul(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, imag, lhs.numerator), r2).negate(ctx),
                            lhs.denominator,
                        ),
                    ))
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, real, Self::Fixnum(rhs)),
                        Self::div(ctx, imag, Self::Fixnum(rhs)),
                    )),

                    Number::Flonum(rhs) => Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, real, Self::Flonum(rhs)),
                        Self::div(ctx, imag, Self::Flonum(rhs)),
                    )),

                    Number::BigInt(rhs) => Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, real, Self::BigInt(rhs)),
                        Self::div(ctx, imag, Self::BigInt(rhs)),
                    )),

                    Number::Rational(rhs) => {
                        let r2 = Self::add(
                            ctx,
                            Self::mul(ctx, rhs.numerator, rhs.numerator),
                            Self::mul(ctx, rhs.denominator, rhs.denominator),
                        );

                        Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, real, rhs.numerator), r2),
                            Self::mul(
                                ctx,
                                Self::div(ctx, Self::mul(ctx, imag, rhs.numerator), r2).negate(ctx),
                                rhs.denominator,
                            ),
                        ))
                    }

                    Self::Complex(rhs) => {
                        let real2 = rhs.real;
                        let imag2 = rhs.imag;
                        let r2 = Self::add(
                            ctx,
                            Self::mul(ctx, real2, real2),
                            Self::mul(ctx, imag2, imag2),
                        );
                        let real3 = Self::div(
                            ctx,
                            Self::add(
                                ctx,
                                Self::mul(ctx, real, real2),
                                Self::mul(ctx, imag, imag2),
                            ),
                            r2,
                        );
                        let imag3 = Self::div(
                            ctx,
                            Self::sub(
                                ctx,
                                Self::mul(ctx, imag, real2),
                                Self::mul(ctx, real, imag2),
                            ),
                            r2,
                        );
                        Self::Complex(Complex::new(ctx, real3, imag3))
                    }
                }
            }
        }
    }

    pub fn quotient(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Fixnum(lhs / rhs);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs as f64 / rhs);
                            }

                            Number::BigInt(rhs) => {
                                return BigInt::div(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                    .into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() && lhs == 0.0 {
                        return Number::Flonum(0.0);
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum(lhs / rhs as f64);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs / rhs);
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum(lhs / rhs.as_f64());
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            return Self::BigInt(BigInt::div(
                                lhs,
                                ctx,
                                BigInt::from_i64(ctx, rhs as i64),
                            ));
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum(lhs.as_f64() / rhs);
                        }

                        Number::BigInt(rhs) => {
                            return BigInt::div(lhs, ctx, rhs).into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    pub fn remainder(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Fixnum(lhs % rhs);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs as f64 % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return BigInt::rem(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                    .into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() {
                        if lhs == 0.0 {
                            return Number::Flonum(0.0);
                        }
                    } else {
                        unreachable!()
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum(lhs % rhs as f64);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum(lhs % rhs.as_f64());
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            return Self::BigInt(BigInt::rem(
                                lhs,
                                ctx,
                                BigInt::from_i64(ctx, rhs as i64),
                            ));
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum(lhs.as_f64() % rhs);
                        }

                        Number::BigInt(rhs) => {
                            return BigInt::rem(lhs, ctx, rhs).into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    pub fn modulo(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    return Number::Fixnum(0);
                                }
                                let mut rem = lhs.wrapping_rem(rhs);
                                if rem == 0 {
                                    return Self::Fixnum(0);
                                }

                                if (rem > 0) as usize + (rhs > 0) as usize == 1 {
                                    rem = rem.wrapping_add(rhs);
                                }

                                return Self::Fixnum(rem);
                            }

                            Number::Flonum(rhs) => {
                                let rem = libm::fmod(lhs as f64, rhs);
                                if rem == 0.0 {
                                    return Self::Flonum(0.0);
                                }

                                if (rhs > 0.0) as usize + (rem > 0.0) as usize == 1 {
                                    return Self::Flonum(rem + rhs);
                                }

                                return Self::Flonum(rem);
                            }

                            Number::BigInt(rhs) => {
                                /*return BigInt::rem(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                .into_number(ctx);*/
                                let bi_lhs = BigInt::from_i64(ctx, lhs as i64);
                                let rem = BigInt::rem(bi_lhs, ctx, rhs);
                                if rem.is_zero() {
                                    return Self::Fixnum(0);
                                }

                                if (rhs.is_negative() as usize) + (rem.is_negative() as usize) == 1
                                {
                                    return Self::add(ctx, Self::BigInt(rem), Self::BigInt(rhs));
                                }

                                return rem.into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() {
                        if lhs == 0.0 {
                            return Number::Flonum(0.0);
                        }
                    } else {
                        unreachable!()
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                let rem = libm::fmod(lhs, rhs as f64);
                                if rem == 0.0 {
                                    return Self::Flonum(0.0);
                                }

                                if (rhs > 0) as usize + (rem > 0.0) as usize == 1 {
                                    return Self::Flonum(rem + rhs as f64);
                                }

                                return Self::Flonum(rem);
                            }

                            Number::Flonum(rhs) => {
                                let rem = libm::fmod(lhs, rhs);
                                if rem == 0.0 {
                                    return Self::Flonum(0.0);
                                }
                                if (rhs > 0.0) as usize + (rem > 0.0) as usize == 1 {
                                    return Self::Flonum(rem + rhs);
                                }

                                return Self::Flonum(rem);
                            }

                            Number::BigInt(rhs) => {
                                let rem = libm::fmod(lhs, rhs.as_f64());
                                if rem == 0.0 {
                                    return Self::Flonum(0.0);
                                }

                                if rhs.is_positive() as usize + (rem > 0.0) as usize == 1 {
                                    return Self::Flonum(rem + rhs.as_f64());
                                }

                                return Self::Flonum(rem);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            let bi_rhs = BigInt::from_i64(ctx, rhs as i64);
                            let rem = BigInt::rem(lhs, ctx, bi_rhs);
                            if rem.is_zero() {
                                return Self::Fixnum(0);
                            }

                            if bi_rhs.is_negative() as usize + rem.is_negative() as usize == 1 {
                                return Self::add(ctx, Self::BigInt(rem), Self::BigInt(bi_rhs));
                            }

                            return rem.into_number(ctx);
                        }

                        Number::Flonum(rhs) => {
                            let rem = libm::fmod(lhs.as_f64(), rhs);
                            if rem == 0.0 {
                                return Self::Flonum(0.0);
                            }

                            if rhs.is_sign_positive() as usize + (rem > 0.0) as usize == 1 {
                                return Self::Flonum(rem + rhs);
                            }

                            return Self::Flonum(rem);
                        }

                        Number::BigInt(rhs) => {
                            let rem = BigInt::rem(lhs, ctx, rhs);
                            if rem.is_zero() {
                                return Self::Fixnum(0);
                            }

                            if rhs.is_positive() as usize + rem.is_positive() as usize == 1 {
                                return Self::add(ctx, Self::BigInt(rem), Self::BigInt(rhs));
                            }

                            return rem.into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    fn expt_impl(ctx: Context<'gc>, mut lhs: Self, rhs: Self) -> Self {
        let Number::Fixnum(mut n) = rhs else {
            unreachable!()
        };

        if n == 0 {
            return Number::Fixnum(1);
        }

        if n == 1 {
            return lhs;
        }

        if n < 0 {
            let x = Self::expt_impl(ctx, lhs, Number::Fixnum(-n));
            return Self::inverse(ctx, x);
        }

        if !matches!(lhs, Number::Complex(_)) && lhs.is_negative() {
            let ans = Self::expt_impl(ctx, lhs.negate(ctx), Number::Fixnum(n));
            if n & 1 != 0 {
                return ans.negate(ctx);
            }

            return ans;
        }

        if matches!(lhs, Number::Fixnum(0) | Number::Fixnum(1)) {
            return lhs;
        }

        if matches!(lhs, Number::Fixnum(2)) {
            if n < 31 {
                return Self::Fixnum(1 << n);
            }

            if n < 63 {
                let ans = BigInt::from_u64(ctx, 1 << n as u64);
                return Self::BigInt(ans);
            }
        }

        if let Number::Rational(rn) = lhs {
            let nume = Self::expt_impl(ctx, rn.numerator, rhs);
            let deno = Self::expt_impl(ctx, rn.denominator, rhs);
            return Self::reduce(ctx, deno, nume);
        }

        let mut ans = Self::Fixnum(1);

        loop {
            if n & 1 != 0 {
                if matches!(ans, Number::Fixnum(1)) {
                    ans = lhs;
                } else {
                    ans = Self::mul(ctx, ans, lhs);
                }

                if n == 1 {
                    return ans.normalize_integer();
                }
            }

            lhs = Self::mul(ctx, lhs, lhs);
            n >>= 1;
        }
    }

    pub fn expt(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        if let Number::Flonum(0.0) = lhs {
            if let Number::Complex(cn) = rhs {
                if cn.real.is_positive() {
                    return Number::Flonum(0.0);
                }
            } else {
                if rhs.is_positive() {
                    return Number::Flonum(0.0);
                }
            }
        }

        if rhs.is_exact() {
            if let Number::Fixnum(rhs) = rhs {
                if rhs == 0 {
                    return Number::Fixnum(1);
                }

                if let Number::Flonum(lhs) = lhs {
                    return Number::Flonum(lhs.powi(rhs));
                }

                return Self::expt_impl(ctx, lhs, Number::Fixnum(rhs));
            }

            if let Number::BigInt(rhs) = rhs {
                if lhs.is_real_valued() {
                    let n = rhs.as_f64();
                    return Number::Flonum(lhs.real_to_f64(ctx).powf(n));
                }

                return Self::exp(
                    ctx,
                    Self::mul(ctx, Number::BigInt(rhs), Self::log(ctx, lhs)),
                );
            }

            if let Number::Rational(_) = rhs {
                let n = rhs.real_to_f64(ctx);
                if lhs.is_real_valued() && !lhs.is_negative() {
                    return Number::Flonum(lhs.real_to_f64(ctx).powf(n));
                }

                return Self::exp(ctx, Self::mul(ctx, rhs, Self::log(ctx, lhs)));
            }

            if let Number::Complex(_) = rhs {
                return Self::exp(ctx, Self::mul(ctx, rhs, Self::log(ctx, lhs)));
            }
        } else {
            if let Number::Flonum(f) = rhs
                && lhs.is_real_valued()
                && !rhs.is_negative()
            {
                return Number::Flonum(lhs.real_to_f64(ctx).powf(f));
            }
        }

        Self::exp(ctx, Self::mul(ctx, rhs, Self::log(ctx, lhs)))
    }

    pub fn exp(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(1);
                }

                Number::Flonum((n as f64).exp())
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let a = real.exp();
                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(a * imag.cos()),
                    Self::Flonum(a * imag.sin()),
                ))
            }

            _ => {
                if n.is_real_valued() {
                    return Self::Flonum(n.real_to_f64(ctx).exp());
                }

                unreachable!()
            }
        }
    }

    pub fn log(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let value = n as IDigit;

                if value > 0 {
                    if value == 1 {
                        return Number::Fixnum(0);
                    }

                    return Number::Flonum(libm::log(value as f64));
                }

                let real = value as f64;

                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * libm::log(real * real)),
                    Self::Flonum(libm::atan2(0.0, real)),
                ))
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(libm::log(real * real + imag * imag) * 0.5),
                    Self::Flonum(libm::atan2(imag, real)),
                ))
            }

            _ => {
                let real = n.real_to_f64(ctx);
                if real.is_infinite() && n.is_exact_positive_integer() {
                    let (sqrt, _) = Self::exact_integer_sqrt(ctx, n);
                    return Self::add(ctx, Self::log(ctx, sqrt), Self::log(ctx, sqrt));
                }

                if real > 0.0 {
                    return Self::Flonum(libm::log(real));
                }

                let imag = libm::atan2(0.0, real);
                if imag == 0.0 {
                    Self::Flonum(0.5 * libm::log(real * real))
                } else {
                    Self::Complex(Complex::new(
                        ctx,
                        Self::Flonum(0.5 * libm::log(real * real)),
                        Self::Flonum(imag),
                    ))
                }
            }
        }
    }

    pub fn sin(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(0);
                }

                Number::Flonum(libm::sin(n as f64))
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;

                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * real.sin() * (e + f)),
                    Self::Flonum(0.5 * real.cos() * (e - f)),
                ))
            }

            _ => Self::Flonum(n.real_to_f64(ctx).sin()),
        }
    }

    pub fn cos(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(1);
                }

                Number::Flonum(libm::cos(n as f64))
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;

                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * real.cos() * (f + e)),
                    Self::Flonum(0.5 * real.sin() * (f - e)),
                ))
            }

            _ => Self::Flonum(n.real_to_f64(ctx).cos()),
        }
    }

    pub fn tan(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(0);
                }

                Number::Flonum(libm::tan(n as f64))
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;
                let d = (2.0 * real).cos() + 0.5 * (e + f);

                Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum((2.0 * real).sin() / d),
                    Self::Flonum(0.5 * (e - f) / d),
                ))
            }

            _ => Self::Flonum(n.real_to_f64(ctx).tan()),
        }
    }

    pub fn sqrt(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let mut value = n as IDigit;
                if value == 0 {
                    return Number::Fixnum(0);
                }

                if value > 0 {
                    let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                    if iroot.wrapping_mul(iroot) == value {
                        Number::Fixnum(iroot as i32)
                    } else {
                        Self::Flonum(libm::sqrt(value as f64))
                    }
                } else {
                    value = -value;
                    let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                    if iroot.wrapping_mul(iroot) == value {
                        Self::Complex(Complex::new(
                            ctx,
                            Self::Fixnum(0),
                            Number::Fixnum(iroot as i32),
                        ))
                    } else {
                        Self::Complex(Complex::new(
                            ctx,
                            Self::Flonum(0.0),
                            Self::Flonum(libm::sqrt(value as f64)),
                        ))
                    }
                }
            }

            Number::BigInt(n) => {
                if n.is_positive() {
                    let s = BigInt::sqrt(n, ctx);
                    if BigInt::times(s, ctx, s) == n {
                        s.into_number(ctx)
                    } else {
                        Self::Flonum(n.as_f64().sqrt())
                    }
                } else {
                    let n = BigInt::negate(n, ctx);
                    let s = BigInt::sqrt(n, ctx);
                    if BigInt::times(s, ctx, s) == n {
                        Self::Complex(Complex::new(ctx, Self::Fixnum(0), s.into_number(ctx)))
                    } else {
                        Self::Complex(Complex::new(
                            ctx,
                            Self::Flonum(0.0),
                            Self::Flonum(n.as_f64().sqrt()),
                        ))
                    }
                }
            }

            Number::Rational(rn) => {
                let numerator;

                let complex;
                if rn.numerator.is_negative() {
                    numerator = rn.numerator.negate(ctx);
                    complex = true;
                } else {
                    complex = false;
                    numerator = Self::sqrt(ctx, rn.numerator);
                }

                let denominator = Self::sqrt(ctx, rn.denominator);

                if matches!(numerator, Number::Fixnum(_) | Number::BigInt(_))
                    && matches!(denominator, Number::Fixnum(_) | Number::BigInt(_))
                {
                    if complex {
                        return Self::Complex(Complex::new(
                            ctx,
                            Number::Fixnum(0),
                            Self::reduce(ctx, numerator, denominator),
                        ));
                    } else {
                        return Self::reduce(ctx, numerator, denominator);
                    }
                }

                if complex {
                    Self::Complex(Complex::new(
                        ctx,
                        Number::Fixnum(0),
                        Self::div(ctx, numerator, denominator),
                    ))
                } else {
                    Self::div(ctx, numerator, denominator)
                }
            }

            Number::Flonum(fl) => {
                if fl < 0.0 {
                    Self::Complex(Complex::new(
                        ctx,
                        Self::Flonum(0.0),
                        Self::Flonum(libm::sqrt(-fl)),
                    ))
                } else {
                    Self::Flonum(libm::sqrt(fl))
                }
            }

            Number::Complex(cn) => {
                if n.is_exact() {
                    let m = Self::magnitude(ctx, n);
                    let x = Self::div(ctx, Self::add(ctx, cn.real, m), Self::Fixnum(2));
                    let y = Self::div(ctx, cn.imag, Self::Fixnum(2));
                    let s = Self::sqrt(
                        ctx,
                        Self::div(
                            ctx,
                            m,
                            Self::add(ctx, Self::mul(ctx, x, x), Self::mul(ctx, y, y)),
                        ),
                    );

                    return Self::normalize_complex(
                        ctx,
                        Self::mul(ctx, x, s),
                        Self::mul(ctx, y, s),
                    );
                }

                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);
                let m = libm::sqrt(real * real + imag * imag);
                let x = (real + m) / 2.0;
                let y = imag / 2.0;
                let s = libm::sqrt(m / (x * x + y * y));

                Self::Complex(Complex::new(ctx, Self::Flonum(x * s), Self::Flonum(y * s)))
            }
        }
    }

    pub fn magnitude(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Self::Complex(cn) => {
                // (magnitude 3/22+2/11i) => 5/22
                if n.is_exact() {
                    if cn.real.is_zero() {
                        return Self::magnitude(ctx, cn.imag);
                    }

                    if cn.imag.is_zero() {
                        return Self::magnitude(ctx, cn.real);
                    }

                    Self::sqrt(
                        ctx,
                        Self::add(
                            ctx,
                            Self::mul(ctx, cn.real, cn.imag),
                            Self::mul(ctx, cn.imag, cn.imag),
                        ),
                    )
                } else {
                    let real = cn.real.real_to_f64(ctx);
                    let imag = cn.imag.real_to_f64(ctx);
                    if real.is_infinite() || imag.is_infinite() {
                        return Self::Flonum(f64::INFINITY);
                    }

                    let m = libm::sqrt(real * real + imag * imag);
                    if m < f64::EPSILON || m.is_infinite() {
                        return Self::Flonum(imag / imag.atan2(real).sin());
                    }

                    Self::Flonum(m)
                }
            }

            _ => {
                if n.is_negative() {
                    return n.negate(ctx);
                }

                n
            }
        }
    }

    pub fn exact_integer_sqrt(ctx: Context<'gc>, n: Self) -> (Self, Self) {
        match n {
            Number::Fixnum(n) => {
                let value = n as IDigit;

                if value == 0 {
                    return (Number::Fixnum(0), Number::Fixnum(0));
                }

                let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                (
                    Number::Fixnum(iroot as i32),
                    Number::Fixnum((value - iroot * iroot) as i32),
                )
            }

            Number::BigInt(x) => {
                let s = BigInt::sqrt(x, ctx).into_number(ctx);
                let r = Self::sub(ctx, n, Self::mul(ctx, s, s));
                (s, r)
            }

            _ => unreachable!(),
        }
    }

    pub fn inverse(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n > 0 {
                    if n == 1 {
                        return Number::Fixnum(1);
                    } else {
                        return Number::Rational(Rational::new(
                            ctx,
                            Number::Fixnum(1),
                            Number::Fixnum(n),
                        ));
                    }
                }

                if n == -1 {
                    Number::Fixnum(-1)
                } else {
                    Number::Rational(Rational::new(
                        ctx,
                        Number::Fixnum(-1),
                        Number::Fixnum(n).negate(ctx),
                    ))
                }
            }

            Number::Flonum(fl) => Number::Flonum(1.0 / fl),
            Number::BigInt(bi) => {
                if bi.is_positive() {
                    Number::Rational(Rational::new(ctx, Number::Fixnum(1), Number::BigInt(bi)))
                } else {
                    Number::Rational(Rational::new(
                        ctx,
                        Number::Fixnum(-1),
                        Number::BigInt(BigInt::negate(bi, ctx)),
                    ))
                }
            }

            Number::Rational(rn) => {
                if !rn.numerator.is_negative() {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return rn.denominator.normalize_integer();
                    }

                    return Number::Rational(Rational::new(ctx, rn.denominator, rn.numerator));
                }

                if matches!(rn.numerator, Number::Fixnum(-1)) {
                    rn.denominator.negate(ctx)
                } else {
                    Number::Rational(Rational::new(
                        ctx,
                        rn.denominator.negate(ctx),
                        rn.numerator.negate(ctx),
                    ))
                }
            }
            Number::Complex(_) => Self::div(ctx, Number::Fixnum(1), n),
        }
    }

    pub fn asin(ctx: Context<'gc>, n: Self) -> Self {
        let cn = if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            if (-1.0..=1.0).contains(&x) {
                return Self::Flonum(libm::asin(x));
            }

            if x < 0.0 {
                return Self::asin(ctx, Self::Flonum(-x)).negate(ctx);
            }

            Complex::new(ctx, Self::Flonum(0.0), Self::Flonum(x))
        } else {
            match n {
                Self::Complex(x) => {
                    if x.imag.is_positive() {
                        return Self::asin(ctx, n.negate(ctx)).negate(ctx);
                    }

                    Complex::new(ctx, x.imag.negate(ctx), x.real)
                }

                _ => unreachable!(),
            }
        };

        let ans = Self::log(
            ctx,
            Self::add(
                ctx,
                Self::sqrt(ctx, Self::sub(ctx, Number::Fixnum(1), Self::mul(ctx, n, n))),
                cn.into_number(ctx),
            ),
        );

        match ans {
            Number::Complex(cn) => Number::Complex(Complex::new(
                ctx,
                Self::Flonum(cn.imag.real_to_f64(ctx)),
                Self::Flonum(cn.real.real_to_f64(ctx)),
            )),
            _ => Self::Complex(Complex::new(
                ctx,
                Self::Flonum(0.0),
                Self::Flonum(-ans.real_to_f64(ctx)),
            )),
        }
    }

    pub fn acos(ctx: Context<'gc>, n: Self) -> Self {
        if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            if (-1.0..=1.0).contains(&x) {
                return Self::Flonum(libm::acos(x));
            }
        }

        Self::sub(
            ctx,
            Self::Flonum(std::f64::consts::PI / 2.0),
            Self::asin(ctx, n),
        )
    }

    pub fn atan(ctx: Context<'gc>, n: Self) -> Self {
        if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            return Self::Flonum(libm::atan(x));
        }

        let Number::Complex(cn) = n else {
            unreachable!("what? {n}")
        };

        let ans = Self::div(
            ctx,
            Self::add(ctx, n, Number::Fixnum(1)),
            Self::sub(ctx, n, Number::Fixnum(1)),
        );
        if let Number::Complex(cn) = ans {
            return Number::Complex(Complex::new(
                ctx,
                Number::mul(ctx, Number::Flonum(0.5), cn.imag),
                Number::mul(ctx, Number::Flonum(-0.5), cn.real),
            ));
        }

        Number::Complex(Complex::new(
            ctx,
            Number::Flonum(0.0),
            Number::mul(ctx, Number::Flonum(-0.5), cn.real),
        ))
    }

    pub fn atan2(ctx: Context<'gc>, lhs: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        if matches!(lhs, Number::Fixnum(0)) {
            return Number::Fixnum(0);
        }

        let lhs = lhs.real_to_f64(ctx);
        let rhs = rhs.real_to_f64(ctx);

        Number::Flonum(libm::atan2(lhs, rhs))
    }

    pub fn floor(self, ctx: Context<'gc>) -> Self {
        let n = self;
        match n {
            Number::Fixnum(_) | Number::BigInt(_) => n,
            Number::Flonum(fl) => Self::Flonum(libm::floor(fl)),

            Number::Rational(rn) => {
                if rn.numerator.is_negative() {
                    return Self::sub(
                        ctx,
                        Self::quotient(ctx, rn.numerator, rn.denominator),
                        Number::Fixnum(1),
                    );
                }

                Self::quotient(ctx, rn.numerator, rn.denominator)
            }

            _ => unreachable!(),
        }
    }

    pub fn ceiling(self, ctx: Context<'gc>) -> Self {
        let n = self;
        match n {
            Number::Fixnum(_) | Number::BigInt(_) => n,
            Number::Flonum(fl) => Self::Flonum(libm::ceil(fl)),

            Number::Rational(rn) => {
                if rn.numerator.is_positive() {
                    return Self::add(
                        ctx,
                        Self::quotient(ctx, rn.numerator, rn.denominator),
                        Number::Fixnum(1),
                    );
                }

                Self::quotient(ctx, rn.numerator, rn.denominator)
            }

            _ => unreachable!(),
        }
    }

    pub fn truncate(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self,
            Self::Flonum(fl) => Self::Flonum(fl.trunc()),

            Self::Rational(rn) => Self::quotient(ctx, rn.numerator, rn.denominator),

            _ => unreachable!(),
        }
    }

    pub fn round(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self,
            Self::Flonum(fl) => {
                /*let ans = (fl + 0.5).floor();
                if ans != fl + 0.5 {
                    return Self::Flonum(fl);
                }

                if ans * 0.5 == (ans * 0.5).floor() {
                    return Self::Flonum(ans - 1.0);
                } else {
                    return Self::Flonum(ans);
                }*/
                Self::Flonum(fl.round())
            }

            Self::Rational(rn) => {
                let negative = rn.numerator.is_negative();
                let half = Self::Rational(Rational::new(
                    ctx,
                    Number::Fixnum(if negative { -1 } else { 1 }),
                    Number::Fixnum(2),
                ));
                let n_and_half = Self::add(ctx, self, half);
                if let Number::Rational(rn) = n_and_half {
                    Self::quotient(ctx, rn.numerator, rn.denominator)
                } else {
                    if n_and_half.is_even() {
                        return n_and_half;
                    }

                    Self::add(
                        ctx,
                        n_and_half,
                        Number::Fixnum(if negative { -1 } else { 1 }),
                    )
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn to_exact(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Flonum(n) => {
                if n == 0.0 {
                    return Self::Fixnum(0);
                }
                let (mant, exp, sign) = decode_double(n);

                if mant > 0 {
                    if exp == 0 {
                        return Self::from_i64(ctx, sign as i64 * mant);
                    }

                    if exp > 0 {
                        let bn = BigInt::from_i64(ctx, sign as i64 * mant);
                        let bn = BigInt::shift_left(bn, ctx, exp as usize);
                        return bn.into_number(ctx);
                    }

                    let mut bn = BigInt::one(ctx);
                    bn = BigInt::shift_left(bn, ctx, (-exp) as usize);

                    if sign < 0 {
                        bn = BigInt::negate(bn, ctx);
                    }
                    return Self::reduce(ctx, Self::from_i64(ctx, mant), bn.into_number(ctx));
                }
                unreachable!()
            }

            _ => self,
        }
    }

    pub fn exact_equal(lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                _ => false,
            },

            Number::BigInt(lhs) => match rhs {
                Number::BigInt(rhs) => lhs == rhs,
                _ => false,
            },

            Number::Rational(lhs) => match rhs {
                Number::Rational(rhs) => {
                    Self::exact_equal(lhs.numerator, rhs.numerator)
                        && Self::exact_equal(lhs.denominator, rhs.denominator)
                }
                _ => false,
            },

            Number::Flonum(_) => unreachable!(),

            Number::Complex(lhs) => match rhs {
                Number::Complex(rhs) => {
                    Self::exact_equal(lhs.real, rhs.real) && Self::exact_equal(lhs.imag, rhs.imag)
                }

                _ => false,
            },
        }
    }

    pub fn inexact_equal(lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Flonum(lhs) => match rhs {
                Number::Flonum(rhs) => lhs.to_bits() == rhs.to_bits(),

                Number::Complex(cn) if cn.imag.is_zero() => {
                    Self::inexact_equal(Self::Flonum(lhs), cn.real)
                }

                _ => false,
            },

            Number::Complex(cn) => {
                if cn.imag.is_zero() {
                    return Self::inexact_equal(cn.real, rhs);
                }

                match rhs {
                    Number::Complex(cn2) => {
                        Self::inexact_equal(cn.real, cn2.real)
                            && Self::inexact_equal(cn.imag, cn2.imag)
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    pub fn equal(ctx: Context<'gc>, lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                Number::Flonum(rhs) => (lhs as f64 - rhs).abs() < f64::EPSILON,
                Number::BigInt(rhs) => false,
                Number::Rational(rn) => false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::Fixnum(lhs), cn.real)
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => (lhs - rhs as f64).abs() < f64::EPSILON,
                Number::Flonum(rhs) => {
                    if lhs.is_infinite() && rhs.is_infinite() {
                        return lhs.is_sign_positive() == rhs.is_sign_positive();
                    }

                    (lhs - rhs).abs() < f64::EPSILON
                }
                Number::BigInt(rhs) => {
                    if rhs.as_f64() == lhs {
                        return Self::compare(ctx, Self::Flonum(lhs), Self::BigInt(rhs))
                            == Some(std::cmp::Ordering::Equal);
                    }

                    false
                }
                Number::Rational(rn) => rn.to_f64(ctx) == lhs,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::Flonum(lhs), cn.real)
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => false,

                Number::Flonum(rhs) => (lhs.as_f64() - rhs).abs() < f64::EPSILON,
                Number::BigInt(rhs) => lhs == rhs,
                Number::Rational(rn) => false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::BigInt(lhs), cn.real)
                }
            },

            Number::Rational(lhs) => {
                let nume = lhs.numerator;
                let deno = lhs.denominator;

                match rhs {
                    Number::Fixnum(rhs) => false,

                    Number::Flonum(rhs) => (lhs.to_f64(ctx) - rhs).abs() < f64::EPSILON,
                    Number::BigInt(rhs) => false,
                    Number::Rational(rhs) => {
                        Self::equal(ctx, nume, rhs.numerator)
                            && Self::equal(ctx, deno, rhs.denominator)
                    }

                    Number::Complex(cn) => {
                        cn.imag.is_zero() && Self::equal(ctx, Self::Rational(lhs), cn.real)
                    }
                }
            }

            Number::Complex(cn) => {
                if cn.imag.is_zero() {
                    return Self::equal(ctx, cn.real, rhs);
                }

                match rhs {
                    Number::Complex(cn2) => {
                        Self::equal(ctx, cn.real, cn2.real) && Self::equal(ctx, cn.imag, cn2.imag)
                    }
                    Number::Fixnum(_)
                    | Number::Flonum(_)
                    | Number::BigInt(_)
                    | Number::Rational(_) => false,
                }
            }
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Number::Flonum(f) => f.is_nan(),
            _ => false,
        }
    }

    pub fn compare(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Option<std::cmp::Ordering> {
        if lhs.is_nan() || rhs.is_nan() {
            return None;
        }
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    let n = lhs.wrapping_sub(rhs);
                    if n == 0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if n > 0 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }
                Number::Flonum(rhs) => {
                    if rhs.is_nan() {
                        return None;
                    }
                    let d = lhs as f64 - rhs;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }
                Number::BigInt(rhs) => Some(if rhs.is_negative() {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }),
                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    Self::compare(ctx, Self::mul(ctx, Number::Fixnum(lhs), deno), nume)
                }
                Number::Complex(cn) => {
                    if cn.imag.is_zero() {
                        return Self::compare(ctx, Number::Fixnum(lhs), cn.real);
                    }
                    None
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    let d = lhs - rhs as f64;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }
                Number::Flonum(rhs) => {
                    if lhs.is_infinite() && rhs.is_infinite() {
                        if lhs.is_sign_positive() == rhs.is_sign_positive() {
                            return Some(std::cmp::Ordering::Equal);
                        } else if lhs.is_sign_positive() {
                            return Some(std::cmp::Ordering::Greater);
                        } else {
                            return Some(std::cmp::Ordering::Less);
                        }
                    }

                    let d = lhs - rhs;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }
                Number::BigInt(rhs) => {
                    if Number::Flonum(lhs).is_integer() {
                        return Self::compare(
                            ctx,
                            Self::to_exact(Number::Flonum(lhs), ctx),
                            Number::BigInt(rhs),
                        );
                    }

                    if lhs > rhs.as_f64() {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }
                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    Self::compare(ctx, Self::mul(ctx, Number::Flonum(lhs), deno), nume)
                }
                Number::Complex(cn) => {
                    if cn.imag.is_zero() {
                        return Self::compare(ctx, Number::Flonum(lhs), cn.real);
                    }
                    None
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if lhs.is_negative() {
                        Some(std::cmp::Ordering::Less)
                    } else {
                        Some(std::cmp::Ordering::Greater)
                    }
                }

                Number::Flonum(rhs) => {
                    if Number::Flonum(rhs).is_integer() {
                        return Self::compare(
                            ctx,
                            Number::BigInt(lhs),
                            Self::to_exact(Number::Flonum(rhs), ctx),
                        );
                    }

                    if lhs.as_f64() > rhs {
                        Some(std::cmp::Ordering::Greater)
                    } else {
                        Some(std::cmp::Ordering::Less)
                    }
                }

                Number::BigInt(rhs) => Some(lhs.cmp(&rhs)),

                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    Self::compare(ctx, Self::mul(ctx, Self::BigInt(lhs), deno), nume)
                }

                Number::Complex(rhs) => {
                    if rhs.imag.is_zero() {
                        return Self::compare(ctx, Number::BigInt(lhs), rhs.real);
                    }
                    None
                }
            },

            Number::Rational(lhs) => {
                let nume = lhs.numerator;
                let deno = lhs.denominator;

                match rhs {
                    Number::Fixnum(rhs) => {
                        Self::compare(ctx, nume, Self::mul(ctx, Number::Fixnum(rhs), deno))
                    }

                    Number::Flonum(rhs) => {
                        let d = lhs.to_f64(ctx) - rhs;
                        if d == 0.0 {
                            return Some(std::cmp::Ordering::Equal);
                        }

                        if d > 0.0 {
                            Some(std::cmp::Ordering::Greater)
                        } else {
                            Some(std::cmp::Ordering::Less)
                        }
                    }

                    Number::BigInt(rhs) => {
                        Self::compare(ctx, nume, Self::mul(ctx, Number::BigInt(rhs), deno))
                    }

                    Number::Rational(rhs) => {
                        let nume2 = rhs.numerator;
                        let deno2 = rhs.denominator;

                        Self::compare(
                            ctx,
                            Self::mul(ctx, nume, deno2),
                            Self::mul(ctx, nume2, deno),
                        )
                    }

                    Number::Complex(cn) => {
                        if cn.imag.is_zero() {
                            return Self::compare(ctx, nume, Self::mul(ctx, cn.real, deno));
                        }
                        None
                    }
                }
            }

            Number::Complex(lhs) => {
                if lhs.imag.is_zero() {
                    return Self::compare(ctx, lhs.real, rhs);
                }

                None
            }
        }
    }

    pub fn from_i64(ctx: Context<'gc>, n: i64) -> Self {
        if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n))
    }

    pub fn from_u64(ctx: Context<'gc>, n: u64) -> Self {
        if n <= i32::MAX as u64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n))
    }

    pub fn from_u128(ctx: Context<'gc>, n: u128) -> Self {
        if n <= i32::MAX as u128 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u128(ctx, n))
    }

    pub fn from_u32(ctx: Context<'gc>, n: u32) -> Self {
        if n <= i32::MAX as u32 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n as _))
    }

    pub fn from_i32(n: i32) -> Self {
        Self::Fixnum(n)
    }

    pub fn from_u8(n: u8) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_i8(n: i8) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_u16(n: u16) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_i16(n: i16) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_usize(ctx: Context<'gc>, n: usize) -> Self {
        if n <= i32::MAX as usize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n as _))
    }

    pub fn from_isize(ctx: Context<'gc>, n: isize) -> Self {
        if n >= i32::MIN as isize && n <= i32::MAX as isize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n as _))
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::Flonum(n) => *n > 0.0,
            Self::BigInt(n) => n.is_positive(),
            Self::Rational(rn) => rn.numerator.is_positive(),
            Self::Complex(c) => c.real.is_positive(),
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(_) => false,
            Self::Complex(c) => c.real.is_exact() && c.imag.is_exact(),
        }
    }

    pub fn is_real_valued(&self) -> bool {
        if self.is_real() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero(),
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::Flonum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Complex(c) => false,
        }
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(n) => n.is_finite(),
            Self::Complex(c) => c.real.is_finite() && c.imag.is_finite(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        match self {
            Self::Flonum(n) => n.is_infinite(),
            Self::Complex(c) => c.real.is_infinite() || c.imag.is_infinite(),
            _ => false,
        }
    }

    pub fn is_exact_integer(&self) -> bool {
        matches!(self, Self::Fixnum(_) | Self::BigInt(_))
    }

    pub fn is_exact_positive_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::BigInt(n) => n.is_positive() && !n.is_zero(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Rational(_) => false,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }

                fl.round() == *fl
            }
            _ => false,
        }
    }

    pub fn is_integer_valued(&self) -> bool {
        if self.is_integer() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_integer_valued(),
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Self::Rational(_) | Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }
                true
            }
            _ => false,
        }
    }

    pub fn is_rational_valued(&self) -> bool {
        if self.is_rational() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_rational_valued(),
            _ => false,
        }
    }

    pub fn is_exact_non_negative_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n >= 0,
            Self::BigInt(n) => n.is_positive() || n.is_zero(),
            _ => false,
        }
    }

    pub fn is_even(&self) -> bool {
        match self {
            Self::Fixnum(n) => n & 1 == 0,
            Self::BigInt(n) => n[0] & 1 == 0,
            Self::Flonum(n) => n * 0.5 == (n * 0.5).floor(),

            Self::Complex(n) => n.real.is_even(),
            _ => false,
        }
    }

    pub fn ash(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        if count.is_negative() {
            return self.rsh(ctx, count.negate(ctx));
        }

        self.lsh(ctx, count)
    }

    pub fn lsh(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        if self.is_zero() {
            return Some(self);
        }

        let count = count.coerce_exact_integer_to_usize();

        if count == 0 {
            return Some(self);
        } else if count / i32::BITS as usize >= i32::MAX as usize {
            return None;
        }

        match self {
            Self::Fixnum(n) => {
                let s = n.checked_shl(count as _);
                if let Some(n) = s {
                    return Some(Self::Fixnum(n));
                }

                let bn = BigInt::from_i64(ctx, n as _);
                Some(Self::BigInt(BigInt::shift_left(bn, ctx, count)).normalize_integer())
            }

            Self::BigInt(n) => {
                Some(Self::BigInt(BigInt::shift_left(n, ctx, count)).normalize_integer())
            }

            _ => None,
        }
    }

    pub fn rsh(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        let count = count.coerce_exact_integer_to_usize();
        if count == 0 {
            return Some(self);
        }

        match self {
            Self::Fixnum(n) => {
                let s = n.checked_shr(count as _);
                if let Some(n) = s {
                    return Some(Self::Fixnum(n));
                }

                let bn = BigInt::from_i64(ctx, n as _);
                Some(Self::BigInt(BigInt::shift_right(bn, ctx, count)).normalize_integer())
            }

            Self::BigInt(n) => {
                Some(Self::BigInt(BigInt::shift_right(n, ctx, count)).normalize_integer())
            }

            _ => None,
        }
    }

    pub fn logior(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs | rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::or(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::or(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => Self::BigInt(BigInt::or(lhs, ctx, rhs)),

            _ => unreachable!(),
        }
    }

    pub fn logand(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs & rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::and(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::and(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => Self::BigInt(BigInt::and(lhs, ctx, rhs)),

            _ => unreachable!(),
        }
    }

    pub fn logxor(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs ^ rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::xor(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::xor(lhs, ctx, rhs))
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => Self::BigInt(BigInt::xor(lhs, ctx, rhs)),

            _ => unreachable!(),
        }
    }

    pub fn lognot(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(n) => {
                let bn = BigInt::from_i64(ctx, n as _);
                Self::BigInt(BigInt::not(bn, ctx))
            }

            Self::BigInt(n) => Self::BigInt(BigInt::not(n, ctx)),

            _ => unreachable!(),
        }
    }

    pub fn logtest(self, ctx: Context<'gc>, other: Self) -> bool {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => (lhs & rhs) != 0,
            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::BigInt(rhs)) => !BigInt::and(lhs, ctx, rhs).is_zero(),
            _ => unreachable!(),
        }
    }

    pub fn logbit(self, ctx: Context<'gc>, n: Self) -> bool {
        let n = n.coerce_exact_integer_to_usize();
        if n / i32::BITS as usize >= i32::MAX as usize {
            return false;
        }

        match self {
            Self::Fixnum(num) => {
                if n < i32::BITS as usize {
                    return (num & (1 << n)) != 0;
                }

                let bn = BigInt::from_i64(ctx, num as _);
                bn.is_bit_set(n)
            }

            Self::BigInt(bi) => {
                if n < Digit::BITS as usize {
                    return (bi[0] & (1 << n)) != 0;
                }

                bi.is_bit_set(n)
            }

            _ => false,
        }
    }

    pub fn abs(self, ctx: Context<'gc>) -> Self {
        if self.is_positive() {
            return self;
        }

        self.negate(ctx)
    }

    pub fn rectangular(self, ctx: Context<'gc>, rhs: Self) -> Self {
        Self::normalize_complex(ctx, self, rhs)
    }

    pub fn polar(self, ctx: Context<'gc>, rhs: Self) -> Self {
        if rhs.is_zero() {
            return self;
        }

        let r = self.real_to_f64(ctx);
        let a = rhs.real_to_f64(ctx);
        Self::Complex(Complex::new(
            ctx,
            Self::Flonum(r * libm::cos(a)),
            Self::Flonum(r * libm::sin(a)),
        ))
    }

    pub fn angle(self, ctx: Context<'gc>) -> Self {
        if let Self::Complex(c) = self {
            let real = c.real.real_to_f64(ctx);
            let imag = c.imag.real_to_f64(ctx);
            return Self::Flonum(libm::atan2(imag, real));
        }

        if self.is_negative() {
            return Self::Flonum(std::f64::consts::PI);
        }

        if let Self::Flonum(n) = self {
            return Self::Flonum(0.0);
        }

        Self::Fixnum(0)
    }
}

impl<'gc> Rational<'gc> {
    pub fn add(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::add(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn mul(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.numerator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn sub(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::sub(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn div(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.denominator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.numerator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn to_f64(&self, ctx: Context<'gc>) -> f64 {
        let mut nume = self.numerator.real_to_f64(ctx);
        let mut deno = self.denominator.real_to_f64(ctx);

        if nume.is_infinite() || deno.is_infinite() {
            if nume.is_infinite() && deno.is_infinite() {
                let nume_bitsize = BigInt::bitsize(self.numerator.as_bigint().unwrap());
                let deno_bitsize = BigInt::bitsize(self.denominator.as_bigint().unwrap());
                let mut shift = if nume_bitsize > deno_bitsize {
                    nume_bitsize as isize - 96
                } else {
                    deno_bitsize as isize - 96
                };

                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(self.numerator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
                deno = BigInt::shift_right(self.denominator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
            } else if deno.is_infinite() {
                let deno_bitsize = BigInt::bitsize(self.denominator.as_bigint().unwrap());
                let mut shift = deno_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = libm::ldexp(nume, -(shift as i32));
                deno = BigInt::shift_right(self.denominator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
            } else {
                let nume_bitsize = BigInt::bitsize(self.numerator.as_bigint().unwrap());
                let mut shift = nume_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(self.numerator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
                deno = libm::ldexp(deno, -(shift as i32));
            }
        }

        nume / deno
    }
}

pub trait IntoNumber<'gc> {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc>;
}

impl<'gc> IntoNumber<'gc> for i32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self)
    }
}

impl<'gc> IntoNumber<'gc> for i64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self >= i32::MIN as i64 && self <= i32::MAX as i64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u32 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self as u64))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for f64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Flonum(self)
    }
}

impl<'gc> IntoNumber<'gc> for f32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Flonum(self as f64)
    }
}

impl<'gc> IntoNumber<'gc> for u16 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for u8 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i16 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i8 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for usize {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as usize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u128(ctx, self as u128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for isize {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self >= i32::MIN as isize && self <= i32::MAX as isize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i128(ctx, self as i128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u128 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_u128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for i128 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_i128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, BigInt<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        if let Some(i) = self
            .try_as_i64()
            .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
        {
            Number::Fixnum(i as i32)
        } else {
            Number::BigInt(self)
        }
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Rational<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        Number::Rational(self)
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Complex<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        Number::Complex(self)
    }
}

impl<'gc> fmt::Display for Number<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Fixnum(i) => write!(f, "{}", i),
            Number::Flonum(fl) => write!(f, "{}", fl),
            Number::BigInt(b) => write!(f, "{}", b),
            Number::Rational(r) => write!(f, "{}", r),
            Number::Complex(c) => write!(f, "{}", c),
        }
    }
}

impl<'gc> fmt::Display for Rational<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl<'gc> fmt::Display for Complex<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let real = self.real.to_string();
        let imag = self.imag.to_string();

        let need_plus = !imag.starts_with('+') && !imag.starts_with("-");

        let mut out = String::new();
        out.push_str(&real);
        if need_plus {
            out.push('+');
        }
        out.push_str(&imag);
        out.push('i');

        write!(f, "{}", out)
    }
}

/// Decodes a f64 into (mantissa, exponent, sign).
/// Returns (mantissa: u64, exponent: i32, sign: i32)
pub const fn decode_double(n: f64) -> (i64, i32, i32) {
    let bits = n.to_bits();
    let mant_bits = bits as i64 & (IEXPT_2N52 - 1);
    let sign_bits = (bits >> 63) as i32;
    let exp_bits = ((bits >> 52) & 0x7ff) as i32;

    if n == 0.0 {
        return (0, 0, if sign_bits != 0 { -1 } else { 1 });
    }
    if n.is_nan() {
        return (0x18000000000000, 972, 1);
    }
    if n.is_infinite() {
        return (0x10000000000000, 972, if sign_bits != 0 { -1 } else { 1 });
    }
    debug_assert!(exp_bits != 0x7ff);

    let exp = if exp_bits != 0 {
        exp_bits - 1023
    } else {
        -1022
    } - 52;
    let sign = if sign_bits != 0 { -1 } else { 1 };
    let mant = if exp_bits != 0 {
        mant_bits | (1 << 52)
    } else {
        mant_bits
    };
    (mant, exp, sign)
}

pub fn nextfloat(z: f64) -> f64 {
    let (m, k, sign) = decode_double(z);

    assert!(sign >= 0);

    if m == IEXPT_2N53 - 1 {
        return libm::ldexp(IEXPT_2N52 as f64, k + 1);
    }

    libm::ldexp((m + 1) as f64, k)
}

pub fn prevfloat(z: f64) -> f64 {
    let (m, k, sign) = decode_double(z);

    assert!(sign >= 0);

    if m == IEXPT_2N52 {
        return libm::ldexp((IEXPT_2N53 - 1) as f64, k - 1);
    }

    libm::ldexp((m - 1) as f64, k)
}

pub fn parse_ubignum<'gc>(
    ctx: Context<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Gc<'gc, BigInt<'gc>> {
    if s.peek().is_none() {
        return BigInt::zero(ctx);
    }
    let mut ans = BigInt::zero(ctx);

    while let Some(&c) = s.peek() {
        if c == '#' {
            return ans;
        }
        let digit;
        if c.is_ascii_digit() {
            digit = (c as u8) - b'0';
        } else if c >= 'a' {
            digit = (c as u8) - b'a' + 10;
        } else if c >= 'A' {
            digit = (c as u8) - b'A' + 10;
        } else {
            break;
        }

        if digit < radix as u8 {
            ans = BigInt::times(ans, ctx, BigInt::from_u64(ctx, radix as _));
            ans = BigInt::plus(ans, ctx, BigInt::from_u64(ctx, digit as u64));
            s.next();
            continue;
        }
        break;
    }

    ans
}

pub fn parse_uinteger<'gc>(
    ctx: Context<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Option<Number<'gc>> {
    s.peek()?;

    let mut ans = Number::Fixnum(0);
    while let Some(&c) = s.peek() {
        if c == '#' {
            return Some(ans);
        }
        let digit;
        if c.is_ascii_digit() {
            digit = (c as u8) - b'0';
        } else if c >= 'a' {
            digit = (c as u8) - b'a' + 10;
        } else if c >= 'A' {
            digit = (c as u8) - b'A' + 10;
        } else {
            return None;
        }

        if digit < radix as u8 {
            ans = Number::mul(ctx, ans, Number::from_u64(ctx, radix as _));
            ans = Number::add(ctx, ans, Number::from_u64(ctx, digit as u64));
            s.next();
            continue;
        }
        return None;
    }
    Some(ans)
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

#[derive(Debug, Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum ExactInteger<'gc> {
    Fixnum(i32),
    BigInt(Gc<'gc, BigInt<'gc>>),
}

impl<'gc> ExactInteger<'gc> {
    pub fn to_number(self, ctx: Context<'gc>) -> Number<'gc> {
        match self {
            Self::Fixnum(n) => Number::Fixnum(n),
            Self::BigInt(b) => Number::BigInt(b),
        }
    }

    pub fn bitor(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l | r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i32(ctx, l);
                    let res = BigInt::or(lhs, ctx, r);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i32(ctx, r);
                    let res = BigInt::or(l, ctx, rhs);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::or(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitand(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l & r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i32(ctx, l);
                    let res = BigInt::and(lhs, ctx, r);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i32(ctx, r);
                    let res = BigInt::and(l, ctx, rhs);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::and(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitxor(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l ^ r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i64(ctx, l as _);
                    let res = BigInt::xor(lhs, ctx, r);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i64(ctx, r as _);
                    let res = BigInt::xor(l, ctx, rhs);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::xor(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitnot(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Self::Fixnum(n) => Self::Fixnum(!n),
            Self::BigInt(b) => {
                let res = BigInt::not(b, ctx);
                Self::BigInt(res)
            }
        }
    }

    pub fn is_bit_set(ctx: Context<'gc>, n: Self, bit: u32) -> bool {
        match n {
            Self::Fixnum(n) => {
                if bit >= 32 {
                    return false;
                }

                (n & (1 << bit)) != 0
            }
            Self::BigInt(b) => b.is_bit_set(bit as usize),
        }
    }

    pub fn first_bit_set(ctx: Context<'gc>, n: Self) -> i32 {
        match n {
            Self::Fixnum(n) => {
                if n == 0 {
                    return -1;
                }

                n.trailing_zeros() as i32
            }
            Self::BigInt(b) => {
                if b.is_zero() {
                    return -1;
                }

                b.first_bit_set()
            }
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n < 0,
            Self::BigInt(b) => b.is_negative(),
        }
    }

    pub fn bit_count(self, ctx: Context<'gc>) -> i32 {
        match self {
            Self::Fixnum(n) => {
                if n == 0 {
                    0
                } else if n > 0 {
                    nbits32(n as u32) as i32
                } else {
                    !(nbits32((!n) as u32) as i32)
                }
            }
            Self::BigInt(b) => {
                if b.is_zero() {
                    0
                } else if b.is_positive() {
                    b.count_ones() as i32
                } else {
                    !Self::bit_count(Self::BigInt(BigInt::not(b, ctx)), ctx)
                }
            }
        }
    }

    pub fn bit_length(&self, ctx: Context<'gc>) -> u32 {
        match self {
            Self::Fixnum(n) => {
                if *n == 0 {
                    return 0;
                }
                let n2 = if *n < 0 { !*n } else { *n };
                32 - n2.leading_zeros()
            }
            Self::BigInt(b) => {
                if b.is_positive() {
                    return BigInt::bitsize(*b) as _;
                }

                let nb = BigInt::not(*b, ctx);
                BigInt::bitsize(nb) as _
            }
        }
    }
}

fn i32_to_raidx(n: i32, radix: u8) -> String {
    /*if n == 0 {
        return "0".to_string();
    }
    // Use a character map for digits.
    const CHARS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";

    // Handle negative numbers.
    let is_negative = n < 0;
    // Work with the absolute value as a u32.
    // This is safe because i32::MIN.abs() fits in u32.
    let mut num = if is_negative {
        // Safely get the absolute value
        n.unsigned_abs()
    } else {
        n as u32
    };

    let mut result = String::new();

    while num > 0 {
        let remainder = (num % radix as u32) as usize;
        result.push(CHARS[remainder] as char);
        num /= radix as u32;
    }

    // The result string is built in reverse, so we need to reverse it.
    let mut final_string: String = result.chars().rev().collect();

    // Prepend the negative sign if necessary.
    if is_negative {
        final_string.insert(0, '-');
    }

    final_string*/

    match radix {
        2 => format!("{:b}", n),
        8 => format!("{:o}", n),
        10 => format!("{}", n),
        16 => format!("{:x}", n),
        _ => format!("{}", n),
    }
}

impl<'gc> Number<'gc> {
    pub fn integer_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        if !self.is_exact() {
            return format!("#i{}", self.to_exact(ctx).integer_to_string(ctx, radix));
        }
        if let Number::BigInt(n) = self {
            let opts = NumberToStringOptions {
                base: match radix {
                    2 => &Base::BIN,
                    8 => &Base::OCT,
                    10 => &Base::DEC,
                    16 => &Base::HEX,
                    _ => &Base::DEC,
                },
                force_sign: false,
                group_sep: None,
                group_size: 0,
                minus_sign: "-".into(),
                plus_sign: "+".into(),
            };

            return n.to_string_with_options(&opts);
        };

        let Number::Fixnum(n) = self else {
            unreachable!()
        };

        if n < 0 {
            format!("-{}", i32_to_raidx(-n, radix))
        } else {
            i32_to_raidx(n, radix)
        }
    }

    pub fn flonum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let n = self.real_to_f64(ctx);

        if n.is_nan() {
            return "+nan.0".to_string();
        } else if n.is_infinite() {
            if n.is_sign_negative() {
                return "-inf.0".to_string();
            } else {
                return "+inf.0".to_string();
            }
        }

        if radix == 10 {
            if n == 0.0 {
                if n.is_sign_negative() {
                    return "-0.0".to_string();
                } else {
                    return "+0.0".to_string();
                }
            } else {
                return format!("{n:?}");
            }
        }

        if n == 0.0 {
            return "#i0".to_string();
        }

        let p = self.numerator(ctx).to_exact(ctx).abs(ctx);
        let q = self.denominator(ctx).to_exact(ctx);
        let mut buf = "#i".to_string();
        if self.is_negative() {
            buf.push('-');
        }

        buf.push_str(&p.to_string_radix(ctx, radix));
        if !matches!(q, Number::Fixnum(1)) {
            buf.push('/');
            buf.push_str(&q.to_string_radix(ctx, radix));
        }

        buf
    }

    pub fn compnum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let c = match self {
            Self::Complex(c) => c,
            _ => unreachable!(),
        };

        if radix != 10 {
            format!(
                "#i{}",
                Self::make_rectangular(ctx, c.real.to_exact(ctx), c.imag.to_exact(ctx))
                    .to_string_radix(ctx, 10)
            )
        } else {
            let rr = c.real.to_string_radix(ctx, radix);
            let ii = c.imag.to_string_radix(ctx, radix);

            if ii.starts_with('+') || ii.starts_with('-') {
                format!("{rr}{ii}i")
            } else {
                format!("{rr}+{ii}i")
            }
        }
    }

    pub fn ratnum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let r = match self {
            Self::Rational(r) => r,
            _ => unreachable!(),
        };

        let nume = r.numerator.to_string_radix(ctx, radix);
        let deno = r.denominator.to_string_radix(ctx, radix);

        format!("{}/{}", nume, deno)
    }

    pub fn make_rectangular(ctx: Context<'gc>, real: Self, imag: Self) -> Self {
        let real = if let Self::Complex(c) = real {
            c.real
        } else {
            real
        };

        let imag = if let Self::Complex(c) = imag {
            c.imag
        } else {
            imag
        };

        Self::Complex(Complex::new(ctx, real, imag))
    }

    pub fn numerator(self, ctx: Context<'gc>) -> Self {
        let inexact = !self.is_exact();
        let obj = self.to_exact(ctx);
        match obj {
            Self::Rational(r) => {
                if inexact {
                    r.numerator.to_inexact(ctx)
                } else {
                    r.numerator
                }
            }
            _ => {
                if inexact {
                    obj.to_inexact(ctx)
                } else {
                    obj
                }
            }
        }
    }

    pub fn denominator(self, ctx: Context<'gc>) -> Self {
        let inexact = !self.is_exact();
        let obj = self.to_exact(ctx);
        match obj {
            Self::Rational(r) => {
                if inexact {
                    r.denominator.to_inexact(ctx)
                } else {
                    r.denominator
                }
            }
            _ => {
                if inexact {
                    Number::Flonum(1.0)
                } else {
                    Number::Fixnum(1)
                }
            }
        }
    }

    pub fn to_string_radix(self, ctx: Context<'gc>, base: u8) -> String {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self.integer_to_string(ctx, base),
            Self::Flonum(_) => self.flonum_to_string(ctx, base),
            Self::Rational(_) => self.ratnum_to_string(ctx, base),
            Self::Complex(_) => self.compnum_to_string(ctx, base),
        }
    }
}

impl<'gc> FromValue<'gc> for ExactInteger<'gc> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            return Ok(Self::Fixnum(value.as_int32()));
        }

        if value.is::<BigInt>() {
            return Ok(Self::BigInt(value.downcast()));
        }

        Err(ConversionError::TypeMismatch {
            pos: 0,
            expected: "exact integer",
            found: value,
        })
    }
}

impl<'gc> From<ExactInteger<'gc>> for Value<'gc> {
    fn from(value: ExactInteger<'gc>) -> Self {
        match value {
            ExactInteger::Fixnum(n) => Value::new(n),
            ExactInteger::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Value::new(v as i32))
                .unwrap_or_else(|| b.into()),
        }
    }
}

impl<'gc> From<ExactInteger<'gc>> for Number<'gc> {
    fn from(value: ExactInteger<'gc>) -> Self {
        match value {
            ExactInteger::Fixnum(n) => Number::Fixnum(n),
            ExactInteger::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Number::Fixnum(v as i32))
                .unwrap_or_else(|| Number::BigInt(b)),
        }
    }
}

impl<'gc> IntoValue<'gc> for ExactInteger<'gc> {
    fn into_value(self, _ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Self::Fixnum(n) => Value::new(n),
            Self::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Value::new(v as i32))
                .unwrap_or_else(|| b.into()),
        }
    }
}

const fn nbits32(mut x: u32) -> u32 {
    x = x - ((x >> 1) & 0x55555555);
    let t = (x >> 2) & 0x33333333;
    x = (x & 0x33333333) + t;
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x = x * 0x01010101;
    x >> 24
}
