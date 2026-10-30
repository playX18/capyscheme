use std::cmp::Ordering;

use crate::prelude::*;
use crate::runtime::{MutationContext, prelude::*};
use crate::{global, list, vector};

pub const fn decode_flonum(x: f64) -> (i64, i32, i32) {
    let bits = x.to_bits();

    let mant_bits = bits & 0x000F_FFFF_FFFF_FFFF;
    let sign_bits = (bits >> 63) & 0x1;
    let exp_bits = (bits >> 52) & 0x7FF;
    let exp;
    let sign;
    let mantissa = if x == 0.0 {
        exp = 0;
        sign = if sign_bits != 0 { -1 } else { 0 };
        0
    } else if x.is_nan() {
        exp = 972;
        sign = 1;
        0x18000000000000
    } else if x.is_infinite() {
        exp = 972;
        sign = if sign_bits != 0 { -1 } else { 1 };
        0x18000000000000
    } else {
        exp = if exp_bits != 0 {
            exp_bits.wrapping_sub(1023) as i32
        } else {
            -1022
        }
        .wrapping_sub(52);
        sign = if sign_bits != 0 { -1 } else { 1 };
        mant_bits as i64
    };
    let mantissa = if exp_bits != 0 {
        mantissa | 0x0010_0000_0000_0000
    } else {
        mantissa
    };

    (mantissa, exp, sign)
}

pub const fn make_flonum(s: i32, m: i64, e: i32) -> f64 {
    let iexpt_2n52 = 4503599627370496i64;
    const IEXPT_2N63: i64 = 9223372036854775808u64 as i64;

    let t =
        (if s == 0 { 0 } else { IEXPT_2N63 }) + ((e as i64 + 1023) * iexpt_2n52) + (m % iexpt_2n52);
    let u = t as u64;
    f64::from_bits(u)
}

#[scheme(path = capy)]
mod arith_operations {
    #[scheme(name = "compnum?")]
    pub fn is_complex_number(x: Number<'gc>) -> bool {
        matches!(x, Number::Complex(_))
    }

    #[scheme(name = "ratnum?")]
    pub fn is_rat_number(x: Number<'gc>) -> bool {
        matches!(x, Number::Rational(_))
    }

    #[scheme(name = "bignum?")]
    pub fn is_bignum(x: Number<'gc>) -> bool {
        matches!(x, Number::BigInt(_))
    }

    #[scheme(name = "bignum->string")]
    pub fn bignum_to_str(x: Gc<'gc, BigInt<'gc>>, radix: u8) -> Value<'gc> {
        let base = match radix {
            2 => &Base::BIN,
            8 => &Base::OCT,
            10 => &Base::DEC,
            16 => &Base::HEX,
            _ => {
                let x_val = x.into_value(ctx);
                return call.wrong_argument_violation(
                    "bignum->string",
                    "radix must be one of 2, 8, 10, or 16",
                    Some(x_val),
                    Some(2),
                    2,
                    &[x_val],
                );
            }
        };
        let x = x.to_string_with_options(&NumberToStringOptions {
            base,
            group_sep: None,
            group_size: 0,
            force_sign: false,
            plus_sign: "+".into(),
            minus_sign: "-".into(),
        });

        ctx.str(&x)
    }

    /// Return the sign of flonum as 0 (positive) or 1 (negative)
    #[scheme(name = "float-sign")]
    pub fn float_sign(x: f64) -> i32 {
        let sign = if x.is_sign_negative() { 1 } else { 0 };
        sign
    }

    /// Return the unbiased exponent of a flonum as a fixnum.
    ///
    /// The returned value is the "natural" exponent in the sense that if
    /// F is a nonnegative flonum, then
    ///        F = (float-significand F) * (expt 2 (float-exponent F))
    #[scheme(name = "float-exponent")]
    pub fn float_exponent(x: f64) -> i32 {
        let (_mantissa, exp, _sign) = super::decode_flonum(x);
        exp
    }

    #[scheme(name = "float-significand")]
    pub fn float_significand(x: f64) -> i64 {
        let (mantissa, _exp, _sign) = super::decode_flonum(x);
        mantissa
    }

    #[scheme(name = "number->string")]
    pub fn number2string(x: Number<'gc>, radix: Option<u8>, _precision: Option<u8>) -> Value<'gc> {
        let radix = radix.unwrap_or(10);
        if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
            let x_val = x.into_value(ctx);
            return call.wrong_argument_violation(
                "number->string",
                "radix must be one of 2, 8, 10, or 16",
                Some(x_val),
                Some(2),
                2,
                &[x_val],
            );
        }

        // Fixnums in base 10 (hash keys, counters, ids): format directly
        // into a stack buffer, skipping the formatting machinery and the
        // intermediate string.
        if radix == 10
            && let Number::Fixnum(n) = x
        {
            let mut digits = [0u8; 11];
            let negative = n < 0;
            let mut rest = (n as i64).unsigned_abs() as u32;
            let mut end = digits.len();
            loop {
                end -= 1;
                digits[end] = b'0' + (rest % 10) as u8;
                rest /= 10;
                if rest == 0 {
                    break;
                }
            }
            if negative {
                end -= 1;
                digits[end] = b'-';
            }
            let text = std::str::from_utf8(&digits[end..]).unwrap();
            let s = ctx.str(text);
            return (s.downcast::<Str>()).into();
        }

        let s = x.to_string_radix(ctx, radix);
        let s = ctx.str(&s);
        (s.downcast::<Str>()).into()
    }

    #[scheme(name = "bits->flonum")]
    pub fn bits_to_flonum(s: u64) -> f64 {
        let f = f64::from_bits(s);
        f
    }

    #[scheme(name = "flonum->bits")]
    pub fn flonum_to_bits(x: f64) -> u64 {
        let bits = x.to_bits();
        bits
    }

    #[scheme(name = "abs")]
    pub fn abs(x: Number<'gc>) -> Number<'gc> {
        let res = x.abs(ctx);
        res
    }

    #[scheme(name = "nan?")]
    pub fn is_nan(x: Number<'gc>) -> bool {
        x.is_nan()
    }

    #[scheme(name = "infinite?")]
    pub fn is_infinite(x: Number<'gc>) -> bool {
        x.is_infinite()
    }

    #[scheme(name = "finite?")]
    pub fn is_finite(x: Number<'gc>) -> bool {
        x.is_finite()
    }

    #[scheme(name = "exact?")]
    pub fn is_exact(x: Number<'gc>) -> bool {
        x.is_exact()
    }

    #[scheme(name = "inexact?")]
    pub fn is_inexact(x: Number<'gc>) -> bool {
        !x.is_exact()
    }

    #[scheme(name = "odd?")]
    pub fn is_odd(x: Number<'gc>) -> bool {
        let is_odd = match x {
            Number::Fixnum(i) => i % 2 != 0,
            Number::BigInt(b) => b.is_odd(),
            _ => x.real_to_f64(ctx) % 2.0 != 0.0,
        };
        is_odd
    }

    #[scheme(name = "even?")]
    pub fn is_even(x: Number<'gc>) -> bool {
        let is_even = match x {
            Number::Fixnum(i) => i % 2 == 0,
            Number::BigInt(b) => !b.is_odd(),
            _ => x.real_to_f64(ctx) % 2.0 == 0.0,
        };
        is_even
    }

    #[scheme(name = "quotient")]
    pub fn quotient(x: Number<'gc>, y: Number<'gc>) -> Value<'gc> {
        if y.is_zero() && y.is_exact() {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "quotient",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        let result = Number::quotient(ctx, x, y);

        (result).into_value(ctx)
    }

    #[scheme(name = "remainder")]
    pub fn remainder(x: Number<'gc>, y: Number<'gc>) -> Value<'gc> {
        if y.is_zero() && y.is_exact() {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "remainder",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        let result = Number::remainder(ctx, x, y);

        (result).into_value(ctx)
    }

    #[scheme(name = "modulo")]
    pub fn modulo(x: Number<'gc>, y: Number<'gc>) -> Value<'gc> {
        if y.is_zero() && y.is_exact() {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "modulo",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        if !x.is_integer_valued() {
            let x_val = x.into_value(ctx);
            return call.wrong_argument_violation(
                "modulo",
                "first argument must be an integer-valued number",
                Some(x_val),
                Some(1),
                2,
                &[x_val],
            );
        }

        if !y.is_integer_valued() {
            let y_val = y.into_value(ctx);
            return call.wrong_argument_violation(
                "modulo",
                "second argument must be an integer-valued number",
                Some(y_val),
                Some(2),
                2,
                &[y_val],
            );
        }

        let result = Number::modulo(ctx, x, y);

        (result).into_value(ctx)
    }

    #[scheme(name = "-")]
    pub fn minus(x: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            let negated = x.negate(ctx);
            return Ok(negated);
        }

        let mut acc = x;
        for (position, arg) in (1..).zip(rest.iter()) {
            let Some(arg) = arg.number() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(x.into_value(ctx));
                args.extend_from_slice(rest);
                call.wrong_argument_violation(
                    "-",
                    "argument must be a number",
                    Some(*arg),
                    Some(position),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            };
            acc = Number::sub(ctx, acc, arg);
        }

        Ok(acc)
    }

    #[scheme(name = "inexact")]
    pub fn inexact(x: Number<'gc>) -> Number<'gc> {
        let n = x.to_inexact(ctx);

        n
    }

    #[scheme(name = "exact")]
    pub fn exact(x: Number<'gc>) -> Number<'gc> {
        let n = x.to_exact(ctx);

        n
    }

    #[scheme(name = "+")]
    pub fn plus(args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.is_empty() {
            return Ok(Number::Fixnum(0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                call.wrong_argument_violation(
                    "+",
                    "argument must be a number",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].number() else {
            call.wrong_argument_violation(
                "+",
                "argument must be a number",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                call.wrong_argument_violation(
                    "+",
                    "argument must be a number",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            acc = Number::add(ctx, acc, arg);
        }

        Ok(acc)
    }

    #[scheme(name = "*")]
    pub fn times(args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.is_empty() {
            return Ok(Number::Fixnum(1));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                call.wrong_argument_violation(
                    "*",
                    "argument must be a number",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].number() else {
            call.wrong_argument_violation(
                "*",
                "argument must be a number",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                call.wrong_argument_violation(
                    "*",
                    "argument must be a number",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            acc = Number::mul(ctx, acc, arg);
        }

        Ok(acc)
    }

    #[scheme(name = "/")]
    pub fn div(acc: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            if acc.is_zero() && acc.is_exact() {
                let acc = acc.into_value(ctx);
                call.wrong_argument_violation(
                    "/",
                    "division by zero",
                    Some(acc),
                    Some(1),
                    1,
                    &[acc],
                );
                return Err(Value::undefined());
            }
            let one = Number::Fixnum(1);
            let res = Number::div(ctx, one, acc);
            return Ok(res);
        }

        let mut acc = acc;

        for arg in rest.iter() {
            let Some(arg) = arg.number() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(ctx));
                args.extend_from_slice(rest);
                call.wrong_argument_violation(
                    "/",
                    "argument must be a number",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            };

            if arg.is_zero() && arg.is_exact() && acc.is_exact() {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(ctx));
                args.extend_from_slice(rest);
                let arg = arg.into_value(ctx);
                call.wrong_argument_violation(
                    "/",
                    "division by zero",
                    Some(arg),
                    Some(1),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            }
            acc = Number::div(ctx, acc, arg);
        }

        Ok(acc)
    }

    #[scheme(name = "div")]
    pub fn integer_div(x: Number<'gc>, y: Number<'gc>) -> Value<'gc> {
        if !x.is_real() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "div",
                "first argument must be a real number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if x.is_infinite() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "div",
                "first argument must be a finite number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !y.is_real() {
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "div",
                "second argument must be a real number",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }

        if y.is_zero() && y.is_exact() {
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "div",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }
        let res = Number::integer_div(ctx, x, y);
        (res).into_value(ctx)
    }

    #[scheme(name = "div0")]
    pub fn integer_div0(x: Number<'gc>, rhs: Number<'gc>) -> Value<'gc> {
        if !x.is_real() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "div0",
                "first argument must be a real number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if x.is_infinite() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "div0",
                "first argument must be a finite number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !rhs.is_real() {
            let rhs = rhs.into_value(ctx);
            return call.wrong_argument_violation(
                "div0",
                "second argument must be a real number",
                Some(rhs),
                Some(2),
                2,
                &[rhs],
            );
        }

        if rhs.is_zero() && rhs.is_exact() {
            let rhs = rhs.into_value(ctx);
            return call.wrong_argument_violation(
                "div0",
                "division by zero",
                Some(rhs),
                Some(2),
                2,
                &[rhs],
            );
        }

        let res = Number::integer_div0(ctx, x, rhs);
        (res).into_value(ctx)
    }

    #[scheme(name = "denominator")]
    pub fn denominator(x: Number<'gc>) -> Value<'gc> {
        if !x.is_real_valued() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "denominator",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        let inexact = !x.is_exact();
        let obj = x.to_exact(ctx);
        if let Number::Rational(rn) = obj {
            let deno = if inexact {
                rn.denominator.to_inexact(ctx)
            } else {
                rn.denominator
            };

            return (deno).into_value(ctx);
        }

        if inexact {
            (Number::Flonum(1.0)).into_value(ctx)
        } else {
            (Number::Fixnum(1)).into_value(ctx)
        }
    }

    #[scheme(name = "numerator")]
    pub fn numerator(x: Number<'gc>) -> Value<'gc> {
        if !x.is_real_valued() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "numerator",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        (x.numerator(ctx)).into_value(ctx)
    }

    #[scheme(name = "expt")]
    pub fn expt(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        let expt = Number::expt(ctx, x, y);

        expt
    }

    #[scheme(name = "exp")]
    pub fn exp(x: Number<'gc>) -> Number<'gc> {
        let exp = Number::exp(ctx, x);
        exp
    }

    #[scheme(name = "log")]
    pub fn log(x: Number<'gc>, y: Option<Number<'gc>>) -> Value<'gc> {
        if x.is_zero() && x.is_exact() {
            return call.wrong_argument_violation(
                "log",
                "logarithm of zero is undefined",
                Some(x.into_value(ctx)),
                Some(1),
                0,
                &[],
            );
        }
        if let Some(y) = y {
            if y.is_zero() && y.is_exact() {
                return call.implementation_restriction_violation(
                    "log",
                    "logarithm with base zero is undefined",
                    &[x.into_value(ctx), y.into_value(ctx)],
                );
            }

            let log = Number::log(ctx, x);
            let log_base = Number::log(ctx, y);
            let result = Number::div(ctx, log, log_base);
            return (result).into_value(ctx);
        }
        let log = Number::log(ctx, x);
        (log).into_value(ctx)
    }

    #[scheme(name = "sin")]
    pub fn sin(x: Number<'gc>) -> Number<'gc> {
        let sin = Number::sin(ctx, x);
        sin
    }

    #[scheme(name = "cos")]
    pub fn cos(x: Number<'gc>) -> Number<'gc> {
        let cos = Number::cos(ctx, x);
        cos
    }

    #[scheme(name = "tan")]
    pub fn tan(x: Number<'gc>) -> Number<'gc> {
        let tan = Number::tan(ctx, x);
        tan
    }

    #[scheme(name = "atan")]
    pub fn atan(_x: Number<'gc>) -> Number<'gc> {
        let atan = Number::atan(ctx, _x);
        atan
    }

    #[scheme(name = "atan2")]
    pub fn atan2(lhs: Number<'gc>, rhs: Number<'gc>) -> Value<'gc> {
        if !lhs.is_real_valued() {
            return call.wrong_argument_violation(
                "atan2",
                "argument must be a real number",
                Some(lhs.into_value(ctx)),
                Some(1),
                1,
                &[lhs.into_value(ctx)],
            );
        }
        if !rhs.is_real_valued() {
            return call.wrong_argument_violation(
                "atan2",
                "argument must be a real number",
                Some(rhs.into_value(ctx)),
                Some(2),
                2,
                &[rhs.into_value(ctx)],
            );
        }
        let atan2 = Number::atan2(ctx, lhs, rhs);
        (atan2).into_value(ctx)
    }

    #[scheme(name = "sqrt")]
    pub fn sqrt(x: Number<'gc>) -> Number<'gc> {
        let sqrt = Number::sqrt(ctx, x);
        sqrt
    }

    #[scheme(name = "magnitude")]
    pub fn magnitude(x: Number<'gc>) -> Number<'gc> {
        let magnitude = Number::magnitude(ctx, x);
        magnitude
    }

    #[scheme(name = "exact-integer-sqrt")]
    pub fn exact_integer_sqrt(x: Number<'gc>) -> Result<(Number<'gc>, Number<'gc>), Value<'gc>> {
        if !x.is_exact_integer() {
            let x = x.into_value(ctx);
            call.wrong_argument_violation(
                "exact-integer-sqrt",
                "argument must be an exact integer",
                Some(x),
                Some(1),
                1,
                &[x],
            );
            return Err(Value::undefined());
        }
        let sqrt = Number::exact_integer_sqrt(ctx, x);
        Ok(sqrt)
    }

    #[scheme(name = "inverse")]
    pub fn inverse(x: Number<'gc>) -> Number<'gc> {
        let inverse = Number::inverse(ctx, x);
        inverse
    }

    #[scheme(name = "asin")]
    pub fn asin(x: Number<'gc>) -> Number<'gc> {
        let asin = Number::asin(ctx, x);
        asin
    }

    #[scheme(name = "acos")]
    pub fn acos(x: Number<'gc>) -> Number<'gc> {
        let acos = Number::acos(ctx, x);
        acos
    }

    #[scheme(name = "floor")]
    pub fn floor(x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !x.is_real() {
            let x = x.into_value(ctx);
            call.wrong_argument_violation(
                "floor",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
            return Err(Value::undefined());
        }
        let floor = x.floor(ctx);
        Ok(floor)
    }

    #[scheme(name = "ceiling")]
    pub fn ceiling(x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero()
        {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(ctx);
            call.wrong_argument_violation(
                "ceiling",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
            return Err(Value::undefined());
        }
        let ceiling = x.ceiling(ctx);
        Ok(ceiling)
    }

    #[scheme(name = "truncate")]
    pub fn truncate(x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero()
        {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(ctx);
            call.wrong_argument_violation(
                "truncate",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
            return Err(Value::undefined());
        }
        let truncate = x.truncate(ctx);
        Ok(truncate)
    }

    #[scheme(name = "round")]
    pub fn round(x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero()
        {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(ctx);
            call.wrong_argument_violation(
                "round",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
            return Err(Value::undefined());
        }
        let round = x.round(ctx);
        Ok(round)
    }

    #[scheme(name = "random")]
    pub fn random(min: Option<ExactInteger<'gc>>, max: Option<ExactInteger<'gc>>) -> Value<'gc> {
        let Some(min) = min else {
            return Value::new(rand::random::<f64>());
        };

        let Some(max) = max else {
            match min {
                ExactInteger::Fixnum(n) => {
                    return Value::new((rand::random::<u32>() % n as u32) as i32);
                }

                ExactInteger::BigInt(b) => {
                    let max_bitwidth = b.bits();
                    let mut rng = rand::rng();
                    let rb = BigInt::random(ctx, &mut rng, max_bitwidth);
                    let rb = BigInt::rem(rb, ctx, b);
                    return rb.into();
                }
            }
        };

        match (min, max) {
            (ExactInteger::Fixnum(min_val), ExactInteger::Fixnum(max_val)) => {
                if min_val >= max_val {
                    let min = min.into_value(ctx);
                    let max = max.into_value(ctx);
                    return call.wrong_argument_violation(
                        "random",
                        "min must be less than max",
                        Some(min),
                        Some(1),
                        2,
                        &[min, max],
                    );
                }
                let range = max_val - min_val;
                let random_val = (rand::random::<u32>() % range as u32) as i32 + min_val;
                return Value::new(random_val);
            }

            (ExactInteger::BigInt(_), ExactInteger::Fixnum(_)) => {
                let min = min.into_value(ctx);
                let max = max.into_value(ctx);
                return call.wrong_argument_violation(
                    "random",
                    "max must be greater than min",
                    Some(max),
                    Some(2),
                    2,
                    &[min, max],
                );
            }

            (ExactInteger::Fixnum(min), ExactInteger::BigInt(max)) => {
                let min_bigint = BigInt::from_i32(ctx, min);

                let range = BigInt::minus(max, ctx, min_bigint);
                let max_bitwidth = range.bits();
                let mut rng = rand::rng();
                let rb = BigInt::random(ctx, &mut rng, max_bitwidth);
                let rb = BigInt::rem(rb, ctx, range);
                let result = BigInt::plus(rb, ctx, min_bigint);
                return result.into();
            }

            (ExactInteger::BigInt(min), ExactInteger::BigInt(max)) => {
                if min >= max {
                    let min = min.into_value(ctx);
                    let max = max.into_value(ctx);
                    return call.wrong_argument_violation(
                        "random",
                        "min must be less than max",
                        Some(min),
                        Some(1),
                        2,
                        &[min, max],
                    );
                }

                let range = BigInt::minus(max, ctx, min);
                let max_bitwidth = range.bits();
                let mut rng = rand::rng();
                let rb = BigInt::random(ctx, &mut rng, max_bitwidth);
                let rb = BigInt::rem(rb, ctx, range);
                let result = BigInt::plus(rb, ctx, min);
                return result.into();
            }
        }
    }

    #[scheme(name = "=")]
    pub fn equal(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            let eq = Number::equal(ctx, w, w);
            return Ok(eq);
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                call.wrong_argument_violation(
                    "=",
                    "argument must be a number",
                    Some(*z),
                    Some(1),
                    rest.len() + 1,
                    [w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
                return Err(Value::undefined());
            };
            if !Number::equal(ctx, w, z) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "<")]
    pub fn lt(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                call.wrong_argument_violation(
                    "<",
                    "argument must be a number",
                    Some(*z),
                    Some(1),
                    rest.len() + 1,
                    [w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
                return Err(Value::undefined());
            };
            let cmp = Number::compare(ctx, w, z);
            if cmp != Some(Ordering::Less) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "<=")]
    pub fn le(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                call.wrong_argument_violation(
                    "<=",
                    "argument must be a number",
                    Some(*z),
                    Some(1),
                    rest.len() + 1,
                    [w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
                return Err(Value::undefined());
            };
            let cmp = Number::compare(ctx, w, z);
            if !matches!(cmp, Some(Ordering::Less) | Some(Ordering::Equal)) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = ">")]
    pub fn gt(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                call.wrong_argument_violation(
                    ">",
                    "argument must be a number",
                    Some(*z),
                    Some(1),
                    rest.len() + 1,
                    [w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
                return Err(Value::undefined());
            };
            let cmp = Number::compare(ctx, w, z);
            if cmp != Some(Ordering::Greater) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = ">=")]
    pub fn ge(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                call.wrong_argument_violation(
                    ">=",
                    "argument must be a number",
                    Some(*z),
                    Some(1),
                    rest.len() + 1,
                    [w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
                return Err(Value::undefined());
            };
            let cmp = Number::compare(ctx, w, z);
            if cmp == Some(Ordering::Less) || cmp.is_none() {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "positive?")]
    pub fn is_positive(w: Number<'gc>) -> bool {
        w.is_positive()
    }

    #[scheme(name = "negative?")]
    pub fn is_negative(w: Number<'gc>) -> bool {
        w.is_negative()
    }

    #[scheme(name = "zero?")]
    pub fn is_zero(w: Number<'gc>) -> bool {
        w.is_zero()
    }

    #[scheme(name = "fixnum?")]
    pub fn is_fixnum(w: Value<'gc>) -> bool {
        w.is_int32()
    }

    #[scheme(name = "bigint?")]
    pub fn is_bigint(w: Number<'gc>) -> bool {
        w.is_bigint()
    }

    #[scheme(name = "complex?")]
    pub fn is_complex(w: Value<'gc>) -> bool {
        w.is_number()
    }

    #[scheme(name = "real?")]
    pub fn is_real(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return false;
        };

        n.is_real()
    }

    #[scheme(name = "inexact-real?")]
    pub fn is_inexact_real(w: Number<'gc>) -> bool {
        w.is_real() && !w.is_exact()
    }

    #[scheme(name = "rational?")]
    pub fn is_rational(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return false;
        };

        n.is_rational()
    }

    #[scheme(name = "exact-positive-integer?")]
    pub fn is_exact_positive_integer(w: Number<'gc>) -> bool {
        w.is_exact_positive_integer()
    }

    #[scheme(name = "exact-nonnegative-integer?")]
    pub fn is_exact_nonnegative_integer(w: Number<'gc>) -> bool {
        w.is_exact_non_negative_integer()
    }

    #[scheme(name = "exact-integer?")]
    pub fn is_exact_integer(w: Number<'gc>) -> bool {
        w.is_exact_integer()
    }

    #[scheme(name = "integer?")]
    pub fn is_integer(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return false;
        };

        n.is_integer()
    }

    #[scheme(name = "number?")]
    pub fn is_number(w: Value<'gc>) -> bool {
        w.is_number()
    }

    #[scheme(name = "fx=?")]
    pub fn fx_eq(w: i32, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fx=?",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_int32();
            if w != z {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "fxlogand")]
    pub fn fxlogand(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(w);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxlogand",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w &= z;
        }

        Ok(w)
    }

    #[scheme(name = "logior")]
    pub fn logior(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return (w).into();
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return call.conversion_error("logior", e);
                }
            };
            w = ExactInteger::bitor(ctx, w, z);
        }

        (w).into()
    }

    #[scheme(name = "logxor")]
    pub fn logxor(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return (w).into();
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return call.conversion_error("logxor", e);
                }
            };
            w = ExactInteger::bitxor(ctx, w, z);
        }

        (w).into()
    }

    #[scheme(name = "logand")]
    pub fn logand(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return (w).into();
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return call.conversion_error("logand", e);
                }
            };
            w = ExactInteger::bitand(ctx, w, z);
        }

        (w).into()
    }

    #[scheme(name = "lognot")]
    pub fn lognot(w: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let w = ExactInteger::bitnot(ctx, w);
        w
    }

    #[scheme(name = "fxlogior")]
    pub fn fxlogior(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(w);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxlogior",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w |= z;
        }

        Ok(w)
    }

    #[scheme(name = "fxlogxor")]
    pub fn fxlogxor(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(w);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxlogxor",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w ^= z;
        }

        Ok(w)
    }

    #[scheme(name = "real-valued?")]
    pub fn is_real_valued(w: Number<'gc>) -> bool {
        if w.is_real() {
            return true;
        }

        if let Number::Complex(c) = w {
            return c.imag.is_zero();
        }

        false
    }

    #[scheme(name = "rational-valued?")]
    pub fn is_rational_valued(w: Number<'gc>) -> bool {
        if w.is_rational() {
            return true;
        }

        if let Number::Complex(c) = w {
            return c.imag.is_zero() && c.real.is_rational();
        }

        false
    }

    #[scheme(name = "integer-valued?")]
    pub fn is_integer_valued(w: Number<'gc>) -> bool {
        if w.is_integer() {
            return true;
        }

        if let Number::Complex(c) = w {
            return c.imag.is_zero() && c.real.is_integer();
        }

        false
    }

    #[scheme(name = "make-rectangular")]
    pub fn make_rectangular(real: Number<'gc>, imag: Number<'gc>) -> Number<'gc> {
        let real = if let Number::Complex(c) = real {
            c.real
        } else {
            real
        };
        let imag = if let Number::Complex(c) = imag {
            c.imag
        } else {
            imag
        };
        let c = Number::rectangular(real, ctx, imag);
        c
    }

    #[scheme(name = "make-polar")]
    pub fn make_polar(r: Number<'gc>, theta: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !r.is_real_valued() {
            let r = r.into_value(ctx);
            call.wrong_argument_violation(
                "make-polar",
                "magnitude must be a real number",
                Some(r),
                Some(1),
                2,
                &[r],
            );
            return Err(Value::undefined());
        }
        if !theta.is_real_valued() {
            let theta = theta.into_value(ctx);
            call.wrong_argument_violation(
                "make-polar",
                "angle must be a real number",
                Some(theta),
                Some(2),
                2,
                &[theta],
            );
            return Err(Value::undefined());
        }

        if theta.is_zero() {
            return Ok(r);
        }
        let c = r.polar(ctx, theta);
        Ok(c)
    }

    #[scheme(name = "real-part")]
    pub fn real_part(z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            c.real
        } else {
            z
        }
    }

    #[scheme(name = "imag-part")]
    pub fn imag_part(z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            c.imag
        } else {
            Number::Fixnum(0)
        }
    }

    #[scheme(name = "angle")]
    pub fn angle(z: Number<'gc>) -> Number<'gc> {
        let angle = z.angle(ctx);
        angle
    }

    #[scheme(name = "flonum?")]
    pub fn is_flonum(w: Number<'gc>) -> bool {
        w.is_flonum()
    }

    #[scheme(name = "fl=?")]
    pub fn fl_eq(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "fl=?",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if w != z {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "fl<?")]
    pub fn fl_lt(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "fl<?",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if w.partial_cmp(&z) != Some(Ordering::Less) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "fl>?")]
    pub fn fl_gt(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "fl>?",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if w.partial_cmp(&z) != Some(Ordering::Greater) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "fl<=?")]
    pub fn fl_le(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "fl<=?",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if !matches!(w.partial_cmp(&z), Some(Ordering::Less | Ordering::Equal)) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "fl>=?")]
    pub fn fl_ge(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return Ok(false);
            }

            return Ok(true);
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "fl>=?",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if !matches!(w.partial_cmp(&z), Some(Ordering::Greater | Ordering::Equal)) {
                return Ok(false);
            }
            w = z;
        }

        Ok(true)
    }

    #[scheme(name = "flinteger?")]
    pub fn is_fl_integer(w: f64) -> bool {
        w == w.trunc() && !w.is_infinite() && !w.is_nan()
    }

    #[scheme(name = "flzero?")]
    pub fn is_fl_zero(w: f64) -> bool {
        w == 0.0
    }

    #[scheme(name = "flpositive?")]
    pub fn is_fl_positive(w: f64) -> bool {
        w > 0.0
    }

    #[scheme(name = "flnegative?")]
    pub fn is_fl_negative(w: f64) -> bool {
        w < 0.0
    }

    #[scheme(name = "flodd?")]
    pub fn is_fl_odd(w: f64) -> bool {
        w % 2.0 != 0.0
    }

    #[scheme(name = "fleven?")]
    pub fn is_fl_even(w: f64) -> bool {
        w % 2.0 == 0.0
    }

    #[scheme(name = "flfinite?")]
    pub fn is_fl_finite(w: f64) -> bool {
        w.is_finite()
    }

    #[scheme(name = "flinfinite?")]
    pub fn is_fl_infinite(w: f64) -> bool {
        w.is_infinite()
    }

    #[scheme(name = "flnan?")]
    pub fn is_fl_nan(w: f64) -> bool {
        w.is_nan()
    }

    #[scheme(name = "flmax")]
    pub fn fl_max(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "flmax",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[max.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if z > max || max.is_nan() {
                max = z;
            }
        }

        Ok(max)
    }

    #[scheme(name = "flmin")]
    pub fn fl_min(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                call.wrong_argument_violation(
                    "flmin",
                    "argument must be a flonum",
                    Some(*z),
                    Some(1),
                    2,
                    &[min.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_flonum();
            if z < min || min.is_nan() {
                min = z;
            }
        }

        Ok(min)
    }

    #[scheme(name = "fl+")]
    pub fn fl_plus(args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.is_empty() {
            return Ok(0.0);
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                call.wrong_argument_violation(
                    "fl+",
                    "argument must be a flonum",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].flonum() else {
            call.wrong_argument_violation(
                "fl+",
                "argument must be a flonum",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                call.wrong_argument_violation(
                    "fl+",
                    "argument must be a flonum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            acc += arg;
        }

        Ok(acc)
    }

    #[scheme(name = "fl-")]
    pub fn fl_minus(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            return Ok(-x);
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(ctx));
            args.extend_from_slice(rest);
            call.wrong_argument_violation(
                "fl-",
                "argument must be a flonum",
                Some(rest[0]),
                Some(1),
                args.len(),
                &args,
            );
            return Err(Value::undefined());
        };
        acc = x - acc;
        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(ctx));
                args.extend_from_slice(rest);
                call.wrong_argument_violation(
                    "fl-",
                    "argument must be a flonum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            };
            acc -= arg;
        }

        Ok(acc)
    }

    #[scheme(name = "fl*")]
    pub fn fl_times(args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.is_empty() {
            return Ok(1.0);
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                call.wrong_argument_violation(
                    "fl*",
                    "argument must be a flonum",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].flonum() else {
            call.wrong_argument_violation(
                "fl*",
                "argument must be a flonum",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                call.wrong_argument_violation(
                    "fl*",
                    "argument must be a flonum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            acc *= arg;
        }

        Ok(acc)
    }

    #[scheme(name = "fl/")]
    pub fn fl_quotient(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            if x == 0.0 {
                let x = x.into_value(ctx);
                call.wrong_argument_violation("fl/", "division by zero", Some(x), Some(1), 1, &[x]);
                return Err(Value::undefined());
            }
            return Ok(1.0 / x);
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(ctx));
            args.extend_from_slice(rest);
            call.wrong_argument_violation(
                "fl/",
                "argument must be a flonum",
                Some(rest[0]),
                Some(1),
                args.len(),
                &args,
            );
            return Err(Value::undefined());
        };
        acc = x / acc;

        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(ctx));
                args.extend_from_slice(rest);
                call.wrong_argument_violation(
                    "fl/",
                    "argument must be a flonum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            };

            acc /= arg;
        }

        Ok(acc)
    }

    #[scheme(name = "fldiv")]
    pub fn fl_divide(x: f64, y: f64) -> Value<'gc> {
        if y == 0.0 {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "fldiv",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        let res = Number::integer_div(ctx, Number::Flonum(x), Number::Flonum(y));
        (res).into_value(ctx)
    }

    #[scheme(name = "fldiv0")]
    pub fn fl_divide0(x: f64, y: f64) -> Value<'gc> {
        if y == 0.0 {
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "fldiv0",
                "division by zero",
                Some(y),
                Some(1),
                1,
                &[y],
            );
        }
        let res = Number::integer_div0(ctx, Number::Flonum(x), Number::Flonum(y));
        (res).into_value(ctx)
    }

    #[scheme(name = "fxmod")]
    pub fn fx_mod(x: i32, y: i32) -> Value<'gc> {
        if y == 0 {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "fxmod",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        let fxdiv = if x == 0 {
            0
        } else if x > 0 {
            x / y
        } else if y > 0 {
            (x - y + 1) / y
        } else {
            (x + y + 1).wrapping_div(y)
        };

        // (fx- x (fx* (fxdiv x y) y))
        let r = x - (fxdiv.wrapping_mul(y));
        (r).into()
    }

    #[scheme(name = "bitwise-arithmetic-shift")]
    pub fn bitwise_arithmetic_shift(x: Number<'gc>, y: Number<'gc>) -> Value<'gc> {
        if !x.is_exact_integer() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "first argument must be an exact integer",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }
        if !y.is_exact_integer() {
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "second argument must be an exact integer",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }

        let Some(shifted) = x.ash(ctx, y) else {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "shift amount is too large",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        };
        (shifted).into_value(ctx)
    }

    #[scheme(name = "bitwise-bit-count")]
    pub fn bitwise_bit_count(x: ExactInteger<'gc>) -> i32 {
        let count = x.bit_count(ctx);
        count
    }

    #[scheme(name = "bitwise-length")]
    pub fn bitwise_bit_length(x: ExactInteger<'gc>) -> u32 {
        let length = x.bit_length(ctx);
        length
    }

    #[scheme(name = "bitwise-first-bit-set")]
    pub fn bitwise_first_bit_set(x: ExactInteger<'gc>) -> i32 {
        let pos = ExactInteger::first_bit_set(ctx, x);
        pos
    }

    #[scheme(name = "bitwise-not")]
    pub fn bitwise_not(x: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let not = ExactInteger::bitnot(ctx, x);
        not
    }

    #[scheme(name = "bitwise-and")]
    pub fn bitwise_and(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return Value::new(-1i32);
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return call.wrong_argument_violation(
                    "bitwise-and",
                    "argument must be a number",
                    Some(rest[0]),
                    Some(1),
                    1,
                    &[rest[0]],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(ctx);
                return call.wrong_argument_violation(
                    "bitwise-and",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(ctx);

            return n;
        }

        let mut acc = match ExactInteger::try_from_value(ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return call.conversion_error("bitwise-and", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(ctx, *v) {
                Ok(b) => b,
                Err(e) => return call.conversion_error("bitwise-and", e),
            };
            acc = ExactInteger::bitand(ctx, acc, b);
        }

        acc.into()
    }

    #[scheme(name = "bitwise-ior")]
    pub fn bitwise_ior(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return Value::new(0i32);
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return call.wrong_argument_violation(
                    "bitwise-ior",
                    "argument must be a number",
                    Some(rest[0]),
                    Some(1),
                    1,
                    &[rest[0]],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(ctx);
                return call.wrong_argument_violation(
                    "bitwise-ior",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(ctx);

            return n;
        }

        if rest.len() == 2 {
            let a = match ExactInteger::try_from_value(ctx, rest[0]) {
                Ok(a) => a,
                Err(e) => return call.conversion_error("bitwise-ior", e),
            };
            let b = match ExactInteger::try_from_value(ctx, rest[1]) {
                Ok(b) => b,
                Err(e) => return call.conversion_error("bitwise-ior", e),
            };
            let res = ExactInteger::bitor(ctx, a, b);
            return res.into();
        }

        let mut acc = match ExactInteger::try_from_value(ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return call.conversion_error("bitwise-ior", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(ctx, *v) {
                Ok(b) => b,
                Err(e) => return call.conversion_error("bitwise-ior", e),
            };
            acc = ExactInteger::bitor(ctx, acc, b);
        }

        acc.into()
    }

    #[scheme(name = "bitwise-xor")]
    pub fn bitwise_xor(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return Value::new(0i32);
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return call.wrong_argument_violation(
                    "bitwise-xor",
                    "argument must be a number",
                    Some(rest[0]),
                    Some(1),
                    1,
                    &[rest[0]],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(ctx);
                return call.wrong_argument_violation(
                    "bitwise-xor",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(ctx);

            return n;
        }

        if rest.len() == 2 {
            let a = match ExactInteger::try_from_value(ctx, rest[0]) {
                Ok(a) => a,
                Err(e) => return call.conversion_error("bitwise-xor", e),
            };
            let b = match ExactInteger::try_from_value(ctx, rest[1]) {
                Ok(b) => b,
                Err(e) => return call.conversion_error("bitwise-xor", e),
            };
            let res = ExactInteger::bitxor(ctx, a, b);
            return res.into();
        }

        let mut acc = match ExactInteger::try_from_value(ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return call.conversion_error("bitwise-xor", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(ctx, *v) {
                Ok(b) => b,
                Err(e) => return call.conversion_error("bitwise-xor", e),
            };
            acc = ExactInteger::bitxor(ctx, acc, b);
        }

        acc.into()
    }

    #[scheme(name = "fixnum-width")]
    pub fn fixnum_width() -> i32 {
        32
    }

    #[scheme(name = "least-fixnum")]
    pub fn least_fixnum() -> i32 {
        i32::MIN
    }

    #[scheme(name = "greatest-fixnum")]
    pub fn greatest_fixnum() -> i32 {
        i32::MAX
    }

    #[scheme(name = "fx<?")]
    pub fn fx_lt(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(a < b);
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a < b) {
                return Ok(false);
            }
            a = b;
            b = match rest.at(ctx, i) {
                Ok(v) => v,
                Err(e) => return Err(call.conversion_error("fx<?", e)),
            };
        }

        Ok(a < b)
    }

    #[scheme(name = "fx<=?")]
    pub fn fx_le(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(a <= b);
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a <= b) {
                return Ok(false);
            }
            a = b;
            b = match rest.at(ctx, i) {
                Ok(v) => v,
                Err(e) => return Err(call.conversion_error("fx<=?", e)),
            };
        }

        Ok(a <= b)
    }

    #[scheme(name = "fx>?")]
    pub fn fx_gt(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(a > b);
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a > b) {
                return Ok(false);
            }
            a = b;
            b = match rest.at(ctx, i) {
                Ok(v) => v,
                Err(e) => return Err(call.conversion_error("fx>?", e)),
            };
        }

        Ok(a > b)
    }

    #[scheme(name = "fx>=?")]
    pub fn fx_ge(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return Ok(a >= b);
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a >= b) {
                return Ok(false);
            }
            a = b;
            b = match rest.at(ctx, i) {
                Ok(v) => v,
                Err(e) => return Err(call.conversion_error("fx>=?", e)),
            };
        }

        Ok(a >= b)
    }

    #[scheme(name = "fxzero?")]
    pub fn fxzerop(w: i32) -> bool {
        w == 0
    }

    #[scheme(name = "fxpositive?")]
    pub fn fxpositivep(w: i32) -> bool {
        w > 0
    }

    #[scheme(name = "fxnegative?")]
    pub fn fxnegativep(w: i32) -> bool {
        w < 0
    }

    #[scheme(name = "fxodd?")]
    pub fn fxoddp(w: i32) -> bool {
        w % 2 != 0
    }

    #[scheme(name = "fxeven?")]
    pub fn fxevenp(w: i32) -> bool {
        w % 2 == 0
    }

    #[scheme(name = "fxmax")]
    pub fn fx_max(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxmax",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[max.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_int32();
            if z > max {
                max = z;
            }
        }

        Ok(max)
    }

    #[scheme(name = "fxmin")]
    pub fn fx_min(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxmin",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[min.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }
            let z = z.as_int32();
            if z < min {
                min = z;
            }
        }

        Ok(min)
    }

    #[scheme(name = "fx+")]
    pub fn fx_plus(args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.is_empty() {
            return Ok(0);
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                call.wrong_argument_violation(
                    "fx+",
                    "argument must be a fixnum",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].int32() else {
            call.wrong_argument_violation(
                "fx+",
                "argument must be a fixnum",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                call.wrong_argument_violation(
                    "fx+",
                    "argument must be a fixnum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            match acc.checked_add(arg) {
                Some(v) => acc = v,
                None => {
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    call.implementation_restriction_violation(
                        "fx+",
                        "integer overflow",
                        &[acc, arg],
                    );
                    return Err(Value::undefined());
                }
            }
        }

        Ok(acc)
    }

    #[scheme(name = "fx-")]
    pub fn fx_minus(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            match x.checked_neg() {
                Some(v) => return Ok(v),
                None => {
                    let x = x.into_value(ctx);
                    call.implementation_restriction_violation("fx-", "integer overflow", &[x]);
                    return Err(Value::undefined());
                }
            }
        }

        let Some(mut acc) = rest[0].int32() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(ctx));
            args.extend_from_slice(rest);
            call.wrong_argument_violation(
                "fx-",
                "argument must be a fixnum",
                Some(rest[0]),
                Some(1),
                args.len(),
                &args,
            );
            return Err(Value::undefined());
        };

        acc = match x.checked_sub(acc) {
            Some(v) => v,
            None => {
                let x = x.into_value(ctx);
                let acc = acc.into_value(ctx);
                call.implementation_restriction_violation("fx-", "integer overflow", &[x, acc]);
                return Err(Value::undefined());
            }
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.int32() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(ctx));
                args.extend_from_slice(rest);
                call.wrong_argument_violation(
                    "fx-",
                    "argument must be a fixnum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    &args,
                );
                return Err(Value::undefined());
            };
            match acc.checked_sub(arg) {
                Some(v) => acc = v,
                None => {
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    call.implementation_restriction_violation(
                        "fx-",
                        "integer overflow",
                        &[acc, arg],
                    );
                    return Err(Value::undefined());
                }
            }
        }

        Ok(acc)
    }

    #[scheme(name = "fx*")]
    pub fn fx_times(args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.is_empty() {
            return Ok(1);
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                call.wrong_argument_violation(
                    "fx*",
                    "argument must be a fixnum",
                    Some(args[0]),
                    Some(0),
                    1,
                    args,
                );
                return Err(Value::undefined());
            };
            return Ok(arg);
        }

        let Some(mut acc) = args[0].int32() else {
            call.wrong_argument_violation(
                "fx*",
                "argument must be a fixnum",
                Some(args[0]),
                Some(0),
                args.len(),
                args,
            );
            return Err(Value::undefined());
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                call.wrong_argument_violation(
                    "fx*",
                    "argument must be a fixnum",
                    Some(*arg),
                    Some(1),
                    args.len(),
                    args,
                );
                return Err(Value::undefined());
            };
            match acc.checked_mul(arg) {
                Some(v) => acc = v,
                None => {
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    call.implementation_restriction_violation(
                        "fx*",
                        "integer overflow",
                        &[acc, arg],
                    );
                    return Err(Value::undefined());
                }
            }
        }

        Ok(acc)
    }

    #[scheme(name = "fxdiv")]
    pub fn fx_div(x: i32, y: i32) -> Value<'gc> {
        if y == 0 {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "fxdiv",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        /*let Some(res) = x.checked_div(y) else {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.implementation_restriction_violation("fxdiv", "integer overflow", &[x, y]);
        };*/
        let n = Number::integer_div(ctx, Number::Fixnum(x), Number::Fixnum(y));
        if !matches!(n, Number::Fixnum(_)) {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.implementation_restriction_violation("fxdiv", "integer overflow", &[x, y]);
        }
        (n).into_value(ctx)
    }

    #[scheme(name = "fxdiv0")]
    pub fn fx_div0(x: i32, y: i32) -> Value<'gc> {
        if y == 0 {
            let y = y.into_value(ctx);
            return call.wrong_argument_violation(
                "fxdiv0",
                "division by zero",
                Some(y),
                Some(1),
                1,
                &[y],
            );
        }
        let n = Number::integer_div0(ctx, Number::Fixnum(x), Number::Fixnum(y));
        if !matches!(n, Number::Fixnum(_)) {
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return call.implementation_restriction_violation(
                "fxdiv0",
                "integer overflow",
                &[x, y],
            );
        }
        (n).into_value(ctx)
    }

    #[scheme(name = "fxnot")]
    pub fn fx_not(w: i32) -> i32 {
        !w
    }

    #[scheme(name = "fxand")]
    pub fn fx_and(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(-1);
        }
        let Some(mut w) = rest[0].int32() else {
            call.wrong_argument_violation(
                "fxand",
                "argument must be a fixnum",
                Some(rest[0]),
                Some(0),
                rest.len(),
                rest,
            );
            return Err(Value::undefined());
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxand",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w &= z;
        }

        Ok(w)
    }

    #[scheme(name = "fxior")]
    pub fn fxior(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(0);
        }
        let Some(mut w) = rest[0].int32() else {
            call.wrong_argument_violation(
                "fxior",
                "argument must be a fixnum",
                Some(rest[0]),
                Some(0),
                rest.len(),
                rest,
            );
            return Err(Value::undefined());
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxior",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w |= z;
        }

        Ok(w)
    }

    #[scheme(name = "fxxor")]
    pub fn fxxor(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return Ok(0);
        }
        let Some(mut w) = rest[0].int32() else {
            call.wrong_argument_violation(
                "fxxor",
                "argument must be a fixnum",
                Some(rest[0]),
                Some(0),
                rest.len(),
                rest,
            );
            return Err(Value::undefined());
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                call.wrong_argument_violation(
                    "fxxor",
                    "argument must be a fixnum",
                    Some(*z),
                    Some(1),
                    2,
                    &[w.into_value(ctx), *z],
                );
                return Err(Value::undefined());
            }

            let z = z.as_int32();
            w ^= z;
        }

        Ok(w)
    }

    #[scheme(name = "fxif")]
    pub fn fxif(fx1: i32, fx2: i32, fx3: i32) -> i32 {
        (fx1 & fx2) | (!fx1 & fx3)
    }

    #[scheme(name = "fxbit-count")]
    pub fn fxbit_count(w: i32) -> i32 {
        ExactInteger::Fixnum(w).bit_count(ctx)
    }

    #[scheme(name = "fxlength")]
    pub fn fxlength(w: i32) -> i32 {
        if w == 0 {
            return 0;
        }
        ExactInteger::Fixnum(w).bit_length(ctx) as i32
    }

    #[scheme(name = "fxfirst-bit-set")]
    pub fn fxfirst_bit_set(w: i32) -> i32 {
        if w == 0 {
            return -1;
        }
        ExactInteger::first_bit_set(ctx, ExactInteger::Fixnum(w))
    }

    #[scheme(name = "fxbit-set?")]
    pub fn fxbit_setp(w: i32, k: i32) -> Result<bool, Value<'gc>> {
        if !(0..32).contains(&k) {
            let w = w.into_value(ctx);
            let k = k.into_value(ctx);
            call.wrong_argument_violation(
                "fxbit-set?",
                "bit index out of range",
                Some(k),
                Some(2),
                2,
                &[w, k],
            );
            return Err(Value::undefined());
        }
        Ok((w & (1 << k)) != 0)
    }

    #[scheme(name = "fxcopy-bit")]
    pub fn fxcopy_bit(fx1: i32, fx2: i32, fx3: i32) -> Value<'gc> {
        if (0..32).contains(&fx2) {
            if fx3 == 0 || fx3 == 1 {
                let mask = 1 << fx2;
                return ((mask & (fx3 << fx2)) | (!mask & fx1)).into();
            } else {
                let fx1 = fx1.into_value(ctx);
                let fx2 = fx2.into_value(ctx);
                let fx3 = fx3.into_value(ctx);
                return call.wrong_argument_violation(
                    "fxcopy-bit",
                    "third argument must be 0 or 1",
                    Some(fx3),
                    Some(3),
                    3,
                    &[fx1, fx2, fx3],
                );
            }
        }

        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        let fx3 = fx3.into_value(ctx);
        call.wrong_argument_violation(
            "fxcopy-bit",
            "bit index out of range",
            Some(fx2),
            Some(2),
            3,
            &[fx1, fx2, fx3],
        )
    }

    #[scheme(name = "fxarithmetic-shift")]
    pub fn fxarithmetic_shift(fx1: i32, fx2: i32) -> Value<'gc> {
        if fx2 > -32 && fx2 < 32 {
            let fx1 = fx1 as i64;
            let fx2 = fx2 as i64;
            if fx2 > 0 {
                let n = fx1 << fx2;
                if (n >> fx2) == fx1 && (n >= i32::MIN as i64) && (n <= i32::MAX as i64) {
                    return (n as i32).into();
                }
            } else {
                let n = fx1 >> -fx2;
                if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                    return (n as i32).into();
                }
            }
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return call.implementation_restriction_violation(
                "fxarithmetic-shift",
                "shift amount is too largeaaa",
                &[fx1, fx2],
            );
        }

        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        call.implementation_restriction_violation(
            "fxarithmetic-shift",
            "shift amount is too large",
            &[fx1, fx2],
        )
    }

    #[scheme(name = "fxarithmetic-shift-left")]
    pub fn fxarithmetic_shift_left(fx1: i32, fx2: u32) -> Value<'gc> {
        let fx1 = fx1 as i64;
        let fx2 = fx2 as i64;
        if (0..32).contains(&fx2) {
            let n = fx1 << fx2;
            if (n >> fx2) == fx1 && (n >= i32::MIN as i64) && (n <= i32::MAX as i64) {
                return (n as i32).into();
            }
        }

        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        return call.implementation_restriction_violation(
            "fxarithmetic-shift-left",
            "shift amount is too large",
            &[fx1, fx2],
        );
    }

    #[scheme(name = "fxarithmetic-shift-right")]
    pub fn fxarithmetic_shift_right(fx1: i32, fx2: u32) -> Value<'gc> {
        let Some(n) = fx1.checked_shr(fx2) else {
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return call.implementation_restriction_violation(
                "fxarithmetic-shift-right",
                "shift amount is too large",
                &[fx1, fx2],
            );
        };
        (n).into()
    }

    #[scheme(name = "fxbit-field")]
    pub fn fxbit_field(fx1: i32, fx2: i32, fx3: i32) -> Value<'gc> {
        if !(0..32).contains(&fx2) {
            return call.wrong_argument_violation(
                "fxbit-field",
                "second argument must be in the range 0 to 31",
                Some(fx2.into_value(ctx)),
                Some(2),
                3,
                &[
                    fx1.into_value(ctx),
                    fx2.into_value(ctx),
                    fx3.into_value(ctx),
                ],
            );
        }

        if !(0..32).contains(&fx3) {
            return call.wrong_argument_violation(
                "fxbit-field",
                "third argument must be in the range 0 to 31",
                Some(fx3.into()),
                Some(3),
                3,
                &[fx1.into(), fx2.into(), fx3.into()],
            );
        }

        if fx2 > fx3 {
            return call.wrong_argument_violation(
                "fxbit-field",
                "second argument must be less than or equal to the third argument",
                Some(fx2.into_value(ctx)),
                Some(2),
                3,
                &[
                    fx1.into_value(ctx),
                    fx2.into_value(ctx),
                    fx3.into_value(ctx),
                ],
            );
        }

        let mask = !(-1 << fx3);
        return ((fx1 & mask) >> fx2).into();
    }

    #[scheme(name = "fxcopy-bit-field")]
    pub fn fxcopy_bit_field(to: i32, start: i32, end: i32, from: i32) -> Value<'gc> {
        if (0..32).contains(&start) {
            if (0..32).contains(&end) {
                if start <= end {
                    let mask = if end - start == 32 {
                        !0
                    } else {
                        (1 << (end - start)) - 1
                    };
                    let field = from & mask;
                    let clear_mask = !(mask << start);
                    let result = (to & clear_mask) | (field << start);
                    return (result).into();
                } else {
                    return call.wrong_argument_violation(
                        "fxcopy-bit-field",
                        "value out of range",
                        Some(start.into()),
                        Some(2),
                        4,
                        &[to.into(), start.into(), end.into(), from.into()],
                    );
                }
            } else {
                return call.wrong_argument_violation(
                    "fxcopy-bit-field",
                    "value out of range",
                    Some(end.into()),
                    Some(3),
                    4,
                    &[to.into(), start.into(), end.into(), from.into()],
                );
            }
        } else {
            return call.wrong_argument_violation(
                "fxcopy-bit-field",
                "value out of range",
                Some(start.into()),
                Some(2),
                4,
                &[to.into(), start.into(), end.into(), from.into()],
            );
        }
    }

    #[scheme(name = "fixnum->flonum")]
    pub fn fixnum_to_flonum(x: i32) -> f64 {
        x as f64
    }

    #[scheme(name = "real->flonum")]
    pub fn real_to_flonum(x: Number<'gc>) -> Value<'gc> {
        if !x.is_real() {
            let x = x.into_value(ctx);
            return call.wrong_argument_violation(
                "real->flonum",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        let fl = x.real_to_f64(ctx);

        (fl).into()
    }

    #[scheme(name = "flsqrt")]
    pub fn fl_sqrt(x: f64) -> f64 {
        let res = x.sqrt();
        res
    }

    #[scheme(name = "flabs")]
    pub fn fl_abs(x: f64) -> f64 {
        let res = x.abs();
        res
    }

    #[scheme(name = "flatan")]
    pub fn fl_atan(x: f64, y: Option<f64>) -> f64 {
        let res = match y {
            Some(y) => x.atan2(y),
            None => x.atan(),
        };
        res
    }

    #[scheme(name = "flacos")]
    pub fn fl_acos(x: f64) -> f64 {
        let res = x.acos();
        res
    }

    #[scheme(name = "flasin")]
    pub fn fl_asin(x: f64) -> f64 {
        let res = x.asin();
        res
    }

    #[scheme(name = "fllog")]
    pub fn fl_log(x: f64, y: Option<f64>) -> f64 {
        let res = match y {
            Some(base) => x.log(base),
            None => x.ln(),
        };
        res
    }

    #[scheme(name = "fltan")]
    pub fn fl_tan(x: f64) -> f64 {
        let res = x.tan();
        res
    }

    #[scheme(name = "flcos")]
    pub fn fl_cos(x: f64) -> f64 {
        let res = x.cos();
        res
    }

    #[scheme(name = "flsin")]
    pub fn fl_sin(x: f64) -> f64 {
        let res = x.sin();
        res
    }

    #[scheme(name = "flexpt")]
    pub fn fl_expt(x: f64, y: f64) -> f64 {
        let res = x.powf(y);
        res
    }

    #[scheme(name = "flexp")]
    pub fn fl_exp(x: f64) -> f64 {
        let res = x.exp();
        res
    }

    #[scheme(name = "flround")]
    pub fn fl_round(x: f64) -> f64 {
        let res = x.round();
        res
    }

    #[scheme(name = "fltruncate")]
    pub fn fl_truncate(x: f64) -> f64 {
        let res = x.trunc();
        res
    }

    #[scheme(name = "flceiling")]
    pub fn fl_ceiling(x: f64) -> f64 {
        let res = x.ceil();
        res
    }

    #[scheme(name = "flfloor")]
    pub fn fl_floor(x: f64) -> f64 {
        let res = x.floor();
        res
    }

    #[scheme(name = "fldenominator")]
    pub fn fl_denominator(x: f64) -> Number<'gc> {
        let n = Number::Flonum(x).to_exact(ctx);
        let res = if let Number::Rational(r) = n {
            r.denominator.to_inexact(ctx)
        } else {
            Number::Flonum(1.0)
        };
        res
    }

    #[scheme(name = "flnumerator")]
    pub fn fl_numerator(x: f64) -> Number<'gc> {
        let n = Number::Flonum(x).to_exact(ctx);
        let res = if let Number::Rational(r) = n {
            r.numerator.to_inexact(ctx)
        } else {
            Number::Flonum(x)
        };
        res
    }

    #[scheme(name = "decode-flonum")]
    pub fn decode_flonum(x: f64) -> (i64, i32, i32) {
        let bits = x.to_bits();

        let mant_bits = bits & 0x000F_FFFF_FFFF_FFFF;
        let sign_bits = (bits >> 63) & 0x1;
        let exp_bits = (bits >> 52) & 0x7FF;
        let exp;
        let sign;
        let mantissa = if x == 0.0 {
            exp = 0;
            sign = if sign_bits != 0 { -1 } else { 0 };
            0
        } else if x.is_nan() {
            exp = 972;
            sign = 1;
            0x18000000000000
        } else if x.is_infinite() {
            exp = 972;
            sign = if sign_bits != 0 { -1 } else { 1 };
            0x18000000000000
        } else {
            exp = if exp_bits != 0 {
                exp_bits.wrapping_sub(1023) as i32
            } else {
                -1022
            }
            .wrapping_sub(52);
            sign = if sign_bits != 0 { -1 } else { 1 };
            mant_bits as i64
        };
        let mantissa = if exp_bits != 0 {
            mantissa | 0x0010_0000_0000_0000
        } else {
            mantissa
        };

        (mantissa, exp, sign)
    }
}

pub(crate) fn init<'gc>(ctx: MutationContext<'gc>) {
    arith_operations::register(ctx);
}
