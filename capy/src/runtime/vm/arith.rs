use std::cmp::Ordering;

use crate::prelude::*;
use crate::{global, list, vector};
use crate::{
    runtime::value::conversions::*,
    runtime::{Context, prelude::*},
};

#[scheme(path = capy)]
mod arith_operations {
    #[scheme(name = "abs")]
    pub fn abs(x: Number<'gc>) -> Number<'gc> {
        let res = x.abs(nctx.ctx);
        nctx.return_(res)
    }

    #[scheme(name = "nan?")]
    pub fn is_nan(x: Number<'gc>) -> bool {
        nctx.return_(x.is_nan())
    }

    #[scheme(name = "infinite?")]
    pub fn is_infinite(x: Number<'gc>) -> bool {
        nctx.return_(x.is_infinite())
    }

    #[scheme(name = "finite?")]
    pub fn is_finite(x: Number<'gc>) -> bool {
        nctx.return_(x.is_finite())
    }

    #[scheme(name = "exact?")]
    pub fn is_exact(x: Number<'gc>) -> bool {
        nctx.return_(x.is_exact())
    }

    #[scheme(name = "inexact?")]
    pub fn is_inexact(x: Number<'gc>) -> bool {
        nctx.return_(!x.is_exact())
    }

    #[scheme(name = "odd?")]
    pub fn is_odd(x: Number<'gc>) -> bool {
        let is_odd = match x {
            Number::Fixnum(i) => i % 2 != 0,
            Number::BigInt(b) => b.is_odd(),
            _ => x.real_to_f64(nctx.ctx) % 2.0 != 0.0,
        };
        nctx.return_(is_odd)
    }

    #[scheme(name = "even?")]
    pub fn is_even(x: Number<'gc>) -> bool {
        let is_even = match x {
            Number::Fixnum(i) => i % 2 == 0,
            Number::BigInt(b) => !b.is_odd(),
            _ => x.real_to_f64(nctx.ctx) % 2.0 == 0.0,
        };
        nctx.return_(is_even)
    }

    #[scheme(name = "quotient")]
    pub fn quotient(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "quotient",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        let result = Number::quotient(nctx.ctx, x, y);

        nctx.return_(result)
    }

    #[scheme(name = "remainder")]
    pub fn remainder(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "remainder",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        let result = Number::remainder(nctx.ctx, x, y);

        nctx.return_(result)
    }

    #[scheme(name = "modulo")]
    pub fn modulo(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "modulo",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }

        if !x.is_integer_valued() {
            let x_val = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "modulo",
                "first argument must be an integer-valued number",
                Some(x_val),
                Some(1),
                2,
                &[x_val],
            );
        }

        if !y.is_integer_valued() {
            let y_val = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "modulo",
                "second argument must be an integer-valued number",
                Some(y_val),
                Some(2),
                2,
                &[y_val],
            );
        }

        let result = Number::modulo(nctx.ctx, x, y);

        nctx.return_(result)
    }

    #[scheme(name = "-")]
    pub fn minus(x: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            let negated = x.negate(nctx.ctx);
            return nctx.return_(Ok(negated));
        }

        let mut acc = x;
        let mut position = 1;
        for arg in rest.iter() {
            let Some(arg) = arg.number() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(x.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "-",
                    "argument must be a number",
                    Some(arg.clone()),
                    Some(position),
                    args.len(),
                    &args,
                );
            };
            acc = Number::sub(nctx.ctx, acc, arg);
            position += 1;
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "inexact")]
    pub fn inexact(x: Number<'gc>) -> Number<'gc> {
        let n = x.to_inexact(nctx.ctx);

        nctx.return_(n)
    }

    #[scheme(name = "exact")]
    pub fn exact(x: Number<'gc>) -> Number<'gc> {
        let n = x.to_exact(nctx.ctx);

        nctx.return_(n)
    }

    #[scheme(name = "+")]
    pub fn plus(args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(Number::Fixnum(0)));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                return nctx.wrong_argument_violation(
                    "+",
                    "argument must be a number",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else {
            return nctx.wrong_argument_violation(
                "+",
                "argument must be a number",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                return nctx.wrong_argument_violation(
                    "+",
                    "argument must be a number",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            acc = Number::add(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "*")]
    pub fn times(args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(Number::Fixnum(1)));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                return nctx.wrong_argument_violation(
                    "*",
                    "argument must be a number",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else {
            return nctx.wrong_argument_violation(
                "*",
                "argument must be a number",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                return nctx.wrong_argument_violation(
                    "*",
                    "argument must be a number",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            acc = Number::mul(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "/")]
    pub fn div(acc: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            if acc.is_zero() {
                let acc = acc.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "/",
                    "division by zero",
                    Some(acc),
                    Some(1),
                    1,
                    &[acc],
                );
            }
            let one = Number::Fixnum(1);
            let res = Number::div(nctx.ctx, one, acc);
            return nctx.return_(Ok(res));
        }

        let mut acc = acc;

        for arg in rest.iter() {
            let Some(arg) = arg.number() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "/",
                    "argument must be a number",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    &args,
                );
            };
            if arg.is_zero() {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                let arg = arg.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "/",
                    "division by zero",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    &args,
                );
            }
            acc = Number::div(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "div")]
    pub fn integer_div(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div",
                "first argument must be a real number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !x.is_finite() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div",
                "first argument must be a finite number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !y.is_real() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div",
                "second argument must be a real number",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }

        if y.is_zero() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }
        let res = Number::integer_div(nctx.ctx, x, y);
        nctx.return_(res)
    }

    #[scheme(name = "div0")]
    pub fn integer_div0(x: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div0",
                "first argument must be a real number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !x.is_finite() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div0",
                "first argument must be a finite number",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }

        if !rhs.is_real() {
            let rhs = rhs.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div0",
                "second argument must be a real number",
                Some(rhs),
                Some(2),
                2,
                &[rhs],
            );
        }

        if rhs.is_zero() {
            let rhs = rhs.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "div0",
                "division by zero",
                Some(rhs),
                Some(2),
                2,
                &[rhs],
            );
        }

        let res = Number::integer_div0(nctx.ctx, x, rhs);
        nctx.return_(res)
    }

    #[scheme(name = "denominator")]
    pub fn denominator(x: Number<'gc>) -> Number<'gc> {
        if !x.is_real_valued() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "denominator",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        let inexact = !x.is_exact();
        let obj = x.to_exact(nctx.ctx);
        if let Number::Rational(rn) = obj {
            let deno = if inexact {
                rn.denominator.to_inexact(nctx.ctx)
            } else {
                rn.denominator
            };

            return nctx.return_(deno);
        }

        nctx.return_(if inexact {
            Number::Flonum(1.0)
        } else {
            Number::Fixnum(1)
        })
    }

    #[scheme(name = "numerator")]
    pub fn numerator(x: Number<'gc>) -> Number<'gc> {
        if !x.is_real_valued() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "numerator",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        let inexact = !x.is_exact();
        let obj = x.to_exact(nctx.ctx);
        if let Number::Rational(rn) = obj {
            let nume = if inexact {
                rn.numerator.to_inexact(nctx.ctx)
            } else {
                rn.numerator
            };

            return nctx.return_(nume);
        }
        let ctx = nctx.ctx;
        nctx.return_(if inexact { x.to_inexact(ctx) } else { x })
    }

    #[scheme(name = "expt")]
    pub fn expt(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        let expt = Number::expt(nctx.ctx, x, y);

        nctx.return_(expt)
    }

    #[scheme(name = "exp")]
    pub fn exp(x: Number<'gc>) -> Number<'gc> {
        let exp = Number::exp(nctx.ctx, x);
        nctx.return_(exp)
    }

    #[scheme(name = "log")]
    pub fn log(x: Number<'gc>, y: Option<Number<'gc>>) -> Number<'gc> {
        if x.is_zero() {
            return nctx.implementation_restriction_violation(
                "log",
                "logarithm of zero is undefined",
                &[x.into_value(ctx)],
            );
        }
        if let Some(y) = y {
            if y.is_zero() {
                return nctx.implementation_restriction_violation(
                    "log",
                    "logarithm with base zero is undefined",
                    &[x.into_value(ctx), y.into_value(ctx)],
                );
            }

            let log = Number::log(ctx, x);
            let log_base = Number::log(ctx, y);
            let result = Number::div(ctx, log, log_base);
            return nctx.return_(result);
        }
        let log = Number::log(nctx.ctx, x);
        nctx.return_(log)
    }

    #[scheme(name = "sin")]
    pub fn sin(x: Number<'gc>) -> Number<'gc> {
        let sin = Number::sin(nctx.ctx, x);
        nctx.return_(sin)
    }

    #[scheme(name = "cos")]
    pub fn cos(x: Number<'gc>) -> Number<'gc> {
        let cos = Number::cos(nctx.ctx, x);
        nctx.return_(cos)
    }

    #[scheme(name = "tan")]
    pub fn tan(x: Number<'gc>) -> Number<'gc> {
        let tan = Number::tan(nctx.ctx, x);
        nctx.return_(tan)
    }

    #[scheme(name = "atan")]
    pub fn atan(_x: Number<'gc>) -> Number<'gc> {
        let atan = Number::atan(nctx.ctx, _x);
        nctx.return_(atan)
    }

    #[scheme(name = "atan2")]
    pub fn atan2(lhs: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        let ctx = nctx.ctx;
        if !lhs.is_real_valued() {
            return nctx.wrong_argument_violation(
                "atan2",
                "argument must be a real number",
                Some(lhs.into_value(ctx)),
                Some(1),
                1,
                &[lhs.into_value(ctx)],
            );
        }
        if !rhs.is_real_valued() {
            return nctx.wrong_argument_violation(
                "atan2",
                "argument must be a real number",
                Some(rhs.into_value(ctx)),
                Some(2),
                2,
                &[rhs.into_value(ctx)],
            );
        }
        let atan2 = Number::atan2(ctx, lhs, rhs);
        nctx.return_(atan2)
    }

    #[scheme(name = "sqrt")]
    pub fn sqrt(x: Number<'gc>) -> Number<'gc> {
        let sqrt = Number::sqrt(nctx.ctx, x);
        nctx.return_(sqrt)
    }

    #[scheme(name = "magnitude")]
    pub fn magnitude(x: Number<'gc>) -> Number<'gc> {
        let magnitude = Number::magnitude(nctx.ctx, x);
        nctx.return_(magnitude)
    }

    #[scheme(name = "exact-integer-sqrt")]
    pub fn exact_integer_sqrt(x: Number<'gc>) -> Result<(Number<'gc>, Number<'gc>), Value<'gc>> {
        if !x.is_exact_integer() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "exact-integer-sqrt",
                "argument must be an exact integer",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }
        let sqrt = Number::exact_integer_sqrt(nctx.ctx, x);
        nctx.return_(Ok(sqrt))
    }

    #[scheme(name = "inverse")]
    pub fn inverse(x: Number<'gc>) -> Number<'gc> {
        let inverse = Number::inverse(nctx.ctx, x);
        nctx.return_(inverse)
    }

    #[scheme(name = "asin")]
    pub fn asin(x: Number<'gc>) -> Number<'gc> {
        let asin = Number::asin(nctx.ctx, x);
        nctx.return_(asin)
    }

    #[scheme(name = "acos")]
    pub fn acos(x: Number<'gc>) -> Number<'gc> {
        let acos = Number::acos(nctx.ctx, x);
        nctx.return_(acos)
    }

    #[scheme(name = "floor")]
    pub fn floor(x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "floor",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }
        let floor = x.floor(nctx.ctx);
        nctx.return_(Ok(floor))
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
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "ceiling",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }
        let ceiling = x.ceiling(nctx.ctx);
        nctx.return_(Ok(ceiling))
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
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "truncate",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }
        let truncate = x.truncate(nctx.ctx);
        nctx.return_(Ok(truncate))
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
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "round",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }
        let round = x.round(nctx.ctx);
        nctx.return_(Ok(round))
    }

    #[scheme(name = "random")]
    pub fn random(min: Option<ExactInteger<'gc>>, max: Option<ExactInteger<'gc>>) -> Value<'gc> {
        let Some(min) = min else {
            return nctx.return_(Value::new(rand::random::<f64>()));
        };

        let Some(max) = max else {
            match min {
                ExactInteger::Fixnum(n) => {
                    return nctx.return_(Value::new((rand::random::<u32>() % n as u32) as i32));
                }

                ExactInteger::BigInt(b) => {
                    let max_bitwidth = b.bits();
                    let mut rng = rand::rng();
                    let rb = BigInt::random(nctx.ctx, &mut rng, max_bitwidth);
                    let rb = BigInt::rem(rb, nctx.ctx, b);
                    return nctx.return_(rb.into());
                }
            }
        };

        match (min, max) {
            (ExactInteger::Fixnum(min_val), ExactInteger::Fixnum(max_val)) => {
                if min_val >= max_val {
                    let ctx = nctx.ctx;
                    let min = min.into_value(ctx);
                    let max = max.into_value(ctx);
                    return nctx.wrong_argument_violation(
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
                return nctx.return_(Value::new(random_val));
            }

            (ExactInteger::BigInt(_), ExactInteger::Fixnum(_)) => {
                let ctx = nctx.ctx;
                let min = min.into_value(ctx);
                let max = max.into_value(ctx);
                return nctx.wrong_argument_violation(
                    "random",
                    "max must be greater than min",
                    Some(max),
                    Some(2),
                    2,
                    &[min, max],
                );
            }

            (ExactInteger::Fixnum(min), ExactInteger::BigInt(max)) => {
                let min_bigint = BigInt::from_i32(nctx.ctx, min);

                let range = BigInt::minus(max, nctx.ctx, min_bigint);
                let max_bitwidth = range.bits();
                let mut rng = rand::rng();
                let rb = BigInt::random(nctx.ctx, &mut rng, max_bitwidth);
                let rb = BigInt::rem(rb, nctx.ctx, range);
                let result = BigInt::plus(rb, nctx.ctx, min_bigint);
                return nctx.return_(result.into());
            }

            (ExactInteger::BigInt(min), ExactInteger::BigInt(max)) => {
                if min >= max {
                    let ctx = nctx.ctx;
                    let min = min.into_value(ctx);
                    let max = max.into_value(ctx);
                    return nctx.wrong_argument_violation(
                        "random",
                        "min must be less than max",
                        Some(min),
                        Some(1),
                        2,
                        &[min, max],
                    );
                }

                let range = BigInt::minus(max, nctx.ctx, min);
                let max_bitwidth = range.bits();
                let mut rng = rand::rng();
                let rb = BigInt::random(nctx.ctx, &mut rng, max_bitwidth);
                let rb = BigInt::rem(rb, nctx.ctx, range);
                let result = BigInt::plus(rb, nctx.ctx, min);
                return nctx.return_(result.into());
            }
        }
    }

    #[scheme(name = "=")]
    pub fn equal(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            let eq = Number::equal(nctx.ctx, w, w);
            return nctx.return_(Ok(eq));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "=",
                    "argument must be a number",
                    Some(z.clone()),
                    Some(1),
                    rest.len() + 1,
                    &[w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            };
            if !Number::equal(nctx.ctx, w, z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "<")]
    pub fn lt(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "<",
                    "argument must be a number",
                    Some(z.clone()),
                    Some(1),
                    rest.len() + 1,
                    &[w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp != Some(Ordering::Less) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "<=")]
    pub fn le(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "<=",
                    "argument must be a number",
                    Some(z.clone()),
                    Some(1),
                    rest.len() + 1,
                    &[w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if !matches!(cmp, Some(Ordering::Less) | Some(Ordering::Equal)) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = ">")]
    pub fn gt(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    ">",
                    "argument must be a number",
                    Some(z.clone()),
                    Some(1),
                    rest.len() + 1,
                    &[w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp != Some(Ordering::Greater) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = ">=")]
    pub fn ge(w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    ">=",
                    "argument must be a number",
                    Some(z.clone()),
                    Some(1),
                    rest.len() + 1,
                    &[w.into_value(ctx)]
                        .iter()
                        .chain(rest.iter())
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp == Some(Ordering::Less) || cmp.is_none() {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "positive?")]
    pub fn is_positive(w: Number<'gc>) -> bool {
        nctx.return_(w.is_positive())
    }

    #[scheme(name = "negative?")]
    pub fn is_negative(w: Number<'gc>) -> bool {
        nctx.return_(w.is_negative())
    }

    #[scheme(name = "zero?")]
    pub fn is_zero(w: Number<'gc>) -> bool {
        nctx.return_(w.is_zero())
    }

    #[scheme(name = "fixnum?")]
    pub fn is_fixnum(w: Value<'gc>) -> bool {
        nctx.return_(w.is_int32())
    }

    #[scheme(name = "bigint?")]
    pub fn is_bigint(w: Number<'gc>) -> bool {
        nctx.return_(w.is_bigint())
    }

    #[scheme(name = "complex?")]
    pub fn is_complex(w: Value<'gc>) -> bool {
        nctx.return_(w.is_number())
    }

    #[scheme(name = "real?")]
    pub fn is_real(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return nctx.return_(false);
        };

        nctx.return_(n.is_real())
    }

    #[scheme(name = "inexact-real?")]
    pub fn is_inexact_real(w: Number<'gc>) -> bool {
        nctx.return_(w.is_real() && !w.is_exact())
    }

    #[scheme(name = "rational?")]
    pub fn is_rational(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return nctx.return_(false);
        };

        nctx.return_(n.is_rational())
    }

    #[scheme(name = "exact-positive-integer?")]
    pub fn is_exact_positive_integer(w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_positive_integer())
    }

    #[scheme(name = "exact-nonnegative-integer?")]
    pub fn is_exact_nonnegative_integer(w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_non_negative_integer())
    }

    #[scheme(name = "exact-integer?")]
    pub fn is_exact_integer(w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_integer())
    }

    #[scheme(name = "integer?")]
    pub fn is_integer(w: Value<'gc>) -> bool {
        let Some(n) = w.number() else {
            return nctx.return_(false);
        };

        nctx.return_(n.is_integer())
    }

    #[scheme(name = "number?")]
    pub fn is_number(w: Value<'gc>) -> bool {
        nctx.return_(w.is_number())
    }

    #[scheme(name = "number->string")]
    pub fn number_to_string(n: Number<'gc>, base: Option<u8>) -> Gc<'gc, Str<'gc>> {
        let base = base.unwrap_or(10);
        let ctx = nctx.ctx;
        if base < 2 || base > 36 {
            return nctx.wrong_argument_violation(
                "number->string",
                "base must be in range 2 to 36",
                Some(base.into_value(ctx)),
                Some(1),
                2,
                &[n.into_value(ctx), base.into_value(ctx)],
            );
        }

        let s = Str::new(&ctx, n.to_string_radix(base), false);
        nctx.return_(s)
    }

    #[scheme(name = "fx=?")]
    pub fn fx_eq(w: i32, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fx=?",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_int32();
            if w != z {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "fxlogand")]
    pub fn fxlogand(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxlogand",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w &= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "logior")]
    pub fn logior(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
        if rest.is_empty() {
            return nctx.return_(w);
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(nctx.ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return nctx.conversion_error("logior", e);
                }
            };
            w = ExactInteger::bitor(nctx.ctx, w, z);
        }

        nctx.return_(w)
    }

    #[scheme(name = "logxor")]
    pub fn logxor(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
        if rest.is_empty() {
            return nctx.return_(w);
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(nctx.ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return nctx.conversion_error("logxor", e);
                }
            };
            w = ExactInteger::bitxor(nctx.ctx, w, z);
        }

        nctx.return_(w)
    }

    #[scheme(name = "logand")]
    pub fn logand(w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
        if rest.is_empty() {
            return nctx.return_(w);
        }
        let mut w = w;

        for &z in rest.iter() {
            let z = match ExactInteger::try_from_value(nctx.ctx, z) {
                Ok(z) => z,
                Err(e) => {
                    return nctx.conversion_error("logand", e);
                }
            };
            w = ExactInteger::bitand(nctx.ctx, w, z);
        }

        nctx.return_(w)
    }

    #[scheme(name = "lognot")]
    pub fn lognot(w: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let w = ExactInteger::bitnot(nctx.ctx, w);
        nctx.return_(w)
    }

    #[scheme(name = "fxlogior")]
    pub fn fxlogior(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxlogior",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w |= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "fxlogxor")]
    pub fn fxlogxor(w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxlogxor",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w ^= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "real-valued?")]
    pub fn is_real_valued(w: Number<'gc>) -> bool {
        if w.is_real() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero());
        }

        nctx.return_(false)
    }

    #[scheme(name = "rational-valued?")]
    pub fn is_rational_valued(w: Number<'gc>) -> bool {
        if w.is_rational() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero() && c.real.is_rational());
        }

        nctx.return_(false)
    }

    #[scheme(name = "integer-valued?")]
    pub fn is_integer_valued(w: Number<'gc>) -> bool {
        if w.is_integer() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero() && c.real.is_integer());
        }

        nctx.return_(false)
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
        let c = Number::Complex(Complex::new(nctx.ctx, real, imag));
        nctx.return_(c)
    }

    #[scheme(name = "make-polar")]
    pub fn make_polar(r: Number<'gc>, theta: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !r.is_real_valued() {
            let r = r.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "make-polar",
                "magnitude must be a real number",
                Some(r),
                Some(1),
                2,
                &[r],
            );
        }
        if !theta.is_real_valued() {
            let theta = theta.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "make-polar",
                "angle must be a real number",
                Some(theta),
                Some(2),
                2,
                &[theta],
            );
        }
        let c = r.polar(nctx.ctx, theta);
        nctx.return_(Ok(c))
    }

    #[scheme(name = "real-part")]
    pub fn real_part(z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            nctx.return_(c.real)
        } else {
            nctx.return_(z)
        }
    }

    #[scheme(name = "imag-part")]
    pub fn imag_part(z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            nctx.return_(c.imag)
        } else {
            nctx.return_(Number::Fixnum(0))
        }
    }

    #[scheme(name = "angle")]
    pub fn angle(z: Number<'gc>) -> Number<'gc> {
        let angle = z.angle(nctx.ctx);
        nctx.return_(angle)
    }

    #[scheme(name = "flonum?")]
    pub fn is_flonum(w: Number<'gc>) -> bool {
        nctx.return_(w.is_flonum())
    }

    #[scheme(name = "fl=?")]
    pub fn fl_eq(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fl=?",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if w != z {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "fl<?")]
    pub fn fl_lt(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fl<?",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if !(w < z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "fl>?")]
    pub fn fl_gt(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fl>?",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if !(w > z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "fl<=?")]
    pub fn fl_le(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fl<=?",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if !(w <= z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "fl>=?")]
    pub fn fl_ge(w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            if w.is_nan() {
                return nctx.return_(Ok(false));
            }

            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fl>=?",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if !(w >= z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    #[scheme(name = "flinteger?")]
    pub fn is_fl_integer(w: f64) -> bool {
        nctx.return_(w == w.trunc() && !w.is_infinite() && !w.is_nan())
    }

    #[scheme(name = "flzero?")]
    pub fn is_fl_zero(w: f64) -> bool {
        nctx.return_(w == 0.0)
    }

    #[scheme(name = "flpositive?")]
    pub fn is_fl_positive(w: f64) -> bool {
        nctx.return_(w > 0.0)
    }

    #[scheme(name = "flnegative?")]
    pub fn is_fl_negative(w: f64) -> bool {
        nctx.return_(w < 0.0)
    }

    #[scheme(name = "flodd?")]
    pub fn is_fl_odd(w: f64) -> bool {
        nctx.return_(w % 2.0 != 0.0)
    }

    #[scheme(name = "fleven?")]
    pub fn is_fl_even(w: f64) -> bool {
        nctx.return_(w % 2.0 == 0.0)
    }

    #[scheme(name = "flfinite?")]
    pub fn is_fl_finite(w: f64) -> bool {
        nctx.return_(w.is_finite())
    }

    #[scheme(name = "flinfinite?")]
    pub fn is_fl_infinite(w: f64) -> bool {
        nctx.return_(w.is_infinite())
    }

    #[scheme(name = "flnan?")]
    pub fn is_fl_nan(w: f64) -> bool {
        nctx.return_(w.is_nan())
    }

    #[scheme(name = "flmax")]
    pub fn fl_max(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "flmax",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[max.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if z > max || max.is_nan() {
                max = z;
            }
        }

        nctx.return_(Ok(max))
    }

    #[scheme(name = "flmin")]
    pub fn fl_min(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "flmin",
                    "argument must be a flonum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[min.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_flonum();
            if z < min || min.is_nan() {
                min = z;
            }
        }

        nctx.return_(Ok(min))
    }

    #[scheme(name = "fl+")]
    pub fn fl_plus(args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(0.0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                return nctx.wrong_argument_violation(
                    "fl+",
                    "argument must be a flonum",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].flonum() else {
            return nctx.wrong_argument_violation(
                "fl+",
                "argument must be a flonum",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                return nctx.wrong_argument_violation(
                    "fl+",
                    "argument must be a flonum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            acc += arg;
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fl-")]
    pub fn fl_minus(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-x));
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation(
                "fl-",
                "argument must be a flonum",
                Some(rest[0].clone()),
                Some(1),
                args.len(),
                &args,
            );
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "fl-",
                    "argument must be a flonum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    &args,
                );
            };
            acc -= arg;
        }

        acc = x - acc;

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fl*")]
    pub fn fl_times(args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(1.0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                return nctx.wrong_argument_violation(
                    "fl*",
                    "argument must be a flonum",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].flonum() else {
            return nctx.wrong_argument_violation(
                "fl*",
                "argument must be a flonum",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                return nctx.wrong_argument_violation(
                    "fl*",
                    "argument must be a flonum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            acc *= arg;
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fl/")]
    pub fn fl_div(x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            if x == 0.0 {
                let x = x.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "fl/",
                    "division by zero",
                    Some(x),
                    Some(1),
                    1,
                    &[x],
                );
            }
            return nctx.return_(Ok(1.0 / x));
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation(
                "fl/",
                "argument must be a flonum",
                Some(rest[0].clone()),
                Some(1),
                args.len(),
                &args,
            );
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "fl/",
                    "argument must be a flonum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    &args,
                );
            };
            if arg == 0.0 {
                let arg = arg.into_value(nctx.ctx);
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(x.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "fl/",
                    "division by zero",
                    Some(arg),
                    Some(2),
                    args.len(),
                    &args,
                );
            }
            acc /= arg;
        }

        if acc == 0.0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let acc = acc.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fl/",
                "division by zero",
                Some(acc),
                Some(2),
                2,
                &[x, acc],
            );
        }

        acc = x / acc;

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fldiv")]
    pub fn fl_divide(x: f64, y: f64) -> Result<f64, Value<'gc>> {
        if y == 0.0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fldiv",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        let res = x / y;
        nctx.return_(Ok(res))
    }

    #[scheme(name = "fldiv0")]
    pub fn fl_divide0(x: f64, y: f64) -> Number<'gc> {
        if y == 0.0 {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "fldiv0",
                "division by zero",
                Some(y),
                Some(1),
                1,
                &[y],
            );
        }
        let res = Number::integer_div0(ctx, Number::Flonum(x), Number::Flonum(y));
        nctx.return_(res)
    }

    #[scheme(name = "fxmod")]
    pub fn fx_mod(x: i32, y: i32) -> i32 {
        if y == 0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fxmod",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        // (fx- x (fx* (fxdiv x y) y))
        let r = x - ((x / y) * y);
        nctx.return_(r)
    }

    #[scheme(name = "bitwise-arithmetic-shift")]
    pub fn bitwise_arithmetic_shift(x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if !x.is_exact_integer() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "first argument must be an exact integer",
                Some(x),
                Some(1),
                2,
                &[x],
            );
        }
        if !y.is_exact_integer() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "second argument must be an exact integer",
                Some(y),
                Some(2),
                2,
                &[y],
            );
        }

        let Some(shifted) = x.ash(nctx.ctx, y) else {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation(
                "bitwise-arithmetic-shift",
                "shift amount is too large",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        };
        nctx.return_(shifted)
    }

    #[scheme(name = "bitwise-bit-count")]
    pub fn bitwise_bit_count(x: ExactInteger<'gc>) -> i32 {
        let count = x.bit_count(nctx.ctx);
        nctx.return_(count)
    }

    #[scheme(name = "bitwise-length")]
    pub fn bitwise_bit_length(x: ExactInteger<'gc>) -> u32 {
        let length = x.bit_length(nctx.ctx);
        nctx.return_(length)
    }

    #[scheme(name = "bitwise-first-bit-set")]
    pub fn bitwise_first_bit_set(x: ExactInteger<'gc>) -> i32 {
        let pos = ExactInteger::first_bit_set(nctx.ctx, x);
        nctx.return_(pos)
    }

    #[scheme(name = "bitwise-not")]
    pub fn bitwise_not(x: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let not = ExactInteger::bitnot(nctx.ctx, x);
        nctx.return_(not)
    }

    #[scheme(name = "bitwise-and")]
    pub fn bitwise_and(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(-1i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation(
                    "bitwise-and",
                    "argument must be a number",
                    Some(rest[0].clone()),
                    Some(1),
                    1,
                    &[rest[0].clone()],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "bitwise-and",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(nctx.ctx);

            return nctx.return_(n);
        }

        if rest.len() == 2 {
            let a = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
                Ok(a) => a,
                Err(e) => return nctx.conversion_error("bitwise-and", e),
            };
            let b = match ExactInteger::try_from_value(nctx.ctx, rest[1]) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-and", e),
            };
            let res = ExactInteger::bitand(nctx.ctx, a, b);
            return nctx.return_(res.into());
        }

        let mut acc = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return nctx.conversion_error("bitwise-and", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(nctx.ctx, *v) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-and", e),
            };
            acc = ExactInteger::bitand(nctx.ctx, acc, b);
        }

        nctx.return_(acc.into())
    }

    #[scheme(name = "bitwise-ior")]
    pub fn bitwise_ior(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(0i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation(
                    "bitwise-ior",
                    "argument must be a number",
                    Some(rest[0].clone()),
                    Some(1),
                    1,
                    &[rest[0].clone()],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "bitwise-ior",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(nctx.ctx);

            return nctx.return_(n);
        }

        if rest.len() == 2 {
            let a = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
                Ok(a) => a,
                Err(e) => return nctx.conversion_error("bitwise-ior", e),
            };
            let b = match ExactInteger::try_from_value(nctx.ctx, rest[1]) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-ior", e),
            };
            let res = ExactInteger::bitor(nctx.ctx, a, b);
            return nctx.return_(res.into());
        }

        let mut acc = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return nctx.conversion_error("bitwise-ior", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(nctx.ctx, *v) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-ior", e),
            };
            acc = ExactInteger::bitor(nctx.ctx, acc, b);
        }

        nctx.return_(acc.into())
    }

    #[scheme(name = "bitwise-xor")]
    pub fn bitwise_xor(rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(0i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation(
                    "bitwise-xor",
                    "argument must be a number",
                    Some(rest[0].clone()),
                    Some(1),
                    1,
                    &[rest[0].clone()],
                );
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation(
                    "bitwise-xor",
                    "argument must be an exact integer",
                    Some(n),
                    Some(1),
                    1,
                    &[n],
                );
            }
            let n = n.into_value(nctx.ctx);

            return nctx.return_(n);
        }

        if rest.len() == 2 {
            let a = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
                Ok(a) => a,
                Err(e) => return nctx.conversion_error("bitwise-xor", e),
            };
            let b = match ExactInteger::try_from_value(nctx.ctx, rest[1]) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-xor", e),
            };
            let res = ExactInteger::bitxor(nctx.ctx, a, b);
            return nctx.return_(res.into());
        }

        let mut acc = match ExactInteger::try_from_value(nctx.ctx, rest[0]) {
            Ok(a) => a,
            Err(e) => return nctx.conversion_error("bitwise-xor", e),
        };

        for v in &rest[1..] {
            let b = match ExactInteger::try_from_value(nctx.ctx, *v) {
                Ok(b) => b,
                Err(e) => return nctx.conversion_error("bitwise-xor", e),
            };
            acc = ExactInteger::bitxor(nctx.ctx, acc, b);
        }

        nctx.return_(acc.into())
    }

    #[scheme(name = "fixnum-width")]
    pub fn fixnum_width() -> i32 {
        nctx.return_(32)
    }

    #[scheme(name = "least-fixnum")]
    pub fn least_fixnum() -> i32 {
        nctx.return_(i32::MIN)
    }

    #[scheme(name = "greatest-fixnum")]
    pub fn greatest_fixnum() -> i32 {
        nctx.return_(i32::MAX)
    }

    #[scheme(name = "fx<?")]
    pub fn fx_lt(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(a < b));
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a < b) {
                return nctx.return_(Ok(false));
            }
            a = b;
            b = match rest.at(nctx.ctx, i) {
                Ok(v) => v,
                Err(e) => return nctx.conversion_error("fx<?", e),
            };
        }

        nctx.return_(Ok(a < b))
    }

    #[scheme(name = "fx<=?")]
    pub fn fx_le(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(a <= b));
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a <= b) {
                return nctx.return_(Ok(false));
            }
            a = b;
            b = match rest.at(nctx.ctx, i) {
                Ok(v) => v,
                Err(e) => return nctx.conversion_error("fx<=?", e),
            };
        }

        nctx.return_(Ok(a <= b))
    }

    #[scheme(name = "fx>?")]
    pub fn fx_gt(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(a > b));
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a > b) {
                return nctx.return_(Ok(false));
            }
            a = b;
            b = match rest.at(nctx.ctx, i) {
                Ok(v) => v,
                Err(e) => return nctx.conversion_error("fx>?", e),
            };
        }

        nctx.return_(Ok(a > b))
    }

    #[scheme(name = "fx>=?")]
    pub fn fx_ge(a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(a >= b));
        }
        let mut a = a;
        let mut b = b;

        for i in 0..rest.len() {
            if !(a >= b) {
                return nctx.return_(Ok(false));
            }
            a = b;
            b = match rest.at(nctx.ctx, i) {
                Ok(v) => v,
                Err(e) => return nctx.conversion_error("fx>=?", e),
            };
        }

        nctx.return_(Ok(a >= b))
    }

    #[scheme(name = "fxzero?")]
    pub fn fxzerop(w: i32) -> bool {
        nctx.return_(w == 0)
    }

    #[scheme(name = "fxpositive?")]
    pub fn fxpositivep(w: i32) -> bool {
        nctx.return_(w > 0)
    }

    #[scheme(name = "fxnegative?")]
    pub fn fxnegativep(w: i32) -> bool {
        nctx.return_(w < 0)
    }

    #[scheme(name = "fxodd?")]
    pub fn fxoddp(w: i32) -> bool {
        nctx.return_(w % 2 != 0)
    }

    #[scheme(name = "fxeven?")]
    pub fn fxevenp(w: i32) -> bool {
        nctx.return_(w % 2 == 0)
    }

    #[scheme(name = "fxmax")]
    pub fn fx_max(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxmax",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[max.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_int32();
            if z > max {
                max = z;
            }
        }

        nctx.return_(Ok(max))
    }

    #[scheme(name = "fxmin")]
    pub fn fx_min(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxmin",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[min.into_value(ctx), z.clone()],
                );
            }
            let z = z.as_int32();
            if z < min {
                min = z;
            }
        }

        nctx.return_(Ok(min))
    }

    #[scheme(name = "fx+")]
    pub fn fx_plus(args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                return nctx.wrong_argument_violation(
                    "fx+",
                    "argument must be a fixnum",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].int32() else {
            return nctx.wrong_argument_violation(
                "fx+",
                "argument must be a fixnum",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                return nctx.wrong_argument_violation(
                    "fx+",
                    "argument must be a fixnum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            match acc.checked_add(arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.implementation_restriction_violation(
                        "fx+",
                        "integer overflow",
                        &[acc, arg],
                    );
                }
            }
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fx-")]
    pub fn fx_minus(x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-x));
        }

        let Some(mut acc) = rest[0].int32() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation(
                "fx-",
                "argument must be a fixnum",
                Some(rest[0].clone()),
                Some(1),
                args.len(),
                &args,
            );
        };

        acc = match x.checked_sub(acc) {
            Some(v) => v,
            None => {
                let ctx = nctx.ctx;
                let x = x.into_value(ctx);
                let acc = acc.into_value(ctx);
                return nctx.implementation_restriction_violation(
                    "fx-",
                    "integer overflow",
                    &[x, acc],
                );
            }
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.int32() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation(
                    "fx-",
                    "argument must be a fixnum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    &args,
                );
            };
            match acc.checked_sub(arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.implementation_restriction_violation(
                        "fx-",
                        "integer overflow",
                        &[acc, arg],
                    );
                }
            }
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fx*")]
    pub fn fx_times(args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(1));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                return nctx.wrong_argument_violation(
                    "fx*",
                    "argument must be a fixnum",
                    Some(args[0].clone()),
                    Some(0),
                    1,
                    args,
                );
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].int32() else {
            return nctx.wrong_argument_violation(
                "fx*",
                "argument must be a fixnum",
                Some(args[0].clone()),
                Some(0),
                args.len(),
                args,
            );
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                return nctx.wrong_argument_violation(
                    "fx*",
                    "argument must be a fixnum",
                    Some(arg.clone()),
                    Some(1),
                    args.len(),
                    args,
                );
            };
            match acc.checked_mul(arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.implementation_restriction_violation(
                        "fx*",
                        "integer overflow",
                        &[acc, arg],
                    );
                }
            }
        }

        nctx.return_(Ok(acc))
    }

    #[scheme(name = "fxdiv")]
    pub fn fx_div(x: i32, y: i32) -> Result<i32, Value<'gc>> {
        if y == 0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fxdiv",
                "division by zero",
                Some(y),
                Some(2),
                2,
                &[x, y],
            );
        }
        let Some(res) = x.checked_div(y) else {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.implementation_restriction_violation("fxdiv", "integer overflow", &[x, y]);
        };
        nctx.return_(Ok(res))
    }

    #[scheme(name = "fxdiv0")]
    pub fn fx_div0(x: i32, y: i32) -> Result<i32, Value<'gc>> {
        if y == 0 {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "fxdiv0",
                "division by zero",
                Some(y),
                Some(1),
                1,
                &[y],
            );
        }
        let Some(div) = x.checked_div(y) else {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.implementation_restriction_violation(
                "fxdiv0",
                "integer overflow",
                &[x, y],
            );
        };
        let mod_ = x - (div * y);
        if mod_ < (y / 2).abs() {
            return nctx.return_(Ok(div));
        }
        if y > 0 {
            return nctx.return_(Ok(div + 1));
        } else {
            return nctx.return_(Ok(div - 1));
        }
    }

    #[scheme(name = "fxnot")]
    pub fn fx_not(w: i32) -> i32 {
        nctx.return_(!w)
    }

    #[scheme(name = "fxand")]
    pub fn fx_and(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-1));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation(
                "fxand",
                "argument must be a fixnum",
                Some(rest[0].clone()),
                Some(0),
                rest.len(),
                rest,
            );
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxand",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w &= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "fxior")]
    pub fn fxior(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(0));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation(
                "fxior",
                "argument must be a fixnum",
                Some(rest[0].clone()),
                Some(0),
                rest.len(),
                rest,
            );
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxior",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w |= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "fxxor")]
    pub fn fxxor(rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(0));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation(
                "fxxor",
                "argument must be a fixnum",
                Some(rest[0].clone()),
                Some(0),
                rest.len(),
                rest,
            );
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation(
                    "fxxor",
                    "argument must be a fixnum",
                    Some(z.clone()),
                    Some(1),
                    2,
                    &[w.into_value(ctx), z.clone()],
                );
            }

            let z = z.as_int32();
            w ^= z;
        }

        nctx.return_(Ok(w))
    }

    #[scheme(name = "fxif")]
    pub fn fxif(fx1: i32, fx2: i32, fx3: i32) -> i32 {
        nctx.return_((fx1 & fx2) | (!fx1 & fx3))
    }

    #[scheme(name = "fxbit-count")]
    pub fn fxbit_count(w: i32) -> i32 {
        nctx.return_(w.count_ones() as i32)
    }

    #[scheme(name = "fxlength")]
    pub fn fxlength(w: i32) -> i32 {
        if w == 0 {
            return nctx.return_(0);
        }
        nctx.return_(32 - w.leading_zeros() as i32)
    }

    #[scheme(name = "fxfirst-bit-set")]
    pub fn fxfirst_bit_set(w: i32) -> i32 {
        if w == 0 {
            return nctx.return_(-1);
        }
        nctx.return_((w & -w).trailing_zeros() as i32 + 1)
    }

    #[scheme(name = "fxbit-set?")]
    pub fn fxbit_setp(w: i32, k: i32) -> Result<bool, Value<'gc>> {
        if k < 0 || k >= 32 {
            let ctx = nctx.ctx;
            let w = w.into_value(ctx);
            let k = k.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fxbit-set?",
                "bit index out of range",
                Some(k),
                Some(2),
                2,
                &[w, k],
            );
        }
        nctx.return_(Ok((w & (1 << k)) != 0))
    }

    #[scheme(name = "fxcopy-bit")]
    pub fn fxcopy_bit(fx1: i32, fx2: i32, fx3: i32) -> i32 {
        if fx2 >= 0 && fx2 < 32 {
            if fx3 == 0 || fx3 == 1 {
                let mask = 1 << fx2;
                return nctx.return_((mask & (fx3 << fx2)) | (!mask & fx1));
            } else {
                let ctx = nctx.ctx;
                let fx1 = fx1.into_value(ctx);
                let fx2 = fx2.into_value(ctx);
                let fx3 = fx3.into_value(ctx);
                return nctx.wrong_argument_violation(
                    "fxcopy-bit",
                    "third argument must be 0 or 1",
                    Some(fx3),
                    Some(3),
                    3,
                    &[fx1, fx2, fx3],
                );
            }
        }

        let ctx = nctx.ctx;
        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        let fx3 = fx3.into_value(ctx);
        nctx.wrong_argument_violation(
            "fxcopy-bit",
            "bit index out of range",
            Some(fx2),
            Some(2),
            3,
            &[fx1, fx2, fx3],
        )
    }

    #[scheme(name = "fxarithmetic-shift")]
    pub fn fxarithmetic_shift(fx1: i32, fx2: i32) -> i32 {
        if fx2 > -32 && fx2 < 32 {
            let n;
            if fx2 > 0 {
                n = fx1.wrapping_shl(fx2 as u32);
                if (n.wrapping_shr(fx2 as u32) == fx1) && (n >= i32::MIN) && n <= i32::MAX {
                    return nctx.return_(n);
                }
            } else {
                n = fx1.wrapping_shr((-fx2) as u32);
                if (n.wrapping_shl((-fx2) as u32) == fx1) && (n >= i32::MIN) && n <= i32::MAX {
                    return nctx.return_(n);
                }
            }
            let ctx = nctx.ctx;
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return nctx.implementation_restriction_violation(
                "fxarithmetic-shift",
                "shift amount is too large",
                &[fx1, fx2],
            );
        }

        let ctx = nctx.ctx;
        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        nctx.implementation_restriction_violation(
            "fxarithmetic-shift",
            "shift amount is too large",
            &[fx1, fx2],
        )
    }

    #[scheme(name = "fxarithmetic-shift-left")]
    pub fn fxarithmetic_shift_left(fx1: i32, fx2: u32) -> i32 {
        let Some(n) = fx1.checked_shl(fx2) else {
            let ctx = nctx.ctx;
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return nctx.implementation_restriction_violation(
                "fxarithmetic-shift-left",
                "shift amount is too large",
                &[fx1, fx2],
            );
        };
        nctx.return_(n)
    }

    #[scheme(name = "fxarithmetic-shift-right")]
    pub fn fxarithmetic_shift_right(fx1: i32, fx2: u32) -> i32 {
        let Some(n) = fx1.checked_shr(fx2) else {
            let ctx = nctx.ctx;
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return nctx.implementation_restriction_violation(
                "fxarithmetic-shift-right",
                "shift amount is too large",
                &[fx1, fx2],
            );
        };
        nctx.return_(n)
    }

    #[scheme(name = "fxbit-field")]
    pub fn fxbit_field(fx1: i32, fx2: i32, fx3: i32) -> i32 {
        let ctx = nctx.ctx;
        if fx2 < 0 || fx2 >= 32 {
            return nctx.wrong_argument_violation(
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

        if fx3 < 0 || fx3 >= 32 {
            return nctx.wrong_argument_violation(
                "fxbit-field",
                "third argument must be in the range 0 to 31",
                Some(fx3.into()),
                Some(3),
                3,
                &[fx1.into(), fx2.into(), fx3.into()],
            );
        }

        if fx2 > fx3 {
            return nctx.wrong_argument_violation(
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
        return nctx.return_((fx1 & mask) >> fx2);
    }

    #[scheme(name = "fxcopy-bit-field")]
    pub fn fxcopy_bit_field(fx1: i32, fx2: i32, fx3: i32, fx4: i32) -> i32 {
        let mask1 = -1 << fx2;
        let mask2 = !(-1 << fx3);
        let mask = mask1 & !mask2;
        nctx.return_((mask & (fx4 << fx2)) | (!mask & fx1))
    }

    #[scheme(name = "fixnum->flonum")]
    pub fn fixnum_to_flonum(x: i32) -> f64 {
        nctx.return_(x as f64)
    }

    #[scheme(name = "real->flonum")]
    pub fn real_to_flonum(x: Number<'gc>) -> f64 {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation(
                "real->flonum",
                "argument must be a real number",
                Some(x),
                Some(1),
                1,
                &[x],
            );
        }

        let fl = x.real_to_f64(nctx.ctx);

        nctx.return_(fl)
    }

    #[scheme(name = "flsqrt")]
    pub fn fl_sqrt(x: f64) -> f64 {
        let res = x.sqrt();
        nctx.return_(res)
    }

    #[scheme(name = "flabs")]
    pub fn fl_abs(x: f64) -> f64 {
        let res = x.abs();
        nctx.return_(res)
    }

    #[scheme(name = "flatan")]
    pub fn fl_atan(x: f64) -> f64 {
        let res = x.atan();
        nctx.return_(res)
    }

    #[scheme(name = "flacos")]
    pub fn fl_acos(x: f64) -> f64 {
        let res = x.acos();
        nctx.return_(res)
    }

    #[scheme(name = "flasin")]
    pub fn fl_asin(x: f64) -> f64 {
        let res = x.asin();
        nctx.return_(res)
    }

    #[scheme(name = "fllog")]
    pub fn fl_log(x: f64) -> f64 {
        let res = x.ln();
        nctx.return_(res)
    }

    #[scheme(name = "fltan")]
    pub fn fl_tan(x: f64) -> f64 {
        let res = x.tan();
        nctx.return_(res)
    }

    #[scheme(name = "flcos")]
    pub fn fl_cos(x: f64) -> f64 {
        let res = x.cos();
        nctx.return_(res)
    }

    #[scheme(name = "flsin")]
    pub fn fl_sin(x: f64) -> f64 {
        let res = x.sin();
        nctx.return_(res)
    }

    #[scheme(name = "flexpt")]
    pub fn fl_expt(x: f64, y: f64) -> f64 {
        let res = x.powf(y);
        nctx.return_(res)
    }

    #[scheme(name = "flexp")]
    pub fn fl_exp(x: f64) -> f64 {
        let res = x.exp();
        nctx.return_(res)
    }

    #[scheme(name = "flround")]
    pub fn fl_round(x: f64) -> f64 {
        let res = x.round();
        nctx.return_(res)
    }

    #[scheme(name = "fltruncate")]
    pub fn fl_truncate(x: f64) -> f64 {
        let res = x.trunc();
        nctx.return_(res)
    }

    #[scheme(name = "flceiling")]
    pub fn fl_ceiling(x: f64) -> f64 {
        let res = x.ceil();
        nctx.return_(res)
    }

    #[scheme(name = "flfloor")]
    pub fn fl_floor(x: f64) -> f64 {
        let res = x.floor();
        nctx.return_(res)
    }

    #[scheme(name = "fldenominator")]
    pub fn fl_denominator(x: f64) -> Number<'gc> {
        let n = Number::Flonum(x).to_exact(nctx.ctx);
        let res = if let Number::Rational(r) = n {
            r.denominator.to_inexact(nctx.ctx)
        } else {
            Number::Flonum(1.0)
        };
        nctx.return_(res)
    }

    #[scheme(name = "flnumerator")]
    pub fn fl_numerator(x: f64) -> Number<'gc> {
        let n = Number::Flonum(x).to_exact(nctx.ctx);
        let res = if let Number::Rational(r) = n {
            r.numerator.to_inexact(nctx.ctx)
        } else {
            Number::Flonum(x)
        };
        nctx.return_(res)
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

        nctx.return_((mantissa, exp, sign))
    }
}

pub(crate) fn init<'gc>(ctx: Context<'gc>) {
    arith_operations::register(ctx);
    //register_arith(ctx);
}
