use std::cmp::Ordering;

use crate::{
    native_fn,
    runtime::value::conversions::*,
    runtime::{Context, prelude::*},
};

native_fn!(
    register_arith:
    pub ("exact?") fn is_exact<'gc>(nctx, x: Number<'gc>) -> bool {
        nctx.return_(x.is_exact())
    }

    pub ("inexact?") fn is_inexact<'gc>(nctx, x: Number<'gc>) -> bool {
        nctx.return_(!x.is_exact())
    }

    pub ("odd?") fn is_odd<'gc>(nctx, x: Number<'gc>) -> bool {
        let is_odd = match x {
            Number::Fixnum(i) => i % 2 != 0,
            Number::BigInt(b) => b.is_odd(),
            _ => x.real_to_f64(nctx.ctx) % 2.0 != 0.0,
        };
        nctx.return_(is_odd)
    }

    pub ("even?") fn is_even<'gc>(nctx, x: Number<'gc>) -> bool {
        let is_even = match x {
            Number::Fixnum(i) => i % 2 == 0,
            Number::BigInt(b) => !b.is_odd(),
            _ => x.real_to_f64(nctx.ctx) % 2.0 == 0.0,
        };
        nctx.return_(is_even)
    }

    pub ("quotient") fn quotient<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("quotient", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }

        let result = Number::quotient(nctx.ctx, x, y);

        nctx.return_(result)
    }

    pub ("remainder") fn remainder<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("remainder", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }

        let result = Number::remainder(nctx.ctx, x, y);

        nctx.return_(result)
    }

    pub ("modulo") fn modulo<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            let x = x.into_value(nctx.ctx);
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("modulo", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }

        let result = Number::modulo(nctx.ctx, x, y);

        nctx.return_(result)
    }


    pub ("-") fn minus<'gc>(nctx, x: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
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
                return nctx.wrong_argument_violation("-", "argument must be a number", Some(arg.clone()), Some(position), args.len(), &args);
            };
            acc = Number::sub(nctx.ctx, acc, arg);
            position += 1;
        }

        nctx.return_(Ok(acc))
    }

    pub ("inexact") fn inexact<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let n = x.to_inexact(nctx.ctx);

        nctx.return_(n)
    }

    pub ("exact") fn exact<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let n = x.to_exact(nctx.ctx);

        nctx.return_(n)
    }

    pub ("+") fn plus<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(Number::Fixnum(0)));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                return nctx.wrong_argument_violation("+", "argument must be a number", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else {
            return nctx.wrong_argument_violation("+", "argument must be a number", Some(args[0].clone()), Some(0), args.len(), args);
         };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                return nctx.wrong_argument_violation("+", "argument must be a number", Some(arg.clone()), Some(1), args.len(), args);
             };
            acc = Number::add(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    pub ("*") fn times<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(Number::Fixnum(1)));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else {
                return nctx.wrong_argument_violation("*", "argument must be a number", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else {
            return nctx.wrong_argument_violation("*", "argument must be a number", Some(args[0].clone()), Some(0), args.len(), args);
        };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else {
                return nctx.wrong_argument_violation("*", "argument must be a number", Some(arg.clone()), Some(1), args.len(), args);
            };
            acc = Number::mul(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    pub ("/") fn div<'gc>(nctx, acc: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            if acc.is_zero() {
                let acc = acc.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("/", "division by zero", Some(acc), Some(1), 1, &[acc]);
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
                return nctx.wrong_argument_violation("/", "argument must be a number", Some(arg.clone()), Some(1), args.len(), &args);
            };
            acc = Number::div(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    pub ("div") fn integer_div<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div", "first argument must be a real number", Some(x), Some(1), 2, &[x]);
        }

        if !x.is_finite() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div", "first argument must be a finite number", Some(x), Some(1), 2, &[x]);
        }

        if !y.is_real() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div", "second argument must be a real number", Some(y), Some(2), 2, &[y]);
        }

        if y.is_zero() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div", "division by zero", Some(y), Some(2), 2, &[y]);
        }
        let res = Number::integer_div(nctx.ctx, x, y);
        nctx.return_(res)
    }

    pub ("div0") fn integer_div0<'gc>(nctx, x: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div0", "first argument must be a real number", Some(x), Some(1), 2, &[x]);
        }

        if !x.is_finite() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div0", "first argument must be a finite number", Some(x), Some(1), 2, &[x]);
        }

        if !rhs.is_real() {
            let rhs = rhs.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div0", "second argument must be a real number", Some(rhs), Some(2), 2, &[rhs]);
        }

        if rhs.is_zero() {
            let rhs = rhs.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("div0", "division by zero", Some(rhs), Some(2), 2, &[rhs]);
        }

        let res = Number::integer_div0(nctx.ctx, x, rhs);
        nctx.return_(res)
    }

    pub ("denominator") fn denominator<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        if !x.is_real_valued() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("denominator", "argument must be a real number", Some(x), Some(1), 1, &[x]);
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

    pub ("expt") fn expt<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        let expt = Number::expt(nctx.ctx, x, y);

        nctx.return_(expt)
    }

    pub ("exp") fn exp<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let exp = Number::exp(nctx.ctx, x);
        nctx.return_(exp)
    }

    pub ("log") fn log<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let log = Number::log(nctx.ctx, x);
        nctx.return_(log)
    }

    pub ("sin") fn sin<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let sin = Number::sin(nctx.ctx, x);
        nctx.return_(sin)
    }

    pub ("cos") fn cos<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let cos = Number::cos(nctx.ctx, x);
        nctx.return_(cos)
    }

    pub ("tan") fn tan<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let tan = Number::tan(nctx.ctx, x);
        nctx.return_(tan)
    }

    pub ("sqrt") fn sqrt<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let sqrt = Number::sqrt(nctx.ctx, x);
        nctx.return_(sqrt)
    }

    pub ("magnitude") fn magnitude<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let magnitude = Number::magnitude(nctx.ctx, x);
        nctx.return_(magnitude)
    }

    pub ("exact-integer-sqrt") fn exact_integer_sqrt<'gc>(nctx, x: Number<'gc>) -> Result<(Number<'gc>, Number<'gc>), Value<'gc>> {
        if !x.is_exact_integer() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("exact-integer-sqrt", "argument must be an exact integer", Some(x), Some(1), 1, &[x]);
        }
        let sqrt = Number::exact_integer_sqrt(nctx.ctx, x);
        nctx.return_(Ok(sqrt))
    }

    pub ("inverse") fn inverse<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let inverse = Number::inverse(nctx.ctx, x);
        nctx.return_(inverse)
    }

    pub ("asin") fn asin<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let asin = Number::asin(nctx.ctx, x);
        nctx.return_(asin)
    }

    pub ("acos") fn acos<'gc>(nctx, x: Number<'gc>) -> Number<'gc> {
        let acos = Number::acos(nctx.ctx, x);
        nctx.return_(acos)
    }

    pub ("floor") fn floor<'gc>(nctx, x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("floor", "argument must be a real number", Some(x), Some(1), 1, &[x]);
        }
        let floor = x.floor(nctx.ctx);
        nctx.return_(Ok(floor))
    }

    pub ("ceiling") fn ceiling<'gc>(nctx, x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero() {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("ceiling", "argument must be a real number", Some(x), Some(1), 1, &[x]);
        }
        let ceiling = x.ceiling(nctx.ctx);
        nctx.return_(Ok(ceiling))
    }

    pub ("truncate") fn truncate<'gc>(nctx, x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero() {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("truncate", "argument must be a real number", Some(x), Some(1), 1, &[x]);
        }
        let truncate = x.truncate(nctx.ctx);
        nctx.return_(Ok(truncate))
    }

    pub ("round") fn round<'gc>(nctx, x: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        let x = if let Number::Complex(cn) = x
            && cn.imag.is_zero() {
            x
        } else {
            x
        };
        if !x.is_real() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("round", "argument must be a real number", Some(x), Some(1), 1, &[x]);
        }
        let round = x.round(nctx.ctx);
        nctx.return_(Ok(round))
    }


    pub ("=") fn equal<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            let eq = Number::equal(nctx.ctx, w, w);
            return nctx.return_(Ok(eq));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("=", "argument must be a number", Some(z.clone()), Some(1), rest.len() + 1, &[w.into_value(ctx)].iter().chain(rest.iter()).cloned().collect::<Vec<_>>().as_slice());
           };
            if !Number::equal(nctx.ctx, w, z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("<") fn lt<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("<", "argument must be a number", Some(z.clone()), Some(1), rest.len() + 1, &[w.into_value(ctx)].iter().chain(rest.iter()).cloned().collect::<Vec<_>>().as_slice());
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp != Some(Ordering::Less) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("<=") fn le<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("<=", "argument must be a number", Some(z.clone()), Some(1), rest.len() + 1, &[w.into_value(ctx)].iter().chain(rest.iter()).cloned().collect::<Vec<_>>().as_slice());
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if !matches!(cmp, Some(Ordering::Less) | Some(Ordering::Equal)) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub (">") fn gt<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation(">", "argument must be a number", Some(z.clone()), Some(1), rest.len() + 1, &[w.into_value(ctx)].iter().chain(rest.iter()).cloned().collect::<Vec<_>>().as_slice());
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp != Some(Ordering::Greater) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub (">=") fn ge<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation(">=", "argument must be a number", Some(z.clone()), Some(1), rest.len() + 1, &[w.into_value(ctx)].iter().chain(rest.iter()).cloned().collect::<Vec<_>>().as_slice());
            };
            let cmp = Number::compare(nctx.ctx, w, z);
            if cmp == Some(Ordering::Less) || cmp.is_none() {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("positive?") fn is_positive<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_positive())
    }

    pub ("negative?") fn is_negative<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_negative())
    }

    pub ("zero?") fn is_zero<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_zero())
    }

    pub ("fixnum?") fn is_fixnum<'gc>(nctx, w: Value<'gc>) -> bool {
        nctx.return_(w.is_int32())
    }

    pub("bigint?") fn is_bigint<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_bigint())
    }

    pub ("complex?") fn is_complex<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_complex())
    }

    pub ("real?") fn is_real<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_real())
    }

    pub ("inexact-real?") fn is_inexact_real<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_real() && !w.is_exact())
    }

    pub ("rational?") fn is_rational<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_rational())
    }

    pub ("exact-positive-integer?") fn is_exact_positive_integer<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_positive_integer())
    }

    pub ("exact-nonnegative-integer?") fn is_exact_nonnegative_integer<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_non_negative_integer())
    }

    pub ("exact-integer?") fn is_exact_integer<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_exact_integer())
    }

    pub ("integer?") fn is_integer<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_integer())
    }

    pub ("number?") fn is_number<'gc>(nctx, w: Value<'gc>) -> bool {
        nctx.return_(w.is_number())
    }

    pub ("number->string") fn number_to_string<'gc>(nctx, n: Number<'gc>, base: Option<u8>) -> Gc<'gc, Str<'gc>> {
        let base = base.unwrap_or(10);
        let ctx = nctx.ctx;
        if base < 2 || base > 36 {
            return nctx.wrong_argument_violation("number->string", "base must be in range 2 to 36", Some(base.into_value(ctx)), Some(1), 2, &[n.into_value(ctx), base.into_value(ctx)]);
        }

        let s = Str::new(&ctx, n.to_string_radix(base), false);
        nctx.return_(s)
    }


    pub ("fx=?") fn fx_eq<'gc>(nctx, w: i32, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fx=?", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_int32();
            if w != z {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("fxlogand") fn fxlogand<'gc>(nctx, w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxlogand", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w &= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("logior") fn logior<'gc>(nctx, w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
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

    pub ("logxor") fn logxor<'gc>(nctx, w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
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

    pub ("logand") fn logand<'gc>(nctx, w: ExactInteger<'gc>, rest: &'gc [Value<'gc>]) -> ExactInteger<'gc> {
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

    pub ("lognot") fn lognot<'gc>(nctx, w: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let w = ExactInteger::bitnot(nctx.ctx, w);
        nctx.return_(w)
    }

    pub ("fxlogior") fn fxlogior<'gc>(nctx, w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxlogior", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w |= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("fxlogxor") fn fxlogxor<'gc>(nctx, w: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(w));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxlogxor", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w ^= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("real-valued?") fn is_real_valued<'gc>(nctx, w: Number<'gc>) -> bool {
        if w.is_real() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero());
        }

        nctx.return_(false)
    }

    pub ("rational-valued?") fn is_rational_valued<'gc>(nctx, w: Number<'gc>) -> bool {
        if w.is_rational() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero() && c.real.is_rational());
        }

        nctx.return_(false)
    }

    pub ("integer-valued?") fn is_integer_valued<'gc>(nctx, w: Number<'gc>) -> bool {
        if w.is_integer() {
            return nctx.return_(true);
        }

        if let Number::Complex(c) = w {
            return nctx.return_(c.imag.is_zero() && c.real.is_integer());
        }

        nctx.return_(false)
    }

    pub ("make-rectangular") fn make_rectangular<'gc>(nctx, real: Number<'gc>, imag: Number<'gc>) -> Number<'gc> {
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

    pub ("make-polar") fn make_polar<'gc>(nctx, r: Number<'gc>, theta: Number<'gc>) -> Result<Number<'gc>, Value<'gc>> {
        if !r.is_real_valued() {
            let r = r.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("make-polar", "magnitude must be a real number", Some(r), Some(1), 2, &[r]);
        }
        if !theta.is_real_valued() {
            let theta = theta.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("make-polar", "angle must be a real number", Some(theta), Some(2), 2, &[theta]);
        }
        let c = r.polar(nctx.ctx, theta);
        nctx.return_(Ok(c))
    }

    pub ("real-part") fn real_part<'gc>(nctx, z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            nctx.return_(c.real)
        } else {
            nctx.return_(z)
        }
    }

    pub ("imag-part") fn imag_part<'gc>(nctx, z: Number<'gc>) -> Number<'gc> {
        if let Number::Complex(c) = z {
            nctx.return_(c.imag)
        } else {
            nctx.return_(Number::Fixnum(0))
        }
    }

    pub ("angle") fn angle<'gc>(nctx, z: Number<'gc>) -> Number<'gc> {
        let angle = z.angle(nctx.ctx);
        nctx.return_(angle)
    }

    pub ("flonum?") fn is_flonum<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_flonum())
    }

    pub ("fl=?") fn fl_eq<'gc>(nctx, w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fl=?", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if w != z {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("fl<?") fn fl_lt<'gc>(nctx, w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("fl<?", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if !(w < z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("fl>?") fn fl_gt<'gc>(nctx, w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("fl>?", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if !(w > z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("fl<=?") fn fl_le<'gc>(nctx, w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("fl<=?", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if !(w <= z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("fl>=?") fn fl_ge<'gc>(nctx, w: f64, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
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
                return nctx.wrong_argument_violation("fl>=?", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if !(w >= z) {
                return nctx.return_(Ok(false));
            }
            w = z;
        }

        nctx.return_(Ok(true))
    }

    pub ("flinteger?") fn is_fl_integer<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w == w.trunc() && !w.is_infinite() && !w.is_nan())
    }

    pub ("flzero?") fn is_fl_zero<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w == 0.0)
    }

    pub ("flpositive?") fn is_fl_positive<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w > 0.0)
    }

    pub ("flnegative?") fn is_fl_negative<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w < 0.0)
    }

    pub ("flodd?") fn is_fl_odd<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w % 2.0 != 0.0)
    }

    pub ("fleven?") fn is_fl_even<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w % 2.0 == 0.0)
    }

    pub ("flfinite?") fn is_fl_finite<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w.is_finite())
    }

    pub ("flinfinite?") fn is_fl_infinite<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w.is_infinite())
    }

    pub ("flnan?") fn is_fl_nan<'gc>(nctx, w: f64) -> bool {
        nctx.return_(w.is_nan())
    }

    pub ("flmax") fn fl_max<'gc>(nctx, x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("flmax", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[max.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if z > max || max.is_nan() {
                max = z;
            }
        }

        nctx.return_(Ok(max))
    }

    pub ("flmin") fn fl_min<'gc>(nctx, x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_flonum() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("flmin", "argument must be a flonum", Some(z.clone()), Some(1), 2, &[min.into_value(ctx), z.clone()]);
            }
            let z = z.as_flonum();
            if z < min || min.is_nan() {
                min = z;
            }
        }

        nctx.return_(Ok(min))
    }

    pub ("fl+") fn fl_plus<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(0.0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                return nctx.wrong_argument_violation("fl+", "argument must be a flonum", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].flonum() else {
            return nctx.wrong_argument_violation("fl+", "argument must be a flonum", Some(args[0].clone()), Some(0), args.len(), args);
         };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                return nctx.wrong_argument_violation("fl+", "argument must be a flonum", Some(arg.clone()), Some(1), args.len(), args);
             };
            acc += arg;
        }

        nctx.return_(Ok(acc))
    }

    pub ("fl-") fn fl_minus<'gc>(nctx, x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-x));
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation("fl-", "argument must be a flonum", Some(rest[0].clone()), Some(1), args.len(), &args);
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation("fl-", "argument must be a flonum", Some(arg.clone()), Some(1), args.len(), &args);
            };
            acc -= arg;
        }

        acc = x - acc;

        nctx.return_(Ok(acc))
    }

    pub ("fl*") fn fl_times<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(1.0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].flonum() else {
                return nctx.wrong_argument_violation("fl*", "argument must be a flonum", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].flonum() else {
            return nctx.wrong_argument_violation("fl*", "argument must be a flonum", Some(args[0].clone()), Some(0), args.len(), args);
         };

        for arg in args[1..].iter() {
            let Some(arg) = arg.flonum() else {
                return nctx.wrong_argument_violation("fl*", "argument must be a flonum", Some(arg.clone()), Some(1), args.len(), args);
             };
            acc *= arg;
        }

        nctx.return_(Ok(acc))
    }

    pub ("fl/") fn fl_div<'gc>(nctx, x: f64, rest: &'gc [Value<'gc>]) -> Result<f64, Value<'gc>> {
        if rest.is_empty() {
            if x == 0.0 {
                let x = x.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("fl/", "division by zero", Some(x), Some(1), 1, &[x]);
            }
            return nctx.return_(Ok(1.0 / x));
        }

        let Some(mut acc) = rest[0].flonum() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation("fl/", "argument must be a flonum", Some(rest[0].clone()), Some(1), args.len(), &args);
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.flonum() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation("fl/", "argument must be a flonum", Some(arg.clone()), Some(1), args.len(), &args);
            };
            if arg == 0.0 {
                let arg = arg.into_value(nctx.ctx);
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(x.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation("fl/", "division by zero", Some(arg), Some(2), args.len(), &args);
            }
            acc /= arg;
        }

        if acc == 0.0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let acc = acc.into_value(ctx);
            return nctx.wrong_argument_violation("fl/", "division by zero", Some(acc), Some(2), 2, &[x, acc]);
        }

        acc = x / acc;

        nctx.return_(Ok(acc))
    }

    pub ("fldiv") fn fl_divide<'gc>(nctx, x: f64, y: f64) -> Result<f64, Value<'gc>> {
        if y == 0.0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation("fldiv", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }
        let res = x / y;
        nctx.return_(Ok(res))
    }

    pub ("fldiv0") fn fl_divide0<'gc>(nctx, y: f64) -> Result<f64, Value<'gc>> {
        if y == 0.0 {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("fldiv0", "division by zero", Some(y), Some(1), 1, &[y]);
        }
        let res = 1.0 / y;
        nctx.return_(Ok(res))
    }

    pub ("flnumerator") fn fl_numerator<'gc>(nctx, x: f64) -> Value<'gc> {
        if x == 0.0 {
            return nctx.return_(Value::new(0.0f64));
        }
        let ctx = nctx.ctx;
        let obj = Number::Flonum(x).to_exact(ctx);
        if let Number::Rational(r) = obj {
            nctx.return_(r.numerator.into_value(ctx))
        } else {
            nctx.return_(obj.to_inexact(ctx).into_value(ctx))
        }
    }

    pub ("fxmod") fn fx_mod<'gc>(nctx, x: i32, y: i32) -> i32 {
        if y == 0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation("fxmod", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }
        // (fx- x (fx* (fxdiv x y) y))
        let r = x - ((x / y) * y);
        nctx.return_(r)
    }

    pub ("bitwise-arithmetic-shift") fn bitwise_arithmetic_shift<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if !x.is_exact_integer() {
            let x = x.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("bitwise-arithmetic-shift", "first argument must be an exact integer", Some(x), Some(1), 2, &[x]);
        }
        if !y.is_exact_integer() {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("bitwise-arithmetic-shift", "second argument must be an exact integer", Some(y), Some(2), 2, &[y]);
        }

        let Some(shifted) = x.ash(nctx.ctx, y) else {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation("bitwise-arithmetic-shift", "shift amount is too large", Some(y), Some(2), 2, &[x, y]);
        };
        nctx.return_(shifted)
    }

    pub ("bitwise-bit-count") fn bitwise_bit_count<'gc>(nctx, x: ExactInteger<'gc>) -> u32 {
        let count = x.bit_count();
        nctx.return_(count)
    }

    pub ("bitwise-bit-length") fn bitwise_bit_length<'gc>(nctx, x: ExactInteger<'gc>) -> u32 {
        let length = x.bit_length(nctx.ctx);
        nctx.return_(length)
    }

    pub ("bitwise-first-bit-set") fn bitwise_first_bit_set<'gc>(nctx, x: ExactInteger<'gc>) -> i32 {
        let pos = ExactInteger::first_bit_set(nctx.ctx, x);
        nctx.return_(pos)
    }

    pub ("bitwise-not") fn bitwise_not<'gc>(nctx, x: ExactInteger<'gc>) -> ExactInteger<'gc> {
        let not = ExactInteger::bitnot(nctx.ctx, x);
        nctx.return_(not)
    }

    pub ("bitwise-and") fn bitwise_and<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(-1i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation("bitwise-and", "argument must be a number", Some(rest[0].clone()), Some(1), 1, &[rest[0].clone()]);
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("bitwise-and", "argument must be an exact integer", Some(n), Some(1), 1, &[n]);
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
    pub ("bitwise-ior") fn bitwise_ior<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(0i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation("bitwise-ior", "argument must be a number", Some(rest[0].clone()), Some(1), 1, &[rest[0].clone()]);
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("bitwise-ior", "argument must be an exact integer", Some(n), Some(1), 1, &[n]);
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

    pub ("bitwise-xor") fn bitwise_xor<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if rest.is_empty() {
            return nctx.return_(Value::new(0i32));
        }

        if rest.len() == 1 {
            let Some(n) = rest[0].number() else {
                return nctx.wrong_argument_violation("bitwise-xor", "argument must be a number", Some(rest[0].clone()), Some(1), 1, &[rest[0].clone()]);
            };

            if !n.is_exact_integer() {
                let n = n.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("bitwise-xor", "argument must be an exact integer", Some(n), Some(1), 1, &[n]);
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

    pub ("fixnum-width") fn fixnum_width<'gc>(nctx) -> i32 {
        nctx.return_(32)
    }

    pub ("least-fixnum") fn least_fixnum<'gc>(nctx) -> i32 {
        nctx.return_(i32::MIN)
    }

    pub ("greatest-fixnum") fn greatest_fixnum<'gc>(nctx) -> i32 {
        nctx.return_(i32::MAX)
    }

    pub ("fx<?") fn fx_lt<'gc>(nctx, a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
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

    pub ("fx<=?") fn fx_le<'gc>(nctx, a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
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

    pub ("fx>?") fn fx_gt<'gc>(nctx, a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
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

    pub ("fx>=?") fn fx_ge<'gc>(nctx, a: i32, b: i32, rest: RestOf<'gc, 'gc, i32>) -> Result<bool, Value<'gc>> {
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

    pub ("fxzero?") fn fxzerop<'gc>(nctx, w: i32) -> bool {
        nctx.return_(w == 0)
    }

    pub ("fxpositive?") fn fxpositivep<'gc>(nctx, w: i32) -> bool {
        nctx.return_(w > 0)
    }

    pub ("fxnegative?") fn fxnegativep<'gc>(nctx, w: i32) -> bool {
        nctx.return_(w < 0)
    }

    pub ("fxodd?") fn fxoddp<'gc>(nctx, w: i32) -> bool {
        nctx.return_(w % 2 != 0)
    }

    pub ("fxeven?") fn fxevenp<'gc>(nctx, w: i32) -> bool {
        nctx.return_(w % 2 == 0)
    }

    pub ("fxmax") fn fx_max<'gc>(nctx, x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut max = x;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxmax", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[max.into_value(ctx), z.clone()]);
            }
            let z = z.as_int32();
            if z > max {
                max = z;
            }
        }

        nctx.return_(Ok(max))
    }

    pub ("fxmin") fn fx_min<'gc>(nctx, x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        let mut min = x;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxmin", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[min.into_value(ctx), z.clone()]);
            }
            let z = z.as_int32();
            if z < min {
                min = z;
            }
        }

        nctx.return_(Ok(min))
    }

    pub ("fx+") fn fx_plus<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(0));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                return nctx.wrong_argument_violation("fx+", "argument must be a fixnum", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].int32() else {
            return nctx.wrong_argument_violation("fx+", "argument must be a fixnum", Some(args[0].clone()), Some(0), args.len(), args);
         };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                return nctx.wrong_argument_violation("fx+", "argument must be a fixnum", Some(arg.clone()), Some(1), args.len(), args);
             };
            match acc.checked_add(arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.wrong_argument_violation("fx+", "integer overflow", Some(arg), Some(1), args.len(), &[acc, arg]);
                }
            }
        }

        nctx.return_(Ok(acc))
    }

    pub ("fx-") fn fx_minus<'gc>(nctx, x: i32, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-x));
        }

        let Some(mut acc) = rest[0].int32() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(x.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation("fx-", "argument must be a fixnum", Some(rest[0].clone()), Some(1), args.len(), &args);
        };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.int32() else {
                let mut args = Vec::with_capacity(1 + rest.len());
                args.push(acc.into_value(nctx.ctx));
                args.extend_from_slice(rest);
                return nctx.wrong_argument_violation("fx-", "argument must be a fixnum", Some(arg.clone()), Some(1), args.len(), &args);
            };
            match acc.checked_add(-arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.wrong_argument_violation("fx-", "integer overflow", Some(arg), Some(1), rest.len(), &[acc, arg]);
                }
            }
        }

        match acc.checked_neg() {
            Some(v) => acc = v,
            None => {
                let ctx = nctx.ctx;
                let x = x.into_value(ctx);
                let acc = acc.into_value(ctx);
                return nctx.wrong_argument_violation("fx-", "integer overflow", Some(acc), Some(1), 2, &[x, acc]);
            }
        }

        nctx.return_(Ok(acc))
    }

    pub ("fx*") fn fx_times<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(1));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].int32() else {
                return nctx.wrong_argument_violation("fx*", "argument must be a fixnum", Some(args[0].clone()), Some(0), 1, args);
            };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].int32() else {
            return nctx.wrong_argument_violation("fx*", "argument must be a fixnum", Some(args[0].clone()), Some(0), args.len(), args);
         };

        for arg in args[1..].iter() {
            let Some(arg) = arg.int32() else {
                return nctx.wrong_argument_violation("fx*", "argument must be a fixnum", Some(arg.clone()), Some(1), args.len(), args);
             };
            match acc.checked_mul(arg) {
                Some(v) => acc = v,
                None => {
                    let ctx = nctx.ctx;
                    let acc = acc.into_value(ctx);
                    let arg = arg.into_value(ctx);
                    return nctx.wrong_argument_violation("fx*", "integer overflow", Some(arg), Some(1), args.len(), &[acc, arg]);
                }
            }
        }

        nctx.return_(Ok(acc))
    }

    pub ("fxdiv") fn fx_div<'gc>(nctx, x: i32, y: i32) -> Result<i32, Value<'gc>> {
        if y == 0 {
            let ctx = nctx.ctx;
            let x = x.into_value(ctx);
            let y = y.into_value(ctx);
            return nctx.wrong_argument_violation("fxdiv", "division by zero", Some(y), Some(2), 2, &[x, y]);
        }
        let res = x / y;
        nctx.return_(Ok(res))
    }

    pub ("fxdiv0") fn fx_div0<'gc>(nctx, x: i32, y: i32) -> Result<i32, Value<'gc>> {
        if y == 0 {
            let y = y.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("fxdiv0", "division by zero", Some(y), Some(1), 1, &[y]);
        }
        let div = x / y;
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

    pub ("fxnot") fn fx_not<'gc>(nctx, w: i32) -> i32 {
        nctx.return_(!w)
    }

    pub ("fxand") fn fx_and<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(-1));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation("fxand", "argument must be a fixnum", Some(rest[0].clone()), Some(0), rest.len(), rest);
        };


        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxand", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w &= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("fxior") fn fxior<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(0));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation("fxior", "argument must be a fixnum", Some(rest[0].clone()), Some(0), rest.len(), rest);
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxior", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w |= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("fxxor") fn fxxor<'gc>(nctx, rest: &'gc [Value<'gc>]) -> Result<i32, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(0));
        }
        let Some(mut w) = rest[0].int32() else {
            return nctx.wrong_argument_violation("fxxor", "argument must be a fixnum", Some(rest[0].clone()), Some(0), rest.len(), rest);
        };

        for z in rest.iter().skip(1) {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("fxxor", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
            }

            let z = z.as_int32();
            w ^= z;
        }

        nctx.return_(Ok(w))
    }

    pub ("fxif") fn fxif<'gc>(nctx, fx1: i32, fx2: i32, fx3: i32) -> i32 {
        nctx.return_((fx1 & fx2) | (!fx1 & fx3))
    }

    pub ("fxbit-count") fn fxbit_count<'gc>(nctx, w: i32) -> i32 {
        nctx.return_(w.count_ones() as i32)
    }

    pub ("fxlength") fn fxlength<'gc>(nctx, w: i32) -> i32 {
        if w == 0 {
            return nctx.return_(0);
        }
        nctx.return_(32 - w.leading_zeros() as i32)
    }

    pub ("fxfirst-bit-set") fn fxfirst_bit_set<'gc>(nctx, w: i32) -> i32 {
        if w == 0 {
            return nctx.return_(-1);
        }
        nctx.return_((w & -w).trailing_zeros() as i32 + 1)
    }

    pub ("fxbit-set?") fn fxbit_setp<'gc>(nctx, w: i32, k: i32) -> Result<bool, Value<'gc>> {
        if k < 0 || k >= 32 {
            let ctx = nctx.ctx;
            let w = w.into_value(ctx);
            let k = k.into_value(ctx);
            return nctx.wrong_argument_violation("fxbit-set?", "bit index out of range", Some(k), Some(2), 2, &[w, k]);
        }
        nctx.return_(Ok((w & (1 << k)) != 0))
    }

    pub ("fxcopy-bit") fn fxcopy_bit<'gc>(
        nctx,
        fx1: i32,
        fx2: i32,
        fx3: i32
    ) -> i32 {
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
                    &[fx1, fx2, fx3]
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
            &[fx1, fx2, fx3]
        )
    }

    pub ("fxarithmetic-shift") fn fxarithmetic_shift<'gc>(
        nctx,
        fx1: i32,
        fx2: i32
    ) -> i32 {
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
            return nctx.wrong_argument_violation(
                "fxarithmetic-shift",
                "shift amount is too large",
                Some(fx2),
                Some(2),
                2,
                &[fx1, fx2]
            );
        }

        let ctx = nctx.ctx;
        let fx1 = fx1.into_value(ctx);
        let fx2 = fx2.into_value(ctx);
        nctx.wrong_argument_violation(
            "fxarithmetic-shift",
            "shift amount is too large",
            Some(fx2),
            Some(2),
            2,
            &[fx1, fx2]
        )
    }

    pub ("fxarithmetic-shift-left") fn fxarithmetic_shift_left<'gc>(
        nctx,
        fx1: i32,
        fx2: u32
    ) -> i32 {
        let Some(n) = fx1.checked_shl(fx2) else {
            let ctx = nctx.ctx;
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fxarithmetic-shift-left",
                "shift amount is too large",
                Some(fx2),
                Some(2),
                2,
                &[fx1, fx2]
            );
        };
        nctx.return_(n)
    }

    pub ("fxarithmetic-shift-right") fn fxarithmetic_shift_right<'gc>(
        nctx,
        fx1: i32,
        fx2: u32
    ) -> i32 {
        let Some(n) = fx1.checked_shr(fx2) else {
            let ctx = nctx.ctx;
            let fx1 = fx1.into_value(ctx);
            let fx2 = fx2.into_value(ctx);
            return nctx.wrong_argument_violation(
                "fxarithmetic-shift-right",
                "shift amount is too large",
                Some(fx2),
                Some(2),
                2,
                &[fx1, fx2]
            );
        };
        nctx.return_(n)
    }

    pub ("fxbit-field") fn fxbit_field<'gc>(
        nctx,
        fx1: i32,
        fx2: i32,
        fx3: i32
    ) -> i32 {
        let ctx = nctx.ctx;
        if fx2 < 0 || fx2 >= 32 {
            return nctx.wrong_argument_violation(
                "fxbit-field",
                "second argument must be in the range 0 to 31",
                Some(fx2.into_value(ctx)),
                Some(2),
                3,
                &[fx1.into_value(ctx), fx2.into_value(ctx), fx3.into_value(ctx)]
            );
        }

        if fx3 < 0 || fx3 >= 32 {
            return nctx.wrong_argument_violation(
                "fxbit-field",
                "third argument must be in the range 0 to 31",
                Some(fx3.into()),
                Some(3),
                3,
                &[fx1.into(), fx2.into(), fx3.into()]
            );
        }

        if fx2 > fx3 {
            return nctx.wrong_argument_violation(
                "fxbit-field",
                "second argument must be less than or equal to the third argument",
                Some(fx2.into_value(ctx)),
                Some(2),
                3,
                &[fx1.into_value(ctx), fx2.into_value(ctx), fx3.into_value(ctx)]
            );
        }

        let mask = !(-1 << fx3);
        return nctx.return_((fx1 & mask) >> fx2);
    }

    pub ("fxcopy-bit-field") fn fxcopy_bit_field<'gc>(
        nctx,
        fx1: i32,
        fx2: i32,
        fx3: i32,
        fx4: i32
    ) -> i32 {
        let mask1 = -1 << fx2;
        let mask2 = !(-1 << fx3);
        let mask = mask1 & !mask2;
        nctx.return_((mask & (fx4 << fx2)) | (!mask & fx1))
    }
);

pub(crate) fn init<'gc>(ctx: Context<'gc>) {
    register_arith(ctx);
}
