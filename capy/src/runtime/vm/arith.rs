use std::cmp::Ordering;

use crate::{
    native_fn,
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

    pub ("/") fn div<'gc>(nctx, z: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            if z.is_zero() {
                let z = z.into_value(nctx.ctx);
                return nctx.wrong_argument_violation("/", "division by zero", Some(z), Some(1), 1, &[z]);
            }
            let one = Number::Fixnum(1);
            let res = Number::div(nctx.ctx, one, z);
            return nctx.return_(Ok(res));
        }

        let Some(mut acc) = rest[0].number() else {
            let mut args = Vec::with_capacity(1 + rest.len());
            args.push(z.into_value(nctx.ctx));
            args.extend_from_slice(rest);
            return nctx.wrong_argument_violation("/", "argument must be a number", Some(rest[0].clone()), Some(1), args.len(), &args);
        };

        for arg in rest[1..].iter() {
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
            if cmp != Some(Ordering::Less) {
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

    pub ("fixnum?") fn is_fixnum<'gc>(nctx, w: Number<'gc>) -> bool {
        nctx.return_(w.is_fixnum())
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


    pub ("=") fn fx_eq<'gc>(nctx, w: i32, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            return nctx.return_(Ok(true));
        }
        let mut w = w;

        for z in rest.iter() {
            if !z.is_int32() {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("=", "argument must be a fixnum", Some(z.clone()), Some(1), 2, &[w.into_value(ctx), z.clone()]);
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
);

pub(crate) fn init<'gc>(ctx: Context<'gc>) {
    register_arith(ctx);
}
