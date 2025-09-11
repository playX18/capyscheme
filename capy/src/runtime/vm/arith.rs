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
            todo!()
        }

        let result = Number::quotient(nctx.ctx, x, y);

        nctx.return_(result)
    }

    pub ("remainder") fn remainder<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            todo!()
        }

        let result = Number::remainder(nctx.ctx, x, y);

        nctx.return_(result)
    }

    pub ("modulo") fn modulo<'gc>(nctx, x: Number<'gc>, y: Number<'gc>) -> Number<'gc> {
        if y.is_zero() {
            todo!()
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

        for arg in rest.iter() {
            let Some(arg) = arg.number() else { todo!() };
            acc = Number::sub(nctx.ctx, acc, arg);
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
            let Some(arg) = args[0].number() else { todo!() };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else { todo!() };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else { todo!() };
            acc = Number::add(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    pub ("*") fn times<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if args.len() == 0 {
            return nctx.return_(Ok(Number::Fixnum(1)));
        }

        if args.len() == 1 {
            let Some(arg) = args[0].number() else { todo!() };
            return nctx.return_(Ok(arg));
        }

        let Some(mut acc) = args[0].number() else { todo!() };

        for arg in args[1..].iter() {
            let Some(arg) = arg.number() else { todo!() };
            acc = Number::mul(nctx.ctx, acc, arg);
        }

        nctx.return_(Ok(acc))
    }

    pub ("/") fn div<'gc>(nctx, z: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<Number<'gc>, Value<'gc>> {
        if rest.is_empty() {
            if z.is_zero() {
                todo!()
            }
            let one = Number::Fixnum(1);
            let res = Number::div(nctx.ctx, one, z);
            return nctx.return_(Ok(res));
        }

        let Some(mut acc) = rest[0].number() else { todo!() };

        for arg in rest[1..].iter() {
            let Some(arg) = arg.number() else { todo!() };
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
            todo!()
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
            todo!()
        }
        let floor = Number::floor(nctx.ctx, x);
        nctx.return_(Ok(floor))
    }

    pub ("=") fn equal<'gc>(nctx, w: Number<'gc>, rest: &'gc [Value<'gc>]) -> Result<bool, Value<'gc>> {
        if rest.is_empty() {
            let eq = Number::equal(nctx.ctx, w, w);
            return nctx.return_(Ok(eq));
        }
        let mut w = w;

        for z in rest.iter() {
            let Some(z) = z.number() else { todo!() };
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
            let Some(z) = z.number() else { todo!() };
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
            let Some(z) = z.number() else { todo!() };
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
            let Some(z) = z.number() else { todo!() };
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
            let Some(z) = z.number() else { todo!() };
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
);

pub(crate) fn init<'gc>(ctx: Context<'gc>) {
    register_arith(ctx);
}
