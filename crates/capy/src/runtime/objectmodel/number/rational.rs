use super::*;

impl<'gc> Rational<'gc> {
    pub fn add(ctx: MutationContext<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::add(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn mul(ctx: MutationContext<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.numerator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn sub(ctx: MutationContext<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::sub(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn div(ctx: MutationContext<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.denominator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.numerator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn to_f64(&self, ctx: MutationContext<'gc>) -> f64 {
        let mut nume = self.numerator.real_to_f64(ctx);
        let mut deno = self.denominator.real_to_f64(ctx);

        if nume.is_infinite() || deno.is_infinite() {
            if nume.is_infinite() && deno.is_infinite() {
                let nume_bitsize =
                    BigInt::bitsize(self.numerator.as_bigint().expect("value is bigint"));
                let deno_bitsize =
                    BigInt::bitsize(self.denominator.as_bigint().expect("value is bigint"));
                let mut shift = if nume_bitsize > deno_bitsize {
                    nume_bitsize as isize - 96
                } else {
                    deno_bitsize as isize - 96
                };

                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(
                    self.numerator.as_bigint().expect("value is bigint"),
                    ctx,
                    shift as _,
                )
                .as_f64();
                deno = BigInt::shift_right(
                    self.denominator.as_bigint().expect("value is bigint"),
                    ctx,
                    shift as _,
                )
                .as_f64();
            } else if deno.is_infinite() {
                let deno_bitsize =
                    BigInt::bitsize(self.denominator.as_bigint().expect("value is bigint"));
                let mut shift = deno_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = libm::ldexp(nume, -(shift as i32));
                deno = BigInt::shift_right(
                    self.denominator.as_bigint().expect("value is bigint"),
                    ctx,
                    shift as _,
                )
                .as_f64();
            } else {
                let nume_bitsize =
                    BigInt::bitsize(self.numerator.as_bigint().expect("value is bigint"));
                let mut shift = nume_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(
                    self.numerator.as_bigint().expect("value is bigint"),
                    ctx,
                    shift as _,
                )
                .as_f64();
                deno = libm::ldexp(deno, -(shift as i32));
            }
        }

        nume / deno
    }
}
