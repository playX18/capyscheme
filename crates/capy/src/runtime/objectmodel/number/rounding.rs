use super::*;

impl<'gc> Number<'gc> {
    pub fn floor(self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn ceiling(self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn truncate(self, ctx: MutationContext<'gc>) -> Self {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self,
            Self::Flonum(fl) => Self::Flonum(fl.trunc()),

            Self::Rational(rn) => Self::quotient(ctx, rn.numerator, rn.denominator),

            _ => unreachable!(),
        }
    }

    pub fn round(self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn to_exact(self, ctx: MutationContext<'gc>) -> Self {
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

}
