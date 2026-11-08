use super::*;

impl<'gc> Number<'gc> {
    pub fn is_negative(&self) -> bool {
        match self {
            Number::Fixnum(i) => *i < 0,
            Number::Flonum(f) => *f < 0.0,
            Number::BigInt(b) => b.is_negative(),
            Number::Rational(r) => r.numerator.is_negative(),
            Number::Complex(c) => c.real.is_negative() || c.imag.is_negative(),
        }
    }

    pub fn negate(&self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn to_inexact(&self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn add(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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

    pub fn sub(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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
                    // `lhs - numerator/denominator` is
                    // `(lhs*denominator - numerator)/denominator`. The
                    // operands of the subtraction used to be swapped here,
                    // which computed `(numerator - lhs*denominator)/d`.
                    Self::sub(
                        ctx,
                        Self::mul(ctx, rn.denominator, Self::BigInt(lhs)),
                        rn.numerator,
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

    pub fn mul(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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
                Number::BigInt(rhs) => BigInt::mul_i64(rhs, ctx, lhs as i64).into_number(ctx),
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
                Number::Fixnum(rhs) => BigInt::mul_i64(lhs, ctx, rhs as i64).into_number(ctx),
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

    pub fn integer_div(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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

    pub fn integer_div0(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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

    pub fn div(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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

    pub fn quotient(ctx: MutationContext<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
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

                                return match lhs.checked_div(rhs) {
                                    Some(quotient) => Self::Fixnum(quotient),
                                    None => BigInt::from_i64(ctx, -(lhs as i64)).into_number(ctx),
                                };
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum((lhs as f64 / rhs).trunc());
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
                    // Same policy as `remainder`: only integral flonums.
                    if lhs != lhs.round() {
                        unreachable!()
                    }
                    if lhs == 0.0 {
                        return Number::Flonum(0.0);
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum((lhs / rhs as f64).trunc());
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum((lhs / rhs).trunc());
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum((lhs / rhs.as_f64()).trunc());
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

                            // Single-digit divisor: one pass, no boxed divisor
                            // or multi-word division, normalized like the other arms.
                            let (quotient, _) =
                                BigInt::div_digit(lhs, ctx, rhs.unsigned_abs() as Digit);
                            let quotient = if rhs < 0 {
                                BigInt::negate(quotient, ctx)
                            } else {
                                quotient
                            };
                            return quotient.into_number(ctx);
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum((lhs.as_f64() / rhs).trunc());
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

    pub fn remainder(ctx: MutationContext<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
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

                                return Self::Fixnum(lhs.checked_rem(rhs).unwrap_or(0));
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

                            // |remainder| < |rhs| always fits a fixnum, it takes
                            // the dividend's sign.
                            let rem = lhs.remainder_digit(rhs.unsigned_abs() as Digit) as i32;
                            return Self::Fixnum(if lhs.negative() { -rem } else { rem });
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

    pub fn modulo(ctx: MutationContext<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
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
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            let rem = lhs.remainder_digit(rhs.unsigned_abs() as Digit) as i32;
                            if rem == 0 {
                                return Self::Fixnum(0);
                            }

                            // `remainder` takes the dividend's sign, `modulo`
                            // takes the divisor's, adding it once when they differ.
                            let rem = if lhs.negative() { -rem } else { rem };
                            if (rhs < 0) != (rem < 0) {
                                return Self::add(ctx, Self::Fixnum(rem), Self::Fixnum(rhs));
                            }
                            return Self::Fixnum(rem);
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

    fn expt_impl(ctx: MutationContext<'gc>, mut lhs: Self, rhs: Self) -> Self {
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
            // `(n/d)^k = n^k / d^k`; the reduction takes the numerator first
            // (the operands used to be swapped here, which inverted every
            // rational power).
            let nume = Self::expt_impl(ctx, rn.numerator, rhs);
            let deno = Self::expt_impl(ctx, rn.denominator, rhs);
            return Self::reduce(ctx, nume, deno);
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

    pub fn expt(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
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

}
