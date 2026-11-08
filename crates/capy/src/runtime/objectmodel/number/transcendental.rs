use super::*;

impl<'gc> Number<'gc> {
    pub fn exp(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn log(ctx: MutationContext<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let value = n as SignedDigit;

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

    pub fn sin(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn cos(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn tan(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn sqrt(ctx: MutationContext<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let mut value = n as SignedDigit;
                if value == 0 {
                    return Number::Fixnum(0);
                }

                if value > 0 {
                    let iroot = libm::floor(libm::sqrt(value as f64)) as SignedDigit;
                    if iroot.wrapping_mul(iroot) == value {
                        Number::Fixnum(iroot as i32)
                    } else {
                        Self::Flonum(libm::sqrt(value as f64))
                    }
                } else {
                    value = -value;
                    let iroot = libm::floor(libm::sqrt(value as f64)) as SignedDigit;
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

    pub fn magnitude(ctx: MutationContext<'gc>, n: Self) -> Self {
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
                        return Self::Flonum(<f64>::INFINITY);
                    }

                    let m = libm::sqrt(real * real + imag * imag);
                    if m < <f64>::EPSILON || m.is_infinite() {
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

    pub fn exact_integer_sqrt(ctx: MutationContext<'gc>, n: Self) -> (Self, Self) {
        match n {
            Number::Fixnum(n) => {
                let value = n as SignedDigit;

                if value == 0 {
                    return (Number::Fixnum(0), Number::Fixnum(0));
                }

                let iroot = libm::floor(libm::sqrt(value as f64)) as SignedDigit;
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

    pub fn inverse(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn asin(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn acos(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn atan(ctx: MutationContext<'gc>, n: Self) -> Self {
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

    pub fn atan2(ctx: MutationContext<'gc>, lhs: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        if matches!(lhs, Number::Fixnum(0)) {
            return Number::Fixnum(0);
        }

        let lhs = lhs.real_to_f64(ctx);
        let rhs = rhs.real_to_f64(ctx);

        Number::Flonum(libm::atan2(lhs, rhs))
    }

}
