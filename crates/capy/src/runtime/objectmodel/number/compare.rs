use super::*;

impl<'gc> Number<'gc> {
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

    pub fn equal(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                Number::Flonum(rhs) => lhs as f64 == rhs,
                Number::BigInt(_rhs) => false,
                Number::Rational(_rn) => false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::Fixnum(lhs), cn.real)
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs as f64,
                Number::Flonum(rhs) => lhs == rhs,
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
                Number::Fixnum(_rhs) => false,

                Number::Flonum(rhs) => {
                    Self::compare(ctx, Self::BigInt(lhs), Self::Flonum(rhs))
                        == Some(std::cmp::Ordering::Equal)
                }
                Number::BigInt(rhs) => lhs == rhs,
                Number::Rational(_rn) => false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::BigInt(lhs), cn.real)
                }
            },

            Number::Rational(lhs) => {
                let nume = lhs.numerator;
                let deno = lhs.denominator;

                match rhs {
                    Number::Fixnum(_rhs) => false,

                    Number::Flonum(rhs) => {
                        Self::compare(ctx, Self::Rational(lhs), Self::Flonum(rhs))
                            == Some(std::cmp::Ordering::Equal)
                    }
                    Number::BigInt(_rhs) => false,
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

    pub fn compare(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Option<std::cmp::Ordering> {
        if lhs.is_nan() || rhs.is_nan() {
            return None;
        }
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    // Not a subtraction: it wraps for operands of opposite sign.
                    Some(lhs.cmp(&rhs))
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
                Number::Fixnum(_rhs) => {
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

}
