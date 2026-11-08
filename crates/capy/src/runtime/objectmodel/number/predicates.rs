use super::*;

impl<'gc> Number<'gc> {
    pub fn is_positive(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::Flonum(n) => *n > 0.0,
            Self::BigInt(n) => n.is_positive(),
            Self::Rational(rn) => rn.numerator.is_positive(),
            Self::Complex(c) => c.real.is_positive(),
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(_) => false,
            Self::Complex(c) => c.real.is_exact() && c.imag.is_exact(),
        }
    }

    pub fn is_real_valued(&self) -> bool {
        if self.is_real() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero(),
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::Flonum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Complex(_c) => false,
        }
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(n) => n.is_finite(),
            Self::Complex(c) => c.real.is_finite() && c.imag.is_finite(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        match self {
            Self::Flonum(n) => n.is_infinite(),
            Self::Complex(c) => c.real.is_infinite() || c.imag.is_infinite(),
            _ => false,
        }
    }

    pub fn is_exact_integer(&self) -> bool {
        matches!(self, Self::Fixnum(_) | Self::BigInt(_))
    }

    pub fn is_exact_positive_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::BigInt(n) => n.is_positive() && !n.is_zero(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Rational(_) => false,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }

                fl.round() == *fl
            }
            _ => false,
        }
    }

    pub fn is_integer_valued(&self) -> bool {
        if self.is_integer() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_integer_valued(),
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Self::Rational(_) | Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }
                true
            }
            _ => false,
        }
    }

    pub fn is_rational_valued(&self) -> bool {
        if self.is_rational() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_rational_valued(),
            _ => false,
        }
    }

    pub fn is_exact_non_negative_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n >= 0,
            Self::BigInt(n) => n.is_positive() || n.is_zero(),
            _ => false,
        }
    }

    pub fn is_even(&self) -> bool {
        match self {
            Self::Fixnum(n) => n & 1 == 0,
            Self::BigInt(n) => n[0] & 1 == 0,
            Self::Flonum(n) => n * 0.5 == (n * 0.5).floor(),

            Self::Complex(n) => n.real.is_even(),
            _ => false,
        }
    }

}
