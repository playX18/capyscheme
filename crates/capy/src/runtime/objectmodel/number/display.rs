use super::*;

impl<'gc> fmt::Display for Number<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Fixnum(i) => write!(f, "{}", i),
            Number::Flonum(fl) => write!(f, "{}", fl),
            Number::BigInt(b) => write!(f, "{}", b),
            Number::Rational(r) => write!(f, "{}", r),
            Number::Complex(c) => write!(f, "{}", c),
        }
    }
}

impl<'gc> fmt::Display for Rational<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl<'gc> fmt::Display for Complex<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let real = self.real.to_string();
        let imag = self.imag.to_string();

        let need_plus = !imag.starts_with('+') && !imag.starts_with("-");

        let mut out = String::new();
        out.push_str(&real);
        if need_plus {
            out.push('+');
        }
        out.push_str(&imag);
        out.push('i');

        write!(f, "{}", out)
    }
}
