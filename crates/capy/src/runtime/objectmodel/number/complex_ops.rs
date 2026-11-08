use super::*;

impl<'gc> Number<'gc> {
    pub fn abs(self, ctx: MutationContext<'gc>) -> Self {
        // Flonums take hardware abs: `is_positive` is strict (`> 0.0`), so
        // the general path below would negate `+0.0` to `-0.0`. This matches
        // `FlAbs`, which always did `f64::abs`.
        if let Self::Flonum(n) = self {
            return Self::Flonum(n.abs());
        }

        if self.is_positive() {
            return self;
        }

        self.negate(ctx)
    }

    pub fn rectangular(self, ctx: MutationContext<'gc>, rhs: Self) -> Self {
        Self::normalize_complex(ctx, self, rhs)
    }

    pub fn polar(self, ctx: MutationContext<'gc>, rhs: Self) -> Self {
        if rhs.is_zero() {
            return self;
        }

        let r = self.real_to_f64(ctx);
        let a = rhs.real_to_f64(ctx);
        Self::Complex(Complex::new(
            ctx,
            Self::Flonum(r * libm::cos(a)),
            Self::Flonum(r * libm::sin(a)),
        ))
    }

    pub fn angle(self, ctx: MutationContext<'gc>) -> Self {
        if let Self::Complex(c) = self {
            let real = c.real.real_to_f64(ctx);
            let imag = c.imag.real_to_f64(ctx);
            return Self::Flonum(libm::atan2(imag, real));
        }

        if self.is_negative() {
            return Self::Flonum(std::f64::consts::PI);
        }

        if let Self::Flonum(_n) = self {
            return Self::Flonum(0.0);
        }

        Self::Fixnum(0)
    }
}
