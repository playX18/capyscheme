use super::*;

pub trait IntoNumber<'gc> {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc>;
}

impl<'gc> IntoNumber<'gc> for i32 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Fixnum(self)
    }
}

impl<'gc> IntoNumber<'gc> for i64 {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        if self >= i32::MIN as i64 && self <= i32::MAX as i64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u32 {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u32 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self as u64))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u64 {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for f64 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Flonum(self)
    }
}

impl<'gc> IntoNumber<'gc> for f32 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Flonum(self as f64)
    }
}

impl<'gc> IntoNumber<'gc> for u16 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for u8 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i16 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i8 {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for usize {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        if self <= i32::MAX as usize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u128(ctx, self as u128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for isize {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        if self >= i32::MIN as isize && self <= i32::MAX as isize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i128(ctx, self as i128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u128 {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_u128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for i128 {
    fn into_number(self, ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_i128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, BigInt<'gc>> {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        if let Some(i) = self
            .try_as_i64()
            .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
        {
            Number::Fixnum(i as i32)
        } else {
            Number::BigInt(self)
        }
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Rational<'gc>> {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Rational(self)
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Complex<'gc>> {
    fn into_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        Number::Complex(self)
    }
}
