use super::*;

#[derive(Debug, Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum ExactInteger<'gc> {
    Fixnum(i32),
    BigInt(Gc<'gc, BigInt<'gc>>),
}

impl<'gc> ExactInteger<'gc> {
    /// A bignum result as an integer, as a fixnum when it fits the i32 range.
    fn from_big(res: Gc<'gc, BigInt<'gc>>) -> Self {
        match res.try_as_i64() {
            Some(v) if v >= i32::MIN as i64 && v <= i32::MAX as i64 => Self::Fixnum(v as i32),
            _ => Self::BigInt(res),
        }
    }

    pub fn to_number(self, _ctx: MutationContext<'gc>) -> Number<'gc> {
        match self {
            Self::Fixnum(n) => Number::Fixnum(n),
            Self::BigInt(b) => Number::BigInt(b),
        }
    }

    pub fn bitor(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l | r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i32(ctx, l);
                    let res = BigInt::or(lhs, ctx, r);
                    Self::from_big(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i32(ctx, r);
                    let res = BigInt::or(l, ctx, rhs);
                    Self::from_big(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::or(l, ctx, r);
                    Self::from_big(res)
                }
            },
        }
    }

    pub fn bitand(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l & r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i32(ctx, l);
                    let res = BigInt::and(lhs, ctx, r);
                    Self::from_big(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i32(ctx, r);
                    let res = BigInt::and(l, ctx, rhs);
                    Self::from_big(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::and(l, ctx, r);
                    Self::from_big(res)
                }
            },
        }
    }

    pub fn bitxor(ctx: MutationContext<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l ^ r),
                Self::BigInt(r) => {
                    let lhs = BigInt::from_i64(ctx, l as _);
                    let res = BigInt::xor(lhs, ctx, r);
                    Self::from_big(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let rhs = BigInt::from_i64(ctx, r as _);
                    let res = BigInt::xor(l, ctx, rhs);
                    Self::from_big(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::xor(l, ctx, r);
                    Self::from_big(res)
                }
            },
        }
    }

    pub fn bitnot(ctx: MutationContext<'gc>, n: Self) -> Self {
        match n {
            Self::Fixnum(n) => Self::Fixnum(!n),
            Self::BigInt(b) => {
                let res = BigInt::not(b, ctx);
                Self::BigInt(res)
            }
        }
    }

    pub fn is_bit_set(_ctx: MutationContext<'gc>, n: Self, bit: u32) -> bool {
        match n {
            Self::Fixnum(n) => {
                if bit >= 32 {
                    return false;
                }

                (n & (1 << bit)) != 0
            }
            Self::BigInt(b) => b.is_bit_set(bit as usize),
        }
    }

    pub fn first_bit_set(_ctx: MutationContext<'gc>, n: Self) -> i32 {
        match n {
            Self::Fixnum(n) => {
                if n == 0 {
                    return -1;
                }

                n.trailing_zeros() as i32
            }
            Self::BigInt(b) => {
                if b.is_zero() {
                    return -1;
                }

                b.first_bit_set()
            }
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n < 0,
            Self::BigInt(b) => b.is_negative(),
        }
    }

    pub fn bit_count(self, ctx: MutationContext<'gc>) -> i32 {
        match self {
            Self::Fixnum(n) => {
                if n == 0 {
                    0
                } else if n > 0 {
                    nbits32(n as u32) as i32
                } else {
                    !(nbits32((!n) as u32) as i32)
                }
            }
            Self::BigInt(b) => {
                if b.is_zero() {
                    0
                } else if b.is_positive() {
                    b.count_ones() as i32
                } else {
                    !Self::bit_count(Self::BigInt(BigInt::not(b, ctx)), ctx)
                }
            }
        }
    }

    pub fn bit_length(&self, ctx: MutationContext<'gc>) -> u32 {
        match self {
            Self::Fixnum(n) => {
                if *n == 0 {
                    return 0;
                }
                let n2 = if *n < 0 { !*n } else { *n };
                32 - n2.leading_zeros()
            }
            Self::BigInt(b) => {
                if b.is_positive() {
                    return BigInt::bitsize(*b) as _;
                }

                let nb = BigInt::not(*b, ctx);
                BigInt::bitsize(nb) as _
            }
        }
    }
}

impl<'gc> FromValue<'gc> for ExactInteger<'gc> {
    fn try_from_value(
        _ctx: MutationContext<'gc>,
        value: Value<'gc>,
    ) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            return Ok(Self::Fixnum(value.as_int32()));
        }

        if value.is::<BigInt>() {
            return Ok(Self::BigInt(value.downcast()));
        }

        Err(ConversionError::TypeMismatch {
            pos: 0,
            expected: "exact integer",
            found: value,
        })
    }
}

impl<'gc> From<ExactInteger<'gc>> for Value<'gc> {
    fn from(value: ExactInteger<'gc>) -> Self {
        match value {
            ExactInteger::Fixnum(n) => Value::new(n),
            ExactInteger::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Value::new(v as i32))
                .unwrap_or_else(|| b.into()),
        }
    }
}

impl<'gc> From<ExactInteger<'gc>> for Number<'gc> {
    fn from(value: ExactInteger<'gc>) -> Self {
        match value {
            ExactInteger::Fixnum(n) => Number::Fixnum(n),
            ExactInteger::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Number::Fixnum(v as i32))
                .unwrap_or_else(|| Number::BigInt(b)),
        }
    }
}

impl<'gc> IntoValue<'gc> for ExactInteger<'gc> {
    fn into_value(self, _ctx: MutationContext<'gc>) -> Value<'gc> {
        match self {
            Self::Fixnum(n) => Value::new(n),
            Self::BigInt(b) => b
                .try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| Value::new(v as i32))
                .unwrap_or_else(|| b.into()),
        }
    }
}
