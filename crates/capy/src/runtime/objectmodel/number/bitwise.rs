use super::*;

impl<'gc> Number<'gc> {
    pub fn ash(self, ctx: MutationContext<'gc>, count: Self) -> Option<Self> {
        if count.is_negative() {
            return self.rsh(ctx, count.negate(ctx));
        }

        self.lsh(ctx, count)
    }

    pub fn lsh(self, ctx: MutationContext<'gc>, count: Self) -> Option<Self> {
        if self.is_zero() {
            return Some(self);
        }

        let count = count.coerce_exact_integer_to_usize();

        if count == 0 {
            return Some(self);
        } else if count / i32::BITS as usize >= i32::MAX as usize {
            return None;
        }

        match self {
            Self::Fixnum(n) => {
                // `checked_shl` only rejects shift amounts >= 32, not results
                // that leave the i32 range, so shift in i64 below 32 bits.
                if count < i32::BITS as usize {
                    return Some(Number::from_i64(ctx, (n as i64) << count));
                }

                let bn = BigInt::from_i64(ctx, n as _);
                Some(Self::BigInt(BigInt::shift_left(bn, ctx, count)).normalize_integer())
            }

            Self::BigInt(n) => {
                Some(Self::BigInt(BigInt::shift_left(n, ctx, count)).normalize_integer())
            }

            _ => None,
        }
    }

    pub fn rsh(self, ctx: MutationContext<'gc>, count: Self) -> Option<Self> {
        let count = count.coerce_exact_integer_to_usize();
        if count == 0 {
            return Some(self);
        }

        match self {
            // Shifting out every bit leaves the sign.
            Self::Fixnum(n) => Some(Self::Fixnum(n >> count.min(i32::BITS as usize - 1))),

            Self::BigInt(n) => {
                Some(Self::BigInt(BigInt::shift_right(n, ctx, count)).normalize_integer())
            }

            _ => None,
        }
    }

    pub fn logior(self, ctx: MutationContext<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs | rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::or(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::or(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                Self::BigInt(BigInt::or(lhs, ctx, rhs)).normalize_integer()
            }

            _ => unreachable!(),
        }
    }

    pub fn logand(self, ctx: MutationContext<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs & rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::and(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::and(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                Self::BigInt(BigInt::and(lhs, ctx, rhs)).normalize_integer()
            }

            _ => unreachable!(),
        }
    }

    pub fn logxor(self, ctx: MutationContext<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => Self::Fixnum(lhs ^ rhs),

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                Self::BigInt(BigInt::xor(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                Self::BigInt(BigInt::xor(lhs, ctx, rhs)).normalize_integer()
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                Self::BigInt(BigInt::xor(lhs, ctx, rhs)).normalize_integer()
            }

            _ => unreachable!(),
        }
    }

    pub fn lognot(self, ctx: MutationContext<'gc>) -> Self {
        match self {
            Self::Fixnum(n) => {
                let bn = BigInt::from_i64(ctx, n as _);
                Self::BigInt(BigInt::not(bn, ctx))
            }

            Self::BigInt(n) => Self::BigInt(BigInt::not(n, ctx)),

            _ => unreachable!(),
        }
    }

    pub fn logtest(self, ctx: MutationContext<'gc>, other: Self) -> bool {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => (lhs & rhs) != 0,
            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::BigInt(rhs)) => !BigInt::and(lhs, ctx, rhs).is_zero(),
            _ => unreachable!(),
        }
    }

    pub fn logbit(self, ctx: MutationContext<'gc>, n: Self) -> bool {
        let n = n.coerce_exact_integer_to_usize();
        if n / i32::BITS as usize >= i32::MAX as usize {
            return false;
        }

        match self {
            Self::Fixnum(num) => {
                if n < i32::BITS as usize {
                    return (num & (1 << n)) != 0;
                }

                let bn = BigInt::from_i64(ctx, num as _);
                bn.is_bit_set(n)
            }

            Self::BigInt(bi) => {
                if n < Digit::BITS as usize {
                    return (bi[0] & (1 << n)) != 0;
                }

                bi.is_bit_set(n)
            }

            _ => false,
        }
    }

}
