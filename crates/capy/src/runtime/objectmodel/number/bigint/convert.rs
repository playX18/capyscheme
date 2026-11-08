use super::*;

impl<'gc> BigInt<'gc> {
    pub fn from_u64(ctx: MutationContext<'gc>, value: u64) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        BigInt::new::<false>(ctx, &[value], false)
    }

    pub fn from_i32(ctx: MutationContext<'gc>, value: i32) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        // `unsigned_abs`: `wrapping_abs(i32::MIN) as u64` would sign-extend to
        // 0xffff_ffff_8000_0000 instead of 2^31.
        let abs_value = value.unsigned_abs() as u64;

        BigInt::new::<false>(ctx, &[abs_value], value < 0)
    }

    pub fn from_i64(ctx: MutationContext<'gc>, value: i64) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        let abs_value = value.wrapping_abs() as u64;

        BigInt::new::<false>(ctx, &[abs_value], value < 0)
    }

    pub fn from_u128(ctx: MutationContext<'gc>, value: u128) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        Self::new::<true>(ctx, &[loword(value), hiword(value)], false)
    }

    pub fn from_i128(ctx: MutationContext<'gc>, value: i128) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        let abs_value = if value == i128::MIN {
            i128::MAX as u128 + 1
        } else {
            if value < 0 {
                (-value) as u128
            } else {
                value as u128
            }
        };

        Self::new::<true>(ctx, &[loword(abs_value), hiword(abs_value)], value < 0)
    }

    pub fn from_f64(ctx: MutationContext<'gc>, value: f64) -> Gc<'gc, Self> {
        if value > -1.0 && value < 1.0 {
            Self::new::<true>(ctx, &[0], value < 0.0)
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = value.abs() as u64;

            Self::new::<false>(ctx, &[absvalue], value < 0.0)
        } else if value.is_finite() {
            let s = format!("{:.0}", value);
            Self::parse(ctx, &s, &Base::DEC).unwrap_or_else(|_| Self::zero(ctx))
        } else {
            Self::zero(ctx)
        }
    }

    pub fn try_as_i64(&self) -> Option<i64> {
        if self.count() > 1 {
            return None;
        }

        // A zero-limb value is zero. Normal constructions produce a single
        // zero limb instead, but the Karatsuba split can hand back an empty
        // half, so treat both spellings alike rather than indexing `words[0]`
        // past the payload.
        if self.count() == 0 {
            return Some(0);
        }

        let value = self[0];
        if self.negative() && value == i64::MAX as u64 + 1 {
            return Some(i64::MIN);
        }

        if value <= i64::MAX as u64 {
            if self.negative() {
                Some(-(value as i64))
            } else {
                Some(value as i64)
            }
        } else {
            None
        }
    }

    pub fn try_as_u64(&self) -> Option<u64> {
        if self.count() > 1 {
            return None;
        }

        if self.count() == 0 {
            return Some(0);
        }

        let value = self[0];
        Some(value)
    }

    pub fn try_as_u128(&self) -> Option<u128> {
        if self.count() > 2 {
            return None;
        }

        if self.count() == 0 {
            return Some(0);
        }

        let mut value = 0u128;
        for i in (0..self.count()).rev() {
            value <<= DIGIT_BIT;
            value |= self[i] as u128;
        }
        Some(value)
    }

    pub fn try_as_i128(&self) -> Option<i128> {
        if self.count() > 2 {
            return None;
        }

        if self.count() == 0 {
            return Some(0);
        }

        let mut value = 0i128;
        for i in (0..self.count()).rev() {
            value <<= DIGIT_BIT;
            value |= self[i] as i128;
        }
        Some(value)
    }

    pub fn as_f64(&self) -> f64 {
        let mut result = 0.0;

        for &word in self.iter().rev() {
            result = result * BASE as f64 + word as f64;
        }

        if self.negative() { -result } else { result }
    }

    pub fn is_zero(&self) -> bool {
        // An empty value is zero as well. It is not produced by the normal
        // constructors, but a Karatsuba half may be empty.
        self.count() == 0 || (self.count() == 1 && self[0] == 0)
    }

    pub fn is_one(&self) -> bool {
        self.count() == 1 && self[0] == 1 && !self.negative()
    }

    pub fn is_negative(&self) -> bool {
        self.negative()
    }

    pub fn is_positive(&self) -> bool {
        !self.negative()
    }

    pub fn is_odd(&self) -> bool {
        (self[0] & 1) != 0
    }

    pub fn negate(this: Gc<'gc, Self>, ctx: MutationContext<'gc>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return this;
        }

        this.dup(ctx, !this.negative())
    }

    pub fn abs(this: Gc<'gc, Self>, ctx: MutationContext<'gc>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return this;
        }

        if this.negative() {
            this.dup(ctx, false)
        } else {
            this
        }
    }

}
