use super::*;

impl<'gc> Number<'gc> {
    pub fn from_i64(ctx: MutationContext<'gc>, n: i64) -> Self {
        if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n))
    }

    pub fn from_u64(ctx: MutationContext<'gc>, n: u64) -> Self {
        if n <= i32::MAX as u64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n))
    }

    pub fn from_u128(ctx: MutationContext<'gc>, n: u128) -> Self {
        if n <= i32::MAX as u128 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u128(ctx, n))
    }

    pub fn from_u32(ctx: MutationContext<'gc>, n: u32) -> Self {
        if n <= i32::MAX as u32 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n as _))
    }

    pub fn from_i32(n: i32) -> Self {
        Self::Fixnum(n)
    }

    pub fn from_u8(n: u8) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_i8(n: i8) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_u16(n: u16) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_i16(n: i16) -> Self {
        Self::Fixnum(n as _)
    }

    pub fn from_usize(ctx: MutationContext<'gc>, n: usize) -> Self {
        if n <= i32::MAX as usize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n as _))
    }

    pub fn from_isize(ctx: MutationContext<'gc>, n: isize) -> Self {
        if n >= i32::MIN as isize && n <= i32::MAX as isize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n as _))
    }

}
