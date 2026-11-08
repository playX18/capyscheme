use super::*;

/// Decodes a f64 into (mantissa, exponent, sign).
/// Returns (mantissa: u64, exponent: i32, sign: i32)
pub const fn decode_double(n: f64) -> (i64, i32, i32) {
    let bits = n.to_bits();
    let mant_bits = bits as i64 & (IEXPT_2N52 - 1);
    let sign_bits = (bits >> 63) as i32;
    let exp_bits = ((bits >> 52) & 0x7ff) as i32;

    if n == 0.0 {
        return (0, 0, if sign_bits != 0 { -1 } else { 1 });
    }
    if n.is_nan() {
        return (0x18000000000000, 972, 1);
    }
    if n.is_infinite() {
        return (0x10000000000000, 972, if sign_bits != 0 { -1 } else { 1 });
    }
    debug_assert!(exp_bits != 0x7ff);

    let exp = if exp_bits != 0 {
        exp_bits - 1023
    } else {
        -1022
    } - 52;
    let sign = if sign_bits != 0 { -1 } else { 1 };
    let mant = if exp_bits != 0 {
        mant_bits | (1 << 52)
    } else {
        mant_bits
    };
    (mant, exp, sign)
}

pub fn nextfloat(z: f64) -> f64 {
    let (m, k, sign) = decode_double(z);

    assert!(sign >= 0);

    if m == IEXPT_2N53 - 1 {
        return libm::ldexp(IEXPT_2N52 as f64, k + 1);
    }

    libm::ldexp((m + 1) as f64, k)
}

pub fn prevfloat(z: f64) -> f64 {
    let (m, k, sign) = decode_double(z);

    assert!(sign >= 0);

    if m == IEXPT_2N52 {
        return libm::ldexp((IEXPT_2N53 - 1) as f64, k - 1);
    }

    libm::ldexp((m - 1) as f64, k)
}

pub fn parse_ubignum<'gc>(
    ctx: MutationContext<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Gc<'gc, BigInt<'gc>> {
    if s.peek().is_none() {
        return BigInt::zero(ctx);
    }
    let mut ans = BigInt::zero(ctx);

    while let Some(&c) = s.peek() {
        if c == '#' {
            return ans;
        }
        let digit = if c.is_ascii_digit() {
            (c as u8) - b'0'
        } else if c >= 'a' {
            (c as u8) - b'a' + 10
        } else if c >= 'A' {
            (c as u8) - b'A' + 10
        } else {
            break;
        };

        if digit < radix as u8 {
            ans = BigInt::times(ans, ctx, BigInt::from_u64(ctx, radix as _));
            ans = BigInt::plus(ans, ctx, BigInt::from_u64(ctx, digit as u64));
            s.next();
            continue;
        }
        break;
    }

    ans
}

pub fn parse_uinteger<'gc>(
    ctx: MutationContext<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Option<Number<'gc>> {
    s.peek()?;

    let mut ans = Number::Fixnum(0);
    while let Some(&c) = s.peek() {
        if c == '#' {
            return Some(ans);
        }
        let digit = if c.is_ascii_digit() {
            (c as u8) - b'0'
        } else if c >= 'a' {
            (c as u8) - b'a' + 10
        } else if c >= 'A' {
            (c as u8) - b'A' + 10
        } else {
            return None;
        };

        if digit < radix as u8 {
            ans = Number::mul(ctx, ans, Number::from_u64(ctx, radix as _));
            ans = Number::add(ctx, ans, Number::from_u64(ctx, digit as u64));
            s.next();
            continue;
        }
        return None;
    }
    Some(ans)
}
