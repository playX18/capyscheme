//! Number parsing helpers for the Scheme reader.
//!
//! Provides [`parse_ubignum`] and [`parse_uinteger`] used by the Scheme reader
//! to parse unsigned integer literals.

use std::iter::Peekable;

use crate::rsgc::Gc;
use crate::runtime::Context;

use super::Number;
use super::bigint::BigInt;

/// Parses an unsigned bignum from a character stream.
///
/// Reads digits from `s` in the given `radix`, building a BigInt. Stops
/// early when encountering `#` (number prefix delimiter) or a non-digit
/// character.
pub fn parse_ubignum<'gc>(
    ctx: Context<'gc>,
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
        let digit;
        if c.is_ascii_digit() {
            digit = (c as u8) - b'0';
        } else if c >= 'a' {
            digit = (c as u8) - b'a' + 10;
        } else if c >= 'A' {
            digit = (c as u8) - b'A' + 10;
        } else {
            break;
        }

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

/// Parses an unsigned (Scheme) integer from a character stream.
///
/// Returns `None` if the stream does not start with a digit character.
/// Stops on `#` (prefix sentinel) or a non-digit.
pub fn parse_uinteger<'gc>(
    ctx: Context<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Option<Number<'gc>> {
    s.peek()?;

    let mut ans = Number::Fixnum(0);
    while let Some(&c) = s.peek() {
        if c == '#' {
            return Some(ans);
        }
        let digit;
        if c.is_ascii_digit() {
            digit = (c as u8) - b'0';
        } else if c >= 'a' {
            digit = (c as u8) - b'a' + 10;
        } else if c >= 'A' {
            digit = (c as u8) - b'A' + 10;
        } else {
            return None;
        }

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
