//! The native fast path for `format`

use crate::prelude::*;
use crate::runtime::objectmodel::{Str, Value};
use crate::runtime::prelude::*;
use crate::utils::flags;

use std::fmt::Write as _;

pub fn init_format<'gc>(ctx: MutationContext<'gc>) {
    let _ = format_ops::register(ctx);
}

#[scheme(path = capy)]
mod format_ops {
    /// `(%format-fast format-string args)` => the formatted string, or `#f`
    /// when the fast path does not handle the call.
    #[scheme(name = "%format-fast")]
    pub fn format_fast(fmt: Value<'gc>, args: Value<'gc>) -> Value<'gc> {
        if !flags::format_fast() || !fmt.is::<Str>() {
            return Value::new(false);
        }
        let fmt = fmt.downcast::<Str>();
        // Narrow strings hold UTF-8 bytes; a wide format string falls back.
        let Some(bytes) = fmt.chars() else {
            return Value::new(false);
        };
        match format_into(bytes, args) {
            Some(text) => Value::from_gc(Str::new(ctx, text, false)),
            None => Value::new(false),
        }
    }

    /// Whether the fast path is enabled.
    #[scheme(name = "%format-fast-enabled?")]
    pub fn format_fast_enabled() -> bool {
        flags::format_fast()
    }
}

fn next_arg<'gc>(args: &mut Value<'gc>) -> Option<Value<'gc>> {
    if !args.is_pair() {
        return None;
    }
    let car = args.car();
    *args = args.cdr();
    Some(car)
}

/// Formats `fmt` with `args`, or returns `None` to fall back.
fn format_into<'gc>(fmt: &[u8], mut args: Value<'gc>) -> Option<String> {
    let mut out = String::with_capacity(fmt.len() + 16);
    let mut i = 0;
    while i < fmt.len() {
        if fmt[i] != b'~' {
            let start = i;
            while i < fmt.len() && fmt[i] != b'~' {
                i += 1;
            }
            out.push_str(std::str::from_utf8(&fmt[start..i]).ok()?);
            continue;
        }
        // A dangling `~` is an error in the Scheme formatter.
        let directive = *fmt.get(i + 1)?;
        match directive {
            b'~' => {
                out.push('~');
                i += 2;
            }
            b'%' => {
                out.push('\n');
                i += 2;
            }
            // Flushing a string output port has no observable effect.
            b'!' => i += 2,
            b'a' => {
                let value = next_arg(&mut args)?;
                display_arg(&mut out, value)?;
                i += 2;
            }
            b's' | b'w' => {
                let value = next_arg(&mut args)?;
                write_arg(&mut out, value)?;
                i += 2;
            }
            b'c' => {
                let value = next_arg(&mut args)?;
                if !value.is_char() {
                    return None;
                }
                out.push(value.char());
                i += 2;
            }
            b'f' => {
                let value = next_arg(&mut args)?;
                flonum_display(&mut out, to_double(value)?);
                i += 2;
            }
            b'x' | b'o' => {
                let value = next_arg(&mut args)?;
                let radix = if directive == b'x' { 16 } else { 8 };
                let n = value.int32()?;
                integer_radix(&mut out, n, radix);
                i += 2;
            }
            // `~,<digits>f` is the fixed-point form `vec->string` uses.
            b',' => {
                let (digits, after) = parse_digits(fmt, i + 2)?;
                if *fmt.get(after)? != b'f' {
                    return None;
                }
                let value = next_arg(&mut args)?;
                fixed_flonum(&mut out, value, digits)?;
                i = after + 1;
            }
            // Numeric widths, `~?`, `~b`, `~B` and anything unknown keep the
            // Scheme formatter (including its error message).
            _ => return None,
        }
    }
    Some(out)
}

fn parse_digits(fmt: &[u8], start: usize) -> Option<(u32, usize)> {
    let mut i = start;
    let mut n: u32 = 0;
    while let Some(&b) = fmt.get(i) {
        if !b.is_ascii_digit() {
            break;
        }
        n = n.checked_mul(10)?.checked_add((b - b'0') as u32)?;
        i += 1;
    }
    if i == start || n > 38 {
        return None;
    }
    Some((n, i))
}

/// `display` of a value the fast path can reproduce exactly.
fn display_arg(out: &mut String, value: Value<'_>) -> Option<()> {
    if let Some(n) = value.int32() {
        let _ = write!(out, "{n}");
        return Some(());
    }
    if let Some(f) = value.flonum() {
        flonum_display(out, f);
        return Some(());
    }
    if value.is_char() {
        out.push(value.char());
        return Some(());
    }
    if value.is_bool() {
        out.push_str(if value.as_bool() { "#t" } else { "#f" });
        return Some(());
    }
    if value.is::<Str>() {
        let string = value.downcast::<Str>();
        let _ = write!(out, "{string}");
        return Some(());
    }
    // Symbols, pairs, vectors, procedures, heap numbers: the Scheme printer.
    None
}

fn write_arg(out: &mut String, value: Value<'_>) -> Option<()> {
    if let Some(n) = value.int32() {
        let _ = write!(out, "{n}");
        return Some(());
    }
    // `write` and `display` share `number->string` for flonums.
    if let Some(f) = value.flonum() {
        flonum_display(out, f);
        return Some(());
    }
    if value.is_bool() {
        out.push_str(if value.as_bool() { "#t" } else { "#f" });
        return Some(());
    }
    None
}

fn flonum_display(out: &mut String, f: f64) {
    if f.is_nan() {
        out.push_str("+nan.0");
    } else if f.is_infinite() {
        out.push_str(if f.is_sign_negative() {
            "-inf.0"
        } else {
            "+inf.0"
        });
    } else if f == 0.0 {
        out.push_str(if f.is_sign_negative() { "-0.0" } else { "+0.0" });
    } else {
        let _ = write!(out, "{f:?}");
    }
}

fn to_double(value: Value<'_>) -> Option<f64> {
    if let Some(n) = value.int32() {
        return Some(n as f64);
    }
    value.flonum()
}

fn integer_radix(out: &mut String, n: i32, radix: u32) {
    if n < 0 {
        out.push('-');
    }
    let magnitude = (n as i64).unsigned_abs();
    let digits = if radix == 16 {
        format!("{magnitude:x}")
    } else {
        format!("{magnitude:o}")
    };
    out.push_str(&digits);
}

fn fixed_flonum(out: &mut String, value: Value<'_>, digits: u32) -> Option<()> {
    if digits == 0 {
        return None;
    }
    let x = to_double(value)?;
    if !x.is_finite() {
        return None;
    }

    let negative = x < 0.0;
    let magnitude = x.abs();
    let scaled = round_scaled(magnitude, digits)?;
    let scale = 10u128.checked_pow(digits)?;
    let int_part = scaled / scale;
    let frac_part = scaled % scale;
    if negative {
        out.push('-');
    }
    let _ = write!(out, "{int_part}");
    if digits > 0 {
        out.push('.');
        let frac = frac_part.to_string();
        for _ in frac.len()..digits as usize {
            out.push('0');
        }
        out.push_str(&frac);
    }
    Some(())
}

fn round_scaled(magnitude: f64, digits: u32) -> Option<u128> {
    if magnitude == 0.0 {
        return Some(0);
    }
    let bits = magnitude.to_bits();
    let exponent_bits = ((bits >> 52) & 0x7ff) as i32;
    let fraction = bits & ((1u64 << 52) - 1);
    let (mantissa, exponent) = if exponent_bits == 0 {
        (fraction, -1022 - 52)
    } else {
        (fraction | (1u64 << 52), exponent_bits - 1023 - 52)
    };
    let scale = 10u128.checked_pow(digits)?;
    let numerator = (mantissa as u128).checked_mul(scale)?;
    let (numerator, denominator) = if exponent >= 0 {
        let factor = 1u128.checked_shl(exponent as u32)?;
        (numerator.checked_mul(factor)?, 1u128)
    } else {
        let shift = exponent.unsigned_abs();
        if shift >= 128 {
            return None;
        }
        (numerator, 1u128 << shift)
    };
    let quotient = numerator / denominator;
    let remainder = numerator % denominator;

    let other_half = denominator - remainder;
    if remainder == other_half {
        return None;
    }
    if remainder > other_half {
        quotient.checked_add(1)
    } else {
        Some(quotient)
    }
}
