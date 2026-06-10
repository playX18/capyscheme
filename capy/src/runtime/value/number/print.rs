//! Number-to-string formatting helpers for the Scheme numeric tower.
//!
//! Provides [`Display`] implementations for [`Number`], [`Rational`], and
//! [`Complex`], plus radix-aware formatting functions used when Scheme code
//! serializes numbers (e.g. `number->string`).

use std::fmt;

use crate::runtime::Context;

use super::bigint::{Base, NumberToStringOptions};
use super::{Complex, IEXPT_2N52, IEXPT_2N53, Number, Rational};

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

impl<'gc> Number<'gc> {
    pub fn integer_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        if !self.is_exact() {
            return format!("#i{}", self.to_exact(ctx).integer_to_string(ctx, radix));
        }
        if let Number::BigInt(n) = self {
            let opts = NumberToStringOptions {
                base: match radix {
                    2 => &Base::BIN,
                    8 => &Base::OCT,
                    10 => &Base::DEC,
                    16 => &Base::HEX,
                    _ => &Base::DEC,
                },
                force_sign: false,
                group_sep: None,
                group_size: 0,
                minus_sign: "-".into(),
                plus_sign: "+".into(),
            };

            return n.to_string_with_options(&opts);
        };

        let Number::Fixnum(n) = self else {
            unreachable!()
        };

        if n < 0 {
            format!("-{}", i32_to_raidx(-n, radix))
        } else {
            i32_to_raidx(n, radix)
        }
    }

    pub fn flonum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let n = self.real_to_f64(ctx);

        if n.is_nan() {
            return "+nan.0".to_string();
        } else if n.is_infinite() {
            if n.is_sign_negative() {
                return "-inf.0".to_string();
            } else {
                return "+inf.0".to_string();
            }
        }

        if radix == 10 {
            if n == 0.0 {
                if n.is_sign_negative() {
                    return "-0.0".to_string();
                } else {
                    return "+0.0".to_string();
                }
            } else {
                return format!("{n:?}");
            }
        }

        if n == 0.0 {
            return "#i0".to_string();
        }

        let p = self.numerator(ctx).to_exact(ctx).abs(ctx);
        let q = self.denominator(ctx).to_exact(ctx);
        let mut buf = "#i".to_string();
        if self.is_negative() {
            buf.push('-');
        }

        buf.push_str(&p.to_string_radix(ctx, radix));
        if !matches!(q, Number::Fixnum(1)) {
            buf.push('/');
            buf.push_str(&q.to_string_radix(ctx, radix));
        }

        buf
    }

    pub fn compnum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let c = match self {
            Self::Complex(c) => c,
            _ => unreachable!(),
        };

        if radix != 10 {
            format!(
                "#i{}",
                Self::make_rectangular(ctx, c.real.to_exact(ctx), c.imag.to_exact(ctx))
                    .to_string_radix(ctx, 10)
            )
        } else {
            let rr = c.real.to_string_radix(ctx, radix);
            let ii = c.imag.to_string_radix(ctx, radix);

            if ii.starts_with('+') || ii.starts_with('-') {
                format!("{rr}{ii}i")
            } else {
                format!("{rr}+{ii}i")
            }
        }
    }

    pub fn ratnum_to_string(self, ctx: Context<'gc>, radix: u8) -> String {
        let r = match self {
            Self::Rational(r) => r,
            _ => unreachable!(),
        };

        let nume = r.numerator.to_string_radix(ctx, radix);
        let deno = r.denominator.to_string_radix(ctx, radix);

        format!("{}/{}", nume, deno)
    }

    pub fn make_rectangular(ctx: Context<'gc>, real: Self, imag: Self) -> Self {
        let real = if let Self::Complex(c) = real {
            c.real
        } else {
            real
        };

        let imag = if let Self::Complex(c) = imag {
            c.imag
        } else {
            imag
        };

        Self::Complex(Complex::new(ctx, real, imag))
    }

    pub fn numerator(self, ctx: Context<'gc>) -> Self {
        let inexact = !self.is_exact();
        let obj = self.to_exact(ctx);
        match obj {
            Self::Rational(r) => {
                if inexact {
                    r.numerator.to_inexact(ctx)
                } else {
                    r.numerator
                }
            }
            _ => {
                if inexact {
                    obj.to_inexact(ctx)
                } else {
                    obj
                }
            }
        }
    }

    pub fn denominator(self, ctx: Context<'gc>) -> Self {
        let inexact = !self.is_exact();
        let obj = self.to_exact(ctx);
        match obj {
            Self::Rational(r) => {
                if inexact {
                    r.denominator.to_inexact(ctx)
                } else {
                    r.denominator
                }
            }
            _ => {
                if inexact {
                    Number::Flonum(1.0)
                } else {
                    Number::Fixnum(1)
                }
            }
        }
    }

    pub fn to_string_radix(self, ctx: Context<'gc>, base: u8) -> String {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self.integer_to_string(ctx, base),
            Self::Flonum(_) => self.flonum_to_string(ctx, base),
            Self::Rational(_) => self.ratnum_to_string(ctx, base),
            Self::Complex(_) => self.compnum_to_string(ctx, base),
        }
    }
}

fn i32_to_raidx(n: i32, radix: u8) -> String {
    match radix {
        2 => format!("{:b}", n),
        8 => format!("{:o}", n),
        10 => format!("{}", n),
        16 => format!("{:x}", n),
        _ => format!("{}", n),
    }
}
