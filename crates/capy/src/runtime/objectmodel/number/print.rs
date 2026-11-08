use super::*;

fn i32_to_raidx(n: i64, radix: u8) -> String {
    /*if n == 0 {
        return "0".to_string();
    }
    // Use a character map for digits.
    const CHARS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";

    let is_negative = n < 0;
    // Work with the absolute value as a u32.
    // This is safe because i32::MIN.abs() fits in u32.
    let mut num = if is_negative {
        // Safely get the absolute value
        n.unsigned_abs()
    } else {
        n as u32
    };

    let mut result = String::new();

    while num > 0 {
        let remainder = (num % radix as u32) as usize;
        result.push(CHARS[remainder] as char);
        num /= radix as u32;
    }

    // The result string is built in reverse, so we need to reverse it.
    let mut final_string: String = result.chars().rev().collect();

    // Prepend the negative sign if necessary.
    if is_negative {
        final_string.insert(0, '-');
    }

    final_string*/

    match radix {
        2 => format!("{:b}", n),
        8 => format!("{:o}", n),
        10 => format!("{}", n),
        16 => format!("{:x}", n),
        _ => format!("{}", n),
    }
}

impl<'gc> Number<'gc> {
    pub fn integer_to_string(self, ctx: MutationContext<'gc>, radix: u8) -> String {
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
            format!("-{}", i32_to_raidx(-(n as i64), radix))
        } else {
            i32_to_raidx(n as i64, radix)
        }
    }

    pub fn flonum_to_string(self, ctx: MutationContext<'gc>, radix: u8) -> String {
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

    pub fn compnum_to_string(self, ctx: MutationContext<'gc>, radix: u8) -> String {
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

    pub fn ratnum_to_string(self, ctx: MutationContext<'gc>, radix: u8) -> String {
        let r = match self {
            Self::Rational(r) => r,
            _ => unreachable!(),
        };

        let nume = r.numerator.to_string_radix(ctx, radix);
        let deno = r.denominator.to_string_radix(ctx, radix);

        format!("{}/{}", nume, deno)
    }

    pub fn make_rectangular(ctx: MutationContext<'gc>, real: Self, imag: Self) -> Self {
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

    pub fn numerator(self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn denominator(self, ctx: MutationContext<'gc>) -> Self {
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

    pub fn to_string_radix(self, ctx: MutationContext<'gc>, base: u8) -> String {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self.integer_to_string(ctx, base),
            Self::Flonum(_) => self.flonum_to_string(ctx, base),
            Self::Rational(_) => self.ratnum_to_string(ctx, base),
            Self::Complex(_) => self.compnum_to_string(ctx, base),
        }
    }
}
