use super::*;

impl<'gc> Number<'gc> {
    pub fn exact_integer_to_i32(self) -> Option<i32> {
        if let Number::Fixnum(i) = self {
            Some(i)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64()
                .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
                .map(|v| v as i32)
        } else {
            None
        }
    }

    pub fn exact_integer_to_u32(self) -> Option<u32> {
        if let Number::Fixnum(i) = self {
            u32::try_from(i).ok()
        } else if let Number::BigInt(b) = self {
            b.try_as_u64()
                .filter(|&v| v <= u32::MAX as u64)
                .map(|v| v as u32)
        } else {
            None
        }
    }

    pub fn exact_integer_to_i64(self) -> Option<i64> {
        if let Number::Fixnum(i) = self {
            Some(i as i64)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64()
        } else {
            None
        }
    }

    pub fn exact_integer_to_u64(self) -> Option<u64> {
        if let Number::Fixnum(i) = self {
            u64::try_from(i).ok()
        } else if let Number::BigInt(b) = self {
            b.try_as_u64()
        } else {
            None
        }
    }

    pub fn exact_integer_to_usize(self) -> Option<usize> {
        if let Number::Fixnum(i) = self {
            usize::try_from(i).ok()
        } else if let Number::BigInt(b) = self {
            b.try_as_u64().map(|v| v as usize)
        } else {
            None
        }
    }

    pub fn exact_integer_to_isize(self) -> Option<isize> {
        if let Number::Fixnum(i) = self {
            Some(i as isize)
        } else if let Number::BigInt(b) = self {
            b.try_as_i64().map(|v| v as isize)
        } else {
            None
        }
    }

    pub fn exact_integer_to_u128(self) -> Option<u128> {
        if let Number::Fixnum(i) = self {
            u128::try_from(i).ok()
        } else if let Number::BigInt(b) = self {
            b.try_as_u128()
        } else {
            None
        }
    }

    pub fn exact_integer_to_i128(self) -> Option<i128> {
        if let Number::Fixnum(i) = self {
            Some(i as i128)
        } else if let Number::BigInt(b) = self {
            b.try_as_i128()
        } else {
            None
        }
    }

    pub fn exact_integer_to_f64(self) -> Option<f64> {
        if let Number::Fixnum(i) = self {
            Some(i as f64)
        } else if let Number::BigInt(b) = self {
            Some(b.as_f64())
        } else {
            None
        }
    }

    pub fn exact_integer_to_u16(self) -> Option<u16> {
        self.exact_integer_to_u32()
            .filter(|&v| v <= u16::MAX as u32)
            .map(|v| v as u16)
    }

    pub fn exact_integer_to_u8(self) -> Option<u8> {
        self.exact_integer_to_u32()
            .filter(|&v| v <= u8::MAX as u32)
            .map(|v| v as u8)
    }

    pub fn exact_integer_to_i16(self) -> Option<i16> {
        self.exact_integer_to_i32()
            .filter(|&v| v >= i16::MIN as i32 && v <= i16::MAX as i32)
            .map(|v| v as i16)
    }

    pub fn exact_integer_to_i8(self) -> Option<i8> {
        self.exact_integer_to_i32()
            .filter(|&v| v >= i8::MIN as i32 && v <= i8::MAX as i32)
            .map(|v| v as i8)
    }

    pub fn coerce_exact_integer_to_i32(self) -> i32 {
        match self {
            Number::Fixnum(i) => i,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u32;
                if b.negative() {
                    (v as i32).wrapping_neg()
                } else {
                    v as i32
                }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i32")
            }
        }
    }

    pub fn coerce_exact_integer_to_u32(self) -> u32 {
        match self {
            Number::Fixnum(i) => i as u32,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u32
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u32")
            }
        }
    }

    pub fn coerce_exact_integer_to_i64(self) -> i64 {
        match self {
            Number::Fixnum(i) => i as i64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() {
                    (v as i64).wrapping_neg()
                } else {
                    v as i64
                }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i64")
            }
        }
    }

    pub fn coerce_exact_integer_to_u64(self) -> u64 {
        match self {
            Number::Fixnum(i) => i as u64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0]
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u64")
            }
        }
    }

    pub fn coerce_exact_integer_to_usize(self) -> usize {
        match self {
            Number::Fixnum(i) => i as usize,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as usize
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to usize")
            }
        }
    }

    pub fn coerce_exact_integer_to_isize(self) -> isize {
        match self {
            Number::Fixnum(i) => i as isize,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() {
                    (v as isize).wrapping_neg()
                } else {
                    v as isize
                }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to isize")
            }
        }
    }

    pub fn coerce_exact_integer_to_f64(self) -> f64 {
        match self {
            Number::Fixnum(i) => i as f64,
            Number::BigInt(b) => b.as_f64(),
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to f64")
            }
        }
    }

    pub fn coerce_exact_integer_to_u8(self) -> u8 {
        match self {
            Number::Fixnum(i) => i as u8,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u8
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u8")
            }
        }
    }

    pub fn coerce_exact_integer_to_i8(self) -> i8 {
        match self {
            Number::Fixnum(i) => i as i8,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() { -(v as i8) } else { v as i8 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i8")
            }
        }
    }

    pub fn coerce_exact_integer_to_u16(self) -> u16 {
        match self {
            Number::Fixnum(i) => i as u16,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u16
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to u16")
            }
        }
    }

    pub fn coerce_exact_integer_to_i16(self) -> i16 {
        match self {
            Number::Fixnum(i) => i as i16,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0];
                if b.negative() { -(v as i16) } else { v as i16 }
            }
            Number::Flonum(_) | Number::Rational(_) | Number::Complex(_) => {
                panic!("Cannot coerce non-integer to i16")
            }
        }
    }

    pub fn real_to_f64(&self, ctx: MutationContext<'gc>) -> f64 {
        match self {
            Number::Fixnum(i) => *i as f64,
            Number::Flonum(f) => *f,
            Number::BigInt(b) => b.as_f64(),
            Number::Rational(r) => r.to_f64(ctx),
            Number::Complex(c) => {
                if c.imag.is_zero() {
                    c.real.real_to_f64(ctx)
                } else {
                    panic!("Cannot convert complex number to f64");
                }
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Number::Fixnum(i) => *i == 0,
            Number::Flonum(f) => *f == 0.0,
            Number::BigInt(b) => b.is_zero(),
            Number::Rational(_) => false,
            Number::Complex(c) => c.imag.is_zero() && c.real.is_zero(),
        }
    }

    pub fn is_fixnum(&self) -> bool {
        matches!(self, Number::Fixnum(_))
    }

    pub fn is_flonum(&self) -> bool {
        matches!(self, Number::Flonum(_))
    }

    pub fn is_bigint(&self) -> bool {
        matches!(self, Number::BigInt(_))
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, Number::Complex(_))
    }

    pub fn inexact_negate(&self, ctx: MutationContext<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::Flonum(-(*i as f64)),
            Number::Flonum(f) => Number::Flonum(-f),
            Number::BigInt(b) => Number::Flonum(-b.as_f64()),
            Number::Rational(r) => Number::Flonum(-r.to_f64(ctx)),
            Number::Complex(c) => {
                let imag = c.imag.negate(ctx).to_inexact(ctx);
                let real = c.real.negate(ctx).to_inexact(ctx);
                Self::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn normalize_integer(&self) -> Self {
        match self {
            Self::BigInt(n) => n.try_as_i64().map_or_else(
                || *self,
                |v| {
                    if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
                        Number::Fixnum(v as i32)
                    } else {
                        *self
                    }
                },
            ),
            _ => *self,
        }
    }

    pub fn normalize_complex(ctx: MutationContext<'gc>, real: Self, imag: Self) -> Self {
        assert!(!imag.is_complex());
        assert!(!real.is_complex());

        if let Number::Fixnum(0) = imag {
            return real;
        }

        if let Number::BigInt(b) = imag
            && b.is_zero()
        {
            return real;
        }

        if real.is_flonum() || imag.is_flonum() {
            return Number::Complex(Complex::new(
                ctx,
                real.to_inexact(ctx),
                imag.to_inexact(ctx),
            ));
        }

        Number::Complex(Complex::new(ctx, real, imag))
    }

    pub fn reduce_fix_fix(ctx: MutationContext<'gc>, nume: i32, deno: i32) -> Self {
        let mut nume = nume as SignedDigit;
        let mut deno = deno as SignedDigit;

        if deno == 1 {
            return Number::Fixnum(nume as _);
        }

        if deno == -1 {
            return Number::Fixnum(nume as _).negate(ctx);
        }

        if nume == 0 {
            return Number::Fixnum(0);
        }

        if nume == 1 {
            if deno < 0 {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    (-deno).into_number(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                1i32.into_number(ctx),
                deno.into_number(ctx),
            ));
        }

        let mut ans_sign = 1;

        if nume < 0 {
            ans_sign = -ans_sign;
            nume = -nume;
        }

        if deno < 0 {
            ans_sign = -ans_sign;
            deno = -deno;
        }

        let mut n1 = nume;
        let mut n2 = deno;

        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }

        let gcd = n1;

        if deno == gcd {
            return (ans_sign * nume / gcd).into_number(ctx);
        }

        Self::Rational(Rational::new(
            ctx,
            (ans_sign * nume / gcd).into_number(ctx),
            (deno / gcd).into_number(ctx),
        ))
    }

    pub fn reduce_fix_big(
        ctx: MutationContext<'gc>,
        nume: i32,
        deno: Gc<'gc, BigInt<'gc>>,
    ) -> Self {
        let mut nume = nume as SignedDigit;
        if nume == 0 {
            return Number::Fixnum(0);
        }

        if nume == 1 {
            if deno.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    Self::BigInt(deno).negate(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                1i32.into_number(ctx),
                Self::BigInt(deno),
            ));
        }

        if nume == -1 {
            // -1 / -d = 1 / d
            if deno.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    1i32.into_number(ctx),
                    Self::BigInt(deno).negate(ctx),
                ));
            }

            return Number::Rational(Rational::new(
                ctx,
                (-1i32).into_number(ctx),
                Self::BigInt(deno),
            ));
        }

        let mut ans_sign = 1;
        if nume < 0 {
            ans_sign = -ans_sign;
            nume = -nume;
        }

        if deno.is_negative() {
            ans_sign = -ans_sign;
        }

        let mut n1 = deno.remainder_digit(nume as _) as SignedDigit;
        let mut n2 = nume;

        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }

        let gcd = n1;
        nume /= gcd;
        if ans_sign < 0 {
            nume = -nume;
        }

        let (mut quo, _) = BigInt::div_digit(deno, ctx, gcd as _);
        if quo.is_negative() {
            quo = BigInt::negate(quo, ctx);
        }

        let ans_nume = nume.into_number(ctx);
        let ans_deno = quo.into_number(ctx);

        if matches!(ans_deno, Number::Fixnum(1)) {
            return ans_nume;
        }

        Self::Rational(Rational::new(ctx, ans_nume, ans_deno))
    }

    pub fn reduce_big_fix(
        ctx: MutationContext<'gc>,
        nume: Gc<'gc, BigInt<'gc>>,
        deno: i32,
    ) -> Self {
        let mut nume = nume;
        let mut deno = deno as SignedDigit;

        if nume.is_zero() {
            return Number::Fixnum(0);
        }
        if deno == 1 {
            return Number::BigInt(nume);
        }
        if deno == -1 {
            return Number::BigInt(BigInt::negate(nume, ctx));
        }

        let mut ans_sign = 1;
        if deno < 0 {
            ans_sign = -ans_sign;
            deno = -deno;
        }
        if nume.is_negative() {
            ans_sign = -ans_sign;
            nume = BigInt::negate(nume, ctx);
        }

        let mut n1 = nume.remainder_digit(deno as Digit) as SignedDigit;
        let mut n2 = deno;
        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }
        let gcd = n1;
        deno /= gcd;

        let (mut quo, _) = BigInt::div_digit(nume, ctx, gcd as Digit);
        if ans_sign < 0 {
            quo = BigInt::negate(quo, ctx);
        }
        let ans_nume = quo.into_number(ctx);
        let ans_deno = deno.into_number(ctx);

        if let Number::Fixnum(1) = ans_deno {
            return ans_nume;
        }
        Self::Rational(Rational::new(ctx, ans_nume, ans_deno))
    }

    pub fn reduce(ctx: MutationContext<'gc>, numerator: Self, denominator: Self) -> Self {
        assert!(matches!(numerator, Number::Fixnum(_) | Number::BigInt(_)));
        assert!(matches!(denominator, Number::Fixnum(_) | Number::BigInt(_)));
        if let Number::Fixnum(nume) = numerator {
            if let Number::Fixnum(deno) = denominator {
                return Self::reduce_fix_fix(ctx, nume, deno);
            }

            return Self::reduce_fix_big(
                ctx,
                nume,
                denominator.as_bigint().expect("value is bigint"),
            );
        }

        if let Number::Fixnum(deno) = denominator {
            return Self::reduce_big_fix(
                ctx,
                numerator.as_bigint().expect("value is bigint"),
                deno,
            );
        }

        if matches!(denominator, Number::Fixnum(1)) {
            return numerator;
        }

        if matches!(denominator, Number::Fixnum(-1)) {
            return numerator.negate(ctx);
        }

        if matches!(numerator, Number::Fixnum(0)) {
            return Number::Fixnum(0);
        }

        if matches!(numerator, Number::Fixnum(1)) {
            if denominator.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
                    denominator.negate(ctx),
                ));
            }
            return Number::Rational(Rational::new(ctx, 1i32.into_number(ctx), denominator));
        }

        if matches!(numerator, Number::Fixnum(-1)) {
            // -1 / -d = 1 / d
            if denominator.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    1i32.into_number(ctx),
                    denominator.negate(ctx),
                ));
            }
            return Number::Rational(Rational::new(ctx, (-1i32).into_number(ctx), denominator));
        }

        let mut n1 = match numerator {
            Number::Fixnum(deno) => BigInt::from_i64(ctx, deno as i64),
            Number::BigInt(b) => b,
            _ => unreachable!(),
        };
        let mut n2 = match denominator {
            Number::Fixnum(nume) => BigInt::from_i64(ctx, nume as i64),
            Number::BigInt(b) => b,
            _ => unreachable!(),
        };

        let mut ans_sign = 1;

        if n1.is_negative() {
            ans_sign = -ans_sign;
            n1 = BigInt::negate(n1, ctx)
        }

        if n2.is_negative() {
            ans_sign = -ans_sign;
            n2 = BigInt::negate(n2, ctx);
        }

        let gcd = BigInt::gcd(n1, ctx, n2);

        // `n1` and `n2` are magnitudes; the sign is applied once, below.
        let mut res = BigInt::div(n1, ctx, gcd);
        if ans_sign < 0 {
            res = BigInt::negate(res, ctx);
        }

        let den = BigInt::div(n2, ctx, gcd);
        if den.try_as_i64() == Some(1) {
            return res.into_number(ctx);
        }

        Self::Rational(Rational::new(
            ctx,
            res.into_number(ctx),
            den.into_number(ctx),
        ))
    }

}
