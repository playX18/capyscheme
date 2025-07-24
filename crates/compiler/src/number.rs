//! R7RS Number Parser
//!
//! This module implements parsing of R7RS numbers according to the specification.
//! It handles the complete numerical tower including:
//! - Integers (exact and inexact)
//! - Rationals
//! - Real numbers
//! - Complex numbers
//! - Various radixes (binary, octal, decimal, hexadecimal)
//! - Exactness prefixes (#e, #i)
use num::{BigInt, BigRational, Num, One, ToPrimitive, Zero};
use std::{fmt, hash::Hash, rc::Rc};

/// R6RS Numerical Tower
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    ExactFixnum(i64),
    /// Exact integers (arbitrary precision)
    ExactInteger(Rc<BigInt>),

    /// Exact rationals
    ExactRational(Rc<BigRational>),

    /// Inexact reals (typically f64)
    InexactReal(f64),

    /// Exact complex numbers
    ExactComplex(Rc<(BigInt, BigInt)>), // real and imaginary parts
    /// Inexact complex numbers
    InexactComplex {
        real: f64,
        imag: f64,
    },
}

impl Number {
    pub fn from_uinteger(digits: &String, radix: u32) -> Self {
        let value = BigInt::from_str_radix(digits, radix).unwrap();
        Number::ExactInteger(Rc::new(value))
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Number::ExactFixnum(i) => i.hash(state),
            Number::ExactInteger(i) => i.hash(state),
            Number::ExactRational(r) => r.hash(state),
            Number::InexactReal(f) => f.to_bits().hash(state),
            Number::ExactComplex(cn) => {
                cn.0.hash(state);
                cn.1.hash(state);
            }
            Number::InexactComplex { real, imag } => {
                real.to_bits().hash(state);
                imag.to_bits().hash(state);
            }
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::ExactFixnum(i) => write!(f, "{}", i),
            Number::ExactInteger(i) => write!(f, "{}", i),
            Number::ExactRational(r) => {
                if r.denom() == &BigInt::from(1) {
                    write!(f, "{}", r.numer())
                } else {
                    write!(f, "{}/{}", r.numer(), r.denom())
                }
            }
            Number::InexactReal(real) => {
                if real.is_infinite() {
                    if *real > 0.0 {
                        write!(f, "+inf.0")
                    } else {
                        write!(f, "-inf.0")
                    }
                } else if real.is_nan() {
                    write!(f, "+nan.0")
                } else if real.fract() == 0.0 {
                    write!(f, "{}.0", *real as i64)
                } else {
                    write!(f, "{}", real)
                }
            }
            Number::ExactComplex(c) => {
                let (real, imag) = c.as_ref();
                if real.is_zero() {
                    if imag.is_one() {
                        write!(f, "i")
                    } else if *imag == BigInt::from(-1) {
                        write!(f, "-i")
                    } else {
                        write!(f, "{}i", imag)
                    }
                } else if imag.is_zero() {
                    write!(f, "{}", real)
                } else if imag.is_one() {
                    write!(f, "{}+i", real)
                } else if *imag == BigInt::from(-1) {
                    write!(f, "{}-i", real)
                } else if imag > &BigInt::zero() {
                    write!(f, "{}+{}i", real, imag)
                } else {
                    write!(f, "{}{}i", real, imag)
                }
            }
            Number::InexactComplex { real, imag } => {
                if *real == 0.0 {
                    if *imag == 1.0 {
                        write!(f, "+i")
                    } else if *imag == -1.0 {
                        write!(f, "-i")
                    } else {
                        write!(f, "{}i", imag)
                    }
                } else if *imag == 0.0 {
                    if real.fract() == 0.0 {
                        write!(f, "{}.0", *real as i64)
                    } else {
                        write!(f, "{}", real)
                    }
                } else if *imag == 1.0 {
                    if real.fract() == 0.0 {
                        write!(f, "{}.0+i", *real as i64)
                    } else {
                        write!(f, "{}+i", real)
                    }
                } else if *imag == -1.0 {
                    if real.fract() == 0.0 {
                        write!(f, "{}.0-i", *real as i64)
                    } else {
                        write!(f, "{}-i", real)
                    }
                } else if *imag > 0.0 {
                    let real_str = if real.fract() == 0.0 {
                        format!("{}.0", *real as i64)
                    } else {
                        format!("{}", real)
                    };
                    let imag_str = if imag.fract() == 0.0 {
                        format!("{}.0", *imag as i64)
                    } else {
                        format!("{}", imag)
                    };
                    write!(f, "{}+{}i", real_str, imag_str)
                } else {
                    let real_str = if real.fract() == 0.0 {
                        format!("{}.0", *real as i64)
                    } else {
                        format!("{}", real)
                    };
                    let imag_str = if imag.fract() == 0.0 {
                        format!("{}.0", *imag as i64)
                    } else {
                        format!("{}", imag)
                    };
                    write!(f, "{}{}i", real_str, imag_str)
                }
            }
        }
    }
}

impl Number {
    /// Check if the number is exact
    pub fn is_exact(&self) -> bool {
        matches!(
            self,
            Number::ExactInteger(_) | Number::ExactRational(_) | Number::ExactComplex { .. }
        )
    }

    /// Check if the number is inexact
    pub fn is_inexact(&self) -> bool {
        !self.is_exact()
    }

    /// Check if the number is real (not complex)
    pub fn is_real(&self) -> bool {
        matches!(
            self,
            Number::ExactInteger(_) | Number::ExactRational(_) | Number::InexactReal(_)
        )
    }

    /// Check if the number is complex
    pub fn is_complex(&self) -> bool {
        matches!(
            self,
            Number::ExactComplex { .. } | Number::InexactComplex { .. }
        )
    }

    /// Check if the number is an integer
    pub fn is_integer(&self) -> bool {
        match self {
            Number::ExactInteger(_) => true,
            Number::ExactRational(r) => r.denom() == &BigInt::from(1),
            Number::InexactReal(f) => f.fract() == 0.0 && f.is_finite(),
            _ => false,
        }
    }
}

/// Number prefix information (exactness and radix)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberPrefix {
    pub exactness: Option<Exactness>,
    pub radix: Radix,
}

impl NumberPrefix {
    pub fn new() -> Self {
        Self {
            exactness: None,
            radix: Radix::Decimal,
        }
    }

    pub fn with_exactness(mut self, exactness: Exactness) -> Self {
        self.exactness = Some(exactness);
        self
    }

    pub fn with_radix(mut self, radix: Radix) -> Self {
        self.radix = radix;
        self
    }
}

impl Default for NumberPrefix {
    fn default() -> Self {
        Self::new()
    }
}

/// Exactness specification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exactness {
    /// #e prefix - force exact
    Exact,

    /// #i prefix - force inexact
    Inexact,
}

/// Radix specification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Radix {
    /// #b prefix - binary
    Binary,

    /// #o prefix - octal
    Octal,

    /// #d prefix - decimal (default)
    Decimal,

    /// #x prefix - hexadecimal
    Hexadecimal,
}

impl Radix {
    /// Get the numeric base for this radix
    pub fn base(&self) -> u32 {
        match self {
            Radix::Binary => 2,
            Radix::Octal => 8,
            Radix::Decimal => 10,
            Radix::Hexadecimal => 16,
        }
    }
}

/// Parse a number string into a Number AST node
pub fn parse_number(text: &str) -> Result<Number, NumberParseError> {
    let mut parser = NumberParser::new(text);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberParseError {
    InvalidFormat(String),
    InvalidRadix(String),
    InvalidExactness(String),
    InvalidDigit(char, u32),
    EmptyNumber,
    MultipleDecimalPoints,
    InvalidComplex(String),
}

impl std::fmt::Display for NumberParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberParseError::InvalidFormat(s) => write!(f, "Invalid number format: {}", s),
            NumberParseError::InvalidRadix(s) => write!(f, "Invalid radix: {}", s),
            NumberParseError::InvalidExactness(s) => write!(f, "Invalid exactness: {}", s),
            NumberParseError::InvalidDigit(c, radix) => {
                write!(f, "Invalid digit '{}' for radix {}", c, radix)
            }
            NumberParseError::EmptyNumber => write!(f, "Empty number"),
            NumberParseError::MultipleDecimalPoints => {
                write!(f, "Multiple decimal points in number")
            }
            NumberParseError::InvalidComplex(s) => write!(f, "Invalid complex number: {}", s),
        }
    }
}

impl std::error::Error for NumberParseError {}

struct NumberParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> NumberParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse(&mut self) -> Result<Number, NumberParseError> {
        if self.input.is_empty() {
            return Err(NumberParseError::EmptyNumber);
        }

        // Parse prefix (exactness and radix)
        let prefix = self.parse_prefix()?;

        // Parse the main number part
        self.parse_number_with_prefix(prefix)
    }

    fn parse_prefix(&mut self) -> Result<NumberPrefix, NumberParseError> {
        let mut prefix = NumberPrefix::new();

        while self.pos < self.input.len() && self.current_char() == '#' {
            self.advance(); // consume '#'

            if self.pos >= self.input.len() {
                return Err(NumberParseError::InvalidFormat(
                    "Incomplete prefix".to_string(),
                ));
            }

            match self.current_char() {
                'e' | 'E' => {
                    if prefix.exactness.is_some() {
                        return Err(NumberParseError::InvalidExactness(
                            "Duplicate exactness prefix".to_string(),
                        ));
                    }
                    prefix.exactness = Some(Exactness::Exact);
                    self.advance();
                }
                'i' | 'I' => {
                    if prefix.exactness.is_some() {
                        return Err(NumberParseError::InvalidExactness(
                            "Duplicate exactness prefix".to_string(),
                        ));
                    }
                    prefix.exactness = Some(Exactness::Inexact);
                    self.advance();
                }
                'b' | 'B' => {
                    if prefix.radix != Radix::Decimal {
                        return Err(NumberParseError::InvalidRadix(
                            "Duplicate radix prefix".to_string(),
                        ));
                    }
                    prefix.radix = Radix::Binary;
                    self.advance();
                }
                'o' | 'O' => {
                    if prefix.radix != Radix::Decimal {
                        return Err(NumberParseError::InvalidRadix(
                            "Duplicate radix prefix".to_string(),
                        ));
                    }
                    prefix.radix = Radix::Octal;
                    self.advance();
                }
                'd' | 'D' => {
                    if prefix.radix != Radix::Decimal {
                        return Err(NumberParseError::InvalidRadix(
                            "Duplicate radix prefix".to_string(),
                        ));
                    }
                    // Radix is already decimal by default
                    self.advance();
                }
                'x' | 'X' => {
                    if prefix.radix != Radix::Decimal {
                        return Err(NumberParseError::InvalidRadix(
                            "Duplicate radix prefix".to_string(),
                        ));
                    }
                    prefix.radix = Radix::Hexadecimal;
                    self.advance();
                }
                c => {
                    return Err(NumberParseError::InvalidFormat(format!(
                        "Unknown prefix character: {}",
                        c
                    )));
                }
            }
        }

        Ok(prefix)
    }

    fn parse_number_with_prefix(
        &mut self,
        prefix: NumberPrefix,
    ) -> Result<Number, NumberParseError> {
        let remaining = &self.input[self.pos..];

        // Check for complex numbers (contains 'i' or 'j' at the end, or '+' or '-' in the middle)
        if self.is_complex_number(remaining) {
            return self.parse_complex_number(prefix);
        }

        // Check for rational numbers (contains '/')
        if remaining.contains('/') {
            return self.parse_rational_number(prefix);
        }

        // Check for decimal numbers (contains '.' or scientific notation)
        if remaining.contains('.') || remaining.to_lowercase().contains('e') {
            return self.parse_decimal_number(prefix);
        }

        // Parse as integer
        self.parse_integer_number(prefix)
    }

    fn is_complex_number(&self, s: &str) -> bool {
        // Simple heuristic: ends with 'i' or contains '+' or '-' in the middle
        s.ends_with('i')
            || s.ends_with('I')
            || (s.len() > 1 && (s[1..].contains('+') || s[1..].contains('-')))
    }

    fn parse_integer_number(&mut self, prefix: NumberPrefix) -> Result<Number, NumberParseError> {
        let remaining = &self.input[self.pos..];
        let radix = prefix.radix.base();

        // Handle sign
        let (sign, digits) = if remaining.starts_with('+') {
            (1, &remaining[1..])
        } else if remaining.starts_with('-') {
            (-1, &remaining[1..])
        } else {
            (1, remaining)
        };

        if digits.is_empty() {
            return Err(NumberParseError::EmptyNumber);
        }

        // Validate digits for the given radix
        for ch in digits.chars() {
            if !self.is_valid_digit(ch, radix) {
                return Err(NumberParseError::InvalidDigit(ch, radix));
            }
        }

        // Parse the integer
        let abs_value = BigInt::from_str_radix(digits, radix)
            .map_err(|_| NumberParseError::InvalidFormat(remaining.to_string()))?;

        let value = if sign == -1 { -abs_value } else { abs_value };

        // Determine if should be exact or inexact
        let should_be_inexact = matches!(prefix.exactness, Some(Exactness::Inexact));

        if should_be_inexact {
            // Convert to inexact (floating point)
            let float_val = value.to_string().parse::<f64>().map_err(|_| {
                NumberParseError::InvalidFormat("Number too large for f64".to_string())
            })?;
            Ok(Number::InexactReal(float_val))
        } else {
            // Check if it fits in a fixnum (i64)
            if let Some(fixnum) = value.to_i64() {
                Ok(Number::ExactFixnum(fixnum))
            } else {
                Ok(Number::ExactInteger(Rc::new(value)))
            }
        }
    }

    fn parse_rational_number(&mut self, prefix: NumberPrefix) -> Result<Number, NumberParseError> {
        let remaining = &self.input[self.pos..];
        let radix = prefix.radix.base();

        let parts: Vec<&str> = remaining.split('/').collect();
        if parts.len() != 2 {
            return Err(NumberParseError::InvalidFormat(
                "Invalid rational format".to_string(),
            ));
        }

        let numerator_str = parts[0];
        let denominator_str = parts[1];

        // Parse numerator
        let (num_sign, num_digits) = if numerator_str.starts_with('+') {
            (1, &numerator_str[1..])
        } else if numerator_str.starts_with('-') {
            (-1, &numerator_str[1..])
        } else {
            (1, numerator_str)
        };

        // Validate numerator digits
        for ch in num_digits.chars() {
            if !self.is_valid_digit(ch, radix) {
                return Err(NumberParseError::InvalidDigit(ch, radix));
            }
        }

        // Validate denominator digits
        for ch in denominator_str.chars() {
            if !self.is_valid_digit(ch, radix) {
                return Err(NumberParseError::InvalidDigit(ch, radix));
            }
        }

        let numerator = BigInt::from_str_radix(num_digits, radix)
            .map_err(|_| NumberParseError::InvalidFormat("Invalid numerator".to_string()))?;
        let numerator = if num_sign == -1 {
            -numerator
        } else {
            numerator
        };

        let denominator = BigInt::from_str_radix(denominator_str, radix)
            .map_err(|_| NumberParseError::InvalidFormat("Invalid denominator".to_string()))?;

        if denominator.is_zero() {
            return Err(NumberParseError::InvalidFormat(
                "Division by zero".to_string(),
            ));
        }

        let rational = BigRational::new(numerator, denominator);

        // Determine if should be exact or inexact
        let should_be_inexact = matches!(prefix.exactness, Some(Exactness::Inexact));

        if should_be_inexact {
            // Convert to inexact (floating point)
            let float_val = rational.to_f64().unwrap_or(f64::NAN);
            Ok(Number::InexactReal(float_val))
        } else {
            Ok(Number::ExactRational(Rc::new(rational)))
        }
    }

    fn parse_decimal_number(&mut self, prefix: NumberPrefix) -> Result<Number, NumberParseError> {
        let remaining = &self.input[self.pos..];

        // For non-decimal radix, decimal points are not allowed
        if prefix.radix != Radix::Decimal && remaining.contains('.') {
            return Err(NumberParseError::InvalidFormat(
                "Decimal point not allowed in non-decimal radix".to_string(),
            ));
        }

        // Parse as floating point number
        let float_val = remaining
            .parse::<f64>()
            .map_err(|_| NumberParseError::InvalidFormat(remaining.to_string()))?;

        // Determine if should be exact or inexact
        let should_be_exact = matches!(prefix.exactness, Some(Exactness::Exact));

        if should_be_exact {
            // Try to convert to exact rational
            // This is a simplified approach - a full implementation would need more sophisticated conversion
            if float_val.fract() == 0.0 && float_val.is_finite() {
                // It's actually an integer
                let int_val = float_val as i64;
                Ok(Number::ExactFixnum(int_val))
            } else {
                // Convert to rational (simplified - would need better precision handling)
                let rational = BigRational::from_float(float_val).ok_or_else(|| {
                    NumberParseError::InvalidFormat("Cannot convert to exact rational".to_string())
                })?;
                Ok(Number::ExactRational(Rc::new(rational)))
            }
        } else {
            Ok(Number::InexactReal(float_val))
        }
    }

    fn parse_complex_number(&mut self, prefix: NumberPrefix) -> Result<Number, NumberParseError> {
        let remaining = &self.input[self.pos..];

        // Handle pure imaginary numbers like "3i" or "+i" or "-i"

        // Handle complex numbers like "3+4i" or "3-4i"
        // Find the last '+' or '-' that's not at the beginning
        let mut split_pos = None;
        for (i, ch) in remaining.char_indices().skip(1) {
            if ch == '+' || ch == '-' {
                split_pos = Some(i);
            }
        }

        if let Some(pos) = split_pos {
            let real_part = &remaining[..pos];
            let imag_part = &remaining[pos..];

            // Parse real part
            let real_val = real_part.parse::<f64>().map_err(|_| {
                NumberParseError::InvalidComplex(format!("Invalid real part: {}", real_part))
            })?;

            // Parse imaginary part (remove 'i' at the end)
            let imag_str = if imag_part.ends_with('i') || imag_part.ends_with('I') {
                &imag_part[..imag_part.len() - 1]
            } else {
                return Err(NumberParseError::InvalidComplex(
                    "Missing 'i' in imaginary part".to_string(),
                ));
            };

            let imag_val = if imag_str == "+" {
                1.0
            } else if imag_str == "-" {
                -1.0
            } else {
                imag_str.parse::<f64>().map_err(|_| {
                    NumberParseError::InvalidComplex(format!(
                        "Invalid imaginary part: {}",
                        imag_str
                    ))
                })?
            };

            let should_be_exact = matches!(prefix.exactness, Some(Exactness::Exact));

            if should_be_exact {
                // Convert to exact complex (simplified)
                let real_big = BigInt::from_str_radix(
                    real_part.trim_start_matches(['+', '-']),
                    prefix.radix.base(),
                )
                .map_err(|_| {
                    NumberParseError::InvalidComplex("Invalid exact real part".to_string())
                })?;
                let real_big = if real_part.starts_with('-') {
                    -real_big
                } else {
                    real_big
                };

                let imag_big = if imag_str == "+" {
                    BigInt::one()
                } else if imag_str == "-" {
                    -BigInt::one()
                } else {
                    let abs_imag = BigInt::from_str_radix(
                        imag_str.trim_start_matches(['+', '-']),
                        prefix.radix.base(),
                    )
                    .map_err(|_| {
                        NumberParseError::InvalidComplex("Invalid exact imaginary part".to_string())
                    })?;
                    if imag_str.starts_with('-') {
                        -abs_imag
                    } else {
                        abs_imag
                    }
                };

                Ok(Number::ExactComplex(Rc::new((real_big, imag_big))))
            } else {
                Ok(Number::InexactComplex {
                    real: real_val,
                    imag: imag_val,
                })
            }
        } else {
            let without_i = &remaining[..remaining.len() - 1];

            if without_i.is_empty() || without_i == "+" {
                // Just "i" or "+i" means 0+1i
                return Ok(Number::InexactComplex {
                    real: 0.0,
                    imag: 1.0,
                });
            } else if without_i == "-" {
                // "-i" means 0-1i
                return Ok(Number::InexactComplex {
                    real: 0.0,
                    imag: -1.0,
                });
            }

            // Parse the imaginary part
            let imag_val = without_i
                .parse::<f64>()
                .map_err(|_| NumberParseError::InvalidComplex(remaining.to_string()))?;

            let should_be_exact = matches!(prefix.exactness, Some(Exactness::Exact));

            if should_be_exact {
                let imag_big = BigInt::from_str_radix(
                    without_i.trim_start_matches(['+', '-']),
                    prefix.radix.base(),
                )
                .map_err(|_| {
                    NumberParseError::InvalidComplex("Invalid exact imaginary part".to_string())
                })?;
                let imag_big = if without_i.starts_with('-') {
                    -imag_big
                } else {
                    imag_big
                };
                Ok(Number::ExactComplex(Rc::new((BigInt::zero(), imag_big))))
            } else {
                Ok(Number::InexactComplex {
                    real: 0.0,
                    imag: imag_val,
                })
            }
        }
    }

    fn is_valid_digit(&self, ch: char, radix: u32) -> bool {
        match radix {
            2 => matches!(ch, '0' | '1'),
            8 => matches!(ch, '0'..='7'),
            10 => matches!(ch, '0'..='9'),
            16 => matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F'),
            _ => false,
        }
    }

    fn current_char(&self) -> char {
        self.input.chars().nth(self.pos).unwrap_or('\0')
    }

    fn advance(&mut self) {
        if self.pos < self.input.len() {
            self.pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_integers() {
        assert!(matches!(parse_number("42"), Ok(Number::ExactFixnum(42))));
        assert!(matches!(parse_number("-17"), Ok(Number::ExactFixnum(-17))));
        assert!(matches!(parse_number("+123"), Ok(Number::ExactFixnum(123))));
    }

    #[test]
    fn test_parse_binary_numbers() {
        assert!(matches!(parse_number("#b101"), Ok(Number::ExactFixnum(5))));
        assert!(matches!(
            parse_number("#B1010"),
            Ok(Number::ExactFixnum(10))
        ));
    }

    #[test]
    fn test_parse_hex_numbers() {
        assert!(matches!(parse_number("#xff"), Ok(Number::ExactFixnum(255))));
        assert!(matches!(parse_number("#XFF"), Ok(Number::ExactFixnum(255))));
    }

    #[test]
    fn test_parse_rationals() {
        if let Ok(Number::ExactRational(r)) = parse_number("3/4") {
            assert_eq!(r.numer(), &BigInt::from(3));
            assert_eq!(r.denom(), &BigInt::from(4));
        } else {
            panic!("Expected rational number");
        }
    }

    #[test]
    fn test_parse_decimals() {
        assert!(
            matches!(parse_number("3.14"), Ok(Number::InexactReal(f)) if (f - 3.14).abs() < f64::EPSILON)
        );
        assert!(
            matches!(parse_number("-2.5"), Ok(Number::InexactReal(f)) if (f + 2.5).abs() < f64::EPSILON)
        );
    }

    #[test]
    fn test_parse_complex() {
        assert!(matches!(
            parse_number("3+4i"),
            Ok(Number::InexactComplex {
                real: 3.0,
                imag: 4.0
            })
        ));
        assert!(matches!(
            parse_number("2-5i"),
            Ok(Number::InexactComplex {
                real: 2.0,
                imag: -5.0
            })
        ));
        assert!(matches!(
            parse_number("i"),
            Ok(Number::InexactComplex {
                real: 0.0,
                imag: 1.0
            })
        ));
        assert!(matches!(
            parse_number("-i"),
            Ok(Number::InexactComplex {
                real: 0.0,
                imag: -1.0
            })
        ));
    }

    #[test]
    fn test_exactness_prefixes() {
        assert!(matches!(
            parse_number("#e3.14"),
            Ok(Number::ExactRational(_))
        ));
        assert!(matches!(
            parse_number("#i42"),
            Ok(Number::InexactReal(42.0))
        ));
    }

    #[test]
    fn test_invalid_numbers() {
        assert!(parse_number("").is_err());
        assert!(parse_number("#b102").is_err()); // Invalid binary digit
        assert!(parse_number("3/0").is_err()); // Division by zero
        assert!(parse_number("3.14.15").is_err()); // Multiple decimal points would be caught by f64 parsing
    }
}
