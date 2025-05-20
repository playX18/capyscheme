use std::{
    cell::Cell, fmt, mem::{align_of, size_of, MaybeUninit}, ops::{Deref, Index}, sync::atomic::{AtomicU16, Ordering}
};

use rand::rand_core::le;
use rsgc::{
    Collect, EnsureGCInfo, GCInfo, GCInfoIndex, GCInfoIndexForT, Gc, Trace, Visitor,
    alloc::array::Array,
    gc::GLOBAL_GC_INFO_TABLE,
    generic_static::Namespace,
    vmkit::prelude::{GCMetadata, TraceCallback},
};

use crate::runtime::Context;

use super::{Tagged, TypeCode8, TypeCode16, Value, ValuesNamespace};

pub const DIGIT_BIT: usize = 64;
pub const DIGIT_MASK: u128 = u64::MAX as u128;
pub const DIGIT_BIT_SHIFT_COUNT: u64 = 64;

pub type Digit = u64;

#[derive(Collect)]
#[collect(no_drop)]
pub struct BigInt<'gc> {
    words: Gc<'gc, Array<Digit>>,
    negative: bool,
}

const fn hiword(x: u128) -> u64 {
    ((x >> DIGIT_BIT) & DIGIT_MASK) as u64
}

const fn loword(x: u128) -> u64 {
    (x & DIGIT_MASK) as u64
}

const fn joinwords(lo: u64, hi: u64) -> u128 {
    ((hi as u128) << DIGIT_BIT) | (lo as u128)
}

impl<'gc> BigInt<'gc> {
    const BASE: u128 = u64::MAX as u128 + 1;
}

pub struct BigIntBase<'a> {
    pub digit_space: &'a [char],
    pub digit_map: &'a [(char, u8)],
}

impl<'a> BigIntBase<'a> {
    pub const BIN: Self = Self {
        digit_space: &['0', '1'],
        digit_map: &[('0', 0), ('1', 1)],
    };

    pub const OCT: Self = Self {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
        ],
    };

    pub const DEC: Self = Self {
        digit_space: &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
        ],
    };

    pub const HEX: Self = Self {
        digit_space: &[
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        ],
        digit_map: &[
            ('0', 0),
            ('1', 1),
            ('2', 2),
            ('3', 3),
            ('4', 4),
            ('5', 5),
            ('6', 6),
            ('7', 7),
            ('8', 8),
            ('9', 9),
            ('A', 10),
            ('B', 11),
            ('C', 12),
            ('D', 13),
            ('E', 14),
            ('F', 15),
        ],
    };

    pub const fn radix(&self) -> u64 {
        self.digit_space.len() as u64
    }

    pub const fn from_radix(radix: u64) -> Self {
        match radix {
            2 => Self::BIN,
            8 => Self::OCT,
            10 => Self::DEC,
            16 => Self::HEX,
            _ => panic!("Unsupported radix"),
        }
    }
}

unsafe impl<'gc> Tagged for BigInt<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::BIG];
    const ONLY_TC16: bool = true;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Complex<'gc> {
    real: Value<'gc>,
    imag: Value<'gc>,
}

unsafe impl<'gc> Tagged for Complex<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::COMPLEX];
    const ONLY_TC16: bool = true;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Rational<'gc> {
    numerator: Value<'gc>,
    denominator: Value<'gc>,
}

unsafe impl<'gc> Tagged for Rational<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::RATIONAL];
    const ONLY_TC16: bool = true;
}

impl<'gc> BigInt<'gc> {
    pub fn from_slice(
        ctx: Context<'gc>,
        slice: impl AsRef<[u64]>,
        negative: bool,
    ) -> Gc<'gc, Self> {
        let mut words = slice.as_ref();
        while words.len() > 1 && words[words.len() - 1] == 0 {
            words = &words[..words.len() - 1];
        }

        Gc::new(
            &ctx,
            Self {
                words: Array::new(&ctx, words.iter().copied()),
                negative,
            },
        )
    }

    pub fn from_u64_words(ctx: Context<'gc>, words: &[u64], negative: bool) -> Gc<'gc, Self> {

        Self::from_slice(ctx, words, negative)
    }

    pub fn from_u64(ctx: Context<'gc>, value: u64) -> Gc<'gc, Self> {
        Self::from_slice(ctx, [value], false)
    }

    pub fn from_i64(ctx: Context<'gc>, value: i64) -> Gc<'gc, Self> {
        let absvalue = if value == i64::MIN {
            i64::MAX as u64 + 1
        } else if value < 0 {
            -value as u64
        } else {
            value as u64
        };

        let negative = value < 0;

        Self::from_slice(ctx, [absvalue], negative)
    }

    pub fn from_f64(ctx: Context<'gc>, value: f64) -> Gc<'gc, Self> {
        if value > -1.0 || value < 1.0 {
            return Self::from_slice(ctx, [0], value < 0.0);
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = value.abs() as u64;
            return Self::from_slice(ctx, [absvalue], value < 0.0);
        } else {
            todo!()
        }
    }

    pub fn from_digits(
        ctx: Context<'gc>,
        digits: &[u8],
        radix: u64,
        negative: bool,
    ) -> Gc<'gc, Self> {
        if radix < 2 || radix > 256 {
            // Radix must be between 2 and 256 (inclusive) because digits are u8.
            // A digit's value must be less than the radix.
            // If radix is 256, max digit value is 255 (u8::MAX).
            // If radix were > 256, say 257, then u8 could not represent all possible digit values (e.g., 256).
            panic!("Radix must be between 2 and 256 for u8 digits");
        }

        if digits.is_empty() {
            return Self::from_u64(ctx, 0);
        }

        let mut words: Vec<u64> = vec![0];

        for &digit_byte in digits {
            let digit_val = digit_byte as u64;
            if digit_val >= radix {
                panic!("Invalid digit {} for radix {}", digit_val, radix);
            }

            // Multiply current words by radix
            let mut carry: u128 = 0;
            for word_ref in words.iter_mut() {
                let prod = (*word_ref as u128) * (radix as u128) + carry;
                *word_ref = loword(prod);
                carry = hiword(prod) as u128;
            }
            if carry > 0 {
                words.push(carry as u64);
            }

            // Add new digit_val to words
            let mut current_add_carry: u128 = digit_val as u128;
            for word_ref in words.iter_mut() {
                if current_add_carry == 0 {
                    break;
                }
                let sum = (*word_ref as u128) + current_add_carry;
                *word_ref = loword(sum);
                current_add_carry = hiword(sum) as u128;
            }
            if current_add_carry > 0 {
                words.push(current_add_carry as u64);
            }
        }
        
        let is_zero = words.len() == 1 && words[0] == 0;
        let actual_negative = if is_zero { false } else { negative };

        Self::from_slice(ctx, &words, actual_negative)
    }

    pub fn is_zero(&self) -> bool {
        (self.words.len() == 1 && self.words[0] == 0) || self.words.is_empty()
    }

    pub fn to_string(&self, base_info: &BigIntBase) -> String {
        if self.is_zero() {
            // Ensure digit_space is not empty, though radix check below would also catch it.
            if base_info.digit_space.is_empty() {
                panic!("Cannot convert zero to string: digit_space is empty.");
            }
            return base_info.digit_space[0].to_string();
        }

        let radix_u64 = base_info.radix();
        if radix_u64 < 2 {
            panic!("Radix for to_string must be >= 2. Got: {}", radix_u64);
        }
        if base_info.digit_space.len() < radix_u64 as usize {
            panic!("digit_space has fewer characters than the radix requires.");
        }


        let mut temp_num_words: Vec<u64> = self.words.iter().copied().collect();
        let mut result_chars: Vec<char> = Vec::new();

        loop {
            let mut is_temp_zero = true;
            if temp_num_words.is_empty() { // Should be caught by self.is_zero or normalized to [0]
                 is_temp_zero = true;
            } else {
                for &word in &temp_num_words {
                    if word != 0 {
                        is_temp_zero = false;
                        break;
                    }
                }
            }
            if is_temp_zero {
                break;
            }

            let mut k_remainder: u128 = 0; // Remainder from division, carried to next less significant word processing
            // new_quotient_words will store the quotient of temp_num_words / radix_u64
            // It's built LSW-first, same as temp_num_words.
            let mut new_quotient_words: Vec<u64> = vec![0; temp_num_words.len()];

            // Iterate from Most Significant Word to Least Significant Word of temp_num_words
            for i in (0..temp_num_words.len()).rev() {
                let u_i = temp_num_words[i]; // Current word from dividend (u_i from Knuth)
                // Combine k_remainder (from more significant part) with current word u_i
                let dividend_segment: u128 = (k_remainder << DIGIT_BIT_SHIFT_COUNT) | (u_i as u128);
                
                let q_i = dividend_segment / (radix_u64 as u128); // Quotient part for this word
                k_remainder = dividend_segment % (radix_u64 as u128);    // New remainder to carry over

                new_quotient_words[i] = q_i as u64; // q_i fits in u64
            }
            
            // The final k_remainder is the remainder of the entire number division: (BigInt % radix_u64)
            result_chars.push(base_info.digit_space[k_remainder as usize]);

            // Normalize new_quotient_words (it's LSW first)
            // Remove trailing zeros (which are MSW zeros of the quotient)
            let mut effective_len = new_quotient_words.len();
            while effective_len > 1 && new_quotient_words[effective_len - 1] == 0 {
                effective_len -= 1;
            }
            
            if effective_len == 0 { // Should not happen if original number was not zero
                temp_num_words = vec![0];
            } else {
                temp_num_words = new_quotient_words[0..effective_len].to_vec();
                 // If quotient is truly zero, it should be [0] after this.
                if temp_num_words.is_empty() && effective_len == 1 && new_quotient_words[0] == 0 {
                    temp_num_words = vec![0];
                } else if temp_num_words.is_empty() { // Safeguard, if all words were stripped
                     temp_num_words = vec![0];
                }
            }
        }
        
        // If after all operations result_chars is empty, it means the number was 0.
        // This should have been caught by the initial self.is_zero() check.
        if result_chars.is_empty() {
             if base_info.digit_space.is_empty() { // Should be caught by radix check
                panic!("Cannot convert zero to string: digit_space is empty.");
            }
            return base_info.digit_space[0].to_string();
        }

        let mut final_string = String::with_capacity(result_chars.len() + if self.negative { 1 } else { 0 });
        if self.negative {
            final_string.push('-');
        }
        
        // result_chars are in LSB to MSB order, so iterate in reverse.
        for c in result_chars.iter().rev() {
            final_string.push(*c);
        }

        final_string
    }

    pub fn from_string(
        ctx: Context<'gc>,
        s: &str,
        base_info: &BigIntBase,
    ) -> Result<Gc<'gc, Self>, String> {
        if s.is_empty() {
            return Err("Input string cannot be empty".to_string());
        }

        let radix = base_info.radix();
        if radix < 2 {
            // digit_map lookup would fail for radix < 2 if not caught here.
            // Also, from_digits has a similar check for u8 digits.
            return Err(format!("Radix must be >= 2. Got: {}", radix));
        }
        
        // Build a reverse map for faster digit lookup if digit_map is not sorted
        // or if direct indexing by char is not feasible/efficient.
        // For small, fixed maps like BIN, OCT, DEC, HEX, direct iteration is fine.
        // For a more general BigIntBase, a HashMap might be better if performance critical.
        // Here, we'll iterate through digit_map for simplicity.
        
        let mut chars = s.chars().peekable();
        let mut negative = false;

        if let Some(&ch) = chars.peek() {
            if ch == '-' {
                negative = true;
                chars.next(); // Consume '-'
            } else if ch == '+' {
                chars.next(); // Consume '+'
            }
        }

        // Collect actual digit characters
        let digit_chars: Vec<char> = chars.collect();

        if digit_chars.is_empty() {
            return Err("Input string has no digits after sign".to_string());
        }

        let mut words: Vec<u64> = vec![0];

        for (char_idx, &ch) in digit_chars.iter().enumerate() {
            let digit_val_opt = base_info
                .digit_map
                .iter()
                .find(|&&(map_char, _)| map_char == ch)
                .map(|&(_, val)| val as u64);

            let digit_val = match digit_val_opt {
                Some(val) => val,
                None => return Err(format!("Invalid character '{}' for the given base at position {}", ch, char_idx)),
            };

            if digit_val >= radix {
                 // This should ideally be caught by the digit_map lookup if map is correct for radix
                return Err(format!(
                    "Digit '{}' (value {}) is out of range for radix {} at position {}",
                    ch, digit_val, radix, char_idx
                ));
            }

            // Multiply current words by radix
            let mut carry: u128 = 0;
            for word_ref in words.iter_mut() {
                let prod = (*word_ref as u128) * (radix as u128) + carry;
                *word_ref = loword(prod);
                carry = hiword(prod) as u128;
            }
            if carry > 0 {
                words.push(carry as u64);
            }

            // Add new digit_val to words
            let mut current_add_carry: u128 = digit_val as u128;
            for word_ref in words.iter_mut() {
                if current_add_carry == 0 {
                    break;
                }
                let sum = (*word_ref as u128) + current_add_carry;
                *word_ref = loword(sum);
                current_add_carry = hiword(sum) as u128;
            }
            if current_add_carry > 0 {
                words.push(current_add_carry as u64);
            }
        }
        
        let is_zero = words.len() == 1 && words[0] == 0;
        let actual_negative = if is_zero { false } else { negative };

        Ok(Self::from_slice(ctx, &words, actual_negative))
    }
}

impl<'gc> Index<usize> for BigInt<'gc> {
    type Output = Digit;

    fn index(&self, index: usize) -> &Self::Output {
        &self.words[index]
    }
}

impl<'gc> PartialEq for BigInt<'gc> {
    fn eq(&self, other: &Self) -> bool {
        if self.negative != other.negative {
            return false;
        }

        if self.words.len() != other.words.len() {
            return false;
        }

        for i in 0..self.words.len() {
            if self.words[i] != other.words[i] {
                return false;
            }
        }

        true
    }
}

impl<'gc> Eq for BigInt<'gc> {

}