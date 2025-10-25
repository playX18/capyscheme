#![allow(unused_variables)]

//! # Numerical tower of Scheme
//!
//! Implements all the math things from Scheme including numerical tower.
//!
//! # DISCLAIMER
//!
//! THIS FILE IS A BIG MESS. Math works but changes to this file
//! need to be done with care to not break anything.

use core::f64;
use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    iter::Peekable,
    marker::PhantomData,
    mem::{align_of, size_of},
    ops::{Deref, DerefMut, Index, IndexMut, Range, RangeFrom, RangeTo},
    sync::OnceLock,
};

use easy_bitfield::{BitField, BitFieldTrait};
use num_bigint::{BigInt as NumBigInt, Sign as NumSign};

use rand::Rng;
use rsgc::{
    Gc, Rootable, Trace,
    collection::Visitor,
    global::Global,
    mmtk::{AllocationSemantics, util::conversions::raw_align_up},
    mutator::Mutation,
    object::VTable,
};

use crate::runtime::{
    Context,
    value::{ConversionError, ScmHeader, TypeBits},
};

use super::{FromValue, IntoValue, Tagged, TypeCode8, TypeCode16, Value};

pub const DIGIT_BIT: usize = 64;
pub const DIGIT_MASK: u64 = u64::MAX;
pub const DIGIT_BIT_SHIFT_COUNT: u32 = 6;

const IEXPT_2N53: i64 = 0x20000000000000;
const IEXPT_2N52: i64 = 0x10000000000000;

const BASE: Digit2X = Digit::MAX as Digit2X + 1;

pub type Digit2X = u128;
pub type Digit = u64;
pub type IDigit = i64;
pub type IDigit2X = i128;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Sign {
    Negative,
    Zero,
    Positive,
}

impl Sign {
    pub fn flip(&self) -> Sign {
        match self {
            Sign::Negative => Sign::Positive,
            Sign::Zero => Sign::Zero,
            Sign::Positive => Sign::Negative,
        }
    }
    pub fn is_zero(&self) -> bool {
        matches!(self, Sign::Zero)
    }

    pub fn is_positive(&self) -> bool {
        matches!(self, Sign::Positive)
    }

    pub fn is_negative(&self) -> bool {
        matches!(self, Sign::Negative)
    }
}

type BigIntCount = BitField<u64, u32, { TypeBits::NEXT_BIT }, 32, false>;
type BigIntNegative = BitField<u64, bool, { BigIntCount::NEXT_BIT }, 1, false>;

#[inline(always)]
fn make_header(count: u32, negative: bool) -> ScmHeader {
    let mut hdr = ScmHeader::new();
    hdr.set_type_bits(TypeCode16::BIG.bits());
    hdr.word = BigIntCount::update(count, hdr.word);
    hdr.word = BigIntNegative::update(negative, hdr.word);
    hdr
}

#[repr(C, align(8))]
pub struct BigInt<'gc> {
    header: ScmHeader,
    words: [Digit; 0],              // Flexible array member: data follows here
    _phantom: PhantomData<&'gc ()>, // To ensure 'gc is used
}

impl<'gc> fmt::Binary for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(
                f,
                "0b{}",
                self.to_string_with_options(&NumberToStringOptions {
                    base: &Base::BIN,
                    ..Default::default()
                })
            )
        } else {
            write!(
                f,
                "{}",
                self.to_string_with_options(&NumberToStringOptions {
                    base: &Base::BIN,
                    ..Default::default()
                })
            )
        }
    }
}

impl<'gc> fmt::Display for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad_integral(
            !self.is_negative(),
            "",
            &self.to_string_with_options(&Default::default()),
        )
    }
}

impl<'gc> fmt::Debug for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (base, group_sep, group_size) = if f.alternate() {
            (&Base::HEX, Some(Cow::Borrowed("_")), 4)
        } else {
            (&Base::DEC, Some(Cow::Borrowed(",")), 3)
        };

        let options = NumberToStringOptions {
            base,
            group_sep,
            group_size,
            force_sign: f.sign_plus(),
            plus_sign: Cow::Borrowed("+"),
            minus_sign: Cow::Borrowed("-"),
        };
        let s = self.to_string_with_options(&options);
        if f.alternate() && base.radix() == 16 {
            write!(f, "BigInt(0x{})", s)
        } else {
            write!(f, "BigInt({})", s)
        }
    }
}

impl<'gc> BigInt<'gc> {
    /// Creates a new BigInt with the given number of words, initialized to zero.
    ///
    /// # Panics
    ///
    /// Panics if `num_words` exceeds `u32::MAX`.
    pub fn new<const NORMALIZE: bool>(
        mc: Context<'gc>,
        mut words: &[Digit],
        negative: bool,
    ) -> Gc<'gc, Self> {
        if NORMALIZE {
            while words.len() > 1 && words[words.len() - 1] == 0 {
                words = &words[..words.len() - 1]; // Remove trailing zeros
            }
        }

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + words.len() * std::mem::size_of::<Digit>(),
                std::mem::align_of::<Self>(),
            );

            let bigint =
                mc.allocate_with_layout::<Self>(layout, Self::VT, AllocationSemantics::Default);
            //bigint.set_user_header(TypeCode16::BIG.into()); // Set the type code

            bigint.as_mut_ptr().write(BigInt {
                header: make_header(words.len() as _, negative),
                words: [],
                _phantom: PhantomData,
            });

            // Initialize words to 0
            let bigint_data_ptr = (*bigint.as_mut_ptr()).words.as_mut_ptr();
            bigint_data_ptr.copy_from_nonoverlapping(words.as_ptr(), words.len());

            bigint.assume_init()
        }
    }

    pub fn negative(&self) -> bool {
        BigIntNegative::decode(self.header.word)
    }

    #[inline]
    pub fn zeroed<F, E>(
        mc: &Mutation<'gc>,
        mut count: usize,
        negative: bool,
        fill: F,
    ) -> Result<Gc<'gc, Self>, E>
    where
        F: FnOnce(&mut Self) -> Result<(), E>,
    {
        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + count * std::mem::size_of::<Digit>(),
                std::mem::align_of::<Self>(),
            );

            let bigint =
                mc.allocate_with_layout::<Self>(layout, Self::VT, AllocationSemantics::Default);

            bigint.as_mut_ptr().write(BigInt {
                header: make_header(count as _, negative),
                words: [],
                _phantom: PhantomData,
            });

            // Initialize words to 0
            let bigint_data_ptr = (*bigint.as_mut_ptr()).words.as_mut_ptr();
            bigint_data_ptr.write_bytes(0, count);

            fill(&mut *bigint.as_mut_ptr())?;
            while bigint_data_ptr.add(count - 1).read() == 0 && count > 1 {
                // Remove trailing zeros

                count -= 1;
            }
            (*bigint.as_mut_ptr()).header.word =
                BigIntCount::update(count as _, (*bigint.as_mut_ptr()).header.word);

            Ok(bigint.assume_init())
        }
    }

    /// Returns the number of words in this BigInt.
    pub fn num_words(&self) -> usize {
        self.count() as usize
    }

    /// Returns a slice to the words of this BigInt.
    pub fn words_slice(&self) -> &[Digit] {
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), self.num_words()) }
    }

    /// Returns a mutable slice to the words of this BigInt.
    /// Note: This requires mutable access to the Gc'd BigInt, e.g., via `Gc::write`.
    pub fn words_slice_mut(&mut self) -> &mut [Digit] {
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), self.num_words()) }
    }

    pub fn count(&self) -> usize {
        BigIntCount::decode(self.header.word) as _
    }

    pub fn flip2sc(&mut self) {
        let count = self.count();

        let mut acc: Digit2X = 1;

        for i in 0..count {
            acc = (!self[i] as Digit2X).wrapping_add(acc);
            self[i] = acc as Digit;
            acc >>= DIGIT_BIT;
        }
    }

    pub fn dup(&self, ctx: Context<'gc>, negative: bool) -> Gc<'gc, Self> {
        Self::new::<false>(ctx, &**self, negative)
    }

    pub fn to_num_bigint(&self) -> NumBigInt {
        if self.is_zero() {
            return NumBigInt::from(0);
        }

        let sign = if self.is_negative() {
            NumSign::Minus
        } else {
            NumSign::Plus
        };

        let mut bytes = Vec::new();
        for &word in self.words_slice().iter().rev() {
            bytes.extend_from_slice(&word.to_be_bytes());
        }
        // num_bigint::BigInt::from_bytes_be expects most significant byte first,
        // and our words are stored least significant first. We also need to handle potential leading zeros in the byte vector if the most significant word is small.
        // A simple reversal of words_slice and then to_be_bytes for each word handles this naturally.

        // Remove leading zero bytes from the vector, as num-bigint expects this for positive numbers.
        // For negative numbers, it's more complex due to two's complement, but from_bytes_be handles it.
        let first_non_zero = bytes.iter().position(|&b| b != 0).unwrap_or(bytes.len());
        NumBigInt::from_bytes_be(sign, &bytes[first_non_zero..])
    }

    pub fn from_num_bigint(ctx: Context<'gc>, num_bigint: &NumBigInt) -> Gc<'gc, Self> {
        if *num_bigint == NumBigInt::from(0) {
            return Self::zero(ctx);
        }

        let (sign, bytes) = num_bigint.to_bytes_be();
        let negative = match sign {
            NumSign::Minus => true,
            _ => false,
        };

        let mut words = Vec::new();
        let chunk_size = std::mem::size_of::<Digit>();

        for chunk in bytes.rchunks(chunk_size) {
            let mut temp_chunk = [0u8; 8];

            let offset = chunk_size - chunk.len();
            temp_chunk[offset..].copy_from_slice(chunk);

            let mut current_word_val = 0u64;
            for &byte in temp_chunk.iter() {
                // Iterate over the 8-byte (or padded) chunk
                current_word_val = (current_word_val << 8) | (byte as u64);
            }
            words.push(current_word_val);
        }

        Self::new::<true>(ctx, &words, negative)
    }

    const VT: &'static VTable = &VTable {
        type_name: "BigInt",
        instance_size: 0,
        alignment: align_of::<usize>(),
        compute_alignment: None,
        compute_size: Some(|object| {
            let bigint = unsafe { object.to_address().as_ref::<BigInt<'static>>() };
            // Size of the variable part (the words data)
            let raw = bigint.num_words() * size_of::<Digit>() + size_of::<Self>();
            raw_align_up(raw, align_of::<Self>())
        }),
        trace: |_, _| {},
        weak_proc: |_, _| {},
    };
}

unsafe impl<'gc> Trace for BigInt<'gc> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor<'_>) {}
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

unsafe impl<'gc> Tagged for BigInt<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::BIG];
    const ONLY_TC16: bool = true;
    const TYPE_NAME: &'static str = "bigint";
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Complex<'gc> {
    pub hdr: ScmHeader,
    pub real: Number<'gc>,
    pub imag: Number<'gc>,
}

impl<'gc> Complex<'gc> {
    pub fn new(ctx: Context<'gc>, real: Number<'gc>, imag: Number<'gc>) -> Gc<'gc, Self> {
        let complex = Complex {
            hdr: ScmHeader::with_type_bits(TypeCode16::COMPLEX.bits()),
            real,
            imag,
        };
        Gc::new(&ctx, complex)
    }
}
unsafe impl<'gc> Tagged for Complex<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::COMPLEX];
    const ONLY_TC16: bool = true;
    const TYPE_NAME: &'static str = "complex";
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Rational<'gc> {
    pub hdr: ScmHeader,
    pub numerator: Number<'gc>,
    pub denominator: Number<'gc>,
}

impl<'gc> Rational<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        numerator: Number<'gc>,
        denominator: Number<'gc>,
    ) -> Gc<'gc, Self> {
        let rational = Rational {
            hdr: ScmHeader::with_type_bits(TypeCode16::RATIONAL.bits()),
            numerator,
            denominator,
        };
        Gc::new(&ctx, rational)
    }
}

unsafe impl<'gc> Tagged for Rational<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER;
    const TC16: &'static [TypeCode16] = &[TypeCode16::RATIONAL];
    const ONLY_TC16: bool = true;
    const TYPE_NAME: &'static str = "rational";
}

impl<'gc> Index<usize> for BigInt<'gc> {
    type Output = Digit;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.count() as usize);
        unsafe { &*(self.words.as_ptr().add(index) as *const Digit) }
    }
}

impl<'gc> IndexMut<usize> for BigInt<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(index < self.count() as usize);
        unsafe { &mut *(self.words.as_mut_ptr().add(index) as *mut Digit) }
    }
}

impl<'gc> Index<Range<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        assert!(range.start < self.count() as usize);
        assert!(range.end <= self.count() as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), range.len()) }
    }
}

impl<'gc> Index<RangeTo<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeTo<usize>) -> &Self::Output {
        assert!(range.end <= self.count() as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), range.end) }
    }
}

impl<'gc> IndexMut<RangeTo<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeTo<usize>) -> &mut Self::Output {
        assert!(range.end <= self.count() as usize);
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), range.end) }
    }
}

impl<'gc> Index<RangeFrom<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeFrom<usize>) -> &Self::Output {
        assert!(range.start < self.count() as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), self.count()) }
    }
}

impl<'gc> IndexMut<RangeFrom<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeFrom<usize>) -> &mut Self::Output {
        assert!(range.start < self.count() as usize);
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), self.count())
        }
    }
}

impl<'gc> IndexMut<Range<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: Range<usize>) -> &mut Self::Output {
        assert!(range.start < self.count() as usize);
        assert!(range.end <= self.count() as usize);
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), range.len())
        }
    }
}

impl<'gc> Deref for BigInt<'gc> {
    type Target = [Digit];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), self.count() as usize) }
    }
}

impl<'gc> DerefMut for BigInt<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), self.count() as usize) }
    }
}

const fn loword(x: Digit2X) -> Digit {
    (x & DIGIT_MASK as Digit2X) as Digit
}

const fn hiword(x: Digit2X) -> Digit {
    (x >> DIGIT_BIT) as Digit
}
const fn joinwords(loword: Digit, hiword: Digit) -> Digit2X {
    ((hiword as Digit2X) << DIGIT_BIT) | (loword as Digit2X)
}

impl<'gc> BigInt<'gc> {
    pub fn from_u64(ctx: Context<'gc>, value: u64) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        BigInt::new::<false>(ctx, &[value], false)
    }

    pub fn from_i32(ctx: Context<'gc>, value: i32) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        let abs_value = value.wrapping_abs() as u64;

        BigInt::new::<false>(ctx, &[abs_value], value < 0)
    }

    pub fn from_i64(ctx: Context<'gc>, value: i64) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        let abs_value = value.wrapping_abs() as u64;

        BigInt::new::<false>(ctx, &[abs_value], value < 0)
    }

    pub fn from_u128(ctx: Context<'gc>, value: u128) -> Gc<'gc, Self> {
        if value == 0 {
            return Self::zero(ctx);
        }

        Self::new::<true>(ctx, &[loword(value), hiword(value)], false)
    }

    pub fn from_i128(ctx: Context<'gc>, value: i128) -> Gc<'gc, Self> {
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

    pub fn from_f64(ctx: Context<'gc>, value: f64) -> Gc<'gc, Self> {
        if value > -1.0 && value < 1.0 {
            Self::new::<true>(ctx, &[0], value < 0.0)
        } else if value > -(u64::MAX as f64) && value < u64::MAX as f64 {
            let absvalue = (-value) as u64;

            Self::new::<false>(ctx, &[absvalue], value < 0.0)
        } else {
            todo!()
        }
    }

    pub fn try_as_i64(&self) -> Option<i64> {
        if self.count() > 1 {
            return None;
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

        result
    }

    pub fn is_zero(&self) -> bool {
        self.count() == 1 && self[0] == 0
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

    pub fn negate(this: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return this;
        }

        this.dup(ctx, !this.negative())
    }

    pub fn abs(this: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return this;
        }

        if this.negative() {
            this.dup(ctx, false)
        } else {
            this
        }
    }

    fn compare_digits(&self, rhs: &Self) -> std::cmp::Ordering {
        if self.count() != rhs.count() {
            return self.count().cmp(&rhs.count());
        }

        for i in (0..self.count()).rev() {
            if self[i] != rhs[i] {
                return self[i].cmp(&rhs[i]);
            }
        }

        std::cmp::Ordering::Equal
    }

    pub fn plus(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if rhs.is_zero() {
            return this;
        }

        if this.is_negative() != rhs.is_negative() {
            return Self::minus(this, ctx, Self::negate(rhs, ctx));
        }

        let (b1, b2) = if this.count() < rhs.count() {
            (rhs, this)
        } else {
            (this, rhs)
        };

        Self::zeroed::<_, ()>(&ctx, b1.count() + 1, this.negative(), |result| {
            let mut sum: Digit2X = 0;
            let mut off = 0;
            for i in 0..b2.count() {
                sum += b1[i] as Digit2X;
                sum += b2[i] as Digit2X;
                result[off] = loword(sum);
                off += 1;
                sum = hiword(sum) as _;
            }

            for i in b2.count()..b1.count() {
                sum += b1[i] as Digit2X;
                result[off] = loword(sum);
                off += 1;
                sum = hiword(sum) as _;
            }

            if sum > 0 {
                result[off] = loword(sum);
                off += 1;
            }

            result.header.word = BigIntNegative::update(this.negative(), result.header.word);
            result.header.word = BigIntCount::update(off as u32, result.header.word);
            Ok(())
        })
        .unwrap()
    }

    pub fn minus(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if rhs.is_zero() {
            return this;
        }
        if this.negative() != rhs.negative() {
            return Self::plus(this, ctx, Self::negate(rhs, ctx));
        }

        let cmp = this.compare_digits(&rhs);

        if cmp == std::cmp::Ordering::Equal {
            return Self::zero(ctx);
        }

        let sign = if cmp == std::cmp::Ordering::Less {
            !this.negative()
        } else {
            this.negative()
        };

        let (b1, b2) = if cmp == std::cmp::Ordering::Less {
            (rhs, this)
        } else {
            (this, rhs)
        };

        BigInt::zeroed(&ctx, b1.count() + 1, sign, |res| {
            let mut carry: Digit2X = 0;
            let mut offset = 0;
            for i in 0..b2.count() {
                if (b1[i] as Digit2X) < b2[i] as Digit2X + carry {
                    res[offset] = (BASE + b1[i] as Digit2X - b2[i] as Digit2X - carry) as Digit;
                    carry = 1;
                } else {
                    res[offset] = (b1[i] as Digit2X - b2[i] as Digit2X - carry) as Digit;
                    carry = 0;
                }

                offset += 1;
            }

            for i in b2.count()..b1.count() {
                if (b1[i] as Digit2X) < carry {
                    res[offset] = Digit::MAX;
                    carry = 1;
                } else {
                    res[offset] = (b1[i] as Digit2X - carry) as Digit;
                    carry = 0;
                }

                offset += 1;
            }

            res.header.word = BigIntCount::update(offset as u32, res.header.word);

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    pub fn times(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if this.is_zero() || rhs.is_zero() {
            return Self::zero(ctx);
        }

        let mut lhs = this;
        let mut rhs = rhs;

        if lhs.count() < rhs.count() {
            std::mem::swap(&mut lhs, &mut rhs);
        }

        // Use Karatsuba for larger numbers, simple multiplication for smaller ones
        const KARATSUBA_THRESHOLD: usize = 32;
        if this.count().min(rhs.count()) >= KARATSUBA_THRESHOLD {
            Self::mul_karatsuba(lhs, rhs, ctx)
        } else {
            Self::mul_simple(lhs, rhs, ctx)
        }
    }

    pub fn bits(&self) -> usize {
        if self.is_zero() {
            return 0;
        }

        let zeros: usize = self.last().unwrap().leading_zeros() as usize;
        self.len() * DIGIT_BIT - zeros
    }

    pub fn mul_simple(a: Gc<'gc, Self>, b: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if a.is_zero() || b.is_zero() {
            return Self::zero(ctx);
        }

        let (b1, b2) = if a.count() < b.count() {
            (b, a)
        } else {
            (a, b)
        };

        Self::zeroed(&ctx, b1.count() + b2.count(), a.negative(), |res| {
            for i in 0..b2.count() {
                let mut sum: Digit2X = 0;

                for j in 0..b1.count() {
                    let mult = b1[j] as Digit2X * b2[i] as Digit2X;
                    sum += res[i + j] as Digit2X + mult;
                    res[i + j] = loword(sum);
                    sum = hiword(sum) as Digit2X;
                }

                res[i + b1.count()] = loword(sum);
            }

            res.header.word = BigIntNegative::update(
                if a.negative() != b.negative() {
                    !a.negative()
                } else {
                    a.negative()
                },
                res.header.word,
            );

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    /// Karatsuba multiplication algorithm for large integers
    /// Complexity: O(n^log₂3) ≈ O(n^1.585) vs O(n²) for simple multiplication
    pub fn mul_karatsuba(a: Gc<'gc, Self>, b: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if a.is_zero() || b.is_zero() {
            return Self::zero(ctx);
        }

        // Use simple multiplication for small numbers (threshold can be tuned)
        const KARATSUBA_THRESHOLD: usize = 2;
        if a.count().min(b.count()) <= KARATSUBA_THRESHOLD {
            return Self::mul_simple(a, b, ctx);
        }

        // Make both numbers the same length by padding with zeros
        let max_len = a.count().max(b.count());
        let half = (max_len + 1) / 2;

        // Split numbers: a = a1 * B^half + a0, b = b1 * B^half + b0
        // where B = 2^DIGIT_BIT
        let (a0, a1) = a.split_at(ctx, half);
        let (b0, b1) = b.split_at(ctx, half);

        // Three recursive multiplications:
        // z0 = a0 * b0
        // z2 = a1 * b1
        // z1 = (a0 + a1) * (b0 + b1) - z0 - z2
        let z0 = Self::mul_karatsuba(a0, b0, ctx);
        let z2 = Self::mul_karatsuba(a1, b1, ctx);

        // Calculate (a0 + a1) and (b0 + b1)
        let a_sum = Self::plus(a0, ctx, a1);
        let b_sum = Self::plus(b0, ctx, b1);
        let z1_temp = Self::mul_karatsuba(a_sum, b_sum, ctx);

        // z1 = z1_temp - z0 - z2
        let z1 = Self::minus(Self::minus(z1_temp, ctx, z0), ctx, z2);

        // Combine results: result = z2 * B^(2*half) + z1 * B^half + z0
        let z2_shifted = Self::shift_left_words(z2, ctx, 2 * half);
        let z1_shifted = Self::shift_left_words(z1, ctx, half);

        let result = Self::plus(Self::plus(z2_shifted, ctx, z1_shifted), ctx, z0);

        // Set the correct sign
        let result_negative = a.negative() != b.negative();
        if result.negative() != result_negative {
            Self::new::<true>(ctx, &*result, result_negative)
        } else {
            result
        }
    }

    /// Shift a BigInt left by n words (multiply by B^n where B = 2^DIGIT_BIT)
    fn shift_left_words(num: Gc<'gc, Self>, ctx: Context<'gc>, n: usize) -> Gc<'gc, Self> {
        if n == 0 || num.is_zero() {
            return num;
        }

        let mut result_words = vec![0u64; n];
        result_words.extend_from_slice(&*num);

        Self::new::<true>(ctx, &result_words, num.negative())
    }

    pub fn split_at(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        n: usize,
    ) -> (Gc<'gc, Self>, Gc<'gc, Self>) {
        let n = n.min(self.len());
        let right = &self[n..];
        let left = &self[..n];

        (
            BigInt::new::<true>(ctx, left, self.negative()),
            BigInt::new::<true>(ctx, right, self.negative()),
        )
    }

    pub fn zero(ctx: Context<'gc>) -> Gc<'gc, Self> {
        static ZERO_BIGINT: OnceLock<Global<Rootable!(Gc<'_, BigInt<'_>>)>> = OnceLock::new();

        *ZERO_BIGINT
            .get_or_init(|| {
                let zero = BigInt::new::<false>(ctx, &[0], false);

                let x = Global::new(zero);

                x
            })
            .fetch(&ctx)
    }

    pub fn one(ctx: Context<'gc>) -> Gc<'gc, Self> {
        static ONE_BIGINT: OnceLock<Global<Rootable!(Gc<'_, BigInt<'_>>)>> = OnceLock::new();

        *ONE_BIGINT
            .get_or_init(|| {
                let one = BigInt::new::<true>(ctx, &[1], false);

                Global::new(one)
            })
            .fetch(&ctx)
    }

    pub fn parse(ctx: Context<'gc>, str: &str, base: &Base) -> Result<Gc<'gc, Self>, ()> {
        let mut negative = false;
        let mut i = 0;

        while i < str.len() && str.as_bytes()[i] == b' ' {
            i += 1;
        }

        if i < str.len() && str.as_bytes()[i] == b'-' {
            negative = true;
            i += 1;
        } else if i < str.len() && str.as_bytes()[i] == b'+' {
            i += 1;
        }

        if i < str.len() && str.as_bytes()[i] == b'0' {
            while i < str.len() && str.as_bytes()[i] == b'0' {
                i += 1;
            }

            if i == str.len() {
                return Ok(BigInt::zero(ctx));
            }
        }

        let mut temp = vec![];

        while i < str.len() {
            if let Some(digit) = base.char_to_digit(str.chars().nth(i).unwrap()) {
                temp.push(digit);
                i += 1;
            } else {
                break;
            }
        }

        while i < str.len() && str.as_bytes()[i] == b' ' {
            i += 1;
        }

        if i != str.len() {
            return Err(());
        }

        if temp.is_empty() {
            return Ok(BigInt::zero(ctx));
        }

        Ok(Self::from_digits(ctx, &temp, negative, base))
    }
    /// Converts the `BigInt` to a string in the given base.
    /// `base` is used for digit mapping, `group_sep` and `group_size` for digit grouping,
    /// `force_sign` to always show a sign, `plus_sign` and `minus_sign` for sign strings.
    pub fn to_string_with_options(&self, options: &NumberToStringOptions<'gc>) -> String {
        let NumberToStringOptions {
            base,
            group_sep,
            group_size,
            force_sign,
            plus_sign,
            minus_sign,
        } = options;
        let group_size = *group_size;
        let force_sign = *force_sign;
        if self.is_zero() {
            return if force_sign {
                format!("{}0", plus_sign)
            } else {
                "0".to_string()
            };
        }

        let mut digits = Vec::new();
        let mut temp = self.words_slice().to_vec();
        let radix = base.radix() as u64;

        // Remove leading zeros
        while let Some(&0) = temp.last() {
            temp.pop();
        }

        while !temp.is_empty() {
            let mut rem = 0u64;
            for word in temp.iter_mut().rev() {
                let x = ((rem as u128) << DIGIT_BIT) | (*word as u128);
                *word = (x / radix as u128) as u64;
                rem = (x % radix as u128) as u64;
            }
            digits.push(rem as u8);
            while let Some(&0) = temp.last() {
                temp.pop();
            }
        }

        let mut res = String::new();
        let mut res_digits = 0;
        for (_, &digit) in digits.iter().rev().enumerate() {
            if res_digits > 0 && group_size > 0 && res_digits % group_size == 0 {
                if let Some(sep) = group_sep {
                    res.push_str(sep);
                }
            }
            let ch = base.digit_space.get(digit as usize).copied().unwrap_or('?');
            res.push(ch);
            res_digits += 1;
        }

        if self.is_negative() {
            res = format!("{}{}", minus_sign, res);
        } else if force_sign {
            res = format!("{}{}", plus_sign, res);
        }
        res
    }

    /// Creates a `BigInt` from a sequence of digits for a given base.
    /// The first digit in the array is the least significant one.
    /// `negative` indicates if the result should be negative.
    pub fn from_digits(
        ctx: Context<'gc>,
        digits: &[u8],
        negative: bool,
        base: &Base,
    ) -> Gc<'gc, Self> {
        if digits.is_empty() {
            return BigInt::zero(ctx);
        }

        let mut digits = digits.to_vec();
        let mut words = Vec::new();

        loop {
            let mut sum: u128 = 0;
            let mut res = Vec::new();
            let mut j = 0;
            let radix = base.radix() as u128;

            // Accumulate enough digits to fill a word
            while j < digits.len() && sum < BASE {
                sum = sum * radix + digits[j] as u128;
                j += 1;
            }
            res.push(hiword(sum) as u8);
            let mut iterate = hiword(sum) > 0;
            sum = loword(sum) as u128;

            // Continue for remaining digits
            while j < digits.len() {
                sum = sum * radix + digits[j] as u128;
                j += 1;
                res.push(hiword(sum) as u8);
                iterate = true;
                sum = loword(sum) as u128;
            }

            words.push(sum as Digit);
            digits = res;

            if !iterate {
                break;
            }
        }

        // Remove any trailing zeros
        while let Some(&0) = words.last() {
            words.pop();
        }

        BigInt::new::<true>(ctx, &words, negative)
    }

    fn mult_sub(approx: Digit, divis: &[Digit], rem: &mut Vec<Digit>, from: usize) {
        let mut sum: Digit2X = 0;
        let mut carry = 0;

        for j in 0..divis.len() {
            sum += divis[j] as Digit2X * approx as Digit2X;
            let x = loword(sum) as Digit2X + carry;
            if (rem[from + j] as Digit2X) < x {
                rem[from + j] = (BASE + rem[from + j] as Digit2X - x) as Digit;
                carry = 1;
            } else {
                rem[from + j] = (rem[from + j] as Digit2X - x) as Digit;
                carry = 0;
            }

            sum = hiword(sum) as Digit2X;
        }
    }

    fn sub_if_possible(divis: &[Digit], rem: &mut Vec<Digit>, from: usize) -> bool {
        let mut i = divis.len();
        while i > 0 && divis[i - 1] >= rem[from + i - 1] {
            if divis[i - 1] > rem[from + i - 1] {
                return false;
            }

            i -= 1;
        }

        let mut carry = 0;
        for j in 0..divis.len() {
            let x = divis[j] as Digit2X + carry;
            if (rem[from + j] as Digit2X) < x {
                rem[from + j] = (BASE + rem[from + j] as Digit2X - x) as Digit;
                carry = 1;
            } else {
                rem[from + j] = (rem[from + j] as Digit2X - x) as Digit;
                carry = 0;
            }
        }

        true
    }

    pub fn remainder_digit(&self, deno: Digit) -> Digit {
        let numerator_count = self.len();
        let mut remainder = 0 as Digit2X;

        for i in (0..numerator_count).rev() {
            remainder = ((remainder << DIGIT_BIT) | self[i] as Digit2X) % deno as Digit2X;
        }

        remainder as Digit
    }

    /// Divides `self` by `rhs` and returns (quotient, remainder) as BigInt.
    /// Returns (0, self) if rhs > self, or (1, 0) if self == rhs.
    pub fn div_rem(
        this: Gc<'gc, Self>,
        ctx: Context<'gc>,
        rhs: Gc<'gc, Self>,
    ) -> (Gc<'gc, Self>, Gc<'gc, Self>) {
        if rhs.is_zero() {
            panic!("division by zero");
        }
        if rhs.count() > this.count() {
            return (BigInt::zero(ctx), this);
        }
        let neg = this.negative() != rhs.negative();
        if rhs.count() == this.count() {
            let cmp = this.compare_digits(&rhs);
            if cmp == std::cmp::Ordering::Equal {
                return (
                    BigInt::from_i64(ctx, if neg { -1 } else { 1 }),
                    BigInt::zero(ctx),
                );
            } else if cmp == std::cmp::Ordering::Less {
                return (BigInt::zero(ctx), this);
            }
        }

        // Prepare dividend and divisor buffers (with extra zero for overflow)
        let mut rem = this.words_slice().to_vec();
        rem.push(0);
        let mut divis = rhs.words_slice().to_vec();
        divis.push(0);

        let mut sizediff = rem.len() as isize - divis.len() as isize;
        let div = rhs[rhs.len() - 1] as Digit2X + 1;
        let mut res = vec![0 as Digit; sizediff as usize + 1];
        let mut divident = rem.len() as isize - 2;

        loop {
            let mut x = joinwords(rem[divident as usize], rem[divident as usize + 1]);
            let mut approx = x / div;

            while approx > 0 {
                res[sizediff as usize] += approx as Digit;
                Self::mult_sub(approx as _, &divis, &mut rem, sizediff as usize);
                x = joinwords(rem[divident as usize], rem[divident as usize + 1]);
                approx = x / div;
            }

            if Self::sub_if_possible(&divis, &mut rem, sizediff as usize) {
                res[sizediff as usize] += 1;
            }

            divident -= 1;
            sizediff -= 1;
            if sizediff < 0 {
                break;
            }
        }

        let quotient = BigInt::new::<true>(ctx, &res, neg);
        let remainder = BigInt::new::<true>(ctx, &rem[..rhs.count()], this.negative());

        (quotient, remainder)
    }

    pub fn div(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        let (quotient, _) = Self::div_rem(this, ctx, rhs);
        quotient
    }

    pub fn rem(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        let (_, remainder) = Self::div_rem(this, ctx, rhs);
        remainder
    }

    pub fn div_digit(
        this: Gc<'gc, Self>,
        ctx: Context<'gc>,
        divisor: Digit,
    ) -> (Gc<'gc, Self>, u128) {
        if divisor == 0 {
            panic!("BigInt::div_digit: division by zero");
        }

        if this.is_zero() {
            return (BigInt::zero(ctx), 0);
        }

        if divisor == 1 {
            return (this.dup(ctx, this.negative()), 0);
        }

        let count = this.count();

        let mut quotient_digits = vec![0 as Digit; count];

        let mut remainder: Digit2X = 0; // Digit2X is u128

        for i in (0..count).rev() {
            let current_word = this[i];
            let dividend_part: Digit2X = (remainder << 64) | (current_word as Digit2X);

            quotient_digits[i] = (dividend_part / (divisor as Digit2X)) as Digit;
            remainder = dividend_part % (divisor as Digit2X);
        }

        let result_sign = this.negative();

        (
            BigInt::new::<true>(ctx, &quotient_digits, result_sign),
            remainder,
        )
    }

    /// Performs a bitwise AND operation between the BigInt and a single Digit.
    /// The operation is performed on the magnitude of the BigInt.
    /// The sign of the result is preserved, unless the magnitude becomes zero.
    pub fn and_digit(this: Gc<'gc, Self>, ctx: Context<'gc>, digit: Digit) -> Gc<'gc, Self> {
        if this.is_zero() {
            return BigInt::zero(ctx);
        }

        let count = this.count();
        let mut result_digits = vec![0 as Digit; count];

        if count > 0 {
            result_digits[0] = this[0] & digit;
        }

        BigInt::new::<true>(ctx, &result_digits, this.negative())
    }

    pub fn or_digit(this: Gc<'gc, Self>, ctx: Context<'gc>, digit: Digit) -> Gc<'gc, Self> {
        let mut result_digits = this.words_slice().to_vec(); // Clone existing words

        if result_digits.is_empty() {
            // self is zero
            if digit == 0 {
                return BigInt::zero(ctx);
            } else {
                // Result is just the digit, with positive sign
                return BigInt::new::<true>(ctx, &[digit], false);
            }
        }

        result_digits[0] |= digit;

        BigInt::new::<true>(ctx, &result_digits, this.negative())
    }

    pub fn xor_digit(this: Gc<'gc, Self>, ctx: Context<'gc>, digit: Digit) -> Gc<'gc, Self> {
        let mut result_digits = this.words_slice().to_vec(); // Clone existing words

        if result_digits.is_empty() {
            // self is zero
            if digit == 0 {
                return BigInt::zero(ctx);
            } else {
                // Result is just the digit, with positive sign
                return BigInt::new::<true>(ctx, &[digit], false);
            }
        }

        // Perform XOR on the first word (least significant)
        result_digits[0] ^= digit;

        BigInt::new::<true>(ctx, &result_digits, this.negative())
    }

    pub fn from_2sc(ctx: Context<'gc>, words: &[Digit]) -> Gc<'gc, Self> {
        if Self::is_most_significant_bit_set(words) {
            Self::zeroed(&ctx, words.len(), true, |res| {
                let mut carry = true;

                res.copy_from_slice(words);

                for i in 0..words.len() {
                    if carry {
                        (res[i], carry) = (!words[i]).overflowing_add(1);
                    } else {
                        res[i] = !res[i];
                    }
                }

                while let Some(&0) = res.last() {
                    res.header.word = BigIntCount::update(res.len() as u32 - 1, res.header.word);
                }

                res.header.word = BigIntNegative::update(true, res.header.word);
                Result::<(), ()>::Ok(())
            })
            .unwrap()
        } else {
            Self::new::<true>(ctx, words, false)
        }
    }

    pub fn random<R: Rng>(ctx: Context<'gc>, rng: &mut R, bitwidth: usize) -> Gc<'gc, Self> {
        let mut words = vec![0; (bitwidth + DIGIT_BIT - 1) / DIGIT_BIT];
        rng.fill(&mut words[..]);
        Self::new::<true>(ctx, &words, false)
    }

    pub fn is_most_significant_bit_set(words: &[Digit]) -> bool {
        words
            .last()
            .map_or(false, |&word| word & (1 << (DIGIT_BIT - 1)) != 0)
    }

    pub fn twos_complement_size(left: &[Digit], right: &[Digit]) -> usize {
        (left.len()
            + if Self::is_most_significant_bit_set(left) {
                1
            } else {
                0
            })
        .max(
            right.len()
                + if Self::is_most_significant_bit_set(right) {
                    1
                } else {
                    0
                },
        )
    }

    pub fn and(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::twos_complement_size(&this, &rhs);

        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.count() { this[i] } else { 0 };

            if this.is_negative() {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.count() { rhs[i] } else { 0 };

            if rhs.is_negative() {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword & rightword);
        }

        Self::from_2sc(ctx, &res)
    }

    pub fn or(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::twos_complement_size(&this, &rhs);

        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.count() { this[i] } else { 0 };

            if this.is_negative() {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.count() { rhs[i] } else { 0 };

            if rhs.is_negative() {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword | rightword);
        }

        Self::from_2sc(ctx, &res)
    }

    pub fn xor(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        let (mut leftcarry, mut rightcarry) = (true, true);

        let size = Self::twos_complement_size(&this, &rhs);

        let mut res = Vec::with_capacity(size);

        for i in 0..size {
            let mut leftword = if i < this.count() { this[i] } else { 0 };

            if this.is_negative() {
                if leftcarry {
                    (leftword, leftcarry) = (!leftword).overflowing_add(1);
                } else {
                    leftword = !leftword;
                }
            }

            let mut rightword = if i < rhs.count() { rhs[i] } else { 0 };

            if rhs.is_negative() {
                if rightcarry {
                    (rightword, rightcarry) = (!rightword).overflowing_add(1);
                } else {
                    rightword = !rightword;
                }
            }

            res.push(leftword ^ rightword);
        }

        Self::from_2sc(ctx, &res)
    }

    pub fn not(this: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        let size = if Self::is_most_significant_bit_set(&this) {
            this.count() + 1
        } else {
            this.count()
        };

        let mut res = Vec::with_capacity(size);

        let mut carry = true;

        for i in 0..size {
            let mut word = if i < this.count() { this[i] } else { 0 };

            if carry {
                (word, carry) = (!word).overflowing_add(1);
            } else {
                word = !word;
            }

            res.push(word);
        }

        Self::from_2sc(ctx, &res)
    }

    pub fn pow(this: Gc<'gc, Self>, ctx: Context<'gc>, exp: u64) -> Gc<'gc, Self> {
        let (mut expo, mut radix) = (exp, this);
        let mut res = Self::one(ctx);

        while expo != 0 {
            if expo & 1 != 0 {
                res = Self::times(res, ctx, radix);
            }
            expo /= 2;
            radix = Self::times(radix, ctx, radix);
        }

        res
    }

    pub fn gcd(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return Self::abs(rhs, ctx);
        }
        if rhs.is_zero() {
            return Self::abs(this, ctx);
        }

        let mut a = Self::abs(this, ctx);
        let mut b = Self::abs(rhs, ctx);

        while !b.is_zero() {
            let (q, r) = Self::div_rem(a, ctx, b);
            a = b;
            b = r;
        }

        a
    }

    pub fn lcm(this: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if this.is_zero() || rhs.is_zero() {
            return BigInt::zero(ctx);
        }

        let gcd = Self::gcd(this, ctx, rhs);
        let abs_this = Self::abs(this, ctx);
        let abs_rhs = Self::abs(rhs, ctx);
        let (quotient, _) = Self::div_rem(abs_this, ctx, gcd);
        Self::times(quotient, ctx, abs_rhs)
    }

    pub fn sqrt(this: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if this.is_zero() {
            return this;
        }
        let mut y = Self::div_digit(this, ctx, 2).0;
        let mut x = Self::div(this, ctx, y);
        while y > x {
            let plus = Self::plus(x, ctx, y);

            y = Self::div_digit(plus, ctx, 2).0;

            x = Self::div(this, ctx, y);
        }

        y
    }

    pub fn shift_left(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: usize) -> Gc<'gc, Self> {
        let swords = shift / DIGIT_BIT;
        let sbits = shift % DIGIT_BIT;

        /*let mut res = vec![];
        res.reserve(this.len() + swords);

        for _ in 0..swords {
            res.push(0);
        }

        let mut carry: Digit = 0;

        for &word in this.iter() {
            res.push((word << sbits) | carry);
            carry = word >> (DIGIT_BIT - sbits);
        }

        if carry > 0 {
            res.push(carry);
        }

        Self::new::<true>(ctx, &res, this.negative())*/

        Self::zeroed(&ctx, this.len() + swords + 1, this.negative(), |res| {
            for i in 0..swords {
                res[i] = 0;
            }

            let mut carry: Digit = 0;
            let mut offset = swords;

            for &word in this.iter() {
                res[offset] = (word << sbits) | carry;
                carry = word >> (DIGIT_BIT - sbits);
                offset += 1;
            }

            if carry > 0 {
                res[offset] = carry;
                offset += 1;
            }

            res.header.word = BigIntCount::update(offset as u32, res.header.word);

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    pub fn shift_right(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: usize) -> Gc<'gc, Self> {
        let swords = shift / DIGIT_BIT;
        let sbits = shift % DIGIT_BIT;

        let mut res = vec![];
        res.reserve(this.len().saturating_sub(swords));

        let mut carry = 0;
        let mut i = this.len() - 1;

        while i >= swords {
            let word = this[i];
            res.push((word >> sbits) | carry);
            carry = (word << (DIGIT_BIT - sbits)) & Digit::MAX;
            i -= 1;
        }

        let x = Self::new::<true>(ctx, &res, this.negative());

        if this.negative() && carry > 0 {
            Self::minus(x, ctx, Self::one(ctx))
        } else {
            x
        }
    }

    pub fn shift(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: isize) -> Gc<'gc, Self> {
        if shift > 0 {
            Self::shift_left(this, ctx, shift as usize)
        } else if shift < 0 {
            Self::shift_right(this, ctx, -shift as usize)
        } else {
            this
        }
    }

    pub fn bitsize(this: Gc<'gc, Self>) -> usize {
        if this.is_zero() {
            return 0;
        }
        let mut size = this.count() * DIGIT_BIT;
        size -= this.leading_zeros();
        size
    }

    pub fn count_ones(&self) -> usize {
        if self.is_zero() {
            return 0;
        }
        let mut count = 0;
        for word in self.words_slice() {
            count += word.count_ones() as usize;
        }
        count
    }

    pub fn count_zeros(&self) -> usize {
        if self.is_zero() {
            return 0;
        }
        let mut count = 0;
        for word in self.words_slice() {
            count += word.count_zeros() as usize;
        }
        count
    }

    pub fn first_bit_set(&self) -> i32 {
        if self.is_zero() {
            return -1;
        }
        let mut pos = 0;
        for word in self.words_slice() {
            if *word != 0 {
                return pos + word.trailing_zeros() as i32;
            }
            pos += DIGIT_BIT as i32;
        }
        -1
    }

    /// Returns true if the bit at position `n` is set in the two's complement representation.
    pub fn is_bit_set(&self, n: usize) -> bool {
        if self.is_zero() {
            return false;
        }
        let nword = n / DIGIT_BIT;
        let nbit = n % DIGIT_BIT;
        if nword >= self.count() {
            // For out-of-bounds bits, return the sign bit (infinite sign extension)
            return self.negative();
        }
        if !self.negative() {
            return (self[nword] & (1 << nbit)) != 0;
        }
        // For negative numbers, compute two's complement up to nword
        let mut carry = true;
        let mut word = 0;
        for i in 0..=nword {
            word = self[i];
            if carry {
                let (w, c) = (!word).overflowing_add(1);
                word = w;
                carry = c;
            } else {
                word = !word;
            }
        }
        (word & (1 << nbit)) != 0
    }

    /// Sets the bit at position `n` to `value` and returns a new BigInt.
    /// `value == true` sets the bit, `false` clears it.
    pub fn set_bit(this: Gc<'gc, Self>, ctx: Context<'gc>, n: usize, value: bool) -> Gc<'gc, Self> {
        let nword = n / DIGIT_BIT;
        let nbit = n % DIGIT_BIT;
        let mut words = this.words_slice().to_vec();

        // Extend words if needed
        while words.len() <= nword {
            words.push(0);
        }

        if !this.negative() {
            if value {
                words[nword] |= 1 << nbit;
            } else {
                words[nword] &= !(1 << nbit);
            }
            return BigInt::new::<true>(ctx, &words, false);
        }

        // Negative case: two's complement logic
        if Self::is_most_significant_bit_set(&words) {
            words.push(0);
        }
        let mut carry = true;
        for w in &mut words {
            if carry {
                let (v, c) = (!*w).overflowing_add(1);
                *w = v;
                carry = c;
            } else {
                *w = !*w;
            }
        }
        if value {
            words[nword] |= 1 << nbit;
        } else {
            words[nword] &= !(1 << nbit);
        }
        BigInt::from_2sc(ctx, &words)
    }

    pub fn leading_zeros(&self) -> usize {
        let mut count = 0;
        for i in (0..self.count()).rev() {
            if self[i] == 0 {
                count += DIGIT_BIT;
            } else {
                let mut word = self[i];
                while word & 1 == 0 {
                    word >>= 1;
                    count += 1;
                }
                break;
            }
        }
        count
    }

    pub fn trailing_zeros(&self) -> usize {
        let mut count = 0;
        for i in 0..self.count() {
            if self[i] == 0 {
                count += DIGIT_BIT;
            } else {
                let mut word = self[i];
                while word & 1 == 0 {
                    word >>= 1;
                    count += 1;
                }
                break;
            }
        }
        count
    }

    pub fn leading_ones(&self) -> usize {
        let mut count = 0;
        for i in (0..self.count()).rev() {
            if self[i] == Digit::MAX {
                count += DIGIT_BIT;
            } else {
                let mut word = self[i];
                while word & 1 == 1 {
                    word >>= 1;
                    count += 1;
                }
                break;
            }
        }
        count
    }

    pub fn trailing_ones(&self) -> usize {
        let mut count = 0;
        for i in 0..self.count() {
            if self[i] == Digit::MAX {
                count += DIGIT_BIT;
            } else {
                let mut word = self[i];
                while word & 1 == 1 {
                    word >>= 1;
                    count += 1;
                }
                break;
            }
        }
        count
    }

    pub fn rotate_left(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: usize) -> Gc<'gc, Self> {
        let count = this.count();
        let mut res = vec![0; count];
        let shift = shift % (count * DIGIT_BIT);
        let (swords, sbits) = (shift / DIGIT_BIT, shift % DIGIT_BIT);

        for i in 0..count {
            let next = (i + swords) % count;
            res[next] |= this[i] << sbits;
            if i + 1 < count {
                res[next] |= this[i + 1] >> (DIGIT_BIT - sbits);
            }
        }

        BigInt::new::<true>(ctx, &res, this.negative())
    }

    pub fn rotate_right(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: usize) -> Gc<'gc, Self> {
        let count = this.count();
        let mut res = vec![0; count];
        let shift = shift % (count * DIGIT_BIT);
        let (swords, sbits) = (shift / DIGIT_BIT, shift % DIGIT_BIT);

        for i in 0..count {
            let prev = (i + count - swords) % count;
            res[prev] |= this[i] >> sbits;
            if i > 0 {
                res[prev] |= this[i - 1] << (DIGIT_BIT - sbits);
            }
        }

        BigInt::new::<true>(ctx, &res, this.negative())
    }

    pub fn rotate(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: isize) -> Gc<'gc, Self> {
        if shift > 0 {
            Self::rotate_left(this, ctx, shift as usize)
        } else if shift < 0 {
            Self::rotate_right(this, ctx, -shift as usize)
        } else {
            this
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Base {
    pub digit_space: &'static [char],
    pub digit_map: &'static [(char, u8)],
}

impl Base {
    pub const BIN: Base = Base {
        digit_space: &['0', '1'],
        digit_map: &[('0', 0), ('1', 1)],
    };

    pub const OCT: Base = Base {
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

    pub const DEC: Base = Base {
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

    pub const HEX: Base = Base {
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

    pub fn radix(&self) -> usize {
        self.digit_space.len()
    }

    pub fn char_to_digit(&self, c: char) -> Option<u8> {
        self.digit_map
            .iter()
            .find_map(|(ch, v)| if *ch == c { Some(*v) } else { None })
    }

    pub fn of(radix: usize) -> Option<&'static Base> {
        match radix {
            2 => Some(&Base::BIN),
            8 => Some(&Base::OCT),
            10 => Some(&Base::DEC),
            16 => Some(&Base::HEX),
            _ => None,
        }
    }
}

pub struct NumberToStringOptions<'gc> {
    pub base: &'static Base,
    pub group_sep: Option<Cow<'gc, str>>,
    pub group_size: usize,
    pub force_sign: bool,
    pub plus_sign: Cow<'gc, str>,
    pub minus_sign: Cow<'gc, str>,
}

impl<'gc> Default for NumberToStringOptions<'gc> {
    fn default() -> Self {
        Self {
            base: &Base::DEC,
            group_sep: None,
            group_size: 3,
            force_sign: false,
            plus_sign: Cow::Borrowed("+"),
            minus_sign: Cow::Borrowed("-"),
        }
    }
}

impl<'gc> Hash for BigInt<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for &word in self.words_slice() {
            state.write_u64(word);
        }
    }
}

impl<'gc> PartialEq for BigInt<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl<'gc> Eq for BigInt<'gc> {}

impl<'gc> PartialOrd for BigInt<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'gc> Ord for BigInt<'gc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.negative() != other.negative() {
            if self.negative() {
                return std::cmp::Ordering::Less;
            } else {
                return std::cmp::Ordering::Greater;
            }
        }

        if self.negative() {
            other.compare_digits(self)
        } else {
            self.compare_digits(other)
        }
    }
}

impl<'gc> PartialEq<i64> for BigInt<'gc> {
    fn eq(&self, other: &i64) -> bool {
        self.try_as_i64() == Some(*other)
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::Scheme;

    use super::*;
    #[test]
    fn test_bigint_cmp() {
        let scm = Scheme::new();

        scm.enter(|ctx| {
            let b1 = BigInt::from_u64(ctx, 12345678901234567890);
            let b2 = BigInt::from_u64(ctx, 12345678901234567890);

            assert_eq!(b1.cmp(&b2), std::cmp::Ordering::Equal);

            let b3 = BigInt::from_u64(ctx, 12345678901234567891);

            assert_eq!(b1.cmp(&b3), std::cmp::Ordering::Less);
            assert_eq!(b3.cmp(&b1), std::cmp::Ordering::Greater);

            let zero = BigInt::zero(ctx);

            assert_eq!(zero.cmp(&b1), std::cmp::Ordering::Less);
            assert_eq!(b1.cmp(&zero), std::cmp::Ordering::Greater);
            assert_eq!(zero.cmp(&zero), std::cmp::Ordering::Equal);
        });
    }

    #[test]
    fn test_bigint_ops() {
        let scm = Scheme::new();

        scm.enter(|ctx| {
            let a = BigInt::parse(ctx, "12345678901234567890", &Base::DEC).unwrap();
            let b = BigInt::parse(ctx, "98765432109876543210", &Base::DEC).unwrap();

            let sum = BigInt::plus(a, ctx, b);
            assert_eq!(sum.to_string(), "111111111011111111100");

            let a =
                BigInt::parse(ctx, "123414124512512356132616532623515231151", &Base::DEC).unwrap();
            let b = BigInt::parse(ctx, "12412412512356324632651651361561", &Base::DEC).unwrap();

            let (q, r) = BigInt::div_rem(a, ctx, b);
            let mul = BigInt::times(a, ctx, b);

            println!("div: ({q}, {r})");
            println!("mul: {}", mul.to_string());

            assert_eq!(q.to_string(), "9942799");
            assert_eq!(r.to_string(), "1797068403931412326117437881912");
            assert_eq!(
                mul.to_string(),
                "1531867023300609762396035507803021632000090312363887487217570791186711"
            );
        });
    }
}

/// A number type which represents one of the types from numerical tower of Scheme:
/// - `integer`: represented by `Fixnum` or `BigInt`, `fixnum` is 32-bit integer
/// used for optimization of value sizes in VM and is a VM detail that might change in the future.
/// while bigint provides virtually unlimited precision for integer values.
/// - `flonum`: double precision IEEE 754 floating point number.
/// - `rational`: a pair of integers representing a rational number.
/// - `complex`: a pair of numbers representing a complex number.
#[derive(Clone, Copy)]
pub enum Number<'gc> {
    Fixnum(i32),
    Flonum(f64),
    BigInt(Gc<'gc, BigInt<'gc>>),
    Rational(Gc<'gc, Rational<'gc>>),
    Complex(Gc<'gc, Complex<'gc>>),
}

unsafe impl<'gc> Trace for Number<'gc> {
    unsafe fn trace(&mut self, tracer: &mut Visitor) {
        match self {
            Number::Fixnum(_) => {}
            Number::Flonum(_) => {}
            Number::BigInt(b) => tracer.trace(b),
            Number::Rational(r) => tracer.trace(r),
            Number::Complex(c) => tracer.trace(c),
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> IntoValue<'gc> for Number<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Number::Fixnum(i) => Value::new(i),
            Number::Flonum(f) => Value::new(f),
            Number::BigInt(b) => Value::new(b),
            Number::Rational(r) => Value::new(r),
            Number::Complex(c) => Value::new(c),
        }
    }
}

impl<'gc> FromValue<'gc> for Number<'gc> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            Ok(Number::Fixnum(value.as_int32()))
        } else if value.is::<BigInt>() {
            Ok(Number::BigInt(value.downcast::<BigInt>()))
        } else if value.is::<Rational>() {
            Ok(Number::Rational(value.downcast::<Rational>()))
        } else if value.is::<Complex>() {
            Ok(Number::Complex(value.downcast::<Complex>()))
        } else if value.is_flonum() {
            Ok(Number::Flonum(value.as_flonum()))
        } else {
            Err(ConversionError::TypeMismatch {
                pos: 0,
                expected: "number",
                found: value,
            })
        }
    }
}

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
            Some(i as u32)
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
            Some(i as u64)
        } else if let Number::BigInt(b) = self {
            b.try_as_u64()
        } else {
            None
        }
    }

    pub fn exact_integer_to_usize(self) -> Option<usize> {
        if let Number::Fixnum(i) = self {
            Some(i as usize)
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
            Some(i as u128)
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
                if b.negative() { -(v as i32) } else { v as i32 }
            }
            _ => panic!("Cannot coerce non-integer to i32"),
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
            _ => panic!("Cannot coerce non-integer to u32"),
        }
    }

    pub fn coerce_exact_integer_to_i64(self) -> i64 {
        match self {
            Number::Fixnum(i) => i as i64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u64;
                if b.negative() { -(v as i64) } else { v as i64 }
            }
            _ => panic!("Cannot coerce non-integer to i64"),
        }
    }

    pub fn coerce_exact_integer_to_u64(self) -> u64 {
        match self {
            Number::Fixnum(i) => i as u64,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                b[0] as u64
            }
            _ => panic!("Cannot coerce non-integer to u64"),
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
            _ => panic!("Cannot coerce non-integer to usize"),
        }
    }

    pub fn coerce_exact_integer_to_isize(self) -> isize {
        match self {
            Number::Fixnum(i) => i as isize,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u64;
                if b.negative() {
                    -(v as isize)
                } else {
                    v as isize
                }
            }
            _ => panic!("Cannot coerce non-integer to isize"),
        }
    }

    pub fn coerce_exact_integer_to_f64(self) -> f64 {
        match self {
            Number::Fixnum(i) => i as f64,
            Number::BigInt(b) => b.as_f64(),
            _ => panic!("Cannot coerce non-integer to f64"),
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
            _ => panic!("Cannot coerce non-integer to u8"),
        }
    }

    pub fn coerce_exact_integer_to_i8(self) -> i8 {
        match self {
            Number::Fixnum(i) => i as i8,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u64;
                if b.negative() { -(v as i8) } else { v as i8 }
            }
            _ => panic!("Cannot coerce non-integer to i8"),
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
            _ => panic!("Cannot coerce non-integer to u16"),
        }
    }

    pub fn coerce_exact_integer_to_i16(self) -> i16 {
        match self {
            Number::Fixnum(i) => i as i16,
            Number::BigInt(b) => {
                if b.count() == 0 {
                    return 0;
                }
                let v = b[0] as u64;
                if b.negative() { -(v as i16) } else { v as i16 }
            }
            _ => panic!("Cannot coerce non-integer to i16"),
        }
    }

    pub fn real_to_f64(&self, ctx: Context<'gc>) -> f64 {
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

    pub fn inexact_negate(&self, ctx: Context<'gc>) -> Self {
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

    pub fn normalize_complex(ctx: Context<'gc>, real: Self, imag: Self) -> Self {
        assert!(!imag.is_complex());
        assert!(!real.is_complex());

        if let Number::Fixnum(0) = imag {
            return real;
        }

        if let Number::BigInt(b) = imag {
            if b.is_zero() {
                return real;
            }
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

    pub fn reduce_fix_fix(ctx: Context<'gc>, nume: i32, deno: i32) -> Self {
        let mut nume = nume as IDigit;
        let mut deno = deno as IDigit;

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

    pub fn reduce_fix_big(ctx: Context<'gc>, nume: i32, deno: Gc<'gc, BigInt<'gc>>) -> Self {
        let mut nume = nume as IDigit;
        let deno = deno.clone();
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
            if deno.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
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

        let mut n1 = deno.remainder_digit(nume as _) as IDigit;
        let mut n2 = nume;

        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }

        let gcd = n1;
        nume = nume / gcd;
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

    pub fn reduce_big_fix(ctx: Context<'gc>, nume: Gc<'gc, BigInt<'gc>>, deno: i32) -> Self {
        let mut nume = nume;
        let mut deno = deno as IDigit;

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

        let mut n1 = nume.remainder_digit(deno as Digit) as IDigit;
        let mut n2 = deno;
        while n2 != 0 {
            let t = n2;
            n2 = n1 % n2;
            n1 = t;
        }
        let gcd = n1;
        deno = deno / gcd;

        let (mut quo, _) = BigInt::div_digit(nume, ctx, gcd as Digit);
        if ans_sign < 0 {
            quo = BigInt::negate(quo, ctx);
        }
        let ans_nume = Number::BigInt(quo);
        let ans_deno = deno.into_number(ctx);

        if let Number::Fixnum(1) = ans_deno {
            return ans_nume;
        }
        Self::Rational(Rational::new(ctx, ans_nume, ans_deno))
    }

    pub fn reduce(ctx: Context<'gc>, numerator: Self, denominator: Self) -> Self {
        assert!(matches!(numerator, Number::Fixnum(_) | Number::BigInt(_)));
        assert!(matches!(denominator, Number::Fixnum(_) | Number::BigInt(_)));
        if let Number::Fixnum(nume) = numerator {
            if let Number::Fixnum(deno) = denominator {
                return Self::reduce_fix_fix(ctx, nume, deno);
            }

            return Self::reduce_fix_big(ctx, nume, denominator.as_bigint().unwrap());
        }

        if let Number::Fixnum(deno) = denominator {
            return Self::reduce_big_fix(ctx, numerator.as_bigint().unwrap(), deno);
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
            if denominator.is_negative() {
                return Number::Rational(Rational::new(
                    ctx,
                    (-1i32).into_number(ctx),
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
        let nume = n1;
        let deno = n2;
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

        if deno == gcd {
            let mut res = BigInt::div(nume, ctx, gcd);
            if ans_sign < 0 {
                res = BigInt::negate(res, ctx);
            }

            return res.into_number(ctx);
        }

        let mut res = BigInt::div(nume, ctx, gcd);
        if ans_sign < 0 {
            res = BigInt::negate(res, ctx);
        }
        Self::Rational(Rational::new(
            ctx,
            res.into_number(ctx),
            BigInt::div(n2, ctx, gcd).into_number(ctx),
        ))
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Number::Fixnum(i) => *i < 0,
            Number::Flonum(f) => *f < 0.0,
            Number::BigInt(b) => b.is_negative(),
            Number::Rational(r) => r.numerator.is_negative(),
            Number::Complex(c) => c.real.is_negative() || c.imag.is_negative(),
        }
    }

    pub fn negate(&self, ctx: Context<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::Fixnum(-*i),
            Number::Flonum(f) => Number::Flonum(-f),
            Number::BigInt(b) => Self::BigInt(BigInt::negate(*b, ctx)),
            Number::Rational(r) => {
                let nume = r.numerator.negate(ctx);
                let deno = r.denominator;

                Self::Rational(Rational::new(ctx, nume, deno))
            }
            Number::Complex(c) => {
                let imag = c.imag.negate(ctx);
                let real = c.real.negate(ctx);
                Self::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn to_inexact(&self, ctx: Context<'gc>) -> Self {
        match self {
            Number::Fixnum(i) => Number::Flonum(*i as f64),
            Number::Flonum(f) => Number::Flonum(*f),
            Number::BigInt(b) => Number::Flonum(b.as_f64()),
            Number::Rational(r) => Number::Flonum(r.to_f64(ctx)),
            Number::Complex(c) => {
                let real = c.real.to_inexact(ctx);
                let imag = c.imag.to_inexact(ctx);
                Number::Complex(Complex::new(ctx, real, imag))
            }
        }
    }

    pub fn as_bigint(&self) -> Option<Gc<'gc, BigInt<'gc>>> {
        if let Number::BigInt(b) = self {
            Some(*b)
        } else {
            None
        }
    }

    pub fn add(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        if let Number::Fixnum(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                match lhs.checked_add(rhs) {
                    Some(res) => return Number::Fixnum(res),
                    None => {
                        return BigInt::from_i64(ctx, lhs as i64 + rhs as i64).into_number(ctx);
                    }
                }
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs as f64 + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return BigInt::plus(BigInt::from_i64(ctx, lhs as i64), ctx, rhs).into_number(ctx);
            } else if let Number::Rational(rn) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        rn.numerator,
                        Self::mul(ctx, rn.denominator, Self::Fixnum(lhs)),
                    ),
                    rn.denominator,
                );
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Fixnum(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Flonum(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return Number::Flonum(lhs as f64 + rhs as f64);
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return Number::Flonum(lhs + rhs.as_f64());
            } else if let Number::Rational(rn) = rhs {
                return Number::Flonum(lhs + rn.to_f64(ctx));
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Flonum(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag.to_inexact(ctx)));
            }
        }

        if let Number::BigInt(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return BigInt::plus(lhs, ctx, BigInt::from_i64(ctx, rhs as i64)).into_number(ctx);
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs.as_f64() + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return BigInt::plus(lhs, ctx, rhs).into_number(ctx);
            } else if let Number::Rational(rn) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        rn.numerator,
                        Self::mul(ctx, rn.denominator, Self::BigInt(lhs)),
                    ),
                    rn.denominator,
                );
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::BigInt(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Rational(lhs) = lhs {
            if let Number::Fixnum(rhs) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                    ),
                    lhs.denominator,
                );
            } else if let Number::Flonum(rhs) = rhs {
                return Number::Flonum(lhs.to_f64(ctx) + rhs);
            } else if let Number::BigInt(rhs) = rhs {
                return Self::reduce(
                    ctx,
                    Self::add(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                    ),
                    lhs.denominator,
                );
            } else if let Number::Rational(rn) = rhs {
                return Rational::add(ctx, lhs, rn);
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, cn.real, Self::Rational(lhs));
                return Self::Complex(Complex::new(ctx, real, cn.imag));
            }
        }

        if let Number::Complex(lhs) = lhs {
            let real = lhs.real;
            let imag = lhs.imag;

            if let Number::Fixnum(rhs) = rhs {
                let real = Self::add(ctx, real, Self::Fixnum(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Flonum(rhs) = rhs {
                let real = Self::add(ctx, real, Self::Flonum(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::BigInt(rhs) = rhs {
                let real = Self::add(ctx, real, Self::BigInt(rhs));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Rational(rn) = rhs {
                let real = Self::add(ctx, real, Self::Rational(rn));
                return Self::Complex(Complex::new(ctx, real, imag));
            } else if let Number::Complex(cn) = rhs {
                let real = Self::add(ctx, real, cn.real);
                let imag = Self::add(ctx, imag, cn.imag);
                return Self::Complex(Complex::new(ctx, real, imag));
            }
        }

        unreachable!()
    }

    pub fn sub(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if let Some(res) = lhs.checked_sub(rhs) {
                        return Number::Fixnum(res);
                    } else {
                        return BigInt::from_i64(ctx, lhs as i64 - rhs as i64).into_number(ctx);
                    }
                }

                Number::Flonum(rhs) => return Number::Flonum(lhs as f64 - rhs),
                Number::BigInt(rhs) => {
                    return BigInt::minus(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                        .into_number(ctx);
                }
                Number::Rational(rn) => {
                    return Self::reduce(
                        ctx,
                        Self::sub(
                            ctx,
                            Self::mul(ctx, rn.denominator, Self::Fixnum(lhs)),
                            rn.numerator,
                        ),
                        rn.denominator,
                    );
                }

                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::Fixnum(lhs), cn.real);
                    return Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)));
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => return Number::Flonum(lhs - rhs as f64),
                Number::Flonum(rhs) => return Number::Flonum(lhs - rhs),
                Number::BigInt(rhs) => return Number::Flonum(lhs - rhs.as_f64()),
                Number::Rational(rn) => return Number::Flonum(lhs - rn.to_f64(ctx)),
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::Flonum(lhs), cn.real);
                    return Self::Complex(Complex::new(ctx, real, cn.imag.inexact_negate(ctx)));
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return BigInt::minus(lhs, ctx, BigInt::from_i64(ctx, rhs as i64))
                        .into_number(ctx);
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.as_f64() - rhs),
                Number::BigInt(rhs) => {
                    return BigInt::minus(lhs, ctx, rhs).into_number(ctx);
                }
                Number::Rational(rn) => {
                    return Self::reduce(
                        ctx,
                        Self::sub(
                            ctx,
                            rn.numerator,
                            Self::mul(ctx, rn.denominator, Self::BigInt(lhs)),
                        ),
                        rn.denominator,
                    );
                }
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, Self::BigInt(lhs), cn.real);
                    return Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)));
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return Self::reduce(
                        ctx,
                        Self::sub(
                            ctx,
                            lhs.numerator,
                            Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                        ),
                        lhs.denominator,
                    );
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.to_f64(ctx) - rhs),
                Number::BigInt(rhs) => {
                    return Self::reduce(
                        ctx,
                        Self::sub(
                            ctx,
                            lhs.numerator,
                            Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                        ),
                        lhs.denominator,
                    );
                }
                Number::Rational(rn) => {
                    return Rational::sub(ctx, lhs, rn);
                }
                Number::Complex(cn) => {
                    let real = Self::sub(ctx, cn.real, Self::Rational(lhs));
                    return Self::Complex(Complex::new(ctx, real, cn.imag.negate(ctx)));
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => {
                        let real = Self::sub(ctx, real, Self::Fixnum(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Flonum(rhs) => {
                        let real = Self::sub(ctx, real, Self::Flonum(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::BigInt(rhs) => {
                        let real = Self::sub(ctx, real, Self::BigInt(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Rational(rn) => {
                        let real = Self::sub(ctx, real, Self::Rational(rn));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Complex(cn) => {
                        let real = Self::sub(ctx, cn.real, lhs.real);
                        let imag = Self::sub(ctx, cn.imag, lhs.imag);
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                }
            }
        }
    }

    pub fn mul(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if let Some(res) = lhs.checked_mul(rhs) {
                        return Number::Fixnum(res);
                    } else {
                        return BigInt::from_i64(ctx, lhs as i64 * rhs as i64).into_number(ctx);
                    }
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs as f64 * rhs),
                Number::BigInt(rhs) => {
                    probe::probe!(mul_bigint, start);
                    let res =
                        BigInt::times(BigInt::from_i64(ctx, lhs as i64), ctx, rhs).into_number(ctx);
                    probe::probe!(mul_bigint, end);

                    res
                }
                Number::Rational(rn) => {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::Fixnum(lhs), rn.denominator);
                    }

                    if matches!(rn.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::Fixnum(lhs), rn.denominator).negate(ctx);
                    }

                    return Self::reduce(
                        ctx,
                        Self::mul(ctx, rn.numerator, Self::Fixnum(lhs)),
                        rn.denominator,
                    );
                }
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Fixnum(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Fixnum(lhs));
                    return Self::Complex(Complex::new(ctx, real, imag));
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => return Number::Flonum(lhs * rhs as f64),
                Number::Flonum(rhs) => return Number::Flonum(lhs * rhs),
                Number::BigInt(rhs) => return Number::Flonum(lhs * rhs.as_f64()),
                Number::Rational(rn) => return Number::Flonum(lhs * rn.to_f64(ctx)),
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Flonum(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Flonum(lhs));
                    return Self::Complex(Complex::new(ctx, real, imag));
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return BigInt::times(lhs, ctx, BigInt::from_i64(ctx, rhs as i64))
                        .into_number(ctx);
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.as_f64() * rhs),
                Number::BigInt(rhs) => {
                    return BigInt::times(lhs, ctx, rhs).into_number(ctx);
                }
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::BigInt(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::BigInt(lhs));
                    return Self::Complex(Complex::new(ctx, real, imag));
                }

                Number::Rational(rn) => {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::BigInt(lhs), rn.denominator);
                    }

                    if matches!(rn.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::BigInt(lhs), rn.denominator).negate(ctx);
                    }

                    return Self::reduce(
                        ctx,
                        Self::mul(ctx, rn.numerator, Self::BigInt(lhs)),
                        rn.denominator,
                    );
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if matches!(lhs.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::Fixnum(rhs), lhs.denominator);
                    }

                    if matches!(lhs.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::Fixnum(rhs), lhs.denominator).negate(ctx);
                    }

                    return Self::reduce(
                        ctx,
                        Self::mul(ctx, lhs.numerator, Self::Fixnum(rhs)),
                        lhs.denominator,
                    );
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.to_f64(ctx) * rhs),
                Number::BigInt(rhs) => {
                    if matches!(lhs.numerator, Number::Fixnum(1)) {
                        return Self::reduce(ctx, Self::BigInt(rhs), lhs.denominator);
                    }

                    if matches!(lhs.numerator, Number::Fixnum(-1)) {
                        return Self::reduce(ctx, Self::BigInt(rhs), lhs.denominator).negate(ctx);
                    }

                    return Self::reduce(
                        ctx,
                        Self::mul(ctx, lhs.numerator, Self::BigInt(rhs)),
                        lhs.denominator,
                    );
                }
                Number::Rational(rn) => {
                    return Rational::mul(ctx, lhs, rn);
                }
                Number::Complex(cn) => {
                    let real = Self::mul(ctx, cn.real, Self::Rational(lhs));
                    let imag = Self::mul(ctx, cn.imag, Self::Rational(lhs));
                    return Self::Complex(Complex::new(ctx, real, imag));
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => {
                        let real = Self::mul(ctx, real, Self::Fixnum(rhs));
                        let imag = Self::mul(ctx, imag, Self::Fixnum(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Flonum(rhs) => {
                        let real = Self::mul(ctx, real, Self::Flonum(rhs));
                        let imag = Self::mul(ctx, imag, Self::Flonum(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::BigInt(rhs) => {
                        let real = Self::mul(ctx, real, Self::BigInt(rhs));
                        let imag = Self::mul(ctx, imag, Self::BigInt(rhs));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Rational(rn) => {
                        let real = Self::mul(ctx, real, Self::Rational(rn));
                        let imag = Self::mul(ctx, imag, Self::Rational(rn));
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                    Number::Complex(cn) => {
                        let real = Self::sub(
                            ctx,
                            Self::mul(ctx, cn.real, lhs.real),
                            Self::mul(ctx, cn.imag, lhs.imag),
                        );
                        let imag = Self::add(
                            ctx,
                            Self::mul(ctx, cn.real, lhs.imag),
                            Self::mul(ctx, cn.imag, lhs.real),
                        );
                        return Self::Complex(Complex::new(ctx, real, imag));
                    }
                }
            }
        }
    }

    pub fn integer_div(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(x) => match rhs {
                Number::Fixnum(y) => {
                    let div = if x == 0 {
                        0
                    } else if x > 0 {
                        x / y
                    } else if y > 0 {
                        (x - y + 1) / y
                    } else {
                        (x + y + 1) / y
                    };

                    return Number::Fixnum(div);
                }

                _ => (),
            },

            _ => (),
        }

        if matches!(lhs, Number::Flonum(_)) || matches!(rhs, Number::Flonum(_)) {
            let x = lhs.real_to_f64(ctx);
            let y = rhs.real_to_f64(ctx);
            return Self::Flonum(if y > 0.0 {
                (x / y).floor()
            } else {
                -((x / -y).floor())
            });
        }

        if rhs.is_negative() {
            return Self::div(ctx, lhs, rhs).floor(ctx);
        }

        Self::div(ctx, lhs, rhs.negate(ctx)).floor(ctx).negate(ctx)
    }

    pub fn integer_div0(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        let div = Self::integer_div(ctx, lhs, rhs);
        let mod_ = Self::sub(ctx, lhs, Self::mul(ctx, div, rhs));
        if Self::compare(
            ctx,
            mod_,
            Self::magnitude(ctx, Self::div(ctx, rhs, Self::Fixnum(2))),
        ) == Some(std::cmp::Ordering::Less)
        {
            return div;
        }
        if rhs.is_positive() {
            return Self::add(ctx, div, Number::Fixnum(1));
        }

        Self::sub(ctx, div, Number::Fixnum(1))
    }

    pub fn div(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Number::Fixnum(lhs) => {
                if lhs == 0 {
                    if rhs.is_exact() {
                        return Number::Fixnum(0);
                    }

                    return Self::div(ctx, Self::Flonum(0.0), rhs);
                }

                match rhs {
                    Number::Fixnum(rhs) => return Self::reduce_fix_fix(ctx, lhs, rhs),
                    Number::Flonum(rhs) => return Number::Flonum(lhs as f64 / rhs),
                    Number::BigInt(rhs) => {
                        return Self::reduce_fix_big(ctx, lhs, rhs);
                    }

                    Number::Rational(rhs) => {
                        return Self::reduce(
                            ctx,
                            Self::mul(ctx, rhs.denominator, Self::Fixnum(lhs)),
                            rhs.numerator,
                        );
                    }

                    Number::Complex(rhs) => {
                        let real = rhs.real;
                        let imag = rhs.imag;
                        let r2 =
                            Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, real, Self::Fixnum(lhs)), r2),
                            Self::div(ctx, Self::mul(ctx, imag, Self::Fixnum(lhs)), r2).negate(ctx),
                        ));
                    }
                }
            }

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => return Number::Flonum(lhs / rhs as f64),
                Number::Flonum(rhs) => return Number::Flonum(lhs / rhs),
                Number::BigInt(rhs) => return Number::Flonum(lhs / rhs.as_f64()),
                Number::Rational(rhs) => return Number::Flonum(lhs / rhs.to_f64(ctx)),
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    return Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, Self::Flonum(lhs)), r2),
                        Self::div(ctx, Self::mul(ctx, imag, Self::Flonum(lhs)), r2).negate(ctx),
                    ));
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return Self::reduce_big_fix(ctx, lhs, rhs);
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.as_f64() / rhs),
                Number::BigInt(rhs) => {
                    return Self::reduce(ctx, Self::BigInt(lhs), Self::BigInt(rhs));
                }
                Number::Rational(rhs) => {
                    return Self::reduce(
                        ctx,
                        Self::mul(ctx, Self::BigInt(lhs), rhs.denominator),
                        rhs.numerator,
                    );
                }
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    return Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, Self::BigInt(lhs)), r2),
                        Self::div(ctx, Self::mul(ctx, imag, Self::BigInt(lhs)), r2).negate(ctx),
                    ));
                }
            },

            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return Self::reduce(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::Fixnum(rhs)),
                    );
                }
                Number::Flonum(rhs) => return Number::Flonum(lhs.to_f64(ctx) / rhs),
                Number::BigInt(rhs) => {
                    return Self::reduce(
                        ctx,
                        lhs.numerator,
                        Self::mul(ctx, lhs.denominator, Self::BigInt(rhs)),
                    );
                }
                Number::Rational(rhs) => {
                    return Rational::div(ctx, lhs, rhs);
                }
                Number::Complex(rhs) => {
                    let real = rhs.real;
                    let imag = rhs.imag;

                    let r2 = Self::add(ctx, Self::mul(ctx, real, real), Self::mul(ctx, imag, imag));

                    return Self::Complex(Complex::new(
                        ctx,
                        Self::div(ctx, Self::mul(ctx, real, lhs.numerator), r2),
                        Self::mul(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, imag, lhs.numerator), r2).negate(ctx),
                            lhs.denominator,
                        ),
                    ));
                }
            },

            Number::Complex(lhs) => {
                let real = lhs.real;
                let imag = lhs.imag;

                match rhs {
                    Number::Fixnum(rhs) => {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, real, Self::Fixnum(rhs)),
                            Self::div(ctx, imag, Self::Fixnum(rhs)),
                        ));
                    }

                    Number::Flonum(rhs) => {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, real, Self::Flonum(rhs)),
                            Self::div(ctx, imag, Self::Flonum(rhs)),
                        ));
                    }

                    Number::BigInt(rhs) => {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, real, Self::BigInt(rhs)),
                            Self::div(ctx, imag, Self::BigInt(rhs)),
                        ));
                    }

                    Number::Rational(rhs) => {
                        let r2 = Self::add(
                            ctx,
                            Self::mul(ctx, rhs.numerator, rhs.numerator),
                            Self::mul(ctx, rhs.denominator, rhs.denominator),
                        );

                        return Self::Complex(Complex::new(
                            ctx,
                            Self::div(ctx, Self::mul(ctx, real, rhs.numerator), r2),
                            Self::mul(
                                ctx,
                                Self::div(ctx, Self::mul(ctx, imag, rhs.numerator), r2).negate(ctx),
                                rhs.denominator,
                            ),
                        ));
                    }

                    Self::Complex(rhs) => {
                        let real2 = rhs.real;
                        let imag2 = rhs.imag;
                        let r2 = Self::add(
                            ctx,
                            Self::mul(ctx, real2, real2),
                            Self::mul(ctx, imag2, imag2),
                        );
                        let real3 = Self::div(
                            ctx,
                            Self::add(
                                ctx,
                                Self::mul(ctx, real, real2),
                                Self::mul(ctx, imag, imag2),
                            ),
                            r2,
                        );
                        let imag3 = Self::div(
                            ctx,
                            Self::sub(
                                ctx,
                                Self::mul(ctx, imag, real2),
                                Self::mul(ctx, real, imag2),
                            ),
                            r2,
                        );
                        return Self::Complex(Complex::new(ctx, real3, imag3));
                    }
                }
            }
        }
    }

    pub fn quotient(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Fixnum(lhs / rhs);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs as f64 / rhs);
                            }

                            Number::BigInt(rhs) => {
                                return BigInt::div(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                    .into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() {
                        if lhs == 0.0 {
                            return Number::Flonum(0.0);
                        }
                    } else {
                        unreachable!()
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum(lhs / rhs as f64);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs / rhs);
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum(lhs / rhs.as_f64());
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            return Self::BigInt(BigInt::div(
                                lhs,
                                ctx,
                                BigInt::from_i64(ctx, rhs as i64),
                            ));
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum(lhs.as_f64() / rhs);
                        }

                        Number::BigInt(rhs) => {
                            return BigInt::div(lhs, ctx, rhs).into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    pub fn remainder(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Fixnum(lhs % rhs);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs as f64 % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return BigInt::rem(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                    .into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() {
                        if lhs == 0.0 {
                            return Number::Flonum(0.0);
                        }
                    } else {
                        unreachable!()
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum(lhs % rhs as f64);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum(lhs % rhs.as_f64());
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            return Self::BigInt(BigInt::rem(
                                lhs,
                                ctx,
                                BigInt::from_i64(ctx, rhs as i64),
                            ));
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum(lhs.as_f64() % rhs);
                        }

                        Number::BigInt(rhs) => {
                            return BigInt::rem(lhs, ctx, rhs).into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    pub fn modulo(ctx: Context<'gc>, mut lhs: Self, mut rhs: Self) -> Self {
        'start_again: loop {
            match lhs {
                Number::Fixnum(lhs) => {
                    if lhs == 0 {
                        return Number::Fixnum(0);
                    }

                    'fixnum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    return Number::Fixnum(0);
                                }

                                return Self::Fixnum(lhs % rhs);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs as f64 % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return BigInt::rem(BigInt::from_i64(ctx, lhs as i64), ctx, rhs)
                                    .into_number(ctx);
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'fixnum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::Flonum(lhs) => {
                    if lhs == lhs.round() {
                        if lhs == 0.0 {
                            return Number::Flonum(0.0);
                        }
                    } else {
                        unreachable!()
                    }

                    'flonum_again: loop {
                        match rhs {
                            Number::Fixnum(rhs) => {
                                if rhs == 0 {
                                    panic!("division by zero");
                                }

                                return Self::Flonum(lhs % rhs as f64);
                            }

                            Number::Flonum(rhs) => {
                                return Self::Flonum(lhs % rhs);
                            }

                            Number::BigInt(rhs) => {
                                return Self::Flonum(lhs % rhs.as_f64());
                            }

                            Number::Rational(_) => unreachable!(),

                            Number::Complex(cn) => {
                                let real = cn.real;
                                let imag = cn.imag;

                                if imag.is_zero() {
                                    rhs = real;
                                    continue 'flonum_again;
                                }

                                unreachable!()
                            }
                        }
                    }
                }

                Number::BigInt(lhs) => 'bigint_again: loop {
                    match rhs {
                        Number::Fixnum(rhs) => {
                            if rhs == 0 {
                                panic!("division by zero");
                            }

                            return Self::BigInt(BigInt::rem(
                                lhs,
                                ctx,
                                BigInt::from_i64(ctx, rhs as i64),
                            ));
                        }

                        Number::Flonum(rhs) => {
                            return Self::Flonum(lhs.as_f64() % rhs);
                        }

                        Number::BigInt(rhs) => {
                            return BigInt::rem(lhs, ctx, rhs).into_number(ctx);
                        }

                        Number::Rational(_) => unreachable!(),

                        Number::Complex(cn) => {
                            let real = cn.real;
                            let imag = cn.imag;

                            if imag.is_zero() {
                                rhs = real;
                                continue 'bigint_again;
                            }

                            unreachable!()
                        }
                    }
                },

                Number::Rational(_) => unreachable!(),
                Number::Complex(cn) => {
                    let real = cn.real;
                    let imag = cn.imag;

                    if imag.is_zero() {
                        lhs = real;
                        continue 'start_again;
                    }

                    unreachable!()
                }
            }
        }
    }

    pub fn expt(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        if let Number::Flonum(0.0) = lhs {
            if let Number::Complex(cn) = rhs {
                if cn.real.is_positive() {
                    return Number::Flonum(0.0);
                }
            } else {
                if rhs.is_positive() {
                    return Number::Flonum(0.0);
                }
            }
        }

        if rhs.is_exact() {
            match rhs {
                Number::Fixnum(rhs) => {
                    if rhs == 0 {
                        return Number::Fixnum(1);
                    } else if let Number::Flonum(lhs) = lhs {
                        return Number::Flonum(lhs.powi(rhs));
                    } else {
                        return Self::expt_impl(ctx, lhs, rhs as _);
                    }
                }

                Number::BigInt(rhs) => {
                    if lhs.is_real_valued() {
                        let n = rhs.as_f64();
                        return Self::Flonum(lhs.real_to_f64(ctx).powf(n));
                    }

                    return Self::exp(
                        ctx,
                        Self::mul(ctx, rhs.into_number(ctx), Self::log(ctx, lhs)),
                    );
                }

                Number::Rational(rhs) => {
                    let n = rhs.to_f64(ctx);
                    if lhs.is_real_valued() {
                        return Self::Flonum(lhs.real_to_f64(ctx).powf(n));
                    }

                    return Self::exp(
                        ctx,
                        Self::mul(ctx, Self::Rational(rhs), Self::log(ctx, lhs)),
                    );
                }

                Number::Complex(cn) => {
                    return Self::exp(ctx, Self::mul(ctx, rhs, Self::log(ctx, lhs)));
                }

                _ => unreachable!(),
            }
        } else {
            match rhs {
                Self::Flonum(rhs) => {
                    if lhs.is_real_valued() {
                        return Self::Flonum(lhs.real_to_f64(ctx).powf(rhs));
                    }

                    return Self::exp(
                        ctx,
                        Self::mul(ctx, rhs.into_number(ctx), Self::log(ctx, lhs)),
                    );
                }

                _ => return Self::exp(ctx, Self::mul(ctx, rhs, Self::log(ctx, lhs))),
            }
        }
    }

    pub fn exp(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(1);
                }

                return Number::Flonum((n as f64).exp());
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let a = real.exp();
                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(a * imag.cos()),
                    Self::Flonum(a * imag.sin()),
                ));
            }

            _ => {
                if n.is_real_valued() {
                    return Self::Flonum(n.real_to_f64(ctx).exp());
                }

                unreachable!()
            }
        }
    }

    pub fn log(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let value = n as IDigit;

                if value > 0 {
                    if value == 1 {
                        return Number::Fixnum(0);
                    }

                    return Number::Flonum(libm::log(value as f64));
                }

                let real = value as f64;

                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * libm::log(real * real)),
                    Self::Flonum(libm::atan2(0.0, real)),
                ));
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(libm::log(real * real + imag * imag) * 0.5),
                    Self::Flonum(libm::atan2(imag, real)),
                ));
            }

            _ => {
                let real = n.real_to_f64(ctx);
                if real.is_infinite() && n.is_exact_positive_integer() {
                    let (sqrt, _) = Self::exact_integer_sqrt(ctx, n);
                    return Self::add(ctx, Self::log(ctx, sqrt), Self::log(ctx, sqrt));
                }

                if real > 0.0 {
                    return Self::Flonum(libm::log(real));
                }

                let imag = libm::atan2(0.0, real);
                if imag == 0.0 {
                    return Self::Flonum(0.5 * libm::log(real * real));
                } else {
                    return Self::Complex(Complex::new(
                        ctx,
                        Self::Flonum(0.5 * libm::log(real * real)),
                        Self::Flonum(imag),
                    ));
                }
            }
        }
    }

    pub fn sin(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(0);
                }

                return Number::Flonum(libm::sin(n as f64));
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;

                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * real.sin() * (e + f)),
                    Self::Flonum(0.5 * real.cos() * (e - f)),
                ));
            }

            _ => return Self::Flonum(n.real_to_f64(ctx).sin()),
        }
    }

    pub fn cos(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(1);
                }

                return Number::Flonum(libm::cos(n as f64));
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;

                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.5 * real.cos() * (f + e)),
                    Self::Flonum(0.5 * real.sin() * (f - e)),
                ));
            }

            _ => return Self::Flonum(n.real_to_f64(ctx).cos()),
        }
    }

    pub fn tan(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n == 0 {
                    return Number::Fixnum(0);
                }

                return Number::Flonum(libm::tan(n as f64));
            }

            Number::Complex(cn) => {
                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);

                let e = libm::exp(imag);
                let f = 1.0 / e;
                let d = (2.0 * real).cos() + 0.5 * (e + f);

                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum((2.0 * real).sin() / d),
                    Self::Flonum(0.5 * (e - f) / d),
                ));
            }

            _ => return Self::Flonum(n.real_to_f64(ctx).tan()),
        }
    }

    pub fn sqrt(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                let mut value = n as IDigit;
                if value == 0 {
                    return Number::Fixnum(0);
                }

                if value > 0 {
                    let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                    if iroot.wrapping_mul(iroot) == value {
                        return Number::Fixnum(iroot as i32);
                    } else {
                        return Self::Flonum(libm::sqrt(value as f64));
                    }
                } else {
                    value = -value;
                    let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                    if iroot.wrapping_mul(iroot) == value {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::Fixnum(0),
                            Number::Fixnum(iroot as i32),
                        ));
                    } else {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::Flonum(0.0),
                            Self::Flonum(libm::sqrt(value as f64)),
                        ));
                    }
                }
            }

            Number::BigInt(n) => {
                if n.is_positive() {
                    let s = BigInt::sqrt(n, ctx);
                    if BigInt::times(s, ctx, s) == n {
                        return s.into_number(ctx);
                    } else {
                        return Self::Flonum(n.as_f64().sqrt());
                    }
                } else {
                    let n = BigInt::negate(n, ctx);
                    let s = BigInt::sqrt(n, ctx);
                    if BigInt::times(s, ctx, s) == n {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::Fixnum(0),
                            s.into_number(ctx),
                        ));
                    } else {
                        return Self::Complex(Complex::new(
                            ctx,
                            Self::Flonum(0.0),
                            Self::Flonum(n.as_f64().sqrt()),
                        ));
                    }
                }
            }

            Number::Rational(rn) => {
                let numerator;
                let denominator;
                let complex;
                if rn.numerator.is_negative() {
                    numerator = rn.numerator.negate(ctx);
                    complex = true;
                } else {
                    complex = false;
                    numerator = Self::sqrt(ctx, rn.numerator);
                }

                denominator = Self::sqrt(ctx, rn.denominator);

                if matches!(numerator, Number::Fixnum(_) | Number::BigInt(_)) {
                    if matches!(denominator, Number::Fixnum(_) | Number::BigInt(_)) {
                        if complex {
                            return Self::Complex(Complex::new(
                                ctx,
                                Number::Fixnum(0),
                                Self::reduce(ctx, numerator, denominator),
                            ));
                        } else {
                            return Self::reduce(ctx, numerator, denominator);
                        }
                    }
                }

                if complex {
                    return Self::Complex(Complex::new(
                        ctx,
                        Number::Fixnum(0),
                        Self::div(ctx, numerator, denominator),
                    ));
                } else {
                    return Self::div(ctx, numerator, denominator);
                }
            }

            Number::Flonum(fl) => {
                if fl < 0.0 {
                    return Self::Complex(Complex::new(
                        ctx,
                        Self::Flonum(0.0),
                        Self::Flonum(libm::sqrt(-fl)),
                    ));
                } else {
                    return Self::Flonum(libm::sqrt(fl));
                }
            }

            Number::Complex(cn) => {
                if n.is_exact() {
                    let m = Self::magnitude(ctx, n);
                    let x = Self::div(ctx, Self::add(ctx, cn.real, m), Self::Fixnum(2));
                    let y = Self::div(ctx, cn.imag, Self::Fixnum(2));
                    let s = Self::sqrt(
                        ctx,
                        Self::div(
                            ctx,
                            m,
                            Self::add(ctx, Self::mul(ctx, x, x), Self::mul(ctx, y, y)),
                        ),
                    );

                    return Self::normalize_complex(
                        ctx,
                        Self::mul(ctx, x, s),
                        Self::mul(ctx, y, s),
                    );
                }

                let real = cn.real.real_to_f64(ctx);
                let imag = cn.imag.real_to_f64(ctx);
                let m = libm::sqrt(real * real + imag * imag);
                let x = (real + m) / 2.0;
                let y = imag / 2.0;
                let s = libm::sqrt(m / (x * x + y * y));

                return Self::Complex(Complex::new(ctx, Self::Flonum(x * s), Self::Flonum(y * s)));
            }
        }
    }

    pub fn magnitude(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Self::Complex(cn) => {
                // (magnitude 3/22+2/11i) => 5/22
                if n.is_exact() {
                    if cn.real.is_zero() {
                        return Self::magnitude(ctx, cn.imag);
                    }

                    if cn.imag.is_zero() {
                        return Self::magnitude(ctx, cn.real);
                    }

                    Self::sqrt(
                        ctx,
                        Self::add(
                            ctx,
                            Self::mul(ctx, cn.real, cn.imag),
                            Self::mul(ctx, cn.imag, cn.imag),
                        ),
                    )
                } else {
                    let real = cn.real.real_to_f64(ctx);
                    let imag = cn.imag.real_to_f64(ctx);
                    if real.is_infinite() || imag.is_infinite() {
                        return Self::Flonum(f64::INFINITY);
                    }

                    let m = libm::sqrt(real * real + imag * imag);
                    if m < f64::EPSILON || m.is_infinite() {
                        return Self::Flonum(imag / imag.atan2(real).sin());
                    }

                    return Self::Flonum(m);
                }
            }

            _ => {
                if n.is_negative() {
                    return n.negate(ctx);
                }

                n
            }
        }
    }

    pub fn exact_integer_sqrt(ctx: Context<'gc>, n: Self) -> (Self, Self) {
        match n {
            Number::Fixnum(n) => {
                let value = n as IDigit;

                if value == 0 {
                    return (Number::Fixnum(0), Number::Fixnum(0));
                }

                let iroot = libm::floor(libm::sqrt(value as f64)) as IDigit;
                return (
                    Number::Fixnum(iroot as i32),
                    Number::Fixnum((value - iroot * iroot) as i32),
                );
            }

            Number::BigInt(x) => {
                let s = BigInt::sqrt(x, ctx).into_number(ctx);
                let r = Self::sub(ctx, n, Self::mul(ctx, s, s));
                return (s, r);
            }

            _ => unreachable!(),
        }
    }

    fn expt_impl(ctx: Context<'gc>, mut lhs: Self, rhs: i32) -> Self {
        let mut n = rhs as IDigit;

        if n == 0 {
            return Number::Fixnum(1);
        }

        if n == 1 {
            return lhs;
        }

        if n < 0 {
            return Self::inverse(ctx, Self::expt_impl(ctx, lhs, (-n) as i32));
        }

        if !matches!(lhs, Self::Complex(_)) && lhs.is_negative() {
            let ans = Self::expt_impl(ctx, lhs.negate(ctx), rhs);
            if n & 1 != 0 {
                return ans.negate(ctx);
            } else {
                return ans;
            }
        }

        if matches!(lhs, Number::Fixnum(0)) {
            return lhs;
        }

        if matches!(lhs, Number::Fixnum(1)) {
            return lhs;
        }

        if matches!(lhs, Number::Fixnum(2)) {
            if n + 1 <= 31 {
                return Number::Fixnum(1 << n);
            }

            let count = ((n as usize + 1) + (DIGIT_BIT - 1)) / DIGIT_BIT;
            let mut ans = vec![0u64; count];
            ans[count - 1] = (1 << (n as usize & (DIGIT_BIT - 1))) as Digit;
            return BigInt::new::<true>(ctx, &ans, false).into_number(ctx);
        }

        if let Number::Rational(lhs) = lhs {
            return Self::reduce(
                ctx,
                Self::expt_impl(ctx, lhs.numerator, rhs),
                Self::expt_impl(ctx, lhs.denominator, rhs),
            );
        }

        let mut ans = Number::Fixnum(1);

        loop {
            if n & 1 != 0 {
                if matches!(ans, Number::Fixnum(1)) {
                    ans = lhs;
                } else {
                    ans = Self::mul(ctx, ans, lhs);
                }

                if n == 1 {
                    return ans;
                }
            }

            lhs = Self::mul(ctx, lhs, lhs);
            n >>= 1;
        }
    }

    pub fn inverse(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Number::Fixnum(n) => {
                if n > 0 {
                    if n == 1 {
                        return Number::Fixnum(1);
                    } else {
                        return Number::Rational(Rational::new(
                            ctx,
                            Number::Fixnum(1),
                            Number::Fixnum(n),
                        ));
                    }
                }

                if n == -1 {
                    return Number::Fixnum(-1);
                } else {
                    return Number::Rational(Rational::new(
                        ctx,
                        Number::Fixnum(-1),
                        Number::Fixnum(n).negate(ctx),
                    ));
                }
            }

            Number::Flonum(fl) => return Number::Flonum(1.0 / fl),
            Number::BigInt(bi) => {
                if bi.is_positive() {
                    return Number::Rational(Rational::new(
                        ctx,
                        Number::BigInt(BigInt::from_i64(ctx, 1)),
                        Number::BigInt(bi),
                    ));
                } else {
                    return Number::Rational(Rational::new(
                        ctx,
                        Number::BigInt(BigInt::from_i64(ctx, -1)),
                        Number::BigInt(BigInt::negate(bi, ctx)),
                    ));
                }
            }

            Number::Rational(rn) => {
                if !rn.numerator.is_negative() {
                    if matches!(rn.numerator, Number::Fixnum(1)) {
                        return rn.denominator.normalize_integer();
                    }

                    return Number::Rational(Rational::new(ctx, rn.denominator, rn.numerator));
                }

                if matches!(rn.numerator, Number::Fixnum(-1)) {
                    return rn.denominator.negate(ctx);
                } else {
                    return Number::Rational(Rational::new(
                        ctx,
                        rn.denominator.negate(ctx),
                        rn.numerator.negate(ctx),
                    ));
                }
            }
            Number::Complex(_) => Self::div(ctx, Number::Fixnum(1), n),
        }
    }

    pub fn asin(ctx: Context<'gc>, n: Self) -> Self {
        let cn = if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            if x >= -1.0 && x <= 1.0 {
                return Self::Flonum(libm::asin(x));
            }

            if x < 0.0 {
                return Self::asin(ctx, Self::Flonum(-x)).negate(ctx);
            }

            Complex::new(ctx, Self::Flonum(0.0), Self::Flonum(x))
        } else {
            match n {
                Self::Complex(x) => {
                    if x.imag.is_positive() {
                        return Self::asin(ctx, n.negate(ctx)).negate(ctx);
                    }

                    Complex::new(ctx, x.imag.negate(ctx), x.real)
                }

                _ => unreachable!(),
            }
        };

        let ans = Self::log(
            ctx,
            Self::add(
                ctx,
                Self::sqrt(ctx, Self::sub(ctx, Number::Fixnum(1), Self::mul(ctx, n, n))),
                cn.into_number(ctx),
            ),
        );

        match ans {
            Number::Complex(cn) => {
                return Number::Complex(Complex::new(
                    ctx,
                    Self::Flonum(cn.imag.real_to_f64(ctx)),
                    Self::Flonum(cn.real.real_to_f64(ctx)),
                ));
            }
            _ => {
                return Self::Complex(Complex::new(
                    ctx,
                    Self::Flonum(0.0),
                    Self::Flonum(-ans.real_to_f64(ctx)),
                ));
            }
        }
    }

    pub fn acos(ctx: Context<'gc>, n: Self) -> Self {
        if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            if x >= -1.0 && x <= 1.0 {
                return Self::Flonum(libm::acos(x));
            }
        }

        Self::sub(
            ctx,
            Self::Flonum(std::f64::consts::PI / 2.0),
            Self::asin(ctx, n),
        )
    }

    pub fn atan(ctx: Context<'gc>, n: Self) -> Self {
        if n.is_real_valued() {
            let x = n.real_to_f64(ctx);
            if x >= -1.0 && x <= 1.0 {
                return Self::Flonum(libm::atan(x));
            }
        }

        let Number::Complex(cn) = n else {
            unreachable!()
        };

        let ans = Self::div(
            ctx,
            Self::add(ctx, n, Number::Fixnum(1)),
            Self::sub(ctx, n, Number::Fixnum(1)),
        );
        if let Number::Complex(cn) = ans {
            return Number::Complex(Complex::new(
                ctx,
                Number::mul(ctx, Number::Flonum(0.5), cn.imag),
                Number::mul(ctx, Number::Flonum(-0.5), cn.real),
            ));
        }

        return Number::Complex(Complex::new(
            ctx,
            Number::Flonum(0.0),
            Number::mul(ctx, Number::Flonum(-0.5), cn.real),
        ));
    }

    pub fn atan2(ctx: Context<'gc>, lhs: Number<'gc>, rhs: Number<'gc>) -> Number<'gc> {
        if matches!(lhs, Number::Fixnum(0)) {
            return Number::Fixnum(0);
        }

        let lhs = lhs.real_to_f64(ctx);
        let rhs = rhs.real_to_f64(ctx);

        Number::Flonum(libm::atan2(lhs, rhs))
    }

    pub fn floor(self, ctx: Context<'gc>) -> Self {
        let n = self;
        match n {
            Number::Fixnum(_) | Number::BigInt(_) => n,
            Number::Flonum(fl) => {
                return Self::Flonum(libm::floor(fl));
            }

            Number::Rational(rn) => {
                if rn.numerator.is_negative() {
                    return Self::sub(
                        ctx,
                        Self::quotient(ctx, rn.numerator, rn.denominator),
                        Number::Fixnum(1),
                    );
                }

                return Self::quotient(ctx, rn.numerator, rn.denominator);
            }

            _ => unreachable!(),
        }
    }

    pub fn ceiling(self, ctx: Context<'gc>) -> Self {
        let n = self;
        match n {
            Number::Fixnum(_) | Number::BigInt(_) => n,
            Number::Flonum(fl) => {
                return Self::Flonum(libm::ceil(fl));
            }

            Number::Rational(rn) => {
                if rn.numerator.is_positive() {
                    return Self::add(
                        ctx,
                        Self::quotient(ctx, rn.numerator, rn.denominator),
                        Number::Fixnum(1),
                    );
                }

                return Self::quotient(ctx, rn.numerator, rn.denominator);
            }

            _ => unreachable!(),
        }
    }

    pub fn truncate(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self,
            Self::Flonum(fl) => {
                return Self::Flonum(fl.trunc());
            }

            Self::Rational(rn) => {
                return Self::quotient(ctx, rn.numerator, rn.denominator);
            }

            _ => unreachable!(),
        }
    }

    pub fn round(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => self,
            Self::Flonum(fl) => {
                let ans = (fl + 0.5).floor();
                if ans != fl + 0.5 {
                    return Self::Flonum(fl);
                }

                if ans * 0.5 == (ans * 0.5).floor() {
                    return Self::Flonum(ans - 1.0);
                } else {
                    return Self::Flonum(ans);
                }
            }

            Self::Rational(rn) => {
                let negative = rn.numerator.is_negative();
                let half = Self::Rational(Rational::new(
                    ctx,
                    Number::Fixnum(if negative { -1 } else { 1 }),
                    Number::Fixnum(2),
                ));
                let n_and_half = Self::add(ctx, self, half);
                if let Number::Rational(rn) = n_and_half {
                    return Self::quotient(ctx, rn.numerator, rn.denominator);
                } else {
                    if n_and_half.is_even() {
                        return n_and_half;
                    }

                    return Self::add(
                        ctx,
                        n_and_half,
                        Number::Fixnum(if negative { -1 } else { 1 }),
                    );
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn to_exact(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Flonum(n) => {
                if n == 0.0 {
                    return Self::Fixnum(0);
                }
                let (mant, exp, sign) = decode_double(n);

                if mant > 0 {
                    if exp == 0 {
                        return Self::from_i64(ctx, sign as i64 * mant);
                    }

                    if exp > 0 {
                        let bn = BigInt::from_i64(ctx, sign as i64 * mant);
                        let bn = BigInt::shift_left(bn, ctx, exp as usize);
                        return bn.into_number(ctx);
                    }

                    let mut bn = BigInt::one(ctx);
                    bn = BigInt::shift_left(bn, ctx, (-exp) as usize);

                    if sign < 0 {
                        bn = BigInt::negate(bn, ctx);
                    }
                    return Self::reduce(ctx, Self::from_i64(ctx, mant), bn.into_number(ctx));
                }
                unreachable!()
            }

            _ => self,
        }
    }

    pub fn exact_equal(lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                _ => false,
            },

            Number::BigInt(lhs) => match rhs {
                Number::BigInt(rhs) => lhs == rhs,
                _ => false,
            },

            Number::Rational(lhs) => match rhs {
                Number::Rational(rhs) => {
                    Self::exact_equal(lhs.numerator, rhs.numerator)
                        && Self::exact_equal(lhs.denominator, rhs.denominator)
                }
                _ => false,
            },

            _ => false,
        }
    }

    pub fn inexact_equal(lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Flonum(lhs) => match rhs {
                Number::Flonum(rhs) => {
                    return lhs.to_bits() == rhs.to_bits();
                }

                Number::Complex(cn) => {
                    if cn.imag.is_zero() {
                        return Self::inexact_equal(Self::Flonum(lhs), cn.real);
                    } else {
                        return false;
                    }
                }

                _ => false,
            },

            Number::Complex(cn) => {
                if cn.imag.is_zero() {
                    return Self::inexact_equal(cn.real, rhs);
                }

                match rhs {
                    Number::Complex(cn2) => {
                        return Self::inexact_equal(cn.real, cn2.real)
                            && Self::inexact_equal(cn.imag, cn2.imag);
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    pub fn equal(ctx: Context<'gc>, lhs: Self, rhs: Self) -> bool {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                Number::Flonum(rhs) => (lhs as f64 - rhs).abs() < f64::EPSILON,
                Number::BigInt(rhs) => false,
                Number::Rational(rn) => false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::Fixnum(lhs), cn.real)
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => (lhs - rhs as f64).abs() < f64::EPSILON,
                Number::Flonum(rhs) => (lhs - rhs).abs() < f64::EPSILON,
                Number::BigInt(rhs) => {
                    if rhs.as_f64() == lhs {
                        return Self::compare(ctx, Self::Flonum(lhs), Self::BigInt(rhs))
                            == Some(std::cmp::Ordering::Equal);
                    }

                    return false;
                }
                Number::Rational(rn) => rn.to_f64(ctx) == lhs,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::Flonum(lhs), cn.real)
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    return false;
                }

                Number::Flonum(rhs) => (lhs.as_f64() - rhs).abs() < f64::EPSILON,
                Number::BigInt(rhs) => lhs == rhs,
                Number::Rational(rn) => return false,
                Number::Complex(cn) => {
                    cn.imag.is_zero() && Self::equal(ctx, Number::BigInt(lhs), cn.real)
                }
            },

            Number::Rational(lhs) => {
                let nume = lhs.numerator;
                let deno = lhs.denominator;

                match rhs {
                    Number::Fixnum(rhs) => {
                        return false;
                    }

                    Number::Flonum(rhs) => (lhs.to_f64(ctx) - rhs).abs() < f64::EPSILON,
                    Number::BigInt(rhs) => return false,
                    Number::Rational(rhs) => {
                        return Self::equal(ctx, nume, rhs.numerator)
                            && Self::equal(ctx, deno, rhs.denominator);
                    }

                    Number::Complex(cn) => {
                        cn.imag.is_zero() && Self::equal(ctx, Self::Rational(lhs), cn.real)
                    }
                }
            }

            Number::Complex(cn) => {
                if cn.imag.is_zero() {
                    return Self::equal(ctx, cn.real, rhs);
                }

                match rhs {
                    Number::Complex(cn2) => {
                        return Self::equal(ctx, cn.real, cn2.real)
                            && Self::equal(ctx, cn.imag, cn2.imag);
                    }
                    Number::Fixnum(_)
                    | Number::Flonum(_)
                    | Number::BigInt(_)
                    | Number::Rational(_) => false,
                }
            }
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Number::Flonum(f) => f.is_nan(),
            _ => false,
        }
    }

    pub fn compare(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Option<std::cmp::Ordering> {
        match lhs {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => Some(lhs.cmp(&rhs)),
                Number::Flonum(rhs) => {
                    let d = lhs as f64 - rhs;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }
                Number::BigInt(rhs) => Some(if rhs.is_negative() {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }),
                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    return Self::compare(ctx, Self::mul(ctx, Number::Fixnum(lhs), deno), nume);
                }
                Number::Complex(cn) => {
                    if cn.imag.is_zero() {
                        return Self::compare(ctx, Number::Fixnum(lhs), cn.real);
                    }
                    None
                }
            },

            Number::Flonum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    let d = lhs - rhs as f64;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }
                Number::Flonum(rhs) => {
                    let d = lhs - rhs;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }
                Number::BigInt(rhs) => Some(if rhs.is_negative() {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }),
                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    return Self::compare(ctx, Self::mul(ctx, Number::Flonum(lhs), deno), nume);
                }
                Number::Complex(cn) => {
                    if cn.imag.is_zero() {
                        return Self::compare(ctx, Number::Flonum(lhs), cn.real);
                    }
                    None
                }
            },

            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if lhs.is_negative() {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }

                Number::Flonum(rhs) => {
                    let d = lhs.as_f64() - rhs;
                    if d == 0.0 {
                        return Some(std::cmp::Ordering::Equal);
                    }

                    if d > 0.0 {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }

                Number::BigInt(rhs) => {
                    return Some(lhs.cmp(&rhs));
                }

                Number::Rational(rn) => {
                    let nume = rn.numerator;
                    let deno = rn.denominator;

                    return Self::compare(ctx, Self::mul(ctx, Self::BigInt(lhs), deno), nume);
                }

                Number::Complex(rhs) => {
                    if rhs.imag.is_zero() {
                        return Self::compare(ctx, Number::BigInt(lhs), rhs.real);
                    }
                    None
                }
            },

            Number::Rational(lhs) => {
                let nume = lhs.numerator;
                let deno = lhs.denominator;

                match rhs {
                    Number::Fixnum(rhs) => {
                        return Self::compare(ctx, nume, Self::mul(ctx, Number::Fixnum(rhs), deno));
                    }

                    Number::Flonum(rhs) => {
                        let d = lhs.to_f64(ctx) - rhs;
                        if d == 0.0 {
                            return Some(std::cmp::Ordering::Equal);
                        }

                        if d > 0.0 {
                            return Some(std::cmp::Ordering::Greater);
                        } else {
                            return Some(std::cmp::Ordering::Less);
                        }
                    }

                    Number::BigInt(rhs) => {
                        return Self::compare(ctx, nume, Self::mul(ctx, Number::BigInt(rhs), deno));
                    }

                    Number::Rational(rhs) => {
                        let nume2 = rhs.numerator;
                        let deno2 = rhs.denominator;

                        return Self::compare(
                            ctx,
                            Self::mul(ctx, nume, deno2),
                            Self::mul(ctx, nume2, deno),
                        );
                    }

                    Number::Complex(cn) => {
                        if cn.imag.is_zero() {
                            return Self::compare(ctx, nume, Self::mul(ctx, cn.real, deno));
                        }
                        None
                    }
                }
            }

            Number::Complex(lhs) => {
                if lhs.imag.is_zero() {
                    return Self::compare(ctx, lhs.real, rhs);
                }

                None
            }
        }
    }

    pub fn from_i64(ctx: Context<'gc>, n: i64) -> Self {
        if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n))
    }

    pub fn from_u64(ctx: Context<'gc>, n: u64) -> Self {
        if n <= i32::MAX as u64 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n))
    }

    pub fn from_u128(ctx: Context<'gc>, n: u128) -> Self {
        if n <= i32::MAX as u128 {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u128(ctx, n))
    }

    pub fn from_u32(ctx: Context<'gc>, n: u32) -> Self {
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

    pub fn from_usize(ctx: Context<'gc>, n: usize) -> Self {
        if n <= i32::MAX as usize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_u64(ctx, n as _))
    }

    pub fn from_isize(ctx: Context<'gc>, n: isize) -> Self {
        if n >= i32::MIN as isize && n <= i32::MAX as isize {
            return Self::Fixnum(n as i32);
        }

        Self::BigInt(BigInt::from_i64(ctx, n as _))
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::Flonum(n) => *n > 0.0,
            Self::BigInt(n) => n.is_positive(),
            Self::Rational(rn) => rn.numerator.is_positive(),
            Self::Complex(c) => c.real.is_positive(),
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(_) => false,
            Self::Complex(c) => c.real.is_exact() && c.imag.is_exact(),
        }
    }

    pub fn is_real_valued(&self) -> bool {
        if self.is_real() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero(),
            _ => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::Flonum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Complex(c) => false,
        }
    }

    pub fn is_finite(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) | Self::Rational(_) => true,
            Self::Flonum(n) => n.is_finite(),
            Self::Complex(c) => c.real.is_finite() && c.imag.is_finite(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        !self.is_finite()
    }

    pub fn is_exact_integer(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => true,
            _ => false,
        }
    }

    pub fn is_exact_positive_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n > 0,
            Self::BigInt(n) => n.is_positive() && !n.is_zero(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Rational(_) => false,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }

                return fl.round() == *fl;
            }
            _ => false,
        }
    }

    pub fn is_integer_valued(&self) -> bool {
        if self.is_integer() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_integer_valued(),
            _ => false,
        }
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Self::Rational(_) | Self::Fixnum(_) | Self::BigInt(_) => true,
            Self::Flonum(fl) => {
                if fl.is_infinite() || fl.is_nan() {
                    return false;
                }
                true
            }
            _ => false,
        }
    }

    pub fn is_rational_valued(&self) -> bool {
        if self.is_rational() {
            return true;
        }

        match self {
            Self::Complex(c) => c.imag.is_zero() && c.real.is_rational_valued(),
            _ => false,
        }
    }

    pub fn is_exact_non_negative_integer(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n >= 0,
            Self::BigInt(n) => n.is_positive() || n.is_zero(),
            _ => false,
        }
    }

    pub fn is_even(&self) -> bool {
        match self {
            Self::Fixnum(n) => n & 1 == 0,
            Self::BigInt(n) => n[0] & 1 == 0,
            Self::Flonum(n) => n * 0.5 == (n * 0.5).floor(),

            Self::Complex(n) => n.real.is_even(),
            _ => false,
        }
    }

    pub fn ash(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        if count.is_negative() {
            return self.rsh(ctx, count.negate(ctx));
        }

        self.lsh(ctx, count)
    }

    pub fn lsh(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        if self.is_zero() {
            return Some(self);
        }

        let count = count.coerce_exact_integer_to_usize();

        if count == 0 {
            return Some(self);
        } else if count / (size_of::<i32>() * 8) >= i32::MAX as usize {
            return None;
        }

        match self {
            Self::Fixnum(n) => {
                let s = n.checked_shl(count as _);
                if let Some(n) = s {
                    return Some(Self::Fixnum(n));
                }

                let bn = BigInt::from_i64(ctx, n as _);
                return Some(Self::BigInt(BigInt::shift_left(bn, ctx, count)));
            }

            Self::BigInt(n) => {
                return Some(Self::BigInt(BigInt::shift_left(n, ctx, count)));
            }

            _ => None,
        }
    }

    pub fn rsh(self, ctx: Context<'gc>, count: Self) -> Option<Self> {
        let count = count.coerce_exact_integer_to_usize();
        if count == 0 {
            return Some(self);
        }

        match self {
            Self::Fixnum(n) => {
                let s = n.checked_shr(count as _);
                if let Some(n) = s {
                    return Some(Self::Fixnum(n));
                }

                let bn = BigInt::from_i64(ctx, n as _);
                return Some(Self::BigInt(BigInt::shift_right(bn, ctx, count)));
            }

            Self::BigInt(n) => {
                return Some(Self::BigInt(BigInt::shift_right(n, ctx, count)));
            }

            _ => None,
        }
    }

    pub fn logior(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => {
                return Self::Fixnum(lhs | rhs);
            }

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                return Self::BigInt(BigInt::or(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                return Self::BigInt(BigInt::or(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                return Self::BigInt(BigInt::or(lhs, ctx, rhs));
            }

            _ => unreachable!(),
        }
    }

    pub fn logand(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => {
                return Self::Fixnum(lhs & rhs);
            }

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                return Self::BigInt(BigInt::and(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                return Self::BigInt(BigInt::and(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                return Self::BigInt(BigInt::and(lhs, ctx, rhs));
            }

            _ => unreachable!(),
        }
    }

    pub fn logxor(self, ctx: Context<'gc>, other: Self) -> Self {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => {
                return Self::Fixnum(lhs ^ rhs);
            }

            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                return Self::BigInt(BigInt::xor(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                return Self::BigInt(BigInt::xor(lhs, ctx, rhs));
            }

            (Self::BigInt(lhs), Self::BigInt(rhs)) => {
                return Self::BigInt(BigInt::xor(lhs, ctx, rhs));
            }

            _ => unreachable!(),
        }
    }

    pub fn lognot(self, ctx: Context<'gc>) -> Self {
        match self {
            Self::Fixnum(n) => {
                let bn = BigInt::from_i64(ctx, n as _);
                return Self::BigInt(BigInt::not(bn, ctx));
            }

            Self::BigInt(n) => {
                return Self::BigInt(BigInt::not(n, ctx));
            }

            _ => unreachable!(),
        }
    }

    pub fn logtest(self, ctx: Context<'gc>, other: Self) -> bool {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => (lhs & rhs) != 0,
            (Self::Fixnum(lhs), Self::BigInt(rhs)) => {
                let lhs = BigInt::from_i64(ctx, lhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::Fixnum(rhs)) => {
                let rhs = BigInt::from_i64(ctx, rhs as _);
                !BigInt::and(lhs, ctx, rhs).is_zero()
            }
            (Self::BigInt(lhs), Self::BigInt(rhs)) => !BigInt::and(lhs, ctx, rhs).is_zero(),
            _ => unreachable!(),
        }
    }

    pub fn logbit(self, ctx: Context<'gc>, n: Self) -> bool {
        let n = n.coerce_exact_integer_to_usize();
        if n / (size_of::<i32>() * 8) >= i32::MAX as usize {
            return false;
        }

        match self {
            Self::Fixnum(num) => {
                if n < (size_of::<i32>() * 8) as usize {
                    return (num & (1 << n)) != 0;
                }

                let bn = BigInt::from_i64(ctx, num as _);
                return bn.is_bit_set(n);
            }

            Self::BigInt(bi) => {
                if n < (size_of::<Digit>() * 8) as usize {
                    return (bi[0] & (1 << n)) != 0;
                }

                return bi.is_bit_set(n);
            }

            _ => false,
        }
    }

    pub fn abs(self, ctx: Context<'gc>) -> Self {
        if self.is_positive() {
            return self;
        }

        self.negate(ctx)
    }

    pub fn polar(self, ctx: Context<'gc>, rhs: Self) -> Self {
        if rhs.is_zero() {
            return self;
        }

        let r = self.real_to_f64(ctx);
        let a = rhs.real_to_f64(ctx);
        Self::Complex(Complex::new(
            ctx,
            Self::Flonum(r * libm::cos(a)),
            Self::Flonum(r * libm::sin(a)),
        ))
    }

    pub fn angle(self, ctx: Context<'gc>) -> Self {
        if let Self::Complex(c) = self {
            let real = c.real.real_to_f64(ctx);
            let imag = c.imag.real_to_f64(ctx);
            return Self::Flonum(libm::atan2(imag, real));
        }

        if self.is_negative() {
            return Self::Flonum(std::f64::consts::PI);
        }

        if let Self::Flonum(n) = self {
            return Self::Flonum(0.0);
        }

        Self::Fixnum(0)
    }
}

impl<'gc> Rational<'gc> {
    pub fn add(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::add(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn mul(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.numerator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn sub(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let deno = Number::mul(ctx, lhs.denominator, rhs.denominator);
        let nume = Number::sub(
            ctx,
            Number::mul(ctx, lhs.numerator, rhs.denominator),
            Number::mul(ctx, rhs.numerator, lhs.denominator),
        );

        Number::reduce(ctx, nume, deno)
    }

    pub fn div(ctx: Context<'gc>, lhs: Gc<'gc, Self>, rhs: Gc<'gc, Self>) -> Number<'gc> {
        let nume = Number::mul(ctx, lhs.numerator, rhs.denominator);
        let deno = Number::mul(ctx, lhs.denominator, rhs.numerator);

        Number::reduce(ctx, nume, deno)
    }

    pub fn to_f64(&self, ctx: Context<'gc>) -> f64 {
        let mut nume = self.numerator.real_to_f64(ctx);
        let mut deno = self.denominator.real_to_f64(ctx);

        if nume.is_infinite() || deno.is_infinite() {
            if nume.is_infinite() && deno.is_infinite() {
                let nume_bitsize = BigInt::bitsize(self.numerator.as_bigint().unwrap());
                let deno_bitsize = BigInt::bitsize(self.denominator.as_bigint().unwrap());
                let mut shift = if nume_bitsize > deno_bitsize {
                    nume_bitsize as isize - 96
                } else {
                    deno_bitsize as isize - 96
                };

                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(self.numerator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
                deno = BigInt::shift_right(self.denominator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
            } else if deno.is_infinite() {
                let deno_bitsize = BigInt::bitsize(self.denominator.as_bigint().unwrap());
                let mut shift = deno_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = libm::ldexp(nume, -(shift as i32));
                deno = BigInt::shift_right(self.denominator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
            } else {
                let nume_bitsize = BigInt::bitsize(self.numerator.as_bigint().unwrap());
                let mut shift = nume_bitsize as isize - 96;
                if shift < 1 {
                    shift = 1;
                }

                nume = BigInt::shift_right(self.numerator.as_bigint().unwrap(), ctx, shift as _)
                    .as_f64();
                deno = libm::ldexp(deno, -(shift as i32));
            }
        }

        nume / deno
    }
}

pub trait IntoNumber<'gc> {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc>;
}

impl<'gc> IntoNumber<'gc> for i32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self)
    }
}

impl<'gc> IntoNumber<'gc> for i64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self >= i32::MIN as i64 && self <= i32::MAX as i64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u32 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self as u64))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as u64 {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u64(ctx, self))
        }
    }
}

impl<'gc> IntoNumber<'gc> for f64 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Flonum(self)
    }
}

impl<'gc> IntoNumber<'gc> for f32 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Flonum(self as f64)
    }
}

impl<'gc> IntoNumber<'gc> for u16 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for u8 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i16 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for i8 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::Fixnum(self as i32)
    }
}

impl<'gc> IntoNumber<'gc> for usize {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self <= i32::MAX as usize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_u128(ctx, self as u128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for isize {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        if self >= i32::MIN as isize && self <= i32::MAX as isize {
            Number::Fixnum(self as i32)
        } else {
            Number::BigInt(BigInt::from_i128(ctx, self as i128))
        }
    }
}

impl<'gc> IntoNumber<'gc> for u128 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_u128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for i128 {
    fn into_number(self, ctx: Context<'gc>) -> Number<'gc> {
        Number::BigInt(BigInt::from_i128(ctx, self))
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, BigInt<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        if let Some(i) = self
            .try_as_i64()
            .filter(|&v| v >= i32::MIN as i64 && v <= i32::MAX as i64)
        {
            Number::Fixnum(i as i32)
        } else {
            Number::BigInt(self)
        }
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Rational<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        Number::Rational(self)
    }
}

impl<'gc> IntoNumber<'gc> for Gc<'gc, Complex<'gc>> {
    fn into_number(self, _ctx: Context<'gc>) -> Number<'gc> {
        Number::Complex(self)
    }
}

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
    (mant as i64, exp, sign)
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
        if c >= '0' && c <= '9' {
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

pub fn parse_uinteger<'gc>(
    ctx: Context<'gc>,
    s: &mut Peekable<impl Iterator<Item = char>>,
    radix: u32,
) -> Option<Number<'gc>> {
    if s.peek().is_none() {
        return None;
    }

    let mut ans = Number::Fixnum(0);
    while let Some(&c) = s.peek() {
        if c == '#' {
            return Some(ans);
        }
        let digit;
        if c >= '0' && c <= '9' {
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

impl<'gc> Value<'gc> {
    pub fn is_number(self) -> bool {
        self.is_inline_number()
            || self.is::<BigInt>()
            || self.is::<Rational>()
            || self.is::<Complex>()
    }

    pub fn number(self) -> Option<Number<'gc>> {
        if self.is_inline_number() {
            if self.is_int32() {
                return Some(Number::Fixnum(self.as_int32()));
            } else {
                return Some(Number::Flonum(self.as_flonum()));
            }
        }

        if self.is::<BigInt>() {
            return Some(Number::BigInt(self.downcast()));
        }

        if self.is::<Rational>() {
            return Some(Number::Rational(self.downcast()));
        }

        if self.is::<Complex>() {
            return Some(Number::Complex(self.downcast()));
        }

        None
    }
}

impl<'gc> Hash for Number<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Fixnum(n) => n.hash(state),
            Number::Flonum(n) => n.to_bits().hash(state),
            Number::BigInt(b) => b.hash(state),
            Number::Rational(r) => {
                r.numerator.hash(state);
                r.denominator.hash(state);
            }
            Number::Complex(c) => {
                c.real.hash(state);
                c.imag.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum ExactInteger<'gc> {
    Fixnum(i32),
    BigInt(Gc<'gc, BigInt<'gc>>),
}

impl<'gc> ExactInteger<'gc> {
    pub fn to_number(self, ctx: Context<'gc>) -> Number<'gc> {
        match self {
            Self::Fixnum(n) => Number::Fixnum(n),
            Self::BigInt(b) => Number::BigInt(b),
        }
    }

    pub fn bitor(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l | r),
                Self::BigInt(r) => {
                    let res = BigInt::or_digit(r, ctx, l as u64);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let res = BigInt::or_digit(l, ctx, r as u64);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::or(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitand(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l & r),
                Self::BigInt(r) => {
                    let res = BigInt::and_digit(r, ctx, l as u64);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let res = BigInt::and_digit(l, ctx, r as u64);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::and(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitxor(ctx: Context<'gc>, lhs: Self, rhs: Self) -> Self {
        match lhs {
            Self::Fixnum(l) => match rhs {
                Self::Fixnum(r) => Self::Fixnum(l ^ r),
                Self::BigInt(r) => {
                    let res = BigInt::xor_digit(r, ctx, l as u64);
                    Self::BigInt(res)
                }
            },

            Self::BigInt(l) => match rhs {
                Self::Fixnum(r) => {
                    let res = BigInt::xor_digit(l, ctx, r as u64);
                    Self::BigInt(res)
                }
                Self::BigInt(r) => {
                    let res = BigInt::xor(l, ctx, r);
                    Self::BigInt(res)
                }
            },
        }
    }

    pub fn bitnot(ctx: Context<'gc>, n: Self) -> Self {
        match n {
            Self::Fixnum(n) => Self::Fixnum(!n),
            Self::BigInt(b) => {
                let res = BigInt::not(b, ctx);
                Self::BigInt(res)
            }
        }
    }

    pub fn is_bit_set(ctx: Context<'gc>, n: Self, bit: u32) -> bool {
        match n {
            Self::Fixnum(n) => {
                if bit >= 32 {
                    return false;
                }

                (n & (1 << bit)) != 0
            }
            Self::BigInt(b) => b.is_bit_set(bit as usize),
        }
    }

    pub fn first_bit_set(ctx: Context<'gc>, n: Self) -> i32 {
        match n {
            Self::Fixnum(n) => {
                if n == 0 {
                    return -1;
                }

                n.trailing_zeros() as i32
            }
            Self::BigInt(b) => {
                if b.is_zero() {
                    return -1;
                }

                b.first_bit_set() as i32
            }
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Self::Fixnum(n) => *n < 0,
            Self::BigInt(b) => b.is_negative(),
        }
    }

    pub fn bit_count(&self) -> u32 {
        match self {
            Self::Fixnum(n) => n.count_ones(),
            Self::BigInt(b) => b.count_ones() as _,
        }
    }

    pub fn bit_length(&self, ctx: Context<'gc>) -> u32 {
        match self {
            Self::Fixnum(n) => {
                if *n == 0 {
                    return 0;
                }
                let n2 = if *n < 0 { !*n } else { *n };
                return 32 - n2.leading_zeros();
            }
            Self::BigInt(b) => {
                if b.is_positive() {
                    return BigInt::bitsize(*b) as _;
                }

                let nb = BigInt::not(*b, ctx);
                return BigInt::bitsize(nb) as _;
            }
        }
    }
}

fn i32_to_raidx(n: i32, radix: u8) -> String {
    if n == 0 {
        return "0".to_string();
    }
    // Use a character map for digits.
    const CHARS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";

    // Handle negative numbers.
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

    final_string
}

impl<'gc> Number<'gc> {
    pub fn to_string_radix(self, base: u8) -> String {
        match self {
            Self::Fixnum(n) => i32_to_raidx(n, base),
            Self::Flonum(n) => {
                lexical::to_string_with_options::<f64, { lexical::format::STANDARD }>(
                    n,
                    &lexical::WriteFloatOptions::from_radix(base),
                )
            }

            Self::BigInt(n) => {
                let opts = NumberToStringOptions {
                    base: match base {
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

                n.to_string_with_options(&opts)
            }

            Self::Rational(rn) => {
                let nume = rn.numerator.to_string_radix(base);
                let denom = rn.denominator.to_string_radix(base);
                format!("{}/{}", nume, denom)
            }

            Self::Complex(c) => {
                let real = c.real.to_string_radix(base);
                let imag = c.imag.to_string_radix(base);
                format!("{} + {}i", real, imag)
            }
        }
    }
}

impl<'gc> FromValue<'gc> for ExactInteger<'gc> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            return Ok(Self::Fixnum(value.as_int32()));
        }

        if value.is::<BigInt>() {
            return Ok(Self::BigInt(value.downcast()));
        }

        Err(ConversionError::TypeMismatch {
            pos: 0,
            expected: "exact integer",
            found: value,
        })
    }
}

impl<'gc> Into<Value<'gc>> for ExactInteger<'gc> {
    fn into(self) -> Value<'gc> {
        match self {
            Self::Fixnum(n) => Value::new(n),
            Self::BigInt(b) => b.into(),
        }
    }
}

impl<'gc> Into<Number<'gc>> for ExactInteger<'gc> {
    fn into(self) -> Number<'gc> {
        match self {
            Self::Fixnum(n) => Number::Fixnum(n),
            Self::BigInt(b) => Number::BigInt(b),
        }
    }
}

impl<'gc> IntoValue<'gc> for ExactInteger<'gc> {
    fn into_value(self, _ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Self::Fixnum(n) => Value::new(n),
            Self::BigInt(b) => b.into(),
        }
    }
}
