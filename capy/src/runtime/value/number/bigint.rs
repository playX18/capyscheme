//! Arbitrary-precision integer representation and formatting.

use std::{
    borrow::Cow,
    fmt,
    hash::Hash,
    marker::PhantomData,
    mem::{align_of, size_of},
    ops::{Deref, DerefMut, Index, IndexMut, Range, RangeFrom, RangeTo},
    sync::OnceLock,
};

#[cfg(feature = "bootstrap")]
use num_bigint::{BigInt as NumBigInt, Sign as NumSign};

use crate::rsgc::{
    Gc, Trace,
    collection::Visitor,
    global::Global,
    mmtk::{AllocationSemantics, util::conversions::raw_align_up},
    mutator::Mutation,
    object::{
        AllocationHooks, ClassId, GCObject, builtin_class_ids, class_header_word,
    },
};
use rand::Rng;

use crate::runtime::Context;

use crate::runtime::value::ClassTagged;

pub const DIGIT_BIT: usize = 64;
pub const DIGIT_MASK: u64 = u64::MAX;
pub const DIGIT_BIT_SHIFT_COUNT: u32 = 6;

const BASE: Digit2X = Digit::MAX as Digit2X + 1;

pub type Digit2X = u128;
pub type Digit = u64;

extern "C" fn compute_bigint_size(object: GCObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    let bigint = unsafe { object.to_address().as_ref::<BigInt<'static>>() };
    let raw = bigint.num_words() * size_of::<Digit>() + size_of::<BigInt>();
    raw_align_up(raw, align_of::<BigInt>())
}

extern "C" fn trace_bigint(_object: GCObject, _visitor: &mut Visitor<'_>) {}

extern "C" fn process_weak_bigint(
    _object: GCObject,
    _weak_processor: &mut crate::rsgc::WeakProcessor,
) {
}
pub type IDigit = i64;
pub type IDigit2X = i128;
type RootedBigInt = crate::Rootable!(Gc<'_, BigInt<'_>>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParseBigIntError;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
/// Sign of an exact integer.
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
#[repr(C, align(8))]
/// Heap-allocated arbitrary-precision integer.
pub struct BigInt<'gc> {
    count: u32,
    negative: bool,
    pad: [u8; 3],
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
    /// Allocates a bigint from little-endian machine words.
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

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + std::mem::size_of_val(words),
                std::mem::align_of::<Self>(),
            );

            let bigint = mc.allocate_with_layout_header_word::<Self>(
                layout,
                bigint_header_word(),
                AllocationSemantics::Default,
            );

            bigint.as_mut_ptr().write(BigInt {
                count: words.len() as u32,
                negative,
                pad: [0; 3],
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
        self.negative
    }

    #[inline]
    pub fn zeroed<F, E>(
        mc: Mutation<'gc>,
        mut count: usize,
        negative: bool,
        fill: F,
    ) -> Result<Gc<'gc, Self>, E>
    where
        F: FnOnce(&mut Self) -> Result<(), E>,
    {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + count * std::mem::size_of::<Digit>(),
                std::mem::align_of::<Self>(),
            );

            let bigint = mc.allocate_with_layout_header_word::<Self>(
                layout,
                bigint_header_word(),
                AllocationSemantics::Default,
            );

            bigint.as_mut_ptr().write(BigInt {
                count: count as u32,
                negative,
                pad: [0; 3],
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
            (*bigint.as_mut_ptr()).count = count as u32;

            Ok(bigint.assume_init())
        }
    }

    /// Returns the number of words in this BigInt.
    pub fn num_words(&self) -> usize {
        self.count()
    }

    /// Returns a slice to the words of this BigInt.
    pub fn words_slice(&self) -> &[Digit] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), self.num_words()) }
    }

    /// Returns a mutable slice to the words of this BigInt.
    /// Note: This requires mutable access to the Gc'd BigInt, e.g., via `Gc::write`.
    pub fn words_slice_mut(&mut self) -> &mut [Digit] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), self.num_words()) }
    }

    pub fn count(&self) -> usize {
        self.count as usize
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
        Self::new::<false>(ctx, self, negative)
    }

    #[cfg(feature = "bootstrap")]
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
    #[cfg(feature = "bootstrap")]
    pub fn from_num_bigint(ctx: Context<'gc>, num_bigint: &NumBigInt) -> Gc<'gc, Self> {
        if *num_bigint == NumBigInt::from(0) {
            return Self::zero(ctx);
        }

        let (sign, bytes) = num_bigint.to_bytes_be();
        let negative = matches!(sign, NumSign::Minus);

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

    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "BigInt",
        instance_size: 0,
        alignment: align_of::<usize>(),
        compute_alignment: None,
        compute_size: Some(compute_bigint_size),
        trace: trace_bigint,
        weak_proc: process_weak_bigint,
    };
}

fn bigint_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::BIGINT).unwrap())
}

// SAFETY: `gc` for `BigInt` upholds all trait invariants
unsafe impl<'gc> Trace for BigInt<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, _visitor: &mut Visitor<'_>) {}
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

// SAFETY: `gc` for `BigInt` upholds all trait invariants
unsafe impl<'gc> ClassTagged for BigInt<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::BIGINT];
    const TYPE_NAME: &'static str = "bigint";
}

impl<'gc> Index<usize> for BigInt<'gc> {
    type Output = Digit;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.count());
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { &*(self.words.as_ptr().add(index) as *const Digit) }
    }
}

impl<'gc> IndexMut<usize> for BigInt<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(index < self.count());
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { &mut *(self.words.as_mut_ptr().add(index) as *mut Digit) }
    }
}

impl<'gc> Index<Range<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        assert!(range.start < self.count());
        assert!(range.end <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), range.len()) }
    }
}

impl<'gc> Index<RangeTo<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeTo<usize>) -> &Self::Output {
        assert!(range.end <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), range.end) }
    }
}

impl<'gc> IndexMut<RangeTo<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeTo<usize>) -> &mut Self::Output {
        assert!(range.end <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), range.end) }
    }
}

impl<'gc> Index<RangeFrom<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeFrom<usize>) -> &Self::Output {
        assert!(range.start < self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), self.count()) }
    }
}

impl<'gc> IndexMut<RangeFrom<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeFrom<usize>) -> &mut Self::Output {
        assert!(range.start < self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), self.count())
        }
    }
}

impl<'gc> IndexMut<Range<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: Range<usize>) -> &mut Self::Output {
        assert!(range.start < self.count());
        assert!(range.end <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), range.len())
        }
    }
}

impl<'gc> Deref for BigInt<'gc> {
    type Target = [Digit];

    fn deref(&self) -> &Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), self.count()) }
    }
}

impl<'gc> DerefMut for BigInt<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), self.count()) }
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

        if self.negative() { -result } else { result }
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

        Self::zeroed::<_, ()>(*ctx, b1.count() + 1, this.negative(), |result| {
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

            result.negative = this.negative();
            result.count = off as u32;
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

        BigInt::zeroed(*ctx, b1.count() + 1, sign, |res| {
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

            res.count = offset as u32;

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
        if false && this.count().min(rhs.count()) >= KARATSUBA_THRESHOLD {
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

        Self::zeroed(*ctx, b1.count() + b2.count(), a.negative(), |res| {
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

            res.negative = a.negative() != b.negative();

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
        let half = max_len.div_ceil(2);

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
            Self::new::<true>(ctx, &result, result_negative)
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
        result_words.extend_from_slice(&num);

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
        static ZERO_BIGINT: OnceLock<Global<RootedBigInt>> = OnceLock::new();

        *ZERO_BIGINT
            .get_or_init(|| {
                let zero = BigInt::new::<false>(ctx, &[0], false);

                Global::new(zero)
            })
            .fetch(*ctx)
    }

    pub fn one(ctx: Context<'gc>) -> Gc<'gc, Self> {
        static ONE_BIGINT: OnceLock<Global<RootedBigInt>> = OnceLock::new();

        *ONE_BIGINT
            .get_or_init(|| {
                let one = BigInt::new::<true>(ctx, &[1], false);

                Global::new(one)
            })
            .fetch(*ctx)
    }

    pub fn parse(
        ctx: Context<'gc>,
        str: &str,
        base: &Base,
    ) -> Result<Gc<'gc, Self>, ParseBigIntError> {
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
            return Err(ParseBigIntError);
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
        for (res_digits, &digit) in digits.iter().rev().enumerate() {
            if res_digits > 0
                && group_size > 0
                && res_digits % group_size == 0
                && let Some(sep) = group_sep
            {
                res.push_str(sep);
            }
            let ch = base.digit_space.get(digit as usize).copied().unwrap_or('?');
            res.push(ch);
        }

        if res.is_empty() {
            res.push('0');
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

    fn mult_sub(approx: Digit, divis: &[Digit], rem: &mut [Digit], from: usize) {
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

    fn sub_if_possible(divis: &[Digit], rem: &mut [Digit], from: usize) -> bool {
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
            Self::zeroed(*ctx, words.len(), true, |res| {
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
                    res.count = res.len() as u32 - 1;
                }

                res.negative = true;
                Result::<(), ()>::Ok(())
            })
            .unwrap()
        } else {
            Self::new::<true>(ctx, words, false)
        }
    }

    pub fn random<R: Rng>(ctx: Context<'gc>, rng: &mut R, bitwidth: usize) -> Gc<'gc, Self> {
        let mut words = vec![0; bitwidth.div_ceil(DIGIT_BIT)];
        rng.fill(&mut words[..]);
        Self::new::<true>(ctx, &words, false)
    }

    pub fn is_most_significant_bit_set(words: &[Digit]) -> bool {
        words
            .last()
            .is_some_and(|&word| word & (1 << (DIGIT_BIT - 1)) != 0)
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
        Self::minus(Self::negate(this, ctx), ctx, Self::one(ctx))
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

        Self::zeroed(*ctx, this.len() + swords + 1, this.negative(), |res| {
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

            res.count = offset as u32;

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    pub fn shift_right(this: Gc<'gc, Self>, ctx: Context<'gc>, shift: usize) -> Gc<'gc, Self> {
        let swords = shift / DIGIT_BIT;
        let sbits = shift % DIGIT_BIT;

        let mut res = Vec::with_capacity(this.len().saturating_sub(swords));

        let mut carry = 0;
        let mut i = this.len() as isize - 1;

        while i >= swords as isize {
            let word = this[i as usize];
            res.push((word >> sbits) | carry);
            carry = word << (DIGIT_BIT - sbits);
            i -= 1;
        }
        res.reverse();
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
        let last = this.count() - 1;

        let size = last * DIGIT_BIT;
        size + DIGIT_BIT - this[last].leading_zeros() as usize
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
            println!("mul: {}", mul);

            assert_eq!(q.to_string(), "9942799");
            assert_eq!(r.to_string(), "1797068403931412326117437881912");
            assert_eq!(
                mul.to_string(),
                "1531867023300609762396035507803021632000090312363887487217570791186711"
            );
        });
    }

    use super::{Base, builtin_class_ids};
    use crate::prelude::BigInt;
    use crate::rsgc::object::ClassId;

    #[test]
    fn test_bigint() {
        let scm = Scheme::new();

        scm.enter(|ctx| {
            let big_int = BigInt::parse(ctx, "2037035976334486086268445688409378161051468393665936250636140449354381299763336706183397376", &Base::DEC)
                .expect("Failed to parse BigInt");

        });
    }

    #[test]
    fn bigint_allocates_with_class_only_header() {
        let scm = Scheme::new_uninit();

        scm.enter(|ctx| {
            let bigint = BigInt::from_u64(ctx, u64::MAX);

            assert_eq!(
                bigint.as_gcobj().header().class_id(),
                ClassId::new(builtin_class_ids::BIGINT).unwrap()
            );
        });
    }
}
