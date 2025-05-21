use core::num;
use std::{
    borrow::Cow,
    fmt,
    marker::PhantomData,
    mem::{MaybeUninit, align_of, size_of},
    ops::{Deref, DerefMut, Index, IndexMut, Range, RangeFrom, RangeTo},
    sync::{
        OnceLock,
        atomic::{AtomicU16, Ordering},
    },
};

use rand::rand_core::le;
use rsgc::{
    Collect, EnsureGCInfo, GCInfo, GCInfoIndex, GCInfoIndexForT, Gc, Rootable, Trace, Visitor,
    alloc::array::Array,
    context::Mutation,
    gc::GLOBAL_GC_INFO_TABLE,
    generic_static::Namespace,
    mutator::Global,
    vmkit::prelude::{GCMetadata, TraceCallback},
};

use crate::runtime::Context;

use super::{Tagged, TypeCode8, TypeCode16, Value, ValuesNamespace, pure_nan::PURE_NAN_BITS};

pub const DIGIT_BIT: usize = 64;
pub const DIGIT_MASK: u64 = u64::MAX;
pub const DIGIT_BIT_SHIFT_COUNT: u32 = 6;
const BASE: Digit2X = Digit::MAX as Digit2X + 1;

pub type Digit2X = u128;
pub type Digit = u64;
pub type IDigit = i64;
pub type IDigit2X = i128;

const BN_QUANTUM: usize = 32;
const BN_STACK_LIMIT: usize = 1024;
const P_DIGITS: usize = 308;
const P_EXP10: usize = 22;

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

#[repr(C)]
pub struct BigInt<'gc> {
    count: u32, // Number of u32 words
    sign: Sign,
    words: [Digit; 0],              // Flexible array member: data follows here
    _phantom: PhantomData<&'gc ()>, // To ensure 'gc is used
}

impl<'gc> fmt::Display for BigInt<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        f.pad_integral(!self.is_negative(), "", &self.to_string_with_options(&Default::default()))
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
        mc: &Mutation<'gc>,
        mut words: &[Digit],
        sign: Sign,
    ) -> Gc<'gc, Self> {
        if NORMALIZE {
            while words.len() > 0 && words[words.len() - 1] == 0 {
                words = &words[..words.len() - 1]; // Remove trailing zeros
            }
        }

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + words.len() * std::mem::size_of::<Digit>(),
                std::mem::align_of::<Self>(),
            );

            let bigint = mc.allocate_with_layout::<Self>(layout);
            bigint.set_user_header(TypeCode16::BIG.into()); // Set the type code

            bigint.as_ptr().write(MaybeUninit::new(BigInt {
                count: words.len() as u32,
                sign,
                words: [],
                _phantom: PhantomData,
            }));

            // Initialize words to 0
            let bigint_data_ptr = bigint
                .as_ptr()
                .as_mut()
                .assume_init_mut()
                .words
                .as_mut_ptr();
            bigint_data_ptr.copy_from_nonoverlapping(words.as_ptr(), words.len());

            bigint.assume_init()
        }
    }

    pub fn zeroed<F, E>(
        mc: &Mutation<'gc>,
        count: usize,
        sign: Sign,
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

            let bigint = mc.allocate_with_layout::<Self>(layout);
            bigint.set_user_header(TypeCode16::BIG.into()); // Set the type code

            bigint.as_ptr().write(MaybeUninit::new(BigInt {
                count: count as _,
                sign,
                words: [],
                _phantom: PhantomData,
            }));

            // Initialize words to 0
            let bigint_data_ptr = bigint
                .as_ptr()
                .as_mut()
                .assume_init_mut()
                .words
                .as_mut_ptr();
            bigint_data_ptr.write_bytes(0, count);

            fill(bigint.as_ptr().as_mut().assume_init_mut())?;
            Ok(bigint.assume_init())
        }
    }

    /// Returns the number of words in this BigInt.
    pub fn num_words(&self) -> usize {
        self.count as usize
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

    unsafe fn raw_slice_mut(&self) -> &mut [Digit] {
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_ptr() as *mut Digit, self.count as usize)
        }
    }

    pub fn count(&self) -> usize {
        self.count as usize
    }

    pub fn flip2sc(&mut self) {
        let mut count = self.count();

        let mut acc: Digit2X = 1;

        for i in 0..count {
            acc = (!self[i] as Digit2X).wrapping_add(acc);
            self[i] = acc as Digit;
            acc >>= DIGIT_BIT;
        }
    }

    pub fn dup(&self, ctx: Context<'gc>, sign: Sign) -> Gc<'gc, Self> {
        Self::new::<false>(&ctx, &**self, sign)
    }
}

unsafe impl<'gc> Trace for BigInt<'gc> {
    fn trace(&mut self, _visitor: &mut Visitor<'_>) {
        // `count` and `negative` are primitive types.
        // `words` are `u32` which are not Gc pointers.
        // `_phantom` does not need tracing.
        // Thus, no fields within BigInt itself require tracing by the GC.
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for BigInt<'gc> {
    fn ensure_gc_info() -> GCInfoIndex {
        let registered_index =
            ValuesNamespace::generic_static::<GCInfoIndexForT<BigInt<'static>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_bigint_info(&registered_index.index)
    }
}

fn register_bigint_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {}, // BigInt does not have weak references
            id: 0,                    // Default ID
            gc_metadata: GCMetadata {
                alignment: align_of::<BigInt<'static>>(),
                compute_alignment: None, // Alignment is fixed
                compute_size: Some(|object| {
                    let bigint = unsafe { object.as_address().as_ref::<BigInt<'static>>() };
                    // Size of the variable part (the words data)
                    bigint.num_words() * size_of::<u32>()
                }),
                instance_size: size_of::<BigInt<'static>>(), // Size of the fixed part (count, negative, _phantom, words header)
                trace: TraceCallback::TraceObject(|object, tracer| {
                    let bigint = unsafe { object.as_address().as_mut_ref::<BigInt<'static>>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object.as_object_unchecked())) };
                    bigint.trace(&mut visitor); // Calls the Trace impl for BigInt
                }),
            },
        },
    )
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

impl<'gc> Index<usize> for BigInt<'gc> {
    type Output = Digit;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.count as usize);
        unsafe { &*(self.words.as_ptr().add(index) as *const Digit) }
    }
}

impl<'gc> IndexMut<usize> for BigInt<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.count as usize);
        unsafe { &mut *(self.words.as_mut_ptr().add(index) as *mut Digit) }
    }
}

impl<'gc> Index<Range<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        assert!(range.start < self.count as usize);
        assert!(range.end <= self.count as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), range.len()) }
    }
}

impl<'gc> Index<RangeTo<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeTo<usize>) -> &Self::Output {
        assert!(range.end <= self.count as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), range.end) }
    }
}

impl<'gc> IndexMut<RangeTo<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeTo<usize>) -> &mut Self::Output {
        assert!(range.end <= self.count as usize);
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), range.end) }
    }
}

impl<'gc> Index<RangeFrom<usize>> for BigInt<'gc> {
    type Output = [Digit];

    fn index(&self, range: RangeFrom<usize>) -> &Self::Output {
        assert!(range.start < self.count as usize);
        unsafe { std::slice::from_raw_parts(self.words.as_ptr().add(range.start), self.count()) }
    }
}

impl<'gc> IndexMut<RangeFrom<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeFrom<usize>) -> &mut Self::Output {
        assert!(range.start < self.count as usize);
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), self.count())
        }
    }
}

impl<'gc> IndexMut<Range<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: Range<usize>) -> &mut Self::Output {
        assert!(range.start < self.count as usize);
        assert!(range.end <= self.count as usize);
        unsafe {
            std::slice::from_raw_parts_mut(self.words.as_mut_ptr().add(range.start), range.len())
        }
    }
}

impl<'gc> Deref for BigInt<'gc> {
    type Target = [Digit];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.words.as_ptr(), self.count as usize) }
    }
}

impl<'gc> DerefMut for BigInt<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.words.as_mut_ptr(), self.count as usize) }
    }
}

const fn loword(x: Digit2X) -> Digit {
    (x & DIGIT_MASK as Digit2X) as Digit
}

const fn hiword(x: Digit2X) -> Digit {
    (x >> DIGIT_BIT) as Digit
}
const fn joinwords(x: Digit, y: Digit) -> Digit2X {
    ((x as Digit2X) << DIGIT_BIT) | (y as Digit2X)
}

impl<'gc> BigInt<'gc> {
    fn set_zero(&mut self) {
        self.sign = Sign::Zero;
        self.count = 0;
    }

    pub fn from_u64(ctx: Context<'gc>, value: u64) -> Gc<'gc, Self> {
        if value == 0 {
            return BigInt::new::<false>(&ctx, &[], Sign::Zero);
        }

        BigInt::new::<false>(&ctx, &[value], Sign::Positive)
    }

    pub fn from_i64(ctx: Context<'gc>, value: i64) -> Gc<'gc, Self> {
        if value == 0 {
            return BigInt::new::<false>(&ctx, &[], Sign::Zero);
        }

        let sign = if value < 0 {
            Sign::Negative
        } else {
            Sign::Positive
        };
        let abs_value = value.wrapping_abs() as u64;

        BigInt::new::<false>(&ctx, &[abs_value], sign)
    }

    pub fn try_as_i64(&self) -> Option<i64> {
        if self.count() > 1 {
            return None;
        }

        if self.sign.is_zero() {
            return Some(0);
        }

        let value = self[0];
        if self.sign.is_negative() && value == i64::MAX as u64 + 1 {
            return Some(i64::MIN);
        }

        if value <= i64::MAX as u64 {
            if self.sign.is_negative() {
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

        if self.sign.is_zero() {
            return Some(0);
        }

        let value = self[0];
        Some(value)
    }

    pub fn as_f64(&self) -> f64 {
        let mut result = 0.0;

        for &word in self.iter().rev() {
            result *= BASE as f64 + word as f64;
        }

        result
    }

    pub fn is_zero(&self) -> bool {
        self.sign.is_zero()
    }

    pub fn is_one(&self) -> bool {
        self.count() == 1 && self[0] == 1 && !self.sign.is_negative()
    }

    pub fn is_negative(&self) -> bool {
        self.sign.is_negative()
    }

    pub fn is_positive(&self) -> bool {
        self.sign.is_positive()
    }

    pub fn negate(self: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if self.is_zero() {
            return self;
        }

        self.dup(ctx, self.sign.flip())
    }

    pub fn abs(self: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        if self.is_zero() {
            return self;
        }

        if self.sign.is_negative() {
            self.dup(ctx, Sign::Positive)
        } else {
            self
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

    pub fn plus(self: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if rhs.is_zero() {
            return self;
        }

        if self.sign != rhs.sign {
            return self.minus(ctx, rhs.negate(ctx));
        }

        let (b1, b2) = if self.count() < rhs.count() {
            (self, rhs)
        } else {
            (rhs, self)
        };

        Self::zeroed::<_, ()>(&ctx, b1.count() + 1, self.sign, |result| {
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

            result.count = off as u32;
            result.sign = self.sign;
            Ok(())
        })
        .unwrap()
    }

    pub fn minus(self: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if rhs.is_zero() {
            return self;
        }
        if self.sign != rhs.sign {
            return self.plus(ctx, rhs.negate(ctx));
        }

        let cmp = self.compare_digits(&rhs);

        if cmp == std::cmp::Ordering::Equal {
            return Self::zero(ctx);
        }

        let sign = if cmp == std::cmp::Ordering::Less {
            self.sign.flip()
        } else {
            self.sign
        };

        let (b1, b2) = if cmp == std::cmp::Ordering::Less {
            (rhs, self)
        } else {
            (self, rhs)
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

            res.count = offset as u32;

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    pub fn times(self: Gc<'gc, Self>, ctx: Context<'gc>, rhs: Gc<'gc, Self>) -> Gc<'gc, Self> {
        if self.is_zero() || rhs.is_zero() {
            return Self::zero(ctx);
        }

        let (b1, b2) = if self.count() < rhs.count() {
            (rhs, self)
        } else {
            (self, rhs)
        };

        Self::zeroed(&ctx, b1.count() + b2.count(), self.sign, |res| {
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
            res.sign = if self.sign != rhs.sign {
                self.sign.flip()
            } else {
                self.sign
            };

            Result::<(), ()>::Ok(())
        })
        .unwrap()
    }

    pub fn zero(ctx: Context<'gc>) -> Gc<'gc, Self> {
        static ZERO_BIGINT: OnceLock<Global<Rootable!(Gc<'_, BigInt<'_>>)>> = OnceLock::new();

        *ZERO_BIGINT
            .get_or_init(|| {
                println!("made zero");
                let zero = BigInt::new::<false>(&ctx, &[], Sign::Zero);
                zero.set_user_header(TypeCode16::BIG.into());
                Global::new(zero)
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
    pub fn to_string_with_options(
        &self,
        options: &NumberToStringOptions<'gc>,
    ) -> String {
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
        for (i, &digit) in digits.iter().rev().enumerate() {
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

        let sign = if words.is_empty() {
            Sign::Zero
        } else if negative {
            Sign::Negative
        } else {
            Sign::Positive
        };

        BigInt::new::<true>(&ctx, &words, sign)
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

