//! Arbitrary-precision integer representation and formatting.

use crate::heap::Space;
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

use crate::heap::{
    Gc, Trace,
    collection::Visitor,
    global::Global,
    mmtk::util::conversions::raw_align_up,
    object::{AllocationHooks, ClassId, GcObject, builtin_class_ids, class_header_word},
};
use rand::Rng;

use crate::runtime::MutationContext;

use crate::runtime::objectmodel::ClassTagged;

pub const DIGIT_BIT: usize = 64;
pub const DIGIT_MASK: u64 = u64::MAX;
pub const DIGIT_BIT_SHIFT_COUNT: u32 = 6;

const BASE: WideDigit = Digit::MAX as WideDigit + 1;

pub type WideDigit = u128;
pub type Digit = u64;

extern "C" fn compute_bigint_size(object: GcObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    let bigint = unsafe { object.to_address().as_ref::<BigInt<'static>>() };
    let raw = bigint.num_words() * size_of::<Digit>() + size_of::<BigInt>();
    raw_align_up(raw, align_of::<BigInt>())
}

extern "C" fn trace_bigint(_object: GcObject, _visitor: &mut Visitor<'_>) {}

extern "C" fn process_weak_bigint(
    _object: GcObject,
    _weak_processor: &mut crate::heap::WeakProcessor,
) {
}
pub type SignedDigit = i64;
pub type SignedWideDigit = i128;

mod arith;
mod convert;
mod format;

pub use format::{Base, NumberToStringOptions};
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

impl<'gc> BigInt<'gc> {
    /// Allocates a bigint from little-endian machine words.
    ///
    /// # Panics
    ///
    /// Panics if `num_words` exceeds `u32::MAX`.
    pub fn new<const NORMALIZE: bool>(
        mc: MutationContext<'gc>,
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

            let bigint = mc.allocate_layout::<Self>(layout, bigint_header_word(), Space::Default);

            bigint.as_mut_ptr().write(BigInt {
                count: words.len() as u32,
                negative,
                pad: [0; 3],
                words: [],
                _phantom: PhantomData,
            });

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
        mc: MutationContext<'gc>,
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

            let bigint = mc.allocate_layout::<Self>(layout, bigint_header_word(), Space::Default);

            bigint.as_mut_ptr().write(BigInt {
                count: count as u32,
                negative,
                pad: [0; 3],
                words: [],
                _phantom: PhantomData,
            });

            let bigint_data_ptr = (*bigint.as_mut_ptr()).words.as_mut_ptr();
            bigint_data_ptr.write_bytes(0, count);

            fill(&mut *bigint.as_mut_ptr())?;
            // Normalize by removing high zero limbs. The check is ordered so
            // that a zero count (which a caller should avoid) cannot underflow
            // `count - 1`.
            while count > 1 && bigint_data_ptr.add(count - 1).read() == 0 {
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

        let mut acc: WideDigit = 1;

        for i in 0..count {
            acc = (!self[i] as WideDigit).wrapping_add(acc);
            self[i] = acc as Digit;
            acc >>= DIGIT_BIT;
        }
    }

    pub fn dup(&self, ctx: MutationContext<'gc>, negative: bool) -> Gc<'gc, Self> {
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
    pub fn from_num_bigint(ctx: MutationContext<'gc>, num_bigint: &NumBigInt) -> Gc<'gc, Self> {
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
    class_header_word(ClassId::new(builtin_class_ids::BIGINT).expect("builtin class id is nonzero"))
}

// SAFETY: `gc` for `BigInt` upholds all trait invariants
unsafe impl<'gc> Trace for BigInt<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, _visitor: &mut Visitor<'_>) {}
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}
}

// SAFETY: `gc` for `BigInt` upholds all trait invariants
unsafe impl<'gc> ClassTagged for BigInt<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::heap::object::builtin_class_ids::BIGINT];
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
        assert!(range.start <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts(
                self.words.as_ptr().add(range.start),
                self.count() - range.start,
            )
        }
    }
}

impl<'gc> IndexMut<RangeFrom<usize>> for BigInt<'gc> {
    fn index_mut(&mut self, range: RangeFrom<usize>) -> &mut Self::Output {
        assert!(range.start <= self.count());
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts_mut(
                self.words.as_mut_ptr().add(range.start),
                self.count() - range.start,
            )
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

const fn loword(x: WideDigit) -> Digit {
    (x & DIGIT_MASK as WideDigit) as Digit
}

const fn hiword(x: WideDigit) -> Digit {
    (x >> DIGIT_BIT) as Digit
}
const fn joinwords(loword: Digit, hiword: Digit) -> WideDigit {
    ((hiword as WideDigit) << DIGIT_BIT) | (loword as WideDigit)
}
