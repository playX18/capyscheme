use std::{
    cell::Cell,
    mem::{MaybeUninit, align_of, size_of},
    ops::{Deref, Index},
    sync::atomic::{AtomicU16, Ordering},
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

pub const DIGIT_BIT: usize = 32;
pub const DIGIT_MASK: u64 = u32::MAX as u64;
pub const DIGIT_BIT_SHIFT_COUNT: u32 = 5;

/// A digit in a bignum.
///
/// Rationale for using `u32` instead of `u64`:
///
/// - `u32` is more compact, which can save us memory space sometimes./
/// - Our inline integers are 32-bit so it makes sense to use 32-bit
/// digits for bignums as well.
pub type Digit = u32;

#[derive(Collect)]
#[collect(no_drop)]
pub struct Bignum<'gc> {
    sign: Cell<bool>,
    count: Cell<u32>,
    offset: usize,
    digits: Gc<'gc, Array<Digit>>,
}

/*
#[cold]
fn register_bignum_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: align_of::<u32>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let bignum = unsafe { object.as_address().as_ref::<Bignum>() };
                    bignum.count.get() as usize * size_of::<u32>()
                }),
                instance_size: size_of::<Bignum>(),
                trace: TraceCallback::TraceObject(|object, tracer| {
                    let bignum = unsafe { object.as_address().as_mut_ref::<Bignum>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object.as_object_unchecked())) };
                    bignum.trace(&mut visitor);
                }),
            },
        },
    )
}
*/
unsafe impl<'gc> Tagged for Bignum<'gc> {
    const TC8: TypeCode8 = TypeCode8::NUMBER; // Assuming BIGNUM is a variant in TypeCode8
    // Add TC16 if applicable, similar to Vector
    const TC16: &'static [TypeCode16] = &[TypeCode16::BIG];
    const ONLY_TC16: bool = true;
}

impl<'gc> Bignum<'gc> {
    fn new(ctx: Context<'gc>, ndigits: usize, sign: bool) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                sign: Cell::new(sign),
                count: Cell::new(ndigits as u32),
                offset: 0,
                digits: Array::with(&ctx, ndigits, 0),
            },
        )
    }

    pub fn from_digits(ctx: Context<'gc>, mut digits: &[Digit], sign: bool) -> Gc<'gc, Self> {
        while digits.len() > 0 && digits[digits.len() - 1] == 0 {
            digits = &digits[..digits.len() - 1];
        }
        Gc::new(
            &ctx,
            Self {
                sign: Cell::new(sign),
                count: Cell::new(digits.len() as u32),
                offset: 0,
                digits: Array::new(&ctx, digits.iter().copied()),
            },
        )
    }

    pub fn sign(&self) -> bool {
        self.sign.get()
    }

    pub fn count(&self) -> usize {
        self.count.get() as usize
    }

    pub fn is_normalized(&self) -> bool {
        self.count() == 0 || self[self.count() - 1] != 0
    }

    pub fn subsection(&self, ctx: Context<'gc>, start: usize, end: usize) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= self.count() as usize);

        Gc::new(
            &ctx,
            Self {
                sign: Cell::new(self.sign()),
                count: Cell::new((end - start) as u32),
                offset: self.offset + start,
                digits: self.digits,
            },
        )
    }

    pub fn clone(&self, ctx: Context<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                sign: Cell::new(self.sign()),
                count: Cell::new(self.count() as _),
                offset: self.offset,
                digits: Array::new(&ctx, self.digits.iter().copied()),
            },
        )
    }

    fn digits_mut(&self) -> &mut [Digit] {
        unsafe {
            let digits: *const Digit = (**self.digits).as_ptr();

            std::slice::from_raw_parts_mut((digits as *mut Digit).add(self.offset), self.count())
        }
    }
    fn flip2sc(&self) {
        let count = self.count();
        let mut acc = 1u64;

        for i in 0..count {
            acc = ((!self.digits_mut()[i]) as u64).wrapping_add(acc);
            self.digits_mut()[i] = acc as Digit;
            acc >= 32;
        }
    }

    fn normalize(&self) -> usize {
        let count = self.count();

        if count != 0 {
            let mut index = count - 1;

            while self[index] == 0 {
                if index == 0 {
                    self.set_zero();
                    return 0;
                } else {
                    index -= 1;
                }
            }

            self.count.set(index as u32 + 1);
            return index + 1;
        }

        self.set_zero();
        0
    }

    fn to_integer(&self, ctx: Context<'gc>) -> Value<'gc> {
        if self.count() == 0 {
            return Value::new(0i32);
        }

        if self.count() == 1 {
            let mut n = self[0] as u64 as i64;
            if !self.sign() {
                n = n.wrapping_neg();
            }

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Value::new(n as i32);
            }
        }

        self.clone(ctx).into()
    }

    fn demote(self: Gc<'gc, Self>, ctx: Context<'gc>) -> Value<'gc> {
        if self.count() == 0 {
            return Value::new(0i32);
        }

        if self.count() == 1 {
            let mut n = self[0] as u64 as i64;
            if !self.sign() {
                n = n.wrapping_neg();
            }

            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Value::new(n as i32);
            }
        }

        self.into()
    }

    fn set_zero(&self) {
        self.count.set(0);
        self.sign.set(false);
    }

    fn lognot(&self, ans: Gc<'gc, Self>) {
        let self_count = self.count();
        let ans_count = ans.count();

        assert!(ans_count >= self_count);

        for i in 0..self_count {
            if i < self_count {
                ans.digits_mut()[i] = !self[i];
            } else {
                ans.digits_mut()[i] = u32::MAX;
            }
        }
        ans.normalize();
    }

    fn logand(&self, rhs: &Self, ans: Gc<'gc, Self>, lhs2sc: bool, rhs2sc: bool) {
        let ans_count = ans.count();
        let lhs_count = self.count();
        let rhs_count = rhs.count();

        let lhs = self;

        assert!(ans_count >= lhs_count);

        for i in 0..ans_count {
            let bit1 = if (i < lhs_count) {
                lhs[i]
            } else if lhs2sc {
                u32::MAX
            } else {
                0
            };
            let bit2 = if (i < rhs_count) {
                rhs[i]
            } else if rhs2sc {
                u32::MAX
            } else {
                0
            };

            ans.digits_mut()[i] = bit1 & bit2;
        }

        ans.normalize();
    }

    fn logior(&self, rhs: &Self, ans: Gc<'gc, Self>, lhs2sc: bool, rhs2sc: bool) {
        let ans_count = ans.count();
        let lhs_count = self.count();
        let rhs_count = rhs.count();

        let lhs = self;

        assert!(ans_count >= lhs_count);

        for i in 0..ans_count {
            let bit1 = if (i < lhs_count) {
                lhs[i]
            } else if lhs2sc {
                u32::MAX
            } else {
                0
            };
            let bit2 = if (i < rhs_count) {
                rhs[i]
            } else if rhs2sc {
                u32::MAX
            } else {
                0
            };

            ans.digits_mut()[i] = bit1 | bit2;
        }

        ans.normalize();
    }

    fn logxor(&self, rhs: &Self, ans: Gc<'gc, Self>, lhs2sc: bool, rhs2sc: bool) {
        let ans_count = ans.count();
        let lhs_count = self.count();
        let rhs_count = rhs.count();

        let lhs = self;

        assert!(ans_count >= lhs_count);

        for i in 0..ans_count {
            let bit1 = if (i < lhs_count) {
                lhs[i]
            } else if lhs2sc {
                u32::MAX
            } else {
                0
            };
            let bit2 = if (i < rhs_count) {
                rhs[i]
            } else if rhs2sc {
                u32::MAX
            } else {
                0
            };

            ans.digits_mut()[i] = bit1 ^ bit2;
        }

        ans.normalize();
    }

    fn add(&self, rhs: &Self, ans: Gc<'gc, Self>) -> bool {
        let lhs_count = self.count();
        let rhs_count = rhs.count();
        let ans_count = ans.count();

        let mut acc = 0u64;
        for i in 0..ans_count {
            if i < lhs_count {
                acc = acc.wrapping_add(self[i] as u64);
            }

            if i < rhs_count {
                acc = acc.wrapping_add(rhs[i] as u64);
            }

            ans.digits_mut()[i] = acc as Digit;
            acc >>= 32;
        }

        ans.normalize();
        acc != 0
    }

    fn sub(&self, rhs: &Self, ans: Gc<'gc, Self>) -> bool {
        let lhs_count = self.count();
        let rhs_count = rhs.count();
        let ans_count = ans.count();

        let mut acc: i64 = 0;
        for i in 0..ans_count {
            if i < lhs_count {
                acc = acc.wrapping_add(self[i] as i64);
            }
            if i < rhs_count {
                acc = acc.wrapping_sub(rhs[i] as i64);
            }
            ans.digits_mut()[i] = acc as Digit;
            acc >>= 32;
        }
        if acc >= 0 {
            ans.normalize();
            false
        } else {
            true // underflow
        }
    }

    fn mul(&self, rhs: &Self, ans: Gc<'gc, Self>) {
        let lhs_count = self.count();
        let rhs_count = rhs.count();
        let ans_count = ans.count();

        assert!(ans.digits.as_ptr() != self.digits.as_ptr());
        assert!(ans.digits.as_ptr() != rhs.digits.as_ptr());
        assert!(ans_count >= lhs_count + rhs_count);

        for d in ans.digits_mut().iter_mut() {
            *d = 0;
        }

        for i in 0..lhs_count {
            let mut acc: u64 = 0;
            for j in 0..rhs_count {
                let idx = i + j;
                acc = (self[i] as u64)
                    .wrapping_mul(rhs[j] as u64)
                    .wrapping_add(acc)
                    .wrapping_add(ans.digits_mut()[idx] as u64);
                ans.digits_mut()[idx] = acc as Digit;
                acc >>= 32;
            }
            ans.digits_mut()[i + rhs_count] = acc as Digit;
        }
        ans.count.set((rhs_count + lhs_count) as u32);
        ans.normalize();
    }

    fn mul_add_digit(&self, rhs: Digit, addend: Digit, ans: Gc<'gc, Self>) {
        let lhs_count = self.count();
        assert!(ans.count() > lhs_count);

        let mut acc = addend as u64;
        for i in 0..lhs_count {
            acc = (self[i] as u64).wrapping_mul(rhs as u64).wrapping_add(acc);
            ans.digits_mut()[i] = acc as Digit;
            acc >>= 32;
        }
        ans.digits_mut()[lhs_count] = acc as Digit;
        ans.count.set(lhs_count as u32 + 1);
        ans.normalize();
    }

    fn div_digit(&self, denominator: Digit, ans: Gc<'gc, Self>) -> Digit {
        let numerator_count = self.count();
        assert!(ans.count() >= numerator_count);

        let mut remainder: u64 = 0;
        for i in (0..numerator_count).rev() {
            remainder = (remainder << 32) + self[i] as u64;
            ans.digits_mut()[i] = (remainder / denominator as u64) as Digit;
            remainder %= denominator as u64;
        }
        ans.normalize();
        remainder as Digit
    }

    fn remainder_digit(&self, denominator: Digit) -> Digit {
        let numerator_count = self.count();
        let mut remainder: u64 = 0;
        for i in (0..numerator_count).rev() {
            remainder = ((remainder << 32) + self[i] as u64) % denominator as u64;
        }
        remainder as Digit
    }

    fn raw_cmp(&self, other: &Self) -> std::cmp::Ordering {
        let lhs_count = self.count();
        let rhs_count = other.count();

        if lhs_count > rhs_count {
            return std::cmp::Ordering::Greater;
        } else if lhs_count < rhs_count {
            return std::cmp::Ordering::Less;
        }

        for i in (0..lhs_count).rev() {
            if self[i] > other[i] {
                return std::cmp::Ordering::Greater;
            } else if self[i] < other[i] {
                return std::cmp::Ordering::Less;
            }
        }

        std::cmp::Ordering::Equal
    }
    fn shift_right_digit(&self, mut unit: usize) {
        let bn_count = self.count();

        if bn_count > unit {
            for i in 0..bn_count - unit {
                self.digits_mut()[i] = self.digits_mut()[i + unit];
            }
        } else {
            unit = bn_count;
        }

        for i in (bn_count - unit)..bn_count {
            self.digits_mut()[i] = 0;
        }
    }

    fn shift_right(&self, mut shift: usize) {
        let bit_shift = shift & (DIGIT_BIT - 1);
        shift >>= DIGIT_BIT_SHIFT_COUNT;

        if shift != 0 {
            self.shift_right_digit(shift);
        }

        if bit_shift != 0 {
            let count = self.count() - shift;
            let mut bits = 0;

            for i in (0..=(count - 1)).rev() {
                let bits2 = self[i] << (DIGIT_BIT - bit_shift);
                self.digits_mut()[i] = (self[i] >> bit_shift) | bits;
                bits = bits2;
            }
        }
    }

    
}

impl<'gc> Index<usize> for Bignum<'gc> {
    type Output = Digit;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.count() as usize);
        &self.digits[self.offset + index]
    }
}

impl<'gc> Deref for Bignum<'gc> {
    type Target = [Digit];

    fn deref(&self) -> &Self::Target {
        let digits: &[Digit] = &(**self.digits)[self.offset..self.count()];
        digits
    }
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
