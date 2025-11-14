//! # easy-bitfield
//!
//! An easy to use bitfield crate that allows you to define bitfield type
//! with any storage and value types you could imagine (aka implement `ToBitfield` and `FromBitfield` traits).
//!
//! # Example
//!
//! ```rust
//! use easy_bitfield::*;
//!
//! pub type X = BitField<usize, bool, 0, 1, false>;
//! pub type Y = BitField<usize, u16, { X::NEXT_BIT }, 16, false>;
//! pub type Z = BitField<usize, i32, { Y::NEXT_BIT }, 32, true>;
//!
//! let storage = AtomicBitfieldContainer::new(0);
//!
//! storage.update::<X>(true);
//! storage.update::<Y>(42);
//! storage.update::<Z>(-42);
//!
//! assert_eq!(-42, storage.read::<Z>());
//! assert_eq!(42, storage.read::<Y>());
//! assert!(storage.read::<X>());
//! ```

use atomic::Atomic;
use num::traits::*;
use std::{marker::PhantomData, sync::atomic::Ordering};

/// A bitfield descriptor type. Define an alias to this structure in order to use it properly.
///
/// # Parameters
/// - `S`: Storage container for bitfield, must be larger or equal to `SIZE`.
/// - `T`: Type to store in a bitfield, must implement [`ToBitfield`](ToBitfield) and
///   [`FromBitfield`](FromBitfield) traits, this allows us to also encode booleans and enums
///   easily in bitfield.
/// - `POSITION`: position of bitfield in storage container, must not be out of bounds.
/// - `SIZE`: Size of the bitfield, note that if T is u32 and we set size to 16 then only
/// 16 bits of the 32-bit value will be stored.
/// - `SIGN_EXTEND`: Whether or not to do sign-extension on decode operation.
///
/// # Example usage
///
/// ```rust
/// use easy_bitfield::*;
/// // simple boolean bitfield at position 0
/// pub type MyBitfield = BitField<usize, bool, 0, 1, false>;
/// // simple 17-bit bitfield that is encoded/decoded from u32, located at second bit
/// pub type OtherBitfield = BitField<usize, u32, { MyBitfield::NEXT_BIT }, 17, false>;
///
/// ```
pub struct BitField<S, T, const POSITION: usize, const SIZE: usize, const SIGN_EXTEND: bool>(
    PhantomData<(S, T)>,
);

impl<S, T, const POSITION: usize, const SIZE: usize, const SIGN_EXTEND: bool> BitFieldTrait<S>
    for BitField<S, T, POSITION, SIZE, SIGN_EXTEND>
where
    S: ToPrimitive + FromPrimitive + One + PrimInt,
    T: FromBitfield<S> + ToBitfield<S> + PartialEq + Eq + Copy,
{
    type Type = T;

    const NEXT_BIT: usize = POSITION + SIZE;

    fn mask() -> S {
        S::from_usize((1 << SIZE) - 1).unwrap()
    }

    fn mask_in_place() -> S {
        S::from_usize(((1 << SIZE) - 1) << POSITION).unwrap()
    }

    fn shift() -> usize {
        POSITION
    }

    fn bitsize() -> usize {
        SIZE
    }

    fn encode(value: T) -> S {
        assert!(Self::is_valid(value));
        Self::encode_unchecked(value)
    }

    fn decode(value: S) -> T {
        if SIGN_EXTEND {
            let u = value.to_u64().unwrap();

            let res = ((u << (64 - Self::NEXT_BIT)) as i64) >> (64 - SIZE);

            T::from_i64(res as _)
        } else {
            let u = value;

            T::from_bitfield(u.unsigned_shr(POSITION as _) & Self::mask())
        }
    }

    fn update(value: T, original: S) -> S {
        Self::encode(value) | (!Self::mask_in_place() & original)
    }

    fn is_valid(value: T) -> bool {
        Self::decode(Self::encode_unchecked(value)) == value
    }

    fn encode_unchecked(value: T) -> S {
        let u = value.to_bitfield();

        (u.bitand(Self::mask())).unsigned_shl(POSITION as _)
    }
}

pub trait ToBitfield<S>: Sized {
    fn to_bitfield(self) -> S;
    fn one() -> Self;
    fn zero() -> Self;
}

pub trait FromBitfield<S>: Sized {
    fn from_bitfield(value: S) -> Self;
    fn from_i64(value: i64) -> Self;
}

impl<S: One + Zero> ToBitfield<S> for bool {
    fn to_bitfield(self) -> S {
        if self { S::one() } else { S::zero() }
    }

    fn one() -> Self {
        true
    }

    fn zero() -> Self {
        false
    }
}

impl<S: One + Zero + PartialEq + Eq> FromBitfield<S> for bool {
    fn from_bitfield(value: S) -> Self {
        value != S::zero()
    }

    fn from_i64(value: i64) -> Self {
        value != 0
    }
}

macro_rules! impl_tofrom_bitfield {
    ($($t:ty)*) => {
        $(
            impl<S: NumCast + One + Zero + ToPrimitive + FromPrimitive> ToBitfield<S> for $t {
                fn to_bitfield(self) -> S {
                    <S as NumCast>::from(self).unwrap()
                }

                fn one() -> Self {
                    1
                }

                fn zero() -> Self {
                    0
                }
            }

            impl<S: One + Zero + ToPrimitive + FromPrimitive> FromBitfield<S> for $t {
                fn from_bitfield(value: S) -> Self {
                    <$t as NumCast>::from(value).unwrap()
                }

                fn from_i64(value: i64) -> Self {
                    value as _
                }
            }
        )*
    };
}

impl_tofrom_bitfield!(u8 u16 u32 u64 usize);
macro_rules! impl_tofrom_bitfield_signed {
    ($($t:ty => $unsigned:ty)*) => {
        $(
            impl<S: NumCast + One + Zero + ToPrimitive + FromPrimitive> ToBitfield<S> for $t {
                fn to_bitfield(self) -> S {
                    <S as NumCast>::from(self as $unsigned).unwrap()
                }

                fn one() -> Self {
                    1
                }

                fn zero() -> Self {
                    0
                }
            }

            impl<S: One + Zero + ToPrimitive + FromPrimitive> FromBitfield<S> for $t {
                fn from_bitfield(value: S) -> Self {
                    <$t as NumCast>::from(value).unwrap()
                }

                fn from_i64(value: i64) -> Self {
                    value as _
                }
            }
        )*
    };
}

impl_tofrom_bitfield_signed!(i8 => u8 i16 => u16 i32 => u32 i64 => u64 isize => usize);

/// Bitfield trait implementation, this is used to allow easy generic storage/value usage.
///
/// Without it it's hard to get working storage/value generics.
pub trait BitFieldTrait<S>
where
    S: PrimInt + ToPrimitive + FromPrimitive + One,
{
    /// Type we encode/decode in bitfield
    type Type: Copy + FromBitfield<S> + ToBitfield<S> + PartialEq + Eq;

    /// Next bit after current bitfield, use this to not waste time calculating
    /// what the next bitfield position should be
    const NEXT_BIT: usize;
    /// Mask of this bitfield
    fn mask() -> S;
    /// In place mask of this bitfield
    fn mask_in_place() -> S;
    /// Shift of this bitfield
    fn shift() -> usize;
    /// Bitsize of this bitifield
    fn bitsize() -> usize;
    /// Encodes `value` as bitfield
    ///
    /// # Panics
    ///
    /// Panics if `value` is not valid
    fn encode(value: Self::Type) -> S;
    /// Decodes bitfield into [`Type`](Self::Type)
    fn decode(storage: S) -> Self::Type;
    /// Updates the value in storage `original`, returns updated storage
    fn update(value: Self::Type, original: S) -> S;
    /// Checks if value is valid to encode.
    fn is_valid(value: Self::Type) -> bool;
    /// Unchecked encode of `value`, all bits that can't be encoded will be stripped down.
    fn encode_unchecked(value: Self::Type) -> S;
}

/// Atomic container for bitfield storage.
pub struct AtomicBitfieldContainer<T: bytemuck::NoUninit>(Atomic<T>);

impl<T: bytemuck::NoUninit + FromPrimitive + ToPrimitive + PrimInt> AtomicBitfieldContainer<T> {
    /// Creates the new bitfield container.
    pub fn new(value: T) -> Self {
        Self(Atomic::new(value))
    }

    /// Load underlying value of container ignoring atomic access
    pub fn load_ignore_race(&self) -> T {
        unsafe {
            let ptr = &self.0 as *const Atomic<T> as *const T;
            ptr.read()
        }
    }

    /// Load value of container with `order` ordering.
    pub fn load(&self, order: Ordering) -> T {
        self.0.load(order)
    }

    /// Store new value to container with `order` ordering.
    pub fn store(&self, value: T, order: Ordering) {
        self.0.store(value, order)
    }

    /// Perform CAS, equivalent to [`compare_exchange_weak`](std::sync::atomic::AtomicUsize::compare_exchange_weak)
    /// in any std type.
    pub fn compare_exchange_weak(
        &self,
        current: T,
        new: T,
        success: Ordering,
        failure: Ordering,
    ) -> Result<T, T> {
        self.0.compare_exchange_weak(current, new, success, failure)
    }

    /// Use bitfield `B` to decode the value in container and return the value.
    pub fn read<B: BitFieldTrait<T>>(&self) -> B::Type {
        B::decode(self.load(Ordering::Relaxed))
    }

    /// Update container according to bitfield `B`, performs CAS under the hood.
    pub fn update<B: BitFieldTrait<T>>(&self, value: B::Type) {
        let mut old_field = self.0.load(Ordering::Relaxed);
        let mut new_field;
        loop {
            new_field = B::update(value, old_field);
            match self.0.compare_exchange_weak(
                old_field,
                new_field,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    break;
                }
                Err(x) => {
                    old_field = x;
                }
            }
        }
    }

    /// Update container according to bitfield `B`. Does atomic store under the hood
    pub fn update_synchronized<B: BitFieldTrait<T>>(&self, value: B::Type) {
        self.0.store(
            B::update(value, self.0.load(Ordering::Relaxed)),
            Ordering::Relaxed,
        );
    }

    /// Conditional update of storage container according to bitfield `B`.
    ///
    /// This is equivalent of CAS for bitfields.
    pub fn update_conditional<B: BitFieldTrait<T>>(
        &self,
        value_to_be_set: B::Type,
        conditional_old_value: B::Type,
    ) -> B::Type {
        let mut old_field = self.0.load(Ordering::Relaxed);

        loop {
            let old_value = B::decode(old_field);
            if old_value != conditional_old_value {
                return old_value;
            }

            let new_tags = B::update(value_to_be_set, old_field);

            match self.0.compare_exchange_weak(
                old_field,
                new_tags,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    return conditional_old_value;
                }
                Err(x) => {
                    old_field = x;
                }
            }

            // old_tags was updated to its current value, try again
        }
    }
}

macro_rules! atomic_ops_common {
    ($($t: ty)*) => {
        $(impl AtomicBitfieldContainer<$t> {
            /// `fetch_or` operation for bitfield `B`
            pub fn fetch_or<B: BitFieldTrait<$t>>(&self, value: B::Type) {
                self.0.fetch_or(B::encode(value), Ordering::Relaxed);
            }
            /// `fetch_and` operation for bitfield `B`
            pub fn fetch_and<B: BitFieldTrait<$t>>(&self, value: B::Type) {
                self.0.fetch_and(B::encode(value), Ordering::Relaxed);
            }
            /// `fetch_xor` operation for bitfield `B`
            pub fn fetch_xor<B: BitFieldTrait<$t>>(&self, value: B::Type) {
                self.0.fetch_xor(B::encode(value), Ordering::Relaxed);
            }

            /// Try to acquire bitfield `B`, can be used to implement atomic locks.
            pub fn try_acquire<B: BitFieldTrait<$t>>(&self) -> bool {
                let mask = B::encode(B::Type::one());
                let old_field = self.0.fetch_or(mask, Ordering::Relaxed);
                B::decode(old_field) == B::Type::zero()
            }

            /// Try to release bitfield `B`, can be used to implement atomic locks.
            pub fn try_release<B: BitFieldTrait<$t>>(&self) -> bool {
                let mask = !B::encode(B::Type::one());
                let old_field = self.0.fetch_and(mask, Ordering::Relaxed);
                B::decode(old_field) != B::Type::zero()
            }

            /// Update storage container according to bitfield `B` based on boolean value,
            /// `B::Type` must implement `From<bool>`.
            pub fn update_bool<B: BitFieldTrait<$t>>(&self, value: bool, order: Ordering)
            where
                B::Type: From<bool>,
            {
                if value {
                    self.0.fetch_or(B::encode(B::Type::from(true)), order);
                } else {
                    self.0.fetch_and(!B::encode(B::Type::from(false)), order);
                }
            }
        })*
    };
}

atomic_ops_common!(u8 u16 u32 u64 i8 i16 i32 i64 usize isize);

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    type LockBit = BitField<usize, bool, 0, 1, false>;
    type DataBits = BitField<usize, u32, { LockBit::NEXT_BIT }, 32, false>;

    #[test]
    fn test_acquire_release() {
        let container = Arc::new(AtomicBitfieldContainer::new(0usize));
        let thread = {
            let container = container.clone();

            std::thread::spawn(move || {
                while !container.try_acquire::<LockBit>() {}
                container.update::<DataBits>(42);
                assert!(container.try_release::<LockBit>());
            })
        };

        loop {
            while !container.try_acquire::<LockBit>() {}
            if container.read::<DataBits>() != 0 {
                assert_eq!(container.read::<DataBits>(), 42);
                assert!(container.try_release::<LockBit>());
                break;
            }

            assert!(container.try_release::<LockBit>());
        }

        thread.join().unwrap();
    }

    type A = BitField<u32, bool, 0, 1, false>;
    type B = BitField<u32, i8, { A::NEXT_BIT }, 4, true>;
    type C = BitField<u32, i16, { B::NEXT_BIT }, 16, true>;

    #[test]
    fn test_encode_decoe() {
        let mut storage;

        storage = A::encode(true);
        assert!(A::decode(storage));
        storage = B::update(-1, storage);
        assert_eq!(B::decode(storage), -1);
        storage = B::update(2, storage);
        assert_eq!(B::decode(storage), 2);
        assert!(A::decode(storage));
        assert_eq!(C::decode(storage), 0);
        assert!(B::is_valid(7));
        assert!(!B::is_valid(8));
        assert!(B::is_valid(-8));
        assert!(!B::is_valid(-9));
    }
}
