//! Bit-packed runtime type profiling metadata.
//!
//! Each slot (e.g. result / lhs / rhs of a binary arith op) stores a **bitset** of which
//! [`NumberTower`] kinds have been seen. New observations are merged with **bitwise OR**,
//! so a slot accumulates all observed types until something clears or resets the profile.

use std::ops::{BitOr, BitOrAssign};

/// Tag for the Scheme-style numeric tower used in profiling (see R5RS/R6RS “numerical types”).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum NumberTower {
    /// No classified observation for this position (contributes no bits when merged).
    Unknown = 0,
    Fixnum = 1,
    Flonum = 2,
    BigInt = 3,
    Complex = 4,
    Rational = 5,
}

impl NumberTower {
    /// Single-kind mask for this variant within a slot ([`TowerMask`]), or 0 for [`Unknown`].
    #[inline]
    pub const fn to_tower_mask(self) -> u8 {
        match self {
            NumberTower::Unknown => 0,
            NumberTower::Fixnum => 1 << 0,
            NumberTower::Flonum => 1 << 1,
            NumberTower::BigInt => 1 << 2,
            NumberTower::Complex => 1 << 3,
            NumberTower::Rational => 1 << 4,
        }
    }
}

/// Bitset of observed numeric tower kinds in one profile slot (lower 5 bits; upper bits reserved).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TowerMask(pub u8);

impl TowerMask {
    /// Bits used for Fixnum..=Rational; higher bits in the slot are reserved.
    pub const TOWER_BITS: u8 = 0b1_1111;

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 & Self::TOWER_BITS == 0
    }

    #[inline]
    pub const fn contains(self, k: NumberTower) -> bool {
        match k {
            NumberTower::Unknown => false,
            _ => (self.0 & k.to_tower_mask()) != 0,
        }
    }
}

/// Packed type profile, typically for one bytecode site (e.g. a binary `ArithOp`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeProfile(u32);

impl TypeProfile {
    /// One byte per slot: output, lhs, rhs (masks are [`TowerMask::TOWER_BITS`]-wide in each byte).
    const OUT_SHIFT: u32 = 0;
    const LHS_SHIFT: u32 = 8;
    const RHS_SHIFT: u32 = 16;

    const SLOT_MASK: u32 = 0xFF;

    /// Profile with no observations in any slot.
    pub const EMPTY: Self = Self(0);

    /// Alias for [`Self::EMPTY`].
    pub const UNKNOWN: Self = Self::EMPTY;

    #[inline]
    const fn pack_slot(mask: u8, shift: u32) -> u32 {
        ((mask as u32) & Self::SLOT_MASK) << shift
    }

    /// One observation of `dst = lhs op rhs`, OR-ing these kinds into the three slots.
    /// Use [`Self::merge`] (or `|`) to combine with an existing profile.
    #[inline]
    pub const fn from_binary_observation(
        output: NumberTower,
        lhs: NumberTower,
        rhs: NumberTower,
    ) -> Self {
        Self(
            Self::pack_slot(output.to_tower_mask(), Self::OUT_SHIFT)
                | Self::pack_slot(lhs.to_tower_mask(), Self::LHS_SHIFT)
                | Self::pack_slot(rhs.to_tower_mask(), Self::RHS_SHIFT),
        )
    }

    /// OR this observation into `self` (accumulate kinds per slot).
    #[inline]
    pub const fn merge_observation(
        self,
        output: NumberTower,
        lhs: NumberTower,
        rhs: NumberTower,
    ) -> Self {
        self.merge(Self::from_binary_observation(output, lhs, rhs))
    }

    /// Merge two profiles by OR-ing slot masks (union of all observations seen at this site).
    #[inline]
    pub const fn merge(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Observed result kinds for `dst` (may contain multiple bits).
    #[inline]
    pub const fn output_mask(self) -> TowerMask {
        TowerMask((self.0 >> Self::OUT_SHIFT) as u8)
    }

    /// Observed kinds for the left-hand operand.
    #[inline]
    pub const fn lhs_mask(self) -> TowerMask {
        TowerMask((self.0 >> Self::LHS_SHIFT) as u8)
    }

    /// Observed kinds for the right-hand operand.
    #[inline]
    pub const fn rhs_mask(self) -> TowerMask {
        TowerMask((self.0 >> Self::RHS_SHIFT) as u8)
    }

    /// Raw storage word (e.g. for `AtomicU32::fetch_or` side tables).
    #[inline]
    pub const fn to_bits(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn from_bits(bits: u32) -> Self {
        Self(bits)
    }

    /// True if every slot’s mask is empty.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.output_mask().is_empty() && self.lhs_mask().is_empty() && self.rhs_mask().is_empty()
    }
}

impl BitOr for TypeProfile {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        self.merge(rhs)
    }
}

impl BitOrAssign for TypeProfile {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

#[cfg(test)]
mod tests {
    use super::{NumberTower, TowerMask, TypeProfile};

    #[test]
    fn single_observation_sets_bits_per_slot() {
        let p = TypeProfile::from_binary_observation(
            NumberTower::Fixnum,
            NumberTower::Flonum,
            NumberTower::BigInt,
        );
        assert!(p.output_mask().contains(NumberTower::Fixnum));
        assert!(p.lhs_mask().contains(NumberTower::Flonum));
        assert!(p.rhs_mask().contains(NumberTower::BigInt));
    }

    #[test]
    fn merge_observation_ors_within_slots() {
        let a = TypeProfile::from_binary_observation(
            NumberTower::Fixnum,
            NumberTower::Fixnum,
            NumberTower::Fixnum,
        );
        let b = TypeProfile::from_binary_observation(
            NumberTower::Flonum,
            NumberTower::Flonum,
            NumberTower::Flonum,
        );
        let m = a.merge(b);
        let om = m.output_mask();
        assert!(om.contains(NumberTower::Fixnum) && om.contains(NumberTower::Flonum));
        assert_eq!(
            om.0 & TowerMask::TOWER_BITS,
            NumberTower::Fixnum.to_tower_mask() | NumberTower::Flonum.to_tower_mask()
        );
    }

    #[test]
    fn bitor_merges_profiles() {
        let a = TypeProfile::from_binary_observation(
            NumberTower::Rational,
            NumberTower::Unknown,
            NumberTower::Complex,
        );
        let b = TypeProfile::from_binary_observation(
            NumberTower::Unknown,
            NumberTower::BigInt,
            NumberTower::Unknown,
        );
        let u = a | b;
        assert!(u.output_mask().contains(NumberTower::Rational));
        assert!(u.lhs_mask().contains(NumberTower::BigInt));
        assert!(u.rhs_mask().contains(NumberTower::Complex));
    }

    #[test]
    fn bitor_assign_accumulates() {
        let mut p = TypeProfile::from_binary_observation(
            NumberTower::Fixnum,
            NumberTower::Unknown,
            NumberTower::Unknown,
        );
        p |= TypeProfile::from_binary_observation(
            NumberTower::Flonum,
            NumberTower::Unknown,
            NumberTower::Unknown,
        );
        assert!(p.output_mask().contains(NumberTower::Fixnum));
        assert!(p.output_mask().contains(NumberTower::Flonum));
    }

    #[test]
    fn round_trip_bits() {
        let p = TypeProfile::from_binary_observation(
            NumberTower::Complex,
            NumberTower::Rational,
            NumberTower::Unknown,
        );
        assert_eq!(TypeProfile::from_bits(p.to_bits()), p);
    }

    #[test]
    fn empty_is_zero() {
        assert_eq!(TypeProfile::EMPTY.to_bits(), 0);
        assert!(TypeProfile::EMPTY.is_empty());
    }
}
