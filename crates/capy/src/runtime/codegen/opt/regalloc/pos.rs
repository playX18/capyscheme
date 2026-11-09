//! Position of operands in the instruction stream
//!
//! Each instruction gets four points:
//! - `Pre` (`4g`): parallel move gap before the instruction at the block start
//! - `Early` (`4g+1`): operands are read; a slow path's clobber starts
//! - `Late` (`4g+2`): results are written; calls clobber registers
//! - `Post` (`4g+3`): parallel-move gap after the instruction
//!

/// Positions per instruction.
pub(crate) const POINTS: u32 = 4;

/// The gap before instruction `g`.
pub(crate) fn pre(g: u32) -> u32 {
    POINTS * g
}

/// Where instruction `g` reads its operands.
pub(crate) fn early(g: u32) -> u32 {
    POINTS * g + 1
}

/// Where instruction `g` writes its results and calls clobber registers.
pub(crate) fn late(g: u32) -> u32 {
    POINTS * g + 2
}

/// The gap after instruction `g`.
pub(crate) fn post(g: u32) -> u32 {
    POINTS * g + 3
}

/// The instruction a position belongs to.
pub(crate) fn inst_of(p: u32) -> u32 {
    p / POINTS
}

/// Whether `p` is a gap, the only place a piece may change location.
pub(crate) fn is_gap(p: u32) -> bool {
    matches!(p % POINTS, 0 | 3)
}

/// The largest gap at or before `p`.
pub(crate) fn gap_floor(p: u32) -> u32 {
    if is_gap(p) { p } else { p - p % POINTS }
}

/// The smallest gap at or after `p`.
pub(crate) fn gap_ceil(p: u32) -> u32 {
    if is_gap(p) { p } else { p - p % POINTS + 3 }
}
