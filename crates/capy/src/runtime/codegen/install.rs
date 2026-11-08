//! Install-state word per code block.
//!
//! The word records which tier owns the block's current code, the install
//! generation that produced it, and the recompile count. A generation makes
//! a late transition from an old unit a no-op instead of a state overwrite:
//! a watchpoint fire that retires generation `n` must never clobber the
//! `Opt` state of generation `n + 1`.

use std::sync::atomic::{AtomicU64, Ordering};

/// Recompiles after which a block stays [`Phase::Failed`] and runs baseline.
pub(crate) const MAX_RECOMPILES: u32 = 3;

const PHASE_BITS: u32 = 8;
const GEN_SHIFT: u32 = PHASE_BITS;
const GEN_BITS: u32 = 16;
const RECOMPILE_SHIFT: u32 = GEN_SHIFT + GEN_BITS;
const RECOMPILE_BITS: u32 = 8;
const PREV_PHASE_SHIFT: u32 = RECOMPILE_SHIFT + RECOMPILE_BITS;
const PREV_GEN_SHIFT: u32 = PREV_PHASE_SHIFT + PHASE_BITS;

const PHASE_MASK: u64 = (1 << PHASE_BITS) - 1;
const GEN_MASK: u64 = (1 << GEN_BITS) - 1;
const RECOMPILE_MASK: u64 = (1 << RECOMPILE_BITS) - 1;
const PREV_GEN_MASK: u64 = (1 << GEN_BITS) - 1;

/// A block's install phase.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Phase {
    /// No compiled code and no compile pending.
    NoCode = 0,
    /// A baseline compile is queued or running.
    BaselineQueued = 1,
    /// A baseline unit is installed.
    Baseline = 2,
    /// A Tier 2 compile is queued or running.
    OptQueued = 3,
    /// A Tier 2 unit is installed and current.
    Opt = 4,
    /// The current Tier 2 unit was invalidated; activations may remain.
    NotEntrant = 5,
    /// Tier 2 gave up on this block.
    Failed = 6,
}

impl Phase {
    fn from_word(word: u64) -> Self {
        match word & PHASE_MASK {
            1 => Self::BaselineQueued,
            2 => Self::Baseline,
            3 => Self::OptQueued,
            4 => Self::Opt,
            5 => Self::NotEntrant,
            6 => Self::Failed,
            _ => Self::NoCode,
        }
    }

    fn is_queued(self) -> bool {
        matches!(self, Self::BaselineQueued | Self::OptQueued)
    }
}

/// An install generation. Zero means no install yet.
pub(crate) type Generation = u16;

fn next_generation(current: Generation) -> Generation {
    // Generation 0 is reserved for "no install". A 16-bit wrap skips it.
    match current.wrapping_add(1) {
        0 => 1,
        next => next,
    }
}

#[inline]
fn encode(
    phase: Phase,
    generation: Generation,
    recompiles: u32,
    prev_phase: Phase,
    prev_gen: Generation,
) -> u64 {
    (phase as u64)
        | ((generation as u64) << GEN_SHIFT)
        | (((recompiles as u64) & RECOMPILE_MASK) << RECOMPILE_SHIFT)
        | ((prev_phase as u64) << PREV_PHASE_SHIFT)
        | (((prev_gen as u64) & PREV_GEN_MASK) << PREV_GEN_SHIFT)
}

#[derive(Clone, Copy)]
struct Decoded {
    phase: Phase,
    generation: Generation,
    recompiles: u32,
    prev_phase: Phase,
    prev_gen: Generation,
}

fn decode(word: u64) -> Decoded {
    Decoded {
        phase: Phase::from_word(word),
        generation: ((word >> GEN_SHIFT) & GEN_MASK) as Generation,
        recompiles: ((word >> RECOMPILE_SHIFT) & RECOMPILE_MASK) as u32,
        prev_phase: Phase::from_word(word >> PREV_PHASE_SHIFT),
        prev_gen: ((word >> PREV_GEN_SHIFT) & PREV_GEN_MASK) as Generation,
    }
}

/// The block's install state
pub(crate) struct InstallState(AtomicU64);

impl Default for InstallState {
    fn default() -> Self {
        Self::new()
    }
}

impl InstallState {
    pub(crate) const fn new() -> Self {
        Self(AtomicU64::new(0))
    }

    pub(crate) fn phase(&self) -> Phase {
        Phase::from_word(self.0.load(Ordering::Acquire))
    }

    pub(crate) fn generation(&self) -> Generation {
        decode(self.0.load(Ordering::Acquire)).generation
    }

    pub(crate) fn recompiles(&self) -> u32 {
        decode(self.0.load(Ordering::Acquire)).recompiles
    }

    /// Claim the one baseline compile of a block. Only a block with no code
    /// can take it; the returned generation is what [`commit`](Self::commit)
    /// must publish.
    pub(crate) fn claim_baseline(&self) -> Option<Generation> {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if current.phase != Phase::NoCode {
                return None;
            }
            let generation = next_generation(current.generation);
            let next = encode(
                Phase::BaselineQueued,
                generation,
                current.recompiles,
                Phase::NoCode,
                current.generation,
            );
            match self
                .0
                .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return Some(generation),
                Err(actual) => word = actual,
            }
        }
    }

    /// Claim a Tier 2 compile. Only a block with baseline code, or one whose
    /// previous optimized unit was retired, can take it.
    pub(crate) fn claim_opt(&self) -> Option<Generation> {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if !matches!(current.phase, Phase::Baseline | Phase::NotEntrant) {
                return None;
            }
            let generation = next_generation(current.generation);
            let next = encode(
                Phase::OptQueued,
                generation,
                current.recompiles,
                current.phase,
                current.generation,
            );
            match self
                .0
                .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return Some(generation),
                Err(actual) => word = actual,
            }
        }
    }

    /// Publish a completed install. Fails when the generation is stale, which
    /// is how a retire that raced the install wins.
    pub(crate) fn commit(&self, generation: Generation) -> bool {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if current.generation != generation {
                return false;
            }
            let phase = match current.phase {
                Phase::BaselineQueued => Phase::Baseline,
                Phase::OptQueued => Phase::Opt,
                _ => return false,
            };
            let next = encode(phase, generation, current.recompiles, Phase::NoCode, 0);
            match self
                .0
                .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return true,
                Err(actual) => word = actual,
            }
        }
    }

    /// Release a claim that will not run, restoring the phase the claim
    /// replaced.
    pub(crate) fn abort(&self, generation: Generation) -> bool {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if current.phase.is_queued() && current.generation == generation {
                let next = encode(
                    current.prev_phase,
                    current.prev_gen,
                    current.recompiles,
                    Phase::NoCode,
                    0,
                );
                match self
                    .0
                    .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
                {
                    Ok(_) => return true,
                    Err(actual) => word = actual,
                }
            } else {
                return false;
            }
        }
    }

    /// Retire the installed Tier 2 unit of `generation`. A transition from a
    /// newer generation is a no-op, so a late fire cannot touch a recompile.
    pub(crate) fn retire(&self, generation: Generation) -> bool {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if current.phase != Phase::Opt || current.generation != generation {
                return false;
            }
            let next = encode(
                Phase::NotEntrant,
                generation,
                current.recompiles,
                Phase::NoCode,
                0,
            );
            match self
                .0
                .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return true,
                Err(actual) => word = actual,
            }
        }
    }

    /// Count a failed build, or a deopt-budget invalidation, against the
    /// recompile budget. Returns the resulting phase.
    pub(crate) fn fail(&self, generation: Generation) -> Phase {
        let mut word = self.0.load(Ordering::Acquire);
        loop {
            let current = decode(word);
            if current.generation != generation {
                return current.phase;
            }
            if !matches!(
                current.phase,
                Phase::BaselineQueued | Phase::OptQueued | Phase::Opt | Phase::NotEntrant
            ) {
                return current.phase;
            }

            let recompiles = current.recompiles;
            let (phase, stored) = if recompiles >= MAX_RECOMPILES {
                (Phase::Failed, recompiles)
            } else {
                (Phase::NotEntrant, recompiles + 1)
            };
            let next = encode(phase, generation, stored, Phase::NoCode, 0);
            match self
                .0
                .compare_exchange_weak(word, next, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => return phase,
                Err(actual) => word = actual,
            }
        }
    }

    /// Count one recompile after a retire without incrementing twice for an
    /// already-failed block.
    pub(crate) fn note_recompile(&self, generation: Generation) -> Phase {
        self.fail(generation)
    }

    /// Whether an optimized unit of `generation` is the current one.
    pub(crate) fn is_installed(&self, generation: Generation) -> bool {
        let current = decode(self.0.load(Ordering::Acquire));
        current.phase == Phase::Opt && current.generation == generation
    }
}
