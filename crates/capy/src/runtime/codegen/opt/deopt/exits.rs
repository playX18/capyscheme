//! Exit accounting for deopts. The profile itself (per-pc counts, the unit
//! budget and the recompile policy) lives in `runtime::code::exits` and is
//! re-exported here; this file adds the stress knob.

pub(crate) use crate::runtime::code::exits::*;

use crate::runtime::codegen::jit::config::opt;

/// `CAPY_OPT_STRESS_DEOPT=n`: whether guard number `ordinal` (counted from 0
/// by the emitter, per guard site it emits) must deoptimize. Every n-th guard
/// does, so `n = 1` deoptimizes at every guard. Always false when the knob is 0.
pub(crate) fn stress_deopt_due(ordinal: u64) -> bool {
    let period = opt().stress_deopt as u64;
    period != 0 && ordinal % period == period - 1
}

