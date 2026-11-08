//! Call statistics for Tier 2 (`CAPY_JIT_STATS=1`).
//!
//! The counters are per compile: each call site the builder lowers, and each
//! inlining decision, is counted once per unit built, so a site that is
//! built again counts again. They say what the builder did, not how often
//! the machine code ran.
//!
//! The other half is the tail call lowering. A tail call whose profile names
//! the procedure being built is a self tail call when the callee is the
//! running closure, so the builder guards that identity and jumps to the
//! entry, as `OpTailCallSelf` does. Any other callee keeps the generic tail
//! call.

use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use super::profile::{self, CallShape};
use super::*;

/// Why a call site is not inlined. The order is the report's order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Refusal {
    NoProfile,
    NeverCalled,
    Megamorphic,
    PolyOff,
    Blocked,
    Depth,
    Size,
    Budget,
    Repeat,
    Arity,
    Native,
    NotFixed,
    CalleeBuild,
    Splice,
    Folded,
    Cold,
    Score,
    Other,
}

const REFUSALS: usize = 18;
const NAMES: [&str; REFUSALS] = [
    "no-profile",
    "never-called",
    "megamorphic",
    "poly-off",
    "blocked",
    "depth",
    "size",
    "budget",
    "repeat",
    "arity",
    "native",
    "not-fixed-entry",
    "callee-build",
    "splice",
    "folded",
    "cold",
    "score",
    "other",
];

const ZERO: AtomicU64 = AtomicU64::new(0);
static REFUSED: [AtomicU64; REFUSALS] = [ZERO; REFUSALS];
static SITES: AtomicU64 = ZERO;
static SELF_SITES: AtomicU64 = ZERO;
static INLINED_MONO: AtomicU64 = ZERO;
static INLINED_POLY: AtomicU64 = ZERO;
static INLINED_TARGETS: AtomicU64 = ZERO;
/// Summed call-site hits over the sites that were inlined, so the report
/// shows how much of the hot profile the policy covered.
static INLINED_SITE_HITS: AtomicU64 = ZERO;
/// Polymorphic sites inlined with fewer targets than the profile named.
static PARTIAL_POLY: AtomicU64 = ZERO;
/// Targets dropped from a polymorphic site (cold, negative score, or
/// unbuildable); they fall through to the generic fallback.
static SKIPPED_TARGETS: AtomicU64 = ZERO;
static GENERIC: AtomicU64 = ZERO;
static DIRECT: AtomicU64 = ZERO;
static TAIL_GENERIC: AtomicU64 = ZERO;
static TAIL_SELF_JUMPS: AtomicU64 = ZERO;

/// A call-like instruction the builder lowers. `self_call` is `OpCallSelf`.
pub(crate) fn site(self_call: bool) {
    SITES.fetch_add(1, Relaxed);
    if self_call {
        SELF_SITES.fetch_add(1, Relaxed);
    }
}

/// A call site that was inlined with `targets` callees. `site_hits` is the
/// site's total call count. `poly_site` says the profile was polymorphic;
/// `partial` says at least one profiled target was left to the generic
/// fallback.
pub(crate) fn inlined(targets: usize, site_hits: u32, poly_site: bool, partial: bool) {
    if poly_site {
        INLINED_POLY.fetch_add(1, Relaxed);
    } else {
        INLINED_MONO.fetch_add(1, Relaxed);
    }
    INLINED_TARGETS.fetch_add(targets as u64, Relaxed);
    INLINED_SITE_HITS.fetch_add(u64::from(site_hits), Relaxed);
    if partial {
        PARTIAL_POLY.fetch_add(1, Relaxed);
    }
}

/// One target of a polymorphic site that was left to the generic fallback.
pub(crate) fn target_skipped() {
    SKIPPED_TARGETS.fetch_add(1, Relaxed);
}

/// A call site left as a call. `direct` is a known-callee call (`CallKnown`).
pub(crate) fn call(direct: bool) {
    if direct {
        DIRECT.fetch_add(1, Relaxed);
    } else {
        GENERIC.fetch_add(1, Relaxed);
    }
}

/// A tail call: a generic tail call (`OpTailCall`), or a self tail call
/// that became a jump to the entry (`OpTailCallSelf`).
pub(crate) fn tail(self_call: bool) {
    if self_call {
        TAIL_SELF_JUMPS.fetch_add(1, Relaxed);
    } else {
        TAIL_GENERIC.fetch_add(1, Relaxed);
    }
}

/// A call site the inliner refused, with the inliner's reason.
pub(crate) fn refused(why: &str) {
    REFUSED[classify(why) as usize].fetch_add(1, Relaxed);
}

/// The refusal kind for an inliner message. The messages are the ones
/// `inline.rs` builds, so this matches on their wording.
fn classify(why: &str) -> Refusal {
    if why.starts_with("the site never called") {
        Refusal::NeverCalled
    } else if why.starts_with("no call profile") {
        Refusal::NoProfile
    } else if why.starts_with("the site is megamorphic") {
        Refusal::Megamorphic
    } else if why.starts_with("the site is polymorphic") {
        Refusal::PolyOff
    } else if why.starts_with("a guard of this site") {
        Refusal::Blocked
    } else if why.starts_with("inlining depth") {
        Refusal::Depth
    } else if why.contains("instructions is over") {
        Refusal::Size
    } else if why.contains("budget") {
        Refusal::Budget
    } else if why.starts_with("it is already inlined") {
        Refusal::Repeat
    } else if why.starts_with("it takes") {
        Refusal::Arity
    } else if why.starts_with("it is a native procedure") {
        Refusal::Native
    } else if why.starts_with("it does not start") {
        Refusal::NotFixed
    } else if why.starts_with("the callee does not build") {
        Refusal::CalleeBuild
    } else if why.starts_with("the callee folded") {
        Refusal::Folded
    } else if why.starts_with("the site is cold") {
        Refusal::Cold
    } else if why.starts_with("the cost score")
        || why.starts_with("a tiny accessor")
        || why.starts_with("no target scored")
    {
        Refusal::Score
    } else if why.starts_with("the callee takes")
        || why.starts_with("the callee has OSR")
        || why.starts_with("the callee tail")
        || why.starts_with("the callee reads")
        || why.starts_with("the callee")
    {
        Refusal::Splice
    } else {
        Refusal::Other
    }
}

/// One line per counter group, for the `CAPY_JIT_STATS` exit report.
pub(crate) fn report() -> String {
    let refused: Vec<String> = NAMES
        .iter()
        .zip(REFUSED.iter())
        .filter_map(|(name, n)| match n.load(Relaxed) {
            0 => None,
            n => Some(format!("{name} {n}")),
        })
        .collect();
    format!(
        "tier 2 calls (per compile): {} sites ({} self), inlined {} mono + {} poly ({} targets, \
         {} site hits, {} partial poly, {} targets skipped), refused [{}], \
         generic {}, direct {}, generic tails {}, self tail jumps {}",
        SITES.load(Relaxed),
        SELF_SITES.load(Relaxed),
        INLINED_MONO.load(Relaxed),
        INLINED_POLY.load(Relaxed),
        INLINED_TARGETS.load(Relaxed),
        INLINED_SITE_HITS.load(Relaxed),
        PARTIAL_POLY.load(Relaxed),
        SKIPPED_TARGETS.load(Relaxed),
        refused.join(", "),
        GENERIC.load(Relaxed),
        DIRECT.load(Relaxed),
        TAIL_GENERIC.load(Relaxed),
        TAIL_SELF_JUMPS.load(Relaxed),
    )
}

impl<'s, 'gc> Builder<'s, 'gc> {
    /// Lowers the tail call at `pc`, which passes `argc` arguments in
    /// registers `2..`. The callee is in register 1.
    pub(super) fn lower_tail_call(&mut self, pc: usize, argc: u32) -> Result<(), String> {
        let callee = self.reg(1)?;
        let args = self.regs(2..2 + argc)?;
        let frame = self.frame(pc, pc)?;
        let Some(closure) = self.self_tail_closure(pc, argc) else {
            // In an inlined body, a monomorphic tail call inlines in turn: its
            // frames replace this body's, as the tail call replaces the frame.
            if self.inline.depth > 0 {
                if let Some(result) =
                    self.try_inline(pc, pc, 1, 2, argc, (ReturnValues::Ignore, frame), true)?
                {
                    self.terminate(Terminator::Return(result));
                    return Ok(());
                }
            }
            // A recognized record procedure is cheaper inline than called.
            if self.try_record_tail_call(pc, argc)? {
                return Ok(());
            }
            tail(false);
            // A monomorphic profile target: enter its compiled entry directly
            // after a runtime code check (the emitter falls back to the
            // generic tail call on a mismatch; this frame cannot deopt).
            let check = self
                .mono_tail_target(pc, argc)
                .map(|target| self.code_for_heap(SchemeValue::from(target.callee)));
            let args = EntityList::from_slice(&args, &mut self.func.pool);
            self.terminate(Terminator::TailCall {
                callee,
                args,
                frame,
                check,
            });
            return Ok(());
        };
        tail(true);
        // The jump path: the registers already hold the new arguments.
        let same = self.value_op(Op::EqBits, &[callee, closure], Type::Bool, None);
        let jump = self.func.new_block();
        let generic = self.func.new_block();
        let then = self.func.edge(jump, &[]);
        let otherwise = self.func.edge(generic, &[]);
        self.terminate(Terminator::Branch {
            cond: same,
            then,
            otherwise,
        });

        self.cur = jump;
        self.safepoint(pc)?;
        let edge = self.edge_to(0)?;
        self.terminate(Terminator::Jump(edge));

        self.cur = generic;
        let check = self
            .mono_tail_target(pc, argc)
            .map(|target| self.code_for_heap(SchemeValue::from(target.callee)));
        let args = EntityList::from_slice(&args, &mut self.func.pool);
        self.terminate(Terminator::TailCall {
            callee,
            args,
            frame,
            check,
        });
        Ok(())
    }

    fn mono_tail_target(&self, pc: usize, argc: u32) -> Option<super::profile::CallTarget<'gc>> {
        let super::profile::CallShape::Mono(Some(target)) =
            super::profile::call_shape(self.buffer, pc)?
        else {
            return None;
        };
        if target.callee.is_native() || !super::direct_arity_matches(&target.callee, argc) {
            return None;
        }
        Some(target)
    }

    fn self_tail_closure(&self, pc: usize, argc: u32) -> Option<Value> {
        if self.inline.depth != 0 || self.argc != Some(argc) {
            return None;
        }
        match profile::call_shape(self.buffer, pc)? {
            CallShape::Mono(Some(target))
                if SchemeValue::from(target.callee) == self.inline.chain[0] => {}
            _ => return None,
        }
        let regs = self.param_regs[0].as_ref()?;
        let index = regs.iter().position(|&reg| reg == 1)?;
        let block = self.ir_block[0]?;
        self.func.params(block).get(index).copied()
    }
}
