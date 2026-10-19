use std::sync::atomic::Ordering::{AcqRel, Relaxed};

use super::super::template_image::runs_template;
use super::super::unit::Tier;
use super::compile::{
    claim_compile, compile_allowed, compile_claimed, compiler_for, install, log_tier_up_request,
    poison,
};
use super::config::{TierSet, tiers};
use super::registry::NEXT_ID;
use super::worker::submit;
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::value::{CodeBlock, CodeBlockKind, Promotion};
use crate::utils::flags;

/// Promotion policy: when is a block hot enough to tier up.
pub(crate) trait HotnessPolicy {
    /// Heat threshold, or `None` when tier-up is off
    fn hot_threshold(&self) -> Option<u32>;

    /// The tiers this policy lets compile.
    fn tiers(&self) -> TierSet;

    /// The tier to compile a hot block at: the cheapest allowed tier that
    /// has a compiler in this build, or `None` when the set names none
    /// (tier-up is then skipped).
    fn next_tier(&self) -> Option<Tier> {
        None
    }
}

/// Fixed-threshold policy from `CAPY_JIT`/`CAPY_JIT_THRESHOLD`, tiers from
/// `CAPY_JIT_TIERS`.
#[derive(Clone, Copy)]
pub(crate) struct ThresholdPolicy {
    threshold: Option<u32>,
    tiers: TierSet,
}

impl ThresholdPolicy {
    /// The live process policy.
    pub(crate) fn current() -> Self {
        Self {
            threshold: flags::jit().then_some(flags::jit_threshold() as u32),
            tiers: tiers(),
        }
    }

    /// An explicit threshold, for tests that pin the threshold instead
    /// of reading the live one.
    #[cfg(test)]
    pub(crate) fn new(threshold: u32) -> Self {
        Self {
            threshold: Some(threshold),
            tiers: TierSet::BASELINE,
        }
    }
}

impl HotnessPolicy for ThresholdPolicy {
    fn hot_threshold(&self) -> Option<u32> {
        self.threshold
    }

    fn tiers(&self) -> TierSet {
        self.tiers
    }

    fn next_tier(&self) -> Option<Tier> {
        self.tiers()
            .iter()
            .find(|tier| compiler_for(*tier).is_some())
    }
}

/// What a [`request_tier_up`] call did.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum PromotionOutcome {
    /// The block has compiled code: it already did, this call compiled
    /// it, or it is marked never-to-compile.
    Installed,
    /// A compile was submitted, the caller keeps interpreting.
    Queued,
    /// A compile is already pending, nothing to do.
    AlreadyPending,
    /// Not requested: tier-up is off, the block is cold or native, the
    /// engine is not the template interpreter, or a transient skip (over
    /// the code budget, queue full) released the claim for a later window.
    Skipped,
}

/// Request tier-up of `code_block`: install finished code if there is
/// any, else submit a compile when the block is hot under `policy`. The
/// call never blocks and never re-enters: submission only enqueues (or,
/// with workers off, compiles on the caller), and the caller keeps
/// interpreting either way.
pub(crate) fn request_tier_up<'gc, P: HotnessPolicy>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    policy: &P,
) -> PromotionOutcome {
    request_inner(ctx, code_block, policy, flags::jit_workers() == 0)
}

fn request_inner<'gc, P: HotnessPolicy>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    policy: &P,
    sync: bool,
) -> PromotionOutcome {
    use PromotionOutcome::*;
    // Set already, or marked never-to-compile (`usize::MAX`).
    if code_block.jit_entry.is_decided(Relaxed) {
        code_block.hot_count.store(0, Relaxed);
        return Installed;
    }

    if install(ctx, code_block) {
        code_block.hot_count.store(0, Relaxed);
        return Installed;
    }
    let Some(threshold) = policy.hot_threshold() else {
        return Skipped;
    };

    let heat = code_block.hot_count.load(Relaxed);
    if code_block.kind != CodeBlockKind::Bytecode || heat < threshold || !runs_template(ctx) {
        return Skipped;
    }
    if !compile_allowed(code_block) {
        return Skipped;
    }
    let Some(tier) = policy.next_tier() else {
        return Skipped;
    };
    if !claim_compile(code_block) {
        // A submit is already pending, reset so completion is re-polled
        // next window instead of every entry.
        code_block.hot_count.store(0, Relaxed);
        return AlreadyPending;
    }
    let id = NEXT_ID.fetch_add(1, Relaxed);
    if sync {
        log_tier_up_request(code_block, heat, threshold, id);
        return compile_sync(ctx, code_block, tier, id);
    }

    submit(ctx, code_block, id, tier);
    log_tier_up_request(code_block, heat, threshold, id);
    code_block.hot_count.store(0, Relaxed);
    Queued
}

/// Release a submit claim so a later window retries. Best-effort: anything but `Submitted` is left
/// alone.
pub(crate) fn release_claim<'gc>(code_block: Gc<'gc, CodeBlock<'gc>>) {
    let _ = code_block.promotion.compare_exchange(
        Promotion::Submitted as u8,
        Promotion::Interpreted as u8,
        AcqRel,
        Relaxed,
    );
}

/// Synchronous tier-up for `CAPY_JIT_WORKERS=0`: compile on the caller,
/// which holds the compile claim.
fn compile_sync<'gc>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    tier: Tier,
    id: usize,
) -> PromotionOutcome {
    let Some(compiler) = compiler_for(tier) else {
        release_claim(code_block);
        return PromotionOutcome::Skipped;
    };
    code_block.hot_count.store(0, Relaxed);
    if let Err(err) = compile_claimed(ctx, code_block, &*compiler, id) {
        log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " {err}");
        // Do not try again.
        poison(code_block);
    }
    PromotionOutcome::Installed
}
