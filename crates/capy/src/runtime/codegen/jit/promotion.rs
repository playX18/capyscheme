use std::panic::AssertUnwindSafe;
use std::sync::atomic::Ordering::{Acquire, Relaxed};

use super::super::addr::CodeState;
use super::super::cache::Partition;
use super::super::install::Phase;
use super::super::masm::Masm;
use super::super::template_image::runs_template;
use super::super::unit::Tier;
use super::compile::{
    claim_compile, compile_allowed, compile_claimed, compiler_for, finish_optimized, install,
    log_tier_up_request, poison,
};
use super::config::{TierSet, opt, tiers};
use super::registry::NEXT_ID;
use super::worker::{compile_job, submit};
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::{Closure, CodeBlock, CodeBlockKind};
use crate::utils::flags;

/// While set, the runtime is still booting: boot procedures run a handful of
/// times each, so tiering them up is pure startup cost.
static BOOT_HOLD: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// RAII guard for [`set_boot_hold`]: releases the hold on drop.
pub(crate) struct BootHold;

impl BootHold {
    pub(crate) fn hold() -> Self {
        set_boot_hold(true);
        Self
    }
}

impl Drop for BootHold {
    fn drop(&mut self) {
        set_boot_hold(false);
    }
}

pub(crate) fn set_boot_hold(hold: bool) {
    BOOT_HOLD.store(hold, Relaxed);
}

/// Promotion policy: when is a block hot enough to tier up.
pub(crate) trait HotnessPolicy {
    /// Heat threshold for baseline, or `None` when tier-up is off
    fn hot_threshold(&self) -> Option<u32>;

    /// The tiers this policy lets compile.
    fn tiers(&self) -> TierSet;

    /// The tier to compile `code_block` at next, given its state. A block
    /// with no code gets baseline. A block with baseline code gets Tier 2
    /// when the set names it and Tier 2 has not been tried. A block marked
    /// never to compile gets `None`.
    fn next_tier(&self, code_block: &CodeBlock<'_>) -> Option<Tier> {
        match code_block.entry.load(Acquire) {
            CodeState::None => Some(Tier::Baseline),
            // An invalidated block warms up again for its recompile.
            CodeState::Entry(_) => (self.tiers().contains(Tier::Optimizing)
                && matches!(
                    code_block.install.phase(),
                    Phase::Baseline | Phase::NotEntrant
                ))
            .then_some(Tier::Optimizing),
            CodeState::Poisoned => None,
        }
    }
}

/// Fixed-threshold policy from `CAPY_JIT`/`CAPY_JIT_THRESHOLD`, with the
/// optimizing tier enabled unless `CAPY_OPT_JIT=0`.
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
    closure: Option<Gc<'gc, Closure<'gc>>>,
) -> PromotionOutcome {
    request_inner(ctx, code_block, policy, flags::jit_workers() == 0, closure)
}

fn request_inner<'gc, P: HotnessPolicy>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    policy: &P,
    sync: bool,
    closure: Option<Gc<'gc, Closure<'gc>>>,
) -> PromotionOutcome {
    use PromotionOutcome::*;
    // Set already, or marked never-to-compile (`usize::MAX`).
    if code_block.entry.is_decided(Relaxed) {
        return request_optimized(ctx, code_block, policy, sync, closure);
    }

    if install(ctx, code_block) {
        code_block.hot_count.store(0, Relaxed);
        return Installed;
    }
    if BOOT_HOLD.load(Relaxed) {
        // Still booting: never tier up, and drop the accumulated heat
        code_block.hot_count.store(0, Relaxed);
        return Skipped;
    }
    let Some(threshold) = policy.hot_threshold() else {
        return Skipped;
    };

    let heat = code_block.hot_count.load(Relaxed);
    if code_block.kind != CodeBlockKind::Bytecode || heat < threshold || !runs_template(ctx) {
        return Skipped;
    }
    if !compile_allowed(code_block, Partition::Baseline) {
        return Skipped;
    }
    let Some(tier) = policy.next_tier(&code_block) else {
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

    submit(ctx, code_block, id, tier, 0, closure);
    log_tier_up_request(code_block, heat, threshold, id);
    code_block.hot_count.store(0, Relaxed);
    Queued
}

fn request_optimized<'gc, P: HotnessPolicy>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    policy: &P,
    sync: bool,
    closure: Option<Gc<'gc, Closure<'gc>>>,
) -> PromotionOutcome {
    use PromotionOutcome::*;
    if policy.next_tier(&code_block) != Some(Tier::Optimizing) {
        code_block.hot_count.store(0, Relaxed);
        return Installed;
    }
    let heat = code_block.hot_count.load(Relaxed);
    let threshold = u32::try_from(opt().threshold).unwrap_or(u32::MAX);
    if heat < threshold || code_block.kind != CodeBlockKind::Bytecode || !runs_template(ctx) {
        return Installed;
    }
    if !compile_allowed(code_block, Partition::Optimizing) {
        code_block.hot_count.store(0, Relaxed);
        return Skipped;
    }
    let Some(generation) = code_block.claim_opt_compile() else {
        code_block.hot_count.store(0, Relaxed);
        return AlreadyPending;
    };
    let id = NEXT_ID.fetch_add(1, Relaxed);
    code_block.hot_count.store(0, Relaxed);
    if sync {
        log_tier_up_request(code_block, heat, threshold, id);
        return compile_optimized_sync(ctx, code_block, id, generation, closure);
    }
    submit(ctx, code_block, id, Tier::Optimizing, generation, closure);
    log_tier_up_request(code_block, heat, threshold, id);
    Queued
}

/// Tier 2 compile on the calling thread, for `CAPY_JIT_WORKERS=0`. The
/// caller holds the Tier 2 claim.
fn compile_optimized_sync<'gc>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    id: usize,
    generation: crate::runtime::codegen::install::Generation,
    closure: Option<Gc<'gc, Closure<'gc>>>,
) -> PromotionOutcome {
    let label = Some(crate::runtime::objectmodel::proc::procedure_label(
        code_block,
    ));
    // No worker to borrow from on this path: a throwaway assembler is as
    // fast as a cached one for a single compile.
    let mut masm = Masm::new();
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
        compile_job(
            ctx,
            id,
            Tier::Optimizing,
            code_block,
            label.as_deref(),
            closure,
            &mut masm,
        )
    }))
    .unwrap_or(Err(()));
    finish_optimized(ctx, code_block, id, generation, result);
    PromotionOutcome::Installed
}

/// Heal `closure.code` to `code_block`'s optimized entry, once there is one,
/// so the closure's next call goes straight to optimized code. Until then
/// the closure keeps the entry it has.
pub(crate) fn heal_to_entry<'gc>(
    closure: Gc<'gc, Closure<'gc>>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
) {
    let entry = if flags::jit_stable_entry() {
        code_block.baseline_entry.entry(Acquire)
    } else {
        code_block.entry.entry(Acquire)
    };
    let Some(entry) = entry else {
        return;
    };
    // SAFETY: the closure is live, and this mutator is the only writer of its
    // `code`, a plain machine address that the collector does not trace.
    unsafe { (*(closure.as_ptr() as *mut Closure<'gc>)).code = entry };
}

/// Release a submit claim so a later window retries. Best-effort: a stale
/// generation is a no-op.
pub(crate) fn release_claim<'gc>(code_block: Gc<'gc, CodeBlock<'gc>>) {
    let generation = code_block.install.generation();
    let _ = code_block.abort_opt(generation);
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
