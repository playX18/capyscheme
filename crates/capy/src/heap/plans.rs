use mmtk::BarrierSelector;
use mmtk::util::options::PlanSelector;
use std::sync::atomic::{AtomicU8, Ordering};

/// GC plans supported by the Capy binding.
pub const ALLOWED_GC_PLAN_NAMES: &[&str] = &[
    "StickyImmix",
    "ConcurrentImmix",
    "MarkSweep",
    "Immix",
    "SemiSpace",
    "GenImmix",
    "GenCopy",
];

pub fn is_allowed_plan(plan: PlanSelector) -> bool {
    matches!(
        plan,
        PlanSelector::StickyImmix
            | PlanSelector::ConcurrentImmix
            | PlanSelector::MarkSweep
            | PlanSelector::Immix
            | PlanSelector::SemiSpace
            | PlanSelector::GenImmix
            | PlanSelector::GenCopy
    )
}

pub fn validate_plan(plan: PlanSelector) {
    assert!(
        is_allowed_plan(plan),
        "unsupported MMTK plan {plan:?}; allowed: {}",
        ALLOWED_GC_PLAN_NAMES.join(", ")
    );
}

/// Stdlib / cache directory bucket for FASL keyed by the live plan's write barrier.
pub fn barrier_artifact_kind(plan: PlanSelector) -> &'static str {
    match plan {
        PlanSelector::MarkSweep | PlanSelector::SemiSpace | PlanSelector::Immix => "nobarrier",
        PlanSelector::StickyImmix | PlanSelector::GenImmix | PlanSelector::GenCopy => "objbarrier",
        PlanSelector::ConcurrentImmix => "satbbarrier",
        _ => unreachable!("GC plan validated at startup"),
    }
}

/// Parse a barrier artifact kind name into a [`BarrierSelector`].
pub fn parse_barrier_artifact_kind(s: &str) -> Option<BarrierSelector> {
    match s {
        "nobarrier" => Some(BarrierSelector::NoBarrier),
        "objbarrier" => Some(BarrierSelector::ObjectBarrier),
        "satbbarrier" => Some(BarrierSelector::SATBBarrier),
        _ => None,
    }
}

pub fn barrier_selector_artifact_kind(barrier: BarrierSelector) -> &'static str {
    match barrier {
        BarrierSelector::NoBarrier => "nobarrier",
        BarrierSelector::ObjectBarrier => "objbarrier",
        BarrierSelector::SATBBarrier => "satbbarrier",
        BarrierSelector::FieldBarrier => "fieldbarrier",
    }
}

/// 0 = unset (fall through to env / live plan); 1..=3 = BarrierSelector as u8 + 1
static COMPILE_BARRIER_OVERRIDE: AtomicU8 = AtomicU8::new(0);

fn encode_barrier(barrier: BarrierSelector) -> u8 {
    (barrier as u8).wrapping_add(1)
}

fn decode_barrier(encoded: u8) -> Option<BarrierSelector> {
    match encoded {
        0 => None,
        n => match n.wrapping_sub(1) {
            0 => Some(BarrierSelector::NoBarrier),
            1 => Some(BarrierSelector::ObjectBarrier),
            2 => Some(BarrierSelector::SATBBarrier),
            _ => None,
        },
    }
}

/// Set a process-wide compile-time barrier override (codegen only).
pub fn set_compile_barrier_override(kind: Option<BarrierSelector>) {
    let encoded = kind.map(encode_barrier).unwrap_or(0);
    COMPILE_BARRIER_OVERRIDE.store(encoded, Ordering::Relaxed);
}

pub fn get_compile_barrier_override() -> Option<BarrierSelector> {
    decode_barrier(COMPILE_BARRIER_OVERRIDE.load(Ordering::Relaxed))
}

/// Barrier kind Cranelift should emit: explicit override, else `CAPY_BARRIER_KIND`, else live plan.
pub fn compile_barrier(live: BarrierSelector) -> BarrierSelector {
    if let Some(b) = get_compile_barrier_override() {
        return b;
    }
    if let Ok(s) = std::env::var("CAPY_BARRIER_KIND") {
        if let Some(b) = parse_barrier_artifact_kind(s.trim()) {
            return b;
        }
    }
    live
}

pub fn compile_barrier_artifact_kind(live: BarrierSelector) -> &'static str {
    barrier_selector_artifact_kind(compile_barrier(live))
}
