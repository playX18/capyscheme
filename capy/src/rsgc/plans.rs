use mmtk::util::options::PlanSelector;

/// GC plans supported by the Capy binding.
pub const ALLOWED_GC_PLAN_NAMES: &[&str] =
    &["StickyImmix", "ConcurrentImmix", "MarkSweep", "Immix"];

pub fn is_allowed_plan(plan: PlanSelector) -> bool {
    matches!(
        plan,
        PlanSelector::StickyImmix
            | PlanSelector::ConcurrentImmix
            | PlanSelector::MarkSweep
            | PlanSelector::Immix
    )
}
