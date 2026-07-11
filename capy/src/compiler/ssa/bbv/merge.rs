//! Version merging for SBBV (ECOOP'24 Section 3.5).
//!
//! When an incoming version would reach the configured limit, it is merged
//! with the most similar active version. Their entry contexts are joined with
//! widening so the replacement covers both and the version count stays
//! bounded.

use super::types::{Type, TypeContext, union_types};
use std::collections::HashSet;

/// Counts live-in values whose types are *identical* in both contexts.
///
/// Two versions are "most similar" when they agree on the most live-ins; those
/// are the cheapest to merge because the join loses the least precision.
fn similarity(a: &TypeContext, b: &TypeContext) -> usize {
    let ids: HashSet<_> = a.types.keys().chain(b.types.keys()).collect();
    ids.into_iter()
        .filter(|id| a.get(**id) == b.get(**id))
        .count()
}

/// Selects the active context most similar to an incoming context.
///
/// The incoming context must participate in the merge so widening applies to
/// an unbounded stream of interval contexts and guarantees convergence.
pub(super) fn select_version_to_merge(active: &[TypeContext], incoming: &TypeContext) -> usize {
    assert!(
        !active.is_empty(),
        "need at least one active version to select a merge"
    );

    active
        .iter()
        .enumerate()
        .max_by_key(|(_, context)| similarity(context, incoming))
        .map(|(index, _)| index)
        .expect("active contexts are non-empty")
}

/// Joins two contexts by unioning the type of every live-in value.
///
/// `widen` applies interval widening on the joined ranges to guarantee
/// termination of the specialization fixpoint.
pub(super) fn merge_contexts(ctx1: &TypeContext, ctx2: &TypeContext, widen: bool) -> TypeContext {
    let ids: HashSet<_> = ctx1.types.keys().chain(ctx2.types.keys()).collect();
    let mut merged = TypeContext::new();
    for id in ids {
        let joined: Type = union_types(ctx1.get(*id), ctx2.get(*id), widen);
        merged.set(*id, joined);
    }
    merged
}
