//! Version merging for SBBV (ECOOP'24 Section 3.5).
//!
//! When the work queue exposes more live versions than the configured limit,
//! the most similar pair is merged. Their entry contexts are joined with
//! widening so the replacement covers both and the version count converges.

use super::types::{Type, TypeContext, union_types};
use std::collections::HashSet;

/// Counts live-in values whose runtime type shapes agree in both contexts.
///
/// Two versions are "most similar" when they agree on the most live-ins; those
/// are the cheapest to merge because the join loses the least precision. The
/// paper's heuristic compares types, not interval endpoints, so changing an
/// integer range within one runtime kind does not make a loop look
/// polymorphic.
fn similarity(a: &TypeContext, b: &TypeContext) -> usize {
    let ids: HashSet<_> = a.types.keys().chain(b.types.keys()).collect();
    ids.into_iter()
        .filter(|id| a.get(**id).kinds == b.get(**id).kinds)
        .count()
}

/// Selects the most similar pair of active contexts to merge.
pub(super) fn select_versions_to_merge(active: &[TypeContext]) -> (usize, usize) {
    assert!(
        active.len() >= 2,
        "need at least two active versions to select a merge"
    );

    let mut selected = (0, 1);
    let mut best = similarity(&active[0], &active[1]);
    for first in 0..active.len() {
        for second in (first + 1)..active.len() {
            let score = similarity(&active[first], &active[second]);
            if score > best {
                best = score;
                selected = (first, second);
            }
        }
    }
    selected
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
