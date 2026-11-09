//! Block layout and fallthrough ordering.
//!
//! Runs on the LIR after register allocation, before emission. The allocator
//! numbers blocks in reverse postorder; this pass replaces that order with a
//! depth-first trace that keeps a block's most frequent successor next, so
//! the emitter's existing fallthrough checks drop the jump and let a
//! conditional branch fall through on its hotter arm.
//!
//! No branch profile is recorded at the moment, so the only frequency source in the
//! builder is the arithmetic profile word, which says nothing about control
//! flow. Weights are therefore static and conservative:
//!
//! - the entry starts hot;
//! - every block of a natural loop (found from DFS back edges) is scaled up;
//! - cold blocks (weight 1) stay reachable only through the trace that needs
//!   them, which puts them after the hot path (hot/cold splitting).
//!
//! Switch cases are not reordered: a dense table is indexed by key, and a
//! sparse tree must stay sorted. Branch arms are ordered only by the static
//! weight, a no-op when they tie.

use cranelift_entity::SecondaryMap;

use crate::runtime::codegen::opt::lir::weights::{block_weights, successors};
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, LirTerm};

/// Replaces `order` (the allocator's block order) with a hot-first trace.
pub(crate) fn reorder(func: &LirFunc, order: &mut Vec<LBlock>) {
    if order.len() < 3 {
        return;
    }
    // A dense switch is an interpreter-style dispatch table: its targets are
    // indirect-branch destinations, and scattering them by trace order costs
    // far more (BTB and I-cache footprint) than straight-line fallthrough
    // saves. Keep the allocator's reverse postorder for such functions.
    if func.blocks.values().any(|block| {
        matches!(
            &block.term,
            Some(LirTerm::Switch { cases, .. }) if cases.len() >= 8
        )
    }) {
        return;
    }
    let weights = block_weights(func);
    let mut visited: SecondaryMap<LBlock, bool> = SecondaryMap::with_default(false);
    let mut out = Vec::with_capacity(order.len());
    let mut stack = vec![func.entry];
    while let Some(block) = stack.pop() {
        if visited[block] {
            continue;
        }
        visited[block] = true;
        out.push(block);
        let mut next = successors(func, block);
        // Stable sort by weight, then push in reverse so the heaviest
        // successor is visited (and laid out) first.
        next.sort_by_key(|&successor| std::cmp::Reverse(weights[successor]));
        for successor in next.into_iter().rev() {
            if !visited[successor] {
                stack.push(successor);
            }
        }
    }
    // Unreachable blocks (which `dce` should have removed) keep their old
    // relative order at the cold end.
    for &block in order.iter() {
        if !visited[block] {
            out.push(block);
        }
    }
    if out.len() == order.len() {
        *order = out;
    }
}
