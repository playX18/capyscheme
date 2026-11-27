//! Block layout and fallthrough ordering.
//!
//! Runs on the LIR after register allocation, before emission.
//!
//! Weights so far are static and conservative:
//!
//! - the entry starts hot;
//! - every block of a natural loop (found from DFS back edges) is scaled up;
//! - cold blocks (weight 1) stay reachable only through the trace that needs
//!   them, which puts them after the hot path (hot/cold splitting).

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
