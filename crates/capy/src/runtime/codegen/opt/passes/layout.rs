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
    let weights = weights(func);
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

/// The successors of `block`, without duplicates.
fn successors(func: &LirFunc, block: LBlock) -> Vec<LBlock> {
    let Some(term) = &func.blocks[block].term else {
        return Vec::new();
    };
    let mut out: Vec<LBlock> = Vec::new();
    match term {
        LirTerm::Jump(edge) => out.push(edge.block),
        LirTerm::Branch {
            then, otherwise, ..
        } => {
            out.push(then.block);
            if otherwise.block != then.block {
                out.push(otherwise.block);
            }
        }
        LirTerm::Switch { cases, default, .. } => {
            // The default is the likely exit of a dispatch loop, so it is
            // considered before the individual cases.
            out.push(default.block);
            for (_, edge) in cases {
                if !out.contains(&edge.block) {
                    out.push(edge.block);
                }
            }
        }
        LirTerm::Return { .. }
        | LirTerm::TailCall { .. }
        | LirTerm::Deopt { .. }
        | LirTerm::Unreachable => {}
    }
    out
}

/// Static block weights: a natural-loop body counts more than straight-line
/// code, and the entry is always hot.
fn weights(func: &LirFunc) -> SecondaryMap<LBlock, u64> {
    let mut loops: SecondaryMap<LBlock, u64> = SecondaryMap::with_default(0);
    let mut state: SecondaryMap<LBlock, u8> = SecondaryMap::with_default(0);
    let mut path: Vec<LBlock> = Vec::new();
    // Iterative DFS, recording which blocks sit on the current path so a
    // back edge's natural loop can be marked.
    let mut stack: Vec<(LBlock, usize, Vec<LBlock>)> =
        vec![(func.entry, 0, successors(func, func.entry))];
    state[func.entry] = 1;
    path.push(func.entry);
    while !stack.is_empty() {
        let top = stack.len() - 1;
        let (block, index, len) = {
            let frame = &mut stack[top];
            (frame.0, frame.1, frame.2.len())
        };
        if index == len {
            state[block] = 2;
            path.pop();
            stack.pop();
            continue;
        }
        let successor = {
            let frame = &mut stack[top];
            frame.1 += 1;
            frame.2[index]
        };
        match state[successor] {
            0 => {
                state[successor] = 1;
                path.push(successor);
                stack.push((successor, 0, successors(func, successor)));
            }
            1 => {
                // A back edge: every block from `successor` to the current
                // block is part of its natural loop.
                if let Some(at) = path.iter().position(|&b| b == successor) {
                    for &member in &path[at..] {
                        loops[member] += 1;
                    }
                }
            }
            _ => {}
        }
    }
    let mut weights: SecondaryMap<LBlock, u64> = SecondaryMap::with_default(1);
    for block in func.blocks.keys() {
        let entry = u64::from(block == func.entry) * 8;
        weights[block] = 1 + 8 * loops[block] + entry;
    }
    weights
}
