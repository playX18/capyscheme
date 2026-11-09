//! Static block weights, shared by block layout and register allocation.
//!
//! Tier 2 has no runtime profile, so the weights are purely structural: a
//! natural-loop body counts more than straight-line code, and multiplying the
//! nestings makes an inner loop body outrank an outer one.

use cranelift_entity::SecondaryMap;

use super::*;

/// The successors of `block`, without duplicates.
pub(crate) fn successors(func: &LirFunc, block: LBlock) -> Vec<LBlock> {
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
        | LirTerm::ReturnMany { .. }
        | LirTerm::TailCall { .. }
        | LirTerm::Deopt { .. }
        | LirTerm::Unreachable => {}
    }
    out
}

/// Static block weights: a natural-loop body counts more than straight-line
/// code, and the entry is always hot.
pub(crate) fn block_weights(func: &LirFunc) -> SecondaryMap<LBlock, u64> {
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
