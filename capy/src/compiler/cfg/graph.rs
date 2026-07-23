use super::*;
use std::collections::{HashMap, HashSet};

/// Blocks that participate in a cycle (SCC size > 1, or a self-loop).
///
/// Uses Kosaraju in O(V+E). The previous implementation ran a full reachability
/// query per edge and became pathological once CFG lowering introduced many
/// small Assign-helper blocks on contified branches.
pub(super) fn loop_blocks<'gc>(procedure: &Procedure<'gc>) -> HashSet<BlockId> {
    let successors = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block_successors(procedure, block)))
        .collect::<HashMap<_, _>>();

    let mut order = Vec::new();
    let mut seen = HashSet::new();
    for block in &procedure.blocks {
        dfs_postorder(block.id, &successors, &mut seen, &mut order);
    }

    let mut predecessors: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (source, targets) in &successors {
        for target in targets {
            predecessors.entry(*target).or_default().push(*source);
        }
    }

    let mut loop_nodes = HashSet::new();
    seen.clear();
    for &block in order.iter().rev() {
        if seen.contains(&block) {
            continue;
        }
        let mut component = Vec::new();
        dfs_collect(block, &predecessors, &mut seen, &mut component);
        let cyclic = component.len() > 1
            || component
                .first()
                .is_some_and(|id| successors.get(id).is_some_and(|succ| succ.contains(id)));
        if cyclic {
            loop_nodes.extend(component);
        }
    }
    loop_nodes
}

fn dfs_postorder(
    block: BlockId,
    successors: &HashMap<BlockId, Vec<BlockId>>,
    seen: &mut HashSet<BlockId>,
    order: &mut Vec<BlockId>,
) {
    if !seen.insert(block) {
        return;
    }
    if let Some(next) = successors.get(&block) {
        for successor in next {
            dfs_postorder(*successor, successors, seen, order);
        }
    }
    order.push(block);
}

fn dfs_collect(
    block: BlockId,
    predecessors: &HashMap<BlockId, Vec<BlockId>>,
    seen: &mut HashSet<BlockId>,
    component: &mut Vec<BlockId>,
) {
    if !seen.insert(block) {
        return;
    }
    component.push(block);
    if let Some(next) = predecessors.get(&block) {
        for predecessor in next {
            dfs_collect(*predecessor, predecessors, seen, component);
        }
    }
}

/// Edges that close a cycle in the CFG (DFS back-edges to a gray ancestor).
///
/// Used by SBBV to recognize loop headers and recurrent versions. Dominator-based
/// back-edge discovery is equivalent on reducible CFGs but the classic iterative
/// dominator algorithm is O(V²) per iteration and became the specialize hot path
/// on large mutable-uvar CFGs (thousands of blocks from contified Assign helpers).
pub(super) fn backedges<'gc>(procedure: &Procedure<'gc>) -> HashSet<(BlockId, BlockId)> {
    let successors = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block.terminator.successors()))
        .collect::<HashMap<_, _>>();

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Color {
        White,
        Gray,
        Black,
    }

    let mut color: HashMap<BlockId, Color> = HashMap::new();
    let mut back = HashSet::new();
    let mut stack = vec![(procedure.entry, 0usize)];
    color.insert(procedure.entry, Color::Gray);

    while let Some((block, idx)) = stack.pop() {
        let succs = successors.get(&block).map(Vec::as_slice).unwrap_or(&[]);
        if idx < succs.len() {
            stack.push((block, idx + 1));
            let succ = succs[idx];
            match color.get(&succ).copied().unwrap_or(Color::White) {
                Color::Gray => {
                    back.insert((block, succ));
                }
                Color::White => {
                    color.insert(succ, Color::Gray);
                    stack.push((succ, 0));
                }
                Color::Black => {}
            }
        } else {
            color.insert(block, Color::Black);
        }
    }

    back
}

fn block_successors<'gc>(procedure: &Procedure<'gc>, block: &Block<'gc>) -> Vec<BlockId> {
    let mut successors = block.terminator.successors();
    if let Terminator::TailCall {
        callee: Operand::Local(callee),
        ..
    } = block.terminator
        && callee == procedure.binding
    {
        successors.push(procedure.entry);
    }
    successors
}
