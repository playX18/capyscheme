use super::*;
use std::collections::{HashMap, HashSet};
pub(super) fn loop_blocks<'gc>(procedure: &Procedure<'gc>) -> HashSet<BlockId> {
    let successors = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block_successors(procedure, block)))
        .collect::<HashMap<_, _>>();
    procedure
        .blocks
        .iter()
        .filter_map(|block| {
            successors
                .get(&block.id)
                .into_iter()
                .flatten()
                .any(|successor| can_reach(*successor, block.id, &successors))
                .then_some(block.id)
        })
        .collect()
}

pub(super) fn backedges<'gc>(procedure: &Procedure<'gc>) -> HashSet<(BlockId, BlockId)> {
    let successors = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block.terminator.successors()))
        .collect::<HashMap<_, _>>();
    let reachable = reachable_blocks(procedure.entry, &successors);
    let all_reachable = reachable.clone();
    let mut dominators = reachable
        .iter()
        .map(|block| {
            let initial = if *block == procedure.entry {
                HashSet::from([procedure.entry])
            } else {
                all_reachable.clone()
            };
            (*block, initial)
        })
        .collect::<HashMap<_, _>>();

    let mut changed = true;
    while changed {
        changed = false;
        for block in reachable
            .iter()
            .copied()
            .filter(|block| *block != procedure.entry)
        {
            let mut predecessors = successors.iter().filter_map(|(predecessor, targets)| {
                (reachable.contains(predecessor) && targets.contains(&block))
                    .then_some(*predecessor)
            });
            let Some(first) = predecessors.next() else {
                continue;
            };
            let mut next = dominators[&first].clone();
            for predecessor in predecessors {
                next.retain(|dominator| dominators[&predecessor].contains(dominator));
            }
            next.insert(block);
            if next != dominators[&block] {
                dominators.insert(block, next);
                changed = true;
            }
        }
    }

    successors
        .into_iter()
        .flat_map(|(source, targets)| {
            let dominators = &dominators;
            targets
                .into_iter()
                .filter(move |target| dominators[&source].contains(target))
                .map(move |target| (source, target))
        })
        .collect()
}

fn reachable_blocks(
    entry: BlockId,
    successors: &HashMap<BlockId, Vec<BlockId>>,
) -> HashSet<BlockId> {
    let mut reachable = HashSet::new();
    let mut stack = vec![entry];
    while let Some(block) = stack.pop() {
        if !reachable.insert(block) {
            continue;
        }
        if let Some(next) = successors.get(&block) {
            stack.extend(next.iter().copied());
        }
    }
    reachable
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

fn can_reach(start: BlockId, target: BlockId, successors: &HashMap<BlockId, Vec<BlockId>>) -> bool {
    let mut seen = HashSet::new();
    let mut stack = vec![start];

    while let Some(block) = stack.pop() {
        if block == target {
            return true;
        }
        if !seen.insert(block) {
            continue;
        }
        if let Some(next) = successors.get(&block) {
            stack.extend(next.iter().copied());
        }
    }

    false
}
