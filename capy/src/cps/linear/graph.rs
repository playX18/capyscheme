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

fn block_successors<'gc>(procedure: &Procedure<'gc>, block: &Block<'gc>) -> Vec<BlockId> {
    let mut successors = block.terminator.successors();
    if let Terminator::TailCall {
        callee: LinearAtom::Local(callee),
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
