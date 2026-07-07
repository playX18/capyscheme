use super::*;
use crate::{
    compiler::ssa::primitive::Primitive,
    runtime::value::{Symbol, Value},
};
use std::collections::{HashMap, HashSet};
const SYMBOL_HASH_DISPATCH_MIN_LENGTH: usize = 4;

#[derive(Debug, Clone, PartialEq, Eq)]
struct SwitchCandidate<'gc> {
    kind: SwitchKind,
    scrutinee: LinearAtom<'gc>,
    cases: Vec<SwitchCase<'gc>>,
    default: BranchTarget<'gc>,
    chain_blocks: Vec<BlockId>,
    instruction_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SwitchNode<'gc> {
    kind: SwitchKind,
    scrutinee: LinearAtom<'gc>,
    value: SwitchCaseValue<'gc>,
    target: BranchTarget<'gc>,
    next: BranchTarget<'gc>,
    removed_blocks: Vec<BlockId>,
    instruction_count: usize,
}

pub(super) fn infer_switches<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    loop {
        let predecessors = local_predecessor_counts(&procedure);
        let Some(candidate) = procedure
            .blocks
            .iter()
            .find_map(|block| infer_switch_candidate(&procedure, block.id, &predecessors))
        else {
            break;
        };
        apply_switch_candidate(&mut procedure, candidate);
    }

    procedure
}

fn local_predecessor_counts<'gc>(procedure: &Procedure<'gc>) -> HashMap<BlockId, usize> {
    let mut predecessors = HashMap::new();
    for block in &procedure.blocks {
        for successor in block.terminator.successors() {
            *predecessors.entry(successor).or_insert(0) += 1;
        }
    }
    predecessors
}

fn infer_switch_candidate<'gc>(
    procedure: &Procedure<'gc>,
    start: BlockId,
    predecessors: &HashMap<BlockId, usize>,
) -> Option<SwitchCandidate<'gc>> {
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<HashMap<_, _>>();
    let first = switch_node(&blocks, predecessors, blocks[&start], true)?;
    let mut kind = first.kind;
    let scrutinee = first.scrutinee;
    let mut cases = vec![SwitchCase {
        value: first.value,
        target: first.target,
    }];
    let mut chain_blocks = vec![start];
    chain_blocks.extend(first.removed_blocks);
    let instruction_count = first.instruction_count;
    let mut seen_values = HashSet::from([first.value]);
    let mut next = first.next;

    while let Some((block, skipped_blocks)) = switch_chain_next_block(&blocks, predecessors, &next)
    {
        let Some(node) = blocks
            .get(&block)
            .and_then(|block| switch_node(&blocks, predecessors, block, false))
        else {
            break;
        };
        if node.kind != kind || node.scrutinee != scrutinee || !seen_values.insert(node.value) {
            break;
        }
        chain_blocks.extend(skipped_blocks);
        chain_blocks.push(block);
        chain_blocks.extend(node.removed_blocks);
        cases.push(SwitchCase {
            value: node.value,
            target: node.target,
        });
        next = node.next;
    }

    if cases.len() < 2
        || (is_symbol_switch_kind(kind) && cases.len() <= SYMBOL_HASH_DISPATCH_MIN_LENGTH)
    {
        return None;
    }

    if is_symbol_switch_kind(kind) {
        let mask = symbol_hash_dispatch_mask(cases.len());
        bucket_symbol_switch_cases(&mut cases, mask);
        kind = SwitchKind::SymbolEq { mask };
    }

    let removed = chain_blocks.iter().copied().skip(1).collect::<HashSet<_>>();
    let removed_defs = switch_removed_defs(&blocks, start, &chain_blocks, instruction_count);
    if cases
        .iter()
        .any(|case| branch_target_mentions_removed(&case.target, &removed))
        || branch_target_mentions_removed(&next, &removed)
        || atom_mentions_defs(scrutinee, &removed_defs)
        || cases
            .iter()
            .any(|case| branch_target_mentions_defs(&case.target, &removed_defs))
        || branch_target_mentions_defs(&next, &removed_defs)
        || surviving_blocks_mention_defs(
            &procedure.blocks,
            start,
            &removed,
            &removed_defs,
            instruction_count,
        )
    {
        return None;
    }

    Some(SwitchCandidate {
        kind,
        scrutinee,
        cases,
        default: next,
        chain_blocks,
        instruction_count,
    })
}

fn switch_chain_next_block<'gc>(
    blocks: &HashMap<BlockId, &Block<'gc>>,
    predecessors: &HashMap<BlockId, usize>,
    next: &BranchTarget<'gc>,
) -> Option<(BlockId, Vec<BlockId>)> {
    let BranchTarget::Local { block, args } = next else {
        return None;
    };
    if !args.is_empty() || predecessors.get(block).copied().unwrap_or(0) != 1 {
        return None;
    }

    let mut block = *block;
    let mut skipped_blocks = Vec::new();
    loop {
        let current = blocks[&block];
        if switch_node(blocks, predecessors, current, false).is_some() {
            return Some((block, skipped_blocks));
        }

        let Terminator::Jump {
            target,
            args: jump_args,
        } = &current.terminator
        else {
            return Some((block, skipped_blocks));
        };
        if !current.params.is_empty()
            || current.variadic.is_some()
            || !current.instructions.is_empty()
            || !jump_args.is_empty()
            || predecessors.get(target).copied().unwrap_or(0) != 1
        {
            return Some((block, skipped_blocks));
        }

        skipped_blocks.push(block);
        block = *target;
    }
}

fn switch_node<'gc>(
    blocks: &HashMap<BlockId, &Block<'gc>>,
    predecessors: &HashMap<BlockId, usize>,
    block: &Block<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    switch_direct_node(block, allow_prefix)
        .or_else(|| switch_jump_branch_node(blocks, predecessors, block, allow_prefix))
}

fn switch_direct_node<'gc>(block: &Block<'gc>, allow_prefix: bool) -> Option<SwitchNode<'gc>> {
    let Terminator::Branch {
        test: LinearAtom::Local(test),
        consequent,
        alternative,
        hints: _,
    } = &block.terminator
    else {
        return None;
    };
    let test = *test;
    switch_test_node(block, test, consequent, alternative, allow_prefix)
}

fn switch_jump_branch_node<'gc>(
    blocks: &HashMap<BlockId, &Block<'gc>>,
    predecessors: &HashMap<BlockId, usize>,
    block: &Block<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    let Terminator::Jump { target, args } = &block.terminator else {
        return None;
    };
    if predecessors.get(target).copied().unwrap_or(0) != 1 {
        return None;
    }

    let branch = blocks[target];
    if !branch.instructions.is_empty() {
        return None;
    }
    let Terminator::Branch {
        test: LinearAtom::Local(branch_test),
        consequent,
        alternative,
        hints: _,
    } = &branch.terminator
    else {
        return None;
    };
    let test = branch_test_jump_arg(branch, *branch_test, args)?;

    let mut node = switch_test_node(block, test, consequent, alternative, allow_prefix)?;
    node.removed_blocks.push(*target);
    Some(node)
}

fn branch_test_jump_arg<'gc>(
    branch: &Block<'gc>,
    branch_test: ValueId,
    args: &[LinearAtom<'gc>],
) -> Option<ValueId> {
    let fixed_param_count = branch
        .params
        .len()
        .saturating_sub(usize::from(branch.variadic.is_some()));
    let index = branch.params[..fixed_param_count]
        .iter()
        .position(|param| *param == branch_test)?;
    let LinearAtom::Local(test) = args.get(index).copied()? else {
        return None;
    };
    Some(test)
}

fn switch_test_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    switch_fixnum_node(block, test, consequent, alternative, allow_prefix)
        .or_else(|| switch_eq_char_node(block, test, consequent, alternative, allow_prefix))
        .or_else(|| switch_eq_symbol_node(block, test, consequent, alternative, allow_prefix))
        .or_else(|| switch_char_node(block, test, consequent, alternative, allow_prefix))
}

fn switch_fixnum_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    let SwitchPrimCall {
        prim,
        args,
        instruction_count,
    } = switch_prim_call(block, test, allow_prefix)?;
    let kind = switch_kind_for_primitive(prim)?;
    if args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = fixnum_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count,
    })
}

fn switch_kind_for_primitive(prim: Primitive) -> Option<SwitchKind> {
    match prim {
        Primitive::is_eq | Primitive::is_eqv | Primitive::is_equal => Some(SwitchKind::Eq),
        Primitive::fx_eq => Some(SwitchKind::Fixnum),
        Primitive::numeric_equal => Some(SwitchKind::Numeric),
        _ => None,
    }
}

fn fixnum_switch_test<'gc>(
    lhs: LinearAtom<'gc>,
    rhs: LinearAtom<'gc>,
) -> Option<(LinearAtom<'gc>, i32)> {
    match (lhs, rhs) {
        (scrutinee, LinearAtom::Constant(value)) if value.is_int32() => {
            Some((scrutinee, value.as_int32()))
        }
        (LinearAtom::Constant(value), scrutinee) if value.is_int32() => {
            Some((scrutinee, value.as_int32()))
        }
        _ => None,
    }
}

fn switch_eq_char_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    let SwitchPrimCall {
        prim,
        args,
        instruction_count,
    } = switch_prim_call(block, test, allow_prefix)?;
    if !is_eq_like_primitive(prim) || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = char_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::CharEq,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count,
    })
}

fn char_switch_test<'gc>(
    lhs: LinearAtom<'gc>,
    rhs: LinearAtom<'gc>,
) -> Option<(LinearAtom<'gc>, i32)> {
    match (lhs, rhs) {
        (scrutinee, LinearAtom::Constant(value)) if value.is_char() => {
            Some((scrutinee, value.char() as i32))
        }
        (LinearAtom::Constant(value), scrutinee) if value.is_char() => {
            Some((scrutinee, value.char() as i32))
        }
        _ => None,
    }
}

fn switch_eq_symbol_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    let SwitchPrimCall {
        prim,
        args,
        instruction_count,
    } = switch_prim_call(block, test, allow_prefix)?;
    if !is_eq_like_primitive(prim) || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = symbol_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::SymbolEq { mask: 0 },
        scrutinee,
        value,
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count,
    })
}

fn is_eq_like_primitive(prim: Primitive) -> bool {
    matches!(
        prim,
        Primitive::is_eq | Primitive::is_eqv | Primitive::is_equal
    )
}

struct SwitchPrimCall<'gc> {
    prim: Primitive,
    args: Vec<LinearAtom<'gc>>,
    instruction_count: usize,
}

fn switch_prim_call<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    allow_prefix: bool,
) -> Option<SwitchPrimCall<'gc>> {
    let Instruction::PrimCall {
        dst, prim, args, ..
    } = block.instructions.last()?
    else {
        return None;
    };
    if *dst != test {
        return None;
    }

    let mut instruction_count = 1;
    let mut constants = HashMap::new();
    let mut needed = args
        .iter()
        .filter_map(|arg| match arg {
            LinearAtom::Local(value) => Some(*value),
            LinearAtom::Constant(_) => None,
        })
        .collect::<HashSet<_>>();

    for instruction in block.instructions[..block.instructions.len() - 1]
        .iter()
        .rev()
    {
        match instruction {
            Instruction::Const { dst, value } if needed.remove(dst) => {
                constants.insert(*dst, *value);
                instruction_count += 1;
            }
            _ => break,
        }
    }

    if block.instructions.len() != instruction_count && !allow_prefix {
        return None;
    }

    let args = args
        .iter()
        .copied()
        .map(|arg| resolve_local_constant(arg, &constants))
        .collect();

    Some(SwitchPrimCall {
        prim: *prim,
        args,
        instruction_count,
    })
}

fn resolve_local_constant<'gc>(
    atom: LinearAtom<'gc>,
    constants: &HashMap<ValueId, Value<'gc>>,
) -> LinearAtom<'gc> {
    match atom {
        LinearAtom::Local(value) => constants
            .get(&value)
            .copied()
            .map(LinearAtom::Constant)
            .unwrap_or(atom),
        LinearAtom::Constant(_) => atom,
    }
}

fn symbol_switch_test<'gc>(
    lhs: LinearAtom<'gc>,
    rhs: LinearAtom<'gc>,
) -> Option<(LinearAtom<'gc>, SwitchCaseValue<'gc>)> {
    match (lhs, rhs) {
        (scrutinee, LinearAtom::Constant(value)) if value.is::<Symbol>() => {
            let symbol = value.downcast::<Symbol<'_>>();
            Some((
                scrutinee,
                SwitchCaseValue::Symbol {
                    hash: symbol.hash.get(),
                    value,
                },
            ))
        }
        (LinearAtom::Constant(value), scrutinee) if value.is::<Symbol>() => {
            let symbol = value.downcast::<Symbol<'_>>();
            Some((
                scrutinee,
                SwitchCaseValue::Symbol {
                    hash: symbol.hash.get(),
                    value,
                },
            ))
        }
        _ => None,
    }
}

fn is_symbol_switch_kind(kind: SwitchKind) -> bool {
    matches!(kind, SwitchKind::SymbolEq { .. })
}

fn symbol_hash_dispatch_mask(ntargets: usize) -> u64 {
    let ntargets = ntargets as u128;
    let mut nbits = 2;
    while nbits < u64::BITS && ntargets > (1u128 << nbits) {
        nbits += 1;
    }

    if nbits == u64::BITS {
        u64::MAX
    } else {
        (1u64 << nbits) - 1
    }
}

fn bucket_symbol_switch_cases<'gc>(cases: &mut [SwitchCase<'gc>], mask: u64) {
    for case in cases {
        if let SwitchCaseValue::Symbol { hash, .. } = &mut case.value {
            *hash &= mask;
        }
    }
}

fn switch_char_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() < 3 || (block.instructions.len() != 3 && !allow_prefix) {
        return None;
    }

    let instructions = &block.instructions[block.instructions.len() - 3..];
    let Instruction::PrimCall {
        dst: scrutinee_int,
        prim: scrutinee_prim,
        args: scrutinee_args,
        ..
    } = &instructions[0]
    else {
        return None;
    };
    let Instruction::PrimCall {
        dst: case_int,
        prim: case_prim,
        args: case_args,
        ..
    } = &instructions[1]
    else {
        return None;
    };
    let Instruction::PrimCall {
        dst: cmp,
        prim: cmp_prim,
        args: cmp_args,
        ..
    } = &instructions[2]
    else {
        return None;
    };

    if *scrutinee_prim != Primitive::char_to_integer
        || *case_prim != Primitive::char_to_integer
        || *cmp_prim != Primitive::numeric_equal
        || scrutinee_args.len() != 1
        || case_args.len() != 1
        || *cmp != test
        || cmp_args.len() != 2
    {
        return None;
    }

    let LinearAtom::Constant(case_char) = case_args[0] else {
        return None;
    };
    if !case_char.is_char() {
        return None;
    }

    let compares_ints = matches!(
        (cmp_args[0], cmp_args[1]),
        (LinearAtom::Local(lhs), LinearAtom::Local(rhs))
            if lhs == *scrutinee_int && rhs == *case_int
    ) || matches!(
        (cmp_args[0], cmp_args[1]),
        (LinearAtom::Local(lhs), LinearAtom::Local(rhs))
            if lhs == *case_int && rhs == *scrutinee_int
    );
    if !compares_ints {
        return None;
    }

    Some(SwitchNode {
        kind: SwitchKind::Char,
        scrutinee: scrutinee_args[0],
        value: SwitchCaseValue::Integer(case_char.char() as i32),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count: 3,
    })
}

fn branch_target_mentions_removed<'gc>(
    target: &BranchTarget<'gc>,
    removed: &HashSet<BlockId>,
) -> bool {
    matches!(target, BranchTarget::Local { block, .. } if removed.contains(block))
}

fn switch_removed_defs<'gc>(
    blocks: &HashMap<BlockId, &Block<'gc>>,
    start: BlockId,
    chain_blocks: &[BlockId],
    instruction_count: usize,
) -> HashSet<ValueId> {
    let mut defs = HashSet::new();

    for block in chain_blocks.iter().copied().skip(1) {
        if let Some(block) = blocks.get(&block) {
            defs.extend(block.params.iter().copied());
            defs.extend(block.variadic);
            defs.extend(block.instructions.iter().flat_map(Instruction::defs));
        }
    }

    if let Some(block) = blocks.get(&start) {
        let consumed_start = block.instructions.len().saturating_sub(instruction_count);
        defs.extend(
            block.instructions[consumed_start..]
                .iter()
                .flat_map(Instruction::defs),
        );
    }

    defs
}

fn atom_mentions_defs<'gc>(atom: LinearAtom<'gc>, defs: &HashSet<ValueId>) -> bool {
    matches!(atom, LinearAtom::Local(value) if defs.contains(&value))
}

fn branch_target_mentions_defs<'gc>(target: &BranchTarget<'gc>, defs: &HashSet<ValueId>) -> bool {
    target
        .uses()
        .iter()
        .any(|atom| matches!(atom, LinearAtom::Local(value) if defs.contains(value)))
}

fn surviving_blocks_mention_defs<'gc>(
    blocks: &[Block<'gc>],
    start: BlockId,
    removed: &HashSet<BlockId>,
    defs: &HashSet<ValueId>,
    instruction_count: usize,
) -> bool {
    for block in blocks {
        if removed.contains(&block.id) {
            continue;
        }

        if block.id == start {
            let keep = block.instructions.len().saturating_sub(instruction_count);
            if block.instructions[..keep]
                .iter()
                .flat_map(Instruction::uses)
                .any(|atom| matches!(atom, LinearAtom::Local(value) if defs.contains(&value)))
            {
                return true;
            }
            continue;
        }

        if block
            .instructions
            .iter()
            .flat_map(Instruction::uses)
            .chain(block.terminator.uses())
            .any(|atom| matches!(atom, LinearAtom::Local(value) if defs.contains(&value)))
        {
            return true;
        }
    }

    false
}

fn apply_switch_candidate<'gc>(procedure: &mut Procedure<'gc>, candidate: SwitchCandidate<'gc>) {
    let removed = candidate
        .chain_blocks
        .iter()
        .copied()
        .skip(1)
        .collect::<HashSet<_>>();
    let start = candidate.chain_blocks[0];

    for block in &mut procedure.blocks {
        if block.id == start {
            let keep = block
                .instructions
                .len()
                .checked_sub(candidate.instruction_count)
                .expect("switch candidate consumes no more than the block instructions");
            block.instructions.truncate(keep);
            block.terminator = Terminator::Switch {
                kind: candidate.kind,
                scrutinee: candidate.scrutinee,
                cases: candidate.cases,
                default: candidate.default,
            };
            break;
        }
    }

    procedure
        .blocks
        .retain(|block| !removed.contains(&block.id));
}
