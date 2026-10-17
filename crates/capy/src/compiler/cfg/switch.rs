use super::*;
use crate::runtime::{
    prims::def::Primitive,
    value::{Symbol, Value},
};
use std::collections::{HashMap, HashSet};
const SYMBOL_HASH_DISPATCH_MIN_LENGTH: usize = 4;

#[derive(Debug, Clone, PartialEq, Eq)]
struct SwitchCandidate<'gc> {
    kind: SwitchKind,
    scrutinee: Operand<'gc>,
    cases: Vec<SwitchCase<'gc>>,
    default: BranchTarget<'gc>,
    chain_blocks: Vec<BlockId>,
    instruction_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SwitchNode<'gc> {
    kind: SwitchKind,
    scrutinee: Operand<'gc>,
    value: SwitchCaseValue<'gc>,
    target: BranchTarget<'gc>,
    next: BranchTarget<'gc>,
    removed_blocks: Vec<BlockId>,
    instruction_count: usize,
}

pub(crate) fn infer_switches<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    loop {
        let predecessors = local_predecessor_counts(&procedure);
        // Build the block index once per iteration. It must not be rebuilt per
        // `find_map` attempt: `infer_switch_candidate` may be tried against
        // many blocks before the first candidate, and a fresh O(blocks) map per
        // attempt is quadratic on huge procedures
        let blocks: HashMap<_, _> = procedure
            .blocks
            .iter()
            .map(|block| (block.id, block))
            .collect();
        let Some(candidate) = procedure.blocks.iter().find_map(|block| {
            infer_switch_candidate(&blocks, &procedure.blocks, block.id, &predecessors)
        }) else {
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
    blocks: &HashMap<BlockId, &Block<'gc>>,
    procedure_blocks: &[Block<'gc>],
    start: BlockId,
    predecessors: &HashMap<BlockId, usize>,
) -> Option<SwitchCandidate<'gc>> {
    let first = switch_node(blocks, predecessors, blocks[&start], true)?;
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

    while let Some((block, skipped_blocks)) = switch_chain_next_block(blocks, predecessors, &next) {
        let Some(node) = blocks
            .get(&block)
            .and_then(|block| switch_node(blocks, predecessors, block, false))
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
    let removed_defs = switch_removed_defs(blocks, start, &chain_blocks, instruction_count);
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
            procedure_blocks,
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
    let BranchTarget::Local {
        block,
        edge_assigns,
    } = next
    else {
        return None;
    };
    if !edge_assigns.is_empty() {
        return None;
    }
    if predecessors.get(block).copied().unwrap_or(0) != 1 {
        return None;
    }

    let mut block = *block;
    let mut skipped_blocks = Vec::new();
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(block) {
            // Empty jump cycle, stop rather than spinning forever.
            return None;
        }
        let current = blocks[&block];
        if switch_node(blocks, predecessors, current, false).is_some() {
            return Some((block, skipped_blocks));
        }

        let Terminator::Jump { target } = &current.terminator else {
            return Some((block, skipped_blocks));
        };
        if !current.instructions.is_empty() || predecessors.get(target).copied().unwrap_or(0) != 1 {
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
        .or_else(|| switch_branch_prim_node(block, allow_prefix))
        .or_else(|| switch_jump_branch_prim_node(blocks, predecessors, block, allow_prefix))
}

fn switch_direct_node<'gc>(block: &Block<'gc>, allow_prefix: bool) -> Option<SwitchNode<'gc>> {
    let Terminator::Branch {
        test: Operand::Local(test),
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
    let Terminator::Jump { target } = &block.terminator else {
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
        test: Operand::Local(branch_test),
        consequent,
        alternative,
        hints: _,
    } = &branch.terminator
    else {
        return None;
    };

    // Mutable uvars: the test home is shared, no edge-arg remapping.
    let mut node = switch_test_node(block, *branch_test, consequent, alternative, allow_prefix)?;
    node.removed_blocks.push(*target);
    Some(node)
}

fn switch_test_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if let Some(call) = switch_prim_call(block, test, allow_prefix) {
        if let Some(node) = switch_direct_comparison_node(&call, consequent, alternative) {
            return Some(node);
        }
    }
    switch_char_node(block, test, consequent, alternative, allow_prefix)
}

fn switch_branch_prim_node<'gc>(block: &Block<'gc>, allow_prefix: bool) -> Option<SwitchNode<'gc>> {
    let Terminator::BranchPrim {
        prim,
        args,
        consequent,
        alternative,
        hints: _,
    } = &block.terminator
    else {
        return None;
    };
    switch_branch_prim_test_node(
        &block.instructions,
        *prim,
        args,
        consequent,
        alternative,
        allow_prefix,
    )
}

fn switch_jump_branch_prim_node<'gc>(
    blocks: &HashMap<BlockId, &Block<'gc>>,
    predecessors: &HashMap<BlockId, usize>,
    block: &Block<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    let Terminator::Jump { target } = &block.terminator else {
        return None;
    };
    if predecessors.get(target).copied().unwrap_or(0) != 1 {
        return None;
    }

    let branch = blocks[target];
    if !branch.instructions.is_empty() {
        return None;
    }
    let Terminator::BranchPrim {
        prim,
        args,
        consequent,
        alternative,
        hints: _,
    } = &branch.terminator
    else {
        return None;
    };

    // Mutable uvars: the test home is shared, no edge-arg remapping.
    let mut node = switch_branch_prim_test_node(
        &block.instructions,
        *prim,
        args,
        consequent,
        alternative,
        allow_prefix,
    )?;
    node.removed_blocks.push(*target);
    Some(node)
}

fn switch_branch_prim_test_node<'gc>(
    instructions: &[Instruction<'gc>],
    prim: Primitive,
    args: &[Operand<'gc>],
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if let Some(call) = switch_branch_prim_call(instructions, prim, args, allow_prefix) {
        if let Some(node) = switch_direct_comparison_node(&call, consequent, alternative) {
            return Some(node);
        }
    }
    switch_char_branch_prim_node(
        instructions,
        prim,
        args,
        consequent,
        alternative,
        allow_prefix,
    )
}

fn switch_direct_comparison_node<'gc>(
    call: &SwitchPrimCall<'gc>,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    switch_fixnum_node(call, consequent, alternative)
        .or_else(|| switch_eq_char_node(call, consequent, alternative))
        .or_else(|| switch_eq_symbol_node(call, consequent, alternative))
}

fn switch_fixnum_node<'gc>(
    call: &SwitchPrimCall<'gc>,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    let kind = switch_kind_for_primitive(call.prim)?;
    if call.args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = fixnum_switch_test(call.args[0], call.args[1])?;
    Some(SwitchNode {
        kind,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count: call.instruction_count,
    })
}

fn switch_kind_for_primitive(prim: Primitive) -> Option<SwitchKind> {
    match prim {
        Primitive::IsEq | Primitive::IsEqv | Primitive::IsEqual => Some(SwitchKind::Eq),
        Primitive::FxEq => Some(SwitchKind::Fixnum),
        Primitive::NumericEqual => Some(SwitchKind::Numeric),
        _ => None,
    }
}

fn fixnum_switch_test<'gc>(lhs: Operand<'gc>, rhs: Operand<'gc>) -> Option<(Operand<'gc>, i32)> {
    match (lhs, rhs) {
        (scrutinee, Operand::Constant(value)) if value.is_int32() => {
            Some((scrutinee, value.as_int32()))
        }
        (Operand::Constant(value), scrutinee) if value.is_int32() => {
            Some((scrutinee, value.as_int32()))
        }
        _ => None,
    }
}

fn switch_eq_char_node<'gc>(
    call: &SwitchPrimCall<'gc>,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    if !is_eq_like_primitive(call.prim) || call.args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = char_switch_test(call.args[0], call.args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::CharEq,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count: call.instruction_count,
    })
}

fn char_switch_test<'gc>(lhs: Operand<'gc>, rhs: Operand<'gc>) -> Option<(Operand<'gc>, i32)> {
    match (lhs, rhs) {
        (scrutinee, Operand::Constant(value)) if value.is_char() => {
            Some((scrutinee, value.char() as i32))
        }
        (Operand::Constant(value), scrutinee) if value.is_char() => {
            Some((scrutinee, value.char() as i32))
        }
        _ => None,
    }
}

fn switch_eq_symbol_node<'gc>(
    call: &SwitchPrimCall<'gc>,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    if !is_eq_like_primitive(call.prim) || call.args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = symbol_switch_test(call.args[0], call.args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::SymbolEq { mask: 0 },
        scrutinee,
        value,
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count: call.instruction_count,
    })
}

fn is_eq_like_primitive(prim: Primitive) -> bool {
    matches!(
        prim,
        Primitive::IsEq | Primitive::IsEqv | Primitive::IsEqual
    )
}

struct SwitchPrimCall<'gc> {
    prim: Primitive,
    args: Vec<Operand<'gc>>,
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

    let mut needed = prim_call_arg_locals(args);
    let (constants, const_count) = trailing_const_defs(
        &block.instructions[..block.instructions.len() - 1],
        &mut needed,
    );
    let instruction_count = 1 + const_count;

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

/// The fused-test twin of [`switch_prim_call`]: `memv`/`memq` over a short
/// quoted list expand to `eqv?`/`eq?` chains (`ex_memv`/`ex_memq`), and
/// `(if (prim args...) ...)` fuses into `BranchPrim` (`convert_branch_prim`),
/// so `case` reaches inference in this fused shape. The prim and its args
/// come from the terminator,
/// only trailing `Const` instructions are consumed.
fn switch_branch_prim_call<'gc>(
    instructions: &[Instruction<'gc>],
    prim: Primitive,
    args: &[Operand<'gc>],
    allow_prefix: bool,
) -> Option<SwitchPrimCall<'gc>> {
    let mut needed = prim_call_arg_locals(args);
    let (constants, instruction_count) = trailing_const_defs(instructions, &mut needed);

    if instructions.len() != instruction_count && !allow_prefix {
        return None;
    }

    let args = args
        .iter()
        .copied()
        .map(|arg| resolve_local_constant(arg, &constants))
        .collect();

    Some(SwitchPrimCall {
        prim,
        args,
        instruction_count,
    })
}

fn prim_call_arg_locals<'gc>(args: &[Operand<'gc>]) -> HashSet<ValueId> {
    args.iter()
        .filter_map(|arg| match arg {
            Operand::Local(value) => Some(*value),
            Operand::Constant(_) => None,
        })
        .collect::<HashSet<_>>()
}

/// Trailing `Const` definitions for locals in `needed`, walking back from
/// the end of `instructions`: the shared tail of both test shapes above.
/// Returns the resolved constants and how many trailing instructions they
/// occupy. Only trailing constants count: a mutable uvar may be rebound
/// by an earlier non-`Const`, which stops the walk.
fn trailing_const_defs<'gc>(
    instructions: &[Instruction<'gc>],
    needed: &mut HashSet<ValueId>,
) -> (HashMap<ValueId, Value<'gc>>, usize) {
    let mut constants = HashMap::new();
    let mut instruction_count = 0;
    for instruction in instructions.iter().rev() {
        match instruction {
            Instruction::Const { dst, value } if needed.remove(dst) => {
                constants.insert(*dst, *value);
                instruction_count += 1;
            }
            _ => break,
        }
    }
    (constants, instruction_count)
}

fn resolve_local_constant<'gc>(
    atom: Operand<'gc>,
    constants: &HashMap<ValueId, Value<'gc>>,
) -> Operand<'gc> {
    match atom {
        Operand::Local(value) => constants
            .get(&value)
            .copied()
            .map(Operand::Constant)
            .unwrap_or(atom),
        Operand::Constant(_) => atom,
    }
}

fn symbol_switch_test<'gc>(
    lhs: Operand<'gc>,
    rhs: Operand<'gc>,
) -> Option<(Operand<'gc>, SwitchCaseValue<'gc>)> {
    match (lhs, rhs) {
        (scrutinee, Operand::Constant(value)) if value.is::<Symbol>() => {
            let symbol = value.downcast::<Symbol<'_>>();
            Some((
                scrutinee,
                SwitchCaseValue::Symbol {
                    hash: symbol.hash.get(),
                    value,
                },
            ))
        }
        (Operand::Constant(value), scrutinee) if value.is::<Symbol>() => {
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

    if *scrutinee_prim != Primitive::CharToInteger
        || *case_prim != Primitive::CharToInteger
        || *cmp_prim != Primitive::NumericEqual
        || scrutinee_args.len() != 1
        || case_args.len() != 1
        || *cmp != test
        || cmp_args.len() != 2
    {
        return None;
    }

    let Operand::Constant(case_char) = case_args[0] else {
        return None;
    };
    if !case_char.is_char() {
        return None;
    }

    let compares_ints = matches!(
        (cmp_args[0], cmp_args[1]),
        (Operand::Local(lhs), Operand::Local(rhs))
            if lhs == *scrutinee_int && rhs == *case_int
    ) || matches!(
        (cmp_args[0], cmp_args[1]),
        (Operand::Local(lhs), Operand::Local(rhs))
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

/// `char=?` chains through `BranchPrim`: `char=?` becomes
/// `(= (char->integer a) (char->integer b))` (`ex_char_eq`), fused by
/// `convert_branch_prim`, so the two conversions sit in the block and the
/// `=` is the terminator. Either side may be the constant case char.
fn switch_char_branch_prim_node<'gc>(
    instructions: &[Instruction<'gc>],
    prim: Primitive,
    args: &[Operand<'gc>],
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if prim != Primitive::NumericEqual || args.len() != 2 {
        return None;
    }
    let (Operand::Local(lhs), Operand::Local(rhs)) = (args[0], args[1]) else {
        return None;
    };
    if instructions.len() < 2 {
        return None;
    }

    let tail = &instructions[instructions.len() - 2..];
    let (
        Instruction::PrimCall {
            dst: convert_lhs,
            prim: prim_lhs,
            args: args_lhs,
            ..
        },
        Instruction::PrimCall {
            dst: convert_rhs,
            prim: prim_rhs,
            args: args_rhs,
            ..
        },
    ) = (&tail[0], &tail[1])
    else {
        return None;
    };
    if *prim_lhs != Primitive::CharToInteger
        || *prim_rhs != Primitive::CharToInteger
        || args_lhs.len() != 1
        || args_rhs.len() != 1
        || !(*convert_lhs == lhs && *convert_rhs == rhs
            || *convert_lhs == rhs && *convert_rhs == lhs)
    {
        return None;
    }

    let mut needed = prim_call_arg_locals(&[args_lhs[0], args_rhs[0]]);
    let (constants, const_count) =
        trailing_const_defs(&instructions[..instructions.len() - 2], &mut needed);
    let instruction_count = 2 + const_count;
    if instructions.len() != instruction_count && !allow_prefix {
        return None;
    }

    let lhs_input = resolve_local_constant(args_lhs[0], &constants);
    let rhs_input = resolve_local_constant(args_rhs[0], &constants);
    let (scrutinee, case_char) = match (lhs_input, rhs_input) {
        (scrutinee, Operand::Constant(value)) if value.is_char() => (scrutinee, value),
        (Operand::Constant(value), scrutinee) if value.is_char() => (scrutinee, value),
        _ => return None,
    };

    Some(SwitchNode {
        kind: SwitchKind::Char,
        scrutinee,
        value: SwitchCaseValue::Integer(case_char.char() as i32),
        target: consequent.clone(),
        next: alternative.clone(),
        removed_blocks: Vec::new(),
        instruction_count,
    })
}

fn branch_target_mentions_removed<'gc>(
    target: &BranchTarget<'gc>,
    removed: &HashSet<BlockId>,
) -> bool {
    matches!(target, BranchTarget::Local { block, edge_assigns } if removed.contains(block))
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

fn atom_mentions_defs<'gc>(atom: Operand<'gc>, defs: &HashSet<ValueId>) -> bool {
    matches!(atom, Operand::Local(value) if defs.contains(&value))
}

fn branch_target_mentions_defs<'gc>(target: &BranchTarget<'gc>, defs: &HashSet<ValueId>) -> bool {
    target
        .uses()
        .iter()
        .any(|atom| matches!(atom, Operand::Local(value) if defs.contains(value)))
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
                .any(|atom| matches!(atom, Operand::Local(value) if defs.contains(&value)))
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
            .any(|atom| matches!(atom, Operand::Local(value) if defs.contains(&value)))
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
