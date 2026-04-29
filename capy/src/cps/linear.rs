use crate::{
    compiler::ssa::primitive::Primitive,
    cps::{
        ReifyInfo,
        term::{Atom, BranchHint, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::value::{Symbol, Value},
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinearAtom<'gc> {
    Constant(Value<'gc>),
    Local(ValueId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcedureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeId<'gc> {
    Function(FuncRef<'gc>),
    Continuation(ContRef<'gc>),
}

#[derive(Debug, Clone)]
pub struct LinearProgram<'gc> {
    pub entry: FuncRef<'gc>,
    pub procedures: Vec<Procedure<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Procedure<'gc> {
    pub code: CodeId<'gc>,
    pub kind: ProcedureKind,
    pub binding: ValueId,
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub meta: Value<'gc>,
    pub return_cont: Option<ValueId>,
    pub params: Vec<ValueId>,
    pub variadic: Option<ValueId>,
    pub free_vars: Vec<ValueId>,
    pub sources: HashMap<ValueId, LVarRef<'gc>>,
    pub entry: BlockId,
    pub blocks: Vec<Block<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub params: Vec<ValueId>,
    pub variadic: Option<ValueId>,
    pub instructions: Vec<Instruction<'gc>>,
    pub terminator: Terminator<'gc>,
    pub source: Value<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestPredicate {
    Null,
    Pair,
    List,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<'gc> {
    MakeClosure {
        dst: ValueId,
        code: CodeId<'gc>,
        kind: ClosureKind,
        free_count: usize,
    },
    ClosureRef {
        dst: ValueId,
        closure: LinearAtom<'gc>,
        index: usize,
    },
    ClosureSet {
        closure: LinearAtom<'gc>,
        index: usize,
        value: LinearAtom<'gc>,
    },
    PrimCall {
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    RestToList {
        dst: ValueId,
        rest: ValueId,
        source: Value<'gc>,
    },
    RestRef {
        dst: ValueId,
        rest: ValueId,
        index: usize,
        source: Value<'gc>,
    },
    RestLength {
        dst: ValueId,
        rest: ValueId,
        skip: usize,
        source: Value<'gc>,
    },
    RestPredicate {
        dst: ValueId,
        rest: ValueId,
        predicate: RestPredicate,
        skip: usize,
        source: Value<'gc>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchTarget<'gc> {
    Local {
        block: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Reified {
        continuation: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase<'gc> {
    pub value: SwitchCaseValue<'gc>,
    pub target: BranchTarget<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SwitchCaseValue<'gc> {
    Integer(i32),
    Symbol { hash: u64, value: Value<'gc> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwitchKind {
    Eq,
    Fixnum,
    Numeric,
    Char,
    CharEq,
    SymbolEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator<'gc> {
    Call {
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    TailCall {
        callee: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    Jump {
        target: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Branch {
        test: LinearAtom<'gc>,
        consequent: BranchTarget<'gc>,
        alternative: BranchTarget<'gc>,
        hints: [BranchHint; 2],
    },
    Switch {
        kind: SwitchKind,
        scrutinee: LinearAtom<'gc>,
        cases: Vec<SwitchCase<'gc>>,
        default: BranchTarget<'gc>,
    },
}

pub fn linearize<'gc>(reify: &ReifyInfo<'gc>) -> LinearProgram<'gc> {
    let mut procedures = Vec::new();

    for func in reify.functions.iter() {
        procedures.push(lower_rest_arguments(infer_switches(linearize_function(
            *func,
        ))));
    }

    for cont in reify.continuations.iter().filter(|cont| cont.reified.get()) {
        procedures.push(lower_rest_arguments(infer_switches(
            linearize_continuation(*cont),
        )));
    }

    LinearProgram {
        entry: reify.entrypoint,
        procedures,
    }
}

fn linearize_function<'gc>(func: FuncRef<'gc>) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        func.free_vars
            .get()
            .expect("function free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let binding = builder.value(func.binding);
    let return_cont = builder.value(func.return_cont);
    let params = builder.values(func.args);
    let variadic = func.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        func.body(),
    );
    let (blocks, sources) = builder.finish();

    Procedure {
        code: CodeId::Function(func),
        kind: ProcedureKind::Function,
        binding,
        name: func.name,
        source: func.source,
        meta: func.meta,
        return_cont: Some(return_cont),
        params,
        variadic,
        free_vars,
        entry,
        sources,
        blocks,
    }
}

fn linearize_continuation<'gc>(cont: ContRef<'gc>) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        cont.free_vars
            .get()
            .expect("continuation free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let binding = builder.value(cont.binding);
    let params = builder.values(cont.args);
    let variadic = cont.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        cont.body(),
    );
    let (blocks, sources) = builder.finish();

    Procedure {
        code: CodeId::Continuation(cont),
        kind: ProcedureKind::Continuation,
        binding,
        name: cont.name,
        source: cont.source,
        meta: cont.meta,
        return_cont: None,
        params,
        variadic,
        free_vars,
        entry,
        sources,
        blocks,
    }
}

fn vars_to_vec<'gc>(vars: crate::cps::term::Vars<'gc>) -> Vec<LVarRef<'gc>> {
    vars.iter().copied().collect()
}

fn primitive_from_value<'gc>(value: Value<'gc>) -> Primitive {
    let name = value
        .downcast::<crate::runtime::value::Symbol>()
        .to_string();
    Primitive::from_name(&name).unwrap_or_else(|| panic!("undefined primitive: {value}"))
}

fn params_with_variadic(mut args: Vec<ValueId>, variadic: Option<ValueId>) -> Vec<ValueId> {
    args.extend(variadic);
    args
}

fn closure_refs<'gc>(
    builder: &mut ProcedureBuilder<'gc>,
    binding: ValueId,
    free_vars: &[LVarRef<'gc>],
) -> Vec<Instruction<'gc>> {
    free_vars
        .iter()
        .enumerate()
        .map(|(index, free_var)| Instruction::ClosureRef {
            dst: builder.value(*free_var),
            closure: LinearAtom::Local(binding),
            index,
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RestAlias {
    rest: ValueId,
    skip: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RestRewrite {
    Cdr,
    Ref(RestAlias),
    Length(RestAlias),
    Predicate(RestAlias, RestPredicate),
}

fn lower_rest_arguments<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let Some(rest) = procedure.variadic else {
        return procedure;
    };

    let aliases = collect_rest_aliases(&procedure, rest);
    let loop_blocks = loop_blocks(&procedure);
    if has_incompatible_rest_use(&procedure, rest, &aliases, &loop_blocks) {
        insert_rest_to_list(&mut procedure, rest);
    } else {
        rewrite_rest_uses(&mut procedure, rest, &aliases);
    }

    procedure
}

fn collect_rest_aliases<'gc>(
    procedure: &Procedure<'gc>,
    rest: ValueId,
) -> HashMap<ValueId, RestAlias> {
    let mut aliases = HashMap::new();
    let mut changed = true;

    while changed {
        changed = false;
        for block in &procedure.blocks {
            for instruction in &block.instructions {
                let Instruction::PrimCall {
                    dst, prim, args, ..
                } = instruction
                else {
                    continue;
                };
                if *prim != Primitive::cdr || args.len() != 1 {
                    continue;
                }
                if let Some(alias) = rest_alias_for_atom(args[0], rest, &aliases) {
                    let next_alias = RestAlias {
                        rest: alias.rest,
                        skip: alias.skip + 1,
                    };
                    if aliases.insert(*dst, next_alias) != Some(next_alias) {
                        changed = true;
                    }
                }
            }
        }
    }

    aliases
}

fn rest_alias_for_atom<'gc>(
    atom: LinearAtom<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> Option<RestAlias> {
    let LinearAtom::Local(var) = atom else {
        return None;
    };
    if var == rest {
        return Some(RestAlias { rest, skip: 0 });
    }
    aliases.get(&var).copied()
}

fn rest_rewrite_for_prim<'gc>(
    prim: Primitive,
    args: &[LinearAtom<'gc>],
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> Option<RestRewrite> {
    if args.len() != 1 {
        return None;
    }
    let alias = rest_alias_for_atom(args[0], rest, aliases)?;
    match prim {
        Primitive::cdr => Some(RestRewrite::Cdr),
        Primitive::car => Some(RestRewrite::Ref(alias)),
        Primitive::length => Some(RestRewrite::Length(alias)),
        Primitive::is_null => Some(RestRewrite::Predicate(alias, RestPredicate::Null)),
        Primitive::is_pair => Some(RestRewrite::Predicate(alias, RestPredicate::Pair)),
        Primitive::is_list => Some(RestRewrite::Predicate(alias, RestPredicate::List)),
        _ => None,
    }
}

fn instruction_mentions_rest<'gc>(
    instruction: &Instruction<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> bool {
    instruction
        .uses()
        .iter()
        .any(|atom| rest_alias_for_atom(*atom, rest, aliases).is_some())
}

fn terminator_mentions_rest<'gc>(
    terminator: &Terminator<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> bool {
    terminator
        .uses()
        .iter()
        .any(|atom| rest_alias_for_atom(*atom, rest, aliases).is_some())
}

fn has_incompatible_rest_use<'gc>(
    procedure: &Procedure<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
    loop_blocks: &HashSet<BlockId>,
) -> bool {
    for block in &procedure.blocks {
        let is_loop_block = loop_blocks.contains(&block.id);
        for instruction in &block.instructions {
            let mentions_rest = instruction_mentions_rest(instruction, rest, aliases);
            if !mentions_rest {
                continue;
            }
            if is_loop_block {
                return true;
            }
            let compatible = match instruction {
                Instruction::PrimCall { prim, args, .. } => {
                    rest_rewrite_for_prim(*prim, args, rest, aliases).is_some()
                }
                _ => false,
            };
            if !compatible {
                return true;
            }
        }

        if terminator_mentions_rest(&block.terminator, rest, aliases) {
            return true;
        }
    }

    false
}

fn insert_rest_to_list<'gc>(procedure: &mut Procedure<'gc>, rest: ValueId) {
    let entry = procedure
        .blocks
        .iter_mut()
        .find(|block| block.id == procedure.entry)
        .expect("procedure should contain entry block");
    entry.instructions.insert(
        0,
        Instruction::RestToList {
            dst: rest,
            rest,
            source: entry.source,
        },
    );
}

fn rewrite_rest_uses<'gc>(
    procedure: &mut Procedure<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) {
    for block in &mut procedure.blocks {
        let mut lowered = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } => match rest_rewrite_for_prim(prim, &args, rest, aliases) {
                    Some(RestRewrite::Cdr) => {}
                    Some(RestRewrite::Ref(alias)) => lowered.push(Instruction::RestRef {
                        dst,
                        rest: alias.rest,
                        index: alias.skip,
                        source,
                    }),
                    Some(RestRewrite::Length(alias)) => lowered.push(Instruction::RestLength {
                        dst,
                        rest: alias.rest,
                        skip: alias.skip,
                        source,
                    }),
                    Some(RestRewrite::Predicate(alias, predicate)) => {
                        lowered.push(Instruction::RestPredicate {
                            dst,
                            rest: alias.rest,
                            predicate,
                            skip: alias.skip,
                            source,
                        })
                    }
                    None => lowered.push(Instruction::PrimCall {
                        dst,
                        prim,
                        args,
                        source,
                    }),
                },
                other => lowered.push(other),
            }
        }
        block.instructions = lowered;
    }
}

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
    instruction_count: usize,
}

fn infer_switches<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
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
    let first = switch_node(blocks[&start])?;
    let kind = first.kind;
    let scrutinee = first.scrutinee;
    let mut cases = vec![SwitchCase {
        value: first.value,
        target: first.target,
    }];
    let mut chain_blocks = vec![start];
    let instruction_count = first.instruction_count;
    let mut seen_values = HashSet::from([first.value]);
    let mut next = first.next;

    loop {
        let Some((block, skipped_blocks)) = switch_chain_next_block(&blocks, &predecessors, &next)
        else {
            break;
        };
        let Some(node) = blocks.get(&block).and_then(|block| switch_node(block)) else {
            break;
        };
        if node.kind != kind || node.scrutinee != scrutinee || !seen_values.insert(node.value) {
            break;
        }
        chain_blocks.extend(skipped_blocks);
        chain_blocks.push(block);
        cases.push(SwitchCase {
            value: node.value,
            target: node.target,
        });
        next = node.next;
    }

    if cases.len() < 2 {
        return None;
    }

    let removed = chain_blocks.iter().copied().skip(1).collect::<HashSet<_>>();
    let removed_defs = switch_removed_defs(
        &blocks,
        start,
        &chain_blocks,
        instruction_count,
    );
    if cases
        .iter()
        .any(|case| branch_target_mentions_removed(&case.target, &removed))
        || branch_target_mentions_removed(&next, &removed)
        || cases
            .iter()
            .any(|case| branch_target_mentions_defs(&case.target, &removed_defs))
        || branch_target_mentions_defs(&next, &removed_defs)
        || surviving_blocks_mention_defs(&procedure.blocks, start, &removed, &removed_defs, instruction_count)
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
        if switch_node(current).is_some() {
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

fn switch_node<'gc>(block: &Block<'gc>) -> Option<SwitchNode<'gc>> {
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
    switch_fixnum_node(block, test, consequent, alternative)
        .or_else(|| switch_eq_char_node(block, test, consequent, alternative))
        .or_else(|| switch_eq_symbol_node(block, test, consequent, alternative))
        .or_else(|| switch_char_node(block, test, consequent, alternative))
}

fn switch_fixnum_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 1 {
        return None;
    }
    let Instruction::PrimCall { dst, prim, args, .. } = &block.instructions[0] else {
        return None;
    };
    let kind = switch_kind_for_primitive(*prim)?;
    if *dst != test || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = fixnum_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        instruction_count: 1,
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
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 1 {
        return None;
    }
    let Instruction::PrimCall { dst, prim, args, .. } = &block.instructions[0] else {
        return None;
    };
    if *dst != test || !is_eq_like_primitive(*prim) || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = char_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::CharEq,
        scrutinee,
        value: SwitchCaseValue::Integer(value),
        target: consequent.clone(),
        next: alternative.clone(),
        instruction_count: 1,
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
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 1 {
        return None;
    }
    let Instruction::PrimCall { dst, prim, args, .. } = &block.instructions[0] else {
        return None;
    };
    if *dst != test || !is_eq_like_primitive(*prim) || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = symbol_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::SymbolEq,
        scrutinee,
        value,
        target: consequent.clone(),
        next: alternative.clone(),
        instruction_count: 1,
    })
}

fn is_eq_like_primitive(prim: Primitive) -> bool {
    matches!(
        prim,
        Primitive::is_eq | Primitive::is_eqv | Primitive::is_equal
    )
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

fn switch_char_node<'gc>(
    block: &Block<'gc>,
    test: ValueId,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 3 {
        return None;
    }

    let Instruction::PrimCall {
        dst: scrutinee_int,
        prim: scrutinee_prim,
        args: scrutinee_args,
        ..
    } = &block.instructions[0]
    else {
        return None;
    };
    let Instruction::PrimCall {
        dst: case_int,
        prim: case_prim,
        args: case_args,
        ..
    } = &block.instructions[1]
    else {
        return None;
    };
    let Instruction::PrimCall {
        dst: cmp,
        prim: cmp_prim,
        args: cmp_args,
        ..
    } = &block.instructions[2]
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
            defs.extend(block.instructions.iter().filter_map(Instruction::def));
        }
    }

    if let Some(block) = blocks.get(&start) {
        let consumed_start = block.instructions.len().saturating_sub(instruction_count);
        defs.extend(
            block.instructions[consumed_start..]
                .iter()
                .filter_map(Instruction::def),
        );
    }

    defs
}

fn branch_target_mentions_defs<'gc>(
    target: &BranchTarget<'gc>,
    defs: &HashSet<ValueId>,
) -> bool {
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

fn loop_blocks<'gc>(procedure: &Procedure<'gc>) -> HashSet<BlockId> {
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

struct ProcedureBuilder<'gc> {
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<LVarRef<'gc>, BlockId>,
    values: HashMap<LVarRef<'gc>, ValueId>,
    sources: HashMap<ValueId, LVarRef<'gc>>,
    next_value: u32,
    next_block: usize,
}

impl<'gc> ProcedureBuilder<'gc> {
    fn new() -> Self {
        Self {
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            values: HashMap::new(),
            sources: HashMap::new(),
            next_value: 0,
            next_block: 1,
        }
    }

    fn finish(mut self) -> (Vec<Block<'gc>>, HashMap<ValueId, LVarRef<'gc>>) {
        self.blocks.sort_by_key(|block| block.id.0);
        (self.blocks, self.sources)
    }

    fn value(&mut self, var: LVarRef<'gc>) -> ValueId {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = ValueId(self.next_value);
        self.next_value += 1;
        self.values.insert(var, id);
        self.sources.insert(id, var);
        id
    }

    fn values(&mut self, vars: crate::cps::term::Vars<'gc>) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn value_slice(&mut self, vars: &[LVarRef<'gc>]) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn atom(&mut self, atom: Atom<'gc>) -> LinearAtom<'gc> {
        match atom {
            Atom::Constant(value) => LinearAtom::Constant(value),
            Atom::Local(var) => LinearAtom::Local(self.value(var)),
        }
    }

    fn atoms(&mut self, args: crate::cps::term::Atoms<'gc>) -> Vec<LinearAtom<'gc>> {
        args.iter().copied().map(|atom| self.atom(atom)).collect()
    }

    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    fn convert_block(
        &mut self,
        id: BlockId,
        params: Vec<ValueId>,
        variadic: Option<ValueId>,
        mut instructions: Vec<Instruction<'gc>>,
        term: TermRef<'gc>,
    ) {
        let source = term.source();
        let terminator = self.convert_term(term, &mut instructions);
        self.blocks.push(Block {
            id,
            params,
            variadic,
            instructions,
            terminator,
            source,
        });
    }

    fn convert_term(
        &mut self,
        term: TermRef<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match *term {
            Term::Let(var, Expression::PrimCall(prim, args, source), next) => {
                let prim = primitive_from_value(prim);
                let args = self.atoms(args);
                let dst = self.value(var);
                instructions.push(Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                });
                self.convert_term(next, instructions)
            }

            Term::Fix(funcs, next) => {
                for func in funcs.iter() {
                    let free_vars = vars_to_vec(
                        func.free_vars
                            .get()
                            .expect("function free vars are reified"),
                    );
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(func.binding),
                        code: CodeId::Function(*func),
                        kind: ClosureKind::Function,
                        free_count: free_vars.len(),
                    });
                }

                for func in funcs.iter() {
                    let closure = self.value(func.binding);
                    let free_vars = vars_to_vec(
                        func.free_vars
                            .get()
                            .expect("function free vars are reified"),
                    );
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                self.convert_term(next, instructions)
            }

            Term::Letk(conts, next) => {
                let local_conts: Vec<_> = conts
                    .iter()
                    .filter(|cont| !cont.reified.get())
                    .copied()
                    .collect();

                for cont in conts.iter().filter(|cont| cont.reified.get()) {
                    let free_vars = vars_to_vec(
                        cont.free_vars
                            .get()
                            .expect("continuation free vars are reified"),
                    );
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(cont.binding),
                        code: CodeId::Continuation(*cont),
                        kind: ClosureKind::Continuation,
                        free_count: free_vars.len(),
                    });
                }

                for cont in conts.iter().filter(|cont| cont.reified.get()) {
                    let closure = self.value(cont.binding);
                    let free_vars = vars_to_vec(
                        cont.free_vars
                            .get()
                            .expect("continuation free vars are reified"),
                    );
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                for cont in &local_conts {
                    let id = self.alloc_block();
                    self.local_blocks.insert(cont.binding, id);
                }

                for cont in local_conts {
                    let id = self.local_blocks[&cont.binding];
                    let params = self.values(cont.args);
                    let variadic = cont.variadic.map(|var| self.value(var));
                    self.convert_block(
                        id,
                        params_with_variadic(params, variadic),
                        variadic,
                        vec![],
                        cont.body(),
                    );
                }

                self.convert_term(next, instructions)
            }

            Term::Continue(k, args, source) => {
                if let Some(target) = self.local_blocks.get(&k) {
                    Terminator::Jump {
                        target: *target,
                        args: self.atoms(args),
                    }
                } else {
                    Terminator::TailCall {
                        callee: LinearAtom::Local(self.value(k)),
                        args: self.atoms(args),
                        source,
                    }
                }
            }

            Term::App(callee, retk, args, source) => Terminator::Call {
                callee: self.atom(callee),
                retk: LinearAtom::Local(self.value(retk)),
                args: self.atoms(args),
                source,
            },

            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints,
            } => Terminator::Branch {
                test: self.atom(test),
                consequent: self.branch_target(consequent, consequent_args),
                alternative: self.branch_target(alternative, alternative_args),
                hints,
            },
        }
    }

    fn branch_target(
        &mut self,
        continuation: LVarRef<'gc>,
        args: Option<crate::cps::term::Atoms<'gc>>,
    ) -> BranchTarget<'gc> {
        let args = args.map(|args| self.atoms(args)).unwrap_or_default();
        if let Some(block) = self.local_blocks.get(&continuation) {
            BranchTarget::Local {
                block: *block,
                args,
            }
        } else {
            BranchTarget::Reified {
                continuation: LinearAtom::Local(self.value(continuation)),
                args,
            }
        }
    }
}

fn emit_closure_sets<'gc>(
    builder: &mut ProcedureBuilder<'gc>,
    instructions: &mut Vec<Instruction<'gc>>,
    closure: ValueId,
    free_vars: &[LVarRef<'gc>],
) {
    for (index, free_var) in free_vars.iter().enumerate() {
        instructions.push(Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index,
            value: LinearAtom::Local(builder.value(*free_var)),
        });
    }
}

impl<'gc> Instruction<'gc> {
    pub fn def(&self) -> Option<ValueId> {
        match self {
            Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. } => Some(*dst),
            Self::ClosureSet { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::MakeClosure { .. } => vec![],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::PrimCall { args, .. } => args.clone(),
            Self::RestToList { .. } => vec![],
            Self::RestRef { rest, .. }
            | Self::RestLength { rest, .. }
            | Self::RestPredicate { rest, .. } => vec![LinearAtom::Local(*rest)],
        }
    }
}

impl<'gc> BranchTarget<'gc> {
    pub fn local_successor(&self) -> Option<BlockId> {
        match self {
            Self::Local { block, .. } => Some(*block),
            Self::Reified { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Local { args, .. } => args.clone(),
            Self::Reified { continuation, args } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*continuation);
                uses.extend(args.iter().copied());
                uses
            }
        }
    }
}

impl<'gc> Terminator<'gc> {
    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Call {
                callee, retk, args, ..
            } => {
                let mut uses = Vec::with_capacity(args.len() + 2);
                uses.push(*callee);
                uses.push(*retk);
                uses.extend(args.iter().copied());
                uses
            }
            Self::TailCall { callee, args, .. } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*callee);
                uses.extend(args.iter().copied());
                uses
            }
            Self::Jump { args, .. } => args.clone(),
            Self::Branch {
                test,
                consequent,
                alternative,
                ..
            } => {
                let mut uses =
                    Vec::with_capacity(1 + consequent.uses().len() + alternative.uses().len());
                uses.push(*test);
                uses.extend(consequent.uses());
                uses.extend(alternative.uses());
                uses
            }
            Self::Switch {
                kind: _,
                scrutinee,
                cases,
                default,
            } => {
                let mut uses = Vec::with_capacity(
                    1 + default.uses().len()
                        + cases
                            .iter()
                            .map(|case| case.target.uses().len())
                            .sum::<usize>(),
                );
                uses.push(*scrutinee);
                for case in cases {
                    uses.extend(case.target.uses());
                }
                uses.extend(default.uses());
                uses
            }
        }
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Self::Call { .. } | Self::TailCall { .. } => vec![],
            Self::Jump { target, .. } => vec![*target],
            Self::Branch {
                consequent,
                alternative,
                ..
            } => [consequent.local_successor(), alternative.local_successor()]
                .into_iter()
                .flatten()
                .collect(),
            Self::Switch { cases, default, .. } => cases
                .iter()
                .filter_map(|case| case.target.local_successor())
                .chain(default.local_successor())
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::ssa::primitive::Primitive,
        cps::{
            ReifyInfo,
            free_vars::FreeVars,
            term::{Atom, Cont, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[Atom<'gc>]) -> crate::cps::term::Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    fn vars<'gc>(ctx: Context<'gc>, vars: &[LVarRef<'gc>]) -> crate::cps::term::Vars<'gc> {
        Array::from_slice(*ctx, vars)
    }

    fn empty_term<'gc>(ctx: Context<'gc>, k: LVarRef<'gc>) -> Gc<'gc, Term<'gc>> {
        Gc::new(*ctx, Term::Continue(k, atoms(ctx, &[]), Value::new(false)))
    }

    fn mk_func<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        retk: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: Gc<'gc, Term<'gc>>,
        free_vars: &[LVarRef<'gc>],
    ) -> Gc<'gc, Func<'gc>> {
        mk_func_with_variadic(ctx, name, binding, retk, args, None, body, Some(free_vars))
    }

    fn mk_func_with_variadic<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        retk: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
        free_vars: Option<&[LVarRef<'gc>]>,
    ) -> Gc<'gc, Func<'gc>> {
        Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, name).into(),
                source: Value::new(false),
                binding,
                return_cont: retk,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                free_vars: Lock::new(free_vars.map(|free_vars| vars(ctx, free_vars))),
                meta: Value::new(false),
            },
        )
    }

    fn mk_cont<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: Gc<'gc, Term<'gc>>,
        free_vars: &[LVarRef<'gc>],
        reified: bool,
    ) -> Gc<'gc, Cont<'gc>> {
        mk_cont_with_variadic(
            ctx,
            name,
            binding,
            args,
            None,
            body,
            Some(free_vars),
            reified,
        )
    }

    fn mk_cont_with_variadic<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
        free_vars: Option<&[LVarRef<'gc>]>,
        reified: bool,
    ) -> Gc<'gc, Cont<'gc>> {
        Gc::new(
            *ctx,
            Cont {
                name: Symbol::from_str(ctx, name).into(),
                binding,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                source: Value::new(false),
                free_vars: Lock::new(free_vars.map(|free_vars| vars(ctx, free_vars))),
                reified: std::cell::Cell::new(reified),
                cold: false,
                noinline: false,
                meta: Value::new(false),
            },
        )
    }

    fn reify_info<'gc>(
        ctx: Context<'gc>,
        entrypoint: Gc<'gc, Func<'gc>>,
        functions: &[Gc<'gc, Func<'gc>>],
        continuations: &[Gc<'gc, Cont<'gc>>],
    ) -> ReifyInfo<'gc> {
        let mut free_vars = FreeVars::new();
        for func in functions {
            free_vars.funcs.insert(func.binding, *func);
        }
        for cont in continuations {
            free_vars.conts.insert(cont.binding, *cont);
        }

        ReifyInfo {
            entrypoint,
            functions: Array::from_slice(*ctx, functions),
            continuations: Array::from_slice(*ctx, continuations),
            free_vars,
        }
    }

    fn procedure<'gc>(program: &LinearProgram<'gc>, code: CodeId<'gc>) -> Procedure<'gc> {
        program
            .procedures
            .iter()
            .find(|procedure| procedure.code == code)
            .cloned()
            .expect("procedure exists")
    }

    #[test]
    fn instruction_defs_and_uses_are_explicit() {
        let dst = ValueId(1);
        let closure = ValueId(2);
        let value = LinearAtom::Local(ValueId(3));
        let instr = Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index: 0,
            value,
        };

        assert_eq!(instr.def(), None);
        assert_eq!(
            instr.uses(),
            vec![LinearAtom::Local(closure), LinearAtom::Local(ValueId(3))]
        );

        let instr = Instruction::ClosureRef {
            dst,
            closure: LinearAtom::Local(closure),
            index: 1,
        };
        assert_eq!(instr.def(), Some(dst));
        assert_eq!(instr.uses(), vec![LinearAtom::Local(closure)]);
    }

    #[test]
    fn terminator_successors_are_explicit() {
        let then_block = BlockId(1);
        let else_block = BlockId(2);
        let term = Terminator::Branch {
            test: LinearAtom::Local(ValueId(9)),
            consequent: BranchTarget::Local {
                block: then_block,
                args: vec![],
            },
            alternative: BranchTarget::Local {
                block: else_block,
                args: vec![],
            },
            hints: [crate::cps::term::BranchHint::Normal; 2],
        };

        assert_eq!(term.successors(), vec![then_block, else_block]);
    }

    #[test]
    fn linear_values_are_ids_not_source_lvars() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let value = lvar(ctx, "value");
            let entry = mk_func(
                ctx,
                "entry",
                f,
                retk,
                &[value],
                Gc::new(
                    *ctx,
                    Term::Continue(retk, atoms(ctx, &[Atom::Local(value)]), Value::new(false)),
                ),
                &[],
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(entry_proc.binding, ValueId(0));
            assert_eq!(entry_proc.return_cont, Some(ValueId(1)));
            assert_eq!(entry_block.params, vec![ValueId(2)]);
            assert_eq!(
                entry_block.terminator,
                Terminator::TailCall {
                    callee: LinearAtom::Local(ValueId(1)),
                    args: vec![LinearAtom::Local(ValueId(2))],
                    source: Value::new(false),
                }
            );
        });
    }

    #[test]
    fn linear_primcalls_use_primitive_enum() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let out = lvar(ctx, "out");
            let prim = Symbol::from_str(ctx, "not").into();
            let body = Gc::new(
                *ctx,
                Term::Let(
                    out,
                    Expression::PrimCall(prim, atoms(ctx, &[Atom::Local(arg)]), Value::new(false)),
                    Gc::new(
                        *ctx,
                        Term::Continue(retk, atoms(ctx, &[Atom::Local(out)]), Value::new(false)),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(
                entry_block.instructions,
                vec![Instruction::PrimCall {
                    dst: ValueId(3),
                    prim: Primitive::not,
                    args: vec![LinearAtom::Local(ValueId(2))],
                    source: Value::new(false),
                }]
            );
        });
    }

    #[test]
    fn variadic_car_lowers_to_rest_ref() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let rest = lvar(ctx, "rest");
            let out = lvar(ctx, "out");
            let body = Gc::new(
                *ctx,
                Term::Let(
                    out,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "car").into(),
                        atoms(ctx, &[Atom::Local(rest)]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Continue(retk, atoms(ctx, &[Atom::Local(out)]), Value::new(false)),
                    ),
                ),
            );
            let entry =
                mk_func_with_variadic(ctx, "entry", f, retk, &[], Some(rest), body, Some(&[]));
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(
                entry_block.instructions,
                vec![Instruction::RestRef {
                    dst: ValueId(3),
                    rest: ValueId(2),
                    index: 0,
                    source: Value::new(false),
                }]
            );
        });
    }

    #[test]
    fn variadic_cadr_lowers_to_rest_ref_offset() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let rest = lvar(ctx, "rest");
            let tail = lvar(ctx, "tail");
            let out = lvar(ctx, "out");
            let body = Gc::new(
                *ctx,
                Term::Let(
                    tail,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "cdr").into(),
                        atoms(ctx, &[Atom::Local(rest)]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            out,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "car").into(),
                                atoms(ctx, &[Atom::Local(tail)]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Continue(
                                    retk,
                                    atoms(ctx, &[Atom::Local(out)]),
                                    Value::new(false),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let entry =
                mk_func_with_variadic(ctx, "entry", f, retk, &[], Some(rest), body, Some(&[]));
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(
                entry_block.instructions,
                vec![Instruction::RestRef {
                    dst: ValueId(4),
                    rest: ValueId(2),
                    index: 1,
                    source: Value::new(false),
                }]
            );
        });
    }

    #[test]
    fn variadic_length_and_predicates_lower_to_rest_ops() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let rest = lvar(ctx, "rest");
            let len = lvar(ctx, "len");
            let empty = lvar(ctx, "empty");
            let pair = lvar(ctx, "pair");
            let list = lvar(ctx, "list");
            let body = Gc::new(
                *ctx,
                Term::Let(
                    len,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "length").into(),
                        atoms(ctx, &[Atom::Local(rest)]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            empty,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "null?").into(),
                                atoms(ctx, &[Atom::Local(rest)]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    pair,
                                    Expression::PrimCall(
                                        Symbol::from_str(ctx, "pair?").into(),
                                        atoms(ctx, &[Atom::Local(rest)]),
                                        Value::new(false),
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Let(
                                            list,
                                            Expression::PrimCall(
                                                Symbol::from_str(ctx, "list?").into(),
                                                atoms(ctx, &[Atom::Local(rest)]),
                                                Value::new(false),
                                            ),
                                            Gc::new(
                                                *ctx,
                                                Term::Continue(
                                                    retk,
                                                    atoms(
                                                        ctx,
                                                        &[
                                                            Atom::Local(len),
                                                            Atom::Local(empty),
                                                            Atom::Local(pair),
                                                            Atom::Local(list),
                                                        ],
                                                    ),
                                                    Value::new(false),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let entry =
                mk_func_with_variadic(ctx, "entry", f, retk, &[], Some(rest), body, Some(&[]));
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(
                entry_block.instructions,
                vec![
                    Instruction::RestLength {
                        dst: ValueId(3),
                        rest: ValueId(2),
                        skip: 0,
                        source: Value::new(false),
                    },
                    Instruction::RestPredicate {
                        dst: ValueId(4),
                        rest: ValueId(2),
                        predicate: RestPredicate::Null,
                        skip: 0,
                        source: Value::new(false),
                    },
                    Instruction::RestPredicate {
                        dst: ValueId(5),
                        rest: ValueId(2),
                        predicate: RestPredicate::Pair,
                        skip: 0,
                        source: Value::new(false),
                    },
                    Instruction::RestPredicate {
                        dst: ValueId(6),
                        rest: ValueId(2),
                        predicate: RestPredicate::List,
                        skip: 0,
                        source: Value::new(false),
                    },
                ]
            );
        });
    }

    #[test]
    fn incompatible_variadic_use_emits_rest_to_list() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let rest = lvar(ctx, "rest");
            let entry = mk_func_with_variadic(
                ctx,
                "entry",
                f,
                retk,
                &[],
                Some(rest),
                Gc::new(
                    *ctx,
                    Term::Continue(retk, atoms(ctx, &[Atom::Local(rest)]), Value::new(false)),
                ),
                Some(&[]),
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                entry_block.instructions.first(),
                Some(Instruction::RestToList {
                    dst: ValueId(2),
                    rest: ValueId(2),
                    source,
                }) if *source == Value::new(false)
            ));
        });
    }

    #[test]
    fn variadic_use_in_loop_emits_rest_to_list() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let rest = lvar(ctx, "rest");
            let loop_k = lvar(ctx, "loop");
            let len = lvar(ctx, "len");
            let loop_cont = mk_cont(
                ctx,
                "loop",
                loop_k,
                &[],
                Gc::new(
                    *ctx,
                    Term::Let(
                        len,
                        Expression::PrimCall(
                            Symbol::from_str(ctx, "length").into(),
                            atoms(ctx, &[Atom::Local(rest)]),
                            Value::new(false),
                        ),
                        Gc::new(
                            *ctx,
                            Term::Continue(loop_k, atoms(ctx, &[]), Value::new(false)),
                        ),
                    ),
                ),
                &[],
                false,
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[loop_cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(loop_k, atoms(ctx, &[]), Value::new(false)),
                    ),
                ),
            );
            let entry =
                mk_func_with_variadic(ctx, "entry", f, retk, &[], Some(rest), body, Some(&[]));
            let reify = reify_info(ctx, entry, &[entry], &[loop_cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                entry_block.instructions.first(),
                Some(Instruction::RestToList {
                    dst: ValueId(2),
                    rest: ValueId(2),
                    source,
                }) if *source == Value::new(false)
            ));
        });
    }

    #[test]
    fn fixnum_equality_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(
                ctx,
                "k1",
                k1,
                &[],
                Gc::new(
                    *ctx,
                    Term::Continue(
                        retk,
                        atoms(ctx, &[Atom::Constant(Value::new(10i32))]),
                        Value::new(false),
                    ),
                ),
                &[],
                false,
            );
            let cont2 = mk_cont(
                ctx,
                "k2",
                k2,
                &[],
                Gc::new(
                    *ctx,
                    Term::Continue(
                        retk,
                        atoms(ctx, &[Atom::Constant(Value::new(20i32))]),
                        Value::new(false),
                    ),
                ),
                &[],
                false,
            );
            let cont_else = mk_cont(
                ctx,
                "kelse",
                kelse,
                &[],
                Gc::new(
                    *ctx,
                    Term::Continue(
                        retk,
                        atoms(ctx, &[Atom::Constant(Value::new(30i32))]),
                        Value::new(false),
                    ),
                ),
                &[],
                false,
            );
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eq?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(2i32))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(1i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![SwitchCaseValue::Integer(1), SwitchCaseValue::Integer(2)]
            ));
        });
    }

    #[test]
    fn char_equality_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let arg_int1 = lvar(ctx, "arg-int1");
            let char_int1 = lvar(ctx, "char-int1");
            let cmp1 = lvar(ctx, "cmp1");
            let arg_int2 = lvar(ctx, "arg-int2");
            let char_int2 = lvar(ctx, "char-int2");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    arg_int2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "char->integer").into(),
                        atoms(ctx, &[Atom::Local(arg)]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            char_int2,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "char->integer").into(),
                                atoms(ctx, &[Atom::Constant(Value::from_char('b'))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    cmp2,
                                    Expression::PrimCall(
                                        Symbol::from_str(ctx, "=").into(),
                                        atoms(ctx, &[Atom::Local(arg_int2), Atom::Local(char_int2)]),
                                        Value::new(false),
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::If {
                                            test: Atom::Local(cmp2),
                                            consequent: k2,
                                            consequent_args: None,
                                            alternative: kelse,
                                            alternative_args: None,
                                            hints: [BranchHint::Normal; 2],
                                        },
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            arg_int1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "char->integer").into(),
                                atoms(ctx, &[Atom::Local(arg)]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    char_int1,
                                    Expression::PrimCall(
                                        Symbol::from_str(ctx, "char->integer").into(),
                                        atoms(ctx, &[Atom::Constant(Value::from_char('a'))]),
                                        Value::new(false),
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Let(
                                            cmp1,
                                            Expression::PrimCall(
                                                Symbol::from_str(ctx, "=").into(),
                                                atoms(
                                                    ctx,
                                                    &[
                                                        Atom::Local(arg_int1),
                                                        Atom::Local(char_int1),
                                                    ],
                                                ),
                                                Value::new(false),
                                            ),
                                            Gc::new(
                                                *ctx,
                                                Term::If {
                                                    test: Atom::Local(cmp1),
                                                    consequent: k1,
                                                    consequent_args: None,
                                                    alternative: kcheck2,
                                                    alternative_args: None,
                                                    hints: [BranchHint::Normal; 2],
                                                },
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::Char,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Integer('a' as i32),
                        SwitchCaseValue::Integer('b' as i32)
                    ]
            ));
            assert!(entry_block.instructions.is_empty());
        });
    }

    #[test]
    fn switch_inference_rejects_targets_using_removed_defs() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let arg_int1 = lvar(ctx, "arg-int1");
            let char_int1 = lvar(ctx, "char-int1");
            let cmp1 = lvar(ctx, "cmp1");
            let arg_int2 = lvar(ctx, "arg-int2");
            let char_int2 = lvar(ctx, "char-int2");
            let cmp2 = lvar(ctx, "cmp2");
            let result = lvar(ctx, "result");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(
                ctx,
                "k2",
                k2,
                &[result],
                Gc::new(
                    *ctx,
                    Term::Continue(retk, atoms(ctx, &[Atom::Local(result)]), Value::new(false)),
                ),
                &[],
                false,
            );
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    arg_int2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "char->integer").into(),
                        atoms(ctx, &[Atom::Local(arg)]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            char_int2,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "char->integer").into(),
                                atoms(ctx, &[Atom::Constant(Value::from_char('b'))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    cmp2,
                                    Expression::PrimCall(
                                        Symbol::from_str(ctx, "=").into(),
                                        atoms(ctx, &[Atom::Local(arg_int2), Atom::Local(char_int2)]),
                                        Value::new(false),
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::If {
                                            test: Atom::Local(cmp2),
                                            consequent: k2,
                                            consequent_args: Some(atoms(ctx, &[Atom::Local(arg_int2)])),
                                            alternative: kelse,
                                            alternative_args: None,
                                            hints: [BranchHint::Normal; 2],
                                        },
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            arg_int1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "char->integer").into(),
                                atoms(ctx, &[Atom::Local(arg)]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    char_int1,
                                    Expression::PrimCall(
                                        Symbol::from_str(ctx, "char->integer").into(),
                                        atoms(ctx, &[Atom::Constant(Value::from_char('a'))]),
                                        Value::new(false),
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Let(
                                            cmp1,
                                            Expression::PrimCall(
                                                Symbol::from_str(ctx, "=").into(),
                                                atoms(
                                                    ctx,
                                                    &[
                                                        Atom::Local(arg_int1),
                                                        Atom::Local(char_int1),
                                                    ],
                                                ),
                                                Value::new(false),
                                            ),
                                            Gc::new(
                                                *ctx,
                                                Term::If {
                                                    test: Atom::Local(cmp1),
                                                    consequent: k1,
                                                    consequent_args: None,
                                                    alternative: kcheck2,
                                                    alternative_args: None,
                                                    hints: [BranchHint::Normal; 2],
                                                },
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(entry_block.terminator, Terminator::Branch { .. }));
        });
    }

    #[test]
    fn eq_char_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eq?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('b'))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('a'))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::CharEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Integer('a' as i32),
                        SwitchCaseValue::Integer('b' as i32)
                    ]
            ));
        });
    }

    #[test]
    fn eqv_char_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eqv?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('b'))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eqv?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('a'))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::CharEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Integer('a' as i32),
                        SwitchCaseValue::Integer('b' as i32)
                    ]
            ));
        });
    }

    #[test]
    fn equal_char_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "equal?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('b'))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "equal?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::from_char('a'))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::CharEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Integer('a' as i32),
                        SwitchCaseValue::Integer('b' as i32)
                    ]
            ));
        });
    }

    #[test]
    fn eq_symbol_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");
            let foo = Symbol::from_str(ctx, "foo");
            let bar = Symbol::from_str(ctx, "bar");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eq?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(bar.into())]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(foo.into())]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::SymbolEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Symbol {
                            hash: foo.hash.get(),
                            value: foo.into(),
                        },
                        SwitchCaseValue::Symbol {
                            hash: bar.hash.get(),
                            value: bar.into(),
                        }
                    ]
            ));
        });
    }

    #[test]
    fn equal_symbol_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");
            let foo = Symbol::from_str(ctx, "foo");
            let bar = Symbol::from_str(ctx, "bar");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "equal?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(bar.into())]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "equal?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(foo.into())]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::SymbolEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Symbol {
                            hash: foo.hash.get(),
                            value: foo.into(),
                        },
                        SwitchCaseValue::Symbol {
                            hash: bar.hash.get(),
                            value: bar.into(),
                        }
                    ]
            ));
        });
    }

    #[test]
    fn eqv_symbol_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");
            let foo = Symbol::from_str(ctx, "foo");
            let bar = Symbol::from_str(ctx, "bar");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eqv?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(bar.into())]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eqv?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(foo.into())]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::SymbolEq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![
                        SwitchCaseValue::Symbol {
                            hash: foo.hash.get(),
                            value: foo.into(),
                        },
                        SwitchCaseValue::Symbol {
                            hash: bar.hash.get(),
                            value: bar.into(),
                        }
                    ]
            ));
        });
    }

    #[test]
    fn switch_inference_rejects_mixed_scrutinees() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg1 = lvar(ctx, "arg1");
            let arg2 = lvar(ctx, "arg2");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "eq?").into(),
                        atoms(ctx, &[Atom::Local(arg2), Atom::Constant(Value::new(2i32))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg1), Atom::Constant(Value::new(1i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg1, arg2], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(entry_block.terminator, Terminator::Branch { .. }));
        });
    }

    #[test]
    fn switch_inference_rejects_chain_blocks_with_extra_definitions() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let extra = lvar(ctx, "extra");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(
                ctx,
                "k2",
                k2,
                &[],
                Gc::new(
                    *ctx,
                    Term::Continue(retk, atoms(ctx, &[Atom::Local(extra)]), Value::new(false)),
                ),
                &[],
                false,
            );
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    extra,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "not").into(),
                        atoms(ctx, &[Atom::Constant(Value::new(false))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp2,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(2i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp2),
                                    consequent: k2,
                                    consequent_args: None,
                                    alternative: kelse,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "eq?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(1i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(entry_block.terminator, Terminator::Branch { .. }));
        });
    }

    #[test]
    fn numeric_fixnum_equality_branch_chain_lowers_to_switch() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "=").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(2i32))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "=").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(1i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kcheck2,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![SwitchCaseValue::Integer(1), SwitchCaseValue::Integer(2)]
            ));
        });
    }

    #[test]
    fn switch_inference_skips_jump_trampolines() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let cmp1 = lvar(ctx, "cmp1");
            let cmp2 = lvar(ctx, "cmp2");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let kelse = lvar(ctx, "kelse");
            let kskip = lvar(ctx, "kskip");
            let kcheck2 = lvar(ctx, "kcheck2");

            let cont1 = mk_cont(ctx, "k1", k1, &[], empty_term(ctx, retk), &[], false);
            let cont2 = mk_cont(ctx, "k2", k2, &[], empty_term(ctx, retk), &[], false);
            let cont_else = mk_cont(ctx, "kelse", kelse, &[], empty_term(ctx, retk), &[], false);
            let check2_body = Gc::new(
                *ctx,
                Term::Let(
                    cmp2,
                    Expression::PrimCall(
                        Symbol::from_str(ctx, "equal?").into(),
                        atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(2i32))]),
                        Value::new(false),
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(cmp2),
                            consequent: k2,
                            consequent_args: None,
                            alternative: kelse,
                            alternative_args: None,
                            hints: [BranchHint::Normal; 2],
                        },
                    ),
                ),
            );
            let cont_check2 = mk_cont(ctx, "kcheck2", kcheck2, &[], check2_body, &[], false);
            let skip_body = Gc::new(
                *ctx,
                Term::Continue(kcheck2, atoms(ctx, &[]), Value::new(false)),
            );
            let cont_skip = mk_cont(ctx, "kskip", kskip, &[], skip_body, &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont1, cont2, cont_else, cont_skip, cont_check2]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cmp1,
                            Expression::PrimCall(
                                Symbol::from_str(ctx, "equal?").into(),
                                atoms(ctx, &[Atom::Local(arg), Atom::Constant(Value::new(1i32))]),
                                Value::new(false),
                            ),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(cmp1),
                                    consequent: k1,
                                    consequent_args: None,
                                    alternative: kskip,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal; 2],
                                },
                            ),
                        ),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[arg], body, &[]);
            let reify = reify_info(
                ctx,
                entry,
                &[entry],
                &[cont1, cont2, cont_else, cont_skip, cont_check2],
            );

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(matches!(
                &entry_block.terminator,
                Terminator::Switch {
                    kind: SwitchKind::Eq,
                    scrutinee: LinearAtom::Local(ValueId(2)),
                    cases,
                    ..
                } if cases.iter().map(|case| case.value).collect::<Vec<_>>()
                    == vec![SwitchCaseValue::Integer(1), SwitchCaseValue::Integer(2)]
            ));
        });
    }

    #[test]
    fn linearize_creates_procedures_for_functions() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let entry = mk_func(ctx, "entry", f, retk, &[], empty_term(ctx, retk), &[]);
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);

            assert_eq!(program.entry, entry);
            assert!(
                program
                    .procedures
                    .iter()
                    .any(|procedure| procedure.code == CodeId::Function(entry)
                        && procedure.kind == ProcedureKind::Function)
            );
        });
    }

    #[test]
    fn function_entry_block_records_variadic_parameter() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let rest = lvar(ctx, "rest");
            let entry = mk_func_with_variadic(
                ctx,
                "entry",
                f,
                retk,
                &[arg],
                Some(rest),
                empty_term(ctx, retk),
                Some(&[]),
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(entry_block.params, vec![ValueId(2), ValueId(3)]);
            assert_eq!(entry_block.variadic, Some(ValueId(3)));
        });
    }

    #[test]
    fn non_reified_continuation_becomes_local_block() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let value = lvar(ctx, "value");
            let cont_arg = lvar(ctx, "cont-arg");
            let cont = mk_cont(ctx, "k", k, &[cont_arg], empty_term(ctx, retk), &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, atoms(ctx, &[Atom::Local(value)]), Value::new(false)),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));

            assert!(
                !program
                    .procedures
                    .iter()
                    .any(|procedure| procedure.code == CodeId::Continuation(cont))
            );

            let local_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id != entry_proc.entry)
                .expect("local continuation block");
            assert_eq!(local_block.params, vec![ValueId(2)]);

            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");
            assert_eq!(
                entry_block.terminator,
                Terminator::Jump {
                    target: local_block.id,
                    args: vec![LinearAtom::Local(ValueId(3))]
                }
            );
        });
    }

    #[test]
    fn local_continuation_block_records_variadic_metadata() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let cont_arg = lvar(ctx, "cont-arg");
            let rest = lvar(ctx, "rest");
            let cont = mk_cont_with_variadic(
                ctx,
                "k",
                k,
                &[cont_arg],
                Some(rest),
                empty_term(ctx, retk),
                Some(&[]),
                false,
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(*ctx, Term::Continue(k, atoms(ctx, &[]), Value::new(false))),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let local_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id != entry_proc.entry)
                .expect("local continuation block");

            assert_eq!(local_block.params, vec![ValueId(2), ValueId(3)]);
            assert_eq!(local_block.variadic, Some(ValueId(3)));
        });
    }

    #[test]
    fn reified_continuation_gets_make_closure() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let captured = lvar(ctx, "captured");
            let cont = mk_cont(ctx, "k", k, &[], empty_term(ctx, retk), &[captured], true);
            let body = Gc::new(
                *ctx,
                Term::Letk(Array::from_slice(*ctx, &[cont]), empty_term(ctx, retk)),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(
                entry_block
                    .instructions
                    .contains(&Instruction::MakeClosure {
                        dst: ValueId(2),
                        code: CodeId::Continuation(cont),
                        kind: ClosureKind::Continuation,
                        free_count: 1,
                    })
            );
            assert!(entry_block.instructions.contains(&Instruction::ClosureSet {
                closure: LinearAtom::Local(ValueId(2)),
                index: 0,
                value: LinearAtom::Local(ValueId(3)),
            }));
        });
    }

    #[test]
    fn function_fix_gets_closure_sets_for_free_vars() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let child_binding = lvar(ctx, "child");
            let child_retk = lvar(ctx, "child-retk");
            let captured = lvar(ctx, "captured");
            let child = mk_func(
                ctx,
                "child",
                child_binding,
                child_retk,
                &[],
                empty_term(ctx, child_retk),
                &[captured],
            );
            let body = Gc::new(
                *ctx,
                Term::Fix(Array::from_slice(*ctx, &[child]), empty_term(ctx, retk)),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry, child], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(
                entry_block
                    .instructions
                    .contains(&Instruction::MakeClosure {
                        dst: ValueId(2),
                        code: CodeId::Function(child),
                        kind: ClosureKind::Function,
                        free_count: 1,
                    })
            );
            assert!(entry_block.instructions.contains(&Instruction::ClosureSet {
                closure: LinearAtom::Local(ValueId(2)),
                index: 0,
                value: LinearAtom::Local(ValueId(3)),
            }));
        });
    }

    #[test]
    fn linearize_panics_when_function_free_vars_are_missing() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let entry = mk_func_with_variadic(
                ctx,
                "entry",
                f,
                retk,
                &[],
                None,
                empty_term(ctx, retk),
                None,
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                linearize(&reify);
            }));

            assert!(result.is_err());
        });
    }

    #[test]
    fn linearize_panics_when_reified_continuation_free_vars_are_missing() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let entry = mk_func(ctx, "entry", f, retk, &[], empty_term(ctx, retk), &[]);
            let cont =
                mk_cont_with_variadic(ctx, "k", k, &[], None, empty_term(ctx, retk), None, true);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                linearize(&reify);
            }));

            assert!(result.is_err());
        });
    }
}
