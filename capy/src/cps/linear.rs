use crate::{
    compiler::ssa::primitive::Primitive,
    cps::{
        ReifyInfo,
        term::{Atom, BranchHint, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::{
        value::{Str, Symbol, Tuple, TypeCode8, Value, Vector},
        vm::exceptions::RaiseKind,
    },
};
use smallvec::{SmallVec, smallvec};
use std::{
    collections::{HashMap, HashSet},
    mem::{offset_of, size_of},
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LowType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Value,
    Ptr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LowPrim {
    HasType8(u8),
    IsFixnum,
    UntagFixnum,
    TagFixnum,
    IReduce(LowType),
    SExt(LowType),
    ZExt(LowType),
    ICmp(LowIntPredicate, LowType),
    IAdd(LowType),
    ISub(LowType),
    IMul(LowType),
    IDiv(LowType),
    IRem(LowType),
    IShl(LowType),
    IShr(LowType),
    IAnd(LowType),
    IOr(LowType),
    IXor(LowType),
    IAddImm(LowType, i64),
    IMulImm(LowType, i64),
    IAddOverflow(LowType),
    ISubOverflow(LowType),
    IMulOverflow(LowType),
    Load { ty: LowType, offset: i32 },
    FAdd(LowType),
    FSub(LowType),
    FMul(LowType),
    FDiv(LowType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LowIntPredicate {
    Equal,
    NotEqual,
    SignedLessThan,
    SignedLessThanOrEqual,
    SignedGreaterThan,
    SignedGreaterThanOrEqual,
    UnsignedLessThan,
    UnsignedLessThanOrEqual,
    UnsignedGreaterThan,
    UnsignedGreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<'gc> {
    Const {
        dst: ValueId,
        value: Value<'gc>,
    },
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
    CacheRef {
        dst: ValueId,
        cache_key: LinearAtom<'gc>,
        source: Value<'gc>,
    },
    CacheSet {
        dst: ValueId,
        cache_key: LinearAtom<'gc>,
        value: LinearAtom<'gc>,
        source: Value<'gc>,
    },
    PrimCall {
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    LowPrimCall {
        dsts: SmallVec<[ValueId; 2]>,
        op: LowPrim,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    RuntimePrimCall {
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
    SymbolEq { mask: u64 },
}

const SYMBOL_HASH_DISPATCH_MIN_LENGTH: usize = 4;

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
    Raise {
        kind: RaiseKind,
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
        procedures.push(hoist_constants(lower_low_level_primitives(
            lower_cache_operations(lower_rest_arguments(infer_switches(linearize_function(
                *func,
            )))),
        )));
    }

    for cont in reify.continuations.iter().filter(|cont| cont.reified.get()) {
        procedures.push(hoist_constants(lower_low_level_primitives(
            lower_cache_operations(lower_rest_arguments(infer_switches(
                linearize_continuation(*cont),
            ))),
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
    let first = switch_node(blocks[&start], true)?;
    let mut kind = first.kind;
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
        let Some(node) = blocks
            .get(&block)
            .and_then(|block| switch_node(block, false))
        else {
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
        if switch_node(current, false).is_some() {
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

fn switch_node<'gc>(block: &Block<'gc>, allow_prefix: bool) -> Option<SwitchNode<'gc>> {
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
    if block.instructions.len() != 1 && !allow_prefix {
        return None;
    }
    let Instruction::PrimCall {
        dst, prim, args, ..
    } = block.instructions.last()?
    else {
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
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 1 && !allow_prefix {
        return None;
    }
    let Instruction::PrimCall {
        dst, prim, args, ..
    } = block.instructions.last()?
    else {
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
    allow_prefix: bool,
) -> Option<SwitchNode<'gc>> {
    if block.instructions.len() != 1 && !allow_prefix {
        return None;
    }
    let Instruction::PrimCall {
        dst, prim, args, ..
    } = block.instructions.last()?
    else {
        return None;
    };
    if *dst != test || !is_eq_like_primitive(*prim) || args.len() != 2 {
        return None;
    }
    let (scrutinee, value) = symbol_switch_test(args[0], args[1])?;
    Some(SwitchNode {
        kind: SwitchKind::SymbolEq { mask: 0 },
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

fn hoist_constants<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let mut hoister = ConstantHoister::new(&procedure);
    for block in &mut procedure.blocks {
        let mut instructions = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            let instruction = hoister.instruction(instruction, &mut instructions);
            instructions.push(instruction);
        }
        block.terminator = hoister.terminator(block.terminator.clone(), &mut instructions);
        block.instructions = instructions;
    }
    procedure
}

fn lower_cache_operations<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    for block in &mut procedure.blocks {
        let mut lowered = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim: Primitive::cache_ref,
                    args,
                    source,
                } => {
                    let [cache_key] = args.as_slice() else {
                        panic!("cache-ref expects 1 argument, got {}", args.len());
                    };
                    lowered.push(Instruction::CacheRef {
                        dst,
                        cache_key: *cache_key,
                        source,
                    });
                }
                Instruction::PrimCall {
                    dst,
                    prim: Primitive::cache_set,
                    args,
                    source,
                } => {
                    let [cache_key, value] = args.as_slice() else {
                        panic!("cache-set! expects 2 arguments, got {}", args.len());
                    };
                    lowered.push(Instruction::CacheSet {
                        dst,
                        cache_key: *cache_key,
                        value: *value,
                        source,
                    });
                }
                other => lowered.push(other),
            }
        }
        block.instructions = lowered;
    }
    procedure
}

struct LowLevelLowerer {
    next_value: u32,
    next_block: usize,
}

impl LowLevelLowerer {
    fn new<'gc>(procedure: &Procedure<'gc>) -> Self {
        let mut max_value = procedure.binding.0;
        if let Some(return_cont) = procedure.return_cont {
            max_value = max_value.max(return_cont.0);
        }
        let mut max_block = procedure.entry.0;
        for value in procedure
            .params
            .iter()
            .chain(procedure.variadic.iter())
            .chain(procedure.free_vars.iter())
            .copied()
        {
            max_value = max_value.max(value.0);
        }
        for block in &procedure.blocks {
            max_block = max_block.max(block.id.0);
            for value in block.params.iter().chain(block.variadic.iter()).copied() {
                max_value = max_value.max(value.0);
            }
            for instruction in &block.instructions {
                for def in instruction.defs() {
                    max_value = max_value.max(def.0);
                }
                for value in local_values(instruction.uses()) {
                    max_value = max_value.max(value.0);
                }
            }
            for value in local_values(block.terminator.uses()) {
                max_value = max_value.max(value.0);
            }
        }
        Self {
            next_value: max_value + 1,
            next_block: max_block + 1,
        }
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn fresh_block(&mut self) -> BlockId {
        let block = BlockId(self.next_block);
        self.next_block += 1;
        block
    }

    fn lower_procedure<'gc>(&mut self, mut procedure: Procedure<'gc>) -> Procedure<'gc> {
        let mut lowered = Vec::with_capacity(procedure.blocks.len());
        let blocks = std::mem::take(&mut procedure.blocks);
        for block in blocks {
            self.lower_block(block, &mut lowered);
        }
        procedure.blocks = lowered;
        procedure
    }

    fn lower_block<'gc>(&mut self, block: Block<'gc>, out: &mut Vec<Block<'gc>>) {
        let Block {
            id,
            params,
            variadic,
            instructions,
            terminator,
            source: block_source,
        } = block;

        let mut lowered = Vec::with_capacity(instructions.len());
        let mut iter = instructions.into_iter();
        while let Some(instruction) = iter.next() {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::vector_ref && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_indexed_ref_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        TypeCode8::VECTOR.bits(),
                        offset_of!(Vector, length) as i32,
                        Vector::OFFSET_OF_DATA as i64,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::tuple_ref && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_indexed_ref_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        TypeCode8::TUPLE.bits(),
                        offset_of!(Tuple, length) as i32,
                        offset_of!(Tuple, data) as i64,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::string_length && args.len() == 1 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_type_checked_length_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args[0],
                        TypeCode8::STRING.bits(),
                        offset_of!(Str, length) as i32,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if args.len() == 1 => {
                    if let Some(typecode) = low_type_predicate_prim(prim) {
                        lowered.push(Instruction::low_prim_call(
                            dst,
                            LowPrim::HasType8(typecode),
                            args,
                            source,
                        ));
                    } else {
                        lowered.push(Instruction::PrimCall {
                            dst,
                            prim,
                            args,
                            source,
                        });
                    }
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::plus && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_fixnum_binary_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        source,
                        LowPrim::IAddOverflow(LowType::I32),
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if args.len() == 2 => {
                    if let Some((ty, op)) = low_integer_binary_prim(prim) {
                        let lhs = self.fresh_value();
                        let rhs = self.fresh_value();
                        lowered.push(Instruction::low_prim_call(
                            lhs,
                            LowPrim::IReduce(ty),
                            vec![args[0]],
                            source,
                        ));
                        lowered.push(Instruction::low_prim_call(
                            rhs,
                            LowPrim::IReduce(ty),
                            vec![args[1]],
                            source,
                        ));
                        lowered.push(Instruction::low_prim_call(
                            dst,
                            op,
                            vec![LinearAtom::Local(lhs), LinearAtom::Local(rhs)],
                            source,
                        ));
                    } else {
                        lowered.push(Instruction::PrimCall {
                            dst,
                            prim,
                            args,
                            source,
                        });
                    }
                }
                other => lowered.push(other),
            }
        }

        out.push(Block {
            id,
            params,
            variadic,
            instructions: lowered,
            terminator,
            source: block_source,
        });
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_type_checked_length_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        arg: LinearAtom<'gc>,
        typecode: u8,
        length_offset: i32,
        source: Value<'gc>,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();
        let type_ok = self.fresh_value();
        let raw_len = self.fresh_value();
        let tagged_len = self.fresh_value();

        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            type_ok,
            LowPrim::HasType8(typecode),
            vec![arg],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(type_ok),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(
                    raw_len,
                    LowPrim::Load {
                        ty: LowType::I64,
                        offset: length_offset,
                    },
                    vec![arg],
                    source,
                ),
                Instruction::low_prim_call(
                    tagged_len,
                    LowPrim::TagFixnum,
                    vec![LinearAtom::Local(raw_len)],
                    source,
                ),
            ],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(tagged_len)],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args: vec![arg],
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_indexed_ref_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        typecode: u8,
        length_offset: i32,
        data_offset: i64,
        source: Value<'gc>,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let index_check = self.fresh_block();
        let bounds_check = self.fresh_block();
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();

        let type_ok = self.fresh_value();
        let index_is_fixnum = self.fresh_value();
        let raw_index = self.fresh_value();
        let index64 = self.fresh_value();
        let length = self.fresh_value();
        let in_bounds = self.fresh_value();
        let index_offset = self.fresh_value();
        let data_ptr = self.fresh_value();
        let elem_ptr = self.fresh_value();
        let elem = self.fresh_value();

        let vector = args[0];
        let index = args[1];
        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            type_ok,
            LowPrim::HasType8(typecode),
            vec![vector],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(type_ok),
                consequent: BranchTarget::Local {
                    block: index_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: index_check,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::low_prim_call(
                index_is_fixnum,
                LowPrim::IsFixnum,
                vec![index],
                source,
            )],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(index_is_fixnum),
                consequent: BranchTarget::Local {
                    block: bounds_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: bounds_check,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(raw_index, LowPrim::UntagFixnum, vec![index], source),
                Instruction::low_prim_call(
                    index64,
                    LowPrim::ZExt(LowType::I64),
                    vec![LinearAtom::Local(raw_index)],
                    source,
                ),
                Instruction::low_prim_call(
                    length,
                    LowPrim::Load {
                        ty: LowType::I64,
                        offset: length_offset,
                    },
                    vec![vector],
                    source,
                ),
                Instruction::low_prim_call(
                    in_bounds,
                    LowPrim::ICmp(LowIntPredicate::UnsignedLessThan, LowType::I64),
                    vec![LinearAtom::Local(index64), LinearAtom::Local(length)],
                    source,
                ),
            ],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(in_bounds),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(
                    index_offset,
                    LowPrim::IMulImm(LowType::I64, size_of::<Value>() as i64),
                    vec![LinearAtom::Local(index64)],
                    source,
                ),
                Instruction::low_prim_call(
                    data_ptr,
                    LowPrim::IAddImm(LowType::Ptr, data_offset),
                    vec![vector],
                    source,
                ),
                Instruction::low_prim_call(
                    elem_ptr,
                    LowPrim::IAdd(LowType::Ptr),
                    vec![LinearAtom::Local(data_ptr), LinearAtom::Local(index_offset)],
                    source,
                ),
                Instruction::low_prim_call(
                    elem,
                    LowPrim::Load {
                        ty: LowType::Value,
                        offset: 0,
                    },
                    vec![LinearAtom::Local(elem_ptr)],
                    source,
                ),
            ],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(elem)],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args,
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_fixnum_binary_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
        overflow_op: LowPrim,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let rhs_check = self.fresh_block();
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();

        let lhs_is_fixnum = self.fresh_value();
        let rhs_is_fixnum = self.fresh_value();
        let lhs_raw = self.fresh_value();
        let rhs_raw = self.fresh_value();
        let overflow = self.fresh_value();
        let raw_result = self.fresh_value();
        let tagged_result = self.fresh_value();

        let lhs = args[0];
        let rhs = args[1];
        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            lhs_is_fixnum,
            LowPrim::IsFixnum,
            vec![lhs],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(lhs_is_fixnum),
                consequent: BranchTarget::Local {
                    block: rhs_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: rhs_check,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::low_prim_call(
                rhs_is_fixnum,
                LowPrim::IsFixnum,
                vec![rhs],
                source,
            )],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(rhs_is_fixnum),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(lhs_raw, LowPrim::UntagFixnum, vec![lhs], source),
                Instruction::low_prim_call(rhs_raw, LowPrim::UntagFixnum, vec![rhs], source),
                Instruction::low_prim_multi_call(
                    smallvec![raw_result, overflow],
                    overflow_op,
                    vec![LinearAtom::Local(lhs_raw), LinearAtom::Local(rhs_raw)],
                    source,
                ),
                Instruction::low_prim_call(
                    tagged_result,
                    LowPrim::TagFixnum,
                    vec![LinearAtom::Local(raw_result)],
                    source,
                ),
            ],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(overflow),
                consequent: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: merge,
                    args: vec![LinearAtom::Local(tagged_result)],
                },
                hints: [BranchHint::Cold, BranchHint::Hot],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args,
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }
}

fn low_integer_binary_prim(prim: Primitive) -> Option<(LowType, LowPrim)> {
    let name = prim.name();
    let (rest, ty) = integer_prefix(name)?;
    let op = match rest {
        "+" => LowPrim::IAdd(ty),
        "-" => LowPrim::ISub(ty),
        "*" => LowPrim::IMul(ty),
        "/" => LowPrim::IDiv(ty),
        "%" => LowPrim::IRem(ty),
        "<<" => LowPrim::IShl(ty),
        ">>" => LowPrim::IShr(ty),
        "&" => LowPrim::IAnd(ty),
        "|" => LowPrim::IOr(ty),
        "^" => LowPrim::IXor(ty),
        _ => return None,
    };
    Some((ty, op))
}

fn integer_prefix(name: &str) -> Option<(&str, LowType)> {
    [
        ("s64", LowType::I64),
        ("s32", LowType::I32),
        ("s16", LowType::I16),
        ("s8", LowType::I8),
        ("u64", LowType::U64),
        ("u32", LowType::U32),
        ("u16", LowType::U16),
        ("u8", LowType::U8),
    ]
    .into_iter()
    .find_map(|(prefix, ty)| name.strip_prefix(prefix).map(|rest| (rest, ty)))
}

fn low_type_predicate_prim(prim: Primitive) -> Option<u8> {
    match prim {
        Primitive::is_vector => Some(TypeCode8::VECTOR.bits()),
        Primitive::is_bytevector => Some(TypeCode8::BYTEVECTOR.bits()),
        Primitive::is_string => Some(TypeCode8::STRING.bits()),
        Primitive::is_tuple => Some(TypeCode8::TUPLE.bits()),
        Primitive::is_pair => Some(TypeCode8::PAIR.bits()),
        Primitive::is_procedure => Some(TypeCode8::CLOSURE.bits()),
        _ => None,
    }
}

fn lower_low_level_primitives<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    LowLevelLowerer::new(&procedure).lower_procedure(procedure)
}

struct ConstantHoister {
    next_value: u32,
}

impl ConstantHoister {
    fn new<'gc>(procedure: &Procedure<'gc>) -> Self {
        let mut max_value = procedure.binding.0;
        if let Some(return_cont) = procedure.return_cont {
            max_value = max_value.max(return_cont.0);
        }
        for value in procedure
            .params
            .iter()
            .chain(procedure.variadic.iter())
            .chain(procedure.free_vars.iter())
            .copied()
        {
            max_value = max_value.max(value.0);
        }
        for block in &procedure.blocks {
            for value in block.params.iter().chain(block.variadic.iter()).copied() {
                max_value = max_value.max(value.0);
            }
            for instruction in &block.instructions {
                for def in instruction.defs() {
                    max_value = max_value.max(def.0);
                }
                for value in local_values(instruction.uses()) {
                    max_value = max_value.max(value.0);
                }
            }
            for value in local_values(block.terminator.uses()) {
                max_value = max_value.max(value.0);
            }
        }
        Self {
            next_value: max_value + 1,
        }
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn atom<'gc>(
        &mut self,
        atom: LinearAtom<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> LinearAtom<'gc> {
        match atom {
            LinearAtom::Local(_) => atom,
            LinearAtom::Constant(value) => {
                let dst = self.fresh_value();
                instructions.push(Instruction::Const { dst, value });
                LinearAtom::Local(dst)
            }
        }
    }

    fn atoms<'gc>(
        &mut self,
        atoms: Vec<LinearAtom<'gc>>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Vec<LinearAtom<'gc>> {
        atoms
            .into_iter()
            .map(|atom| self.atom(atom, instructions))
            .collect()
    }

    fn instruction<'gc>(
        &mut self,
        instruction: Instruction<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Instruction<'gc> {
        match instruction {
            Instruction::Const { .. }
            | Instruction::MakeClosure { .. }
            | Instruction::CacheRef { .. }
            | Instruction::CacheSet { .. }
            | Instruction::RestToList { .. }
            | Instruction::RestRef { .. }
            | Instruction::RestLength { .. }
            | Instruction::RestPredicate { .. } => instruction,
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => Instruction::ClosureRef {
                dst,
                closure: self.atom(closure, instructions),
                index,
            },
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => Instruction::ClosureSet {
                closure: self.atom(closure, instructions),
                index,
                value: self.atom(value, instructions),
            },
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => Instruction::PrimCall {
                dst,
                prim,
                args: self.atoms(args, instructions),
                source,
            },
            Instruction::LowPrimCall {
                dsts,
                op,
                args,
                source,
            } => Instruction::LowPrimCall {
                dsts,
                op,
                args: self.atoms(args, instructions),
                source,
            },
            Instruction::RuntimePrimCall {
                dst,
                prim,
                args,
                source,
            } => Instruction::RuntimePrimCall {
                dst,
                prim,
                args: self.atoms(args, instructions),
                source,
            },
        }
    }

    fn branch_target<'gc>(
        &mut self,
        target: BranchTarget<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local { block, args } => BranchTarget::Local {
                block,
                args: self.atoms(args, instructions),
            },
            BranchTarget::Reified { continuation, args } => BranchTarget::Reified {
                continuation: self.atom(continuation, instructions),
                args: self.atoms(args, instructions),
            },
        }
    }

    fn terminator<'gc>(
        &mut self,
        terminator: Terminator<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => Terminator::Call {
                callee: self.atom(callee, instructions),
                retk: self.atom(retk, instructions),
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::TailCall {
                callee,
                args,
                source,
            } => Terminator::TailCall {
                callee: self.atom(callee, instructions),
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::Raise { kind, args, source } => Terminator::Raise {
                kind,
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::Jump { target, args } => Terminator::Jump {
                target,
                args: self.atoms(args, instructions),
            },
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints,
            } => Terminator::Branch {
                test: self.atom(test, instructions),
                consequent: self.branch_target(consequent, instructions),
                alternative: self.branch_target(alternative, instructions),
                hints,
            },
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => Terminator::Switch {
                kind,
                scrutinee: self.atom(scrutinee, instructions),
                cases: cases
                    .into_iter()
                    .map(|case| SwitchCase {
                        value: case.value,
                        target: self.branch_target(case.target, instructions),
                    })
                    .collect(),
                default: self.branch_target(default, instructions),
            },
        }
    }
}

fn local_values<'gc>(uses: Vec<LinearAtom<'gc>>) -> impl Iterator<Item = ValueId> {
    uses.into_iter().filter_map(|atom| match atom {
        LinearAtom::Local(value) => Some(value),
        LinearAtom::Constant(_) => None,
    })
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

            Term::Raise { kind, args, source } => Terminator::Raise {
                kind,
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
    fn low_prim_call(
        dst: ValueId,
        op: LowPrim,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    ) -> Self {
        Self::LowPrimCall {
            dsts: smallvec![dst],
            op,
            args,
            source,
        }
    }

    fn low_prim_multi_call(
        dsts: SmallVec<[ValueId; 2]>,
        op: LowPrim,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    ) -> Self {
        Self::LowPrimCall {
            dsts,
            op,
            args,
            source,
        }
    }

    pub fn def(&self) -> Option<ValueId> {
        match self {
            Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RuntimePrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. } => Some(*dst),
            Self::LowPrimCall { dsts, .. } => dsts.first().copied(),
            Self::ClosureSet { .. } => None,
        }
    }

    pub fn defs(&self) -> SmallVec<[ValueId; 2]> {
        match self {
            Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RuntimePrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. } => smallvec![*dst],
            Self::LowPrimCall { dsts, .. } => dsts.clone(),
            Self::ClosureSet { .. } => SmallVec::new(),
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Const { .. } => vec![],
            Self::MakeClosure { .. } => vec![],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::CacheRef { cache_key, .. } => vec![*cache_key],
            Self::CacheSet {
                cache_key, value, ..
            } => vec![*cache_key, *value],
            Self::PrimCall { args, .. }
            | Self::LowPrimCall { args, .. }
            | Self::RuntimePrimCall { args, .. } => args.clone(),
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
            Self::Raise { args, .. } => args.clone(),
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
            Self::Call { .. } | Self::TailCall { .. } | Self::Raise { .. } => vec![],
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
    use crate::{
        compiler::ssa::primitive::Primitive,
        cps::{
            linear::{
                Block, BlockId, BranchTarget, CodeId, Instruction, LinearAtom, LinearProgram,
                Procedure, ProcedureKind, Terminator, ValueId, linearize,
            },
            linear_pretty::render_program,
            reify::reify,
            term::{Atom, BranchHint, Expression, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
            vm::exceptions::RaiseKind,
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

    fn prim_call_func<'gc>(
        ctx: Context<'gc>,
        name: &str,
        args: &[LVarRef<'gc>],
    ) -> Gc<'gc, Func<'gc>> {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let result = lvar(ctx, "result");
        let prim = Symbol::from_str(ctx, name).into();
        let prim_args = args.iter().copied().map(Atom::Local).collect::<Vec<_>>();
        let body = Gc::new(
            *ctx,
            Term::Let(
                result,
                Expression::PrimCall(prim, Array::from_slice(*ctx, &prim_args), Value::new(false)),
                Gc::new(
                    *ctx,
                    Term::Continue(
                        retk,
                        Array::from_slice(*ctx, &[Atom::Local(result)]),
                        Value::new(false),
                    ),
                ),
            ),
        );

        Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "prim-call-entry").into(),
                source: Value::new(false),
                binding: f,
                return_cont: retk,
                args: Array::from_slice(*ctx, args),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        )
    }

    #[test]
    fn linearize_raise_term_to_raise_terminator() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let retk = lvar(ctx, "retk");
            let who = Symbol::from_str(ctx, "car").into();
            let irritant = lvar(ctx, "x");
            let body = Gc::new(
                *ctx,
                Term::Raise {
                    kind: RaiseKind::AssertionViolation,
                    args: Array::from_slice(
                        *ctx,
                        &[
                            Atom::Constant(who),
                            Atom::Constant(Value::new(false)),
                            Atom::Local(irritant),
                        ],
                    ),
                    source: Value::new(false),
                },
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Symbol::from_str(ctx, "raise-entry").into(),
                    source: Value::new(false),
                    binding: f,
                    return_cont: retk,
                    args: Array::from_slice(*ctx, &[irritant]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure");

            let Terminator::Raise { kind, args, .. } = &procedure.blocks[0].terminator else {
                panic!("raise term should linearize to raise terminator");
            };

            assert_eq!(*kind, RaiseKind::AssertionViolation);
            assert_eq!(args.len(), 3);
            assert_eq!(procedure.blocks[0].terminator.successors(), Vec::new());
            assert_eq!(procedure.blocks[0].terminator.uses(), args.clone());
        });
    }

    #[test]
    fn low_level_lowering_expands_plus_into_fixnum_fastpath() {
        with_ctx(|ctx| {
            let lhs = lvar(ctx, "lhs");
            let rhs = lvar(ctx, "rhs");
            let func = prim_call_func(ctx, "+", &[lhs, rhs]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("(low-call")
                    && rendered.contains("is-fixnum")
                    && rendered.contains("iadd-overflow.i32")
                    && rendered.contains("(runtime-call"),
                "expected + to lower to explicit fixnum fastpath and runtime slowpath:\n{rendered}"
            );
            assert!(
                !rendered.contains("(prim-call"),
                "low-level lowering should remove high-level primitive calls from this program:\n{rendered}"
            );
        });
    }

    #[test]
    fn low_level_lowering_reuses_checked_add_result_without_second_iadd() {
        with_ctx(|ctx| {
            let lhs = lvar(ctx, "lhs");
            let rhs = lvar(ctx, "rhs");
            let func = prim_call_func(ctx, "+", &[lhs, rhs]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);

            let mut checked_adds = 0;
            let mut plain_i32_adds = 0;
            for procedure in &linear.procedures {
                for block in &procedure.blocks {
                    for instruction in &block.instructions {
                        match instruction {
                            Instruction::LowPrimCall {
                                dsts,
                                op: super::LowPrim::IAddOverflow(super::LowType::I32),
                                ..
                            } => {
                                checked_adds += 1;
                                assert_eq!(
                                    dsts.len(),
                                    2,
                                    "checked i32 add should define raw result and overflow flag"
                                );
                            }
                            Instruction::LowPrimCall {
                                op: super::LowPrim::IAdd(super::LowType::I32),
                                ..
                            } => plain_i32_adds += 1,
                            _ => {}
                        }
                    }
                }
            }

            assert_eq!(checked_adds, 1, "expected one checked i32 add");
            assert_eq!(
                plain_i32_adds,
                0,
                "checked fixnum addition should not also emit a second plain iadd:\n{}",
                render_program(&linear)
            );
        });
    }

    #[test]
    fn low_level_lowering_keeps_split_merge_before_existing_successors() {
        with_ctx(|ctx| {
            let func = prim_call_func(ctx, "unused", &[]);
            let retk = ValueId(1);
            let lhs = ValueId(2);
            let variable = ValueId(3);
            let sum = ValueId(7);
            let predicate = ValueId(8);
            let stored = ValueId(9);
            let procedure = Procedure {
                code: CodeId::Function(func),
                kind: ProcedureKind::Function,
                binding: ValueId(0),
                name: Value::new(false),
                source: Value::new(false),
                meta: Value::new(false),
                return_cont: Some(retk),
                params: vec![lhs, variable],
                variadic: None,
                free_vars: vec![],
                sources: Default::default(),
                entry: BlockId(0),
                blocks: vec![
                    Block {
                        id: BlockId(0),
                        params: vec![lhs, variable],
                        variadic: None,
                        instructions: vec![
                            Instruction::PrimCall {
                                dst: sum,
                                prim: Primitive::plus,
                                args: vec![
                                    LinearAtom::Local(lhs),
                                    LinearAtom::Constant(Value::new(1)),
                                ],
                                source: Value::new(false),
                            },
                            Instruction::PrimCall {
                                dst: predicate,
                                prim: Primitive::is_variable,
                                args: vec![LinearAtom::Local(variable)],
                                source: Value::new(false),
                            },
                        ],
                        terminator: Terminator::Branch {
                            test: LinearAtom::Local(predicate),
                            consequent: BranchTarget::Local {
                                block: BlockId(1),
                                args: vec![],
                            },
                            alternative: BranchTarget::Local {
                                block: BlockId(2),
                                args: vec![],
                            },
                            hints: [BranchHint::Normal, BranchHint::Normal],
                        },
                        source: Value::new(false),
                    },
                    Block {
                        id: BlockId(1),
                        params: vec![],
                        variadic: None,
                        instructions: vec![Instruction::PrimCall {
                            dst: stored,
                            prim: Primitive::variable_set,
                            args: vec![LinearAtom::Local(variable), LinearAtom::Local(sum)],
                            source: Value::new(false),
                        }],
                        terminator: Terminator::TailCall {
                            callee: LinearAtom::Local(retk),
                            args: vec![LinearAtom::Local(stored)],
                            source: Value::new(false),
                        },
                        source: Value::new(false),
                    },
                    Block {
                        id: BlockId(2),
                        params: vec![],
                        variadic: None,
                        instructions: vec![],
                        terminator: Terminator::TailCall {
                            callee: LinearAtom::Local(retk),
                            args: vec![LinearAtom::Local(lhs)],
                            source: Value::new(false),
                        },
                        source: Value::new(false),
                    },
                ],
            };

            let lowered = super::lower_low_level_primitives(procedure);
            let merge_position = lowered
                .blocks
                .iter()
                .position(|block| {
                    block.params == vec![sum]
                        && matches!(
                            block.instructions.first(),
                            Some(Instruction::PrimCall {
                                prim: Primitive::is_variable,
                                ..
                            })
                        )
                })
                .expect("plus merge block should contain the original suffix");
            let successor_position = lowered
                .blocks
                .iter()
                .position(|block| block.id == BlockId(1))
                .expect("original successor should still exist");

            assert!(
                merge_position < successor_position,
                "the merge block defining the split result must be lowered before existing successors that use it:\n{}",
                render_program(&LinearProgram {
                    entry: func,
                    procedures: vec![lowered],
                })
            );
        });
    }

    #[test]
    fn low_level_lowering_reuses_integer_op_family_for_s8_add() {
        with_ctx(|ctx| {
            let lhs = lvar(ctx, "lhs");
            let rhs = lvar(ctx, "rhs");
            let func = prim_call_func(ctx, "s8+", &[lhs, rhs]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("ireduce.i8") && rendered.contains("iadd.i8"),
                "expected s8+ to lower through shared ireduce/iadd low ops:\n{rendered}"
            );
            assert!(
                !rendered.contains("s8+"),
                "typed integer primitives should not survive as primitive-specific low ops:\n{rendered}"
            );
        });
    }

    #[test]
    fn low_level_lowering_expands_vector_ref_into_type_and_bounds_fastpath() {
        with_ctx(|ctx| {
            let vector = lvar(ctx, "vector");
            let index = lvar(ctx, "index");
            let func = prim_call_func(ctx, "vector-ref", &[vector, index]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("has-type8.vector")
                    && rendered.contains("is-fixnum")
                    && rendered.contains("icmp.ult.i64")
                    && rendered.contains("load.value")
                    && rendered.contains("(runtime-call"),
                "expected vector-ref to lower to type/index/bounds fastpath and runtime slowpath:\n{rendered}"
            );
            assert!(
                !rendered.contains("(prim-call"),
                "low-level vector-ref lowering should remove high-level primitive calls:\n{rendered}"
            );
        });
    }

    #[test]
    fn low_level_lowering_expands_string_length_into_type_checked_load() {
        with_ctx(|ctx| {
            let string = lvar(ctx, "string");
            let func = prim_call_func(ctx, "string-length", &[string]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("has-type8.string")
                    && rendered.contains("load.i64")
                    && rendered.contains("tag-fixnum")
                    && rendered.contains("(runtime-call"),
                "expected string-length to lower to type-checked length load and runtime slowpath:\n{rendered}"
            );
            assert!(
                !rendered.contains("(prim-call"),
                "low-level string-length lowering should remove high-level primitive calls:\n{rendered}"
            );
        });
    }

    #[test]
    fn low_level_lowering_expands_tuple_ref_into_type_and_bounds_fastpath() {
        with_ctx(|ctx| {
            let tuple = lvar(ctx, "tuple");
            let index = lvar(ctx, "index");
            let func = prim_call_func(ctx, "tuple-ref", &[tuple, index]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("has-type8.tuple")
                    && rendered.contains("is-fixnum")
                    && rendered.contains("icmp.ult.i64")
                    && rendered.contains("load.value")
                    && rendered.contains("(runtime-call"),
                "expected tuple-ref to lower to type/index/bounds fastpath and runtime slowpath:\n{rendered}"
            );
            assert!(
                !rendered.contains("(prim-call"),
                "low-level tuple-ref lowering should remove high-level primitive calls:\n{rendered}"
            );
        });
    }

    #[test]
    fn low_level_lowering_expands_bytevector_predicate_to_type_test() {
        with_ctx(|ctx| {
            let value = lvar(ctx, "value");
            let func = prim_call_func(ctx, "bytevector?", &[value]);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let rendered = render_program(&linear);

            assert!(
                rendered.contains("has-type8.bytevector"),
                "expected bytevector? to lower to a low-level type test:\n{rendered}"
            );
            assert!(
                !rendered.contains("(prim-call"),
                "low-level bytevector? lowering should remove high-level primitive calls:\n{rendered}"
            );
        });
    }
}
