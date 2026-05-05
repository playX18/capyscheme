use crate::{
    cps::{
        ReifyInfo,
        packed::Primitive,
        term::{Atom, BranchHint, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    rsgc::{
        Trace,
        alloc::{Array, ArrayRef},
    },
    runtime::{
        Context,
        value::{Symbol, Value},
    },
};
use std::collections::{HashMap, HashSet};

pub type Procedures<'gc> = ArrayRef<'gc, Procedure<'gc>>;
pub type Blocks<'gc> = ArrayRef<'gc, Block<'gc>>;
pub type ValueIds<'gc> = ArrayRef<'gc, ValueId>;
pub type LinearAtoms<'gc> = ArrayRef<'gc, LinearAtom<'gc>>;
pub type Instructions<'gc> = ArrayRef<'gc, Instruction<'gc>>;
pub type SwitchCases<'gc> = ArrayRef<'gc, SwitchCase<'gc>>;
pub type ValueSources<'gc> = HashMap<ValueId, ValueSource<'gc>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
pub enum LinearAtom<'gc> {
    Constant(Value<'gc>),
    Local(ValueId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum ProcedureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum ClosureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
pub struct CodeId(pub u32);

#[derive(Debug, Clone, Trace)]
#[collect(no_drop)]
pub struct LinearProgram<'gc> {
    pub entry: CodeId,
    pub procedures: Procedures<'gc>,
}

#[derive(Debug, Clone, Trace)]
#[collect(no_drop)]
pub struct Procedure<'gc> {
    pub code: CodeId,
    pub kind: ProcedureKind,
    pub binding: ValueId,
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub meta: Value<'gc>,
    pub return_cont: Option<ValueId>,
    pub params: ValueIds<'gc>,
    pub variadic: Option<ValueId>,
    pub free_vars: ValueIds<'gc>,
    pub sources: ValueSources<'gc>,
    pub entry: BlockId,
    pub blocks: Blocks<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct ValueSource<'gc> {
    pub name: Value<'gc>,
    pub id: Value<'gc>,
    pub set_count: u32,
    pub ref_count: u32,
}

impl<'gc> ValueSource<'gc> {
    pub fn new(name: Value<'gc>, id: Value<'gc>, set_count: u32, ref_count: u32) -> Self {
        Self {
            name,
            id,
            set_count,
            ref_count,
        }
    }

    pub fn from_lvar(var: LVarRef<'gc>) -> Self {
        Self::new(var.name, var.id, var.set_count.get(), var.ref_count.get())
    }

    pub fn is_assigned(&self) -> bool {
        self.set_count > 0
    }

    pub fn is_referenced(&self) -> bool {
        self.ref_count > 0
    }
}

#[derive(Debug, Clone, Trace)]
#[collect(no_drop)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub params: ValueIds<'gc>,
    pub variadic: Option<ValueId>,
    pub instructions: Instructions<'gc>,
    pub terminator: Terminator<'gc>,
    pub source: Value<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum RestPredicate {
    Null,
    Pair,
    List,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum Instruction<'gc> {
    Const {
        dst: ValueId,
        value: Value<'gc>,
    },
    MakeClosure {
        dst: ValueId,
        code: CodeId,
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
        args: LinearAtoms<'gc>,
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

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum BranchTarget<'gc> {
    Local {
        block: BlockId,
        args: LinearAtoms<'gc>,
    },
    Reified {
        continuation: LinearAtom<'gc>,
        args: LinearAtoms<'gc>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct SwitchCase<'gc> {
    pub value: SwitchCaseValue<'gc>,
    pub target: BranchTarget<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Trace)]
#[collect(no_drop)]
pub enum SwitchCaseValue<'gc> {
    Integer(i32),
    Symbol { hash: u64, value: Value<'gc> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum SwitchKind {
    Eq,
    Fixnum,
    Numeric,
    Char,
    CharEq,
    SymbolEq { mask: u64 },
}

const SYMBOL_HASH_DISPATCH_MIN_LENGTH: usize = 4;

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum Terminator<'gc> {
    Call {
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: LinearAtoms<'gc>,
        source: Value<'gc>,
    },
    TailCall {
        callee: LinearAtom<'gc>,
        args: LinearAtoms<'gc>,
        source: Value<'gc>,
    },
    Jump {
        target: BlockId,
        args: LinearAtoms<'gc>,
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
        cases: SwitchCases<'gc>,
        default: BranchTarget<'gc>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CodeRef<'gc> {
    Function(FuncRef<'gc>),
    Continuation(ContRef<'gc>),
}

pub(crate) struct CodeIdTable<'gc> {
    funcs: HashMap<FuncRef<'gc>, CodeId>,
    conts: HashMap<ContRef<'gc>, CodeId>,
    refs: HashMap<CodeId, CodeRef<'gc>>,
}

impl<'gc> CodeIdTable<'gc> {
    pub(crate) fn new(reify: &ReifyInfo<'gc>) -> Self {
        let mut table = Self {
            funcs: HashMap::new(),
            conts: HashMap::new(),
            refs: HashMap::new(),
        };

        for func in reify.functions.iter().copied() {
            table.insert(CodeRef::Function(func));
        }
        for cont in reify
            .continuations
            .iter()
            .copied()
            .filter(|cont| cont.reified.get())
        {
            table.insert(CodeRef::Continuation(cont));
        }

        table
    }

    fn insert(&mut self, code_ref: CodeRef<'gc>) -> CodeId {
        let code = CodeId(self.refs.len() as u32);
        match code_ref {
            CodeRef::Function(func) => {
                self.funcs.insert(func, code);
            }
            CodeRef::Continuation(cont) => {
                self.conts.insert(cont, code);
            }
        }
        self.refs.insert(code, code_ref);
        code
    }

    pub(crate) fn function(&self, func: FuncRef<'gc>) -> CodeId {
        self.funcs[&func]
    }

    pub(crate) fn continuation(&self, cont: ContRef<'gc>) -> CodeId {
        self.conts[&cont]
    }

    pub(crate) fn code_ref(&self, code: CodeId) -> CodeRef<'gc> {
        self.refs[&code]
    }
}

fn array_from_slice<'gc, T>(ctx: Context<'gc>, values: &[T]) -> ArrayRef<'gc, T>
where
    T: Trace + Clone,
{
    Array::from_slice(*ctx, values)
}

fn array_from_vec<'gc, T>(ctx: Context<'gc>, values: Vec<T>) -> ArrayRef<'gc, T>
where
    T: Trace + Clone,
{
    array_from_slice(ctx, &values)
}

pub fn linearize<'gc>(ctx: Context<'gc>, reify: &ReifyInfo<'gc>) -> LinearProgram<'gc> {
    let code_ids = CodeIdTable::new(reify);
    let mut procedures = Vec::new();

    for func in reify.functions.iter() {
        procedures.push(hoist_constants(
            ctx,
            lower_cache_operations(
                ctx,
                lower_rest_arguments(
                    ctx,
                    infer_switches(ctx, linearize_function(ctx, &code_ids, *func)),
                ),
            ),
        ));
    }

    for cont in reify.continuations.iter().filter(|cont| cont.reified.get()) {
        procedures.push(hoist_constants(
            ctx,
            lower_cache_operations(
                ctx,
                lower_rest_arguments(
                    ctx,
                    infer_switches(ctx, linearize_continuation(ctx, &code_ids, *cont)),
                ),
            ),
        ));
    }

    LinearProgram {
        entry: code_ids.function(reify.entrypoint),
        procedures: array_from_vec(ctx, procedures),
    }
}

fn linearize_function<'gc>(
    ctx: Context<'gc>,
    code_ids: &CodeIdTable<'gc>,
    func: FuncRef<'gc>,
) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        func.free_vars
            .get()
            .expect("function free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new(ctx, code_ids);
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
        code: code_ids.function(func),
        kind: ProcedureKind::Function,
        binding,
        name: func.name,
        source: func.source,
        meta: func.meta,
        return_cont: Some(return_cont),
        params: array_from_vec(ctx, params),
        variadic,
        free_vars: array_from_vec(ctx, free_vars),
        entry,
        sources,
        blocks,
    }
}

fn linearize_continuation<'gc>(
    ctx: Context<'gc>,
    code_ids: &CodeIdTable<'gc>,
    cont: ContRef<'gc>,
) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        cont.free_vars
            .get()
            .expect("continuation free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new(ctx, code_ids);
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
        code: code_ids.continuation(cont),
        kind: ProcedureKind::Continuation,
        binding,
        name: cont.name,
        source: cont.source,
        meta: cont.meta,
        return_cont: None,
        params: array_from_vec(ctx, params),
        variadic,
        free_vars: array_from_vec(ctx, free_vars),
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
    builder: &mut ProcedureBuilder<'gc, '_>,
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

fn lower_rest_arguments<'gc>(ctx: Context<'gc>, procedure: Procedure<'gc>) -> Procedure<'gc> {
    let Some(rest) = procedure.variadic else {
        return procedure;
    };

    let aliases = collect_rest_aliases(&procedure, rest);
    let loop_blocks = loop_blocks(&procedure);
    if has_incompatible_rest_use(&procedure, rest, &aliases, &loop_blocks) {
        insert_rest_to_list(ctx, procedure, rest)
    } else {
        rewrite_rest_uses(ctx, procedure, rest, &aliases)
    }
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
                if *prim != Primitive::Cdr || args.len() != 1 {
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
        Primitive::Cdr => Some(RestRewrite::Cdr),
        Primitive::Car => Some(RestRewrite::Ref(alias)),
        Primitive::Length => Some(RestRewrite::Length(alias)),
        Primitive::IsNull => Some(RestRewrite::Predicate(alias, RestPredicate::Null)),
        Primitive::IsPair => Some(RestRewrite::Predicate(alias, RestPredicate::Pair)),
        Primitive::IsList => Some(RestRewrite::Predicate(alias, RestPredicate::List)),
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

fn insert_rest_to_list<'gc>(
    ctx: Context<'gc>,
    mut procedure: Procedure<'gc>,
    rest: ValueId,
) -> Procedure<'gc> {
    let mut found_entry = false;
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| {
            let mut block = block.clone();
            if block.id == procedure.entry {
                found_entry = true;
                let mut instructions = Vec::with_capacity(block.instructions.len() + 1);
                instructions.push(Instruction::RestToList {
                    dst: rest,
                    rest,
                    source: block.source,
                });
                instructions.extend(block.instructions.iter().cloned());
                block.instructions = array_from_vec(ctx, instructions);
            }
            block
        })
        .collect::<Vec<_>>();
    assert!(found_entry, "procedure should contain entry block");
    procedure.blocks = array_from_vec(ctx, blocks);
    procedure
}

fn rewrite_rest_uses<'gc>(
    ctx: Context<'gc>,
    mut procedure: Procedure<'gc>,
    rest: ValueId,
    aliases: &HashMap<ValueId, RestAlias>,
) -> Procedure<'gc> {
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| {
            let mut block = block.clone();
            let mut lowered = Vec::with_capacity(block.instructions.len());
            for instruction in block.instructions.iter().cloned() {
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
            block.instructions = array_from_vec(ctx, lowered);
            block
        })
        .collect::<Vec<_>>();
    procedure.blocks = array_from_vec(ctx, blocks);
    procedure
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

fn infer_switches<'gc>(ctx: Context<'gc>, mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    loop {
        let predecessors = local_predecessor_counts(&procedure);
        let Some(candidate) = procedure
            .blocks
            .iter()
            .find_map(|block| infer_switch_candidate(&procedure, block.id, &predecessors))
        else {
            break;
        };
        apply_switch_candidate(ctx, &mut procedure, candidate);
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
            procedure.blocks.as_slice(),
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
        Primitive::IsEq | Primitive::IsEqv | Primitive::IsEqual => Some(SwitchKind::Eq),
        Primitive::FxEq => Some(SwitchKind::Fixnum),
        Primitive::NumericEqual => Some(SwitchKind::Numeric),
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
        Primitive::IsEq | Primitive::IsEqv | Primitive::IsEqual
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

fn apply_switch_candidate<'gc>(
    ctx: Context<'gc>,
    procedure: &mut Procedure<'gc>,
    candidate: SwitchCandidate<'gc>,
) {
    let removed = candidate
        .chain_blocks
        .iter()
        .copied()
        .skip(1)
        .collect::<HashSet<_>>();
    let start = candidate.chain_blocks[0];

    let mut found_start = false;
    let blocks = procedure
        .blocks
        .iter()
        .filter_map(|block| {
            if removed.contains(&block.id) {
                return None;
            }
            let mut block = block.clone();
            if block.id == start {
                found_start = true;
                let keep = block
                    .instructions
                    .len()
                    .checked_sub(candidate.instruction_count)
                    .expect("switch candidate consumes no more than the block instructions");
                block.instructions = array_from_slice(ctx, &block.instructions[..keep]);
                block.terminator = Terminator::Switch {
                    kind: candidate.kind,
                    scrutinee: candidate.scrutinee,
                    cases: array_from_vec(ctx, candidate.cases.clone()),
                    default: candidate.default.clone(),
                };
            }
            Some(block)
        })
        .collect::<Vec<_>>();
    assert!(found_start, "switch candidate start block should exist");
    procedure.blocks = array_from_vec(ctx, blocks);
}

fn hoist_constants<'gc>(ctx: Context<'gc>, mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let mut hoister = ConstantHoister::new(ctx, &procedure);
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| {
            let mut block = block.clone();
            let mut instructions = Vec::with_capacity(block.instructions.len());
            for instruction in block.instructions.iter().cloned() {
                let instruction = hoister.instruction(instruction, &mut instructions);
                instructions.push(instruction);
            }
            block.terminator = hoister.terminator(block.terminator.clone(), &mut instructions);
            block.instructions = array_from_vec(ctx, instructions);
            block
        })
        .collect::<Vec<_>>();
    procedure.blocks = array_from_vec(ctx, blocks);
    procedure
}

fn lower_cache_operations<'gc>(ctx: Context<'gc>, mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let blocks = procedure
        .blocks
        .iter()
        .map(|block| {
            let mut block = block.clone();
            let mut lowered = Vec::with_capacity(block.instructions.len());
            for instruction in block.instructions.iter().cloned() {
                match instruction {
                    Instruction::PrimCall {
                        dst,
                        prim: Primitive::CacheRef,
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
                        prim: Primitive::CacheSet,
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
            block.instructions = array_from_vec(ctx, lowered);
            block
        })
        .collect::<Vec<_>>();
    procedure.blocks = array_from_vec(ctx, blocks);
    procedure
}

struct ConstantHoister<'gc> {
    ctx: Context<'gc>,
    next_value: u32,
}

impl<'gc> ConstantHoister<'gc> {
    fn new(ctx: Context<'gc>, procedure: &Procedure<'gc>) -> Self {
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
                if let Some(def) = instruction.def() {
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
            ctx,
            next_value: max_value + 1,
        }
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn atom(
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

    fn atoms<I>(&mut self, atoms: I, instructions: &mut Vec<Instruction<'gc>>) -> LinearAtoms<'gc>
    where
        I: IntoIterator<Item = LinearAtom<'gc>>,
    {
        let atoms = atoms
            .into_iter()
            .map(|atom| self.atom(atom, instructions))
            .collect::<Vec<_>>();
        array_from_vec(self.ctx, atoms)
    }

    fn instruction(
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
                args: self.atoms(args.iter().copied(), instructions),
                source,
            },
        }
    }

    fn branch_target(
        &mut self,
        target: BranchTarget<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local { block, args } => BranchTarget::Local {
                block,
                args: self.atoms(args.iter().copied(), instructions),
            },
            BranchTarget::Reified { continuation, args } => BranchTarget::Reified {
                continuation: self.atom(continuation, instructions),
                args: self.atoms(args.iter().copied(), instructions),
            },
        }
    }

    fn terminator(
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
                args: self.atoms(args.iter().copied(), instructions),
                source,
            },
            Terminator::TailCall {
                callee,
                args,
                source,
            } => Terminator::TailCall {
                callee: self.atom(callee, instructions),
                args: self.atoms(args.iter().copied(), instructions),
                source,
            },
            Terminator::Jump { target, args } => Terminator::Jump {
                target,
                args: self.atoms(args.iter().copied(), instructions),
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
                cases: array_from_vec(
                    self.ctx,
                    cases
                        .iter()
                        .cloned()
                        .map(|case| SwitchCase {
                            value: case.value,
                            target: self.branch_target(case.target, instructions),
                        })
                        .collect(),
                ),
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

struct ProcedureBuilder<'gc, 'a> {
    ctx: Context<'gc>,
    code_ids: &'a CodeIdTable<'gc>,
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<LVarRef<'gc>, BlockId>,
    values: HashMap<LVarRef<'gc>, ValueId>,
    sources: ValueSources<'gc>,
    next_value: u32,
    next_block: usize,
}

impl<'gc, 'a> ProcedureBuilder<'gc, 'a> {
    fn new(ctx: Context<'gc>, code_ids: &'a CodeIdTable<'gc>) -> Self {
        Self {
            ctx,
            code_ids,
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            values: HashMap::new(),
            sources: HashMap::new(),
            next_value: 0,
            next_block: 1,
        }
    }

    fn finish(mut self) -> (Blocks<'gc>, ValueSources<'gc>) {
        self.blocks.sort_by_key(|block| block.id.0);
        (array_from_vec(self.ctx, self.blocks), self.sources)
    }

    fn value(&mut self, var: LVarRef<'gc>) -> ValueId {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = ValueId(self.next_value);
        self.next_value += 1;
        self.values.insert(var, id);
        self.sources.insert(id, ValueSource::from_lvar(var));
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

    fn atoms(&mut self, args: crate::cps::term::Atoms<'gc>) -> LinearAtoms<'gc> {
        let atoms = args
            .iter()
            .copied()
            .map(|atom| self.atom(atom))
            .collect::<Vec<_>>();
        array_from_vec(self.ctx, atoms)
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
            params: array_from_vec(self.ctx, params),
            variadic,
            instructions: array_from_vec(self.ctx, instructions),
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
                        code: self.code_ids.function(*func),
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
                        code: self.code_ids.continuation(*cont),
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
        let args = args
            .map(|args| self.atoms(args))
            .unwrap_or_else(|| array_from_slice(self.ctx, &[]));
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
    builder: &mut ProcedureBuilder<'gc, '_>,
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
            Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
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
            Self::Const { .. } => vec![],
            Self::MakeClosure { .. } => vec![],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::CacheRef { cache_key, .. } => vec![*cache_key],
            Self::CacheSet {
                cache_key, value, ..
            } => vec![*cache_key, *value],
            Self::PrimCall { args, .. } => args.iter().copied().collect(),
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
            Self::Local { args, .. } => args.iter().copied().collect(),
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
            Self::Jump { args, .. } => args.iter().copied().collect(),
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
