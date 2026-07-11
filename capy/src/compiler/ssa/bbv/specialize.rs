//! SBBV specialization driver (ECOOP'24 Sections 3.4-3.6).
//!
//! [`specialize_procedure`] clones each original block once per distinct entry
//! typing context, threading the context through the block's instructions and
//! terminators. Type predicates fold to constants, checked primitives collapse
//! to unchecked variants when the context proves the guard, and branches whose
//! outcome is statically known become unconditional jumps.
//!
//! # Maintaining SSA
//!
//! Every cloned block gets fresh [`ValueId`]s for the values it defines, with a
//! per-walk `old -> new` substitution threaded along control-flow edges so that
//! successor versions reference the correct incoming definitions. Procedure
//! level values (parameters, free variables, `binding`, `return_cont`) are
//! shared across all versions and are never renamed. Values that escape their
//! defining block without being carried as block parameters are resolved
//! through the incoming edge's substitution; at a merge point that fuses
//! several incoming edges the substitution of the first materialized edge is
//! used, which is exact when such escaping values are block parameters (as they
//! are for the CPS-contified join points this IR produces).

use super::types::{Bound, CmpOp, Type, TypeContext, TypeKind, exclude_kind};
use super::{infer, merge};
use crate::compiler::cranelift::primitive::Primitive;
use crate::compiler::ssa::{
    Block, BlockId, BranchTarget, Instruction, Operand, Procedure, SwitchCase, SwitchCaseValue,
    SwitchKind, Terminator, ValueId,
};
use std::collections::{HashMap, HashSet, VecDeque};

/// How a boolean-valued instruction can narrow the context when branched on.
#[derive(Clone, Copy)]
enum Predicate {
    /// Binary comparison over (possibly) fixnum operands.
    Cmp(CmpOp, ValueId, ValueId),
    /// Unary type test (`fixnum?`, `pair?`, ...).
    TypeTest(Primitive, ValueId),
}

/// Records the provenance of a specialized block for debugging and tooling.
pub struct BlockAnnotation {
    /// Original block this version was cloned from.
    pub orig: BlockId,
    /// Rendered entry context under which the version was specialized.
    pub ctx: String,
}

struct VersionInfo {
    #[allow(dead_code)]
    orig: BlockId,
    ctx: TypeContext,
    key: String,
}

struct Task {
    orig: BlockId,
    new: BlockId,
    ctx: TypeContext,
    subst: HashMap<ValueId, ValueId>,
}

struct Specializer<'gc> {
    orig_blocks: HashMap<BlockId, Block<'gc>>,
    block_params: HashMap<BlockId, Vec<ValueId>>,
    proc_values: HashSet<ValueId>,
    entry_orig: BlockId,

    versions_of: HashMap<BlockId, Vec<BlockId>>,
    all_versions: HashMap<BlockId, VersionInfo>,
    version_by_key: HashMap<(BlockId, String), BlockId>,
    replacement: HashMap<BlockId, BlockId>,

    out_blocks: HashMap<BlockId, Block<'gc>>,
    annotations: HashMap<BlockId, BlockAnnotation>,
    queue: VecDeque<Task>,

    next_block: usize,
    next_value: u32,
    version_limit: usize,
}

/// Specializes a procedure with Static Basic Block Versioning.
pub(super) fn specialize_procedure<'gc>(
    procedure: Procedure<'gc>,
    version_limit: usize,
) -> (Procedure<'gc>, HashMap<BlockId, BlockAnnotation>) {
    let mut specializer = Specializer::new(&procedure, version_limit.max(1));
    if let Some((entry, blocks, annotations)) = specializer.run() {
        (
            Procedure {
                entry,
                blocks,
                ..procedure
            },
            annotations,
        )
    } else {
        (procedure, HashMap::new())
    }
}

impl<'gc> Specializer<'gc> {
    fn new(procedure: &Procedure<'gc>, version_limit: usize) -> Self {
        let mut orig_blocks = HashMap::new();
        let mut block_params = HashMap::new();
        for block in &procedure.blocks {
            block_params.insert(block.id, block.params.clone());
            orig_blocks.insert(block.id, block.clone());
        }

        let mut proc_values = HashSet::new();
        proc_values.insert(procedure.binding);
        proc_values.extend(procedure.return_cont);
        proc_values.extend(procedure.params.iter().copied());
        proc_values.extend(procedure.variadic);
        proc_values.extend(procedure.free_vars.iter().copied());

        Self {
            orig_blocks,
            block_params,
            proc_values,
            entry_orig: procedure.entry,
            versions_of: HashMap::new(),
            all_versions: HashMap::new(),
            version_by_key: HashMap::new(),
            replacement: HashMap::new(),
            out_blocks: HashMap::new(),
            annotations: HashMap::new(),
            queue: VecDeque::new(),
            next_block: max_block_id(procedure) + 1,
            next_value: max_value_id(procedure) + 1,
            version_limit,
        }
    }

    fn run(&mut self) -> Option<(BlockId, Vec<Block<'gc>>, HashMap<BlockId, BlockAnnotation>)> {
        if !self.orig_blocks.contains_key(&self.entry_orig) {
            return None;
        }

        let entry_new = self.reach(self.entry_orig, TypeContext::new(), HashMap::new());
        while let Some(task) = self.queue.pop_front() {
            if self.replacement.contains_key(&task.new) || self.out_blocks.contains_key(&task.new) {
                continue;
            }
            self.walk_block(task);
        }

        Some(self.finalize(entry_new))
    }

    // --- fresh identifiers -------------------------------------------------

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

    // --- version management ------------------------------------------------

    fn resolve(&self, mut id: BlockId) -> BlockId {
        while let Some(next) = self.replacement.get(&id) {
            id = *next;
        }
        id
    }

    fn ctx_key(&self, orig: BlockId, ctx: &TypeContext) -> String {
        let params = self.block_params.get(&orig).cloned().unwrap_or_default();
        params
            .iter()
            .map(|param| ctx.get(*param).to_string())
            .collect::<Vec<_>>()
            .join(";")
    }

    fn active_versions(&self, orig: BlockId) -> Vec<(BlockId, TypeContext)> {
        self.versions_of
            .get(&orig)
            .map(|ids| {
                ids.iter()
                    .filter(|id| !self.replacement.contains_key(id))
                    .map(|id| (*id, self.all_versions[id].ctx.clone()))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn create_version(
        &mut self,
        orig: BlockId,
        ctx: TypeContext,
        key: String,
        subst: HashMap<ValueId, ValueId>,
    ) -> BlockId {
        let new = self.fresh_block();
        self.all_versions.insert(
            new,
            VersionInfo {
                orig,
                ctx: ctx.clone(),
                key: key.clone(),
            },
        );
        self.versions_of.entry(orig).or_default().push(new);
        self.version_by_key.insert((orig, key), new);
        self.queue.push_back(Task {
            orig,
            new,
            ctx,
            subst,
        });
        new
    }

    /// Returns the specialized block for `orig` under `ctx`, creating or
    /// merging versions as needed to stay within the version limit.
    fn reach(
        &mut self,
        orig: BlockId,
        ctx: TypeContext,
        subst: HashMap<ValueId, ValueId>,
    ) -> BlockId {
        let key = self.ctx_key(orig, &ctx);
        if let Some(id) = self.version_by_key.get(&(orig, key.clone())) {
            return self.resolve(*id);
        }

        let active = self.active_versions(orig);
        if active.len() < self.version_limit {
            return self.create_version(orig, ctx, key, subst);
        }

        // At the limit, merge the incoming context into its closest active
        // version. This makes interval widening monotone even when a loop
        // produces an unbounded sequence of distinct incoming contexts.
        let active_contexts: Vec<_> = active.iter().map(|(_, ctx)| ctx.clone()).collect();
        let selected = merge::select_version_to_merge(&active_contexts, &ctx);
        let (old, old_ctx) = &active[selected];
        let merged_ctx = merge::merge_contexts(old_ctx, &ctx, true);
        let merged_key = self.ctx_key(orig, &merged_ctx);
        let merged_id = match self.version_by_key.get(&(orig, merged_key.clone())) {
            Some(id) => self.resolve(*id),
            None => self.create_version(orig, merged_ctx, merged_key, subst),
        };

        if *old != merged_id {
            self.replacement.insert(*old, merged_id);
        }
        merged_id
    }

    // --- block walking -----------------------------------------------------

    fn walk_block(&mut self, task: Task) {
        let block = self.orig_blocks[&task.orig].clone();
        let mut ctx = task.ctx.clone();
        let mut map = task.subst.clone();
        let mut pred_of: HashMap<ValueId, Predicate> = HashMap::new();

        let mut new_params = Vec::with_capacity(block.params.len());
        for param in &block.params {
            let new_param = if self.proc_values.contains(param) {
                *param
            } else {
                self.fresh_value()
            };
            map.insert(*param, new_param);
            new_params.push(new_param);
        }
        // The rest formal is already the last entry in `params` (see
        // `params_with_variadic`); reuse that rename so CLIF binding and
        // body uses stay the same ValueId.
        let new_variadic = block.variadic.map(|variadic| {
            if let Some(&renamed) = map.get(&variadic) {
                renamed
            } else if self.proc_values.contains(&variadic) {
                variadic
            } else {
                let renamed = self.fresh_value();
                map.insert(variadic, renamed);
                renamed
            }
        });

        let mut new_instructions = Vec::with_capacity(block.instructions.len());
        for instruction in &block.instructions {
            self.walk_instruction(
                instruction,
                &mut ctx,
                &mut map,
                &mut pred_of,
                &mut new_instructions,
            );
        }

        let terminator = self.walk_terminator(&block.terminator, &ctx, &map, &pred_of);

        self.annotations.insert(
            task.new,
            BlockAnnotation {
                orig: task.orig,
                ctx: format!("{}", task.ctx),
            },
        );
        self.out_blocks.insert(
            task.new,
            Block {
                id: task.new,
                params: new_params,
                variadic: new_variadic,
                instructions: new_instructions,
                terminator,
                source: block.source,
            },
        );
    }

    fn walk_instruction(
        &mut self,
        instruction: &Instruction<'gc>,
        ctx: &mut TypeContext,
        map: &mut HashMap<ValueId, ValueId>,
        pred_of: &mut HashMap<ValueId, Predicate>,
        out: &mut Vec<Instruction<'gc>>,
    ) {
        match instruction {
            Instruction::Const { dst, value } => {
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, infer::type_of_constant(*value));
                out.push(Instruction::Const {
                    dst: new_dst,
                    value: *value,
                });
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                let arg_types: Vec<Type> = args.iter().map(|arg| atom_type(arg, ctx)).collect();
                let spec = infer::specialize_prim(*prim, &arg_types);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);

                // Vector lengths get a symbolic `[[v]]` interval so that
                // narrowing `i < len` proves `i` in bounds of `v`.
                if spec.prim == Primitive::VectorLengthUnchecked
                    || spec.prim == Primitive::StringLengthUnchecked
                    || spec.prim == Primitive::BytevectorLengthUnchecked
                {
                    if let Some(vector) = local_of(args.first()) {
                        ctx.set(
                            *dst,
                            Type::fixnum(
                                Bound::VecLenMinus(vector, 0),
                                Bound::VecLenMinus(vector, 0),
                            ),
                        );
                    } else {
                        ctx.set(*dst, spec.result);
                    }
                } else {
                    ctx.set(*dst, spec.result);
                }

                if let Some(op) = infer::cmp_op(*prim) {
                    if let (Some(lhs), Some(rhs)) = (local_of(args.first()), local_of(args.get(1)))
                    {
                        pred_of.insert(*dst, Predicate::Cmp(op, lhs, rhs));
                    }
                } else if infer::is_type_test(*prim)
                    && let Some(arg) = local_of(args.first())
                {
                    pred_of.insert(*dst, Predicate::TypeTest(*prim, arg));
                }

                if let Some(value) = spec.fold {
                    out.push(Instruction::Const {
                        dst: new_dst,
                        value,
                    });
                } else {
                    out.push(Instruction::PrimCall {
                        dst: new_dst,
                        prim: spec.prim,
                        args: remap_atoms(map, args),
                        source: *source,
                    });
                }
            }
            Instruction::MakeClosure {
                dst,
                code,
                kind,
                free_count,
            } => {
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::kind(TypeKind::Procedure));
                out.push(Instruction::MakeClosure {
                    dst: new_dst,
                    code: *code,
                    kind: *kind,
                    free_count: *free_count,
                });
            }
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => {
                let closure = remap_atom(map, closure);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::TOP);
                out.push(Instruction::ClosureRef {
                    dst: new_dst,
                    closure,
                    index: *index,
                });
            }
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => {
                out.push(Instruction::ClosureSet {
                    closure: remap_atom(map, closure),
                    index: *index,
                    value: remap_atom(map, value),
                });
            }
            Instruction::CacheRef {
                dst,
                cache_key,
                source,
            } => {
                let cache_key = remap_atom(map, cache_key);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::TOP);
                out.push(Instruction::CacheRef {
                    dst: new_dst,
                    cache_key,
                    source: *source,
                });
            }
            Instruction::CacheSet {
                dst,
                cache_key,
                value,
                source,
            } => {
                let cache_key = remap_atom(map, cache_key);
                let value = remap_atom(map, value);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::TOP);
                out.push(Instruction::CacheSet {
                    dst: new_dst,
                    cache_key,
                    value,
                    source: *source,
                });
            }
            Instruction::RestToList { dst, rest, source } => {
                let rest = remap_value(map, *rest);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::TOP);
                out.push(Instruction::RestToList {
                    dst: new_dst,
                    rest,
                    source: *source,
                });
            }
            Instruction::RestRef {
                dst,
                rest,
                index,
                source,
            } => {
                let rest = remap_value(map, *rest);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::TOP);
                out.push(Instruction::RestRef {
                    dst: new_dst,
                    rest,
                    index: *index,
                    source: *source,
                });
            }
            Instruction::RestLength {
                dst,
                rest,
                skip,
                source,
            } => {
                let rest = remap_value(map, *rest);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, Type::kind(TypeKind::Fixnum));
                out.push(Instruction::RestLength {
                    dst: new_dst,
                    rest,
                    skip: *skip,
                    source: *source,
                });
            }
            Instruction::RestPredicate {
                dst,
                rest,
                predicate,
                skip,
                source,
            } => {
                let rest = remap_value(map, *rest);
                let new_dst = self.fresh_value();
                map.insert(*dst, new_dst);
                ctx.set(*dst, infer::boolean_type());
                out.push(Instruction::RestPredicate {
                    dst: new_dst,
                    rest,
                    predicate: *predicate,
                    skip: *skip,
                    source: *source,
                });
            }
        }
    }

    fn walk_terminator(
        &mut self,
        terminator: &Terminator<'gc>,
        ctx: &TypeContext,
        map: &HashMap<ValueId, ValueId>,
        pred_of: &HashMap<ValueId, Predicate>,
    ) -> Terminator<'gc> {
        match terminator {
            Terminator::Jump { target, args } => {
                let successor_ctx = self.successor_ctx(*target, args, ctx);
                let new_target = self.reach(*target, successor_ctx, map.clone());
                Terminator::Jump {
                    target: new_target,
                    args: remap_atoms(map, args),
                }
            }
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints,
            } => {
                let test_ty = atom_type(test, ctx);
                match infer::truthiness(&test_ty) {
                    Some(true) => {
                        if let Some(jump) = self.jump_if_local(consequent, ctx, map) {
                            return jump;
                        }
                    }
                    Some(false) => {
                        if let Some(jump) = self.jump_if_local(alternative, ctx, map) {
                            return jump;
                        }
                    }
                    None => {}
                }

                let (true_ctx, false_ctx) = narrow_contexts(test, &test_ty, ctx, pred_of);
                let consequent = self.walk_branch_target(consequent, &true_ctx, map);
                let alternative = self.walk_branch_target(alternative, &false_ctx, map);
                Terminator::Branch {
                    test: remap_atom(map, test),
                    consequent,
                    alternative,
                    hints: *hints,
                }
            }
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => {
                let scrutinee_local = local_of(Some(scrutinee));
                let new_cases = cases
                    .iter()
                    .map(|case| {
                        let mut case_ctx = ctx.clone();
                        if let (Some(id), SwitchCaseValue::Integer(value)) =
                            (scrutinee_local, case.value)
                            && matches!(kind, SwitchKind::Fixnum)
                        {
                            case_ctx.set(id, Type::constant(value as i64));
                        }
                        SwitchCase {
                            value: case.value,
                            target: self.walk_branch_target(&case.target, &case_ctx, map),
                        }
                    })
                    .collect();
                Terminator::Switch {
                    kind: *kind,
                    scrutinee: remap_atom(map, scrutinee),
                    cases: new_cases,
                    default: self.walk_branch_target(default, ctx, map),
                }
            }
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => Terminator::Call {
                callee: remap_atom(map, callee),
                retk: remap_atom(map, retk),
                args: remap_atoms(map, args),
                source: *source,
            },
            Terminator::TailCall {
                callee,
                args,
                source,
            } => Terminator::TailCall {
                callee: remap_atom(map, callee),
                args: remap_atoms(map, args),
                source: *source,
            },
            Terminator::Raise { kind, args, source } => Terminator::Raise {
                kind: *kind,
                args: remap_atoms(map, args),
                source: *source,
            },
        }
    }

    fn jump_if_local(
        &mut self,
        target: &BranchTarget<'gc>,
        ctx: &TypeContext,
        map: &HashMap<ValueId, ValueId>,
    ) -> Option<Terminator<'gc>> {
        match target {
            BranchTarget::Local { block, args } => {
                let successor_ctx = self.successor_ctx(*block, args, ctx);
                let new_target = self.reach(*block, successor_ctx, map.clone());
                Some(Terminator::Jump {
                    target: new_target,
                    args: remap_atoms(map, args),
                })
            }
            BranchTarget::Reified { .. } => None,
        }
    }

    fn walk_branch_target(
        &mut self,
        target: &BranchTarget<'gc>,
        ctx: &TypeContext,
        map: &HashMap<ValueId, ValueId>,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local { block, args } => {
                let successor_ctx = self.successor_ctx(*block, args, ctx);
                let new_block = self.reach(*block, successor_ctx, map.clone());
                BranchTarget::Local {
                    block: new_block,
                    args: remap_atoms(map, args),
                }
            }
            BranchTarget::Reified { continuation, args } => BranchTarget::Reified {
                continuation: remap_atom(map, continuation),
                args: remap_atoms(map, args),
            },
        }
    }

    fn successor_ctx(
        &self,
        target: BlockId,
        args: &[Operand<'gc>],
        ctx: &TypeContext,
    ) -> TypeContext {
        let params = self.block_params.get(&target).cloned().unwrap_or_default();
        let mut successor = TypeContext::new();
        for (param, arg) in params.iter().zip(args.iter()) {
            successor.set(*param, atom_type(arg, ctx));
        }
        successor
    }

    // --- finalization ------------------------------------------------------

    fn finalize(
        &mut self,
        entry_new: BlockId,
    ) -> (BlockId, Vec<Block<'gc>>, HashMap<BlockId, BlockAnnotation>) {
        let entry = self.resolve(entry_new);

        // Breadth-first walk over live blocks, resolving replacement chains.
        let mut order = Vec::new();
        let mut seen = HashSet::new();
        let mut queue = VecDeque::from([entry]);
        while let Some(id) = queue.pop_front() {
            let id = self.resolve(id);
            if !seen.insert(id) {
                continue;
            }
            let Some(block) = self.out_blocks.get(&id) else {
                continue;
            };
            order.push(id);
            for successor in block.terminator.successors() {
                queue.push_back(self.resolve(successor));
            }
        }

        let mut renumber = HashMap::new();
        for (index, id) in order.iter().enumerate() {
            renumber.insert(*id, BlockId(index));
        }

        let mut blocks = Vec::with_capacity(order.len());
        let mut annotations = HashMap::new();
        for id in &order {
            let resolved = self.resolve(*id);
            let new_id = renumber[&resolved];
            let mut block = self.out_blocks.remove(&resolved).expect("live block");
            block.id = new_id;
            block.terminator =
                map_terminator_targets(block.terminator, &|target| renumber[&self.resolve(target)]);
            if let Some(annotation) = self.annotations.remove(&resolved) {
                annotations.insert(new_id, annotation);
            }
            blocks.push(block);
        }

        (renumber[&self.resolve(entry)], blocks, annotations)
    }
}

// --- free helpers ---------------------------------------------------------

fn atom_type<'gc>(atom: &Operand<'gc>, ctx: &TypeContext) -> Type {
    match atom {
        Operand::Local(id) => ctx.get(*id),
        Operand::Constant(value) => infer::type_of_constant(*value),
    }
}

fn local_of(atom: Option<&Operand<'_>>) -> Option<ValueId> {
    match atom {
        Some(Operand::Local(id)) => Some(*id),
        _ => None,
    }
}

fn remap_value(map: &HashMap<ValueId, ValueId>, id: ValueId) -> ValueId {
    map.get(&id).copied().unwrap_or(id)
}

fn remap_atom<'gc>(map: &HashMap<ValueId, ValueId>, atom: &Operand<'gc>) -> Operand<'gc> {
    match atom {
        Operand::Local(id) => Operand::Local(remap_value(map, *id)),
        Operand::Constant(value) => Operand::Constant(*value),
    }
}

fn remap_atoms<'gc>(map: &HashMap<ValueId, ValueId>, atoms: &[Operand<'gc>]) -> Vec<Operand<'gc>> {
    atoms.iter().map(|atom| remap_atom(map, atom)).collect()
}

fn narrow_contexts(
    test: &Operand<'_>,
    test_ty: &Type,
    ctx: &TypeContext,
    pred_of: &HashMap<ValueId, Predicate>,
) -> (TypeContext, TypeContext) {
    let Operand::Local(test_id) = test else {
        return (ctx.clone(), ctx.clone());
    };

    let (mut true_ctx, mut false_ctx) = match pred_of.get(test_id) {
        Some(Predicate::Cmp(op, lhs, rhs)) => ctx.narrow_for_predicate(*op, *lhs, *rhs),
        Some(Predicate::TypeTest(prim, arg)) => {
            infer::narrow_type_test(*prim, *arg, ctx).unwrap_or_else(|| (ctx.clone(), ctx.clone()))
        }
        None => (ctx.clone(), ctx.clone()),
    };

    true_ctx.set(*test_id, exclude_kind(test_ty.clone(), TypeKind::BoolFalse));
    false_ctx.set(*test_id, Type::kind(TypeKind::BoolFalse));
    (true_ctx, false_ctx)
}

fn map_branch_target_targets<'gc>(
    target: BranchTarget<'gc>,
    remap: &impl Fn(BlockId) -> BlockId,
) -> BranchTarget<'gc> {
    match target {
        BranchTarget::Local { block, args } => BranchTarget::Local {
            block: remap(block),
            args,
        },
        reified @ BranchTarget::Reified { .. } => reified,
    }
}

fn map_terminator_targets<'gc>(
    terminator: Terminator<'gc>,
    remap: &impl Fn(BlockId) -> BlockId,
) -> Terminator<'gc> {
    match terminator {
        Terminator::Jump { target, args } => Terminator::Jump {
            target: remap(target),
            args,
        },
        Terminator::Branch {
            test,
            consequent,
            alternative,
            hints,
        } => Terminator::Branch {
            test,
            consequent: map_branch_target_targets(consequent, remap),
            alternative: map_branch_target_targets(alternative, remap),
            hints,
        },
        Terminator::Switch {
            kind,
            scrutinee,
            cases,
            default,
        } => Terminator::Switch {
            kind,
            scrutinee,
            cases: cases
                .into_iter()
                .map(|case| SwitchCase {
                    value: case.value,
                    target: map_branch_target_targets(case.target, remap),
                })
                .collect(),
            default: map_branch_target_targets(default, remap),
        },
        other => other,
    }
}

pub(super) fn max_block_id(procedure: &Procedure<'_>) -> usize {
    procedure
        .blocks
        .iter()
        .map(|block| block.id.0)
        .max()
        .unwrap_or(procedure.entry.0)
}

pub(super) fn max_value_id(procedure: &Procedure<'_>) -> u32 {
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
            for atom in instruction.uses() {
                if let Operand::Local(value) = atom {
                    max_value = max_value.max(value.0);
                }
            }
        }
        for atom in block.terminator.uses() {
            if let Operand::Local(value) = atom {
                max_value = max_value.max(value.0);
            }
        }
    }
    max_value
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ssa::bbv::merge::{merge_contexts, select_version_to_merge};
    use crate::compiler::ssa::{CodeId, GraphCodeId, ProcedureKind};
    use crate::runtime::value::Value;
    use std::collections::HashMap;

    #[test]
    fn select_version_to_merge_picks_most_similar_active_context() {
        let shared = ValueId(1);
        let other = ValueId(2);

        let mut ctx_a = TypeContext::new();
        ctx_a.set(shared, Type::kind(TypeKind::Fixnum));
        ctx_a.set(other, Type::kind(TypeKind::Pair));

        // Most similar to A: agrees on both live-ins.
        let mut ctx_b = TypeContext::new();
        ctx_b.set(shared, Type::kind(TypeKind::Fixnum));
        ctx_b.set(other, Type::kind(TypeKind::Pair));

        // Least similar: disagrees on both live-ins.
        let mut ctx_c = TypeContext::new();
        ctx_c.set(shared, Type::kind(TypeKind::Flonum));
        ctx_c.set(other, Type::kind(TypeKind::Vector));

        let active = vec![ctx_a, ctx_c];

        assert_eq!(select_version_to_merge(&active, &ctx_b), 0);
    }

    #[test]
    fn merge_contexts_joins_live_in_types() {
        let value = ValueId(1);
        let mut ctx_a = TypeContext::new();
        ctx_a.set(value, Type::fixnum_int(1, 3));
        let mut ctx_b = TypeContext::new();
        ctx_b.set(value, Type::fixnum_int(8, 12));

        let merged = merge_contexts(&ctx_a, &ctx_b, false);
        let range = merged.get(value).fixnum_range.expect("fixnum range");
        assert_eq!(range.lo, super::super::types::Bound::Int(1));
        assert_eq!(range.hi, super::super::types::Bound::Int(12));

        // Widening pushes the joined range out to the lattice bounds.
        let widened = merge_contexts(&ctx_a, &ctx_b, true);
        let widened_range = widened.get(value).fixnum_range.expect("fixnum range");
        assert_eq!(widened_range.lo, super::super::types::Bound::Int(0));
        assert_eq!(widened_range.hi, super::super::types::Bound::Max);
    }

    #[test]
    fn interval_loop_converges_at_version_limit() {
        let initial = ValueId(1);
        let current = ValueId(2);
        let one = ValueId(3);
        let next = ValueId(4);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    params: vec![],
                    variadic: None,
                    instructions: vec![Instruction::Const {
                        dst: initial,
                        value: Value::from_i32(0),
                    }],
                    terminator: Terminator::Jump {
                        target: BlockId(1),
                        args: vec![Operand::Local(initial)],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    params: vec![current],
                    variadic: None,
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: next,
                            prim: Primitive::Plus,
                            args: vec![Operand::Local(current), Operand::Local(one)],
                            source,
                        },
                    ],
                    terminator: Terminator::Jump {
                        target: BlockId(1),
                        args: vec![Operand::Local(next)],
                    },
                    source,
                },
            ],
        };

        let (specialized, annotations) = specialize_procedure(procedure, 3);
        assert!(specialized.blocks.len() <= 4);
        assert_eq!(
            annotations
                .values()
                .filter(|annotation| annotation.orig == BlockId(1))
                .count(),
            3
        );
    }

    #[test]
    fn numeric_switch_case_does_not_assume_fixnum_representation() {
        let scrutinee = ValueId(1);
        let case_value = ValueId(2);
        let one = ValueId(3);
        let sum = ValueId(4);
        let result = ValueId(5);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![scrutinee],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    params: vec![scrutinee],
                    variadic: None,
                    instructions: vec![],
                    terminator: Terminator::Switch {
                        kind: SwitchKind::Numeric,
                        scrutinee: Operand::Local(scrutinee),
                        cases: vec![SwitchCase {
                            value: SwitchCaseValue::Integer(1),
                            target: BranchTarget::Local {
                                block: BlockId(1),
                                args: vec![Operand::Local(scrutinee)],
                            },
                        }],
                        default: BranchTarget::Local {
                            block: BlockId(2),
                            args: vec![Operand::Local(scrutinee)],
                        },
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    params: vec![case_value],
                    variadic: None,
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: sum,
                            prim: Primitive::FxAddOvf,
                            args: vec![Operand::Local(case_value), Operand::Local(one)],
                            source,
                        },
                    ],
                    terminator: Terminator::Jump {
                        target: BlockId(2),
                        args: vec![Operand::Local(sum)],
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    params: vec![result],
                    variadic: None,
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(result),
                        args: vec![],
                        source,
                    },
                    source,
                },
            ],
        };

        let (specialized, _) = specialize_procedure(procedure, 2);
        assert!(specialized.blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::PrimCall {
                        prim: Primitive::FxAddOvf,
                        ..
                    }
                )
            })
        }));
    }
}
