//! SBBV specialization driver (ECOOP'24 Algorithms 1–5, Sections 2–3).
//!
//! Versions share the original uvar homes and are keyed by typing contexts
//! projected onto block live-ins (plus alias equivalence classes). Type
//! predicates fold to constants, checked primitives collapse to unchecked
//! variants when the context proves the guard, and branches whose outcome is
//! statically known become unconditional jumps.

use super::liveness::compute_live_in;
use super::types::{
    Bound, CmpOp, Interval, Type, TypeContext, TypeKind, exclude_kind, union_types,
};
use super::{infer, merge};
use crate::compiler::cfg::graph::backedges;
use crate::compiler::cfg::{
    Block, BlockId, BranchTarget, Instruction, Operand, Procedure, RestPredicate, SwitchCase,
    SwitchCaseValue, SwitchKind, Terminator, ValueId,
};
use crate::compiler::cranelift::primitive::Primitive;
use crate::runtime::value::Value;
use std::collections::{HashMap, HashSet, VecDeque};

/// Relation described by a boolean-valued instruction.
#[derive(Clone, Copy)]
enum PredicateRelation {
    /// Binary comparison over (possibly) fixnum operands.
    Cmp(CmpOp, ValueId, ValueId),
    /// Unary type test (`fixnum?`, `pair?`, ...).
    TypeTest(Primitive, ValueId),
}

/// How a boolean-valued instruction narrows the context when branched on.
#[derive(Clone, Copy)]
struct Predicate {
    relation: PredicateRelation,
    inverted: bool,
}

impl Predicate {
    fn direct(relation: PredicateRelation) -> Self {
        Self {
            relation,
            inverted: false,
        }
    }

    fn inverted(self) -> Self {
        Self {
            inverted: !self.inverted,
            ..self
        }
    }
}

/// Records the provenance of a specialized block for debugging and tooling.
pub struct BlockAnnotation {
    /// Original block this version was cloned from.
    pub orig: BlockId,
    /// Rendered entry context under which the version was specialized.
    pub ctx: String,
}

struct VersionInfo {
    ctx: TypeContext,
}

#[derive(Clone)]
struct Task {
    orig: BlockId,
    new: BlockId,
    ctx: TypeContext,
}

struct Specializer<'gc> {
    orig_blocks: HashMap<BlockId, Block<'gc>>,
    live_in: HashMap<BlockId, HashSet<ValueId>>,
    entry_orig: BlockId,
    entry_new: Option<BlockId>,

    versions_of: HashMap<BlockId, Vec<BlockId>>,
    version_orig: HashMap<BlockId, BlockId>,
    all_versions: HashMap<BlockId, VersionInfo>,
    version_by_key: HashMap<(BlockId, TypeContext), BlockId>,
    replacement: HashMap<BlockId, BlockId>,
    merge_targets: HashSet<BlockId>,
    edges: HashSet<(BlockId, BlockId)>,
    /// Resolved successors index (kept in sync with `edges` on insert and with
    /// `replacement` on merge), used for incremental reachability.
    succs: HashMap<BlockId, Vec<BlockId>>,
    /// Resolved block ids reachable from the entry. Maintained incrementally:
    /// reachability over resolved ids only grows (edges are never removed and
    /// merges alias a reachable version onto its merge target), so no full-graph
    /// rescan is ever needed.
    reachable: HashSet<BlockId>,
    limit_checks: HashSet<BlockId>,
    /// Originals flagged for a version-limit check that have not been checked
    /// yet. `ensure_version_limit_tasks` drains this queue instead of rescanning
    /// every known original on every queue pop (which is O(N) per pop on huge
    /// procedures).
    limit_check_queue: VecDeque<BlockId>,
    backedges: HashSet<(BlockId, BlockId)>,
    loop_entry_versions: HashMap<BlockId, BlockId>,
    recurrent_versions: HashMap<BlockId, Vec<BlockId>>,

    out_blocks: HashMap<BlockId, Block<'gc>>,
    annotations: HashMap<BlockId, BlockAnnotation>,
    record_annotations: bool,
    queue: VecDeque<Task>,
    pending: HashMap<BlockId, Task>,
    queued: HashSet<BlockId>,

    next_block: usize,
    version_limit: usize,
}

/// Specializes a procedure with Static Basic Block Versioning.
pub(super) fn specialize_procedure<'gc>(
    procedure: Procedure<'gc>,
    version_limit: usize,
) -> (Procedure<'gc>, HashMap<BlockId, BlockAnnotation>) {
    let record_annotations = cfg!(test)
        || crate::compiler::dump::sbbv_dump_stage_enabled("post-specialize")
        || crate::compiler::dump::sbbv_dump_stage_enabled("all");
    let mut specializer = Specializer::new(&procedure, version_limit.max(1), record_annotations);
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
    fn new(procedure: &Procedure<'gc>, version_limit: usize, record_annotations: bool) -> Self {
        let mut orig_blocks = HashMap::new();
        for block in &procedure.blocks {
            orig_blocks.insert(block.id, block.clone());
        }
        let live_in = {
            let mut _p = crate::utils::pass_profile::ProfileScope::new("cfg.bbv.live_in");
            _p.field("blocks", procedure.blocks.len());
            let live_in = compute_live_in(procedure);
            _p.field("live_in_blocks", live_in.len());
            live_in
        };

        Self {
            orig_blocks,
            live_in,
            entry_orig: procedure.entry,
            entry_new: None,
            versions_of: HashMap::new(),
            version_orig: HashMap::new(),
            all_versions: HashMap::new(),
            version_by_key: HashMap::new(),
            replacement: HashMap::new(),
            merge_targets: HashSet::new(),
            edges: HashSet::new(),
            succs: HashMap::new(),
            reachable: HashSet::new(),
            limit_checks: HashSet::new(),
            limit_check_queue: VecDeque::new(),
            backedges: backedges(procedure),
            loop_entry_versions: HashMap::new(),
            recurrent_versions: HashMap::new(),
            out_blocks: HashMap::new(),
            annotations: HashMap::new(),
            record_annotations,
            queue: VecDeque::new(),
            pending: HashMap::new(),
            queued: HashSet::new(),
            next_block: max_block_id(procedure) + 1,
            version_limit,
        }
    }

    fn run(&mut self) -> Option<(BlockId, Vec<Block<'gc>>, HashMap<BlockId, BlockAnnotation>)> {
        if !self.orig_blocks.contains_key(&self.entry_orig) {
            return None;
        }

        let entry_new = self.reach(self.entry_orig, TypeContext::new(), None);
        self.entry_new = Some(entry_new);
        self.reachable.insert(self.resolve(entry_new));
        loop {
            self.ensure_version_limit_tasks();
            let Some(queued_task) = self.queue.pop_front() else {
                self.ensure_reachable_tasks();
                let originals: Vec<_> = self.versions_of.keys().copied().collect();
                for orig in originals {
                    self.flag_limit_check(orig);
                }
                self.ensure_version_limit_tasks();
                self.reactivate_pending();
                if self.queue.is_empty() {
                    break;
                }
                continue;
            };
            if !self.queued.remove(&queued_task.new) {
                continue;
            }
            let Some(task) = self.pending.remove(&queued_task.new) else {
                continue;
            };
            if !self.is_reachable(task.new) {
                self.pending.insert(task.new, task);
                continue;
            }
            // Algorithm 1 merges on pop when over the limit. Repeat until the
            // live set is within the limit (or this task was merged away) so a
            // burst of Algorithm 2 creates cannot leave specialization running
            // permanently above the cap.
            while self.active_versions(task.orig).len() > self.version_limit {
                self.merge_some(task.orig, Some(task.new));
                if self.resolve(task.new) != task.new {
                    break;
                }
            }
            if self.resolve(task.new) != task.new || self.out_blocks.contains_key(&task.new) {
                continue;
            }
            if !self.is_reachable(task.new) {
                self.pending.insert(task.new, task);
                self.reactivate_pending();
                continue;
            }
            self.walk_block(task);
        }

        Some(self.finalize(entry_new))
    }

    fn fresh_block(&mut self) -> BlockId {
        let block = BlockId(self.next_block);
        self.next_block += 1;
        block
    }

    /// Flags an original block for a future version-limit check. The check is
    /// queued lazily so `ensure_version_limit_tasks` does not rescan every known
    /// original on every queue pop.
    fn flag_limit_check(&mut self, orig: BlockId) {
        if self.limit_checks.insert(orig) {
            self.limit_check_queue.push_back(orig);
        }
    }

    fn resolve(&self, mut id: BlockId) -> BlockId {
        while let Some(next) = self.replacement.get(&id) {
            id = *next;
        }
        id
    }

    fn ctx_key(&self, orig: BlockId, ctx: &TypeContext) -> TypeContext {
        match self.live_in.get(&orig) {
            Some(live) => ctx.canonical(live),
            None => TypeContext::new(),
        }
    }

    fn context_covers(&self, orig: BlockId, broader: &TypeContext, narrower: &TypeContext) -> bool {
        self.live_in.get(&orig).into_iter().flatten().all(|value| {
            let broader_type = broader.get(*value);
            union_types(broader_type.clone(), narrower.get(*value), false) == broader_type
        })
    }

    fn active_versions(&self, orig: BlockId) -> Vec<(BlockId, TypeContext)> {
        self.versions_of
            .get(&orig)
            .map(|ids| {
                ids.iter()
                    .filter(|id| {
                        !self.replacement.contains_key(id)
                            && self.reachable.contains(&self.resolve(**id))
                    })
                    .map(|id| (*id, self.all_versions[id].ctx.clone()))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Marks `seed` and everything reachable from it as reachable, following
    /// the resolved successor index. Returns immediately if already reachable.
    fn mark_reachable_from(&mut self, seed: BlockId) {
        let seed = self.resolve(seed);
        if !self.reachable.insert(seed) {
            return;
        }
        let mut queue = VecDeque::from([seed]);
        while let Some(current) = queue.pop_front() {
            let current = self.resolve(current);
            let Some(targets) = self.succs.get(&current).cloned() else {
                continue;
            };
            for target in targets {
                let target = self.resolve(target);
                if self.reachable.insert(target) {
                    queue.push_back(target);
                }
            }
        }
    }

    fn is_reachable(&self, target: BlockId) -> bool {
        self.reachable.contains(&self.resolve(target))
    }

    fn create_version(&mut self, orig: BlockId, ctx: TypeContext, key: TypeContext) -> BlockId {
        let new = self.fresh_block();
        self.all_versions
            .insert(new, VersionInfo { ctx: ctx.clone() });
        self.versions_of.entry(orig).or_default().push(new);
        self.version_orig.insert(new, orig);
        self.version_by_key.insert((orig, key), new);
        self.flag_limit_check(orig);
        let task = Task { orig, new, ctx };
        self.pending.insert(new, task.clone());
        self.queued.insert(new);
        self.queue.push_back(task);
        new
    }

    fn reach(&mut self, orig: BlockId, ctx: TypeContext, source: Option<BlockId>) -> BlockId {
        let source_orig =
            source.and_then(|source| self.version_orig.get(&self.resolve(source)).copied());
        let is_backedge =
            source_orig.is_some_and(|source| self.backedges.contains(&(source, orig)));
        if is_backedge {
            return self.reach_loop_header(orig, ctx, source);
        }
        let is_loop_header = self.backedges.iter().any(|(_, header)| *header == orig);
        if is_loop_header && let Some(entry) = self.loop_entry_versions.get(&orig).copied() {
            return self.reach_loop_entry(orig, entry, ctx, source);
        }

        let key = self.ctx_key(orig, &ctx);
        let id = match self.version_by_key.get(&(orig, key.clone())).copied() {
            Some(id) => self.resolve(id),
            None => {
                let covering = self
                    .versions_of
                    .get(&orig)
                    .into_iter()
                    .flatten()
                    .copied()
                    .find(|id| {
                        !self.replacement.contains_key(id)
                            && self.merge_targets.contains(id)
                            && self.context_covers(orig, &self.all_versions[id].ctx, &ctx)
                    });
                if let Some(id) = covering {
                    id
                } else {
                    self.create_version(orig, ctx, key.clone())
                }
            }
        };
        self.version_by_key.insert((orig, key), id);
        if let Some(source) = source {
            self.record_edge(orig, Some(source), id);
        }
        if is_loop_header {
            self.loop_entry_versions.entry(orig).or_insert(id);
        }
        id
    }

    fn reach_loop_entry(
        &mut self,
        orig: BlockId,
        entry: BlockId,
        ctx: TypeContext,
        source: Option<BlockId>,
    ) -> BlockId {
        let entry = self.resolve(entry);
        let id = if self.context_covers(orig, &self.all_versions[&entry].ctx, &ctx) {
            entry
        } else {
            let widened = merge::merge_contexts(&self.all_versions[&entry].ctx, &ctx, true);
            let new = self.replace_loop_version(orig, entry, widened);
            self.loop_entry_versions.insert(orig, new);
            new
        };
        self.record_edge(orig, source, id);
        id
    }

    fn reach_loop_header(
        &mut self,
        orig: BlockId,
        ctx: TypeContext,
        source: Option<BlockId>,
    ) -> BlockId {
        let recurrent = self
            .recurrent_versions
            .get(&orig)
            .into_iter()
            .flatten()
            .copied()
            .map(|id| self.resolve(id))
            .find(|id| self.same_kind_shape(orig, &self.all_versions[id].ctx, &ctx));

        let id = if let Some(recurrent) = recurrent {
            if self.context_covers(orig, &self.all_versions[&recurrent].ctx, &ctx) {
                recurrent
            } else {
                self.widen_recurrent_version(orig, recurrent, ctx)
            }
        } else {
            let recurrent_ctx = self
                .loop_entry_versions
                .get(&orig)
                .copied()
                .map(|entry| self.resolve(entry))
                .filter(|entry| self.same_kind_shape(orig, &self.all_versions[entry].ctx, &ctx))
                .map(|entry| merge::merge_contexts(&self.all_versions[&entry].ctx, &ctx, true))
                .unwrap_or(ctx);
            let key = self.ctx_key(orig, &recurrent_ctx);
            let id = self.create_version(orig, recurrent_ctx, key);
            self.merge_targets.insert(id);
            self.recurrent_versions.entry(orig).or_default().push(id);
            id
        };

        self.record_edge(orig, source, id);
        id
    }

    fn record_edge(&mut self, orig: BlockId, source: Option<BlockId>, target: BlockId) {
        if let Some(source) = source
            && self.edges.insert((source, target))
        {
            let src = self.resolve(source);
            let dst = self.resolve(target);
            self.succs.entry(src).or_default().push(dst);
            if self.reachable.contains(&src) {
                self.mark_reachable_from(dst);
            }
        }
        self.flag_limit_check(orig);
    }

    fn same_kind_shape(&self, orig: BlockId, first: &TypeContext, second: &TypeContext) -> bool {
        self.live_in
            .get(&orig)
            .into_iter()
            .flatten()
            .all(|value| first.get(*value).kinds == second.get(*value).kinds)
    }

    fn widen_recurrent_version(
        &mut self,
        orig: BlockId,
        old: BlockId,
        ctx: TypeContext,
    ) -> BlockId {
        let widened = merge::merge_contexts(&self.all_versions[&old].ctx, &ctx, true);
        let new = self.replace_loop_version(orig, old, widened);
        self.merge_targets.insert(new);
        if let Some(versions) = self.recurrent_versions.get_mut(&orig) {
            for version in versions {
                if *version == old {
                    *version = new;
                }
            }
        }
        new
    }

    fn replace_loop_version(&mut self, orig: BlockId, old: BlockId, ctx: TypeContext) -> BlockId {
        let key = self.ctx_key(orig, &ctx);
        let new = self.create_version(orig, ctx, key);
        let old_key = self.resolve(old);
        let old_reachable = self.reachable.contains(&old_key);
        self.replacement.insert(old, new);
        self.merge_succs(old_key, self.resolve(new));
        if old_reachable {
            self.mark_reachable_from(new);
        }
        self.pending.remove(&old);
        self.queued.remove(&old);
        self.ensure_reachable_tasks();
        self.reactivate_pending();
        new
    }

    /// Moves the successor index entries of `from` onto `to` after `from` was
    /// replaced/aliased, so incremental reachability keeps following edges.
    fn merge_succs(&mut self, from: BlockId, to: BlockId) {
        if from == to {
            return;
        }
        if let Some(mut list) = self.succs.remove(&from) {
            self.succs.entry(to).or_default().append(&mut list);
        }
    }

    fn reactivate_pending(&mut self) {
        let reachable: Vec<_> = self
            .pending
            .iter()
            .filter(|(id, _)| {
                !self.queued.contains(id) && self.reachable.contains(&self.resolve(**id))
            })
            .map(|(id, task)| (*id, task.clone()))
            .collect();
        for (id, task) in reachable {
            self.flag_limit_check(task.orig);
            self.queued.insert(id);
            self.queue.push_back(task);
        }
    }

    /// Merges two live versions after the work queue exposes a version-limit
    /// overflow. Delaying this operation until queue pop matches Algorithm 1:
    /// it lets the selection heuristic see all pending contexts first.
    fn merge_some(&mut self, orig: BlockId, incoming: Option<BlockId>) {
        let active = self.active_versions(orig);
        if active.len() < 2 {
            return;
        }

        let contexts: Vec<_> = active.iter().map(|(_, ctx)| ctx.clone()).collect();
        let (first_index, second_index) =
            match incoming.and_then(|incoming| active.iter().position(|(id, _)| *id == incoming)) {
                Some(incoming_index) => (
                    incoming_index,
                    merge::select_version_to_merge_with(&contexts, incoming_index),
                ),
                None => merge::select_versions_to_merge(&contexts),
            };
        self.merge_pair(orig, active[first_index].0, active[second_index].0);
    }

    fn merge_pair(&mut self, orig: BlockId, first: BlockId, second: BlockId) {
        let first_ctx = self.all_versions[&first].ctx.clone();
        let second_ctx = self.all_versions[&second].ctx.clone();
        let merged_ctx = merge::merge_contexts(&first_ctx, &second_ctx, true);
        let merged_key = self.ctx_key(orig, &merged_ctx);
        let merged_id = match self.version_by_key.get(&(orig, merged_key.clone())) {
            Some(id) => self.resolve(*id),
            None => self.create_version(orig, merged_ctx, merged_key),
        };
        self.merge_targets.insert(merged_id);

        let first_key = if first != merged_id {
            Some(self.resolve(first))
        } else {
            None
        };
        let second_key = if second != merged_id {
            Some(self.resolve(second))
        } else {
            None
        };
        let first_reachable = first_key.is_some_and(|key| self.reachable.contains(&key));
        let second_reachable = second_key.is_some_and(|key| self.reachable.contains(&key));

        if first != merged_id {
            self.replacement.insert(first, merged_id);
            self.pending.remove(&first);
            self.queued.remove(&first);
        }
        if second != merged_id {
            self.replacement.insert(second, merged_id);
            self.pending.remove(&second);
            self.queued.remove(&second);
        }
        if let Some(key) = first_key {
            self.merge_succs(key, self.resolve(merged_id));
        }
        if let Some(key) = second_key {
            self.merge_succs(key, self.resolve(merged_id));
        }
        if first_reachable || second_reachable {
            self.mark_reachable_from(merged_id);
        }
        self.flag_limit_check(orig);
        self.ensure_version_task(orig, merged_id);
        self.ensure_reachable_tasks();
        self.reactivate_pending();
    }

    fn ensure_version_task(&mut self, orig: BlockId, new: BlockId) {
        if self.replacement.contains_key(&new)
            || self.out_blocks.contains_key(&new)
            || self.pending.contains_key(&new)
            || self.queued.contains(&new)
        {
            return;
        }
        let version = &self.all_versions[&new];
        let task = Task {
            orig,
            new,
            ctx: version.ctx.clone(),
        };
        self.pending.insert(new, task.clone());
        self.queued.insert(new);
        self.queue.push_back(task);
    }

    /// Keeps the work queue complete after a merge patches existing edges.
    ///
    /// A version can become reachable through an edge that was recorded while
    /// one of its predecessors was still pending. In that case its original
    /// task may already have been consumed by the queue without producing an
    /// output block. Reconstructing the task from the version record is the
    /// worklist equivalent of reactivating the version in Algorithm 1.
    fn ensure_reachable_tasks(&mut self) {
        let missing: Vec<_> = self
            .versions_of
            .iter()
            .flat_map(|(orig, versions)| {
                versions.iter().filter_map(|id| {
                    if self.replacement.contains_key(id)
                        || self.out_blocks.contains_key(id)
                        || self.pending.contains_key(id)
                        || self.queued.contains(id)
                        || !self.reachable.contains(&self.resolve(*id))
                    {
                        return None;
                    }
                    let version = &self.all_versions[id];
                    Some((
                        *id,
                        Task {
                            orig: *orig,
                            new: *id,
                            ctx: version.ctx.clone(),
                        },
                    ))
                })
            })
            .collect();

        for (id, task) in missing {
            self.pending.insert(id, task.clone());
            self.queued.insert(id);
            self.queue.push_back(task);
        }
    }

    /// Schedules a maintenance pop when reachability makes an already
    /// materialized version participate in a limit overflow. Normally a new
    /// version supplies the work-queue pop that triggers Algorithm 1's merge
    /// check. A version that was specialized while unreachable has no such
    /// pending pop, so keep one live version as a merge check until the limit
    /// is restored.
    fn ensure_version_limit_tasks(&mut self) {
        let originals: Vec<_> = self.limit_check_queue.drain(..).collect();
        for orig in originals {
            self.limit_checks.remove(&orig);
            let active = self.active_versions(orig);
            if active.len() <= self.version_limit
                || active
                    .iter()
                    .any(|(id, _)| self.pending.contains_key(id) || self.queued.contains(id))
            {
                continue;
            }

            let (id, ctx) = active[0].clone();
            let task = Task { orig, new: id, ctx };
            self.pending.insert(id, task.clone());
            self.queued.insert(id);
            self.queue.push_back(task);
        }
    }

    fn walk_block(&mut self, task: Task) {
        let block = self.orig_blocks[&task.orig].clone();
        let mut ctx = task.ctx.clone();
        let mut pred_of: HashMap<ValueId, Predicate> = HashMap::new();
        let mut new_instructions = Vec::with_capacity(block.instructions.len());

        for instruction in &block.instructions {
            self.walk_instruction(instruction, &mut ctx, &mut pred_of, &mut new_instructions);
        }

        let terminator = self.walk_terminator(
            &block.terminator,
            &ctx,
            &pred_of,
            task.new,
            &mut new_instructions,
        );
        let successors = terminator.successors();

        self.annotations.insert(
            task.new,
            BlockAnnotation {
                orig: task.orig,
                ctx: if self.record_annotations {
                    format!("{}", task.ctx)
                } else {
                    String::new()
                },
            },
        );
        self.out_blocks.insert(
            task.new,
            Block {
                id: task.new,
                instructions: new_instructions,
                terminator,
                source: block.source,
            },
        );
        for successor in successors {
            let successor = self.resolve(successor);
            if let Some(orig) = self.version_orig.get(&successor).copied() {
                self.flag_limit_check(orig);
            }
            // Reactivate a pending successor version locally: this block's walk
            // is what makes it reachable. (A full `reactivate_pending` scan per
            // walked block is O(pending) per pop and quadratic on huge
            // procedures; merge-driven reachability changes are handled by
            // `ensure_reachable_tasks` in `merge_pair`.)
            if let Some(task) = self.pending.get(&successor).cloned()
                && !self.queued.contains(&successor)
            {
                self.queued.insert(successor);
                self.queue.push_back(task);
            }
        }
    }

    fn walk_instruction(
        &mut self,
        instruction: &Instruction<'gc>,
        ctx: &mut TypeContext,
        pred_of: &mut HashMap<ValueId, Predicate>,
        out: &mut Vec<Instruction<'gc>>,
    ) {
        match instruction {
            Instruction::Assign { dst, src } => {
                match src {
                    Operand::Local(src_id) => ctx.assign_copy(*dst, *src_id),
                    Operand::Constant(value) => {
                        ctx.detach(*dst);
                        ctx.set(*dst, infer::type_of_constant(*value));
                    }
                }
                out.push(instruction.clone());
            }
            Instruction::Const { dst, value } => {
                ctx.detach(*dst);
                ctx.set(*dst, infer::type_of_constant(*value));
                out.push(instruction.clone());
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                let arg_types: Vec<Type> = args.iter().map(|arg| atom_type(arg, ctx)).collect();
                let spec = infer::specialize_prim(*prim, &arg_types);
                ctx.detach(*dst);

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

                if let Some(op) = infer::cmp_op(spec.prim) {
                    if let (Some(lhs), Some(rhs)) = (local_of(args.first()), local_of(args.get(1)))
                    {
                        pred_of.insert(
                            *dst,
                            Predicate::direct(PredicateRelation::Cmp(op, lhs, rhs)),
                        );
                    }
                } else if infer::is_type_test(spec.prim)
                    && let Some(arg) = local_of(args.first())
                {
                    pred_of.insert(
                        *dst,
                        Predicate::direct(PredicateRelation::TypeTest(spec.prim, arg)),
                    );
                } else if spec.prim == Primitive::Not
                    && let Some(arg) = local_of(args.first())
                    && let Some(predicate) = pred_of.get(&arg).copied()
                {
                    pred_of.insert(*dst, predicate.inverted());
                }

                if let Some(value) = spec.fold {
                    out.push(Instruction::Const { dst: *dst, value });
                } else {
                    out.push(Instruction::PrimCall {
                        dst: *dst,
                        prim: spec.prim,
                        args: args.clone(),
                        source: *source,
                    });
                }
            }
            Instruction::MakeClosure { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(*dst, Type::kind(TypeKind::Procedure));
                out.push(instruction.clone());
            }
            Instruction::ClosureRef { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(*dst, Type::TOP);
                out.push(instruction.clone());
            }
            Instruction::ClosureSet { .. } => out.push(instruction.clone()),
            Instruction::CacheRef { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(*dst, Type::TOP);
                out.push(instruction.clone());
            }
            Instruction::CacheSet { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(*dst, Type::TOP);
                out.push(instruction.clone());
            }
            Instruction::RestToList { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(
                    *dst,
                    Type {
                        kinds: super::types::KIND_PAIR | super::types::KIND_NULL,
                        fixnum_range: None,
                        length_range: Some(Interval::TOP_LENGTH),
                        singleton: None,
                    },
                );
                out.push(instruction.clone());
            }
            Instruction::RestRef { dst, .. } => {
                ctx.detach(*dst);
                ctx.set(*dst, Type::TOP);
                out.push(instruction.clone());
            }
            Instruction::RestLength {
                dst, rest, skip, ..
            } => {
                ctx.detach(*dst);
                let mut rest_ty = ctx.get(*rest);
                if rest_ty.length_range.is_none() {
                    rest_ty.length_range = Some(Interval::TOP_LENGTH);
                    ctx.set(*rest, rest_ty);
                }
                if let Some(len) = length_singleton(&ctx.get(*rest)) {
                    let n = (len - *skip as i64).max(0);
                    ctx.set(*dst, Type::constant(n));
                    out.push(Instruction::Const {
                        dst: *dst,
                        value: Value::from_i32(n as i32),
                    });
                } else {
                    ctx.set(
                        *dst,
                        Type::fixnum(
                            Bound::VecLenMinus(*rest, *skip as i64),
                            Bound::VecLenMinus(*rest, *skip as i64),
                        ),
                    );
                    out.push(instruction.clone());
                }
            }
            Instruction::RestPredicate {
                dst,
                rest,
                predicate,
                skip,
                ..
            } => {
                ctx.detach(*dst);
                match fold_rest_predicate(&ctx.get(*rest), *predicate, *skip) {
                    Some(value) => {
                        ctx.set(
                            *dst,
                            Type {
                                kinds: if value {
                                    super::types::KIND_BOOL_TRUE
                                } else {
                                    super::types::KIND_BOOL_FALSE
                                },
                                fixnum_range: None,
                                length_range: None,
                                singleton: None,
                            },
                        );
                        out.push(Instruction::Const {
                            dst: *dst,
                            value: Value::from_bool(value),
                        });
                    }
                    None => {
                        ctx.set(*dst, infer::boolean_type());
                        out.push(instruction.clone());
                    }
                }
            }
        }
    }

    fn walk_terminator(
        &mut self,
        terminator: &Terminator<'gc>,
        ctx: &TypeContext,
        pred_of: &HashMap<ValueId, Predicate>,
        source: BlockId,
        new_instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match terminator {
            Terminator::Jump { target } => {
                let successor_ctx = self.successor_ctx(*target, ctx);
                let new_target = self.reach(*target, successor_ctx, Some(source));
                Terminator::Jump { target: new_target }
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
                        if let Some(jump) = self.jump_if_local(consequent, ctx, source) {
                            return jump;
                        }
                    }
                    Some(false) => {
                        if let Some(jump) = self.jump_if_local(alternative, ctx, source) {
                            return jump;
                        }
                    }
                    None => {}
                }

                if let Some(fused) = try_fuse_branch_prim(
                    test,
                    consequent,
                    alternative,
                    *hints,
                    ctx,
                    pred_of,
                    new_instructions,
                    |target, arm_ctx| self.walk_branch_target(target, arm_ctx, source),
                ) {
                    return fused;
                }

                let (true_ctx, false_ctx) = narrow_contexts(test, &test_ty, ctx, pred_of);
                let consequent = self.walk_branch_target(consequent, &true_ctx, source);
                let alternative = self.walk_branch_target(alternative, &false_ctx, source);
                Terminator::Branch {
                    test: *test,
                    consequent,
                    alternative,
                    hints: *hints,
                }
            }
            Terminator::BranchPrim {
                prim,
                args,
                consequent,
                alternative,
                hints,
            } => {
                // Rebuild a synthetic test local for narrowing when possible.
                let (true_ctx, false_ctx) = narrow_branch_prim(*prim, args, ctx);
                Terminator::BranchPrim {
                    prim: *prim,
                    args: args.clone(),
                    consequent: self.walk_branch_target(consequent, &true_ctx, source),
                    alternative: self.walk_branch_target(alternative, &false_ctx, source),
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
                            target: self.walk_branch_target(&case.target, &case_ctx, source),
                        }
                    })
                    .collect();
                Terminator::Switch {
                    kind: *kind,
                    scrutinee: *scrutinee,
                    cases: new_cases,
                    default: self.walk_branch_target(default, ctx, source),
                }
            }
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => Terminator::Call {
                callee: *callee,
                retk: *retk,
                args: args.clone(),
                source: *source,
            },
            Terminator::TailCall {
                callee,
                args,
                source,
            } => Terminator::TailCall {
                callee: *callee,
                args: args.clone(),
                source: *source,
            },
            Terminator::Raise { kind, args, source } => Terminator::Raise {
                kind: *kind,
                args: args.clone(),
                source: *source,
            },
        }
    }

    fn jump_if_local(
        &mut self,
        target: &BranchTarget<'gc>,
        ctx: &TypeContext,
        source: BlockId,
    ) -> Option<Terminator<'gc>> {
        match target {
            BranchTarget::Local {
                block,
                edge_assigns,
            } => {
                // Plain Jump cannot carry edge_assigns; refuse to fold.
                if !edge_assigns.is_empty() {
                    return None;
                }
                let successor_ctx = self.successor_ctx(*block, ctx);
                let new_target = self.reach(*block, successor_ctx, Some(source));
                Some(Terminator::Jump { target: new_target })
            }
            BranchTarget::Reified { .. } => None,
        }
    }

    fn walk_branch_target(
        &mut self,
        target: &BranchTarget<'gc>,
        ctx: &TypeContext,
        source: BlockId,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local {
                block,
                edge_assigns,
            } => {
                let successor_ctx = self.successor_ctx(*block, ctx);
                let new_block = self.reach(*block, successor_ctx, Some(source));
                BranchTarget::Local {
                    block: new_block,
                    edge_assigns: edge_assigns.clone(),
                }
            }
            BranchTarget::Reified { continuation, args } => BranchTarget::Reified {
                continuation: *continuation,
                args: args.clone(),
            },
        }
    }

    fn successor_ctx(&self, target: BlockId, ctx: &TypeContext) -> TypeContext {
        match self.live_in.get(&target) {
            Some(live) => ctx.canonical(live),
            None => TypeContext::new(),
        }
    }

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

fn length_singleton(ty: &Type) -> Option<i64> {
    let interval = ty.length_range?;
    match (interval.lo, interval.hi) {
        (Bound::Int(lo), Bound::Int(hi)) if lo == hi => Some(lo),
        _ => None,
    }
}

/// Fold `RestPredicate` when the rest value's length interval proves the answer.
///
/// Rest formals are always proper lists, so `list?` is unconditionally true.
fn fold_rest_predicate(rest_ty: &Type, predicate: RestPredicate, skip: usize) -> Option<bool> {
    let skip = skip as i64;
    match predicate {
        RestPredicate::List => Some(true),
        RestPredicate::Null | RestPredicate::Pair => {
            let interval = rest_ty.length_range?;
            let (Bound::Int(lo), Bound::Int(hi)) = (interval.lo, interval.hi) else {
                return None;
            };
            match predicate {
                RestPredicate::Null => {
                    if hi <= skip {
                        Some(true)
                    } else if lo > skip {
                        Some(false)
                    } else {
                        None
                    }
                }
                RestPredicate::Pair => {
                    if lo > skip {
                        Some(true)
                    } else if hi <= skip {
                        Some(false)
                    } else {
                        None
                    }
                }
                RestPredicate::List => unreachable!(),
            }
        }
    }
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

    let predicate = pred_of.get(test_id);
    let (mut true_ctx, mut false_ctx) = match predicate.map(|predicate| predicate.relation) {
        Some(PredicateRelation::Cmp(op, lhs, rhs)) => ctx.narrow_for_predicate(op, lhs, rhs),
        Some(PredicateRelation::TypeTest(prim, arg)) => {
            infer::narrow_type_test(prim, arg, ctx).unwrap_or_else(|| (ctx.clone(), ctx.clone()))
        }
        None => (ctx.clone(), ctx.clone()),
    };

    if predicate.is_some_and(|predicate| predicate.inverted) {
        std::mem::swap(&mut true_ctx, &mut false_ctx);
    }

    true_ctx.set(*test_id, exclude_kind(test_ty.clone(), TypeKind::BoolFalse));
    false_ctx.set(*test_id, Type::kind(TypeKind::BoolFalse));
    (true_ctx, false_ctx)
}

fn map_branch_target_targets<'gc>(
    target: BranchTarget<'gc>,
    remap: &impl Fn(BlockId) -> BlockId,
) -> BranchTarget<'gc> {
    match target {
        BranchTarget::Local {
            block,
            edge_assigns,
        } => BranchTarget::Local {
            block: remap(block),
            edge_assigns,
        },
        reified @ BranchTarget::Reified { .. } => reified,
    }
}

fn narrow_branch_prim(
    prim: Primitive,
    args: &[Operand<'_>],
    ctx: &TypeContext,
) -> (TypeContext, TypeContext) {
    if let Some(op) = infer::cmp_op(prim)
        && let (Some(lhs), Some(rhs)) = (local_of(args.first()), local_of(args.get(1)))
    {
        return ctx.narrow_for_predicate(op, lhs, rhs);
    }
    if infer::is_type_test(prim)
        && let Some(arg) = local_of(args.first())
    {
        return infer::narrow_type_test(prim, arg, ctx)
            .unwrap_or_else(|| (ctx.clone(), ctx.clone()));
    }
    (ctx.clone(), ctx.clone())
}

/// Whether `prim` lowers to an i1 predicate suitable for fused `brif`.
fn is_fusable_branch_prim(prim: Primitive) -> bool {
    infer::cmp_op(prim).is_some()
        || infer::is_type_test(prim)
        || matches!(
            prim,
            Primitive::IsEq
                | Primitive::IsEqv
                | Primitive::IsEqual
                | Primitive::IsEofObject
                | Primitive::IsList
                | Primitive::IsUnspecified
                | Primitive::Not
                | Primitive::IsNan
                | Primitive::IsInexact
                | Primitive::IsExact
                | Primitive::IsInteger
                | Primitive::IsRational
        )
}

fn operand_uses_local(op: &Operand<'_>, local: ValueId) -> bool {
    matches!(op, Operand::Local(id) if *id == local)
}

fn instruction_uses_local(instruction: &Instruction<'_>, local: ValueId) -> bool {
    instruction
        .uses()
        .iter()
        .any(|op| operand_uses_local(op, local))
}

/// Fuse single-use predicate/compare `PrimCall` + `Branch` into `BranchPrim`.
#[allow(clippy::too_many_arguments)]
fn try_fuse_branch_prim<'gc>(
    test: &Operand<'gc>,
    consequent: &BranchTarget<'gc>,
    alternative: &BranchTarget<'gc>,
    hints: [crate::compiler::cps::graph::BranchHint; 2],
    ctx: &TypeContext,
    pred_of: &HashMap<ValueId, Predicate>,
    new_instructions: &mut Vec<Instruction<'gc>>,
    mut walk_target: impl FnMut(&BranchTarget<'gc>, &TypeContext) -> BranchTarget<'gc>,
) -> Option<Terminator<'gc>> {
    let Operand::Local(dst) = *test else {
        return None;
    };

    let idx = new_instructions.iter().rposition(|instruction| {
        matches!(
            instruction,
            Instruction::PrimCall { dst: def, prim, .. }
                if *def == dst && is_fusable_branch_prim(*prim)
        )
    })?;

    // `dst` must not be used by later instructions (only the branch test).
    if new_instructions[idx + 1..]
        .iter()
        .any(|instruction| instruction_uses_local(instruction, dst))
    {
        return None;
    }
    // Edge assigns on either arm must not mention `dst`.
    for target in [consequent, alternative] {
        if let BranchTarget::Local { edge_assigns, .. } = target
            && edge_assigns
                .iter()
                .any(|instruction| instruction_uses_local(instruction, dst))
        {
            return None;
        }
    }

    let Instruction::PrimCall { prim, args, .. } = new_instructions.remove(idx) else {
        unreachable!("rposition matched PrimCall");
    };

    // Prefer pred_of narrowing (handles `not` inversion); fall back to prim/args.
    let test_ty = ctx.get(dst);
    let (true_ctx, false_ctx) = if pred_of.contains_key(&dst) {
        narrow_contexts(test, &test_ty, ctx, pred_of)
    } else {
        narrow_branch_prim(prim, &args, ctx)
    };

    Some(Terminator::BranchPrim {
        prim,
        args,
        consequent: walk_target(consequent, &true_ctx),
        alternative: walk_target(alternative, &false_ctx),
        hints,
    })
}

fn map_terminator_targets<'gc>(
    terminator: Terminator<'gc>,
    remap: &impl Fn(BlockId) -> BlockId,
) -> Terminator<'gc> {
    match terminator {
        Terminator::Jump { target } => Terminator::Jump {
            target: remap(target),
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
        Terminator::BranchPrim {
            prim,
            args,
            consequent,
            alternative,
            hints,
        } => Terminator::BranchPrim {
            prim,
            args,
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
    use crate::compiler::cfg::bbv::merge::{
        merge_contexts, select_version_to_merge_with, select_versions_to_merge,
    };
    use crate::compiler::cfg::{CodeId, GraphCodeId, ProcedureKind};
    use crate::compiler::cps::graph::BranchHint;
    use crate::runtime::value::Value;
    use std::collections::HashMap;

    #[test]
    fn negated_type_test_narrows_assertion_success_path() {
        let input = ValueId(1);
        let is_fixnum = ValueId(2);
        let assertion_fails = ValueId(3);
        let one = ValueId(4);
        let sum = ValueId(5);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![input],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![
                        Instruction::PrimCall {
                            dst: is_fixnum,
                            prim: Primitive::IsFixnum,
                            args: vec![Operand::Local(input)],
                            source,
                        },
                        Instruction::PrimCall {
                            dst: assertion_fails,
                            prim: Primitive::Not,
                            args: vec![Operand::Local(is_fixnum)],
                            source,
                        },
                    ],
                    terminator: Terminator::Branch {
                        test: Operand::Local(assertion_fails),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Cold, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![],
                    terminator: Terminator::Raise {
                        kind: crate::runtime::vm::exceptions::RaiseKind::AssertionViolation,
                        args: vec![],
                        source,
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: sum,
                            prim: Primitive::Plus,
                            args: vec![Operand::Local(input), Operand::Local(one)],
                            source,
                        },
                    ],
                    terminator: Terminator::Raise {
                        kind: crate::runtime::vm::exceptions::RaiseKind::AssertionViolation,
                        args: vec![Operand::Local(sum)],
                        source,
                    },
                    source,
                },
            ],
        };

        let expanded = super::super::expand::expand_procedure(procedure);
        let (specialized, _) = specialize_procedure(expanded, 4);
        assert!(!specialized.blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::PrimCall {
                        prim: Primitive::IsFlonum,
                        ..
                    }
                )
            })
        }));
        assert!(specialized.blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::PrimCall {
                        prim: Primitive::FxAddUnchecked,
                        ..
                    } | Instruction::PrimCall {
                        prim: Primitive::FxAddOvfUnchecked,
                        ..
                    }
                )
            })
        }));
    }

    #[test]
    fn predicate_inversion_is_reversible() {
        let predicate =
            Predicate::direct(PredicateRelation::TypeTest(Primitive::IsFlonum, ValueId(1)));

        assert!(predicate.inverted().inverted);
        assert!(!predicate.inverted().inverted().inverted);
    }

    #[test]
    fn select_versions_to_merge_picks_most_similar_pair() {
        let shared = ValueId(1);
        let other = ValueId(2);

        let mut ctx_a = TypeContext::new();
        ctx_a.set(shared, Type::kind(TypeKind::Fixnum));
        ctx_a.set(other, Type::kind(TypeKind::Pair));

        // Disagrees with A on both live-ins.
        let mut ctx_c = TypeContext::new();
        ctx_c.set(shared, Type::kind(TypeKind::Flonum));
        ctx_c.set(other, Type::kind(TypeKind::Vector));

        let mut ctx_d = TypeContext::new();
        ctx_d.set(shared, Type::kind(TypeKind::Fixnum));
        ctx_d.set(other, Type::kind(TypeKind::String));

        let active = vec![ctx_a, ctx_c, ctx_d];

        assert_eq!(select_versions_to_merge(&active), (0, 2));
    }

    #[test]
    fn incoming_version_merges_with_most_similar_active_version() {
        let value = ValueId(1);
        let mut fixnum = TypeContext::new();
        fixnum.set(value, Type::kind(TypeKind::Fixnum));
        let mut pair = TypeContext::new();
        pair.set(value, Type::kind(TypeKind::Pair));
        let mut incoming = TypeContext::new();
        incoming.set(value, Type::constant(1));

        let active = vec![fixnum, pair, incoming];
        assert_eq!(select_version_to_merge_with(&active, 2), 0);
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
    fn monomorphic_interval_loop_merges_similar_contexts() {
        let current = ValueId(1);
        let one = ValueId(2);
        let next = ValueId(3);
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
                    instructions: vec![Instruction::Const {
                        dst: current,
                        value: Value::from_i32(0),
                    }],
                    terminator: Terminator::Jump { target: BlockId(1) },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: next,
                            prim: Primitive::FxAdd,
                            args: vec![Operand::Local(current), Operand::Local(one)],
                            source,
                        },
                        Instruction::Assign {
                            dst: current,
                            src: Operand::Local(next),
                        },
                    ],
                    terminator: Terminator::Jump { target: BlockId(1) },
                    source,
                },
            ],
        };

        for version_limit in [2, 4, 8] {
            let (specialized, annotations) = specialize_procedure(procedure.clone(), version_limit);
            assert!(specialized.blocks.len() <= 4);
            let contexts: Vec<_> = annotations
                .values()
                .filter(|annotation| annotation.orig == BlockId(1))
                .map(|annotation| annotation.ctx.clone())
                .collect();
            assert_eq!(
                contexts.len(),
                2,
                "unexpected loop contexts at limit {version_limit}: {contexts:?}"
            );
            assert!(
                contexts.iter().any(|context| context.contains("[0..0]")),
                "missing entry context: {contexts:?}"
            );
            assert!(
                contexts
                    .iter()
                    .any(|context| context.contains("fx[>=..<=]")),
                "missing recurrent context: {contexts:?}"
            );
        }
    }

    #[test]
    fn multi_backedge_interval_loop_converges_under_version_limit() {
        let condition = ValueId(1);
        let zero = ValueId(2);
        let ten = ValueId(3);
        let current = ValueId(4);
        let one = ValueId(5);
        let next = ValueId(6);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![condition],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![
                        Instruction::Const {
                            dst: zero,
                            value: Value::from_i32(0),
                        },
                        Instruction::Const {
                            dst: ten,
                            value: Value::from_i32(10),
                        },
                    ],
                    terminator: Terminator::Branch {
                        test: Operand::Local(condition),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![Instruction::Assign {
                        dst: current,
                        src: Operand::Local(zero),
                    }],
                    terminator: Terminator::Jump { target: BlockId(3) },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![Instruction::Assign {
                        dst: current,
                        src: Operand::Local(ten),
                    }],
                    terminator: Terminator::Jump { target: BlockId(3) },
                    source,
                },
                Block {
                    id: BlockId(3),
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: next,
                            prim: Primitive::FxAdd,
                            args: vec![Operand::Local(current), Operand::Local(one)],
                            source,
                        },
                        Instruction::Assign {
                            dst: current,
                            src: Operand::Local(next),
                        },
                    ],
                    terminator: Terminator::Jump { target: BlockId(3) },
                    source,
                },
            ],
        };

        let (specialized, annotations) = specialize_procedure(procedure, 2);
        let header_versions = annotations
            .values()
            .filter(|annotation| annotation.orig == BlockId(3))
            .count();
        assert!(
            header_versions <= 2,
            "expected ≤2 header versions, got {header_versions}: {:?}",
            annotations
                .values()
                .filter(|a| a.orig == BlockId(3))
                .map(|a| a.ctx.clone())
                .collect::<Vec<_>>()
        );
        assert!(specialized.blocks.len() <= 5);
    }

    #[test]
    fn polymorphic_loop_keeps_distinct_kind_versions() {
        let condition = ValueId(1);
        let current = ValueId(2);
        let next_fixnum = ValueId(3);
        let next_char = ValueId(4);
        let is_fixnum = ValueId(5);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![condition],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![Instruction::Const {
                        dst: current,
                        value: Value::from_i32(0),
                    }],
                    terminator: Terminator::Jump { target: BlockId(1) },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![Instruction::PrimCall {
                        dst: is_fixnum,
                        prim: Primitive::IsFixnum,
                        args: vec![Operand::Local(current)],
                        source,
                    }],
                    terminator: Terminator::Branch {
                        test: Operand::Local(condition),
                        consequent: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(3),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![
                        Instruction::Const {
                            dst: next_fixnum,
                            value: Value::from_i32(1),
                        },
                        Instruction::Assign {
                            dst: current,
                            src: Operand::Local(next_fixnum),
                        },
                    ],
                    terminator: Terminator::Jump { target: BlockId(1) },
                    source,
                },
                Block {
                    id: BlockId(3),
                    instructions: vec![
                        Instruction::Const {
                            dst: next_char,
                            value: Value::from_char('s'),
                        },
                        Instruction::Assign {
                            dst: current,
                            src: Operand::Local(next_char),
                        },
                    ],
                    terminator: Terminator::Jump { target: BlockId(1) },
                    source,
                },
            ],
        };

        let (_, annotations) = specialize_procedure(procedure, 2);
        let contexts: Vec<_> = annotations
            .values()
            .filter(|annotation| annotation.orig == BlockId(1))
            .map(|annotation| annotation.ctx.clone())
            .collect();
        assert_eq!(
            contexts.len(),
            2,
            "unexpected loop-header contexts: {contexts:?}"
        );
        assert!(contexts.iter().any(|context| context.contains("fixnum")));
        assert!(contexts.iter().any(|context| context.contains("char")));
    }

    #[test]
    fn numeric_switch_case_does_not_assume_fixnum_representation() {
        let scrutinee = ValueId(1);
        let one = ValueId(2);
        let sum = ValueId(3);
        let result = ValueId(4);
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
                    instructions: vec![],
                    terminator: Terminator::Switch {
                        kind: SwitchKind::Numeric,
                        scrutinee: Operand::Local(scrutinee),
                        cases: vec![SwitchCase {
                            value: SwitchCaseValue::Integer(1),
                            target: BranchTarget::Local {
                                block: BlockId(1),
                                edge_assigns: vec![],
                            },
                        }],
                        default: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: sum,
                            prim: Primitive::FxAddOvf,
                            args: vec![Operand::Local(scrutinee), Operand::Local(one)],
                            source,
                        },
                        Instruction::Assign {
                            dst: result,
                            src: Operand::Local(sum),
                        },
                    ],
                    terminator: Terminator::Jump { target: BlockId(2) },
                    source,
                },
                Block {
                    id: BlockId(2),
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

    #[test]
    fn fold_rest_predicate_uses_length_interval() {
        let unknown = Type {
            kinds: super::super::types::KIND_OTHER,
            fixnum_range: None,
            length_range: Some(Interval::TOP_LENGTH),
            singleton: None,
        };
        assert_eq!(
            fold_rest_predicate(&unknown, RestPredicate::List, 0),
            Some(true)
        );
        assert_eq!(fold_rest_predicate(&unknown, RestPredicate::Null, 0), None);

        let empty = Type {
            kinds: super::super::types::KIND_OTHER,
            fixnum_range: None,
            length_range: Some(Interval::singleton(0)),
            singleton: None,
        };
        assert_eq!(
            fold_rest_predicate(&empty, RestPredicate::Null, 0),
            Some(true)
        );
        assert_eq!(
            fold_rest_predicate(&empty, RestPredicate::Pair, 0),
            Some(false)
        );

        let nonempty = Type {
            kinds: super::super::types::KIND_OTHER,
            fixnum_range: None,
            length_range: Some(Interval {
                lo: Bound::Int(2),
                hi: Bound::Int(5),
            }),
            singleton: None,
        };
        assert_eq!(
            fold_rest_predicate(&nonempty, RestPredicate::Null, 0),
            Some(false)
        );
        assert_eq!(
            fold_rest_predicate(&nonempty, RestPredicate::Pair, 0),
            Some(true)
        );
        assert_eq!(fold_rest_predicate(&nonempty, RestPredicate::Null, 2), None);
    }

    #[test]
    fn specialize_fuses_fx_lt_unchecked_into_branch_prim() {
        let lhs = ValueId(1);
        let rhs = ValueId(2);
        let cmp = ValueId(3);
        let source = Value::new(false);
        let procedure = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![lhs, rhs],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![Instruction::PrimCall {
                        dst: cmp,
                        prim: Primitive::FxLtUnchecked,
                        args: vec![Operand::Local(lhs), Operand::Local(rhs)],
                        source,
                    }],
                    terminator: Terminator::Branch {
                        test: Operand::Local(cmp),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(lhs),
                        args: vec![],
                        source,
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(rhs),
                        args: vec![],
                        source,
                    },
                    source,
                },
            ],
        };

        let (specialized, _) = specialize_procedure(procedure, 2);
        let entry = specialized
            .blocks
            .iter()
            .find(|block| block.id == specialized.entry)
            .expect("entry block");
        assert!(
            !entry.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::PrimCall {
                        prim: Primitive::FxLtUnchecked,
                        ..
                    }
                )
            }),
            "FxLtUnchecked PrimCall should be fused away: {:?}",
            entry.instructions
        );
        assert!(
            matches!(
                &entry.terminator,
                Terminator::BranchPrim {
                    prim: Primitive::FxLtUnchecked,
                    ..
                }
            ),
            "expected BranchPrim FxLtUnchecked, got {:?}",
            entry.terminator
        );
    }

    #[test]
    fn specialize_fuses_is_fixnum_and_numeric_lt_into_branch_prim() {
        let x = ValueId(1);
        let y = ValueId(2);
        let is_fx = ValueId(3);
        let cmp = ValueId(4);
        let source = Value::new(false);

        let type_test = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![x],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![Instruction::PrimCall {
                        dst: is_fx,
                        prim: Primitive::IsFixnum,
                        args: vec![Operand::Local(x)],
                        source,
                    }],
                    terminator: Terminator::Branch {
                        test: Operand::Local(is_fx),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(x),
                        args: vec![],
                        source,
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(x),
                        args: vec![],
                        source,
                    },
                    source,
                },
            ],
        };
        let (specialized, _) = specialize_procedure(type_test, 2);
        let entry = specialized
            .blocks
            .iter()
            .find(|block| block.id == specialized.entry)
            .expect("entry");
        assert!(matches!(
            &entry.terminator,
            Terminator::BranchPrim {
                prim: Primitive::IsFixnum,
                ..
            }
        ));

        let numeric = Procedure {
            code: CodeId::GraphFunction(GraphCodeId(1)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: None,
            params: vec![x, y],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    instructions: vec![Instruction::PrimCall {
                        dst: cmp,
                        prim: Primitive::NumericLt,
                        args: vec![Operand::Local(x), Operand::Local(y)],
                        source,
                    }],
                    terminator: Terminator::Branch {
                        test: Operand::Local(cmp),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            edge_assigns: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            edge_assigns: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(x),
                        args: vec![],
                        source,
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(y),
                        args: vec![],
                        source,
                    },
                    source,
                },
            ],
        };
        let (specialized, _) = specialize_procedure(numeric, 2);
        let entry = specialized
            .blocks
            .iter()
            .find(|block| block.id == specialized.entry)
            .expect("entry");
        // Specialize may rewrite NumericLt → FxLtUnchecked when types prove fixnum;
        // either form must be fused into BranchPrim.
        assert!(
            matches!(
                &entry.terminator,
                Terminator::BranchPrim {
                    prim: Primitive::NumericLt | Primitive::FxLtUnchecked | Primitive::FxLt,
                    ..
                }
            ),
            "expected fused numeric/fx compare branch, got {:?}",
            entry.terminator
        );
    }
}
