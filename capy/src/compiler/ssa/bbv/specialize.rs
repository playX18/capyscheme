//! SBBV specialization driver (ECOOP'24 Algorithms 1–5, Sections 2–3).
//!
//! [`specialize_procedure`] follows the paper worklist: `reach` is Algorithm 2
//! (`BlockNewVersion`) and always materializes a fresh version for an unseen
//! context; Algorithm 1 merges only when a version is popped and the live count
//! exceeds the limit. Widened merge results are reused for covered incoming
//! contexts so interval loops still converge. Type predicates fold to constants,
//! checked primitives collapse to unchecked variants when the context proves the
//! guard, and branches whose outcome is statically known become unconditional
//! jumps.
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

use super::types::{
    Bound, CmpOp, Interval, Type, TypeContext, TypeKind, exclude_kind, union_types,
};
use super::{infer, merge};
use crate::compiler::cranelift::primitive::Primitive;
use crate::compiler::ssa::graph::backedges;
use crate::compiler::ssa::{
    Block, BlockId, BranchTarget, Instruction, Operand, Procedure, RestPredicate, SwitchCase,
    SwitchCaseValue, SwitchKind, Terminator, ValueId,
};
use crate::runtime::value::Value;
use std::cell::{Cell, RefCell};
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
    subst: HashMap<ValueId, ValueId>,
}

#[derive(Clone)]
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
    entry_new: Option<BlockId>,

    versions_of: HashMap<BlockId, Vec<BlockId>>,
    version_orig: HashMap<BlockId, BlockId>,
    all_versions: HashMap<BlockId, VersionInfo>,
    version_by_key: HashMap<(BlockId, String), BlockId>,
    replacement: HashMap<BlockId, BlockId>,
    merge_targets: HashSet<BlockId>,
    edges: HashSet<(BlockId, BlockId)>,
    reachable_cache: RefCell<HashSet<BlockId>>,
    reachability_dirty: Cell<bool>,
    limit_checks: HashSet<BlockId>,
    backedges: HashSet<(BlockId, BlockId)>,
    loop_entry_versions: HashMap<BlockId, BlockId>,
    recurrent_versions: HashMap<BlockId, Vec<BlockId>>,

    out_blocks: HashMap<BlockId, Block<'gc>>,
    annotations: HashMap<BlockId, BlockAnnotation>,
    queue: VecDeque<Task>,
    pending: HashMap<BlockId, Task>,
    queued: HashSet<BlockId>,

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
            entry_new: None,
            versions_of: HashMap::new(),
            version_orig: HashMap::new(),
            all_versions: HashMap::new(),
            version_by_key: HashMap::new(),
            replacement: HashMap::new(),
            merge_targets: HashSet::new(),
            edges: HashSet::new(),
            reachable_cache: RefCell::new(HashSet::new()),
            reachability_dirty: Cell::new(true),
            limit_checks: HashSet::new(),
            backedges: backedges(procedure),
            loop_entry_versions: HashMap::new(),
            recurrent_versions: HashMap::new(),
            out_blocks: HashMap::new(),
            annotations: HashMap::new(),
            queue: VecDeque::new(),
            pending: HashMap::new(),
            queued: HashSet::new(),
            next_block: max_block_id(procedure) + 1,
            next_value: max_value_id(procedure) + 1,
            version_limit,
        }
    }

    fn run(&mut self) -> Option<(BlockId, Vec<Block<'gc>>, HashMap<BlockId, BlockAnnotation>)> {
        if !self.orig_blocks.contains_key(&self.entry_orig) {
            return None;
        }

        let entry_new = self.reach(self.entry_orig, TypeContext::new(), HashMap::new(), None);
        self.entry_new = Some(entry_new);
        loop {
            self.ensure_version_limit_tasks();
            self.reactivate_pending();
            let Some(queued_task) = self.queue.pop_front() else {
                self.ensure_reachable_tasks();
                self.limit_checks.extend(self.versions_of.keys().copied());
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
        // `thread_live_ins` makes original block parameters the complete set
        // of non-procedure values that can be used across an incoming edge.
        // The key therefore projects the full context onto those parameters;
        // the full context remains stored for merging and specialization.
        let params = self.block_params.get(&orig).cloned().unwrap_or_default();
        params
            .iter()
            .map(|param| ctx.get(*param).to_string())
            .collect::<Vec<_>>()
            .join(";")
    }

    fn context_covers(&self, orig: BlockId, broader: &TypeContext, narrower: &TypeContext) -> bool {
        self.block_params
            .get(&orig)
            .into_iter()
            .flatten()
            .all(|param| {
                let broader_type = broader.get(*param);
                union_types(broader_type.clone(), narrower.get(*param), false) == broader_type
            })
    }

    fn active_versions(&self, orig: BlockId) -> Vec<(BlockId, TypeContext)> {
        self.refresh_reachability();
        let reachable = self.reachable_cache.borrow();
        self.versions_of
            .get(&orig)
            .map(|ids| {
                ids.iter()
                    .filter(|id| {
                        !self.replacement.contains_key(id)
                            && reachable.contains(&self.resolve(**id))
                    })
                    .map(|id| (*id, self.all_versions[id].ctx.clone()))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn refresh_reachability(&self) {
        if !self.reachability_dirty.get() {
            return;
        }
        let Some(entry) = self.entry_new else {
            self.reachable_cache.borrow_mut().clear();
            self.reachability_dirty.set(false);
            return;
        };
        let mut successors: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
        for &(source, destination) in &self.edges {
            successors
                .entry(self.resolve(source))
                .or_default()
                .push(self.resolve(destination));
        }

        let mut seen = HashSet::new();
        let mut queue = VecDeque::from([self.resolve(entry)]);
        while let Some(current) = queue.pop_front() {
            if !seen.insert(current) {
                continue;
            }
            if let Some(destinations) = successors.get(&current) {
                queue.extend(destinations.iter().copied());
            }
        }
        *self.reachable_cache.borrow_mut() = seen;
        self.reachability_dirty.set(false);
    }

    fn is_reachable(&self, target: BlockId) -> bool {
        self.refresh_reachability();
        self.reachable_cache
            .borrow()
            .contains(&self.resolve(target))
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
                ctx: ctx.clone(),
                subst: subst.clone(),
            },
        );
        self.versions_of.entry(orig).or_default().push(new);
        self.version_orig.insert(new, orig);
        self.version_by_key.insert((orig, key), new);
        self.limit_checks.insert(orig);
        let task = Task {
            orig,
            new,
            ctx,
            subst,
        };
        self.pending.insert(new, task.clone());
        self.queued.insert(new);
        self.queue.push_back(task);
        new
    }

    /// Returns the specialized block for `orig` under `ctx` (Algorithm 2).
    ///
    /// Matches the paper's `BlockNewVersion`: an unseen context always creates a
    /// fresh version and enqueues it. Version-limit maintenance happens later on
    /// queue pop (Algorithm 1), not here. After a widened merge exists, reuse it
    /// for any covered incoming context so interval loops converge without
    /// recreating precise ranges that the merge already subsumes.
    fn reach(
        &mut self,
        orig: BlockId,
        ctx: TypeContext,
        subst: HashMap<ValueId, ValueId>,
        source: Option<BlockId>,
    ) -> BlockId {
        let source_orig =
            source.and_then(|source| self.version_orig.get(&self.resolve(source)).copied());
        let is_backedge =
            source_orig.is_some_and(|source| self.backedges.contains(&(source, orig)));
        if is_backedge {
            return self.reach_loop_header(orig, ctx, subst, source);
        }
        let is_loop_header = self.backedges.iter().any(|(_, header)| *header == orig);
        if is_loop_header && let Some(entry) = self.loop_entry_versions.get(&orig).copied() {
            return self.reach_loop_entry(orig, entry, ctx, subst, source);
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
                    self.create_version(orig, ctx, key.clone(), subst)
                }
            }
        };
        self.version_by_key.insert((orig, key), id);
        if let Some(source) = source {
            if self.edges.insert((source, id)) {
                self.reachability_dirty.set(true);
            }
            self.limit_checks.insert(orig);
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
        subst: HashMap<ValueId, ValueId>,
        source: Option<BlockId>,
    ) -> BlockId {
        let entry = self.resolve(entry);
        let id = if self.context_covers(orig, &self.all_versions[&entry].ctx, &ctx) {
            entry
        } else {
            let widened = merge::merge_contexts(&self.all_versions[&entry].ctx, &ctx, true);
            let new = self.replace_loop_version(orig, entry, widened, subst);
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
        subst: HashMap<ValueId, ValueId>,
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
                self.widen_recurrent_version(orig, recurrent, ctx, subst)
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
            let id = self.create_version(orig, recurrent_ctx, key, subst);
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
            self.reachability_dirty.set(true);
        }
        self.limit_checks.insert(orig);
    }

    fn same_kind_shape(&self, orig: BlockId, first: &TypeContext, second: &TypeContext) -> bool {
        self.block_params
            .get(&orig)
            .into_iter()
            .flatten()
            .all(|param| first.get(*param).kinds == second.get(*param).kinds)
    }

    fn widen_recurrent_version(
        &mut self,
        orig: BlockId,
        old: BlockId,
        ctx: TypeContext,
        subst: HashMap<ValueId, ValueId>,
    ) -> BlockId {
        let widened = merge::merge_contexts(&self.all_versions[&old].ctx, &ctx, true);
        let new = self.replace_loop_version(orig, old, widened, subst);
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

    fn replace_loop_version(
        &mut self,
        orig: BlockId,
        old: BlockId,
        ctx: TypeContext,
        subst: HashMap<ValueId, ValueId>,
    ) -> BlockId {
        let key = self.ctx_key(orig, &ctx);
        let new = self.create_version(orig, ctx, key, subst);
        self.replacement.insert(old, new);
        self.reachability_dirty.set(true);
        self.pending.remove(&old);
        self.queued.remove(&old);
        self.ensure_reachable_tasks();
        self.reactivate_pending();
        new
    }

    fn reactivate_pending(&mut self) {
        self.refresh_reachability();
        let reachable_cache = self.reachable_cache.borrow();
        let reachable: Vec<_> = self
            .pending
            .iter()
            .filter(|(id, _)| {
                !self.queued.contains(id) && reachable_cache.contains(&self.resolve(**id))
            })
            .map(|(id, task)| (*id, task.clone()))
            .collect();
        drop(reachable_cache);
        for (id, task) in reachable {
            self.limit_checks.insert(task.orig);
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
            None => {
                // `thread_live_ins` normally makes this empty mapping
                // unnecessary, but preserve a materialized edge substitution
                // when the original IR still has a dominated free use.
                let subst = if !self.all_versions[&first].subst.is_empty() {
                    self.all_versions[&first].subst.clone()
                } else {
                    self.all_versions[&second].subst.clone()
                };
                self.create_version(orig, merged_ctx, merged_key, subst)
            }
        };
        self.merge_targets.insert(merged_id);

        if first != merged_id {
            self.replacement.insert(first, merged_id);
            self.reachability_dirty.set(true);
            self.pending.remove(&first);
            self.queued.remove(&first);
        }
        if second != merged_id {
            self.replacement.insert(second, merged_id);
            self.reachability_dirty.set(true);
            self.pending.remove(&second);
            self.queued.remove(&second);
        }
        self.limit_checks.insert(orig);
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
            subst: version.subst.clone(),
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
        self.refresh_reachability();
        let reachable_cache = self.reachable_cache.borrow();
        let missing: Vec<_> = self
            .versions_of
            .iter()
            .flat_map(|(orig, versions)| {
                versions.iter().filter_map(|id| {
                    if self.replacement.contains_key(id)
                        || self.out_blocks.contains_key(id)
                        || self.pending.contains_key(id)
                        || self.queued.contains(id)
                        || !reachable_cache.contains(&self.resolve(*id))
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
                            subst: version.subst.clone(),
                        },
                    ))
                })
            })
            .collect();

        drop(reachable_cache);
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
        let originals: Vec<_> = self.limit_checks.drain().collect();
        for orig in originals {
            let active = self.active_versions(orig);
            if active.len() <= self.version_limit
                || active
                    .iter()
                    .any(|(id, _)| self.pending.contains_key(id) || self.queued.contains(id))
            {
                continue;
            }

            let (id, ctx) = active[0].clone();
            let subst = self.all_versions[&id].subst.clone();
            let task = Task {
                orig,
                new: id,
                ctx,
                subst,
            };
            self.pending.insert(id, task.clone());
            self.queued.insert(id);
            self.queue.push_back(task);
        }
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

        let terminator = self.walk_terminator(&block.terminator, &ctx, &map, &pred_of, task.new);
        let successors = terminator.successors();

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
        for successor in successors {
            if let Some(orig) = self.version_orig.get(&self.resolve(successor)).copied() {
                self.limit_checks.insert(orig);
            }
        }
        self.reactivate_pending();
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
                // Materialized rest is always a proper list.
                ctx.set(
                    *dst,
                    Type {
                        kinds: super::types::KIND_PAIR | super::types::KIND_NULL,
                        fixnum_range: None,
                        length_range: Some(Interval::TOP_LENGTH),
                        singleton: None,
                    },
                );
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
                // Track that `rest` is a length-bearing value (like vector-length).
                let mut rest_ty = ctx.get(rest);
                if rest_ty.length_range.is_none() {
                    rest_ty.length_range = Some(Interval::TOP_LENGTH);
                    ctx.set(rest, rest_ty);
                }
                if let Some(len) = length_singleton(&ctx.get(rest)) {
                    let n = (len - *skip as i64).max(0);
                    ctx.set(*dst, Type::constant(n));
                    out.push(Instruction::Const {
                        dst: new_dst,
                        value: Value::from_i32(n as i32),
                    });
                } else {
                    // Symbolic `[[rest]] - skip`, matching vector-length.
                    ctx.set(
                        *dst,
                        Type::fixnum(
                            Bound::VecLenMinus(rest, *skip as i64),
                            Bound::VecLenMinus(rest, *skip as i64),
                        ),
                    );
                    out.push(Instruction::RestLength {
                        dst: new_dst,
                        rest,
                        skip: *skip,
                        source: *source,
                    });
                }
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
                match fold_rest_predicate(&ctx.get(rest), *predicate, *skip) {
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
                            dst: new_dst,
                            value: Value::from_bool(value),
                        });
                    }
                    None => {
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
        }
    }

    fn walk_terminator(
        &mut self,
        terminator: &Terminator<'gc>,
        ctx: &TypeContext,
        map: &HashMap<ValueId, ValueId>,
        pred_of: &HashMap<ValueId, Predicate>,
        source: BlockId,
    ) -> Terminator<'gc> {
        match terminator {
            Terminator::Jump { target, args } => {
                let successor_ctx = self.successor_ctx(*target, args, ctx);
                let new_target = self.reach(*target, successor_ctx, map.clone(), Some(source));
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
                        if let Some(jump) = self.jump_if_local(consequent, ctx, map, source) {
                            return jump;
                        }
                    }
                    Some(false) => {
                        if let Some(jump) = self.jump_if_local(alternative, ctx, map, source) {
                            return jump;
                        }
                    }
                    None => {}
                }

                let (true_ctx, false_ctx) = narrow_contexts(test, &test_ty, ctx, pred_of);
                let consequent = self.walk_branch_target(consequent, &true_ctx, map, source);
                let alternative = self.walk_branch_target(alternative, &false_ctx, map, source);
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
                            target: self.walk_branch_target(&case.target, &case_ctx, map, source),
                        }
                    })
                    .collect();
                Terminator::Switch {
                    kind: *kind,
                    scrutinee: remap_atom(map, scrutinee),
                    cases: new_cases,
                    default: self.walk_branch_target(default, ctx, map, source),
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
        source: BlockId,
    ) -> Option<Terminator<'gc>> {
        match target {
            BranchTarget::Local { block, args } => {
                let successor_ctx = self.successor_ctx(*block, args, ctx);
                let new_target = self.reach(*block, successor_ctx, map.clone(), Some(source));
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
        source: BlockId,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local { block, args } => {
                let successor_ctx = self.successor_ctx(*block, args, ctx);
                let new_block = self.reach(*block, successor_ctx, map.clone(), Some(source));
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
    use crate::compiler::cps::graph::BranchHint;
    use crate::compiler::ssa::bbv::merge::{
        merge_contexts, select_version_to_merge_with, select_versions_to_merge,
    };
    use crate::compiler::ssa::{CodeId, GraphCodeId, ProcedureKind};
    use crate::runtime::value::Value;
    use std::collections::HashMap;

    #[test]
    fn negated_type_test_narrows_assertion_success_path() {
        let input = ValueId(1);
        let is_fixnum = ValueId(2);
        let assertion_fails = ValueId(3);
        let checked_input = ValueId(4);
        let one = ValueId(5);
        let sum = ValueId(6);
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
                    params: vec![input],
                    variadic: None,
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
                            args: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            args: vec![Operand::Local(input)],
                        },
                        hints: [BranchHint::Cold, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    params: vec![],
                    variadic: None,
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
                    params: vec![checked_input],
                    variadic: None,
                    instructions: vec![
                        Instruction::Const {
                            dst: one,
                            value: Value::from_i32(1),
                        },
                        Instruction::PrimCall {
                            dst: sum,
                            prim: Primitive::Plus,
                            args: vec![Operand::Local(checked_input), Operand::Local(one)],
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
                        prim: Primitive::FxAdd,
                        ..
                    } | Instruction::PrimCall {
                        prim: Primitive::FxAddOvf,
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
                            prim: Primitive::FxAdd,
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
        // Two entry edges into the header (0 and 10) plus a self-backedge that
        // increments: Algorithm 2 may temporarily exceed the limit, then
        // Algorithm 1 + widening must bring the header back within it.
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
                    params: vec![condition],
                    variadic: None,
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
                            args: vec![Operand::Local(zero)],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(1),
                            args: vec![Operand::Local(ten)],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
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
                            prim: Primitive::FxAdd,
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

        let (specialized, annotations) = specialize_procedure(procedure, 2);
        let header_versions = annotations
            .values()
            .filter(|annotation| annotation.orig == BlockId(1))
            .count();
        assert!(
            header_versions <= 2,
            "expected ≤2 header versions, got {header_versions}: {:?}",
            annotations
                .values()
                .filter(|a| a.orig == BlockId(1))
                .map(|a| a.ctx.clone())
                .collect::<Vec<_>>()
        );
        assert!(specialized.blocks.len() <= 5);
    }

    #[test]
    fn polymorphic_loop_keeps_distinct_kind_versions() {
        let condition = ValueId(1);
        let initial = ValueId(2);
        let current = ValueId(3);
        let next_fixnum = ValueId(4);
        let next_char = ValueId(5);
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
                    params: vec![condition],
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
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        test: Operand::Local(condition),
                        consequent: BranchTarget::Local {
                            block: BlockId(2),
                            args: vec![Operand::Local(current)],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(3),
                            args: vec![Operand::Local(current)],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                Block {
                    id: BlockId(2),
                    params: vec![current],
                    variadic: None,
                    instructions: vec![Instruction::Const {
                        dst: next_fixnum,
                        value: Value::from_i32(1),
                    }],
                    terminator: Terminator::Jump {
                        target: BlockId(1),
                        args: vec![Operand::Local(next_fixnum)],
                    },
                    source,
                },
                Block {
                    id: BlockId(3),
                    params: vec![current],
                    variadic: None,
                    instructions: vec![Instruction::Const {
                        dst: next_char,
                        value: Value::from_char('s'),
                    }],
                    terminator: Terminator::Jump {
                        target: BlockId(1),
                        args: vec![Operand::Local(next_char)],
                    },
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
}
