use std::collections::{HashMap, HashSet};

use crate::{
    compiler::{
        cps::{
            graph::{
                BoundVar, ContVar, ExprId, ExprKind, FreeVar, FreeVars, FunctionId, FunctionLinks,
                Graph, Subterm, TermId, TermKind,
            },
            reify::{BinderSet, GraphReifyInfo},
            share::{FunctionShare, MemberShare, SharePlan},
        },
        cranelift::primitive::Primitive,
    },
    expander::core::LVarRef,
    heap::Gc,
    runtime::{
        value::{Symbol, Value},
        vm::exceptions::RaiseKind,
    },
};

use super::{
    Block, BlockId, BranchTarget, ClosureKind, CodeId, GraphCodeId, Instruction, Operand,
    Procedure, ProcedureKind, Program, Terminator, UVar, finish_procedure,
};

pub fn lower_graph<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    share: &SharePlan,
) -> Program<'gc> {
    let primitives = collect_primitive_map(graph);
    let mut procedures = Vec::new();
    // Shared cross-procedure seed table: `Call` sites record the slot types of
    // the reified continuation closures they use as `retk`; reified
    // continuation procedures consume those seeds during their own SBBV
    // specialization (see bbv::RetkSeeds).
    let mut seeds = super::bbv::RetkSeeds::new();

    for function in reify.functions.iter().copied() {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.lower.function");
        let lowered = {
            let _c = crate::utils::pass_profile::ProfileScope::new("cfg.lower.convert");
            lower_function(graph, reify, share, function, &primitives)
        };
        procedures.push(finish_procedure(lowered, &mut seeds));
    }

    // Raw (unspecialized) continuations are kept so a continuation whose seed
    // arrives late can be re-finished. `reify.continuations` is in
    // outer-before-inner binding order, so a nested continuation's seed is
    // normally recorded before the nested continuation is finished; the
    // re-finish loop below is a cheap safety net for any ordering surprise.
    let mut raw_continuations: Vec<Procedure<'gc>> = Vec::new();
    let mut finished_versions: Vec<u64> = Vec::new();
    let mut continuation_proc_index: Vec<usize> = Vec::new();
    for continuation in reify.continuations.iter().copied() {
        if graph[continuation].is_reified {
            let _p = crate::utils::pass_profile::ProfileScope::new("cfg.lower.continuation");
            let lowered = {
                let _c = crate::utils::pass_profile::ProfileScope::new("cfg.lower.convert");
                lower_continuation(graph, reify, share, continuation, &primitives)
            };
            raw_continuations.push(lowered.clone());
            finished_versions.push(seeds.version(lowered.code));
            continuation_proc_index.push(procedures.len());
            procedures.push(finish_procedure(lowered, &mut seeds));
        }
    }

    for _round in 0..3 {
        let redo: Vec<usize> = (0..raw_continuations.len())
            .filter(|&index| seeds.version(raw_continuations[index].code) > finished_versions[index])
            .collect();
        if redo.is_empty() {
            break;
        }
        for index in redo {
            procedures[continuation_proc_index[index]] =
                finish_procedure(raw_continuations[index].clone(), &mut seeds);
            finished_versions[index] = seeds.version(raw_continuations[index].code);
        }
    }

    Program {
        entry: graph_code_id(graph, reify.entrypoint),
        procedures,
    }
}

fn collect_primitive_map<'gc>(graph: &Graph<'gc>) -> HashMap<Value<'gc>, Primitive> {
    let mut map = HashMap::new();
    for (_, expr) in graph.exprs_iter() {
        if let ExprKind::PrimCall(prim, _) = expr.kind {
            map.entry(prim).or_insert_with(|| {
                let name = prim.downcast::<Symbol>().to_string();
                Primitive::from_name(&name).unwrap_or_else(|| panic!("undefined primitive: {prim}"))
            });
        }
    }
    map
}

fn lower_function<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    share: &SharePlan,
    function: FunctionId,
    primitives: &HashMap<Value<'gc>, Primitive>,
) -> Procedure<'gc> {
    let data = graph[function];
    let return_cont = data
        .cont
        .expect("graph function should have a return continuation");
    let source_free_vars = binder_set_to_vec(reify.free_vars.function(function));
    let mut builder = ProcedureBuilder::new(graph, reify, share, primitives);
    let binding = builder.uvar(data.var);
    let return_cont = builder.uvar(return_cont);
    let params = builder.uvars(graph.bound_vars_slice(&data.vars));
    let variadic = data.variadic.map(|var| builder.uvar(var));
    let share_info = share.function_share(function).cloned();
    let (instructions, free_vars, env_shared) = if let Some(fs) = &share_info {
        closure_refs_shared(&mut builder, graph, function, binding, fs)
    } else {
        let (instructions, free_vars) =
            closure_refs(&mut builder, graph, function, binding, &source_free_vars);
        (instructions, free_vars, false)
    };
    let entry = BlockId(0);
    builder.convert_block(entry, instructions, data.body);
    let (blocks, sources) = builder.finish();

    Procedure {
        code: graph_code_id(graph, function),
        kind: ProcedureKind::Function,
        binding,
        name: data.name,
        source: data.source,
        meta: data.meta,
        return_cont: Some(return_cont),
        params,
        variadic,
        free_vars,
        env_shared,
        entry,
        sources,
        blocks,
    }
}

fn lower_continuation<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    share: &SharePlan,
    continuation: FunctionId,
    primitives: &HashMap<Value<'gc>, Primitive>,
) -> Procedure<'gc> {
    let data = graph[continuation];
    assert!(
        data.cont.is_none(),
        "graph continuation should not have a return continuation"
    );
    let source_free_vars = binder_set_to_vec(reify.free_vars.continuation(continuation));
    let mut builder = ProcedureBuilder::new(graph, reify, share, primitives);
    let binding = builder.uvar(data.var);
    let params = builder.uvars(graph.bound_vars_slice(&data.vars));
    let variadic = data.variadic.map(|var| builder.uvar(var));
    let entry = BlockId(0);
    let share_info = share.function_share(continuation).cloned();
    let (instructions, free_vars, env_shared) = if let Some(fs) = &share_info {
        closure_refs_shared(&mut builder, graph, continuation, binding, fs)
    } else {
        let (instructions, free_vars) =
            closure_refs(&mut builder, graph, continuation, binding, &source_free_vars);
        (instructions, free_vars, false)
    };
    builder.convert_block(entry, instructions, data.body);
    let (blocks, sources) = builder.finish();

    Procedure {
        code: graph_code_id(graph, continuation),
        kind: ProcedureKind::Continuation,
        binding,
        name: data.name,
        source: data.source,
        meta: data.meta,
        return_cont: None,
        params,
        variadic,
        free_vars,
        env_shared,
        entry,
        sources,
        blocks,
    }
}

fn graph_code_id(graph: &Graph<'_>, function: FunctionId) -> CodeId {
    let id = GraphCodeId(function.as_u32());
    if graph[function].cont.is_some() {
        CodeId::GraphFunction(id)
    } else {
        CodeId::GraphContinuation(id)
    }
}

/// Closure kind of a graph function: a reified continuation creates a
/// continuation closure (header tag), everything else a function closure.
fn closure_kind(graph: &Graph<'_>, function: FunctionId) -> ClosureKind {
    if graph[function].cont.is_some() {
        ClosureKind::Function
    } else {
        ClosureKind::Continuation
    }
}

/// Free-variable set of a function or reified continuation.
fn free_vars_of<'a>(
    graph: &Graph<'_>,
    reify: &'a GraphReifyInfo,
    function: FunctionId,
) -> &'a BinderSet {
    if graph[function].cont.is_some() {
        reify.free_vars.function(function)
    } else {
        reify.free_vars.continuation(function)
    }
}

fn binder_set_to_vec(vars: &BinderSet) -> Vec<BoundVar> {
    vars.iter().collect()
}

/// Whether `var` is the function's own binding (the closure's self reference).
/// The self capture needs no physical slot: codegen's fast path binds `rator`.
fn is_self_var<'gc>(graph: &Graph<'gc>, function: FunctionId, var: BoundVar) -> bool {
    Gc::ptr_eq(graph[var].var, graph[graph[function].var].var)
}

/// Materialize a flat closure's captures at function entry. The self reference
/// is materialized last through a dummy slot index (codegen binds `rator`, so
/// no slot). Returns the entry instructions and physical `free_vars` order.
fn closure_refs<'gc>(
    builder: &mut ProcedureBuilder<'_, 'gc>,
    graph: &Graph<'gc>,
    function: FunctionId,
    binding: UVar,
    free_vars: &[BoundVar],
) -> (Vec<Instruction<'gc>>, Vec<UVar>) {
    let mut instructions = Vec::new();
    let mut physical = Vec::new();
    let mut self_uvar = None;
    for free_var in free_vars.iter().copied() {
        let uvar = builder.uvar(free_var);
        if is_self_var(graph, function, free_var) {
            // Self references are materialized last, after the physical
            // slots, through the dummy index.
            self_uvar = Some(uvar);
            continue;
        }
        instructions.push(Instruction::ClosureRef {
            dst: uvar,
            closure: Operand::Local(binding),
            index: physical.len(),
        });
        physical.push(uvar);
    }
    if let Some(self_uvar) = self_uvar {
        instructions.push(Instruction::ClosureRef {
            dst: self_uvar,
            closure: Operand::Local(binding),
            index: physical.len(),
        });
        physical.push(self_uvar);
    }
    (instructions, physical)
}

/// Materialize a shared closure's captures at function entry: slot 0 is the
/// `EnvRecord` pointer, slots 1.. are private captures, record-resident vars
/// are read from the record. Self reference goes last via a dummy index.
/// Returns the entry instructions and physical `free_vars` (`[env, own...,
/// self?]`), which `Procedure::free_vars` must reflect for index-accurate
/// codegen and GC rooting.
fn closure_refs_shared<'gc>(
    builder: &mut ProcedureBuilder<'_, 'gc>,
    graph: &Graph<'gc>,
    function: FunctionId,
    binding: UVar,
    share: &FunctionShare,
) -> (Vec<Instruction<'gc>>, Vec<UVar>, bool) {
    let env_uvar = builder.fresh_temp();
    builder.env_uvar = Some(env_uvar);
    let mut instructions = vec![Instruction::ClosureRef {
        dst: env_uvar,
        closure: Operand::Local(binding),
        index: 0,
    }];
    let mut own_uvars = Vec::new();
    let mut self_uvar = None;
    for free_var in share.own.iter().copied() {
        let uvar = builder.uvar(free_var);
        if is_self_var(graph, function, free_var) {
            self_uvar = Some(uvar);
            continue;
        }
        instructions.push(Instruction::ClosureRef {
            dst: uvar,
            closure: Operand::Local(binding),
            index: own_uvars.len() + 1,
        });
        own_uvars.push(uvar);
    }
    for (free_var, slot) in share.record.iter() {
        instructions.push(Instruction::EnvRef {
            dst: builder.uvar(*free_var),
            env: Operand::Local(env_uvar),
            index: *slot,
        });
    }
    let mut free_vars = vec![env_uvar];
    free_vars.extend(own_uvars);
    if let Some(self_uvar) = self_uvar {
        // The dummy index must never collide with the env slot (index 0).
        instructions.push(Instruction::ClosureRef {
            dst: self_uvar,
            closure: Operand::Local(binding),
            index: free_vars.len(),
        });
        free_vars.push(self_uvar);
    }
    (instructions, free_vars, true)
}

struct ProcedureBuilder<'a, 'gc> {
    graph: &'a Graph<'gc>,
    reify: &'a GraphReifyInfo,
    share: &'a SharePlan,
    primitives: &'a HashMap<Value<'gc>, Primitive>,
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<BoundVar, BlockId>,
    /// Contified continuation formals (and optional rest), for Assign-on-continue.
    local_formals: HashMap<BoundVar, (Vec<UVar>, Option<UVar>)>,
    values: HashMap<BoundVar, UVar>,
    known_literals: HashMap<BoundVar, Value<'gc>>,
    sources: HashMap<UVar, LVarRef<'gc>>,
    /// This procedure's shared `EnvRecord` (slot 0 of its closure); reuse
    /// sites inside its body point their member closures at it.
    env_uvar: Option<UVar>,
    next_uvar: u32,
    next_block: usize,
}

impl<'a, 'gc> ProcedureBuilder<'a, 'gc> {
    fn new(
        graph: &'a Graph<'gc>,
        reify: &'a GraphReifyInfo,
        share: &'a SharePlan,
        primitives: &'a HashMap<Value<'gc>, Primitive>,
    ) -> Self {
        Self {
            graph,
            reify,
            share,
            primitives,
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            local_formals: HashMap::new(),
            values: HashMap::new(),
            known_literals: HashMap::new(),
            sources: HashMap::new(),
            env_uvar: None,
            next_uvar: 0,
            next_block: 1,
        }
    }

    fn finish(mut self) -> (Vec<Block<'gc>>, HashMap<UVar, LVarRef<'gc>>) {
        self.blocks.sort_by_key(|block| block.id.0);
        (self.blocks, self.sources)
    }

    fn uvar(&mut self, var: BoundVar) -> UVar {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = UVar(self.next_uvar);
        self.next_uvar += 1;
        self.values.insert(var, id);
        self.sources.insert(id, self.graph[var].var);
        id
    }

    fn fresh_temp(&mut self) -> UVar {
        let id = UVar(self.next_uvar);
        self.next_uvar += 1;
        id
    }

    fn uvars(&mut self, vars: &[BoundVar]) -> Vec<UVar> {
        vars.iter().copied().map(|var| self.uvar(var)).collect()
    }

    fn atom(&mut self, var: FreeVar) -> Operand<'gc> {
        Operand::Local(self.uvar(self.graph.free_binder(var)))
    }

    fn atoms(&mut self, vars: &FreeVars) -> Vec<Operand<'gc>> {
        self.graph
            .free_vars_slice(vars)
            .iter()
            .copied()
            .map(|var| self.atom(var))
            .collect()
    }

    fn literal_atom(&self, var: FreeVar) -> Option<Operand<'gc>> {
        let binder = self.graph.free_binder(var);
        self.known_literals
            .get(&binder)
            .copied()
            .map(Operand::Constant)
    }

    fn atoms_for_prim(&mut self, prim: Primitive, vars: &FreeVars) -> Vec<Operand<'gc>> {
        let mut args = self.atoms(vars);
        if matches!(prim, Primitive::CacheRef | Primitive::CacheSet)
            && let Some(first) = self.graph.free_vars_slice(vars).first().copied()
            && let Some(literal) = self.literal_atom(first)
        {
            args[0] = literal;
        }
        args
    }

    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    fn convert_block(
        &mut self,
        id: BlockId,
        mut instructions: Vec<Instruction<'gc>>,
        link: Subterm,
    ) {
        let term = self
            .graph
            .read_term_link(link)
            .unwrap_or_else(|| panic!("dead graph term link while lowering to CFG: {link}"));
        let source = self.graph[term].source;
        let terminator = self.convert_term(term, &mut instructions);
        self.blocks.push(Block {
            id,
            instructions,
            terminator,
            source,
        });
    }

    fn convert_term(
        &mut self,
        term: TermId,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let data = self.graph[term];
        match data.kind {
            TermKind::LetVal((var, expr), body) => {
                let expr = self
                    .graph
                    .read_expr_link(expr)
                    .unwrap_or_else(|| panic!("dead graph expression link while lowering to CFG"));
                self.convert_expr(var, expr, instructions);
                self.convert_term_link(body, instructions)
            }

            TermKind::Fix(functions, body) => {
                let functions = live_functions(self.graph, &functions);
                if let Some(site) = self.share.site(term) {
                    let env_uvar = if site.allocate {
                        // Allocate a fresh shared record holding the common
                        // free variables.
                        let env_uvar = self.fresh_temp();
                        instructions.push(Instruction::MakeEnv {
                            dst: env_uvar,
                            size: site.record_vars.len(),
                        });
                        for (index, record_var) in site.record_vars.iter().enumerate() {
                            instructions.push(Instruction::EnvSet {
                                env: Operand::Local(env_uvar),
                                index,
                                value: Operand::Local(self.uvar(*record_var)),
                            });
                        }
                        env_uvar
                    } else {
                        // Reuse: member closures point at this function's own
                        // shared record.
                        self.env_uvar
                            .expect("reuse site requires an enclosing shared env")
                    };
                    for member in site.members.iter() {
                        let own = physical_own_vars(self.graph, member);
                        if member.flat {
                            // Compacted member: flat closure, full captures,
                            // no env record (hot functions).
                            instructions.push(Instruction::MakeClosure {
                                dst: self.uvar(self.graph[member.function].var),
                                code: graph_code_id(self.graph, member.function),
                                kind: closure_kind(self.graph, member.function),
                                free_count: own.len(),
                                env: None,
                            });
                        } else {
                            instructions.push(Instruction::MakeClosure {
                                dst: self.uvar(self.graph[member.function].var),
                                code: graph_code_id(self.graph, member.function),
                                kind: closure_kind(self.graph, member.function),
                                free_count: 1 + own.len(),
                                env: Some(Operand::Local(env_uvar)),
                            });
                        }
                    }
                    for member in site.members.iter() {
                        let closure = self.uvar(self.graph[member.function].var);
                        let own = physical_own_vars(self.graph, member);
                        if member.flat {
                            for (index, own_var) in own.iter().enumerate() {
                                instructions.push(Instruction::ClosureSet {
                                    closure: Operand::Local(closure),
                                    index,
                                    value: Operand::Local(self.uvar(*own_var)),
                                });
                            }
                        } else {
                            for (index, own_var) in own.iter().enumerate() {
                                instructions.push(Instruction::ClosureSet {
                                    closure: Operand::Local(closure),
                                    index: index + 1,
                                    value: Operand::Local(self.uvar(*own_var)),
                                });
                            }
                        }
                    }
                } else {
                    for function in functions.iter().copied() {
                        let free_vars = binder_set_to_vec(free_vars_of(self.graph, self.reify, function));
                        let physical = physical_free_vars(self.graph, function, &free_vars);
                        instructions.push(Instruction::MakeClosure {
                            dst: self.uvar(self.graph[function].var),
                            code: graph_code_id(self.graph, function),
                            kind: closure_kind(self.graph, function),
                            free_count: physical.len(),
                            env: None,
                        });
                    }

                    for function in functions {
                        let closure = self.uvar(self.graph[function].var);
                        let free_vars = binder_set_to_vec(free_vars_of(self.graph, self.reify, function));
                        let physical = physical_free_vars(self.graph, function, &free_vars);
                        emit_closure_sets(self, instructions, closure, &physical);
                    }
                }

                self.convert_term_link(body, instructions)
            }

            TermKind::Letk(continuations, body) => {
                let continuations = live_functions(self.graph, &continuations);
                let (reified_conts, local_conts): (Vec<_>, Vec<_>) = continuations
                    .into_iter()
                    .partition(|continuation| self.graph[*continuation].is_reified);

                if let Some(site) = self.share.site(term) {
                    // Shared reified-continuation site: one record, member
                    // closures point at it, private captures in slots 1..
                    let env_uvar = if site.allocate {
                        let env_uvar = self.fresh_temp();
                        instructions.push(Instruction::MakeEnv {
                            dst: env_uvar,
                            size: site.record_vars.len(),
                        });
                        for (index, record_var) in site.record_vars.iter().enumerate() {
                            instructions.push(Instruction::EnvSet {
                                env: Operand::Local(env_uvar),
                                index,
                                value: Operand::Local(self.uvar(*record_var)),
                            });
                        }
                        env_uvar
                    } else {
                        self.env_uvar
                            .expect("reuse site requires an enclosing shared env")
                    };
                    for member in site.members.iter() {
                        let own = physical_own_vars(self.graph, member);
                        instructions.push(Instruction::MakeClosure {
                            dst: self.uvar(self.graph[member.function].var),
                            code: graph_code_id(self.graph, member.function),
                            kind: ClosureKind::Continuation,
                            free_count: if member.flat { own.len() } else { 1 + own.len() },
                            env: if member.flat {
                                None
                            } else {
                                Some(Operand::Local(env_uvar))
                            },
                        });
                    }
                    for member in site.members.iter() {
                        let closure = self.uvar(self.graph[member.function].var);
                        let own = physical_own_vars(self.graph, member);
                        let base = if member.flat { 0 } else { 1 };
                        for (index, own_var) in own.iter().enumerate() {
                            instructions.push(Instruction::ClosureSet {
                                closure: Operand::Local(closure),
                                index: index + base,
                                value: Operand::Local(self.uvar(*own_var)),
                            });
                        }
                    }
                } else {
                    for continuation in reified_conts.iter().copied() {
                        let free_vars =
                            binder_set_to_vec(self.reify.free_vars.continuation(continuation));
                        let physical = physical_free_vars(self.graph, continuation, &free_vars);
                        instructions.push(Instruction::MakeClosure {
                            dst: self.uvar(self.graph[continuation].var),
                            code: graph_code_id(self.graph, continuation),
                            kind: ClosureKind::Continuation,
                            free_count: physical.len(),
                            env: None,
                        });
                    }

                    for continuation in reified_conts {
                        let closure = self.uvar(self.graph[continuation].var);
                        let free_vars =
                            binder_set_to_vec(self.reify.free_vars.continuation(continuation));
                        let physical = physical_free_vars(self.graph, continuation, &free_vars);
                        emit_closure_sets(self, instructions, closure, &physical);
                    }
                }

                for continuation in &local_conts {
                    let id = self.alloc_block();
                    let data = self.graph[*continuation];
                    self.local_blocks.insert(data.var, id);
                    let formals = self.uvars(self.graph.bound_vars_slice(&data.vars));
                    let variadic = data.variadic.map(|var| self.uvar(var));
                    self.local_formals.insert(data.var, (formals, variadic));
                }

                for continuation in local_conts {
                    let data = self.graph[continuation];
                    let id = self.local_blocks[&data.var];
                    self.convert_block(id, vec![], data.body);
                }

                self.convert_term_link(body, instructions)
            }

            TermKind::Continue(cont, args) => {
                let target = self.graph.free_binder(cont);
                if let Some(block) = self.local_blocks.get(&target).copied() {
                    let args = self.atoms(&args);
                    let (formals, variadic) = self
                        .local_formals
                        .get(&target)
                        .cloned()
                        .unwrap_or_else(|| (vec![], None));
                    if !contified_arity_ok(&formals, variadic, &args) {
                        // Contified receives must raise &assertion on value
                        // arity mismatch (e.g. (let-values ((() (values 1))) …)).
                        Terminator::Raise {
                            kind: RaiseKind::AssertionViolation,
                            args: vec![],
                            source: data.source,
                        }
                    } else {
                        emit_parallel_assign(self, instructions, &formals, variadic, &args);
                        Terminator::Jump { target: block }
                    }
                } else {
                    Terminator::TailCall {
                        callee: Operand::Local(self.uvar(target)),
                        args: self.atoms(&args),
                        source: data.source,
                    }
                }
            }

            TermKind::App(callee, args, retk) => Terminator::Call {
                callee: self.atom(callee),
                retk: self.atom(retk),
                args: self.atoms(&args),
                source: data.source,
            },

            TermKind::Raise(kind, args) => Terminator::Raise {
                kind,
                args: self.atoms(&args),
                source: data.source,
            },

            TermKind::If(test, consequent, alternative, hints) => Terminator::Branch {
                test: self.atom(test),
                consequent: self.branch_target(consequent),
                alternative: self.branch_target(alternative),
                hints: [hints[0], hints[1]],
            },
        }
    }

    fn convert_term_link(
        &mut self,
        link: Subterm,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let term = self
            .graph
            .read_term_link(link)
            .unwrap_or_else(|| panic!("dead graph term link while lowering to CFG: {link}"));
        self.convert_term(term, instructions)
    }

    fn convert_expr(
        &mut self,
        binding: BoundVar,
        expr: ExprId,
        instructions: &mut Vec<Instruction<'gc>>,
    ) {
        match self.graph[expr].kind {
            ExprKind::Literal(value) => {
                let dst = self.uvar(binding);
                self.known_literals.insert(binding, value);
                instructions.push(Instruction::Const { dst, value });
            }
            ExprKind::PrimCall(prim, args) => {
                let prim = *self
                    .primitives
                    .get(&prim)
                    .unwrap_or_else(|| panic!("undefined primitive: {prim}"));
                let args = self.atoms_for_prim(prim, &args);
                let dst = self.uvar(binding);
                instructions.push(Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source: self.graph[expr].source,
                });
            }
        }
    }

    fn branch_target(&mut self, link: Subterm) -> BranchTarget<'gc> {
        if let Some((continuation, args)) = self.direct_continue(link) {
            return self.continuation_target(continuation, args);
        }

        let block = self.alloc_block();
        self.convert_block(block, vec![], link);
        BranchTarget::Local {
            block,
            edge_assigns: vec![],
        }
    }

    fn direct_continue(&self, link: Subterm) -> Option<(ContVar, FreeVars)> {
        let term = self.graph.read_term_link(link)?;
        let TermKind::Continue(continuation, args) = self.graph[term].kind else {
            return None;
        };
        Some((continuation, args))
    }

    fn continuation_target(&mut self, continuation: ContVar, args: FreeVars) -> BranchTarget<'gc> {
        let continuation = self.graph.free_binder(continuation);
        let args = self.atoms(&args);
        if let Some(block) = self.local_blocks.get(&continuation).copied() {
            // Push Assigns into a helper block, then jump to the cont.
            let helper = self.alloc_block();
            let (formals, variadic) = self
                .local_formals
                .get(&continuation)
                .cloned()
                .unwrap_or_else(|| (vec![], None));
            let source = Value::new(false);
            let (instructions, terminator) = if !contified_arity_ok(&formals, variadic, &args) {
                (
                    Vec::new(),
                    Terminator::Raise {
                        kind: RaiseKind::AssertionViolation,
                        args: vec![],
                        source,
                    },
                )
            } else {
                let mut instructions = Vec::new();
                emit_parallel_assign(self, &mut instructions, &formals, variadic, &args);
                (instructions, Terminator::Jump { target: block })
            };
            self.blocks.push(Block {
                id: helper,
                instructions,
                terminator,
                source,
            });
            BranchTarget::Local {
                block: helper,
                edge_assigns: vec![],
            }
        } else {
            BranchTarget::Reified {
                continuation: Operand::Local(self.uvar(continuation)),
                args,
            }
        }
    }
}

fn contified_arity_ok<'gc>(
    formals: &[UVar],
    variadic: Option<UVar>,
    args: &[Operand<'gc>],
) -> bool {
    if variadic.is_some() {
        args.len() >= formals.len()
    } else {
        args.len() == formals.len()
    }
}

fn emit_parallel_assign<'gc>(
    builder: &mut ProcedureBuilder<'_, 'gc>,
    instructions: &mut Vec<Instruction<'gc>>,
    formals: &[UVar],
    variadic: Option<UVar>,
    args: &[Operand<'gc>],
) {
    debug_assert!(
        contified_arity_ok(formals, variadic, args),
        "emit_parallel_assign requires a matching contified arity"
    );
    let fixed_count = formals.len();
    let fixed_args = &args[..fixed_count.min(args.len())];
    emit_parallel_moves(builder, instructions, formals, fixed_args);

    let Some(rest) = variadic else {
        return;
    };

    // Right-fold cons over the excess arguments into `rest`.
    let mut list = Operand::Constant(Value::null());
    for arg in args.get(fixed_count..).unwrap_or(&[]).iter().rev().copied() {
        let dst = builder.fresh_temp();
        instructions.push(Instruction::PrimCall {
            dst,
            prim: Primitive::Cons,
            args: vec![arg, list],
            source: Value::new(false),
        });
        list = Operand::Local(dst);
    }
    instructions.push(Instruction::Assign {
        dst: rest,
        src: list,
    });
}

/// Parallel `destinations ← sources` via temp staging (handles cycles).
fn emit_parallel_moves<'gc>(
    builder: &mut ProcedureBuilder<'_, 'gc>,
    instructions: &mut Vec<Instruction<'gc>>,
    destinations: &[UVar],
    sources: &[Operand<'gc>],
) {
    if destinations.is_empty() {
        return;
    }

    let mut srcs = sources.to_vec();
    while srcs.len() < destinations.len() {
        srcs.push(Operand::Constant(Value::null()));
    }
    if srcs.len() > destinations.len() {
        srcs.truncate(destinations.len());
    }

    let mut temps = Vec::with_capacity(destinations.len());
    for src in &srcs {
        match *src {
            Operand::Local(var) => temps.push(var),
            Operand::Constant(_) => {
                let temp = builder.fresh_temp();
                instructions.push(Instruction::Assign {
                    dst: temp,
                    src: *src,
                });
                temps.push(temp);
            }
        }
    }

    let dest_set: HashSet<UVar> = destinations.iter().copied().collect();
    let mut staged = temps;
    for (index, src) in srcs.iter().enumerate() {
        if let Operand::Local(var) = *src
            && dest_set.contains(&var)
        {
            let temp = builder.fresh_temp();
            instructions.push(Instruction::Assign {
                dst: temp,
                src: Operand::Local(var),
            });
            staged[index] = temp;
        }
    }

    for (dst, src) in destinations.iter().copied().zip(staged) {
        if dst != src {
            instructions.push(Instruction::Assign {
                dst,
                src: Operand::Local(src),
            });
        }
    }
}

fn live_functions<'gc>(graph: &Graph<'gc>, functions: &FunctionLinks) -> Vec<FunctionId> {
    graph
        .function_links_slice(functions)
        .iter()
        .copied()
        .filter_map(|link| graph.read_function_link(link))
        .collect()
}

fn emit_closure_sets<'gc>(
    builder: &mut ProcedureBuilder<'_, 'gc>,
    instructions: &mut Vec<Instruction<'gc>>,
    closure: UVar,
    free_vars: &[BoundVar],
) {
    // `free_vars` is physical (self reference removed); the self capture needs
    // no slot because codegen's fast path binds `rator`.
    for (index, free_var) in free_vars.iter().enumerate() {
        instructions.push(Instruction::ClosureSet {
            closure: Operand::Local(closure),
            index,
            value: Operand::Local(builder.uvar(*free_var)),
        });
    }
}

/// Physical free-var list for a flat closure: the free variables minus the
/// self reference (which needs no physical slot).
fn physical_free_vars<'gc>(
    graph: &Graph<'gc>,
    function: FunctionId,
    vars: &[BoundVar],
) -> Vec<BoundVar> {
    vars.iter()
        .copied()
        .filter(|var| !is_self_var(graph, function, *var))
        .collect()
}

/// Physical private-capture list for a member at a shared site: its own
/// captures minus the self reference.
fn physical_own_vars<'gc>(graph: &Graph<'gc>, member: &MemberShare) -> Vec<BoundVar> {
    member
        .own_vars
        .iter()
        .copied()
        .filter(|v| !is_self_var(graph, member.function, *v))
        .collect()
}
