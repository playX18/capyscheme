use std::collections::HashMap;

use crate::{
    compiler::cranelift::primitive::Primitive,
    cps::linear::{
        Block, BlockId, BranchTarget, ClosureKind, CodeId, GraphCodeId, Instruction, LinearAtom,
        LinearProgram, Procedure, ProcedureKind, Terminator, ValueId, finish_procedure,
    },
    expander::core::LVarRef,
    runtime::value::Value,
};

use super::{
    graph::{
        BoundVar, ContVar, ExprId, ExprKind, FreeVar, FreeVars, FunctionId, FunctionLinks, Graph,
        Subterm, TermId, TermKind,
    },
    reify::{BinderSet, GraphReifyInfo},
};

pub fn linearize_graph<'gc>(graph: &Graph<'gc>, reify: &GraphReifyInfo) -> LinearProgram<'gc> {
    let mut procedures = Vec::new();

    for function in reify.functions.iter().copied() {
        procedures.push(finish_procedure(linearize_function(graph, reify, function)));
    }

    for continuation in reify.continuations.iter().copied() {
        if graph[continuation].is_reified {
            procedures.push(finish_procedure(linearize_continuation(
                graph,
                reify,
                continuation,
            )));
        }
    }

    LinearProgram {
        entry: graph_code_id(graph, reify.entrypoint),
        procedures,
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use crate::{
        cps::{
            linear::{BranchTarget, ClosureKind, CodeId, Instruction, LinearProgram, Terminator},
            term::{Atom, BranchHint, Cont, Expression, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        compiler::cps::{
            convert::cps_func_to_graph,
            graph::{FunctionId, Graph},
            reify::{GraphReifyInfo, reify_graph},
        },
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value, init_symbols},
        },
    };

    use super::linearize_graph;

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            init_symbols(*ctx);
            f(ctx)
        });
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    fn find_function<'gc>(
        graph: &Graph<'gc>,
        functions: &[FunctionId],
        binding: LVarRef<'gc>,
    ) -> FunctionId {
        functions
            .iter()
            .copied()
            .find(|function| graph[graph[*function].var].var == binding)
            .expect("graph function should be present")
    }

    fn procedure<'a, 'gc>(
        program: &'a LinearProgram<'gc>,
        code: CodeId<'gc>,
    ) -> &'a crate::cps::linear::Procedure<'gc> {
        program
            .procedures
            .iter()
            .find(|procedure| procedure.code == code)
            .expect("linear procedure should be present")
    }

    fn graph_program<'gc>(
        ctx: Context<'gc>,
        func: Gc<'gc, Func<'gc>>,
    ) -> (Graph<'gc>, GraphReifyInfo, LinearProgram<'gc>) {
        let mut program = cps_func_to_graph(ctx, func).expect("function graph conversion");
        let reify = reify_graph(&mut program.graph, program.entry);
        let linear = linearize_graph(&program.graph, &reify);
        (program.graph, reify, linear)
    }

    #[test]
    fn local_continuation_continue_lowers_to_jump() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let x = lvar(ctx, "x");
            let k = lvar(ctx, "k");
            let source = Value::new(false);

            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    ),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let (_graph, _reify, linear) = graph_program(ctx, func);
            let entry = procedure(&linear, linear.entry);

            let Terminator::Jump { target, args } = &entry.blocks[0].terminator else {
                panic!("local continuation should lower to a jump");
            };

            assert_eq!(args.len(), 1);
            assert!(entry.blocks.iter().any(|block| block.id == *target));
        });
    }

    #[test]
    fn reified_continuation_continue_lowers_to_tail_call() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let g = lvar(ctx, "g");
            let g_ret = lvar(ctx, "g-ret");
            let source = Value::new(false);

            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, []), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let nested = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: g,
                    return_cont: g_ret,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, []), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Fix(
                            Array::from_slice(*ctx, &[nested]),
                            Gc::new(*ctx, Term::Continue(k, Array::from_slice(*ctx, []), source)),
                        ),
                    ),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let (graph, reify, linear) = graph_program(ctx, func);
            let entry = procedure(&linear, linear.entry);
            let graph_k = find_function(&graph, &reify.continuations, k);
            let k_code =
                CodeId::GraphContinuation(crate::cps::linear::GraphCodeId(graph_k.as_u32()));

            assert!(graph[graph_k].is_reified);
            assert!(entry.blocks[0].instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instruction::MakeClosure {
                        code,
                        kind: ClosureKind::Continuation,
                        ..
                    } if *code == k_code
                )
            }));
            assert!(matches!(
                entry.blocks[0].terminator,
                Terminator::TailCall { .. }
            ));
            assert!(
                linear
                    .procedures
                    .iter()
                    .any(|procedure| procedure.code == k_code)
            );
        });
    }

    #[test]
    fn branch_targets_preserve_local_and_reified_destinations() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let test = lvar(ctx, "test");
            let x = lvar(ctx, "x");
            let k = lvar(ctx, "k");
            let source = Value::new(false);

            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(test),
                            consequent: k,
                            consequent_args: None,
                            alternative: ret,
                            alternative_args: Some(Array::from_slice(*ctx, &[Atom::Local(x)])),
                            hints: [BranchHint::Hot, BranchHint::Cold],
                        },
                    ),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[test, x]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let (_graph, _reify, linear) = graph_program(ctx, func);
            let entry = procedure(&linear, linear.entry);

            let Terminator::Branch {
                consequent,
                alternative,
                hints,
                ..
            } = &entry.blocks[0].terminator
            else {
                panic!("if term should lower to a branch");
            };

            assert_eq!(*hints, [BranchHint::Hot, BranchHint::Cold]);
            assert!(matches!(consequent, BranchTarget::Local { .. }));
            assert!(matches!(alternative, BranchTarget::Reified { .. }));
        });
    }

    #[test]
    fn cache_ref_literal_key_lowers_to_constant_cache_key() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let key_var = lvar(ctx, "key");
            let cached = lvar(ctx, "cached");
            let key = Value::new(42);
            let source = Value::new(false);
            let cache_ref = Symbol::from_str(ctx, "cache-ref").into();
            let body = Gc::new(
                *ctx,
                Term::Let(
                    key_var,
                    Expression::Literal(key, source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            cached,
                            Expression::PrimCall(
                                cache_ref,
                                Array::from_slice(*ctx, &[Atom::Local(key_var)]),
                                source,
                            ),
                            Gc::new(
                                *ctx,
                                Term::Continue(
                                    ret,
                                    Array::from_slice(*ctx, &[Atom::Local(cached)]),
                                    source,
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let (_graph, _reify, linear) = graph_program(ctx, func);
            let entry = procedure(&linear, linear.entry);

            assert!(entry.blocks.iter().any(|block| {
                block.instructions.iter().any(|instruction| {
                    matches!(
                        instruction,
                        Instruction::CacheRef {
                            cache_key: crate::cps::linear::LinearAtom::Constant(value),
                            ..
                        } if *value == key
                    )
                })
            }));
        });
    }

    #[test]
    fn cache_set_literal_key_lowers_to_constant_cache_key() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let key_var = lvar(ctx, "key");
            let value_var = lvar(ctx, "value");
            let cached = lvar(ctx, "cached");
            let key = Value::new(42);
            let value = Value::new(7);
            let source = Value::new(false);
            let cache_set = Symbol::from_str(ctx, "cache-set!").into();
            let body = Gc::new(
                *ctx,
                Term::Let(
                    key_var,
                    Expression::Literal(key, source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            value_var,
                            Expression::Literal(value, source),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    cached,
                                    Expression::PrimCall(
                                        cache_set,
                                        Array::from_slice(
                                            *ctx,
                                            &[Atom::Local(key_var), Atom::Local(value_var)],
                                        ),
                                        source,
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Continue(
                                            ret,
                                            Array::from_slice(*ctx, &[Atom::Local(cached)]),
                                            source,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let (_graph, _reify, linear) = graph_program(ctx, func);
            let entry = procedure(&linear, linear.entry);

            assert!(entry.blocks.iter().any(|block| {
                block.instructions.iter().any(|instruction| {
                    matches!(
                        instruction,
                        Instruction::CacheSet {
                            cache_key: crate::cps::linear::LinearAtom::Constant(value),
                            ..
                        } if *value == key
                    )
                })
            }));
        });
    }
}

fn linearize_function<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    function: FunctionId,
) -> Procedure<'gc> {
    let data = graph[function];
    let return_cont = data
        .cont
        .expect("graph function should have a return continuation");
    let source_free_vars = binder_set_to_vec(reify.free_vars.function(function));
    let mut builder = ProcedureBuilder::new(graph, reify);
    let binding = builder.value(data.var);
    let return_cont = builder.value(return_cont);
    let params = builder.values(graph.bound_vars_slice(&data.vars));
    let variadic = data.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        data.body,
    );
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
        entry,
        sources,
        blocks,
    }
}

fn linearize_continuation<'gc>(
    graph: &Graph<'gc>,
    reify: &GraphReifyInfo,
    continuation: FunctionId,
) -> Procedure<'gc> {
    let data = graph[continuation];
    assert!(
        data.cont.is_none(),
        "graph continuation should not have a return continuation"
    );
    let source_free_vars = binder_set_to_vec(reify.free_vars.continuation(continuation));
    let mut builder = ProcedureBuilder::new(graph, reify);
    let binding = builder.value(data.var);
    let params = builder.values(graph.bound_vars_slice(&data.vars));
    let variadic = data.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        data.body,
    );
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
        entry,
        sources,
        blocks,
    }
}

fn graph_code_id<'gc>(graph: &Graph<'gc>, function: FunctionId) -> CodeId<'gc> {
    let id = GraphCodeId(function.as_u32());
    if graph[function].cont.is_some() {
        CodeId::GraphFunction(id)
    } else {
        CodeId::GraphContinuation(id)
    }
}

fn binder_set_to_vec(vars: &BinderSet) -> Vec<BoundVar> {
    vars.iter().collect()
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
    builder: &mut ProcedureBuilder<'_, 'gc>,
    binding: ValueId,
    free_vars: &[BoundVar],
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

struct ProcedureBuilder<'a, 'gc> {
    graph: &'a Graph<'gc>,
    reify: &'a GraphReifyInfo,
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<BoundVar, BlockId>,
    values: HashMap<BoundVar, ValueId>,
    known_literals: HashMap<BoundVar, Value<'gc>>,
    sources: HashMap<ValueId, LVarRef<'gc>>,
    next_value: u32,
    next_block: usize,
}

impl<'a, 'gc> ProcedureBuilder<'a, 'gc> {
    fn new(graph: &'a Graph<'gc>, reify: &'a GraphReifyInfo) -> Self {
        Self {
            graph,
            reify,
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            values: HashMap::new(),
            known_literals: HashMap::new(),
            sources: HashMap::new(),
            next_value: 0,
            next_block: 1,
        }
    }

    fn finish(mut self) -> (Vec<Block<'gc>>, HashMap<ValueId, LVarRef<'gc>>) {
        self.blocks.sort_by_key(|block| block.id.0);
        (self.blocks, self.sources)
    }

    fn value(&mut self, var: BoundVar) -> ValueId {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = ValueId(self.next_value);
        self.next_value += 1;
        self.values.insert(var, id);
        self.sources.insert(id, self.graph[var].var);
        id
    }

    fn values(&mut self, vars: &[BoundVar]) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn value_slice(&mut self, vars: &[BoundVar]) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn atom(&mut self, var: FreeVar) -> LinearAtom<'gc> {
        LinearAtom::Local(self.value(self.graph.free_binder(var)))
    }

    fn atoms(&mut self, vars: &FreeVars) -> Vec<LinearAtom<'gc>> {
        self.graph
            .free_vars_slice(vars)
            .iter()
            .copied()
            .map(|var| self.atom(var))
            .collect()
    }

    fn literal_atom(&self, var: FreeVar) -> Option<LinearAtom<'gc>> {
        let binder = self.graph.free_binder(var);
        self.known_literals
            .get(&binder)
            .copied()
            .map(LinearAtom::Constant)
    }

    fn atoms_for_prim(&mut self, prim: Primitive, vars: &FreeVars) -> Vec<LinearAtom<'gc>> {
        let mut args = self.atoms(vars);
        if matches!(prim, Primitive::cache_ref | Primitive::cache_set) {
            if let Some(first) = self.graph.free_vars_slice(vars).first().copied() {
                if let Some(literal) = self.literal_atom(first) {
                    args[0] = literal;
                }
            }
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
        params: Vec<ValueId>,
        variadic: Option<ValueId>,
        mut instructions: Vec<Instruction<'gc>>,
        link: Subterm,
    ) {
        let term = self
            .graph
            .read_term_link(link)
            .unwrap_or_else(|| panic!("dead graph term link while linearizing: {link}"));
        let source = self.graph[term].source;
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
        term: TermId,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        let data = self.graph[term];
        match data.kind {
            TermKind::LetVal((var, expr), body) => {
                let expr = self
                    .graph
                    .read_expr_link(expr)
                    .unwrap_or_else(|| panic!("dead graph expression link while linearizing"));
                self.convert_expr(var, expr, instructions);
                self.convert_term_link(body, instructions)
            }

            TermKind::Fix(functions, body) => {
                let functions = live_functions(self.graph, &functions);
                for function in functions.iter().copied() {
                    let free_vars = binder_set_to_vec(self.reify.free_vars.function(function));
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(self.graph[function].var),
                        code: graph_code_id(self.graph, function),
                        kind: ClosureKind::Function,
                        free_count: free_vars.len(),
                    });
                }

                for function in functions {
                    let closure = self.value(self.graph[function].var);
                    let free_vars = binder_set_to_vec(self.reify.free_vars.function(function));
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                self.convert_term_link(body, instructions)
            }

            TermKind::Letk(continuations, body) => {
                let continuations = live_functions(self.graph, &continuations);
                let (reified_conts, local_conts): (Vec<_>, Vec<_>) = continuations
                    .into_iter()
                    .partition(|continuation| self.graph[*continuation].is_reified);

                for continuation in reified_conts.iter().copied() {
                    let free_vars =
                        binder_set_to_vec(self.reify.free_vars.continuation(continuation));
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(self.graph[continuation].var),
                        code: graph_code_id(self.graph, continuation),
                        kind: ClosureKind::Continuation,
                        free_count: free_vars.len(),
                    });
                }

                for continuation in reified_conts {
                    let closure = self.value(self.graph[continuation].var);
                    let free_vars =
                        binder_set_to_vec(self.reify.free_vars.continuation(continuation));
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                for continuation in &local_conts {
                    let id = self.alloc_block();
                    self.local_blocks.insert(self.graph[*continuation].var, id);
                }

                for continuation in local_conts {
                    let data = self.graph[continuation];
                    let id = self.local_blocks[&data.var];
                    let params = self.values(self.graph.bound_vars_slice(&data.vars));
                    let variadic = data.variadic.map(|var| self.value(var));
                    self.convert_block(
                        id,
                        params_with_variadic(params, variadic),
                        variadic,
                        vec![],
                        data.body,
                    );
                }

                self.convert_term_link(body, instructions)
            }

            TermKind::Continue(cont, args) => {
                let target = self.graph.free_binder(cont);
                if let Some(block) = self.local_blocks.get(&target) {
                    Terminator::Jump {
                        target: *block,
                        args: self.atoms(&args),
                    }
                } else {
                    Terminator::TailCall {
                        callee: LinearAtom::Local(self.value(target)),
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
                hints,
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
            .unwrap_or_else(|| panic!("dead graph term link while linearizing: {link}"));
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
                let dst = self.value(binding);
                self.known_literals.insert(binding, value);
                instructions.push(Instruction::Const { dst, value });
            }
            ExprKind::PrimCall(prim, args) => {
                let prim = primitive_from_value(prim);
                let args = self.atoms_for_prim(prim, &args);
                let dst = self.value(binding);
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
        self.convert_block(block, vec![], None, vec![], link);
        BranchTarget::Local {
            block,
            args: vec![],
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
    closure: ValueId,
    free_vars: &[BoundVar],
) {
    for (index, free_var) in free_vars.iter().enumerate() {
        instructions.push(Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index,
            value: LinearAtom::Local(builder.value(*free_var)),
        });
    }
}
