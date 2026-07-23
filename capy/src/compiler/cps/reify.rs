use cranelift_entity::{EntitySet, SecondaryMap};

use super::graph::{BoundVar, FunctionId, Graph, Subterm, TermKind};

pub type BinderSet = EntitySet<BoundVar>;

#[derive(Clone, Debug, Default)]
pub struct GraphFreeVars {
    pub function_vars: SecondaryMap<FunctionId, BinderSet>,
    pub continuation_vars: SecondaryMap<FunctionId, BinderSet>,
    pub functions_by_binder: SecondaryMap<BoundVar, Option<FunctionId>>,
    pub continuations_by_binder: SecondaryMap<BoundVar, Option<FunctionId>>,
    pub continuation_values: EntitySet<BoundVar>,
}

impl GraphFreeVars {
    pub fn function(&self, function: FunctionId) -> &BinderSet {
        &self.function_vars[function]
    }

    pub fn continuation(&self, continuation: FunctionId) -> &BinderSet {
        &self.continuation_vars[continuation]
    }
}

#[derive(Clone, Debug)]
pub struct GraphReifyInfo {
    pub entrypoint: FunctionId,
    pub functions: Vec<FunctionId>,
    pub continuations: Vec<FunctionId>,
    pub free_vars: GraphFreeVars,
}

/// Collect graph CPS closures, free variables, and heap-allocated continuations.
///
/// Function free variables are recorded for every live function except that the
/// entry function is forced to have no free variables. Continuations are marked
/// reified when they are used as values, captured by a function closure, or
/// transitively captured by another reified continuation.
pub fn reify_graph<'gc>(graph: &mut Graph<'gc>, entrypoint: FunctionId) -> GraphReifyInfo {
    let mut collector = ReifyCollector::new();
    collector.record_function(graph, entrypoint);
    let _ = collector.collect_function(graph, entrypoint);
    collector.free_vars.function_vars[entrypoint].clear();
    collector.propagate_reified_continuations(graph);

    GraphReifyInfo {
        entrypoint,
        functions: collector.functions,
        continuations: collector.continuations,
        free_vars: collector.free_vars,
    }
}

struct ReifyCollector {
    free_vars: GraphFreeVars,
    functions: Vec<FunctionId>,
    continuations: Vec<FunctionId>,
    seen_functions: EntitySet<FunctionId>,
    seen_continuations: EntitySet<FunctionId>,
    computed_functions: EntitySet<FunctionId>,
}

impl ReifyCollector {
    fn new() -> Self {
        Self {
            free_vars: GraphFreeVars::default(),
            functions: Vec::new(),
            continuations: Vec::new(),
            seen_functions: EntitySet::new(),
            seen_continuations: EntitySet::new(),
            computed_functions: EntitySet::new(),
        }
    }

    fn record_function<'gc>(&mut self, graph: &Graph<'gc>, function: FunctionId) {
        let data = graph[function];
        if data.cont.is_some() {
            if self.seen_functions.insert(function) {
                self.functions.push(function);
            }
            self.free_vars.functions_by_binder[data.var] = Some(function);
        } else {
            if self.seen_continuations.insert(function) {
                self.continuations.push(function);
            }
            self.free_vars.continuations_by_binder[data.var] = Some(function);
        }
    }

    fn collect_function<'gc>(&mut self, graph: &Graph<'gc>, function: FunctionId) -> BinderSet {
        self.record_function(graph, function);
        if self.computed_functions.contains(function) {
            return self.vars_for_function(graph, function).clone();
        }

        self.computed_functions.insert(function);
        let data = graph[function];
        let mut vars = self.collect_term(graph, data.body);
        for var in graph.bound_vars_slice(&data.vars).iter().copied() {
            vars.remove(var);
        }
        if let Some(variadic) = data.variadic {
            vars.remove(variadic);
        }

        if let Some(return_cont) = data.cont {
            vars.remove(return_cont);
            self.free_vars.function_vars[function] = vars.clone();
        } else {
            vars.remove(data.var);
            self.free_vars.continuation_vars[function] = vars.clone();
        }

        vars
    }

    fn vars_for_function<'gc>(&self, graph: &Graph<'gc>, function: FunctionId) -> &BinderSet {
        if graph[function].cont.is_some() {
            &self.free_vars.function_vars[function]
        } else {
            &self.free_vars.continuation_vars[function]
        }
    }

    fn collect_term<'gc>(&mut self, graph: &Graph<'gc>, link: Subterm) -> BinderSet {
        let Some(term) = graph.read_term_link(link) else {
            return BinderSet::new();
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                let mut vars = graph
                    .read_expr_link(expr)
                    .map(|expr| self.collect_expr(graph, expr))
                    .unwrap_or_default();
                vars.extend(self.collect_term(graph, body).iter());
                vars.remove(binding);
                vars
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let live_functions = graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| graph.read_function_link(link))
                    .collect::<Vec<_>>();

                for function in live_functions.iter().copied() {
                    self.record_function(graph, function);
                }

                let mut vars = BinderSet::new();
                for function in live_functions.iter().copied() {
                    vars.extend(self.collect_function(graph, function).iter());
                }
                vars.extend(self.collect_term(graph, body).iter());

                for function in live_functions {
                    vars.remove(graph[function].var);
                }
                vars
            }
            TermKind::If(test, consequent, alternative, _) => {
                let mut vars = self.singleton_free_var(graph, test);
                vars.extend(self.collect_term(graph, consequent).iter());
                vars.extend(self.collect_term(graph, alternative).iter());
                vars
            }
            TermKind::Continue(cont, args) => {
                let mut vars = self.singleton_free_var(graph, cont);
                vars.extend(self.free_vars_from_occurrences(graph, &args).iter());
                vars
            }
            TermKind::App(func, args, cont) => {
                let cont_binder = graph.free_binder(cont);
                self.free_vars.continuation_values.insert(cont_binder);

                let mut vars = self.singleton_free_var(graph, func);
                vars.extend(self.free_vars_from_occurrences(graph, &args).iter());
                vars.insert(cont_binder);
                vars
            }
            TermKind::Raise(_, args) => self.free_vars_from_occurrences(graph, &args),
        }
    }

    fn collect_expr<'gc>(&mut self, graph: &Graph<'gc>, expr: super::graph::ExprId) -> BinderSet {
        match graph[expr].kind {
            super::graph::ExprKind::Literal(_) => BinderSet::new(),
            super::graph::ExprKind::PrimCall(_, args) => {
                let vars = self.free_vars_from_occurrences(graph, &args);
                for binder in vars.iter() {
                    if self.free_vars.continuations_by_binder[binder].is_some() {
                        self.free_vars.continuation_values.insert(binder);
                    }
                }
                vars
            }
        }
    }

    fn singleton_free_var<'gc>(&self, graph: &Graph<'gc>, var: super::graph::FreeVar) -> BinderSet {
        let mut vars = BinderSet::new();
        vars.insert(graph.free_binder(var));
        vars
    }

    fn free_vars_from_occurrences<'gc>(
        &self,
        graph: &Graph<'gc>,
        vars: &super::graph::FreeVars,
    ) -> BinderSet {
        let mut out = BinderSet::new();
        out.extend(
            graph
                .free_vars_slice(vars)
                .iter()
                .copied()
                .map(|var| graph.free_binder(var)),
        );
        out
    }

    fn propagate_reified_continuations<'gc>(&mut self, graph: &mut Graph<'gc>) {
        for continuation in self.continuations.iter().copied() {
            graph[continuation].is_reified = false;
        }

        let mut stack = Vec::new();
        for binder in self.free_vars.continuation_values.iter() {
            if let Some(continuation) = self.free_vars.continuations_by_binder[binder] {
                stack.push(continuation);
            }
        }

        for function in self.functions.iter().copied() {
            for binder in self.free_vars.function(function).iter() {
                if let Some(continuation) = self.free_vars.continuations_by_binder[binder] {
                    stack.push(continuation);
                }
            }
        }

        while let Some(continuation) = stack.pop() {
            if graph[continuation].is_reified {
                continue;
            }

            graph[continuation].is_reified = true;
            for binder in self.free_vars.continuation(continuation).iter() {
                if let Some(next) = self.free_vars.continuations_by_binder[binder] {
                    stack.push(next);
                }
            }
        }
    }
}
