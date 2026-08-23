//! 0-CFA flow analysis over the graph CPS: the abstract value of every bound
//! variable is the set of closure `FunctionId`s that may flow to it.
//!
//! A `Fix` member's variable holds its own closure; at `App(f, args, cont)`
//! every function in `flow(f)` may be applied and its parameters receive the
//! argument flows; at `Continue(c, args)` the continuation's parameters
//! receive the argument flows.
//!
//! Outputs: per-function call sites (the `App` terms where its closure may be
//! applied), closure classes (functions that may reach the same operator
//! position and must agree on representation), and escape (a closure flowing
//! to a non-application position keeps the boxed representation).

use std::collections::{BTreeSet, HashMap, HashSet};

use super::graph::{BoundVar, ExprKind, FunctionId, Graph, Subterm, TermId, TermKind};
use super::reify::GraphReifyInfo;

/// Per-function flow results.
#[derive(Clone, Debug, Default)]
pub struct FlowInfo {
    /// `App` term ids where this function's closure may be applied.
    pub call_sites: Vec<TermId>,
    /// True when the closure can flow out of known call positions.
    pub escapes: bool,
}

/// Result of the 0-CFA over one compilation unit.
#[derive(Clone, Debug, Default)]
pub struct FlowAnalysis {
    flow: HashMap<BoundVar, BTreeSet<FunctionId>>,
    by_function: HashMap<FunctionId, FlowInfo>,
    class_of: HashMap<FunctionId, usize>,
    classes: Vec<Vec<FunctionId>>,
}

impl FlowAnalysis {
    pub fn new<'gc>(graph: &Graph<'gc>, reify: &GraphReifyInfo) -> Self {
        let mut analysis = Self::default();
        analysis.compute(graph, reify);
        analysis
    }

    pub fn function(&self, function: FunctionId) -> Option<&FlowInfo> {
        self.by_function.get(&function)
    }

    /// Closure class id of a function (functions that must agree on
    /// representation).
    pub fn class_of(&self, function: FunctionId) -> Option<usize> {
        self.class_of.get(&function).copied()
    }

    pub fn classes(&self) -> &[Vec<FunctionId>] {
        &self.classes
    }

    fn compute<'gc>(&mut self, graph: &Graph<'gc>, reify: &GraphReifyInfo) {
        // Root-resolved binder -> function/continuation map (the optimizer
        // merges binders via the union-find).
        let mut callable_by_binder: HashMap<BoundVar, FunctionId> = HashMap::new();
        for function in reify.functions.iter().copied() {
            let binder = graph.function_binder_rooted(graph[function].var);
            callable_by_binder.insert(binder, function);
        }
        for continuation in reify.continuations.iter().copied() {
            let binder = graph.function_binder_rooted(graph[continuation].var);
            callable_by_binder.insert(binder, continuation);
        }

        // Parameters of each callable, in order.
        let mut params: HashMap<FunctionId, Vec<BoundVar>> = HashMap::new();
        for function in reify.functions.iter().copied() {
            let data = graph[function];
            params.insert(
                function,
                graph.bound_vars_slice(&data.vars).iter().copied().collect(),
            );
        }
        for continuation in reify.continuations.iter().copied() {
            let data = graph[continuation];
            params.insert(
                continuation,
                graph.bound_vars_slice(&data.vars).iter().copied().collect(),
            );
        }

        // Seed: each function's own var flows its own closure.
        for function in reify.functions.iter().copied() {
            let binder = graph.function_binder_rooted(graph[function].var);
            self.flow.entry(binder).or_default().insert(function);
        }

        // Fixed-point propagation over all terms.
        let root = graph[reify.entrypoint].body;
        let root = graph.read_term_link(root).expect("graph root term");
        let mut changed = true;
        while changed {
            changed = false;
            let mut visited = HashSet::new();
            for_term(graph, root, &mut visited, &mut |term| {
                changed |= self.propagate_term(graph, term, &params, &callable_by_binder);
            });
        }

        // Call web + escape detection.
        let mut by_function: HashMap<FunctionId, FlowInfo> = HashMap::new();
        for function in reify.functions.iter().copied() {
            by_function.insert(function, FlowInfo::default());
        }
        let mut escaped = HashSet::new();
        let mut visited = HashSet::new();
        {
            let mut use_visitor = UseVisitor {
                graph,
                flow: &self.flow,
                by_function: &mut by_function,
                escaped: &mut escaped,
            };
            for_term(graph, root, &mut visited, &mut |term| use_visitor.visit(term));
        }

        // Closure classes: union functions whose closures may reach the same
        // App operator position.
        let mut union = UnionFind::new();
        for function in reify.functions.iter().copied() {
            union.add(function);
        }
        let mut visited = HashSet::new();
        {
            let mut class_visitor = ClassVisitor {
                graph,
                flow: &self.flow,
                union: &mut union,
            };
            for_term(graph, root, &mut visited, &mut |term| class_visitor.visit(term));
        }

        let mut class_of: HashMap<FunctionId, usize> = HashMap::new();
        let mut classes: Vec<Vec<FunctionId>> = Vec::new();
        let mut index: HashMap<FunctionId, usize> = HashMap::new();
        for function in reify.functions.iter().copied() {
            let rep = union.find(function);
            if !index.contains_key(&rep) {
                let i = classes.len();
                index.insert(rep, i);
                classes.push(Vec::new());
            }
            let i = index[&rep];
            classes[i].push(function);
            class_of.insert(function, i);
        }

        for function in reify.functions.iter().copied() {
            by_function.get_mut(&function).unwrap().escapes = escaped.contains(&function);
        }

        self.by_function = by_function;
        self.class_of = class_of;
        self.classes = classes;
    }

    /// Propagate one term's App/Continue constraints; returns whether the
    /// flow sets changed.
    fn propagate_term<'gc>(
        &mut self,
        graph: &Graph<'gc>,
        term: TermId,
        params: &HashMap<FunctionId, Vec<BoundVar>>,
        callable_by_binder: &HashMap<BoundVar, FunctionId>,
    ) -> bool {
        let mut changed = false;
        match graph[term].kind {
            TermKind::App(func, args, _) => {
                let op_binder = graph.free_binder_rooted(func);
                let callees: Vec<FunctionId> = self
                    .flow
                    .get(&op_binder)
                    .map(|s| s.iter().copied().collect())
                    .unwrap_or_default();
                let args_binders: Vec<BoundVar> = graph
                    .free_vars_slice(&args)
                    .iter()
                    .copied()
                    .map(|v| graph.free_binder_rooted(v))
                    .collect();
                for callee in callees {
                    if let Some(ps) = params.get(&callee) {
                        for (param, arg) in ps.iter().zip(args_binders.iter()) {
                            let arg_flow: Vec<FunctionId> = self
                                .flow
                                .get(arg)
                                .map(|s| s.iter().copied().collect())
                                .unwrap_or_default();
                            if !arg_flow.is_empty() {
                                let entry = self.flow.entry(*param).or_default();
                                let before = entry.len();
                                for f in arg_flow {
                                    entry.insert(f);
                                }
                                changed |= entry.len() != before;
                            }
                        }
                    }
                }
            }
            TermKind::Continue(cont, args) => {
                let cont_binder = graph.free_binder_rooted(cont);
                let args_binders: Vec<BoundVar> = graph
                    .free_vars_slice(&args)
                    .iter()
                    .copied()
                    .map(|v| graph.free_binder_rooted(v))
                    .collect();
                if let Some(continuation) = callable_by_binder.get(&cont_binder) {
                    if let Some(ps) = params.get(continuation) {
                        for (param, arg) in ps.iter().zip(args_binders.iter()) {
                            let arg_flows: Vec<FunctionId> = self
                                .flow
                                .get(arg)
                                .map(|s| s.iter().copied().collect())
                                .unwrap_or_default();
                            if !arg_flows.is_empty() {
                                let entry = self.flow.entry(*param).or_default();
                                let before = entry.len();
                                for f in arg_flows {
                                    entry.insert(f);
                                }
                                changed |= entry.len() != before;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        changed
    }
}

/// Visit every term reachable from `root` (deduplicated).
fn for_term<'gc>(
    graph: &Graph<'gc>,
    root: TermId,
    visited: &mut HashSet<TermId>,
    f: &mut impl FnMut(TermId),
) {
    if !visited.insert(root) {
        return;
    }
    f(root);
    match graph[root].kind {
        TermKind::LetVal((_, _expr), body) => {
            walk_link(graph, body, visited, f);
        }
        TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
            for link in graph.function_links_slice(&functions).iter().copied() {
                if let Some(function) = graph.read_function_link(link) {
                    walk_link(graph, graph[function].body, visited, f);
                }
            }
            walk_link(graph, body, visited, f);
        }
        TermKind::If(_, consequent, alternative, _) => {
            walk_link(graph, consequent, visited, f);
            walk_link(graph, alternative, visited, f);
        }
        TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
    }
}

fn walk_link<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    visited: &mut HashSet<TermId>,
    f: &mut impl FnMut(TermId),
) {
    if let Some(term) = graph.read_term_link(link) {
        for_term(graph, term, visited, f);
    }
}

/// Escape and call-site visitor: marks functions whose flows reach
/// non-application positions and records per-function call sites.
struct UseVisitor<'a, 'gc> {
    graph: &'a Graph<'gc>,
    flow: &'a HashMap<BoundVar, BTreeSet<FunctionId>>,
    by_function: &'a mut HashMap<FunctionId, FlowInfo>,
    escaped: &'a mut HashSet<FunctionId>,
}

impl<'a, 'gc> UseVisitor<'a, 'gc> {
    fn visit(&mut self, term: TermId) {
        let graph = self.graph;
        match graph[term].kind {
            TermKind::LetVal((_, expr), _) => {
                if let Some(expr) = graph.read_expr_link(expr) {
                    if let ExprKind::PrimCall(_, args) = graph[expr].kind {
                        self.escape_vars(&args);
                    }
                }
            }
            TermKind::If(test, _, _, _) => {
                self.escape_var(graph.free_binder_rooted(test));
            }
            TermKind::Continue(_, args) => {
                // Continuation arguments stay within known control flow.
                let _ = args;
            }
            TermKind::App(func, args, _) => {
                let op_binder = graph.free_binder_rooted(func);
                self.record_call(&op_binder, term);
                let known_callee = self
                    .flow
                    .get(&op_binder)
                    .map_or(false, |s| !s.is_empty());
                if !known_callee {
                    // Arguments passed to an unknown callee: escapes.
                    self.escape_vars(&args);
                } else {
                    // Arguments flow into known callee parameters; they are
                    // only ever applied there or passed further (tracked by
                    // the flow sets, not marked escaping).
                    let _ = &args;
                }
            }
            TermKind::Raise(_, args) => {
                self.escape_vars(&args);
            }
            TermKind::Fix(..) | TermKind::Letk(..) => {}
        }
    }

    fn escape_vars(&mut self, vars: &super::graph::FreeVars) {
        for var in self.graph.free_vars_slice(vars).iter().copied() {
            self.escape_var(self.graph.free_binder_rooted(var));
        }
    }

    fn escape_var(&mut self, binder: BoundVar) {
        if let Some(flow) = self.flow.get(&binder) {
            for f in flow {
                self.escaped.insert(*f);
            }
        }
    }

    fn record_call(&mut self, op_binder: &BoundVar, site: TermId) {
        if let Some(flow) = self.flow.get(op_binder) {
            for f in flow {
                let info = self.by_function.entry(*f).or_default();
                info.call_sites.push(site);
            }
        }
    }
}

/// Closure-class visitor: union functions that may apply at the same site.
struct ClassVisitor<'a, 'gc> {
    graph: &'a Graph<'gc>,
    flow: &'a HashMap<BoundVar, BTreeSet<FunctionId>>,
    union: &'a mut UnionFind,
}

impl<'a, 'gc> ClassVisitor<'a, 'gc> {
    fn visit(&mut self, term: TermId) {
        if let TermKind::App(func, _, _) = self.graph[term].kind {
            let binder = self.graph.free_binder_rooted(func);
            let ops: Vec<FunctionId> = self
                .flow
                .get(&binder)
                .map(|s| s.iter().copied().collect())
                .unwrap_or_default();
            if ops.len() >= 2 {
                let rep = ops[0];
                for op in ops.iter().skip(1) {
                    self.union.union(rep, *op);
                }
            }
        }
    }
}

/// Closure-class union-find (keyed by FunctionId).
struct UnionFind {
    parent: HashMap<FunctionId, FunctionId>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parent: HashMap::new(),
        }
    }

    fn add(&mut self, f: FunctionId) {
        self.parent.entry(f).or_insert(f);
    }

    fn find(&mut self, f: FunctionId) -> FunctionId {
        let parent = self.parent[&f];
        if parent == f {
            f
        } else {
            let root = self.find(parent);
            self.parent.insert(f, root);
            root
        }
    }

    fn union(&mut self, a: FunctionId, b: FunctionId) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra != rb {
            self.parent.insert(ra, rb);
        }
    }
}
