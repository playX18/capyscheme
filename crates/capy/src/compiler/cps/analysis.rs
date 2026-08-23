//! Closure-optimization analyses: stage (first/last-use) analysis and
//! recursion detection.
//!
//! Stage analysis records, per function, the first and last use position of
//! each free variable in body traversal order. It feeds the reuse rule: a
//! shared record may be reused by a nested site only when every use of every
//! reused variable precedes every use of the closure's private variables, so
//! the record is dead before the private slots are touched.
//!
//! The call web, closure classes, and escape info come from the flow-analysis
//! phase; see `flow.rs`.

use std::collections::HashMap;

use super::graph::{BoundVar, ExprId, ExprKind, FunctionId, Graph, Subterm, TermKind};

/// First/last use positions of a function's free variables, in body
/// traversal order.
#[derive(Clone, Debug, Default)]
pub struct FunctionStages {
    uses: HashMap<BoundVar, (usize, usize)>,
}

impl FunctionStages {
    pub fn first(&self, var: BoundVar) -> usize {
        self.uses.get(&var).map_or(usize::MAX, |(first, _)| *first)
    }

    pub fn last(&self, var: BoundVar) -> usize {
        self.uses.get(&var).map_or(0, |(_, last)| *last)
    }

    /// Whether every use of every `reused` variable precedes every use of
    /// every `own` variable (the reverse-linked ordering rule). Vacuously
    /// true when either side is empty.
    pub fn reused_before_own(&self, reused: &[BoundVar], own: &[BoundVar]) -> bool {
        if reused.is_empty() || own.is_empty() {
            return true;
        }
        let last_reused = reused.iter().map(|v| self.last(*v)).max().unwrap_or(0);
        let first_own = own.iter().map(|v| self.first(*v)).min().unwrap_or(usize::MAX);
        last_reused < first_own
    }
}

/// Stage analysis for every live function in a compilation unit.
#[derive(Clone, Debug, Default)]
pub struct StageAnalysis {
    by_function: HashMap<FunctionId, FunctionStages>,
}

impl StageAnalysis {
    pub fn new<'gc>(graph: &Graph<'gc>, reify: &super::reify::GraphReifyInfo) -> Self {
        let mut by_function = HashMap::new();
        for function in reify.functions.iter().copied() {
            by_function.insert(function, function_stages(graph, function));
        }
        Self { by_function }
    }

    pub fn function(&self, function: FunctionId) -> Option<&FunctionStages> {
        self.by_function.get(&function)
    }
}

/// Compute the first/last-use stages of a function's body.
pub fn function_stages<'gc>(
    graph: &Graph<'gc>,
    function: FunctionId,
) -> FunctionStages {
    let mut collector = StageCollector {
        graph,
        counter: 0,
        uses: HashMap::new(),
    };
    collector.walk(graph[function].body);
    FunctionStages {
        uses: collector.uses,
    }
}

/// Recursive functions via the direct call graph: edge `f -> g` when `f`'s
/// body calls a variable bound to `g`. A function is recursive when it lies on
/// a cycle. Serves as the frequency proxy for compaction: closures created in
/// recursive functions are hot and revert to the flat representation.
pub fn recursive_functions<'gc>(
    graph: &Graph<'gc>,
    reify: &super::reify::GraphReifyInfo,
) -> std::collections::HashSet<FunctionId> {
    // Root-resolved binder -> function map (the optimizer merges binders via
    // the union-find; raw `data.var` may be stale).
    let mut functions_by_binder: HashMap<BoundVar, FunctionId> = HashMap::new();
    for function in reify.functions.iter().copied() {
        let binder = graph.function_binder_rooted(graph[function].var);
        functions_by_binder.insert(binder, function);
    }

    let mut callees: HashMap<FunctionId, Vec<FunctionId>> = HashMap::new();
    for function in reify.functions.iter().copied() {
        let mut collector = CallCollector {
            graph,
            functions_by_binder: &functions_by_binder,
            callees: Vec::new(),
        };
        collector.walk(graph[function].body);
        callees.insert(function, collector.callees);
    }

    // DFS-based cycle detection: a function is recursive when it lies on a
    // directed cycle of the direct call graph.
    fn reaches_cycle(
        function: FunctionId,
        callees: &HashMap<FunctionId, Vec<FunctionId>>,
        state: &mut HashMap<FunctionId, u8>, // 0=unvisited 1=in-stack 2=done
    ) -> bool {
        match state.get(&function) {
            Some(1) => return true,
            Some(2) => return false,
            _ => {}
        }
        state.insert(function, 1);
        if let Some(targets) = callees.get(&function) {
            for target in targets {
                if reaches_cycle(*target, callees, state) {
                    return true;
                }
            }
        }
        state.insert(function, 2);
        false
    }

    let mut recursive = std::collections::HashSet::new();
    for function in reify.functions.iter().copied() {
        let mut state = HashMap::new();
        if reaches_cycle(function, &callees, &mut state) {
            recursive.insert(function);
        }
    }
    recursive
}

struct CallCollector<'a, 'gc> {
    graph: &'a Graph<'gc>,
    functions_by_binder: &'a HashMap<BoundVar, FunctionId>,
    callees: Vec<FunctionId>,
}

impl<'a, 'gc> CallCollector<'a, 'gc> {
    fn walk(&mut self, link: Subterm) {
        let Some(term) = self.graph.read_term_link(link) else {
            return;
        };
        match self.graph[term].kind {
            TermKind::LetVal((_, expr), body) => {
                if let Some(expr) = self.graph.read_expr_link(expr) {
                    if let ExprKind::PrimCall(_, _) = self.graph[expr].kind {
                        // primitives call no graph functions
                    }
                }
                self.walk(body);
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let members: Vec<_> = self
                    .graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| self.graph.read_function_link(link))
                    .collect();
                for function in members {
                    self.walk(self.graph[function].body);
                }
                self.walk(body);
            }
            TermKind::If(_, consequent, alternative, _) => {
                self.walk(consequent);
                self.walk(alternative);
            }
            TermKind::Continue(..) => {}
            TermKind::App(func, _, _) => {
                let binder = self.graph.free_binder_rooted(func);
                if let Some(target) = self.functions_by_binder.get(&binder) {
                    self.callees.push(*target);
                }
            }
            TermKind::Raise(..) => {}
        }
    }
}

struct StageCollector<'a, 'gc> {
    graph: &'a Graph<'gc>,
    counter: usize,
    uses: HashMap<BoundVar, (usize, usize)>,
}

impl<'a, 'gc> StageCollector<'a, 'gc> {
    fn walk(&mut self, link: Subterm) {
        let Some(term) = self.graph.read_term_link(link) else {
            return;
        };
        self.counter += 1;
        let position = self.counter;
        match self.graph[term].kind {
            TermKind::LetVal((_, expr), body) => {
                if let Some(expr) = self.graph.read_expr_link(expr) {
                    self.walk_expr(expr);
                }
                self.walk(body);
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let members: Vec<_> = self
                    .graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| self.graph.read_function_link(link))
                    .collect();
                for function in members {
                    self.walk(self.graph[function].body);
                }
                self.walk(body);
            }
            TermKind::If(test, consequent, alternative, _) => {
                self.record(self.graph.free_binder(test), position);
                self.walk(consequent);
                self.walk(alternative);
            }
            TermKind::Continue(_, args) => {
                for var in self.graph.free_vars_slice(&args).iter().copied() {
                    self.record(self.graph.free_binder(var), position);
                }
            }
            TermKind::App(func, args, cont) => {
                self.record(self.graph.free_binder(func), position);
                for var in self.graph.free_vars_slice(&args).iter().copied() {
                    self.record(self.graph.free_binder(var), position);
                }
                self.record(self.graph.free_binder(cont), position);
            }
            TermKind::Raise(_, args) => {
                for var in self.graph.free_vars_slice(&args).iter().copied() {
                    self.record(self.graph.free_binder(var), position);
                }
            }
        }
    }

    fn walk_expr(&mut self, expr: ExprId) {
        self.counter += 1;
        let position = self.counter;
        if let ExprKind::PrimCall(_, args) = self.graph[expr].kind {
            for var in self.graph.free_vars_slice(&args).iter().copied() {
                self.record(self.graph.free_binder(var), position);
            }
        }
    }

    fn record(&mut self, var: BoundVar, position: usize) {
        let entry = self.uses.entry(var).or_insert((position, position));
        entry.0 = entry.0.min(position);
        entry.1 = entry.1.max(position);
    }
}
