use std::{
    env, fs,
    path::Path,
    sync::atomic::{AtomicUsize, Ordering},
};

use cranelift_entity::{EntitySet, SecondaryMap};

use crate::{
    cps::{
        fold::folding_table,
        term::{Atom as CpsAtom, FuncRef},
    },
    runtime::{Context, value::Value},
    utils::pass_profile::ProfileScope,
};

use super::{
    convert::{ConvertResult, cps_to_graph, graph_to_cps},
    dom_contify,
    graph::{
        ActiveLinkStatus, BoundVar, ContVar, ExprKind, FreeVar, FunctionId, FunctionLink,
        FunctionLinks, Graph, GraphWorklist, Parent, Subexpr, Subterm, TermId, TermKind,
        WorklistQueue,
    },
    scc_contify,
};

pub const DEFAULT_GAS: usize = 42_000;

static CONTIFY_DUMP_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct OptimizationStats {
    pub iterations: usize,
    pub gas_used: usize,
    pub ran_out_of_gas: bool,
    pub dead_bindings_processed: usize,
    pub dead_letvals_removed: usize,
    pub eta_continuations: usize,
    pub eta_functions: usize,
    pub constant_branches_simplified: usize,
    pub identical_branches_simplified: usize,
    pub known_prim_propagations: usize,
    pub singleton_calls_inlined: usize,
    pub singleton_continuations_inlined: usize,
    pub contifications: usize,
    pub scc_contifications: usize,
    pub dom_contifications: usize,
    pub contified_functions: usize,
    pub scc_contified_functions: usize,
    pub dom_contified_functions: usize,
}

macro_rules! verbose_log {
    ($($arg:tt)*) => {
        if super::graph::VERBOSE {
            eprintln!($($arg)*);
        }
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GcpsContifyMode {
    Off,
    Scc,
    Dom,
    DomThenScc,
}

impl GcpsContifyMode {
    fn current() -> Self {
        match env::var("CAPY_GCPS_CONTIFY").ok().as_deref() {
            Some("0" | "off" | "none" | "false") => Self::Off,
            Some("scc" | "legacy") => Self::Scc,
            Some("both" | "dom+scc" | "dom-then-scc") => Self::DomThenScc,
            Some("dom" | "dominator" | "dominators") | None => Self::Dom,
            Some(_) => Self::Dom,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ContifySource {
    Scc,
    Dominator,
}

pub fn optimize_func<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> ConvertResult<FuncRef<'gc>> {
    let mut convert_profile = ProfileScope::new("compiler.lower.gcps.convert");
    let mut program = cps_to_graph(ctx, func.body())?;
    if convert_profile.is_enabled() {
        let stats = program.graph.stats();
        convert_profile.field("terms", stats.terms);
        convert_profile.field("functions", stats.functions);
        convert_profile.field("free_occurrences", stats.free_occurrences);
        convert_profile.field("bound_vars", stats.bound_vars);
        convert_profile.field("term_links", stats.term_links);
    }
    drop(convert_profile);

    let mut optimize_profile = ProfileScope::new("compiler.lower.gcps.run");
    let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(DEFAULT_GAS));
    if optimize_profile.is_enabled() {
        let graph_stats = program.graph.stats();
        optimize_profile.field("iterations", stats.iterations);
        optimize_profile.field("gas_used", stats.gas_used);
        optimize_profile.field("ran_out_of_gas", stats.ran_out_of_gas);
        optimize_profile.field("dead_bindings", stats.dead_bindings_processed);
        optimize_profile.field("dead_letvals", stats.dead_letvals_removed);
        optimize_profile.field("eta_continuations", stats.eta_continuations);
        optimize_profile.field("eta_functions", stats.eta_functions);
        optimize_profile.field("constant_branches", stats.constant_branches_simplified);
        optimize_profile.field("identical_branches", stats.identical_branches_simplified);
        optimize_profile.field("known_prim_propagations", stats.known_prim_propagations);
        optimize_profile.field("calls_inlined", stats.singleton_calls_inlined);
        optimize_profile.field("conts_inlined", stats.singleton_continuations_inlined);
        optimize_profile.field("contifications", stats.contifications);
        optimize_profile.field("scc_contifications", stats.scc_contifications);
        optimize_profile.field("dom_contifications", stats.dom_contifications);
        optimize_profile.field("contified_functions", stats.contified_functions);
        optimize_profile.field("scc_contified_functions", stats.scc_contified_functions);
        optimize_profile.field("dom_contified_functions", stats.dom_contified_functions);
        optimize_profile.field("terms", graph_stats.terms);
        optimize_profile.field("free_occurrences", graph_stats.free_occurrences);
        optimize_profile.field("bound_vars", graph_stats.bound_vars);
    }
    drop(optimize_profile);

    let mut reify_profile = ProfileScope::new("compiler.lower.gcps.reify");
    let body = graph_to_cps(ctx, &program.graph, program.root)?;
    if reify_profile.is_enabled() {
        let stats = program.graph.stats();
        reify_profile.field("terms", stats.terms);
        reify_profile.field("functions", stats.functions);
        reify_profile.field("free_occurrences", stats.free_occurrences);
    }
    Ok(func.with_body(ctx, body))
}

pub fn optimize_graph<'gc>(
    ctx: Context<'gc>,
    graph: &mut Graph<'gc>,
    root: Subterm,
    gas: Option<usize>,
) -> OptimizationStats {
    let mut state = OptimizerState::new();
    let contify_mode = GcpsContifyMode::current();
    {
        let mut profile = ProfileScope::new("compiler.lower.gcps.collect");
        state.collect_redexes(graph, root);
        if profile.is_enabled() {
            let stats = graph.stats();
            profile.field("terms", stats.terms);
            profile.field("functions", stats.functions);
            profile.field("pending_subterms", state.worklist.pending_subterm_len());
            profile.field("known_functions", state.known_function_count);
            profile.field("known_exprs", state.known_expr_count);
            profile.field("contify_mode", format!("{contify_mode:?}"));
        }
    }
    state.run(ctx, graph, gas.unwrap_or(usize::MAX), contify_mode);
    state.stats
}

pub(super) struct OptimizerState {
    worklist: GraphWorklist,
    known_functions: SecondaryMap<BoundVar, Option<FunctionLink>>,
    function_defs: SecondaryMap<BoundVar, Option<Subterm>>,
    local_function_binders: SecondaryMap<BoundVar, bool>,
    function_owner_defs: SecondaryMap<FunctionId, Option<Subterm>>,
    return_cont_owners: SecondaryMap<BoundVar, Option<FunctionId>>,
    known_exprs: SecondaryMap<BoundVar, Option<super::graph::Subexpr>>,
    known_expr_defs: SecondaryMap<BoundVar, Option<Subterm>>,
    known_function_count: usize,
    known_expr_count: usize,
    dead_bindings: WorklistQueue<BoundVar>,
    stats: OptimizationStats,
    old_occurrences: Vec<FreeVar>,
    new_occurrences: Vec<FreeVar>,
    occurrence_set: EntitySet<FreeVar>,
}

pub(super) struct ContifyCandidate {
    pub(super) binders: EntitySet<BoundVar>,
    pub(super) return_cont: BoundVar,
    pub(super) site: Subterm,
    pub(super) source: ContifySource,
}

impl OptimizerState {
    fn new() -> Self {
        Self {
            worklist: GraphWorklist::new(),
            known_functions: SecondaryMap::new(),
            function_defs: SecondaryMap::new(),
            local_function_binders: SecondaryMap::new(),
            function_owner_defs: SecondaryMap::new(),
            return_cont_owners: SecondaryMap::new(),
            known_exprs: SecondaryMap::new(),
            known_expr_defs: SecondaryMap::new(),
            known_function_count: 0,
            known_expr_count: 0,
            dead_bindings: WorklistQueue::new(),
            stats: OptimizationStats::default(),
            old_occurrences: Vec::new(),
            new_occurrences: Vec::new(),
            occurrence_set: EntitySet::new(),
        }
    }

    fn run<'gc>(
        &mut self,
        ctx: Context<'gc>,
        graph: &mut Graph<'gc>,
        mut gas: usize,
        contify_mode: GcpsContifyMode,
    ) {
        let initial_gas = gas;
        while gas > 0 {
            gas -= 1;
            self.process_dead_bindings(graph);

            let Some((queued_link, term, parent)) = self.worklist.get_next_live_subterm(graph)
            else {
                verbose_log!("gcps optimize: ran out of work");
                break;
            };

            let active_link = if parent.is_none() {
                queued_link
            } else {
                let active = graph.get_active_link_for(term);
                let ActiveLinkStatus::ActiveSubterm(active_link) = active else {
                    continue;
                };
                active_link
            };
            self.stats.iterations += 1;

            match graph[term].kind {
                TermKind::LetVal((binder, expr), body) => {
                    self.reduce_letval(
                        ctx,
                        graph,
                        queued_link,
                        active_link,
                        term,
                        binder,
                        expr,
                        body,
                    );
                }
                TermKind::App(callee, args, cont) => {
                    self.reduce_call(graph, queued_link, active_link, term, callee, args, cont);
                }
                TermKind::Continue(cont, args) => {
                    self.reduce_continue(graph, queued_link, active_link, term, cont, args);
                }
                TermKind::Letk(functions, _) => {
                    self.reduce_eta_continuations(graph, functions);
                }
                TermKind::Fix(functions, body) => {
                    self.reduce_eta_functions(graph, functions);
                    self.try_contify_fix(graph, active_link, term, functions, body, contify_mode);
                }
                TermKind::If(test, consequent, alternative) => {
                    self.reduce_if(
                        graph,
                        queued_link,
                        active_link,
                        term,
                        test,
                        consequent,
                        alternative,
                    );
                }
                _ => {}
            }
        }

        if gas == 0 {
            verbose_log!("gcps optimize: ran out of gas");
            self.stats.ran_out_of_gas = true;
        }
        self.stats.gas_used = initial_gas.saturating_sub(gas);
    }

    fn collect_redexes<'gc>(&mut self, graph: &Graph<'gc>, root: Subterm) {
        let Some(term) = graph.read_term_link(root) else {
            return;
        };

        match graph[term].kind {
            TermKind::LetVal(..)
            | TermKind::App(..)
            | TermKind::Continue(..)
            | TermKind::If(..)
            | TermKind::Fix(..) => {
                self.worklist.add_subterm(root);
            }
            TermKind::Letk(..) => {}
            _ => {}
        }

        if let TermKind::LetVal((binder, expr), _) = graph[term].kind {
            if self.known_exprs[binder].is_none() {
                self.known_expr_count += 1;
            }
            self.known_exprs[binder] = Some(expr);
            self.known_expr_defs[binder] = Some(root);
        }

        if let TermKind::Fix(functions, _) | TermKind::Letk(functions, _) = graph[term].kind {
            for link in graph.function_links_slice(&functions) {
                let Some(function) = graph.read_function_link(*link) else {
                    continue;
                };
                let binder = graph[function].var;
                if self.known_functions[binder].is_none() {
                    self.known_function_count += 1;
                }
                self.known_functions[binder] = Some(*link);
                self.function_defs[binder] = Some(root);
                self.local_function_binders[binder] = true;
                self.function_owner_defs[function] = Some(root);
                if let Some(return_cont) = graph[function].cont {
                    self.local_function_binders[return_cont] = true;
                    self.return_cont_owners[return_cont] = Some(function);
                }
            }
        }

        if let TermKind::Letk(functions, _) = graph[term].kind
            && self.has_eta_continuation_candidate(graph, functions)
        {
            self.worklist.add_subterm(root);
        }

        graph.for_each_subterm(term, |child| self.collect_redexes(graph, child));

        if let TermKind::Fix(functions, _) | TermKind::Letk(functions, _) = graph[term].kind {
            for link in graph.function_links_slice(&functions) {
                if let Some(function) = graph.read_function_link(*link) {
                    self.collect_redexes(graph, graph[function].body);
                }
            }
        }
    }

    fn process_dead_bindings<'gc>(&mut self, graph: &mut Graph<'gc>) {
        while let Some(binding) = self.dead_bindings.get() {
            self.stats.dead_bindings_processed += 1;
            let expr_def = self.known_expr_defs[binding];
            if let Some(expr) = self.take_known_expr(binding) {
                if let Some(def) = expr_def {
                    self.worklist.add_subterm(def);
                }
                self.kill_free_vars_of_expr_link(graph, expr);
            }

            self.clear_known_function(graph, binding);
        }
    }

    fn clear_known_function<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        binding: BoundVar,
    ) -> Option<FunctionLink> {
        let link = self.known_functions[binding].take()?;
        self.known_function_count = self.known_function_count.saturating_sub(1);
        self.function_defs[binding] = None;
        if let Some(function) = graph.read_function_link(link) {
            self.function_owner_defs[function] = None;
            if let Some(return_cont) = graph[function].cont {
                self.return_cont_owners[return_cont] = None;
            }
        }
        graph.clear_function_link(link);
        Some(link)
    }

    fn take_known_expr(&mut self, binding: BoundVar) -> Option<Subexpr> {
        let expr = self.known_exprs[binding].take();
        if expr.is_some() {
            self.known_expr_count = self.known_expr_count.saturating_sub(1);
            self.known_expr_defs[binding] = None;
        }
        expr
    }

    fn known_literal_value<'gc>(
        &self,
        graph: &Graph<'gc>,
        binding: BoundVar,
    ) -> Option<Value<'gc>> {
        let expr = graph.read_expr_link(self.known_exprs[binding]?)?;
        let ExprKind::Literal(value) = graph[expr].kind else {
            return None;
        };
        Some(value)
    }

    fn kill_free_vars_of_expr_link<'gc>(&mut self, graph: &mut Graph<'gc>, expr: Subexpr) {
        let Some(expr_id) = graph.read_expr_link(expr) else {
            return;
        };
        let mut occurrences = std::mem::take(&mut self.old_occurrences);
        occurrences.clear();
        graph.push_free_vars_of_expr(expr_id, &mut occurrences);
        for occ in occurrences.iter().copied() {
            self.kill_occurrence(graph, occ);
        }
        occurrences.clear();
        self.old_occurrences = occurrences;
    }

    fn reduce_letval<'gc>(
        &mut self,
        ctx: Context<'gc>,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        binder: BoundVar,
        expr: Subexpr,
        body: Subterm,
    ) {
        if !graph.binder_is_dead(binder) {
            self.reduce_known_primcall(ctx, graph, binder, expr);
            return;
        }

        verbose_log!("gcps optimize: remove dead letval {binder}");
        self.stats.dead_letvals_removed += 1;
        if self.take_known_expr(binder).is_some() {
            self.kill_free_vars_of_expr_link(graph, expr);
        }
        self.replace_with_existing_body(graph, active_link, term, body);
        self.worklist.add_subterm(active_link);
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
    }

    fn reduce_known_primcall<'gc>(
        &mut self,
        ctx: Context<'gc>,
        graph: &mut Graph<'gc>,
        binder: BoundVar,
        expr: Subexpr,
    ) {
        let Some(expr_id) = graph.read_expr_link(expr) else {
            return;
        };
        let ExprKind::PrimCall(prim, vars) = graph[expr_id].kind else {
            return;
        };

        let arg_occurrences = graph.free_vars_slice(&vars).to_vec();
        let mut args = Vec::with_capacity(arg_occurrences.len());
        let mut arg_binders = Vec::with_capacity(arg_occurrences.len());
        for var in &arg_occurrences {
            let binder = graph.free_binder(*var);
            let Some(value) = self.known_literal_value(graph, binder) else {
                return;
            };
            arg_binders.push(binder);
            args.push(CpsAtom::Constant(value));
        }

        let Some(value) = folding_table(ctx).try_fold(ctx, prim, &args) else {
            return;
        };

        verbose_log!("gcps optimize: fold known primcall bound to {binder}");
        self.stats.known_prim_propagations += 1;
        for occ in arg_occurrences {
            self.kill_occurrence(graph, occ);
        }
        for binder in arg_binders {
            if graph.binder_is_dead(binder)
                && let Some(def) = self.known_expr_defs[binder]
            {
                self.worklist.add_subterm(def);
            }
        }
        graph[expr_id].kind = ExprKind::Literal(value);
        self.enqueue_occurrence_owners(graph, binder);
    }

    fn reduce_if<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        test: FreeVar,
        consequent: Subterm,
        alternative: Subterm,
    ) {
        if let Some(value) = self.known_literal_value(graph, graph.free_binder(test)) {
            let (taken, skipped) = if value != Value::new(false) {
                (consequent, alternative)
            } else {
                (alternative, consequent)
            };
            verbose_log!("gcps optimize: simplify constant branch tested by {test}");
            self.stats.constant_branches_simplified += 1;
            self.replace_if_with_branch(graph, queued_link, active_link, term, taken, skipped);
            return;
        }

        if self.direct_continue_branches_are_equivalent(graph, consequent, alternative) {
            verbose_log!("gcps optimize: simplify identical branch tested by {test}");
            self.stats.identical_branches_simplified += 1;
            self.replace_if_with_branch(
                graph,
                queued_link,
                active_link,
                term,
                consequent,
                alternative,
            );
        }
    }

    fn replace_if_with_branch<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        taken: Subterm,
        skipped: Subterm,
    ) {
        let parent_term = match graph.read_parent_link(graph[term].link) {
            Some(Parent::Term(parent)) => Some(parent),
            _ => None,
        };
        let same_replacement = graph.read_term_link(taken) == graph.read_term_link(skipped);
        self.replace_with_existing_body(graph, active_link, term, taken);
        if !same_replacement {
            self.kill_free_vars_of_term_subtree_link(graph, skipped);
        } else if skipped != taken {
            graph.clear_term_link(skipped);
        }
        if let Some(parent) = parent_term
            && let ActiveLinkStatus::ActiveSubterm(link) = graph.get_active_link_for(parent)
        {
            self.worklist.add_subterm(link);
        }
        self.worklist.add_subterm(active_link);
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
    }

    fn direct_continue_branches_are_equivalent<'gc>(
        &self,
        graph: &Graph<'gc>,
        left: Subterm,
        right: Subterm,
    ) -> bool {
        let Some(left) = graph.read_term_link(left) else {
            return false;
        };
        let Some(right) = graph.read_term_link(right) else {
            return false;
        };
        let TermKind::Continue(left_cont, left_args) = graph[left].kind else {
            return false;
        };
        let TermKind::Continue(right_cont, right_args) = graph[right].kind else {
            return false;
        };
        graph.free_binder(left_cont) == graph.free_binder(right_cont)
            && self.free_vars_have_same_binders(graph, &left_args, &right_args)
    }

    fn reduce_eta_continuations<'gc>(&mut self, graph: &mut Graph<'gc>, functions: FunctionLinks) {
        let links = graph.function_links_slice(&functions).to_vec();
        for link in links {
            let Some(function) = graph.read_function_link(link) else {
                continue;
            };
            let Some(target) = self.eta_continuation_target(graph, function) else {
                continue;
            };
            self.contract_eta_function(graph, link, function, target, true);
        }
    }

    fn has_eta_continuation_candidate<'gc>(
        &self,
        graph: &Graph<'gc>,
        functions: FunctionLinks,
    ) -> bool {
        graph
            .function_links_slice(&functions)
            .iter()
            .copied()
            .filter_map(|link| graph.read_function_link(link))
            .any(|function| self.eta_continuation_target(graph, function).is_some())
    }

    fn reduce_eta_functions<'gc>(&mut self, graph: &mut Graph<'gc>, functions: FunctionLinks) {
        let links = graph.function_links_slice(&functions).to_vec();
        for link in links {
            let Some(function) = graph.read_function_link(link) else {
                continue;
            };
            let Some(target) = self.eta_function_target(graph, function) else {
                continue;
            };
            self.contract_eta_function(graph, link, function, target, false);
        }
    }

    fn eta_continuation_target<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
    ) -> Option<BoundVar> {
        let function_data = graph[function];
        if function_data.cont.is_some()
            || function_data.variadic.is_some()
            || function_data.is_cold
            || function_data.is_noinline
            || function_data.is_reified
        {
            return None;
        }

        let body = graph.read_term_link(function_data.body)?;
        let TermKind::Continue(target, args) = graph[body].kind else {
            return None;
        };
        let target = graph.free_binder(target);
        if target == function_data.var
            || self.binder_has_local_definition(target)
            || !self.args_are_formals_in_order(graph, &args, &function_data.vars)
            || !self.all_occurrences_are_control_uses_available_at(graph, function_data.var, target)
        {
            return None;
        }

        Some(target)
    }

    fn eta_function_target<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
    ) -> Option<BoundVar> {
        let function_data = graph[function];
        let return_cont = function_data.cont?;
        if function_data.variadic.is_some()
            || function_data.is_cold
            || function_data.is_noinline
            || function_data.is_reified
        {
            return None;
        }

        let body = graph.read_term_link(function_data.body)?;
        let TermKind::App(target, args, cont) = graph[body].kind else {
            return None;
        };
        let target = graph.free_binder(target);
        if target == function_data.var
            || self.binder_has_local_definition(target)
            || graph.free_binder(cont) != return_cont
            || !self.args_are_formals_in_order(graph, &args, &function_data.vars)
            || !self.all_occurrences_are_callee_uses_available_at(graph, function_data.var, target)
        {
            return None;
        }

        Some(target)
    }

    fn binder_has_local_definition(&self, binder: BoundVar) -> bool {
        self.local_function_binders[binder]
            || self.function_defs[binder].is_some()
            || self.return_cont_owners[binder].is_some()
    }

    fn args_are_formals_in_order<'gc>(
        &self,
        graph: &Graph<'gc>,
        args: &super::graph::FreeVars,
        formals: &super::graph::BoundVars,
    ) -> bool {
        let args = graph.free_vars_slice(args);
        let formals = graph.bound_vars_slice(formals);
        args.len() == formals.len()
            && args
                .iter()
                .copied()
                .zip(formals.iter().copied())
                .all(|(arg, formal)| graph.free_binder(arg) == formal)
    }

    fn free_vars_have_same_binders<'gc>(
        &self,
        graph: &Graph<'gc>,
        left: &super::graph::FreeVars,
        right: &super::graph::FreeVars,
    ) -> bool {
        let left = graph.free_vars_slice(left);
        let right = graph.free_vars_slice(right);
        left.len() == right.len()
            && left
                .iter()
                .copied()
                .zip(right.iter().copied())
                .all(|(left, right)| graph.free_binder(left) == graph.free_binder(right))
    }

    fn all_occurrences_are_control_uses_available_at<'gc>(
        &self,
        graph: &Graph<'gc>,
        removed: BoundVar,
        replacement: BoundVar,
    ) -> bool {
        self.all_occurrences_match_available_use(graph, removed, replacement, |graph, term, occ| {
            matches!(
                graph[term].kind,
                TermKind::Continue(cont, _) if cont == occ
            ) || matches!(
                graph[term].kind,
                TermKind::App(_, _, cont) if cont == occ
            )
        })
    }

    fn all_occurrences_are_callee_uses_available_at<'gc>(
        &self,
        graph: &Graph<'gc>,
        removed: BoundVar,
        replacement: BoundVar,
    ) -> bool {
        self.all_occurrences_match_available_use(graph, removed, replacement, |graph, term, occ| {
            matches!(
                graph[term].kind,
                TermKind::App(callee, ..) if callee == occ
            )
        })
    }

    fn all_occurrences_match_available_use<'gc>(
        &self,
        graph: &Graph<'gc>,
        removed: BoundVar,
        replacement: BoundVar,
        mut occurrence_matches: impl FnMut(&Graph<'gc>, TermId, FreeVar) -> bool,
    ) -> bool {
        let mut allowed = true;
        graph.for_each_occurrence(removed, |occ| {
            if !allowed {
                return;
            }
            let owner = graph.free_owner(occ);
            let Some(term) = graph.read_term_link(owner) else {
                allowed = false;
                return;
            };
            allowed = !self.term_is_inside_any_function(graph, term)
                && occurrence_matches(graph, term, occ)
                && self.binder_is_available_at_term(graph, term, replacement);
        });
        allowed
    }

    fn contract_eta_function<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        link: FunctionLink,
        function: FunctionId,
        target: BoundVar,
        is_continuation: bool,
    ) {
        let binding = graph[function].var;
        verbose_log!("gcps optimize: eta-contract {binding} to {target}");

        graph.subst_var_for_binders(target, binding);
        self.kill_direct_free_vars_of_term_link(graph, graph[function].body);
        if self.known_functions[binding] == Some(link) {
            self.clear_known_function(graph, binding);
        } else {
            graph.clear_function_link(link);
        }
        self.enqueue_occurrence_owners(graph, target);
        if is_continuation {
            self.stats.eta_continuations += 1;
        } else {
            self.stats.eta_functions += 1;
        }
    }

    fn kill_direct_free_vars_of_term_link<'gc>(&mut self, graph: &mut Graph<'gc>, link: Subterm) {
        let Some(term) = graph.read_term_link(link) else {
            return;
        };
        let mut occurrences = std::mem::take(&mut self.old_occurrences);
        occurrences.clear();
        graph.push_direct_free_vars_of_term(term, &mut occurrences);
        for occ in occurrences.iter().copied() {
            self.kill_occurrence(graph, occ);
        }
        occurrences.clear();
        self.old_occurrences = occurrences;
    }

    fn kill_free_vars_of_term_subtree_link<'gc>(&mut self, graph: &mut Graph<'gc>, link: Subterm) {
        let Some(term) = graph.read_term_link(link) else {
            return;
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                self.take_known_expr(binding);
                self.kill_free_vars_of_expr_link(graph, expr);
                self.kill_free_vars_of_term_subtree_link(graph, body);
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let function_bodies = graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| graph.read_function_link(link))
                    .map(|function| {
                        let binding = graph[function].var;
                        let body = graph[function].body;
                        (binding, body)
                    })
                    .collect::<Vec<_>>();
                for (binding, body) in function_bodies {
                    self.clear_known_function(graph, binding);
                    self.kill_free_vars_of_term_subtree_link(graph, body);
                }
                self.kill_free_vars_of_term_subtree_link(graph, body);
            }
            TermKind::If(_, consequent, alternative) => {
                self.kill_direct_free_vars_of_term_link(graph, link);
                self.kill_free_vars_of_term_subtree_link(graph, consequent);
                self.kill_free_vars_of_term_subtree_link(graph, alternative);
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {
                self.kill_direct_free_vars_of_term_link(graph, link);
            }
        }
    }

    fn enqueue_occurrence_owners<'gc>(&mut self, graph: &Graph<'gc>, binder: BoundVar) {
        for occ in graph.collect_occurrences(binder) {
            self.worklist.add_subterm(graph.free_owner(occ));
        }
    }

    fn classify_function<'gc>(
        &self,
        graph: &Graph<'gc>,
        callee: FreeVar,
    ) -> Option<(BoundVar, FunctionLink, FunctionId)> {
        let binder = graph.free_binder(callee);
        let link = self.known_functions[binder]?;
        let function = graph.read_function_link(link)?;
        Some((binder, link, function))
    }

    fn reduce_call<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        callee: FreeVar,
        args: super::graph::FreeVars,
        cont: ContVar,
    ) {
        let Some((binder, _link, function)) = self.classify_function(graph, callee) else {
            return;
        };
        let function_data = graph[function];

        if function_data.cont.is_none()
            || function_data.is_rec
            || function_data.variadic.is_some()
            || !graph.binder_is_singleton_or_dead(binder)
        {
            return;
        }

        if !self.arity_matches(graph, function, &args) {
            return;
        }

        verbose_log!("gcps optimize: inline singleton call to {binder}");
        self.stats.singleton_calls_inlined += 1;
        self.beta_reduce_call(graph, function, &args, Some(cont));
        self.replace_with_existing_body(graph, active_link, term, function_data.body);
        self.kill_binding(graph, callee);
        self.worklist.add_occurrences(
            graph,
            graph
                .free_vars_slice(&args)
                .iter()
                .copied()
                .collect::<Vec<_>>(),
        );
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
    }

    fn reduce_continue<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        cont: ContVar,
        args: super::graph::FreeVars,
    ) {
        let Some((binder, _link, function)) = self.classify_function(graph, cont) else {
            return;
        };
        let function_data = graph[function];

        if function_data.cont.is_some()
            || function_data.is_rec
            || function_data.variadic.is_some()
            || function_data.is_noinline
            || function_data.is_cold
            || !graph.binder_is_singleton_or_dead(binder)
        {
            return;
        }

        if !self.arity_matches(graph, function, &args) {
            return;
        }

        if !self.continuation_body_is_simple_inline(graph, function) {
            verbose_log!("gcps optimize: skip complex singleton continuation {binder}");
            return;
        }

        verbose_log!("gcps optimize: inline singleton continuation {binder}");
        self.stats.singleton_continuations_inlined += 1;
        self.beta_reduce_call(graph, function, &args, None);
        self.replace_with_existing_body(graph, active_link, term, function_data.body);
        self.kill_binding(graph, cont);
        self.enqueue_known_occurrences(graph, function, &args);
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
    }

    fn arity_matches<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
        args: &super::graph::FreeVars,
    ) -> bool {
        let function = graph[function];
        let fixed = graph.bound_vars_slice(&function.vars).len();
        let actual = graph.free_vars_slice(args).len();
        match function.variadic {
            Some(_) => actual >= fixed,
            None => actual == fixed,
        }
    }

    fn beta_reduce_call<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        function: FunctionId,
        args: &super::graph::FreeVars,
        cont: Option<ContVar>,
    ) {
        let function_data = graph[function];
        let formals = graph.bound_vars_slice(&function_data.vars).to_vec();
        let actuals = graph.free_vars_slice(args).to_vec();
        for (actual, formal) in actuals.into_iter().zip(formals) {
            graph.subst_var_for_bound(actual, formal);
        }

        if let (Some(actual), Some(formal)) = (cont, function_data.cont) {
            self.collect_redexes_using_fn_ret_cont(graph, formal);
            graph.subst_var_for_bound(actual, formal);
        }
    }

    fn replace_with_existing_body<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        active_link: Subterm,
        old_term: TermId,
        replacement_link: Subterm,
    ) {
        let Some(replacement) = graph.read_term_link(replacement_link) else {
            return;
        };
        self.replace_term_with(graph, active_link, old_term, replacement);
        if replacement_link != active_link {
            graph.clear_term_link(replacement_link);
        }
    }

    fn replace_term_with<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        active_link: Subterm,
        old_term: TermId,
        replacement: TermId,
    ) {
        verbose_log!(
            "gcps optimize: replace {} with {}",
            graph.pretty_term(old_term),
            graph.pretty_term(replacement)
        );

        let mut old_occurrences = std::mem::take(&mut self.old_occurrences);
        let mut new_occurrences = std::mem::take(&mut self.new_occurrences);
        old_occurrences.clear();
        new_occurrences.clear();
        self.occurrence_set.clear();
        graph.push_direct_free_vars_of_term(old_term, &mut old_occurrences);
        graph.push_direct_free_vars_of_term(replacement, &mut new_occurrences);
        for occ in new_occurrences.iter().copied() {
            self.occurrence_set.insert(occ);
        }

        graph.set_term_link(active_link, replacement);
        for occ in old_occurrences.iter().copied() {
            if !self.occurrence_set.contains(occ) {
                self.kill_occurrence(graph, occ);
            }
        }
        for occ in new_occurrences.iter().copied() {
            graph.set_free_owner(occ, active_link);
        }
        old_occurrences.clear();
        new_occurrences.clear();
        self.old_occurrences = old_occurrences;
        self.new_occurrences = new_occurrences;

        match graph.read_parent_link(graph[old_term].link) {
            Some(parent) => graph.set_parent_link(graph[replacement].link, parent),
            None => graph.clear_parent_link(graph[replacement].link),
        }
    }

    fn replace_kind<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        active_link: Subterm,
        term: TermId,
        kind: TermKind,
    ) {
        let mut old_occurrences = std::mem::take(&mut self.old_occurrences);
        let mut new_occurrences = std::mem::take(&mut self.new_occurrences);
        old_occurrences.clear();
        new_occurrences.clear();
        self.occurrence_set.clear();
        graph.push_direct_free_vars_of_term(term, &mut old_occurrences);
        graph[term].kind = kind;
        graph.push_direct_free_vars_of_term(term, &mut new_occurrences);
        for occ in new_occurrences.iter().copied() {
            self.occurrence_set.insert(occ);
        }
        for occ in old_occurrences.iter().copied() {
            if !self.occurrence_set.contains(occ) {
                self.kill_occurrence(graph, occ);
            }
        }
        for occ in new_occurrences.iter().copied() {
            graph.set_free_owner(occ, active_link);
        }
        old_occurrences.clear();
        new_occurrences.clear();
        self.old_occurrences = old_occurrences;
        self.new_occurrences = new_occurrences;
    }

    fn kill_occurrence<'gc>(&mut self, graph: &mut Graph<'gc>, occ: FreeVar) {
        let binder = graph.free_binder(occ);
        if graph.free_occ_is_singleton_verified(occ) {
            verbose_log!("gcps optimize: kill singleton occurrence {occ} of {binder}");
            graph.set_bound_var_occ(binder, None);
            self.dead_bindings.add(binder);
        } else {
            let next = graph.free_occ_next(occ);
            graph.free_occ_remove(occ);
            if graph.bound_var_occ(binder) == Some(occ) {
                graph.set_bound_var_occ(binder, Some(next));
            }
        }
    }

    fn kill_binding<'gc>(&mut self, graph: &mut Graph<'gc>, occ: FreeVar) {
        let binding = graph.free_binder(occ);
        verbose_log!("gcps optimize: kill binding {binding}");
        graph.set_bound_var_occ(binding, None);
        self.dead_bindings.add(binding);
        if let Some(link) = self.known_functions[binding] {
            graph.clear_function_link(link);
        }
    }

    fn enqueue_known_occurrences<'gc>(
        &mut self,
        graph: &Graph<'gc>,
        function: FunctionId,
        args: &super::graph::FreeVars,
    ) {
        let formals = graph.bound_vars_slice(&graph[function].vars).to_vec();
        self.worklist.add_known_occurrences(
            graph,
            graph.free_vars_slice(args).iter().copied(),
            formals,
            |binder| self.known_exprs[binder].is_some(),
        );
    }

    fn collect_redexes_using_fn_ret_cont<'gc>(&mut self, graph: &Graph<'gc>, oldret: BoundVar) {
        graph.for_each_occurrence(oldret, |occ| {
            let owner = graph.free_owner(occ);
            let Some(term) = graph.read_term_link(owner) else {
                return;
            };
            let TermKind::App(callee, ..) = graph[term].kind else {
                return;
            };
            let callee_binder = graph.free_binder(callee);
            if let Some(fn_def) = self.function_defs[callee_binder] {
                self.worklist.add_subterm(fn_def);
            }
        });
    }

    fn continuation_body_is_simple_inline<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
    ) -> bool {
        let function_data = graph[function];
        let mut value_scope = EntitySet::new();
        value_scope.extend(graph.bound_vars_slice(&function_data.vars).iter().copied());
        if let Some(variadic) = function_data.variadic {
            value_scope.insert(variadic);
        }

        self.term_is_simple_continuation_inline_body(graph, function_data.body, &mut value_scope)
    }

    fn term_is_simple_continuation_inline_body<'gc>(
        &self,
        graph: &Graph<'gc>,
        link: Subterm,
        value_scope: &mut EntitySet<BoundVar>,
    ) -> bool {
        let Some(term) = graph.read_term_link(link) else {
            return false;
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                if let Some(expr_id) = graph.read_expr_link(expr) {
                    if !self.expr_free_occurrences_are_allowed(graph, expr_id, value_scope) {
                        return false;
                    }
                }
                value_scope.insert(binding);
                self.term_is_simple_continuation_inline_body(graph, body, value_scope)
            }
            TermKind::If(_, then_branch, else_branch) => {
                let mut then_scope = value_scope.clone();
                let mut else_scope = value_scope.clone();
                self.term_is_simple_continuation_inline_body(graph, then_branch, &mut then_scope)
                    && self.term_is_simple_continuation_inline_body(
                        graph,
                        else_branch,
                        &mut else_scope,
                    )
            }
            TermKind::Continue(cont, vars) => {
                self.continuation_target_is_inline_stable(graph, cont)
                    && self.free_occurrences_are_allowed(
                        graph,
                        graph.free_vars_slice(&vars).iter().copied(),
                        value_scope,
                    )
            }
            TermKind::App(func, vars, cont) => {
                self.continuation_target_is_inline_stable(graph, cont)
                    && self.free_occurrences_are_allowed(
                        graph,
                        graph.free_vars_slice(&vars).iter().copied(),
                        value_scope,
                    )
                    && value_scope.contains(graph.free_binder(func))
            }
            TermKind::Raise(_, vars) => self.free_occurrences_are_allowed(
                graph,
                graph.free_vars_slice(&vars).iter().copied(),
                value_scope,
            ),
            TermKind::Fix(..) | TermKind::Letk(..) => false,
        }
    }

    fn continuation_target_is_inline_stable<'gc>(
        &self,
        graph: &Graph<'gc>,
        target: ContVar,
    ) -> bool {
        let binder = graph.free_binder(target);
        self.known_functions[binder].is_none() && self.return_cont_owners[binder].is_none()
    }

    fn try_contify_fix<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        active_link: Subterm,
        term: TermId,
        functions: super::graph::FunctionLinks,
        body: Subterm,
        mode: GcpsContifyMode,
    ) {
        let live = self.live_function_links(graph, &functions);
        if live.is_empty() {
            self.replace_with_existing_body(graph, active_link, term, body);
            return;
        }

        let candidate = match mode {
            GcpsContifyMode::Off => None,
            GcpsContifyMode::Scc => {
                scc_contify::find_candidate(self, graph, active_link, term, &live, body)
            }
            GcpsContifyMode::Dom => {
                dom_contify::find_candidate(self, graph, active_link, term, &live, body)
            }
            GcpsContifyMode::DomThenScc => {
                dom_contify::find_candidate(self, graph, active_link, term, &live, body).or_else(
                    || scc_contify::find_candidate(self, graph, active_link, term, &live, body),
                )
            }
        };

        let Some(candidate) = candidate else {
            return;
        };

        self.contify_candidate(graph, active_link, term, &live, body, candidate);
    }

    pub(super) fn live_function_links<'gc>(
        &self,
        graph: &Graph<'gc>,
        functions: &super::graph::FunctionLinks,
    ) -> Vec<(FunctionLink, FunctionId)> {
        graph
            .function_links_slice(functions)
            .iter()
            .copied()
            .filter_map(|link| {
                graph
                    .read_function_link(link)
                    .map(|function| (link, function))
            })
            .collect()
    }

    fn contify_candidate<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        active_link: Subterm,
        term: TermId,
        live: &[(FunctionLink, FunctionId)],
        body: Subterm,
        candidate: ContifyCandidate,
    ) {
        let mut contified_links = Vec::new();
        let mut contified_functions = Vec::new();
        let mut untouched_links = Vec::new();

        for (link, function) in live.iter().copied() {
            if candidate.binders.contains(graph[function].var) {
                contified_links.push(link);
                contified_functions.push(function);
            } else {
                untouched_links.push(link);
            }
        }

        if contified_functions.is_empty() {
            return;
        }

        let dump_index = next_contify_dump_index(candidate.source);
        if let Some(index) = dump_index {
            self.dump_contification(
                graph,
                index,
                "before",
                active_link,
                candidate.site,
                body,
                candidate.return_cont,
                candidate.source,
                &candidate.binders,
                &contified_functions,
                &untouched_links,
            );
        }

        let mut site = candidate.site;
        let contified = graph.new_function_links(contified_links.iter().copied());
        if untouched_links.is_empty() {
            self.replace_with_existing_body(graph, active_link, term, body);
            if site == body {
                site = active_link;
            }
        } else {
            let untouched = graph.new_function_links(untouched_links.iter().copied());
            graph[term].kind = TermKind::Fix(untouched, body);
            let subterms = graph.subterms_of(term);
            graph.backpatch_subterms(term, &subterms);
        }

        verbose_log!(
            "gcps optimize: contify {} functions at {}",
            contified_functions.len(),
            site
        );
        self.stats.contifications += 1;
        self.stats.contified_functions += contified_functions.len();
        match candidate.source {
            ContifySource::Scc => {
                self.stats.scc_contifications += 1;
                self.stats.scc_contified_functions += contified_functions.len();
            }
            ContifySource::Dominator => {
                self.stats.dom_contifications += 1;
                self.stats.dom_contified_functions += contified_functions.len();
            }
        }

        for function in &contified_functions {
            let Some(old_ret) = graph[*function].cont else {
                continue;
            };
            self.collect_redexes_using_fn_ret_cont(graph, old_ret);
            graph.subst_var_for_binders(candidate.return_cont, old_ret);
            graph[*function].cont = None;
            graph[*function].is_rec = false;
            graph[*function].is_cold = false;
            graph[*function].is_noinline = false;
            graph[*function].is_reified = false;
            self.return_cont_owners[old_ret] = None;
        }

        let wrapper = self.wrap_link_with_letk(graph, site, contified);
        for function in contified_functions.iter().copied() {
            let binder = graph[function].var;
            self.function_defs[binder] = Some(site);
            self.function_owner_defs[function] = Some(site);
            graph.backpatch_function(function);
        }

        self.transform_apps_to_continues(graph, site, &candidate.binders);
        self.collect_redexes(graph, site);
        self.worklist.add_subterm(site);
        if !untouched_links.is_empty() {
            self.worklist.add_subterm(active_link);
        }
        if let Some(index) = dump_index {
            self.dump_contification(
                graph,
                index,
                "after",
                active_link,
                site,
                body,
                candidate.return_cont,
                candidate.source,
                &candidate.binders,
                &contified_functions,
                &untouched_links,
            );
        }
        verbose_log!(
            "gcps optimize: inserted contification wrapper {}",
            graph.pretty_term(wrapper)
        );
    }

    fn dump_contification<'gc>(
        &self,
        graph: &Graph<'gc>,
        index: usize,
        phase: &str,
        active_link: Subterm,
        site: Subterm,
        body: Subterm,
        return_cont: BoundVar,
        source: ContifySource,
        binders: &EntitySet<BoundVar>,
        contified_functions: &[FunctionId],
        untouched_links: &[FunctionLink],
    ) {
        let stats = graph.stats();
        let active_term = graph
            .read_term_link(active_link)
            .map(|term| graph.pretty_term(term))
            .unwrap_or_else(|| format!("{active_link}:<dead>"));
        let site_term = graph
            .read_term_link(site)
            .map(|term| graph.pretty_term(term))
            .unwrap_or_else(|| format!("{site}:<dead>"));
        let body_term = graph
            .read_term_link(body)
            .map(|term| graph.pretty_term(term))
            .unwrap_or_else(|| format!("{body}:<dead>"));
        let contified = contified_functions
            .iter()
            .copied()
            .map(|function| format!("{} {}", function, graph[function].var))
            .collect::<Vec<_>>()
            .join(", ");
        let candidate_binders = contified_functions
            .iter()
            .copied()
            .filter_map(|function| {
                let binder = graph[function].var;
                binders
                    .contains(binder)
                    .then(|| format!("{} {}", function, binder))
            })
            .collect::<Vec<_>>()
            .join(", ");
        let untouched = untouched_links
            .iter()
            .copied()
            .map(|link| match graph.read_function_link(link) {
                Some(function) => format!("{} -> {} {}", link, function, graph[function].var),
                None => format!("{link}:<dead>"),
            })
            .collect::<Vec<_>>()
            .join(", ");

        let dump = format!(
            "\n=== gcps contify dump #{index} {phase} ===\n\
             source: {source:?}\n\
             active_link: {active_link}\n\
             insertion_site: {site}\n\
             original_body_link: {body}\n\
             return_cont: {return_cont}\n\
             contified_functions: [{contified}]\n\
             candidate_binders: [{candidate_binders}]\n\
             untouched_functions: [{untouched}]\n\
             stats: terms={} functions={} free_occurrences={} bound_vars={} term_links={}\n\
             -- active subtree --\n{active_term}\n\
             -- insertion site subtree --\n{site_term}\n\
             -- original fix body subtree --\n{body_term}\n\
             === end gcps contify dump #{index} {phase} ===\n",
            stats.terms,
            stats.functions,
            stats.free_occurrences,
            stats.bound_vars,
            stats.term_links
        );
        emit_contification_dump(index, phase, &dump);
    }

    fn wrap_link_with_letk<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        link: Subterm,
        functions: super::graph::FunctionLinks,
    ) -> TermId {
        let old_term = graph
            .read_term_link(link)
            .expect("contification insertion site must be live");
        let old_parent = graph.read_parent_link(graph[old_term].link);
        let body = graph.new_term_link(Some(old_term));
        let wrapper_link = graph.new_parent_link(old_parent);
        let wrapper = graph.new_term(
            wrapper_link,
            TermKind::Letk(functions, body),
            graph[old_term].source,
        );
        graph.set_term_link(link, wrapper);
        graph.set_parent_link(graph[old_term].link, Parent::Term(wrapper));
        wrapper
    }

    fn transform_apps_to_continues<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        link: Subterm,
        binders: &EntitySet<BoundVar>,
    ) {
        let Some(term) = graph.read_term_link(link) else {
            return;
        };

        match graph[term].kind {
            TermKind::LetVal(_, body) => self.transform_apps_to_continues(graph, body, binders),
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let function_bodies = graph
                    .function_links_slice(&functions)
                    .iter()
                    .copied()
                    .filter_map(|link| graph.read_function_link(link))
                    .map(|function| graph[function].body)
                    .collect::<Vec<_>>();
                for body in function_bodies {
                    self.transform_apps_to_continues(graph, body, binders);
                }
                self.transform_apps_to_continues(graph, body, binders);
            }
            TermKind::If(_, then_branch, else_branch) => {
                self.transform_apps_to_continues(graph, then_branch, binders);
                self.transform_apps_to_continues(graph, else_branch, binders);
            }
            TermKind::App(callee, args, old_cont)
                if binders.contains(graph.free_binder(callee)) =>
            {
                let _ = old_cont;
                self.replace_kind(graph, link, term, TermKind::Continue(callee, args));
                self.worklist.add_subterm(link);
            }
            TermKind::App(..) | TermKind::Continue(..) | TermKind::Raise(..) => {}
        }
    }

    pub(super) fn binder_is_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        term: TermId,
        binder: BoundVar,
    ) -> bool {
        if let Some(owner) = self.return_cont_owners[binder] {
            return self.term_is_inside_function(graph, term, owner);
        }

        if let Some(expr) = self.known_exprs[binder] {
            let Some(expr) = graph.read_expr_link(expr) else {
                return false;
            };
            let Some(Parent::Term(def_term)) = graph.read_parent_link(graph[expr].link) else {
                return false;
            };
            let TermKind::LetVal((def_binder, _), body) = graph[def_term].kind else {
                return false;
            };
            if def_binder != binder {
                return false;
            }
            let Some(body_term) = graph.read_term_link(body) else {
                return false;
            };
            return self.term_is_inside_term_scope(graph, term, body_term);
        }

        if let Some(def) = self.function_defs[binder] {
            let Some(scope_term) = graph.read_term_link(def) else {
                return false;
            };
            return self.term_is_inside_term_scope(graph, term, scope_term);
        }

        true
    }

    pub(super) fn function_uses_only_available_scope_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
        group_binders: &EntitySet<BoundVar>,
        site_term: TermId,
    ) -> bool {
        let data = graph[function];
        let mut local = group_binders.clone();
        local.extend(graph.bound_vars_slice(&data.vars).iter().copied());
        if let Some(cont) = data.cont {
            local.insert(cont);
        }
        if let Some(variadic) = data.variadic {
            local.insert(variadic);
        }
        self.term_uses_only_available_scope_at_term(graph, data.body, &local, site_term)
    }

    fn term_uses_only_available_scope_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        link: Subterm,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
    ) -> bool {
        let Some(term) = graph.read_term_link(link) else {
            return true;
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                if let Some(expr_id) = graph.read_expr_link(expr) {
                    if !self.expr_free_occurrences_are_available_at_term(
                        graph, expr_id, local, site_term,
                    ) {
                        return false;
                    }
                }
                let mut body_local = local.clone();
                body_local.insert(binding);
                self.term_uses_only_available_scope_at_term(graph, body, &body_local, site_term)
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let mut nested_functions = Vec::new();
                let mut body_local = local.clone();
                for link in graph.function_links_slice(&functions).iter().copied() {
                    let Some(function) = graph.read_function_link(link) else {
                        continue;
                    };
                    nested_functions.push(function);
                    body_local.insert(graph[function].var);
                }
                for function in nested_functions {
                    let data = graph[function];
                    let mut function_local = body_local.clone();
                    function_local.extend(graph.bound_vars_slice(&data.vars).iter().copied());
                    if let Some(cont) = data.cont {
                        function_local.insert(cont);
                    }
                    if let Some(variadic) = data.variadic {
                        function_local.insert(variadic);
                    }
                    if !self.term_uses_only_available_scope_at_term(
                        graph,
                        data.body,
                        &function_local,
                        site_term,
                    ) {
                        return false;
                    }
                }
                self.term_uses_only_available_scope_at_term(graph, body, &body_local, site_term)
            }
            TermKind::If(test, then_branch, else_branch) => {
                self.free_occurrences_are_available_at_term(graph, [test], local, site_term)
                    && self.term_uses_only_available_scope_at_term(
                        graph,
                        then_branch,
                        local,
                        site_term,
                    )
                    && self.term_uses_only_available_scope_at_term(
                        graph,
                        else_branch,
                        local,
                        site_term,
                    )
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {
                self.direct_free_occurrences_are_available_at_term(graph, term, local, site_term)
            }
        }
    }

    fn expr_free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        expr: super::graph::ExprId,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
    ) -> bool {
        let mut available = true;
        graph.for_each_free_var_of_expr(expr, |var| {
            let binder = graph.free_binder(var);
            available &= local.contains(binder)
                || self.binder_is_available_at_term(graph, site_term, binder);
        });
        available
    }

    fn direct_free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        term: TermId,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
    ) -> bool {
        let mut available = true;
        graph.for_each_direct_free_var_of_term(term, |var| {
            let binder = graph.free_binder(var);
            available &= local.contains(binder)
                || self.binder_is_available_at_term(graph, site_term, binder);
        });
        available
    }

    fn free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        vars: impl IntoIterator<Item = FreeVar>,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
    ) -> bool {
        vars.into_iter().all(|var| {
            let binder = graph.free_binder(var);
            local.contains(binder) || self.binder_is_available_at_term(graph, site_term, binder)
        })
    }

    fn term_is_inside_function<'gc>(
        &self,
        graph: &Graph<'gc>,
        mut term: TermId,
        owner: FunctionId,
    ) -> bool {
        loop {
            let Some(parent) = graph.read_parent_link(graph[term].link) else {
                return false;
            };

            match parent {
                Parent::Func(function) => return function == owner,
                Parent::Term(parent) => term = parent,
            }
        }
    }

    fn term_is_inside_any_function<'gc>(&self, graph: &Graph<'gc>, mut term: TermId) -> bool {
        loop {
            let Some(parent) = graph.read_parent_link(graph[term].link) else {
                return false;
            };

            match parent {
                Parent::Func(_) => return true,
                Parent::Term(parent) => term = parent,
            }
        }
    }

    fn term_is_inside_term_scope<'gc>(
        &self,
        graph: &Graph<'gc>,
        mut term: TermId,
        scope: TermId,
    ) -> bool {
        if term == scope {
            return true;
        }

        loop {
            let Some(parent) = graph.read_parent_link(graph[term].link) else {
                return false;
            };

            match parent {
                Parent::Term(parent) => {
                    if parent == scope {
                        return true;
                    }
                    term = parent;
                }
                Parent::Func(function) => {
                    let Some(def) = self.function_owner_defs[function] else {
                        return false;
                    };
                    let Some(def_term) = graph.read_term_link(def) else {
                        return false;
                    };
                    if def_term == scope {
                        return true;
                    }
                    term = def_term;
                }
            }
        }
    }

    fn unique_outer_cont_for_function<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
        old_ret: BoundVar,
    ) -> Option<BoundVar> {
        let binder = graph[function].var;
        let mut found = None;
        let mut valid = true;

        graph.for_each_occurrence(binder, |occ| {
            if !valid {
                return;
            }
            let owner = graph.free_owner(occ);
            let Some(term) = graph.read_term_link(owner) else {
                valid = false;
                return;
            };
            let TermKind::App(callee, _args, cont) = graph[term].kind else {
                valid = false;
                return;
            };
            if graph.free_binder(callee) != binder {
                valid = false;
                return;
            }
            let cont_binder = graph.free_binder(cont);
            if cont_binder == old_ret {
                return;
            }
            match found {
                None => found = Some(cont_binder),
                Some(existing) if existing == cont_binder => {}
                Some(_) => valid = false,
            }
        });

        valid.then_some(found).flatten()
    }

    fn function_uses_only_contifiable_scope<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
        group_binders: &EntitySet<BoundVar>,
    ) -> bool {
        let data = graph[function];
        let mut allowed = group_binders.clone();
        allowed.extend(graph.bound_vars_slice(&data.vars).iter().copied());
        if let Some(cont) = data.cont {
            allowed.insert(cont);
        }
        if let Some(variadic) = data.variadic {
            allowed.insert(variadic);
        }
        self.term_uses_only_allowed_scope(graph, data.body, &allowed)
    }

    fn term_uses_only_allowed_scope<'gc>(
        &self,
        graph: &Graph<'gc>,
        link: Subterm,
        allowed: &EntitySet<BoundVar>,
    ) -> bool {
        let Some(term) = graph.read_term_link(link) else {
            return true;
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                if let Some(expr_id) = graph.read_expr_link(expr) {
                    if !self.expr_free_occurrences_are_allowed(graph, expr_id, allowed) {
                        return false;
                    }
                }
                let mut body_allowed = allowed.clone();
                body_allowed.insert(binding);
                self.term_uses_only_allowed_scope(graph, body, &body_allowed)
            }
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                let mut nested = Vec::new();
                let mut body_allowed = allowed.clone();
                for link in graph.function_links_slice(&functions).iter().copied() {
                    let Some(function) = graph.read_function_link(link) else {
                        continue;
                    };
                    nested.push(function);
                    body_allowed.insert(graph[function].var);
                }
                for function in &nested {
                    let data = graph[*function];
                    let mut function_allowed = body_allowed.clone();
                    function_allowed.extend(graph.bound_vars_slice(&data.vars).iter().copied());
                    if let Some(cont) = data.cont {
                        function_allowed.insert(cont);
                    }
                    if let Some(variadic) = data.variadic {
                        function_allowed.insert(variadic);
                    }
                    if !self.term_uses_only_allowed_scope(graph, data.body, &function_allowed) {
                        return false;
                    }
                }
                self.term_uses_only_allowed_scope(graph, body, &body_allowed)
            }
            TermKind::If(test, then_branch, else_branch) => {
                self.free_occurrences_are_allowed(graph, [test], allowed)
                    && self.term_uses_only_allowed_scope(graph, then_branch, allowed)
                    && self.term_uses_only_allowed_scope(graph, else_branch, allowed)
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {
                self.direct_free_occurrences_are_allowed(graph, term, allowed)
            }
        }
    }

    fn expr_free_occurrences_are_allowed<'gc>(
        &self,
        graph: &Graph<'gc>,
        expr: super::graph::ExprId,
        allowed: &EntitySet<BoundVar>,
    ) -> bool {
        let mut allowed_all = true;
        graph.for_each_free_var_of_expr(expr, |var| {
            allowed_all &= allowed.contains(graph.free_binder(var));
        });
        allowed_all
    }

    fn direct_free_occurrences_are_allowed<'gc>(
        &self,
        graph: &Graph<'gc>,
        term: TermId,
        allowed: &EntitySet<BoundVar>,
    ) -> bool {
        let mut allowed_all = true;
        graph.for_each_direct_free_var_of_term(term, |var| {
            allowed_all &= allowed.contains(graph.free_binder(var));
        });
        allowed_all
    }

    fn free_occurrences_are_allowed<'gc>(
        &self,
        graph: &Graph<'gc>,
        vars: impl IntoIterator<Item = FreeVar>,
        allowed: &EntitySet<BoundVar>,
    ) -> bool {
        vars.into_iter()
            .all(|var| allowed.contains(graph.free_binder(var)))
    }

    fn expr_free_vars_contain_any<'gc>(
        &self,
        graph: &Graph<'gc>,
        expr: super::graph::ExprId,
        binders: &EntitySet<BoundVar>,
    ) -> bool {
        let mut contains_any = false;
        graph.for_each_free_var_of_expr(expr, |var| {
            contains_any |= binders.contains(graph.free_binder(var));
        });
        contains_any
    }
}

fn next_contify_dump_index(source: ContifySource) -> Option<usize> {
    if !contify_dump_enabled_for(source) {
        return None;
    }

    let index = CONTIFY_DUMP_COUNT.fetch_add(1, Ordering::Relaxed) + 1;
    let limit = env::var("CAPY_GCPS_DUMP_LIMIT")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(usize::MAX);
    (index <= limit).then_some(index)
}

fn contify_dump_enabled_for(source: ContifySource) -> bool {
    let Some(value) = env::var("CAPY_GCPS_DUMP_CONTIFY").ok() else {
        return false;
    };

    match value.to_ascii_lowercase().as_str() {
        "" | "0" | "off" | "none" | "false" => false,
        "1" | "on" | "true" | "all" => true,
        "scc" | "legacy" => source == ContifySource::Scc,
        "dom" | "dominator" | "dominators" => source == ContifySource::Dominator,
        _ => true,
    }
}

fn emit_contification_dump(index: usize, phase: &str, dump: &str) {
    let Some(dir) = env::var("CAPY_GCPS_DUMP_DIR").ok() else {
        eprint!("{dump}");
        return;
    };

    let dir = Path::new(&dir);
    if let Err(error) = fs::create_dir_all(dir) {
        eprintln!(
            "gcps contify dump #{index} {phase}: failed to create {}: {error}",
            dir.display()
        );
        eprint!("{dump}");
        return;
    }

    let path = dir.join(format!("gcps-contify-{index:06}-{phase}.txt"));
    match fs::write(&path, dump) {
        Ok(()) => {
            eprintln!(
                "gcps contify dump #{index} {phase}: wrote {}",
                path.display()
            );
        }
        Err(error) => {
            eprintln!(
                "gcps contify dump #{index} {phase}: failed to write {}: {error}",
                path.display()
            );
            eprint!("{dump}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::term::{Atom, BranchHint, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Scheme,
            value::{Symbol, Value},
        },
    };
    use std::cell::Cell;

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, ctx.intern(name))
    }

    fn prim<'gc>(ctx: Context<'gc>, name: &str) -> Value<'gc> {
        Symbol::from_str(ctx, name).into()
    }

    fn fixed_cont<'gc>(
        ctx: Context<'gc>,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: TermRef<'gc>,
        noinline: bool,
    ) -> ContRef<'gc> {
        Gc::new(
            *ctx,
            Cont {
                name: Value::new(false),
                binding,
                args: Array::from_slice(*ctx, args),
                variadic: None,
                body: Lock::new(body),
                source: Value::new(false),
                free_vars: Lock::new(None),
                reified: Cell::new(false),
                cold: false,
                noinline,
                meta: Value::new(false),
            },
        )
    }

    fn fixed_func<'gc>(
        ctx: Context<'gc>,
        binding: LVarRef<'gc>,
        return_cont: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: TermRef<'gc>,
    ) -> FuncRef<'gc> {
        Gc::new(
            *ctx,
            Func {
                name: Value::new(false),
                source: Value::new(false),
                binding,
                return_cont,
                args: Array::from_slice(*ctx, args),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        )
    }

    fn optimize_graph_with_mode<'gc>(
        ctx: Context<'gc>,
        graph: &mut Graph<'gc>,
        root: Subterm,
        mode: GcpsContifyMode,
        gas: usize,
    ) -> OptimizationStats {
        let mut state = OptimizerState::new();
        state.collect_redexes(graph, root);
        state.run(ctx, graph, gas, mode);
        state.stats
    }

    #[test]
    fn dead_letval_is_removed() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let dead = lvar(ctx, "dead");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    dead,
                    Expression::Literal(Value::new(1), source),
                    Gc::new(
                        *ctx,
                        Term::Continue(halt, Array::from_slice(*ctx, &[]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            assert_eq!(stats.dead_letvals_removed, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected dead letval to be removed");
            };
            assert_eq!(cont, halt);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn dead_letval_requeues_binding_made_dead_by_removed_expr() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let outer = lvar(ctx, "outer");
            let inner = lvar(ctx, "inner");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    outer,
                    Expression::Literal(Value::new(1), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            inner,
                            Expression::PrimCall(
                                Value::new(false),
                                Array::from_slice(*ctx, &[Atom::Local(outer)]),
                                source,
                            ),
                            Gc::new(
                                *ctx,
                                Term::Continue(halt, Array::from_slice(*ctx, &[]), source),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            assert_eq!(stats.dead_letvals_removed, 2);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected both dead letvals to be removed");
            };
            assert_eq!(cont, halt);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn dead_letval_retains_outer_binding_still_used_by_body() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let live = lvar(ctx, "live");
            let dead = lvar(ctx, "dead");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    live,
                    Expression::Literal(Value::new(1), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            dead,
                            Expression::PrimCall(
                                Value::new(false),
                                Array::from_slice(*ctx, &[Atom::Local(live)]),
                                source,
                            ),
                            Gc::new(
                                *ctx,
                                Term::Continue(
                                    halt,
                                    Array::from_slice(*ctx, &[Atom::Local(live)]),
                                    source,
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            assert_eq!(stats.dead_letvals_removed, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(binding, Expression::Literal(..), body) = *lowered else {
                panic!("expected live letval to remain");
            };
            assert_eq!(binding, live);
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected dead letval body");
            };
            assert_eq!(cont, halt);
            assert_eq!(args.as_ref(), &[Atom::Local(live)]);
        });
    }

    #[test]
    fn eta_continuation_wrapper_collapses() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let value = lvar(ctx, "value");
            let source = Value::new(false);
            let wrapper = fixed_cont(
                ctx,
                k,
                &[x],
                Gc::new(
                    *ctx,
                    Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                ),
                false,
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[wrapper]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(value)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.eta_continuations, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected eta-continuation wrapper to disappear");
            };
            assert_eq!(cont, halt);
            assert_eq!(args.as_ref(), &[Atom::Local(value)]);
        });
    }

    #[test]
    fn eta_continuation_rejects_reordered_arguments() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let keep_k_live = lvar(ctx, "keep-k-live");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
            let a = lvar(ctx, "a");
            let b = lvar(ctx, "b");
            let source = Value::new(false);
            let wrapper = fixed_cont(
                ctx,
                k,
                &[x, y],
                Gc::new(
                    *ctx,
                    Term::Continue(
                        halt,
                        Array::from_slice(*ctx, &[Atom::Local(y), Atom::Local(x)]),
                        source,
                    ),
                ),
                false,
            );
            let second_use = fixed_cont(
                ctx,
                keep_k_live,
                &[],
                Gc::new(
                    *ctx,
                    Term::Continue(
                        k,
                        Array::from_slice(*ctx, &[Atom::Local(a), Atom::Local(b)]),
                        source,
                    ),
                ),
                false,
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[wrapper, second_use]),
                    Gc::new(
                        *ctx,
                        Term::Continue(
                            k,
                            Array::from_slice(*ctx, &[Atom::Local(a), Atom::Local(b)]),
                            source,
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.eta_continuations, 0);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(..) = *lowered else {
                panic!("expected reordered eta-continuation wrapper to remain");
            };
        });
    }

    #[test]
    fn eta_continuation_rejects_noinline_wrapper() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let value = lvar(ctx, "value");
            let source = Value::new(false);
            let wrapper = fixed_cont(
                ctx,
                k,
                &[x],
                Gc::new(
                    *ctx,
                    Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                ),
                true,
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[wrapper]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(value)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.eta_continuations, 0);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(..) = *lowered else {
                panic!("expected noinline eta-continuation wrapper to remain");
            };
        });
    }

    #[test]
    fn eta_function_wrapper_collapses() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let g = lvar(ctx, "g");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let value = lvar(ctx, "value");
            let source = Value::new(false);
            let wrapper = fixed_func(
                ctx,
                f,
                ret,
                &[x],
                Gc::new(
                    *ctx,
                    Term::App(
                        Atom::Local(g),
                        ret,
                        Array::from_slice(*ctx, &[Atom::Local(x)]),
                        source,
                    ),
                ),
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[wrapper]),
                    Gc::new(
                        *ctx,
                        Term::App(
                            Atom::Local(f),
                            k,
                            Array::from_slice(*ctx, &[Atom::Local(value)]),
                            source,
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.eta_functions, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::App(Atom::Local(func), cont, args, _) = *lowered else {
                panic!("expected eta-function wrapper to disappear");
            };
            assert_eq!(func, g);
            assert_eq!(cont, k);
            assert_eq!(args.as_ref(), &[Atom::Local(value)]);
        });
    }

    #[test]
    fn eta_function_rejects_reordered_arguments() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let g = lvar(ctx, "g");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
            let a = lvar(ctx, "a");
            let b = lvar(ctx, "b");
            let source = Value::new(false);
            let wrapper = fixed_func(
                ctx,
                f,
                ret,
                &[x, y],
                Gc::new(
                    *ctx,
                    Term::App(
                        Atom::Local(g),
                        ret,
                        Array::from_slice(*ctx, &[Atom::Local(y), Atom::Local(x)]),
                        source,
                    ),
                ),
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[wrapper]),
                    Gc::new(
                        *ctx,
                        Term::App(
                            Atom::Local(f),
                            k,
                            Array::from_slice(*ctx, &[Atom::Local(a), Atom::Local(b)]),
                            source,
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.eta_functions, 0);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Fix(..) = *lowered else {
                panic!("expected reordered eta-function wrapper to remain");
            };
        });
    }

    #[test]
    fn known_truthy_if_uses_consequent() {
        Scheme::new_uninit().enter(|ctx| {
            let test = lvar(ctx, "test");
            let consequent = lvar(ctx, "consequent");
            let alternative = lvar(ctx, "alternative");
            let value = lvar(ctx, "value");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    test,
                    Expression::Literal(Value::from_i32(1), source),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(test),
                            consequent,
                            consequent_args: Some(Array::from_slice(*ctx, &[Atom::Local(value)])),
                            alternative,
                            alternative_args: Some(Array::from_slice(*ctx, &[Atom::Local(value)])),
                            hints: [BranchHint::Normal, BranchHint::Normal],
                        },
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.constant_branches_simplified, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected constant branch to lower to direct continue");
            };
            assert_eq!(cont, consequent);
            assert_eq!(args.as_ref(), &[Atom::Local(value)]);
        });
    }

    #[test]
    fn known_false_if_uses_alternative_and_kills_skipped_branch_uses() {
        Scheme::new_uninit().enter(|ctx| {
            let test = lvar(ctx, "test");
            let skipped_value = lvar(ctx, "skipped-value");
            let consequent = lvar(ctx, "consequent");
            let alternative = lvar(ctx, "alternative");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    test,
                    Expression::Literal(Value::new(false), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            skipped_value,
                            Expression::Literal(Value::from_i32(7), source),
                            Gc::new(
                                *ctx,
                                Term::If {
                                    test: Atom::Local(test),
                                    consequent,
                                    consequent_args: Some(Array::from_slice(
                                        *ctx,
                                        &[Atom::Local(skipped_value)],
                                    )),
                                    alternative,
                                    alternative_args: None,
                                    hints: [BranchHint::Normal, BranchHint::Normal],
                                },
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                128,
            );
            assert_eq!(stats.constant_branches_simplified, 1);
            assert!(
                stats.dead_letvals_removed >= 2,
                "expected test and skipped-only value bindings to be removed"
            );
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected false branch to lower to direct continue, got {lowered:?}");
            };
            assert_eq!(cont, alternative);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn identical_continue_branches_collapse() {
        Scheme::new_uninit().enter(|ctx| {
            let test = lvar(ctx, "test");
            let join = lvar(ctx, "join");
            let value = lvar(ctx, "value");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    test,
                    Expression::PrimCall(
                        prim(ctx, "not-a-folded-primitive"),
                        Array::from_slice(*ctx, &[]),
                        source,
                    ),
                    Gc::new(
                        *ctx,
                        Term::If {
                            test: Atom::Local(test),
                            consequent: join,
                            consequent_args: Some(Array::from_slice(*ctx, &[Atom::Local(value)])),
                            alternative: join,
                            alternative_args: Some(Array::from_slice(*ctx, &[Atom::Local(value)])),
                            hints: [BranchHint::Normal, BranchHint::Normal],
                        },
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                128,
            );
            assert_eq!(stats.constant_branches_simplified, 0);
            assert_eq!(stats.identical_branches_simplified, 1);
            assert!(
                stats.dead_letvals_removed >= 1,
                "expected now-unused test binding to be removed"
            );
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected identical branches to lower to direct continue, got {lowered:?}");
            };
            assert_eq!(cont, join);
            assert_eq!(args.as_ref(), &[Atom::Local(value)]);
        });
    }

    #[test]
    fn same_target_branches_with_different_args_do_not_collapse() {
        Scheme::new_uninit().enter(|ctx| {
            let test = lvar(ctx, "test");
            let join = lvar(ctx, "join");
            let left = lvar(ctx, "left");
            let right = lvar(ctx, "right");
            let term = Gc::new(
                *ctx,
                Term::If {
                    test: Atom::Local(test),
                    consequent: join,
                    consequent_args: Some(Array::from_slice(*ctx, &[Atom::Local(left)])),
                    alternative: join,
                    alternative_args: Some(Array::from_slice(*ctx, &[Atom::Local(right)])),
                    hints: [BranchHint::Normal, BranchHint::Normal],
                },
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                64,
            );
            assert_eq!(stats.identical_branches_simplified, 0);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::If {
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                ..
            } = *lowered
            else {
                panic!("expected non-identical branches to remain, got {lowered:?}");
            };
            assert_eq!(consequent, join);
            assert_eq!(alternative, join);
            assert_eq!(
                consequent_args.expect("consequent args").as_ref(),
                &[Atom::Local(left)]
            );
            assert_eq!(
                alternative_args.expect("alternative args").as_ref(),
                &[Atom::Local(right)]
            );
        });
    }

    #[test]
    fn known_primcall_folds_constant_args() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let left = lvar(ctx, "left");
            let right = lvar(ctx, "right");
            let result = lvar(ctx, "result");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    left,
                    Expression::Literal(Value::from_i32(2), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            right,
                            Expression::Literal(Value::from_i32(3), source),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    result,
                                    Expression::PrimCall(
                                        prim(ctx, "+"),
                                        Array::from_slice(
                                            *ctx,
                                            &[Atom::Local(left), Atom::Local(right)],
                                        ),
                                        source,
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Continue(
                                            halt,
                                            Array::from_slice(*ctx, &[Atom::Local(result)]),
                                            source,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(128));
            assert_eq!(stats.known_prim_propagations, 1);
            assert!(
                stats.dead_letvals_removed >= 2,
                "expected folded argument bindings to become dead"
            );
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(binding, Expression::Literal(value, _), body) = *lowered else {
                panic!("expected folded primcall to become a literal let, got {lowered:?}");
            };
            assert_eq!(binding, result);
            assert_eq!(value, Value::from_i32(5));
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected folded result to flow to continue");
            };
            assert_eq!(cont, halt);
            assert_eq!(args.as_ref(), &[Atom::Local(result)]);
        });
    }

    #[test]
    fn folded_boolean_primcall_feeds_branch_cleanup() {
        Scheme::new_uninit().enter(|ctx| {
            let left = lvar(ctx, "left");
            let right = lvar(ctx, "right");
            let test = lvar(ctx, "test");
            let consequent = lvar(ctx, "consequent");
            let alternative = lvar(ctx, "alternative");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    left,
                    Expression::Literal(Value::from_i32(1), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            right,
                            Expression::Literal(Value::from_i32(1), source),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    test,
                                    Expression::PrimCall(
                                        prim(ctx, "="),
                                        Array::from_slice(
                                            *ctx,
                                            &[Atom::Local(left), Atom::Local(right)],
                                        ),
                                        source,
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::If {
                                            test: Atom::Local(test),
                                            consequent,
                                            consequent_args: None,
                                            alternative,
                                            alternative_args: None,
                                            hints: [BranchHint::Normal, BranchHint::Normal],
                                        },
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph_with_mode(
                ctx,
                &mut program.graph,
                program.root,
                GcpsContifyMode::Off,
                256,
            );
            assert_eq!(stats.known_prim_propagations, 1);
            assert_eq!(stats.constant_branches_simplified, 1);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected folded branch to lower to direct continue, got {lowered:?}");
            };
            assert_eq!(cont, consequent);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn unknown_primcall_with_constant_args_does_not_fold() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let left = lvar(ctx, "left");
            let right = lvar(ctx, "right");
            let result = lvar(ctx, "result");
            let unknown_prim = prim(ctx, "not-a-folded-primitive");
            let source = Value::new(false);
            let term = Gc::new(
                *ctx,
                Term::Let(
                    left,
                    Expression::Literal(Value::from_i32(2), source),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            right,
                            Expression::Literal(Value::from_i32(3), source),
                            Gc::new(
                                *ctx,
                                Term::Let(
                                    result,
                                    Expression::PrimCall(
                                        unknown_prim,
                                        Array::from_slice(
                                            *ctx,
                                            &[Atom::Local(left), Atom::Local(right)],
                                        ),
                                        source,
                                    ),
                                    Gc::new(
                                        *ctx,
                                        Term::Continue(
                                            halt,
                                            Array::from_slice(*ctx, &[Atom::Local(result)]),
                                            source,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            let stats = optimize_graph(ctx, &mut program.graph, program.root, Some(128));
            assert_eq!(stats.known_prim_propagations, 0);
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(_, Expression::Literal(..), body) = *lowered else {
                panic!("expected first literal binding to remain, got {lowered:?}");
            };
            let Term::Let(_, Expression::Literal(..), body) = *body else {
                panic!("expected second literal binding to remain");
            };
            let Term::Let(binding, Expression::PrimCall(prim, args, _), _) = *body else {
                panic!("expected unknown primcall to remain");
            };
            assert_eq!(binding, result);
            assert_eq!(prim, unknown_prim);
            assert_eq!(args.as_ref(), &[Atom::Local(left), Atom::Local(right)]);
        });
    }

    #[test]
    fn singleton_continuation_is_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
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
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Continue(cont, args, _) = *lowered else {
                panic!("expected reduced continue");
            };
            assert_eq!(cont, halt);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn variadic_continuation_is_not_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let rest = lvar(ctx, "rest");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: Some(rest),
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(rest)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected variadic continuation to remain bound");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected original continuation call");
            };
            assert_eq!(cont, k);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn noinline_continuation_is_not_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
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
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: true,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected noinline continuation to remain bound");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected original continuation call");
            };
            assert_eq!(cont, k);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn continuation_with_nested_letk_is_not_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let inner = lvar(ctx, "inner");
            let x = lvar(ctx, "x");
            let z = lvar(ctx, "z");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let inner_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: inner,
                    args: Array::from_slice(*ctx, &[z]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(z)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: true,
                    meta: Value::new(false),
                },
            );
            let outer_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Letk(
                            Array::from_slice(*ctx, &[inner_cont]),
                            Gc::new(
                                *ctx,
                                Term::Continue(
                                    inner,
                                    Array::from_slice(*ctx, &[Atom::Local(x)]),
                                    source,
                                ),
                            ),
                        ),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[outer_cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected complex continuation to remain bound");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected original continuation call");
            };
            assert_eq!(cont, k);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn continuation_with_captured_value_is_not_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let k = lvar(ctx, "k");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Let(
                    y,
                    Expression::Literal(Value::new(1), source),
                    Gc::new(
                        *ctx,
                        Term::Letk(
                            Array::from_slice(*ctx, &[cont]),
                            Gc::new(
                                *ctx,
                                Term::Continue(k, Array::from_slice(*ctx, &[]), source),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(_, Expression::Literal(..), body) = *lowered else {
                panic!("expected outer literal binding");
            };
            let Term::Letk(_, body) = *body else {
                panic!("expected captured-value continuation to remain bound");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected original continuation call");
            };
            assert_eq!(cont, k);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn continuation_with_local_control_target_is_not_beta_reduced() {
        Scheme::new_uninit().enter(|ctx| {
            let halt = lvar(ctx, "halt");
            let inner = lvar(ctx, "inner");
            let outer = lvar(ctx, "outer");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let inner_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: inner,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(halt, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let outer_cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: outer,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(inner, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[inner_cont, outer_cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(outer, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected local-target continuation to remain bound");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected original continuation call");
            };
            assert_eq!(cont, outer);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn contification_captures_outer_free_values_at_pushed_site() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[Atom::Local(y)]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Let(
                    y,
                    Expression::Literal(Value::new(1), source),
                    Gc::new(
                        *ctx,
                        Term::Fix(
                            Array::from_slice(*ctx, &[func]),
                            Gc::new(
                                *ctx,
                                Term::App(
                                    Atom::Local(f),
                                    k,
                                    Array::from_slice(*ctx, &[Atom::Local(y)]),
                                    source,
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(_, Expression::Literal(..), body) = *lowered else {
                panic!("expected literal outer binding");
            };
            let Term::Letk(_, body) = *body else {
                panic!("expected contified function under captured value binding");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected contified call");
            };
            assert_eq!(cont, f);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }

    #[test]
    fn contification_rejects_return_cont_outside_owning_function() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let g = lvar(ctx, "g");
            let f_ret = lvar(ctx, "f-ret");
            let g_ret = lvar(ctx, "g-ret");
            let source = Value::new(false);
            let func_f = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: f_ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(f_ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let func_g = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: g,
                    return_cont: g_ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(g_ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[func_g]),
                    Gc::new(
                        *ctx,
                        Term::Fix(
                            Array::from_slice(*ctx, &[func_f]),
                            Gc::new(
                                *ctx,
                                Term::App(
                                    Atom::Local(f),
                                    g_ret,
                                    Array::from_slice(*ctx, &[]),
                                    source,
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Fix(_, body) = *lowered else {
                panic!("expected outer function binding to remain");
            };
            let Term::Fix(..) = *body else {
                panic!("expected inner function to remain a fix");
            };
        });
    }

    #[test]
    fn contification_pushes_inside_local_continuation_scope() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let source = Value::new(false);
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[func]),
                    Gc::new(
                        *ctx,
                        Term::Letk(
                            Array::from_slice(*ctx, &[cont]),
                            Gc::new(
                                *ctx,
                                Term::App(Atom::Local(f), k, Array::from_slice(*ctx, &[]), source),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected original local continuation binding");
            };
            let Term::Letk(_, body) = *body else {
                panic!("expected contified function pushed under local continuation");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected contified call");
            };
            assert_eq!(cont, f);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn contification_accepts_known_local_continuation_target() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let source = Value::new(false);
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let cont = Gc::new(
                *ctx,
                Cont {
                    name: Value::new(false),
                    binding: k,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(k, Array::from_slice(*ctx, &[]), source),
                    )),
                    source,
                    free_vars: Lock::new(None),
                    reified: Cell::new(false),
                    cold: false,
                    noinline: false,
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Fix(
                            Array::from_slice(*ctx, &[func]),
                            Gc::new(
                                *ctx,
                                Term::App(Atom::Local(f), k, Array::from_slice(*ctx, &[]), source),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(1));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Letk(_, body) = *lowered else {
                panic!("expected local continuation binding");
            };
            let Term::Letk(_, body) = *body else {
                panic!("expected contified function under local continuation binding");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected contified call");
            };
            assert_eq!(cont, f);
            assert!(args.is_empty());
        });
    }

    #[test]
    fn contification_site_covers_sibling_function_uses() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let g = lvar(ctx, "g");
            let h = lvar(ctx, "h");
            let f_ret = lvar(ctx, "f-ret");
            let g_ret = lvar(ctx, "g-ret");
            let h_ret = lvar(ctx, "h-ret");
            let root_ret = lvar(ctx, "root-ret");
            let source = Value::new(false);
            let func_f = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: f_ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::App(Atom::Local(g), f_ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let func_g = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: g,
                    return_cont: g_ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(g_ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let func_h = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: h,
                    return_cont: h_ret,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::App(Atom::Local(g), h_ret, Array::from_slice(*ctx, &[]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[func_f, func_g, func_h]),
                    Gc::new(
                        *ctx,
                        Term::App(
                            Atom::Local(f),
                            root_ret,
                            Array::from_slice(*ctx, &[]),
                            source,
                        ),
                    ),
                ),
            );

            let program = cps_to_graph(ctx, term).expect("convert");
            let root = program.graph.read_term_link(program.root).expect("root");
            let TermKind::Fix(functions, body) = program.graph[root].kind else {
                panic!("expected root fix");
            };
            let state = OptimizerState::new();
            let live = state.live_function_links(&program.graph, &functions);
            let mut binders = EntitySet::new();
            binders.insert(
                live.iter()
                    .map(|(_, function)| program.graph[*function].var)
                    .find(|binder| program.graph[*binder].var == g)
                    .expect("g binder"),
            );

            let site = scc_contify::contification_site(
                &program.graph,
                program.root,
                &live,
                body,
                &binders,
            )
            .expect("site");
            assert_eq!(site, program.root);
        });
    }

    #[test]
    fn dominator_contification_handles_multiple_return_locations() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let g1 = lvar(ctx, "g1");
            let g2 = lvar(ctx, "g2");
            let h = lvar(ctx, "h");
            let f_ret = lvar(ctx, "f-ret");
            let g1_ret = lvar(ctx, "g1-ret");
            let g2_ret = lvar(ctx, "g2-ret");
            let h_ret = lvar(ctx, "h-ret");
            let k1 = lvar(ctx, "k1");
            let k2 = lvar(ctx, "k2");
            let f_branch1 = lvar(ctx, "f-branch1");
            let f_branch2 = lvar(ctx, "f-branch2");
            let source = Value::new(false);

            let app = |callee, cont| {
                Gc::new(
                    *ctx,
                    Term::App(
                        Atom::Local(callee),
                        cont,
                        Array::from_slice(*ctx, &[]),
                        source,
                    ),
                )
            };
            let continue_to = |cont| {
                Gc::new(
                    *ctx,
                    Term::Continue(cont, Array::from_slice(*ctx, &[]), source),
                )
            };
            let make_func = |binding, return_cont, body| {
                Gc::new(
                    *ctx,
                    Func {
                        name: Value::new(false),
                        source,
                        binding,
                        return_cont,
                        args: Array::from_slice(*ctx, &[]),
                        variadic: None,
                        body: Lock::new(body),
                        free_vars: Lock::new(None),
                        meta: Value::new(false),
                    },
                )
            };
            let make_cont = |binding, body| {
                Gc::new(
                    *ctx,
                    Cont {
                        name: Value::new(false),
                        binding,
                        args: Array::from_slice(*ctx, &[]),
                        variadic: None,
                        body: Lock::new(body),
                        source,
                        free_vars: Lock::new(None),
                        reified: Cell::new(false),
                        cold: false,
                        noinline: false,
                        meta: Value::new(false),
                    },
                )
            };

            let f_branch1_cont = make_cont(f_branch1, app(g1, f_ret));
            let f_branch2_cont = make_cont(f_branch2, app(g2, f_ret));
            let func_f = make_func(
                f,
                f_ret,
                Gc::new(
                    *ctx,
                    Term::Letk(
                        Array::from_slice(*ctx, &[f_branch1_cont, f_branch2_cont]),
                        continue_to(f_branch1),
                    ),
                ),
            );
            let func_g1 = make_func(g1, g1_ret, app(h, g1_ret));
            let func_g2 = make_func(g2, g2_ret, app(h, g2_ret));
            let func_h = make_func(h, h_ret, continue_to(h_ret));
            let cont_k1 = make_cont(k1, app(f, k1));
            let cont_k2 = make_cont(k2, app(f, k2));
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[func_f, func_g1, func_g2, func_h]),
                    Gc::new(
                        *ctx,
                        Term::Letk(
                            Array::from_slice(*ctx, &[cont_k1, cont_k2]),
                            Gc::new(
                                *ctx,
                                Term::Continue(k1, Array::from_slice(*ctx, &[]), source),
                            ),
                        ),
                    ),
                ),
            );

            let mut scc_program = cps_to_graph(ctx, term).expect("convert scc");
            let scc_stats = optimize_graph_with_mode(
                ctx,
                &mut scc_program.graph,
                scc_program.root,
                GcpsContifyMode::Scc,
                1,
            );
            let mut dom_program = cps_to_graph(ctx, term).expect("convert dom");
            let dom_stats = optimize_graph_with_mode(
                ctx,
                &mut dom_program.graph,
                dom_program.root,
                GcpsContifyMode::Dom,
                1,
            );

            assert!(
                dom_stats.dom_contified_functions > scc_stats.scc_contified_functions,
                "expected ADom to contify more functions than SCC on the Figure 7 shape: dom={}, scc={}",
                dom_stats.dom_contified_functions,
                scc_stats.scc_contified_functions
            );
            assert_eq!(dom_stats.dom_contified_functions, 3);
        });
    }

    #[test]
    fn whole_fix_group_can_be_contified() {
        Scheme::new_uninit().enter(|ctx| {
            let f = lvar(ctx, "f");
            let ret = lvar(ctx, "ret");
            let k = lvar(ctx, "k");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");
            let source = Value::new(false);
            let func = Gc::new(
                *ctx,
                Func {
                    name: Value::new(false),
                    source,
                    binding: f,
                    return_cont: ret,
                    args: Array::from_slice(*ctx, &[x]),
                    variadic: None,
                    body: Lock::new(Gc::new(
                        *ctx,
                        Term::Continue(ret, Array::from_slice(*ctx, &[Atom::Local(x)]), source),
                    )),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );
            let term = Gc::new(
                *ctx,
                Term::Fix(
                    Array::from_slice(*ctx, &[func]),
                    Gc::new(
                        *ctx,
                        Term::Let(
                            y,
                            Expression::Literal(Value::new(1), source),
                            Gc::new(
                                *ctx,
                                Term::App(
                                    Atom::Local(f),
                                    k,
                                    Array::from_slice(*ctx, &[Atom::Local(y)]),
                                    source,
                                ),
                            ),
                        ),
                    ),
                ),
            );

            let mut program = cps_to_graph(ctx, term).expect("convert");
            optimize_graph(ctx, &mut program.graph, program.root, Some(64));
            let lowered = graph_to_cps(ctx, &program.graph, program.root).expect("lower");
            let Term::Let(_, Expression::Literal(..), body) = *lowered else {
                panic!("expected literal body");
            };
            let Term::Continue(cont, args, _) = *body else {
                panic!("expected optimized continuation");
            };
            assert_eq!(cont, k);
            assert_eq!(args.as_slice(), &[Atom::Local(y)]);
        });
    }
}
