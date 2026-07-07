use std::env;

use cranelift_entity::{EntitySet, SecondaryMap};

use crate::{cps::term::FuncRef, runtime::Context, utils::pass_profile::ProfileScope};

use super::{
    convert::{cps_to_graph, graph_to_cps, ConvertResult},
    dom_contify,
    graph::{
        ActiveLinkStatus, BoundVar, ContVar, FreeVar, FunctionId, FunctionLink, Graph,
        GraphWorklist, Parent, Subterm, TermId, TermKind, WorklistQueue,
    },
    scc_contify,
};

pub const DEFAULT_GAS: usize = 42_000;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct OptimizationStats {
    pub iterations: usize,
    pub gas_used: usize,
    pub ran_out_of_gas: bool,
    pub dead_bindings_processed: usize,
    pub singleton_calls_inlined: usize,
    pub singleton_continuations_inlined: usize,
    pub contifications: usize,
    pub scc_contifications: usize,
    pub dom_contifications: usize,
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
    let stats = optimize_graph(&mut program.graph, program.root, Some(DEFAULT_GAS));
    if optimize_profile.is_enabled() {
        let graph_stats = program.graph.stats();
        optimize_profile.field("iterations", stats.iterations);
        optimize_profile.field("gas_used", stats.gas_used);
        optimize_profile.field("ran_out_of_gas", stats.ran_out_of_gas);
        optimize_profile.field("dead_bindings", stats.dead_bindings_processed);
        optimize_profile.field("calls_inlined", stats.singleton_calls_inlined);
        optimize_profile.field("conts_inlined", stats.singleton_continuations_inlined);
        optimize_profile.field("contifications", stats.contifications);
        optimize_profile.field("scc_contifications", stats.scc_contifications);
        optimize_profile.field("dom_contifications", stats.dom_contifications);
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
    state.run(graph, gas.unwrap_or(usize::MAX), contify_mode);
    state.stats
}

pub(super) struct OptimizerState {
    worklist: GraphWorklist,
    known_functions: SecondaryMap<BoundVar, Option<FunctionLink>>,
    function_defs: SecondaryMap<BoundVar, Option<Subterm>>,
    function_owner_defs: SecondaryMap<FunctionId, Option<Subterm>>,
    return_cont_owners: SecondaryMap<BoundVar, Option<FunctionId>>,
    known_exprs: SecondaryMap<BoundVar, Option<super::graph::Subexpr>>,
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
            function_owner_defs: SecondaryMap::new(),
            return_cont_owners: SecondaryMap::new(),
            known_exprs: SecondaryMap::new(),
            known_function_count: 0,
            known_expr_count: 0,
            dead_bindings: WorklistQueue::new(),
            stats: OptimizationStats::default(),
            old_occurrences: Vec::new(),
            new_occurrences: Vec::new(),
            occurrence_set: EntitySet::new(),
        }
    }

    fn run<'gc>(&mut self, graph: &mut Graph<'gc>, mut gas: usize, contify_mode: GcpsContifyMode) {
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
                TermKind::App(callee, args, cont) => {
                    self.reduce_call(graph, queued_link, active_link, term, callee, args, cont);
                }
                TermKind::Continue(cont, args) => {
                    self.reduce_continue(graph, queued_link, active_link, term, cont, args);
                }
                TermKind::Fix(functions, body) => {
                    self.try_contify_fix(graph, active_link, term, functions, body, contify_mode);
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
            TermKind::App(..) | TermKind::Continue(..) | TermKind::Fix(..) => {
                self.worklist.add_subterm(root);
            }
            _ => {}
        }

        if let TermKind::LetVal((binder, expr), _) = graph[term].kind {
            if self.known_exprs[binder].is_none() {
                self.known_expr_count += 1;
            }
            self.known_exprs[binder] = Some(expr);
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
                self.function_owner_defs[function] = Some(root);
                if let Some(return_cont) = graph[function].cont {
                    self.return_cont_owners[return_cont] = Some(function);
                }
            }
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
            if let Some(expr) = self.known_exprs[binding] {
                if let Some(expr_id) = graph.read_expr_link(expr) {
                    let mut occurrences = std::mem::take(&mut self.old_occurrences);
                    occurrences.clear();
                    graph.push_free_vars_of_expr(expr_id, &mut occurrences);
                    for occ in occurrences.iter().copied() {
                        self.kill_occurrence(graph, occ);
                    }
                    occurrences.clear();
                    self.old_occurrences = occurrences;
                }
            }

            if let Some(link) = self.known_functions[binding] {
                graph.clear_function_link(link);
            }
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

        if let Some(parent) = graph.read_parent_link(graph[old_term].link) {
            graph.set_parent_link(graph[replacement].link, parent);
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
        match candidate.source {
            ContifySource::Scc => self.stats.scc_contifications += 1,
            ContifySource::Dominator => self.stats.dom_contifications += 1,
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
        for function in contified_functions {
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
        verbose_log!(
            "gcps optimize: inserted contification wrapper {}",
            graph.pretty_term(wrapper)
        );
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

        if let Some(def) = self.function_defs[binder] {
            let Some(scope_term) = graph.read_term_link(def) else {
                return false;
            };
            return self.term_is_inside_term_scope(graph, term, scope_term);
        }

        true
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::term::{Atom, Cont, Expression, Func, Term},
        expander::core::{fresh_lvar, LVarRef},
        rsgc::{alloc::Array, cell::Lock, Gc},
        runtime::{value::Value, Scheme},
    };
    use std::cell::Cell;

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, ctx.intern(name))
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
            optimize_graph(&mut program.graph, program.root, Some(1));
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
            optimize_graph(&mut program.graph, program.root, Some(64));
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
