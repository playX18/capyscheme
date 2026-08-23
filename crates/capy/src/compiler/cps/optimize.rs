use cranelift_entity::{EntitySet, SecondaryMap};

use crate::{
    compiler::{cfg::Program, dump},
    runtime::{Context, value::Value},
    utils::{flags, pass_profile::ProfileScope},
};

use super::fold::folding_table;

use super::{
    clone::GraphClone,
    convert::{ConvertResult, GraphFunctionProgram},
    dom_contify,
    graph::{
        ActiveLinkStatus, BoundVar, ContVar, ExprKind, FreeVar, FunctionId, FunctionLink,
        FunctionLinks, Graph, GraphWorklist, Parent, Subexpr, Subterm, TermId, TermKind,
        WorklistQueue,
    },
    reify::reify_graph,
    scc_contify,
};

pub const DEFAULT_GAS: usize = 42_000;
const MAX_RECURSIVE_UNROLL_DEPTH: usize = 1;
const MAX_RECURSIVE_UNROLL_TERMS: usize = 48;

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
    pub recursive_unrolls: usize,
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
pub enum ContifyMode {
    Off,
    Scc,
    Dom,
    DomThenScc,
}

impl ContifyMode {
    fn current() -> Self {
        match flags::gcps_contify() {
            "off" | "0" | "none" | "false" => Self::Off,
            "scc" | "legacy" => Self::Scc,
            "both" | "dom+scc" | "dom-then-scc" => Self::DomThenScc,
            // "dom", "dominator", "dominators", or the "" unset marker.
            _ => Self::Dom,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ContifySource {
    Scc,
    Dominator,
}

pub struct OptimizedGraphFunctionProgram<'gc> {
    pub graph: Graph<'gc>,
    pub entry: FunctionId,
    pub stats: OptimizationStats,
}

impl<'gc> OptimizedGraphFunctionProgram<'gc> {
    pub fn root(&self) -> Subterm {
        self.graph[self.entry].body
    }
}

pub struct OptimizedProgram<'gc> {
    pub ssa: Program<'gc>,
    pub stats: OptimizationStats,
}

fn optimize_graph_program<'gc>(
    ctx: Context<'gc>,
    mut program: GraphFunctionProgram<'gc>,
) -> OptimizedGraphFunctionProgram<'gc> {
    let mut optimize_profile = ProfileScope::new("compiler.lower.gcps.run");
    let root = program.root();
    let stats = optimize_graph(ctx, &mut program.graph, root, Some(DEFAULT_GAS));
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
        optimize_profile.field("recursive_unrolls", stats.recursive_unrolls);
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

    OptimizedGraphFunctionProgram {
        graph: program.graph,
        entry: program.entry,
        stats,
    }
}

pub fn optimize_graph_func_to_ssa<'gc>(
    ctx: Context<'gc>,
    program: GraphFunctionProgram<'gc>,
) -> ConvertResult<OptimizedProgram<'gc>> {
    let mut program = optimize_graph_program(ctx, program);

    // Reify before group merging (the merge needs `is_reified`). The merge
    // changes no bindings or uses, so `GraphReifyInfo` stays valid.
    let mut graph_reify_profile = ProfileScope::new("compiler.lower.gcps.graph_reify");
    let graph_reify = reify_graph(&mut program.graph, program.entry);
    if graph_reify_profile.is_enabled() {
        graph_reify_profile.field("functions", graph_reify.functions.len());
        graph_reify_profile.field("continuations", graph_reify.continuations.len());
    }
    drop(graph_reify_profile);

    // Merge adjacent Fix/Letk groups so lambdas and reified continuations bound
    // at the same point share one closure-creation site (see fixmerge.rs).
    let root = program.root();
    super::fixmerge::merge_nested_groups(&mut program.graph, root);

    // Shared-environment analysis (disabled with CAPY_CLOSURE_SHARING=0;
    // decisions dumped with CAPY_SHARE_DUMP=1).
    let mut share_profile = ProfileScope::new("compiler.lower.gcps.closure_share");
    let sharing_enabled = flags::closure_sharing();
    let root = program
        .graph
        .read_term_link(program.root())
        .expect("graph root");
    let stages = super::analysis::StageAnalysis::new(&program.graph, &graph_reify);
    // Flow analysis (call web, closure classes, escape).
    let flow = super::flow::FlowAnalysis::new(&program.graph, &graph_reify);
    if flags::share_dump() {
        let recursive = super::analysis::recursive_functions(&program.graph, &graph_reify);
        let escaping = graph_reify
            .functions
            .iter()
            .filter(|f| flow.function(**f).map_or(false, |info| info.escapes))
            .count();
        let classes = flow.classes().len();
        eprintln!(
            "share: {} functions, {} recursive, {} escaping, {} closure classes",
            graph_reify.functions.len(),
            recursive.len(),
            escaping,
            classes,
        );
    }
    let share = super::share::analyze_sharing(
        &program.graph,
        &graph_reify,
        &stages,
        &flow,
        root,
        sharing_enabled,
    );
    if share_profile.is_enabled() {
        share_profile.field("shared_sites", share.sites().count());
    }
    if flags::share_dump() {
        for (site, decision) in share.sites() {
            let kinds: Vec<&str> = decision
                .members
                .iter()
                .map(|m| {
                    if program.graph[m.function].cont.is_some() {
                        "fn"
                    } else {
                        "cont"
                    }
                })
                .collect();
            eprintln!(
                "share: site {site:?} allocate={} record_vars={} members={} kinds=[{}]",
                decision.allocate,
                decision.record_vars.len(),
                decision.members.len(),
                kinds.join(" ")
            );
        }
    }
    drop(share_profile);

    let mut ssa_profile = ProfileScope::new("compiler.lower.gcps.ssa");
    let ssa = crate::compiler::cfg::lower::lower_graph(&program.graph, &graph_reify, &share);
    if ssa_profile.is_enabled() {
        ssa_profile.field("procedures", ssa.procedures.len());
    }
    drop(ssa_profile);

    Ok(OptimizedProgram {
        ssa,
        stats: program.stats,
    })
}

pub fn optimize_graph<'gc>(
    ctx: Context<'gc>,
    graph: &mut Graph<'gc>,
    root: Subterm,
    gas: Option<usize>,
) -> OptimizationStats {
    let mut state = OptimizerState::new();
    let contify_mode = ContifyMode::current();
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
    pub(super) insertion: ContifyInsertion,
    pub(super) source: ContifySource,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ContifyInsertion {
    Wrap(Subterm),
    ExtendLetk(Subterm),
}

impl ContifyInsertion {
    fn site(self) -> Subterm {
        match self {
            Self::Wrap(site) | Self::ExtendLetk(site) => site,
        }
    }

    fn set_site(&mut self, site: Subterm) {
        match self {
            Self::Wrap(current) | Self::ExtendLetk(current) => *current = site,
        }
    }
}

#[derive(Clone, Copy)]
enum ContifyAvailability {
    AtTerm,
    Wrapper,
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
        contify_mode: ContifyMode,
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
                    self.reduce_call(
                        ctx,
                        graph,
                        queued_link,
                        active_link,
                        term,
                        callee,
                        args,
                        cont,
                    );
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
                TermKind::If(test, consequent, alternative, _) => {
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
            if let Some(expr) = self.known_exprs[binding]
                && self.expr_link_is_dead_removable(graph, expr)
            {
                let expr_def = self.known_expr_defs[binding];
                self.take_known_expr(binding);
                if let Some(def) = expr_def {
                    self.worklist.add_subterm(def);
                }
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

    #[allow(clippy::too_many_arguments)]
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

        if !self.expr_link_is_dead_removable(graph, expr) {
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

    fn expr_link_is_dead_removable<'gc>(&self, graph: &Graph<'gc>, expr: Subexpr) -> bool {
        let Some(expr) = graph.read_expr_link(expr) else {
            return false;
        };
        matches!(graph[expr].kind, ExprKind::Literal(_))
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
            args.push(value);
        }

        let Some(value) = folding_table(ctx).try_fold_values(ctx, prim, &args) else {
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

    #[allow(clippy::too_many_arguments)]
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
            TermKind::If(_, consequent, alternative, _) => {
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

    #[allow(clippy::too_many_arguments)]
    fn reduce_call<'gc>(
        &mut self,
        ctx: Context<'gc>,
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

        if self.try_unroll_recursive_call(
            ctx,
            graph,
            queued_link,
            active_link,
            term,
            binder,
            function,
            &args,
            cont,
        ) {
            return;
        }

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
        self.worklist
            .add_occurrences(graph, graph.free_vars_slice(&args).to_vec());
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn try_unroll_recursive_call<'gc>(
        &mut self,
        ctx: Context<'gc>,
        graph: &mut Graph<'gc>,
        queued_link: Subterm,
        active_link: Subterm,
        term: TermId,
        binder: BoundVar,
        function: FunctionId,
        args: &super::graph::FreeVars,
        cont: ContVar,
    ) -> bool {
        let function_data = graph[function];
        let Some(return_cont) = function_data.cont else {
            return false;
        };

        if function_data.var != binder
            || !function_data.is_rec
            || function_data.variadic.is_some()
            || function_data.unroll_count >= MAX_RECURSIVE_UNROLL_DEPTH
            || function_data.is_cold
            || function_data.is_noinline
            || function_data.is_reified
            || !self.arity_matches(graph, function, args)
            || !self.term_is_inside_function(graph, term, function)
            || !self.term_subtree_fits(graph, function_data.body, MAX_RECURSIVE_UNROLL_TERMS)
        {
            return false;
        }

        let formals = graph.bound_vars_slice(&function_data.vars).to_vec();
        let actuals = graph
            .free_vars_slice(args)
            .iter()
            .copied()
            .map(|actual| graph.free_binder(actual))
            .collect::<Vec<_>>();
        let return_actual = graph.free_binder(cont);
        let mut substitutions = formals
            .into_iter()
            .zip(actuals)
            .collect::<Vec<(BoundVar, BoundVar)>>();
        substitutions.push((return_cont, return_actual));

        let clone_link = graph.new_term_link(None);
        if GraphClone::with_substitutions(ctx, graph, substitutions)
            .clone_subterm_into(function_data.body, clone_link)
            .is_none()
        {
            graph.clear_term_link(clone_link);
            return false;
        }

        verbose_log!("gcps optimize: unroll recursive call to {binder}");
        graph[function].unroll_count += 1;
        self.stats.recursive_unrolls += 1;
        self.replace_with_existing_body(graph, active_link, term, clone_link);
        self.collect_redexes(graph, active_link);
        self.worklist.add_subterm(active_link);
        if queued_link != active_link {
            graph.set_term_link(
                queued_link,
                graph.read_term_link(active_link).unwrap_or(term),
            );
        }
        true
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

    fn term_subtree_fits<'gc>(&self, graph: &Graph<'gc>, link: Subterm, limit: usize) -> bool {
        let mut remaining = limit;
        self.consume_term_budget(graph, link, &mut remaining)
    }

    fn consume_term_budget<'gc>(
        &self,
        graph: &Graph<'gc>,
        link: Subterm,
        remaining: &mut usize,
    ) -> bool {
        let Some(term) = graph.read_term_link(link) else {
            return false;
        };
        let Some(next) = remaining.checked_sub(1) else {
            return false;
        };
        *remaining = next;

        match graph[term].kind {
            TermKind::LetVal(_, body) => self.consume_term_budget(graph, body, remaining),
            TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
                for link in graph.function_links_slice(&functions) {
                    let Some(function) = graph.read_function_link(*link) else {
                        continue;
                    };
                    if !self.consume_term_budget(graph, graph[function].body, remaining) {
                        return false;
                    }
                }
                self.consume_term_budget(graph, body, remaining)
            }
            TermKind::If(_, consequent, alternative, _) => {
                self.consume_term_budget(graph, consequent, remaining)
                    && self.consume_term_budget(graph, alternative, remaining)
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => true,
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
                if graph.read_expr_link(expr).is_some_and(|expr_id| {
                    !self.expr_free_occurrences_are_allowed(graph, expr_id, value_scope)
                }) {
                    return false;
                }
                value_scope.insert(binding);
                self.term_is_simple_continuation_inline_body(graph, body, value_scope)
            }
            TermKind::If(_, then_branch, else_branch, _) => {
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
        mode: ContifyMode,
    ) {
        let live = self.live_function_links(graph, &functions);
        if live.is_empty() {
            self.replace_with_existing_body(graph, active_link, term, body);
            return;
        }

        let candidate = match mode {
            ContifyMode::Off => None,
            ContifyMode::Scc => {
                scc_contify::find_candidate(self, graph, active_link, term, &live, body)
            }
            ContifyMode::Dom => {
                dom_contify::find_candidate(self, graph, active_link, term, &live, body)
            }
            ContifyMode::DomThenScc => {
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
                candidate.insertion.site(),
                body,
                candidate.return_cont,
                candidate.source,
                &candidate.binders,
                &contified_functions,
                &untouched_links,
            );
        }

        let mut insertion = candidate.insertion;
        let contified = graph.new_function_links(contified_links.iter().copied());
        if untouched_links.is_empty() {
            self.replace_with_existing_body(graph, active_link, term, body);
            if insertion.site() == body {
                insertion.set_site(active_link);
            }
        } else {
            let untouched = graph.new_function_links(untouched_links.iter().copied());
            graph[term].kind = TermKind::Fix(untouched, body);
            let subterms = graph.subterms_of(term);
            graph.backpatch_subterms(term, &subterms);
        }
        let site = insertion.site();

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

        let insertion_term = self.insert_contified_functions(graph, insertion, contified);
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
            "gcps optimize: inserted contification {}",
            graph.pretty_term(insertion_term)
        );
    }

    #[allow(clippy::too_many_arguments)]
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

    fn insert_contified_functions<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        insertion: ContifyInsertion,
        functions: super::graph::FunctionLinks,
    ) -> TermId {
        match insertion {
            ContifyInsertion::Wrap(site) => self.wrap_link_with_letk(graph, site, functions),
            ContifyInsertion::ExtendLetk(site) => {
                self.extend_letk_with_functions(graph, site, functions)
            }
        }
    }

    fn extend_letk_with_functions<'gc>(
        &mut self,
        graph: &mut Graph<'gc>,
        link: Subterm,
        functions: super::graph::FunctionLinks,
    ) -> TermId {
        let term = graph
            .read_term_link(link)
            .expect("contification insertion site must be live");
        let TermKind::Letk(existing, body) = graph[term].kind else {
            unreachable!("extended contification insertion must target a Letk term");
        };

        let mut links = graph.function_links_slice(&existing).to_vec();
        links.extend(graph.function_links_slice(&functions).iter().copied());
        let functions = graph.new_function_links(links);
        graph[term].kind = TermKind::Letk(functions, body);
        term
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
            TermKind::If(_, then_branch, else_branch, _) => {
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

    pub(super) fn choose_contification_insertion<'gc>(
        &self,
        graph: &Graph<'gc>,
        site: Subterm,
        return_cont: BoundVar,
        binders: &EntitySet<BoundVar>,
        functions: &[FunctionId],
    ) -> Option<ContifyInsertion> {
        let site_term = graph.read_term_link(site)?;

        if self.contification_bindings_are_available_at(
            graph,
            site_term,
            return_cont,
            binders,
            functions,
            ContifyAvailability::Wrapper,
        ) {
            return Some(ContifyInsertion::Wrap(site));
        }

        if matches!(graph[site_term].kind, TermKind::Letk(..))
            && self.contification_bindings_are_available_at(
                graph,
                site_term,
                return_cont,
                binders,
                functions,
                ContifyAvailability::AtTerm,
            )
        {
            return Some(ContifyInsertion::ExtendLetk(site));
        }

        None
    }

    fn contification_bindings_are_available_at<'gc>(
        &self,
        graph: &Graph<'gc>,
        site_term: TermId,
        return_cont: BoundVar,
        binders: &EntitySet<BoundVar>,
        functions: &[FunctionId],
        availability: ContifyAvailability,
    ) -> bool {
        self.binder_is_available_for_contification(graph, site_term, return_cont, availability)
            && functions.iter().copied().all(|function| {
                self.function_uses_only_available_scope(
                    graph,
                    function,
                    binders,
                    site_term,
                    availability,
                )
            })
    }

    fn binder_is_available_for_contification<'gc>(
        &self,
        graph: &Graph<'gc>,
        term: TermId,
        binder: BoundVar,
        availability: ContifyAvailability,
    ) -> bool {
        match availability {
            ContifyAvailability::AtTerm => self.binder_is_available_at_term(graph, term, binder),
            ContifyAvailability::Wrapper => {
                self.binder_is_available_at_contification_wrapper(graph, term, binder)
            }
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

    fn binder_is_available_at_contification_wrapper<'gc>(
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
            return term != scope_term && self.term_is_inside_term_scope(graph, term, scope_term);
        }

        true
    }

    fn function_uses_only_available_scope<'gc>(
        &self,
        graph: &Graph<'gc>,
        function: FunctionId,
        group_binders: &EntitySet<BoundVar>,
        site_term: TermId,
        availability: ContifyAvailability,
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
        self.term_uses_only_available_scope_at_term(
            graph,
            data.body,
            &local,
            site_term,
            availability,
        )
    }

    fn term_uses_only_available_scope_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        link: Subterm,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
        availability: ContifyAvailability,
    ) -> bool {
        let Some(term) = graph.read_term_link(link) else {
            return true;
        };

        match graph[term].kind {
            TermKind::LetVal((binding, expr), body) => {
                if graph.read_expr_link(expr).is_some_and(|expr_id| {
                    !self.expr_free_occurrences_are_available_at_term(
                        graph,
                        expr_id,
                        local,
                        site_term,
                        availability,
                    )
                }) {
                    return false;
                }
                let mut body_local = local.clone();
                body_local.insert(binding);
                self.term_uses_only_available_scope_at_term(
                    graph,
                    body,
                    &body_local,
                    site_term,
                    availability,
                )
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
                        availability,
                    ) {
                        return false;
                    }
                }
                self.term_uses_only_available_scope_at_term(
                    graph,
                    body,
                    &body_local,
                    site_term,
                    availability,
                )
            }
            TermKind::If(test, then_branch, else_branch, _) => {
                self.free_occurrences_are_available_at_term(
                    graph,
                    [test],
                    local,
                    site_term,
                    availability,
                ) && self.term_uses_only_available_scope_at_term(
                    graph,
                    then_branch,
                    local,
                    site_term,
                    availability,
                ) && self.term_uses_only_available_scope_at_term(
                    graph,
                    else_branch,
                    local,
                    site_term,
                    availability,
                )
            }
            TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => self
                .direct_free_occurrences_are_available_at_term(
                    graph,
                    term,
                    local,
                    site_term,
                    availability,
                ),
        }
    }

    fn expr_free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        expr: super::graph::ExprId,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
        availability: ContifyAvailability,
    ) -> bool {
        let mut available = true;
        graph.for_each_free_var_of_expr(expr, |var| {
            let binder = graph.free_binder(var);
            available &= local.contains(binder)
                || self.binder_is_available_for_contification(
                    graph,
                    site_term,
                    binder,
                    availability,
                );
        });
        available
    }

    fn direct_free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        term: TermId,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
        availability: ContifyAvailability,
    ) -> bool {
        let mut available = true;
        graph.for_each_direct_free_var_of_term(term, |var| {
            let binder = graph.free_binder(var);
            available &= local.contains(binder)
                || self.binder_is_available_for_contification(
                    graph,
                    site_term,
                    binder,
                    availability,
                );
        });
        available
    }

    fn free_occurrences_are_available_at_term<'gc>(
        &self,
        graph: &Graph<'gc>,
        vars: impl IntoIterator<Item = FreeVar>,
        local: &EntitySet<BoundVar>,
        site_term: TermId,
        availability: ContifyAvailability,
    ) -> bool {
        vars.into_iter().all(|var| {
            let binder = graph.free_binder(var);
            local.contains(binder)
                || self.binder_is_available_for_contification(
                    graph,
                    site_term,
                    binder,
                    availability,
                )
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
                Parent::Term(parent) => term = parent,
                Parent::Func(function) => {
                    if function == owner {
                        return true;
                    }
                    let Some(definition) = self.function_owner_defs[function] else {
                        return false;
                    };
                    let Some(definition) = graph.read_term_link(definition) else {
                        return false;
                    };
                    term = definition;
                }
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
                if graph.read_expr_link(expr).is_some_and(|expr_id| {
                    !self.expr_free_occurrences_are_allowed(graph, expr_id, allowed)
                }) {
                    return false;
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
            TermKind::If(test, then_branch, else_branch, _) => {
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
    dump::GCPS_CONTIFY.next_slot_id()
}

fn contify_dump_enabled_for(source: ContifySource) -> bool {
    let words = flags::gcps_dump_contify();
    if words.is_empty() {
        return false;
    }
    let disabled = words.iter().any(|w| matches!(*w, "0" | "off" | "none" | "false"));
    let all = words.iter().any(|w| matches!(*w, "1" | "on" | "true" | "all"));
    let scc = words.iter().any(|w| matches!(*w, "scc" | "legacy"));
    let dom = words.iter().any(|w| matches!(*w, "dom" | "dominator" | "dominators"));
    if disabled {
        false
    } else if all {
        true
    } else if scc {
        source == ContifySource::Scc
    } else if dom {
        source == ContifySource::Dominator
    } else {
        // Only unknown words were given: dump everything, like the old parser.
        true
    }
}

fn emit_contification_dump(index: usize, phase: &str, dump: &str) {
    let Some(path) = dump::GCPS_CONTIFY.resolve_at(index, &format!("contify-{phase}"), "txt") else {
        // Directory unusable: keep the dump visible on stderr.
        eprint!("{dump}");
        return;
    };
    dump::GCPS_CONTIFY.write_at(&format!("contify {phase}"), &path, dump);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::cps::graph::{Function, Graph, TermKind},
        expander::core::fresh_lvar,
        runtime::{Scheme, value::Value},
    };

    #[test]
    fn nested_continuation_is_inside_lexical_function() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let outer_name = graph.new_bound_var(fresh_lvar(ctx, ctx.intern("outer")));
            let outer_return = graph.new_bound_var(fresh_lvar(ctx, ctx.intern("outer-return")));
            let inner_name = graph.new_bound_var(fresh_lvar(ctx, ctx.intern("inner")));
            let inner_return = graph.new_bound_var(fresh_lvar(ctx, ctx.intern("inner-return")));

            let inner_body = graph.new_term_link(None);
            let inner_return_occurrence = graph.new_free_occ_for_binder(inner_return, inner_body);
            let inner_args = graph.new_free_vars([]);
            let inner_body_parent = graph.new_parent_link(None);
            let inner_body_term = graph.new_term(
                inner_body_parent,
                TermKind::Continue(inner_return_occurrence, inner_args),
                Value::new(false),
            );
            graph.set_term_link(inner_body, inner_body_term);
            let inner_vars = graph.new_bound_vars([]);
            let inner = graph.new_function(Function {
                name: Value::new(false),
                source: Value::new(false),
                var: inner_name,
                vars: inner_vars,
                variadic: None,
                cont: Some(inner_return),
                is_variadic: false,
                body: inner_body,
                is_rec: false,
                unroll_count: 0,
                is_cold: false,
                is_noinline: false,
                is_reified: false,
                meta: Value::new(false),
            });
            graph.backpatch_function(inner);

            let outer_tail = graph.new_term_link(None);
            let outer_return_occurrence = graph.new_free_occ_for_binder(outer_return, outer_tail);
            let outer_args = graph.new_free_vars([]);
            let outer_tail_parent = graph.new_parent_link(None);
            let outer_tail_term = graph.new_term(
                outer_tail_parent,
                TermKind::Continue(outer_return_occurrence, outer_args),
                Value::new(false),
            );
            graph.set_term_link(outer_tail, outer_tail_term);

            let outer_body = graph.new_term_link(None);
            let inner_link = graph.new_function_link(Some(inner));
            let inner_functions = graph.new_function_links([inner_link]);
            let outer_body_parent = graph.new_parent_link(None);
            let outer_body_term = graph.new_term(
                outer_body_parent,
                TermKind::Letk(inner_functions, outer_tail),
                Value::new(false),
            );
            graph.set_term_link(outer_body, outer_body_term);
            graph.backpatch_subterms(outer_body_term, &[outer_tail]);

            let outer_vars = graph.new_bound_vars([]);
            let outer = graph.new_function(Function {
                name: Value::new(false),
                source: Value::new(false),
                var: outer_name,
                vars: outer_vars,
                variadic: None,
                cont: Some(outer_return),
                is_variadic: false,
                body: outer_body,
                is_rec: false,
                unroll_count: 0,
                is_cold: false,
                is_noinline: false,
                is_reified: false,
                meta: Value::new(false),
            });
            graph.backpatch_function(outer);

            let mut state = OptimizerState::new();
            state.function_owner_defs[inner] = Some(outer_body);

            assert!(state.term_is_inside_function(&graph, inner_body_term, outer));
            assert!(!state.term_is_inside_function(&graph, outer_tail_term, inner));
        });
    }
}
