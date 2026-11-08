//! Inlining for graph CPS.

use cranelift_entity::EntitySet;
use std::collections::HashMap;

use crate::runtime::MutationContext;

use super::clone::GraphClone;
use super::graph::{BoundVar, FreeVar, FunctionId, FunctionLink, Graph, Subterm, TermKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InlineMode {
    Off,
    Leaf,
    Known,
    Both,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct InlineStats {
    pub calls_inlined: usize,
    pub functions_inlined: usize,
    pub dead_functions: usize,
}

impl std::ops::AddAssign for InlineStats {
    fn add_assign(&mut self, other: Self) {
        self.calls_inlined += other.calls_inlined;
        self.functions_inlined += other.functions_inlined;
        self.dead_functions += other.dead_functions;
    }
}

impl InlineMode {
    pub fn from_flag(value: &str) -> Self {
        match value {
            "leaf" => Self::Leaf,
            "known" | "nonrecursive" => Self::Known,
            "both" | "all" | "1" | "on" | "true" => Self::Both,
            _ => Self::Off,
        }
    }
}

#[derive(Clone, Copy)]
struct CallSite {
    link: Subterm,
    caller: Option<FunctionId>,
}

struct FunctionInfo {
    id: FunctionId,
    binder: BoundVar,
    def_link: FunctionLink,
    body: Subterm,
    size: usize,
    calls: usize,
    has_loop: bool,
    all_uses_are_calls: bool,
    call_sites: Vec<CallSite>,
}

/// Inline eligible calls in `graph`.
pub fn inline_graph<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    entry: FunctionId,
    mode: InlineMode,
    max_size: usize,
    small: usize,
    product: usize,
) -> InlineStats {
    if mode == InlineMode::Off {
        return InlineStats::default();
    }

    let mut functions = Vec::new();
    collect_functions(graph, graph[entry].body, &mut functions);

    let mut infos: HashMap<BoundVar, FunctionInfo> = HashMap::new();
    for (id, def_link) in functions {
        let data = graph[id];
        if data.cont.is_none()
            || id == entry
            || data.is_variadic
            || data.variadic.is_some()
            || data.case_group.is_some()
            || data.is_noinline
            || data.is_cold
        {
            continue;
        }
        let binder = graph.function_binder_rooted(data.var);
        infos.insert(
            binder,
            FunctionInfo {
                id,
                binder,
                def_link,
                body: data.body,
                size: 0,
                calls: 0,
                has_loop: false,
                all_uses_are_calls: all_uses_are_calls(graph, binder),
                call_sites: Vec::new(),
            },
        );
    }

    for info in infos.values_mut() {
        let mut scan = BodyScan {
            terms: 0,
            exprs: 0,
            calls: 0,
            has_loop: false,
        };
        scan_body(graph, info.body, &mut scan);
        info.size = scan.terms + scan.exprs;
        info.calls = scan.calls;
        info.has_loop = scan.has_loop;
    }

    // All call sites in the program, keyed by callee binder.
    let mut sites_by_callee: HashMap<BoundVar, Vec<CallSite>> = HashMap::new();
    collect_call_sites(graph, graph[entry].body, None, &mut sites_by_callee);
    for (binder, info) in infos.iter_mut() {
        info.call_sites = sites_by_callee.remove(binder).unwrap_or_default();
    }

    // Non-recursive eligibility from the call graph.
    let mut self_calls = EntitySet::new();
    for (binder, info) in infos.iter() {
        if info
            .call_sites
            .iter()
            .any(|site| site.caller == Some(info.id))
        {
            self_calls.insert(*binder);
        }
    }
    let mutually_recursive = recursive_binders(graph, &infos);

    let mut eligible = Vec::new();
    for (binder, info) in infos.iter() {
        if !info.all_uses_are_calls || info.call_sites.is_empty() {
            continue;
        }
        if self_calls.contains(*binder) || mutually_recursive.contains(*binder) {
            continue;
        }
        let leaf = info.calls == 0 && !info.has_loop && info.size <= max_size;
        let known = !self_calls.contains(*binder)
            && ((info.call_sites.len() - 1) * (info.size.saturating_sub(small))) <= product;
        let allow = match mode {
            InlineMode::Leaf => leaf,
            InlineMode::Known => known,
            InlineMode::Both => leaf || known,
            InlineMode::Off => false,
        };
        if allow {
            eligible.push(*binder);
        }
    }
    eligible.sort();

    let mut stats = InlineStats::default();
    for binder in eligible {
        let Some(info) = infos.get(&binder) else {
            continue;
        };
        let callee = info.id;
        let sites = info.call_sites.clone();

        let mut now = BodyScan {
            terms: 0,
            exprs: 0,
            calls: 0,
            has_loop: false,
        };
        scan_body(graph, graph[callee].body, &mut now);

        let size = now.terms + now.exprs;
        let leaf = now.calls == 0 && !now.has_loop && size <= max_size;
        let known = (sites.len().saturating_sub(1)) * size.saturating_sub(small) <= product;
        let allow = match mode {
            InlineMode::Leaf => leaf,
            InlineMode::Known => known,
            InlineMode::Both => leaf || known,
            InlineMode::Off => false,
        };

        if !allow {
            continue;
        }

        let mut inlined_here = false;

        for site in sites {
            if inline_site(ctx, graph, callee, site.link) {
                stats.calls_inlined += 1;
                inlined_here = true;
            }
        }

        if inlined_here {
            stats.functions_inlined += 1;
        }

        // All uses were calls, so an empty ring means the closure is dead.
        if graph.collect_occurrences(binder).is_empty() {
            let Some(info) = infos.get(&binder) else {
                continue;
            };
            graph.clear_function_link(info.def_link);
            stats.dead_functions += 1;
        }
    }

    stats
}

fn inline_site<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    callee: FunctionId,
    site: Subterm,
) -> bool {
    let Some(term) = graph.read_term_link(site) else {
        return false;
    };
    let TermKind::App(callee_occ, args, cont) = graph[term].kind else {
        return false;
    };
    if graph.free_binder_rooted(callee_occ) != graph.function_binder_rooted(graph[callee].var) {
        return false;
    }

    let data = graph[callee];
    let formals = graph.bound_vars_slice(&data.vars).to_vec();
    let actuals: Vec<FreeVar> = graph.free_vars_slice(&args).to_vec();
    if formals.len() != actuals.len() {
        return false;
    }

    let mut substitutions: Vec<(BoundVar, BoundVar)> = Vec::with_capacity(formals.len() + 1);
    for (formal, actual) in formals.iter().zip(actuals.iter()) {
        substitutions.push((*formal, graph.free_binder_rooted(*actual)));
    }
    let Some(return_cont) = data.cont else {
        return false;
    };
    substitutions.push((return_cont, graph.free_binder_rooted(cont)));

    let mut old_occurrences = Vec::new();
    graph.push_direct_free_vars_of_term(term, &mut old_occurrences);
    let parent = graph.read_parent_link(graph[term].link);

    let cloned = {
        let mut cloner = GraphClone::with_substitutions(ctx, graph, substitutions);
        let Some(cloned) = cloner.clone_subterm_into(data.body, site) else {
            return false;
        };
        cloned
    };

    for occurrence in old_occurrences {
        graph.remove_free_occ(occurrence);
    }
    if let Some(parent) = parent {
        graph.set_parent_link(graph[cloned].link, parent);
    }

    true
}

struct BodyScan {
    terms: usize,
    exprs: usize,
    calls: usize,
    has_loop: bool,
}

/// Count the body's terms and calls. `Fix` member bodies are separate
/// procedures and are not part of this function's size.
fn scan_body<'gc>(graph: &Graph<'gc>, link: Subterm, scan: &mut BodyScan) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    scan.terms += 1;
    match graph[term].kind {
        TermKind::LetVal((_, expr), body) => {
            scan.exprs += 1;
            if let Some(expr) = graph.read_expr_link(expr) {
                if let super::graph::ExprKind::PrimCall(_, _) = graph[expr].kind {
                    scan.exprs += 1;
                }
            }
            scan_body(graph, body, scan);
        }
        TermKind::Letk(functions, body) => {
            for function in graph.live_functions(&functions) {
                if graph[function].is_rec {
                    scan.has_loop = true;
                }
                scan_body(graph, graph[function].body, scan);
            }
            scan_body(graph, body, scan);
        }
        TermKind::Fix(functions, body) => {
            let (calls, has_loop) = (scan.calls, scan.has_loop);
            for function in graph.live_functions(&functions) {
                scan_body(graph, graph[function].body, scan);
            }
            scan.calls = calls;
            scan.has_loop = has_loop;
            scan_body(graph, body, scan);
        }
        TermKind::If(_, consequent, alternative, _) => {
            scan_body(graph, consequent, scan);
            scan_body(graph, alternative, scan);
        }
        TermKind::App(..) => scan.calls += 1,
        TermKind::Continue(..) | TermKind::BranchPrim(..) | TermKind::Raise(..) => {}
    }
}

fn all_uses_are_calls(graph: &Graph<'_>, binder: BoundVar) -> bool {
    let mut ok = true;
    graph.for_each_occurrence(binder, |occ| {
        if !ok {
            return;
        }
        let Some(term) = graph.read_term_link(graph.free_owner(occ)) else {
            ok = false;
            return;
        };
        match graph[term].kind {
            TermKind::App(callee, _, _) if callee == occ => {}
            _ => ok = false,
        }
    });
    ok
}

fn collect_functions<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    out: &mut Vec<(FunctionId, FunctionLink)>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    match graph[term].kind {
        TermKind::LetVal(_, body) => collect_functions(graph, body, out),
        TermKind::Letk(functions, body) | TermKind::Fix(functions, body) => {
            for link in graph.function_links_slice(&functions).iter().copied() {
                if let Some(function) = graph.read_function_link(link) {
                    out.push((function, link));
                    collect_functions(graph, graph[function].body, out);
                }
            }
            collect_functions(graph, body, out);
        }
        TermKind::If(_, consequent, alternative, _) => {
            collect_functions(graph, consequent, out);
            collect_functions(graph, alternative, out);
        }
        TermKind::App(..)
        | TermKind::Continue(..)
        | TermKind::BranchPrim(..)
        | TermKind::Raise(..) => {}
    }
}

fn collect_call_sites<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    caller: Option<FunctionId>,
    out: &mut HashMap<BoundVar, Vec<CallSite>>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    match graph[term].kind {
        TermKind::LetVal(_, body) => collect_call_sites(graph, body, caller, out),
        TermKind::Letk(functions, body) | TermKind::Fix(functions, body) => {
            for function in graph.live_functions(&functions) {
                // A continuation is a block of the function around it: a call
                // inside one is a call made by that function.
                let owner = if graph[function].cont.is_some() {
                    Some(function)
                } else {
                    caller
                };
                collect_call_sites(graph, graph[function].body, owner, out);
            }
            collect_call_sites(graph, body, caller, out);
        }
        TermKind::If(_, consequent, alternative, _) => {
            collect_call_sites(graph, consequent, caller, out);
            collect_call_sites(graph, alternative, caller, out);
        }
        TermKind::App(callee, _, _) => {
            let binder = graph.free_binder_rooted(callee);
            out.entry(binder)
                .or_default()
                .push(CallSite { link, caller });
        }
        TermKind::Continue(..) | TermKind::BranchPrim(..) | TermKind::Raise(..) => {}
    }
}

/// Binders that participate in a call-graph cycle (self or mutual recursion).
/// An edge runs from the function making a call to the function it calls.
fn recursive_binders<'gc>(
    graph: &Graph<'gc>,
    infos: &HashMap<BoundVar, FunctionInfo>,
) -> EntitySet<BoundVar> {
    let mut nodes: HashMap<BoundVar, petgraph::graph::NodeIndex> = HashMap::new();
    let mut call_graph = petgraph::Graph::<BoundVar, ()>::new();
    let mut node_of = |call_graph: &mut petgraph::Graph<BoundVar, ()>, binder: BoundVar| {
        *nodes
            .entry(binder)
            .or_insert_with(|| call_graph.add_node(binder))
    };
    for binder in infos.keys().copied() {
        node_of(&mut call_graph, binder);
    }
    for (binder, info) in infos.iter() {
        let to = node_of(&mut call_graph, *binder);
        for site in &info.call_sites {
            let Some(caller) = site.caller else {
                continue;
            };
            let from_binder = graph.function_binder_rooted(graph[caller].var);
            let from = node_of(&mut call_graph, from_binder);
            call_graph.add_edge(from, to, ());
        }
    }

    let mut recursive = EntitySet::new();
    for component in petgraph::algo::kosaraju_scc(&call_graph) {
        if component.len() > 1 {
            for node in component {
                recursive.insert(call_graph[node]);
            }
        }
    }
    recursive
}
