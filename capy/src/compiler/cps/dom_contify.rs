use std::collections::HashMap;

use cranelift_entity::{EntitySet, SecondaryMap};
use petgraph::graph::NodeIndex;

use super::{
    graph::{BoundVar, FreeVar, FunctionId, FunctionLink, Graph, Subterm, TermId, TermKind},
    optimize::{ContifyCandidate, ContifySource, OptimizerState},
    scc_contify,
};

macro_rules! verbose_log {
    ($($arg:tt)*) => {
        if super::graph::VERBOSE {
            eprintln!($($arg)*);
        }
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum DomLoc {
    Root,
    Func(FunctionId),
    Cont(BoundVar),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Destination {
    Func(FunctionId),
    Cont(BoundVar),
}

#[derive(Clone, Copy, Debug)]
struct CallFact {
    caller: Option<FunctionId>,
    callee: FunctionId,
    cont: BoundVar,
    is_tail: bool,
}

struct DomFacts {
    functions_by_binder: SecondaryMap<BoundVar, Option<FunctionId>>,
    live_binders: EntitySet<BoundVar>,
    calls: Vec<CallFact>,
    escaped_binders: EntitySet<BoundVar>,
}

pub(super) fn find_candidate<'gc>(
    state: &OptimizerState,
    graph: &Graph<'gc>,
    active_link: Subterm,
    _term: TermId,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
) -> Option<ContifyCandidate> {
    let facts = collect_facts(graph, live, body);
    let reachable = reachable_functions(&facts);
    let groups = analyze_dominators(graph, live, &facts, &reachable);
    choose_candidate(state, graph, active_link, live, body, groups)
}

fn collect_facts<'gc>(
    graph: &Graph<'gc>,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
) -> DomFacts {
    let mut facts = DomFacts {
        functions_by_binder: SecondaryMap::new(),
        live_binders: EntitySet::new(),
        calls: Vec::new(),
        escaped_binders: EntitySet::new(),
    };

    for (_, function) in live.iter().copied() {
        let binder = graph[function].var;
        facts.functions_by_binder[binder] = Some(function);
        facts.live_binders.insert(binder);
    }

    collect_calls(graph, &mut facts, body, None, None);
    for (_, function) in live.iter().copied() {
        let data = graph[function];
        collect_calls(graph, &mut facts, data.body, Some(function), data.cont);
    }

    facts
}

fn collect_calls<'gc>(
    graph: &Graph<'gc>,
    facts: &mut DomFacts,
    link: Subterm,
    caller: Option<FunctionId>,
    current_return_cont: Option<BoundVar>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };

    match graph[term].kind {
        TermKind::LetVal((_, expr), body) => {
            if let Some(expr) = graph.read_expr_link(expr) {
                mark_expr_escapes(graph, facts, expr);
            }
            collect_calls(graph, facts, body, caller, current_return_cont);
        }
        TermKind::Fix(functions, body) => {
            collect_calls(graph, facts, body, caller, current_return_cont);
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
                collect_calls(graph, facts, graph[function].body, None, None);
            }
        }
        TermKind::Letk(functions, body) => {
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
                collect_calls(
                    graph,
                    facts,
                    graph[function].body,
                    caller,
                    current_return_cont,
                );
            }
            collect_calls(graph, facts, body, caller, current_return_cont);
        }
        TermKind::If(test, then_branch, else_branch, _) => {
            mark_if_live_binder_escapes(graph, facts, test);
            collect_calls(graph, facts, then_branch, caller, current_return_cont);
            collect_calls(graph, facts, else_branch, caller, current_return_cont);
        }
        TermKind::Continue(cont, vars) => {
            mark_if_live_binder_escapes(graph, facts, cont);
            mark_free_vars_escape(graph, facts, graph.free_vars_slice(&vars).iter().copied());
        }
        TermKind::Raise(_, vars) => {
            mark_free_vars_escape(graph, facts, graph.free_vars_slice(&vars).iter().copied());
        }
        TermKind::App(callee, args, cont) => {
            mark_free_vars_escape(graph, facts, graph.free_vars_slice(&args).iter().copied());
            mark_if_live_binder_escapes(graph, facts, cont);

            let callee_binder = graph.free_binder(callee);
            let Some(callee) = facts.functions_by_binder[callee_binder] else {
                return;
            };
            let cont_binder = graph.free_binder(cont);
            facts.calls.push(CallFact {
                caller,
                callee,
                cont: cont_binder,
                is_tail: current_return_cont == Some(cont_binder),
            });
        }
    }
}

fn mark_expr_escapes<'gc>(graph: &Graph<'gc>, facts: &mut DomFacts, expr: super::graph::ExprId) {
    graph.for_each_free_var_of_expr(expr, |var| mark_if_live_binder_escapes(graph, facts, var));
}

fn mark_free_vars_escape<'gc>(
    graph: &Graph<'gc>,
    facts: &mut DomFacts,
    vars: impl IntoIterator<Item = FreeVar>,
) {
    for var in vars {
        mark_if_live_binder_escapes(graph, facts, var);
    }
}

fn mark_if_live_binder_escapes<'gc>(graph: &Graph<'gc>, facts: &mut DomFacts, var: FreeVar) {
    let binder = graph.free_binder(var);
    if facts.live_binders.contains(binder) {
        facts.escaped_binders.insert(binder);
    }
}

fn reachable_functions(facts: &DomFacts) -> EntitySet<FunctionId> {
    let mut reachable = EntitySet::new();
    let mut changed = true;

    while changed {
        changed = false;
        for call in &facts.calls {
            let caller_reachable = match call.caller {
                None => true,
                Some(caller) => reachable.contains(caller),
            };
            if caller_reachable && !reachable.contains(call.callee) {
                reachable.insert(call.callee);
                changed = true;
            }
        }
    }

    reachable
}

fn analyze_dominators<'gc>(
    graph: &Graph<'gc>,
    live: &[(FunctionLink, FunctionId)],
    facts: &DomFacts,
    reachable: &EntitySet<FunctionId>,
) -> Vec<(Destination, Vec<FunctionId>)> {
    // ICFP'01 Figure 6: build G over Root, continuations, and functions.
    // Non-tail calls enter a callee from the return continuation node; tail
    // calls enter it from the caller function node.  ADom then chooses the
    // highest non-root dominator as the contification destination.
    let mut dom_graph = petgraph::Graph::<DomLoc, ()>::new();
    let mut nodes = HashMap::<DomLoc, NodeIndex>::new();
    let mut reverse = HashMap::<NodeIndex, DomLoc>::new();

    let root = add_node(&mut dom_graph, &mut nodes, &mut reverse, DomLoc::Root);
    for (_, function) in live.iter().copied() {
        add_node(
            &mut dom_graph,
            &mut nodes,
            &mut reverse,
            DomLoc::Func(function),
        );
    }

    for call in &facts.calls {
        add_node(
            &mut dom_graph,
            &mut nodes,
            &mut reverse,
            DomLoc::Cont(call.cont),
        );
    }

    let conts = nodes
        .iter()
        .filter_map(|(loc, node)| matches!(loc, DomLoc::Cont(_)).then_some(*node))
        .collect::<Vec<_>>();
    for cont in conts {
        dom_graph.add_edge(root, cont, ());
    }

    for (_, function) in live.iter().copied() {
        if !reachable.contains(function) {
            let node = nodes[&DomLoc::Func(function)];
            dom_graph.add_edge(root, node, ());
        }
    }

    for call in &facts.calls {
        if !call_is_reachable(call, reachable) {
            continue;
        }
        let callee = nodes[&DomLoc::Func(call.callee)];
        if call.is_tail {
            if let Some(caller) = call.caller {
                let caller = nodes[&DomLoc::Func(caller)];
                dom_graph.add_edge(caller, callee, ());
            } else {
                let cont = nodes[&DomLoc::Cont(call.cont)];
                dom_graph.add_edge(cont, callee, ());
            }
        } else {
            let cont = nodes[&DomLoc::Cont(call.cont)];
            dom_graph.add_edge(cont, callee, ());
        }
    }

    let dominators = petgraph::algo::dominators::simple_fast(&dom_graph, root);
    let mut groups: Vec<(Destination, Vec<FunctionId>)> = Vec::new();

    for (_, function) in live.iter().copied() {
        let binder = graph[function].var;
        if facts.escaped_binders.contains(binder) || !reachable.contains(function) {
            continue;
        }

        let node = nodes[&DomLoc::Func(function)];
        let Some(top) = topmost_non_root_dominator(&dominators, root, node) else {
            continue;
        };
        let Some(destination) = reverse.get(&top).and_then(|loc| destination_from_loc(*loc)) else {
            continue;
        };
        if matches!(destination, Destination::Func(destination) if destination == function) {
            continue;
        }
        push_group(&mut groups, destination, function);
    }

    groups.sort_by(|left, right| right.1.len().cmp(&left.1.len()));
    groups
}

fn add_node(
    graph: &mut petgraph::Graph<DomLoc, ()>,
    nodes: &mut HashMap<DomLoc, NodeIndex>,
    reverse: &mut HashMap<NodeIndex, DomLoc>,
    loc: DomLoc,
) -> NodeIndex {
    if let Some(node) = nodes.get(&loc).copied() {
        return node;
    }
    let node = graph.add_node(loc);
    nodes.insert(loc, node);
    reverse.insert(node, loc);
    node
}

fn call_is_reachable(call: &CallFact, reachable: &EntitySet<FunctionId>) -> bool {
    match call.caller {
        None => true,
        Some(caller) => reachable.contains(caller),
    }
}

fn topmost_non_root_dominator<N>(
    dominators: &petgraph::algo::dominators::Dominators<N>,
    root: N,
    node: N,
) -> Option<N>
where
    N: Copy + Eq + std::hash::Hash,
{
    let mut cursor = node;
    let mut topmost = None;
    while let Some(parent) = dominators.immediate_dominator(cursor) {
        if parent == root {
            break;
        }
        topmost = Some(parent);
        cursor = parent;
    }
    topmost
}

fn destination_from_loc(loc: DomLoc) -> Option<Destination> {
    match loc {
        DomLoc::Root => None,
        DomLoc::Func(function) => Some(Destination::Func(function)),
        DomLoc::Cont(cont) => Some(Destination::Cont(cont)),
    }
}

fn push_group(
    groups: &mut Vec<(Destination, Vec<FunctionId>)>,
    destination: Destination,
    function: FunctionId,
) {
    for (existing, functions) in groups.iter_mut() {
        if *existing == destination {
            functions.push(function);
            return;
        }
    }
    groups.push((destination, vec![function]));
}

fn choose_candidate<'gc>(
    state: &OptimizerState,
    graph: &Graph<'gc>,
    active_link: Subterm,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
    groups: Vec<(Destination, Vec<FunctionId>)>,
) -> Option<ContifyCandidate> {
    for (destination, functions) in groups {
        let mut binders = EntitySet::new();
        for function in &functions {
            binders.insert(graph[*function].var);
        }

        let return_cont = match destination {
            Destination::Cont(cont) => cont,
            Destination::Func(function) => {
                if binders.contains(graph[function].var) {
                    continue;
                }
                let Some(return_cont) = graph[function].cont else {
                    continue;
                };
                return_cont
            }
        };
        let site = scc_contify::contification_site(graph, active_link, live, body, &binders)?;

        let Some(insertion) =
            state.choose_contification_insertion(graph, site, return_cont, &binders, &functions)
        else {
            verbose_log!("gcps dom contify: skip contification out of target scope");
            continue;
        };

        return Some(ContifyCandidate {
            binders,
            return_cont,
            insertion,
            source: ContifySource::Dominator,
        });
    }

    None
}
