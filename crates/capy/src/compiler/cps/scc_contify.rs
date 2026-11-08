//! Contification via SCCs over the call graph.

use cranelift_entity::{EntitySet, SecondaryMap};
use std::collections::HashMap;

use super::graph::{BoundVar, FunctionId, FunctionLink, Graph, Subterm, TermKind};
use super::optimize::{ContifyCandidate, ContifySource, OptimizerState};

#[derive(Clone, Copy)]
struct CallFact {
    caller: Option<FunctionId>,
    proc: Option<FunctionId>,
    callee: FunctionId,
    ret: BoundVar,
    arg_count: usize,
}

struct Facts {
    by_binder: HashMap<BoundVar, FunctionId>,
    candidates: EntitySet<FunctionId>,
    calls: Vec<CallFact>,
    returns: SecondaryMap<FunctionId, EntitySet<BoundVar>>,
    tails: HashMap<FunctionId, BoundVar>,
}

pub(super) fn find_candidate<'gc>(
    state: &OptimizerState,
    graph: &Graph<'gc>,
    active_link: Subterm,
    _term: super::graph::TermId,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
) -> Option<ContifyCandidate> {
    let facts = collect_facts(graph, live, body);
    let groups = call_sccs(&facts);

    for group in groups {
        if let Some(candidate) = attempt(state, graph, active_link, live, body, &facts, &group) {
            return Some(candidate);
        }
        if group.len() > 1 {
            for function in group {
                if let Some(candidate) =
                    attempt(state, graph, active_link, live, body, &facts, &[function])
                {
                    return Some(candidate);
                }
            }
        }
    }

    None
}

fn collect_facts<'gc>(
    graph: &Graph<'gc>,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
) -> Facts {
    let mut facts = Facts {
        by_binder: HashMap::new(),
        candidates: EntitySet::new(),
        calls: Vec::new(),
        returns: SecondaryMap::new(),
        tails: HashMap::new(),
    };

    for (_, function) in live.iter().copied() {
        facts.by_binder.insert(graph[function].var, function);
    }

    for (_, function) in live.iter().copied() {
        let data = graph[function];
        if data.cont.is_none()
            || data.is_variadic
            || data.variadic.is_some()
            || data.case_group.is_some()
        {
            continue;
        }
        if !all_uses_are_calls(graph, data.var) {
            continue;
        }
        facts.candidates.insert(function);
        facts.tails.insert(
            function,
            data.cont.expect("lambda has a return continuation"),
        );
        facts.returns[function] = EntitySet::new();
    }

    collect_calls(graph, &mut facts, body, None, None);
    for (_, function) in live.iter().copied() {
        let data = graph[function];
        collect_calls(graph, &mut facts, data.body, Some(function), Some(function));
    }

    // Drop candidates with a call whose argument count does not match.
    let mut bad = EntitySet::new();
    for call in &facts.calls {
        if facts.candidates.contains(call.callee) {
            let arity = graph.bound_vars_slice(&graph[call.callee].vars).len();
            if call.arg_count != arity {
                bad.insert(call.callee);
            }
        }
    }
    for function in bad.iter() {
        facts.candidates.remove(function);
    }

    facts
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

fn collect_calls<'gc>(
    graph: &Graph<'gc>,
    facts: &mut Facts,
    link: Subterm,
    caller: Option<FunctionId>,
    proc: Option<FunctionId>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    match graph[term].kind {
        TermKind::LetVal(_, body) => collect_calls(graph, facts, body, caller, proc),
        TermKind::Letk(functions, body) => {
            for function in graph.live_functions(&functions) {
                collect_calls(graph, facts, graph[function].body, caller, proc);
            }
            collect_calls(graph, facts, body, caller, proc);
        }
        TermKind::Fix(functions, body) => {
            for function in graph.live_functions(&functions) {
                collect_calls(
                    graph,
                    facts,
                    graph[function].body,
                    Some(function),
                    Some(function),
                );
            }
            collect_calls(graph, facts, body, caller, proc);
        }
        TermKind::If(_, consequent, alternative, _) => {
            collect_calls(graph, facts, consequent, caller, proc);
            collect_calls(graph, facts, alternative, caller, proc);
        }
        TermKind::App(callee, args, cont) => {
            let callee_binder = graph.free_binder(callee);
            let Some(&callee) = facts.by_binder.get(&callee_binder) else {
                return;
            };
            let ret = graph.free_binder(cont);
            facts.calls.push(CallFact {
                caller,
                proc,
                callee,
                ret,
                arg_count: graph.free_vars_slice(&args).len(),
            });
            if facts.candidates.contains(callee) {
                facts.returns[callee].insert(ret);
            }
        }
        TermKind::Continue(..) | TermKind::BranchPrim(..) | TermKind::Raise(..) => {}
    }
}

fn call_sccs(facts: &Facts) -> Vec<Vec<FunctionId>> {
    let candidates: Vec<FunctionId> = facts.candidates.iter().collect();
    let mut index = HashMap::new();
    let mut call_graph = petgraph::Graph::<FunctionId, ()>::new();
    for function in candidates.iter().copied() {
        let node = call_graph.add_node(function);
        index.insert(function, node);
    }
    for call in &facts.calls {
        let Some(caller) = call.caller else {
            continue;
        };
        if !facts.candidates.contains(caller) || !facts.candidates.contains(call.callee) {
            continue;
        }
        call_graph.add_edge(index[&caller], index[&call.callee], ());
    }

    let mut groups: Vec<Vec<FunctionId>> = petgraph::algo::kosaraju_scc(&call_graph)
        .into_iter()
        .map(|component| component.into_iter().map(|node| call_graph[node]).collect())
        .collect();

    groups.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a[0].cmp(&b[0])));
    groups
}

#[allow(clippy::too_many_arguments)]
fn attempt<'gc>(
    state: &OptimizerState,
    graph: &Graph<'gc>,
    active_link: Subterm,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
    facts: &Facts,
    group: &[FunctionId],
) -> Option<ContifyCandidate> {
    let mut binders = EntitySet::new();
    let mut returns = EntitySet::new();
    let mut tails = EntitySet::new();
    for function in group.iter().copied() {
        if !facts.candidates.contains(function) {
            return None;
        }
        binders.insert(graph[function].var);
        returns.extend(facts.returns[function].iter());
        tails.insert(facts.tails[&function]);
    }
    for tail in tails.iter() {
        returns.remove(tail);
    }

    let mut iter = returns.iter();
    let return_cont = iter.next()?;
    if iter.next().is_some() {
        return None;
    }

    let mut proc = None;
    let mut first = true;
    for call in &facts.calls {
        if !group.contains(&call.callee) {
            continue;
        }
        if first {
            proc = call.proc;
            first = false;
        } else if call.proc != proc {
            return None;
        }
    }

    let site = contification_site(graph, active_link, live, body, &binders)?;
    let functions = group.to_vec();
    let insertion =
        state.choose_contification_insertion(graph, site, return_cont, &binders, &functions)?;

    Some(ContifyCandidate {
        binders,
        return_cont,
        insertion,
        source: ContifySource::Scc,
    })
}

#[derive(Clone, Copy)]
struct PushSearch {
    count: usize,
    site: Option<Subterm>,
}

impl PushSearch {
    fn none() -> Self {
        Self {
            count: 0,
            site: None,
        }
    }

    fn here(site: Subterm) -> Self {
        Self {
            count: 1,
            site: Some(site),
        }
    }
}

/// Find the term where the contified group must be inserted: the nearest
/// common position of every call to a group member.
pub(super) fn contification_site<'gc>(
    graph: &Graph<'gc>,
    active_link: Subterm,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
    binders: &EntitySet<BoundVar>,
) -> Option<Subterm> {
    let mut children = Vec::new();
    for (_, function) in live {
        if binders.contains(graph[*function].var) {
            continue;
        }
        children.push(find_push_site(graph, graph[*function].body, binders));
    }
    children.push(find_push_site(graph, body, binders));
    combine_push_sites(active_link, children).site
}

fn find_push_site<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    binders: &EntitySet<BoundVar>,
) -> PushSearch {
    let Some(term) = graph.read_term_link(link) else {
        return PushSearch::none();
    };

    match graph[term].kind {
        TermKind::LetVal(_, body) => find_push_site(graph, body, binders),
        TermKind::Letk(functions, body) | TermKind::Fix(functions, body) => {
            let mut children = Vec::new();
            for function in graph.live_functions(&functions) {
                children.push(find_push_site(graph, graph[function].body, binders));
            }
            children.push(find_push_site(graph, body, binders));
            combine_push_sites(link, children)
        }
        TermKind::If(_, then_branch, else_branch, _) => combine_push_sites(
            link,
            [
                find_push_site(graph, then_branch, binders),
                find_push_site(graph, else_branch, binders),
            ],
        ),
        TermKind::App(callee, ..) if binders.contains(graph.free_binder(callee)) => {
            PushSearch::here(link)
        }
        TermKind::App(..)
        | TermKind::Continue(..)
        | TermKind::Raise(..)
        | TermKind::BranchPrim(..) => PushSearch::none(),
    }
}

fn combine_push_sites(
    current: Subterm,
    children: impl IntoIterator<Item = PushSearch>,
) -> PushSearch {
    let mut active = children.into_iter().filter(|child| child.count != 0);
    let Some(first) = active.next() else {
        return PushSearch::none();
    };
    match active.next() {
        None => first,
        Some(second) => {
            let count = first.count + second.count + active.map(|child| child.count).sum::<usize>();
            PushSearch {
                count,
                site: Some(current),
            }
        }
    }
}
