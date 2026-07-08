use cranelift_entity::{EntitySet, SecondaryMap};

use crate::cps::SingleValueSet;

use super::{
    graph::{BoundVar, FreeVar, FunctionId, FunctionLink, Graph, Subterm, TermId, TermKind},
    optimize::{ContifyCandidate, ContifySource, OptimizerState},
};

macro_rules! verbose_log {
    ($($arg:tt)*) => {
        if super::graph::VERBOSE {
            eprintln!($($arg)*);
        }
    };
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

pub(super) fn find_candidate<'gc>(
    state: &OptimizerState,
    graph: &Graph<'gc>,
    active_link: Subterm,
    term: TermId,
    live: &[(FunctionLink, FunctionId)],
    body: Subterm,
) -> Option<ContifyCandidate> {
    for binders in tailcall_sccs(graph, live) {
        let SingleValueSet::Singleton(return_cont) =
            common_return_cont(graph, term, &binders, None)
        else {
            continue;
        };

        let site = contification_site(graph, active_link, live, body, &binders)?;
        let functions = live
            .iter()
            .filter_map(|(_, function)| binders.contains(graph[*function].var).then_some(*function))
            .collect::<Vec<_>>();
        let Some(insertion) =
            state.choose_contification_insertion(graph, site, return_cont, &binders, &functions)
        else {
            verbose_log!("gcps scc contify: skip contification out of target scope");
            continue;
        };

        return Some(ContifyCandidate {
            binders,
            return_cont,
            insertion,
            source: ContifySource::Scc,
        });
    }

    None
}

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

fn tailcall_sccs<'gc>(
    graph: &Graph<'gc>,
    live: &[(FunctionLink, FunctionId)],
) -> Vec<EntitySet<BoundVar>> {
    let mut call_graph = petgraph::Graph::<BoundVar, ()>::new();
    let mut node_map: SecondaryMap<BoundVar, Option<petgraph::graph::NodeIndex>> =
        SecondaryMap::new();
    let mut names = EntitySet::new();
    names.extend(live.iter().map(|(_, function)| graph[*function].var));

    for (_, function) in live {
        let binder = graph[*function].var;
        let node = call_graph.add_node(binder);
        node_map[binder] = Some(node);
    }

    for (_, function) in live {
        let data = graph[*function];
        let Some(return_cont) = data.cont else {
            continue;
        };
        let Some(from) = node_map[data.var] else {
            continue;
        };
        for callee in tailcalls(graph, data.body, return_cont).iter() {
            if !names.contains(callee) {
                continue;
            }
            let Some(to) = node_map[callee] else {
                continue;
            };
            call_graph.add_edge(from, to, ());
        }
    }

    petgraph::algo::kosaraju_scc(&call_graph)
        .into_iter()
        .map(|scc| {
            let mut binders = EntitySet::new();
            binders.extend(scc.into_iter().map(|node| call_graph[node]));
            binders
        })
        .collect()
}

fn tailcalls<'gc>(graph: &Graph<'gc>, link: Subterm, return_cont: BoundVar) -> EntitySet<BoundVar> {
    let mut out = EntitySet::new();
    collect_tailcalls(graph, link, return_cont, &mut out);
    out
}

fn collect_tailcalls<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    return_cont: BoundVar,
    out: &mut EntitySet<BoundVar>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };

    match graph[term].kind {
        TermKind::LetVal(_, body) | TermKind::Fix(_, body) => {
            collect_tailcalls(graph, body, return_cont, out);
        }
        TermKind::Letk(functions, body) => {
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
                collect_tailcalls(graph, graph[function].body, return_cont, out);
            }
            collect_tailcalls(graph, body, return_cont, out);
        }
        TermKind::If(_, then_branch, else_branch, _) => {
            collect_tailcalls(graph, then_branch, return_cont, out);
            collect_tailcalls(graph, else_branch, return_cont, out);
        }
        TermKind::App(callee, _, cont) if graph.free_binder(cont) == return_cont => {
            out.insert(graph.free_binder(callee));
        }
        TermKind::App(..) | TermKind::Continue(..) | TermKind::Raise(..) => {}
    }
}

fn common_return_cont<'gc>(
    graph: &Graph<'gc>,
    term: TermId,
    binders: &EntitySet<BoundVar>,
    ignore: Option<BoundVar>,
) -> SingleValueSet<BoundVar> {
    match graph[term].kind {
        TermKind::LetVal((_, expr), body) => {
            if let Some(expr) = graph.read_expr_link(expr) {
                if expr_free_vars_contain_any(graph, expr, binders) {
                    return SingleValueSet::Top;
                }
            }
            common_return_cont_link(graph, body, binders, ignore)
        }
        TermKind::Letk(functions, body) => {
            let mut acc = common_return_cont_link(graph, body, binders, ignore);
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
                acc = acc.join(common_return_cont_link(
                    graph,
                    graph[function].body,
                    binders,
                    ignore,
                ));
            }
            acc
        }
        TermKind::Fix(functions, body) => {
            let mut acc = common_return_cont_link(graph, body, binders, ignore);
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
                let data = graph[function];
                let function_ignore = if binders.contains(data.var) {
                    data.cont
                } else {
                    ignore
                };
                acc = acc.join(common_return_cont_link(
                    graph,
                    data.body,
                    binders,
                    function_ignore,
                ));
            }
            acc
        }
        TermKind::If(test, then_branch, else_branch, _) => {
            if binders.contains(graph.free_binder(test)) {
                return SingleValueSet::Top;
            }
            common_return_cont_link(graph, then_branch, binders, ignore)
                .join(common_return_cont_link(graph, else_branch, binders, ignore))
        }
        TermKind::Continue(_, args) => {
            if free_vars_contain_any(graph, graph.free_vars_slice(&args).iter().copied(), binders) {
                SingleValueSet::Top
            } else {
                SingleValueSet::Bottom
            }
        }
        TermKind::Raise(_, args) => {
            if free_vars_contain_any(graph, graph.free_vars_slice(&args).iter().copied(), binders) {
                SingleValueSet::Top
            } else {
                SingleValueSet::Bottom
            }
        }
        TermKind::App(callee, args, cont) => {
            if free_vars_contain_any(graph, graph.free_vars_slice(&args).iter().copied(), binders) {
                return SingleValueSet::Top;
            }

            let callee = graph.free_binder(callee);
            let return_cont = graph.free_binder(cont);
            if binders.contains(callee) && ignore != Some(return_cont) {
                SingleValueSet::Singleton(return_cont)
            } else {
                SingleValueSet::Bottom
            }
        }
    }
}

fn common_return_cont_link<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    binders: &EntitySet<BoundVar>,
    ignore: Option<BoundVar>,
) -> SingleValueSet<BoundVar> {
    let Some(term) = graph.read_term_link(link) else {
        return SingleValueSet::Bottom;
    };
    common_return_cont(graph, term, binders, ignore)
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
            for link in graph.function_links_slice(&functions).iter().copied() {
                let Some(function) = graph.read_function_link(link) else {
                    continue;
                };
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
        TermKind::App(..) | TermKind::Continue(..) | TermKind::Raise(..) => PushSearch::none(),
    }
}

fn combine_push_sites(
    current: Subterm,
    children: impl IntoIterator<Item = PushSearch>,
) -> PushSearch {
    let mut count = 0;
    let mut site = None;
    for child in children {
        if child.count == 0 {
            continue;
        }
        count += child.count;
        if site.is_none() {
            site = child.site;
        }
    }

    match count {
        0 => PushSearch::none(),
        1 => PushSearch { count, site },
        _ => PushSearch::here(current),
    }
}

fn free_vars_contain_any<'gc>(
    graph: &Graph<'gc>,
    vars: impl IntoIterator<Item = FreeVar>,
    binders: &EntitySet<BoundVar>,
) -> bool {
    vars.into_iter()
        .any(|var| binders.contains(graph.free_binder(var)))
}

fn expr_free_vars_contain_any<'gc>(
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
