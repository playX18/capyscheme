//! Merge nested non-interleaved `Fix`/`Letk` groups.
//!
//! Environment sharing is per-site, so lambdas and reified continuations bound
//! at the same point must be one group. The TreeIL fixing-letrec passes emit a
//! separate single-member `Fix`/`Letk` node per lambda (e.g. `Fix(f, Fix(g,
//! Fix(h, body)))`); this pass merges such chains into one `Fix` group, which
//! is semantics-preserving: the same names stay bound over the same body.
//!
//! Non-reified continuations (contified local blocks) never allocate and stay
//! in a trailing `Letk`. A merged group is always a `Fix`, since only `Fix`
//! may hold function members. The merge runs after reify and before the
//! sharing analysis and lowering, so it does not disturb optimizer decisions.

use std::collections::HashSet;

use super::graph::{FunctionLink, FunctionLinks, Graph, Parent, Subterm, TermId, TermKind};

#[cfg(test)]
use super::graph::Function;

/// Merge every adjacent chain of `Fix`/`Letk` terms reachable from `root`
/// into a single `Fix` group (functions + reified continuations), keeping
/// non-reified continuations in a trailing `Letk`.
pub fn merge_nested_groups<'gc>(graph: &mut Graph<'gc>, root: Subterm) {
    let mut visited = HashSet::new();
    walk_term(graph, root, &mut visited);

    if std::env::var("CAPY_SHARE_DUMP").map_or(false, |v| v != "0") {
        let mut groups = 0usize;
        let mut merged = 0usize;
        let mut visited2 = HashSet::new();
        dump_groups(graph, root, &mut visited2, &mut groups, &mut merged);
        eprintln!("fixmerge: {groups} groups after merge, {merged} merged");
    }
}

fn dump_groups<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    visited: &mut HashSet<TermId>,
    groups: &mut usize,
    merged: &mut usize,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    if !visited.insert(term) {
        return;
    }
    match graph[term].kind {
        TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
            let live: Vec<_> = graph
                .function_links_slice(&functions)
                .iter()
                .copied()
                .filter_map(|l| graph.read_function_link(l))
                .collect();
            if live.len() > 1 {
                *merged += 1;
            }
            *groups += 1;
            eprintln!(
                "fixmerge: group with {} live members{}",
                live.len(),
                if live.len() > 1 {
                    ": merged group"
                } else {
                    ""
                }
            );
            for f in live {
                dump_groups(graph, graph[f].body, visited, groups, merged);
            }
            dump_groups(graph, body, visited, groups, merged);
        }
        TermKind::LetVal(_, body) => dump_groups(graph, body, visited, groups, merged),
        TermKind::If(_, a, b, _) => {
            dump_groups(graph, a, visited, groups, merged);
            dump_groups(graph, b, visited, groups, merged);
        }
        _ => {}
    }
}

fn walk_term<'gc>(graph: &mut Graph<'gc>, link: Subterm, visited: &mut HashSet<TermId>) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    if !visited.insert(term) {
        return;
    }
    match graph[term].kind {
        TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
            merge_chain(graph, term, functions, body, visited);
        }
        TermKind::LetVal(_, body) => walk_term(graph, body, visited),
        TermKind::If(_, consequent, alternative, _) => {
            walk_term(graph, consequent, visited);
            walk_term(graph, alternative, visited);
        }
        TermKind::Continue(..) | TermKind::App(..) | TermKind::Raise(..) => {}
    }
}

/// Merge the maximal chain of adjacent `Fix`/`Letk` terms starting at `term`
/// into one `Fix` group: functions and reified continuations become group
/// members; non-reified continuations stay bound over the final body by a
/// trailing `Letk`. Then recurse into every member body and the final body.
fn merge_chain<'gc>(
    graph: &mut Graph<'gc>,
    term: TermId,
    functions: FunctionLinks,
    body: Subterm,
    visited: &mut HashSet<TermId>,
) {
    let mut group: Vec<FunctionLink> = Vec::new();
    let mut keep: Vec<FunctionLink> = Vec::new();
    let mut bodies: Vec<Subterm> = Vec::new();

    absorb_members(graph, functions, &mut group, &mut keep, &mut bodies);

    // Absorb the chain of Fix/Letk body terms. The first absorbed node is
    // reused as the trailing `Letk` when non-reified continuations survive
    // (its parent link already points at `term`).
    let mut chain_body = body;
    let mut trailing: Option<TermId> = None;
    loop {
        let Some(body_term) = graph.read_term_link(chain_body) else {
            break;
        };
        let (TermKind::Fix(inner, inner_body) | TermKind::Letk(inner, inner_body)) =
            graph[body_term].kind
        else {
            break;
        };
        trailing.get_or_insert(body_term);
        absorb_members(graph, inner, &mut group, &mut keep, &mut bodies);
        chain_body = inner_body;
    }

    // The whole chain holds only non-reified continuations (or dead links):
    // no closure site to merge, leave the chain untouched and walk it as-is.
    if group.is_empty() {
        for member_body in bodies {
            walk_term(graph, member_body, visited);
        }
        walk_term(graph, body, visited);
        return;
    }

    // The merged group is always a `Fix`: only `Fix` may hold function
    // members (the CFG `Letk` path only emits reified-continuation closures
    // and contified local blocks).
    let group_links = graph.new_function_links(group);
    if keep.is_empty() {
        graph[term].kind = TermKind::Fix(group_links, chain_body);
    } else {
        let keep_links = graph.new_function_links(keep);
        let trailing_kind = TermKind::Letk(keep_links, chain_body);
        match trailing {
            Some(node) => {
                // Reuse the first chain node: the head's body link already
                // points at it.
                graph[node].kind = trailing_kind;
                graph[term].kind = TermKind::Fix(group_links, body);
            }
            None => {
                let parent = graph.new_parent_link(None);
                let node = graph.new_term(parent, trailing_kind, graph[term].source);
                graph.set_parent_link(parent, Parent::Term(term));
                let trailing_link = graph.new_term_link(Some(node));
                graph[term].kind = TermKind::Fix(group_links, trailing_link);
            }
        }
    }

    // Walk the merged members' bodies, the kept continuations' bodies, and
    // the final body: nested chains merge recursively.
    for member_body in bodies {
        walk_term(graph, member_body, visited);
    }
    walk_term(graph, chain_body, visited);
}

/// Partition one group's member links: live functions and reified
/// continuations join the merged group; live non-reified continuations are
/// kept for the trailing `Letk`. Dead links stay with the group (consumers
/// filter live members). Member bodies are collected for the recursive walk.
fn absorb_members<'gc>(
    graph: &Graph<'gc>,
    functions: FunctionLinks,
    group: &mut Vec<FunctionLink>,
    keep: &mut Vec<FunctionLink>,
    bodies: &mut Vec<Subterm>,
) {
    for link in graph.function_links_slice(&functions).iter().copied() {
        let Some(function) = graph.read_function_link(link) else {
            group.push(link);
            continue;
        };
        if graph[function].cont.is_none() && !graph[function].is_reified {
            keep.push(link);
        } else {
            group.push(link);
        }
        bodies.push(graph[function].body);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::cps::graph::FunctionId,
        expander::core::fresh_lvar,
        runtime::{value::Value, Context, Scheme},
    };

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> crate::expander::core::LVarRef<'gc> {
        fresh_lvar(ctx, ctx.intern(name))
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    enum Kind {
        Function,
        Reified,
        NonReified,
    }

    /// A function whose `cont`/`is_reified` match `kind` (the fields the
    /// merge reads). Member bodies are dead links by default.
    fn make_function<'gc>(
        graph: &mut Graph<'gc>,
        ctx: Context<'gc>,
        name: &str,
        kind: Kind,
    ) -> FunctionId {
        let var = graph.new_bound_var(lvar(ctx, name));
        let body = graph.new_term_link(None);
        let vars = graph.new_bound_vars([]);
        let (cont, is_reified) = match kind {
            Kind::Function => (Some(var), false),
            Kind::Reified => (None, true),
            Kind::NonReified => (None, false),
        };
        graph.new_function(Function {
            name: Value::new(false),
            source: Value::new(false),
            var,
            vars,
            variadic: None,
            cont,
            is_variadic: false,
            body,
            is_rec: false,
            unroll_count: 0,
            is_cold: false,
            is_noinline: false,
            is_reified,
            meta: Value::new(false),
        })
    }

    /// A `Fix`/`Letk` chain: `groups` in chain order (outermost first), each
    /// `(is_letk, member kinds)`. The tail body is a dead link. Returns the
    /// head link and the live members in chain order.
    fn build_chain<'gc>(
        graph: &mut Graph<'gc>,
        ctx: Context<'gc>,
        groups: &[(bool, &[Kind])],
    ) -> (Subterm, Vec<FunctionId>, Vec<Kind>) {
        // Collect members outer->inner first, then build terms inner->outer
        // (each term wraps the previous one).
        let mut collected: Vec<(bool, Vec<FunctionId>, Vec<Kind>, FunctionLinks)> = Vec::new();
        for (is_letk, group_kinds) in groups.iter() {
            let mut links = Vec::new();
            let mut ids = Vec::new();
            let mut kinds = Vec::new();
            for kind in group_kinds.iter().copied() {
                let function = make_function(graph, ctx, "m", kind);
                links.push(graph.new_function_link(Some(function)));
                ids.push(function);
                kinds.push(kind);
            }
            let functions = graph.new_function_links(links);
            collected.push((*is_letk, ids, kinds, functions));
        }
        let mut members = Vec::new();
        let mut kinds = Vec::new();
        for (_, ids, group_kinds, _) in &collected {
            members.extend(ids.iter().copied());
            kinds.extend(group_kinds.iter().copied());
        }
        let mut body = graph.new_term_link(None);
        for (is_letk, _, _, functions) in collected.into_iter().rev() {
            let parent = graph.new_parent_link(None);
            let node = graph.new_term(
                parent,
                if is_letk {
                    TermKind::Letk(functions, body)
                } else {
                    TermKind::Fix(functions, body)
                },
                Value::new(false),
            );
            body = graph.new_term_link(Some(node));
        }
        (body, members, kinds)
    }

    fn live_links<'gc>(graph: &Graph<'gc>, functions: FunctionLinks) -> Vec<FunctionId> {
        graph
            .function_links_slice(&functions)
            .iter()
            .copied()
            .filter_map(|l| graph.read_function_link(l))
            .collect()
    }

    /// `(kind, members, body)` of a group term.
    fn group_of<'gc>(
        graph: &Graph<'gc>,
        term: TermId,
    ) -> (bool, FunctionLinks, Subterm) {
        match graph[term].kind {
            TermKind::Fix(functions, body) => (false, functions, body),
            TermKind::Letk(functions, body) => (true, functions, body),
            _ => panic!("expected a group term"),
        }
    }

    #[test]
    fn fix_plus_reified_letk_merges_into_one_group() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, members, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (false, &[Kind::Function]),
                    (true, &[Kind::Reified]),
                ],
            );
            let head = graph.read_term_link(root).expect("head");
            merge_nested_groups(&mut graph, root);

            let (is_letk, functions, body) = group_of(&graph, head);
            assert!(!is_letk, "merged group must be a Fix");
            assert_eq!(live_links(&graph, functions), members);
            // No non-reified continuation: nothing wraps the final body.
            assert_eq!(graph.read_term_link(body), None);
        });
    }

    #[test]
    fn mixed_letk_keeps_non_reified_in_trailing_letk() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, members, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (false, &[Kind::Function]),
                    (true, &[Kind::Reified, Kind::NonReified]),
                ],
            );
            let head = graph.read_term_link(root).expect("head");
            let reified = members[1];
            let local = members[2];
            merge_nested_groups(&mut graph, root);

            // Head: Fix(f, k_reified), body -> the reused first chain node.
            let (is_letk, functions, body) = group_of(&graph, head);
            assert!(!is_letk);
            assert_eq!(live_links(&graph, functions), vec![members[0], reified]);

            // The first chain node is now the trailing Letk over the
            // non-reified continuation.
            let trailing = graph.read_term_link(body).expect("trailing letk");
            let (is_letk, functions, final_body) = group_of(&graph, trailing);
            assert!(is_letk);
            assert_eq!(live_links(&graph, functions), vec![local]);
            assert_eq!(graph.read_term_link(final_body), None);
        });
    }

    #[test]
    fn letk_head_converts_to_fix() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, members, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (true, &[Kind::Reified]),
                    (false, &[Kind::Function]),
                ],
            );
            let head = graph.read_term_link(root).expect("head");
            merge_nested_groups(&mut graph, root);

            let (is_letk, functions, body) = group_of(&graph, head);
            assert!(!is_letk, "Letk head converts to Fix");
            assert_eq!(live_links(&graph, functions), members);
            assert_eq!(graph.read_term_link(body), None);
        });
    }

    #[test]
    fn pure_reified_letk_chain_merges() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, members, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (true, &[Kind::Reified]),
                    (true, &[Kind::Reified]),
                ],
            );
            let head = graph.read_term_link(root).expect("head");
            merge_nested_groups(&mut graph, root);

            let (is_letk, functions, body) = group_of(&graph, head);
            assert!(!is_letk);
            assert_eq!(live_links(&graph, functions), members);
            assert_eq!(graph.read_term_link(body), None);
        });
    }

    #[test]
    fn all_non_reified_chain_is_left_untouched() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, _, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (true, &[Kind::NonReified]),
                    (true, &[Kind::NonReified]),
                ],
            );
            let head = graph.read_term_link(root).expect("head");
            merge_nested_groups(&mut graph, root);

            let (is_letk, _, body) = group_of(&graph, head);
            assert!(is_letk, "no closure site: chain must stay a Letk");
            let inner = graph.read_term_link(body).expect("inner letk");
            let (is_letk, _, _) = group_of(&graph, inner);
            assert!(is_letk);
        });
    }

    #[test]
    fn dead_links_are_preserved_in_the_group() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let f = make_function(&mut graph, ctx, "f", Kind::Function);
            let f_link = graph.new_function_link(Some(f));
            let dead = graph.new_function_link(None);
            let functions = graph.new_function_links([f_link, dead]);
            let leaf = graph.new_term_link(None);
            let parent = graph.new_parent_link(None);
            let node = graph.new_term(parent, TermKind::Fix(functions, leaf), Value::new(false));
            let root = graph.new_term_link(Some(node));
            merge_nested_groups(&mut graph, root);

            let (is_letk, merged, body) = group_of(&graph, node);
            assert!(!is_letk);
            assert_eq!(live_links(&graph, merged), vec![f]);
            assert_eq!(graph.function_links_slice(&merged).len(), 2, "dead link kept");
            assert_eq!(graph.read_term_link(body), None);
        });
    }

    #[test]
    fn nested_chains_in_member_bodies_merge() {
        Scheme::new_uninit().enter(|ctx| {
            let mut graph = Graph::new();
            let (root, members, _) = build_chain(
                &mut graph,
                ctx,
                &[(false, &[Kind::Function])],
            );
            // f's body is itself a nested Fix(Fix) chain: it must merge too.
            let (inner_root, inner_members, _) = build_chain(
                &mut graph,
                ctx,
                &[
                    (false, &[Kind::Function]),
                    (false, &[Kind::Function]),
                ],
            );
            graph[members[0]].body = inner_root;

            merge_nested_groups(&mut graph, root);

            let inner_head = graph.read_term_link(inner_root).expect("inner head");
            let (is_letk, functions, _) = group_of(&graph, inner_head);
            assert!(!is_letk);
            assert_eq!(live_links(&graph, functions), inner_members);
        });
    }
}
