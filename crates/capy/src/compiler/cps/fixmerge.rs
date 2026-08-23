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
use crate::utils::flags;

#[cfg(test)]
use super::graph::Function;

/// Merge every adjacent chain of `Fix`/`Letk` terms reachable from `root`
/// into a single `Fix` group (functions + reified continuations), keeping
/// non-reified continuations in a trailing `Letk`.
pub fn merge_nested_groups<'gc>(graph: &mut Graph<'gc>, root: Subterm) {
    let mut visited = HashSet::new();
    walk_term(graph, root, &mut visited);

    if flags::share_dump() {
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
                if live.len() > 1 { ": merged group" } else { "" }
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
