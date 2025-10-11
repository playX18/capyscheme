use std::{collections::HashSet, rc::Rc};

use rsgc::{Gc, alloc::Array};

use crate::{
    cps::Map,
    expander::core::{Fix, LVarRef, Let, LetStyle, Term, TermKind, TermRef, constant, lset, seq},
    runtime::{Context, value::Value},
};

fn compute_ids<'gc>(t: &Term<'gc>) -> Map<LVarRef<'gc>, u32> {
    fn rec<'gc>(t: &Term<'gc>, counter: &mut u32, map: &mut Map<LVarRef<'gc>, u32>) {
        match t.kind {
            TermKind::Let(l) => {
                for &var in l.lhs.iter() {
                    let id = *counter;
                    map.insert(var, id);
                    *counter += 1;
                }

                for rhs in l.rhs.iter() {
                    rec(rhs, counter, map);
                }
                rec(&l.body, counter, map);
            }

            TermKind::Fix(fix) => {
                for &var in fix.lhs.iter() {
                    let id = *counter;
                    map.insert(var.clone(), id);
                    *counter += 1;
                }

                for proc in fix.rhs.iter() {
                    for &var in proc.args.iter() {
                        let id = *counter;
                        map.insert(var, id);
                        *counter += 1;
                    }

                    if let Some(var) = proc.variadic {
                        let id = *counter;
                        map.insert(var, id);
                        *counter += 1;
                    }
                    rec(&proc.body, counter, map);
                }
                rec(&fix.body, counter, map);
            }

            TermKind::Proc(proc) => {
                for &var in proc.args.iter() {
                    let id = *counter;
                    map.insert(var, id);
                    *counter += 1;
                }

                if let Some(var) = proc.variadic {
                    let id = *counter;
                    map.insert(var, id);
                    *counter += 1;
                }
                rec(&proc.body, counter, map);
            }

            TermKind::If(test, cons, alt) => {
                rec(&test, counter, map);
                rec(&cons, counter, map);
                rec(&alt, counter, map);
            }

            TermKind::LSet(_, val) => rec(&val, counter, map),
            TermKind::Call(func, args) => {
                rec(&func, counter, map);
                for arg in args.iter() {
                    rec(arg, counter, map);
                }
            }

            TermKind::Define(_, _, val) => rec(&val, counter, map),
            TermKind::ToplevelSet(_, _, val) => rec(&val, counter, map),
            TermKind::ModuleSet(_, _, _, val) => rec(&val, counter, map),
            TermKind::PrimCall(_, args) => {
                for arg in args.iter() {
                    rec(arg, counter, map);
                }
            }

            TermKind::Receive(vars, variadic, producer, receiver) => {
                rec(&producer, counter, map);

                for &var in vars.iter() {
                    let id = *counter;
                    map.insert(var, id);
                    *counter += 1;
                }

                if let Some(var) = variadic {
                    let id = *counter;
                    map.insert(var, id);
                    *counter += 1;
                }

                rec(&receiver, counter, map);
            }

            TermKind::Seq(seq) => {
                for term in seq.iter() {
                    rec(term, counter, map);
                }
            }

            TermKind::Values(values) => {
                for v in values.iter() {
                    rec(v, counter, map);
                }
            }

            _ => (),
        }
    }

    let mut map = Map::default();
    let mut counter = 0;

    rec(t, &mut counter, &mut map);

    map
}

fn compute_referenced_and_assigned<'gc>(
    t: TermRef<'gc>,
) -> (HashSet<LVarRef<'gc>>, HashSet<LVarRef<'gc>>) {
    let referenced = HashSet::new();
    let assigned = HashSet::new();

    t.fold::<_>(
        |expr, (mut referenced, mut assigned)| match &expr.kind {
            TermKind::LRef(var) => {
                referenced.insert(*var);
                (referenced, assigned)
            }

            TermKind::LSet(var, _) => {
                assigned.insert(*var);
                (referenced, assigned)
            }

            _ => (referenced, assigned),
        },
        |_, acc| acc,
        (referenced, assigned),
    )
}

fn compute_free_variables<'a, 'gc>(
    cache: &'a mut Map<*const Term<'gc>, Rc<HashSet<u32>>>,
    sym_id: &Map<LVarRef<'gc>, u32>,
    t: &Term<'gc>,
) -> Rc<HashSet<u32>> {
    fn recurse_many<'gc>(
        exprs: impl Iterator<Item = TermRef<'gc>>,
        sym_id: &Map<LVarRef<'gc>, u32>,
        free: &mut HashSet<u32>,
    ) {
        exprs.for_each(|e| rec(&e, sym_id, free));
    }

    fn rec<'gc>(t: &Term<'gc>, sym_id: &Map<LVarRef<'gc>, u32>, free: &mut HashSet<u32>) {
        match &t.kind {
            TermKind::Const(_)
            | TermKind::ModuleRef(_, _, _)
            | TermKind::PrimRef(_)
            | TermKind::ToplevelRef(_, _) => (),
            TermKind::LRef(var) => {
                free.insert(sym_id[var]);
            }

            TermKind::LSet(var, val) => {
                rec(val, sym_id, free);
                free.insert(sym_id[var]);
            }

            TermKind::ToplevelSet(_, _, val) => rec(val, sym_id, free),
            TermKind::ModuleSet(_, _, _, val) => rec(val, sym_id, free),
            TermKind::Call(func, args) => recurse_many(
                std::iter::once(*func).chain(args.iter().cloned()),
                sym_id,
                free,
            ),
            TermKind::PrimCall(_, args) => recurse_many(args.iter().cloned(), sym_id, free),
            TermKind::Define(_, _, val) => rec(val, sym_id, free),
            TermKind::Seq(seq) => recurse_many(seq.iter().cloned(), sym_id, free),
            TermKind::If(test, cons, alt) => recurse_many(
                std::iter::once(*test)
                    .chain(std::iter::once(*cons))
                    .chain(std::iter::once(*alt)),
                sym_id,
                free,
            ),

            TermKind::Let(let_) => {
                if matches!(let_.style, LetStyle::LetRec | LetStyle::LetRecStar) {
                    recurse_many(let_.rhs.iter().copied(), sym_id, free);
                    rec(&let_.body, sym_id, free);
                    let_.lhs.iter().for_each(|var| {
                        free.remove(&sym_id[var]);
                    });
                } else {
                    recurse_many(let_.rhs.iter().copied(), sym_id, free);
                    rec(&let_.body, sym_id, free);
                    let_.lhs.iter().for_each(|var| {
                        free.remove(&sym_id[var]);
                    });
                }
            }

            TermKind::Fix(fix) => {
                recurse_many(fix.rhs.iter().map(|p| p.body), sym_id, free);
                rec(&fix.body, sym_id, free);
                fix.lhs.iter().for_each(|var| {
                    free.remove(&sym_id[var]);
                });
            }

            TermKind::Receive(vars, variadic, producer, receiver) => {
                rec(producer, sym_id, free);
                rec(receiver, sym_id, free);
                vars.iter().for_each(|var| {
                    free.remove(&sym_id[var]);
                });
                if let Some(var) = variadic {
                    free.remove(&sym_id[var]);
                }
            }
            TermKind::Proc(proc) => {
                rec(&proc.body, sym_id, free);
                proc.args.iter().for_each(|var| {
                    free.remove(&sym_id[var]);
                });
                if let Some(var) = proc.variadic {
                    free.remove(&sym_id[&var]);
                }
            }
            TermKind::Values(values) => recurse_many(values.iter().cloned(), sym_id, free),
        }
    }

    if let Some(free) = cache.get(&(t as *const Term)) {
        free.clone()
    } else {
        let mut set = HashSet::new();
        rec(t, sym_id, &mut set);
        cache.insert(t as *const Term, Rc::new(set));
        cache[&(t as *const Term)].clone()
    }
}

fn compute_complex<'gc>(
    t: TermRef<'gc>,
    assigned: &HashSet<LVarRef<'gc>>,
    sym_id: &Map<LVarRef<'gc>, u32>,
) -> HashSet<u32> {
    t.fold(
        |_, complex| complex,
        |expr, mut complex| match &expr.kind {
            TermKind::Let(l) => {
                for (lhs, rhs) in l.lhs.iter().zip(l.rhs.iter()) {
                    if assigned.contains(lhs) || !rhs.is_transparent() {
                        complex.insert(sym_id[lhs]);
                    }
                }

                complex
            }
            _ => complex,
        },
        HashSet::new(),
    )
}

fn reorder_bindings<'gc>(
    ctx: Context<'gc>,
    lhs: &[LVarRef<'gc>],
    rhs: &[TermRef<'gc>],
    sym_id: &Map<LVarRef<'gc>, u32>,
    fv_cache: &mut Map<*const Term<'gc>, Rc<HashSet<u32>>>,
) -> Vec<(LVarRef<'gc>, TermRef<'gc>)> {
    let mut possibly_references = |expr: TermRef<'gc>, remaining_ids: &HashSet<u32>| {
        let free = compute_free_variables(fv_cache, sym_id, &expr);
        free.intersection(remaining_ids).next().is_some()
    };

    let mut remaining_ids = lhs.iter().map(|var| sym_id[var]).collect::<HashSet<_>>();

    let mut bindings = lhs.iter().copied().zip(rhs.iter().copied());
    let mut sunk_lambdas = Vec::new();
    let mut sunk_exprs = Vec::new();

    fn visit<'gc>(
        ctx: Context<'gc>,

        sym_id: &Map<LVarRef<'gc>, u32>,
        remaining_ids: &mut HashSet<u32>,
        bindings: &mut impl Iterator<Item = (LVarRef<'gc>, TermRef<'gc>)>,
        sunk_lambdas: &mut Vec<(LVarRef<'gc>, TermRef<'gc>)>,
        sunk_exprs: &mut Vec<(LVarRef<'gc>, TermRef<'gc>)>,
        possibly_references: &mut dyn FnMut(TermRef<'gc>, &HashSet<u32>) -> bool,
    ) -> Vec<(LVarRef<'gc>, TermRef<'gc>)> {
        while let Some(binding) = bindings.next() {
            remaining_ids.remove(&sym_id[&binding.0]);
            if matches!(binding.1.kind, TermKind::Proc(_)) {
                sunk_lambdas.push(binding.clone());
                continue;
            } else if possibly_references(binding.1, remaining_ids) {
                let mut result = sunk_lambdas.clone();
                result.extend(sunk_exprs.iter().copied());

                result.push(binding.clone());
                result.extend(visit(
                    ctx,
                    sym_id,
                    remaining_ids,
                    bindings,
                    &mut Vec::new(),
                    &mut Vec::new(),
                    possibly_references,
                ));
                return result;
            } else {
                sunk_exprs.push(binding.clone());
                continue;
            }
        }

        let mut result = sunk_lambdas.clone();
        result.extend(sunk_exprs.iter().copied());
        result
    }

    visit(
        ctx,
        sym_id,
        &mut remaining_ids,
        &mut bindings,
        &mut sunk_lambdas,
        &mut sunk_exprs,
        &mut possibly_references,
    )
}

pub fn fix_letrec<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    let sym_id = compute_ids(&t);
    let (referenced, assigned) = compute_referenced_and_assigned(t);
    let complex = compute_complex(t, &assigned, &sym_id);
    let mut fv_cache = Map::default();

    t.post_order(ctx, |ctx, x| match &x.kind {
        // sets to unreferenced variables may be replaced with their expression for side effects
        TermKind::LSet(var, val) if !referenced.contains(var) => seq(
            ctx,
            Array::from_slice(&ctx, [*val, constant(ctx, Value::new(false))]),
        ),

        TermKind::Let(l) => {
            if matches!(l.style, LetStyle::LetRec | LetStyle::LetRecStar) {
                let in_order = matches!(l.style, LetStyle::LetRecStar);

                if in_order {
                    let (lhs, rhs) = reorder_bindings(ctx, &l.lhs, &l.rhs, &sym_id, &mut fv_cache)
                        .into_iter()
                        .unzip::<_, _, Vec<_>, Vec<_>>();

                    fix_term(
                        ctx,
                        x.source(),
                        in_order,
                        &lhs,
                        &rhs,
                        l.body,
                        &sym_id,
                        &referenced,
                        &assigned,
                        &mut fv_cache,
                        &complex,
                    )
                } else {
                    fix_term(
                        ctx,
                        x.source(),
                        in_order,
                        &l.lhs,
                        &l.rhs,
                        l.body,
                        &sym_id,
                        &referenced,
                        &assigned,
                        &mut fv_cache,
                        &complex,
                    )
                }
            } else {
                if l.rhs.iter().any(|x| matches!(x.kind, TermKind::Proc(_))) {
                    fix_term(
                        ctx,
                        x.source(),
                        false,
                        &l.lhs,
                        &l.rhs,
                        l.body,
                        &sym_id,
                        &referenced,
                        &assigned,
                        &mut fv_cache,
                        &complex,
                    )
                } else {
                    x
                }
            }
        }

        _ => x,
    })
}

fn fix_term<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    in_order: bool,
    lhs: &[LVarRef<'gc>],
    rhs: &[TermRef<'gc>],
    body: TermRef<'gc>,
    sym_id: &Map<LVarRef<'gc>, u32>,
    referenced: &HashSet<LVarRef<'gc>>,
    assigned: &HashSet<LVarRef<'gc>>,
    fv_cache: &mut Map<*const Term<'gc>, Rc<HashSet<u32>>>,
    _complex: &HashSet<u32>,
) -> TermRef<'gc> {
    let unreferenced = |var: &LVarRef<'gc>| !referenced.contains(var);
    let unassigned = |var: &LVarRef<'gc>| !assigned.contains(var);

    let sccs = compute_sccs(lhs, rhs, in_order, sym_id, fv_cache);
    let mut recursive = |var: &LVarRef<'gc>, rhs: TermRef<'gc>| {
        compute_free_variables(fv_cache, sym_id, &rhs).contains(&sym_id[var])
    };

    sccs.iter().rfold(body, |body, scc| {
        fix_scc(
            ctx,
            src,
            scc,
            body,
            &unreferenced,
            &unassigned,
            &mut recursive,
        )
    })
}

fn compute_sccs<'gc>(
    lhs: &[LVarRef<'gc>],
    rhs: &[TermRef<'gc>],
    in_order: bool,
    sym_id: &Map<LVarRef<'gc>, u32>,

    fv_cache: &mut Map<*const Term<'gc>, Rc<HashSet<u32>>>,
) -> Vec<Vec<(LVarRef<'gc>, TermRef<'gc>)>> {
    let mut graph = petgraph::Graph::new();
    let mut node_ids = Vec::new();
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        node_ids.push(graph.add_node(Vertex {
            lhs: *lhs,
            rhs: *rhs,
        }));
    }

    for &v in node_ids.iter() {
        for &w in node_ids.iter() {
            if w != v
                && compute_free_variables(fv_cache, sym_id, &graph[v].rhs)
                    .contains(&sym_id[&graph[w].lhs])
            {
                graph.add_edge(v, w, ());
            }
        }
    }
    if in_order {
        let mut w = &node_ids[..];

        while !w.is_empty() {
            if w.len() != 1 {
                let xj = w[1];
                let xi = w[0];

                if !graph[xi].rhs.is_transparent() {
                    graph.add_edge(xj, xi, ());
                }
                w = &w[1..];
            } else {
                break;
            }
        }
    }

    let sccs = petgraph::algo::tarjan_scc(&graph);

    sccs.into_iter()
        .map(|scc| {
            scc.into_iter()
                .map(|v| {
                    let vertex = &graph[v];
                    (vertex.lhs, vertex.rhs)
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn fix_scc<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    binds: &[(LVarRef<'gc>, TermRef<'gc>)],
    body: TermRef<'gc>,
    unreferenced: impl Fn(&LVarRef<'gc>) -> bool,
    unassigned: impl Fn(&LVarRef<'gc>) -> bool,
    mut recursive: impl FnMut(&LVarRef<'gc>, TermRef<'gc>) -> bool,
) -> TermRef<'gc> {
    if binds.len() == 1 {
        let (lhs, rhs) = binds[0];
        if unreferenced(&lhs) {
            return seq(ctx, Array::from_slice(&ctx, [rhs, body]));
        } else if let TermKind::Proc(rhs) = rhs.kind
            && unassigned(&lhs)
        {
            return Gc::new(
                &ctx,
                Term {
                    source: src.into(),
                    kind: TermKind::Fix(Fix {
                        lhs: Array::from_slice(&ctx, [lhs]),
                        rhs: Array::from_slice(&ctx, [rhs]),
                        body,
                    }),
                },
            );
        } else if recursive(&lhs, rhs) {
            let mutation = lset(ctx, lhs, rhs);
            let body = seq(ctx, Array::from_slice(&ctx, [mutation, body]));
            return Gc::new(
                &ctx,
                Term {
                    source: src.into(),
                    kind: TermKind::Let(Let {
                        style: LetStyle::Let,
                        lhs: Array::from_slice(&ctx, [lhs]),
                        rhs: Array::from_slice(&ctx, [constant(ctx, Value::undefined())]),
                        body,
                    }),
                },
            );
        } else {
            return Gc::new(
                &ctx,
                Term {
                    source: src.into(),
                    kind: TermKind::Let(Let {
                        style: LetStyle::Let,
                        lhs: Array::from_slice(&ctx, [lhs]),
                        rhs: Array::from_slice(&ctx, [rhs]),
                        body,
                    }),
                },
            );
        }
    } else {
        let (l, c): (Vec<_>, Vec<_>) = binds
            .iter()
            .copied()
            .partition(|(lhs, rhs)| matches!(rhs.kind, TermKind::Proc(_)) && unassigned(lhs));

        let mut body = body;

        for (lhs, rhs) in c.iter().rev().copied() {
            let mutation = lset(ctx, lhs, rhs);
            body = seq(ctx, Array::from_slice(&ctx, [mutation, body]));
        }
        if !l.is_empty() {
            let (lhs, rhs): (Vec<_>, Vec<_>) = l.iter().copied().unzip();
            let rhs = rhs
                .into_iter()
                .map(|t| {
                    if let TermKind::Proc(p) = t.kind {
                        p
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>();
            body = Gc::new(
                &ctx,
                Term {
                    source: src.into(),
                    kind: TermKind::Fix(Fix {
                        lhs: Array::from_slice(&ctx, &lhs),
                        rhs: Array::from_slice(&ctx, &rhs),
                        body,
                    }),
                },
            );
        }

        if !c.is_empty() {
            let (lhs, _init): (Vec<_>, Vec<_>) = c.iter().copied().unzip();
            body = Gc::new(
                &ctx,
                Term {
                    source: src.into(),
                    kind: TermKind::Let(Let {
                        style: LetStyle::Let,
                        lhs: Array::from_slice(&ctx, &lhs),
                        rhs: Array::from_slice(
                            &ctx,
                            &vec![constant(ctx, Value::undefined()); c.len()],
                        ),
                        body,
                    }),
                },
            );
        }

        body
    }
}
struct Vertex<'gc> {
    lhs: LVarRef<'gc>,
    rhs: TermRef<'gc>,
}
