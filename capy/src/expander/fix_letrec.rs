use std::{collections::HashSet, rc::Rc};

use crate::{
    cps::Map,
    expander::core::{
        LVarRef, LetStyle, ProcRef, Term, TermKind, TermRef, constant, seq_from_slice,
    },
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

            TermKind::Seq(head, tail) => {
                rec(&head, counter, map);
                rec(&tail, counter, map);
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

#[allow(dead_code, unused_variables)]
struct ReorderBindings<'gc> {
    ctx: Context<'gc>,
    sym_id: Map<LVarRef<'gc>, u32>,
    fv_cache: Map<*const Term<'gc>, Rc<HashSet<u32>>>,
    remaining_ids: HashSet<u32>,
    sunk_lambdas: Vec<(LVarRef<'gc>, TermRef<'gc>)>,
    sunk_exprs: Vec<(LVarRef<'gc>, TermRef<'gc>)>,
}

impl<'gc> ReorderBindings<'gc> {
    #[allow(unused)]
    fn visit(&mut self, lhs: LVarRef<'gc>, rhs: TermRef<'gc>) -> bool {
        false
    }
}

/// Reorders bindings to minimize false dependencies for letrec*.
///
/// This function sinks (moves later) bindings that do not reference
/// subsequent bindings. Lambdas are sunk further than other expressions.
#[allow(dead_code, unused_variables)]
fn reorder_bindings<'gc>(
    ctx: Context<'gc>,
    lhs: &[LVarRef<'gc>],
    rhs: &[TermRef<'gc>],
    sym_id: &Map<LVarRef<'gc>, u32>,
    fv_cache: &mut Map<*const Term<'gc>, Rc<HashSet<u32>>>,
) -> Vec<(LVarRef<'gc>, TermRef<'gc>)> {
    todo!()
}

pub fn fix_letrec<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    // remove `let*` bindings. This makes fixing letrec substantially easier.
    let t = remove_letstar(ctx, t);
    let sym_id = compute_ids(&t);
    let (referenced, assigned) = compute_referenced_and_assigned(t);
    let complex = compute_complex(t, &assigned, &sym_id);
    let mut compute_free_variables = ComputeFreeVariables::new(&sym_id);

    t.post_order(ctx, |ctx, x| match &x.kind {
        // sets to unreferenced variables may be replaced with their expression for side effects
        TermKind::LSet(var, val) if !referenced.contains(var) => {
            seq_from_slice(ctx, [*val, constant(ctx, Value::new(false))])
        }

        TermKind::Let(l) => {
            if matches!(l.style, LetStyle::LetRec | LetStyle::LetRecStar) {
                let in_order = matches!(l.style, LetStyle::LetRecStar);

                if in_order {
                    //let (lhs, rhs) = reorder_bindings(ctx, &l.lhs, &l.rhs, &sym_id, &mut fv_cache)
                    //    .into_iter()
                    //    .unzip::<_, _, Vec<_>, Vec<_>>();

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
                        &mut compute_free_variables,
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
                        &mut compute_free_variables,
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
                        &mut compute_free_variables,
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
    compute_free_variables: &mut ComputeFreeVariables<'gc, '_>,
    complex: &HashSet<u32>,
) -> TermRef<'gc> {
    let unreferenced = |var: &LVarRef<'gc>| !referenced.contains(var);
    let unassigned = |var: &LVarRef<'gc>| !assigned.contains(var);

    let sccs = compute_sccs(lhs, rhs, in_order, sym_id, complex, compute_free_variables);
    let mut recursive = |var: &LVarRef<'gc>, rhs: TermRef<'gc>| {
        compute_free_variables.get(rhs).contains(&sym_id[var])
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

    complex: &HashSet<u32>,

    compute_free_variables: &mut ComputeFreeVariables<'gc, '_>,
) -> Vec<Vec<(LVarRef<'gc>, TermRef<'gc>)>> {
    let mut graph = petgraph::Graph::<Vertex, ()>::new();
    let mut node_ids = Vec::new();
    let mut node_id_map = Map::default();
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let node = graph.add_node(Vertex {
            lhs: *lhs,
            rhs: *rhs,
        });
        node_id_map.insert(sym_id[lhs], node);
        node_ids.push(node);
    }

    /*for &v in node_ids.iter() {
        for &w in node_ids.iter() {
            if w != v
                && compute_free_variables(fv_cache, sym_id, &graph[v].rhs)
                    .contains(&sym_id[&graph[w].lhs])
            {
                graph.add_edge(v, w, ());
            }
        }
    }*/

    for &node in node_ids.iter() {
        let init = &graph[node].rhs;

        let fv = compute_free_variables.get(*init);
        for id in fv.iter() {
            let Some(target) = node_id_map.get(id).copied() else {
                continue;
            };

            graph.add_edge(node, target, ());
        }
    }

    if in_order {
        /*let mut w = &node_ids[..];

        while !w.is_empty() {
            if w.len() != 1 {
                let xj = w[1];
                let xi = w[0];

                if !graph[xi].rhs.is_transparent() || complex.contains(&sym_id[&graph[xi].lhs]) {
                    graph.add_edge(xj, xi, ());
                }
                w = &w[1..];
            } else {
                break;
            }
        }*/

        let mut prev: Option<petgraph::prelude::NodeIndex> = None;
        for &id in node_ids.iter() {
            if let Some(p) = prev {
                graph.add_edge(id, p, ());
            }
            if complex.contains(&sym_id[&graph[id].lhs]) {
                prev = Some(id);
            }
        }
    }

    let sccs = petgraph::algo::tarjan_scc(&graph);

    sccs.into_iter()
        .map(|mut scc| {
            scc.sort();
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
    match binds {
        [(name, init)] => {
            // SCC with just one binding
            if unreferenced(name) {
                // (seq <init> ...) for side-effects
                Term::seq(&ctx, *init, body, src)
            } else if let TermKind::Proc(proc) = init.kind
                && unassigned(name)
            {
                Term::fix(&ctx, [*name], [proc], body, src)
            } else if recursive(name, *init) {
                Term::let_(
                    &ctx,
                    LetStyle::Let,
                    [*name],
                    [constant(ctx, Value::undefined())],
                    Term::seq(&ctx, Term::lset(&ctx, *name, *init, src), body, src),
                    src,
                )
            } else {
                Term::let_(&ctx, LetStyle::Let, [*name], [*init], body, src)
            }
        }

        _ => {
            let (lambdas, complex): (Vec<_>, Vec<_>) = binds
                .iter()
                .copied()
                .partition(|(name, init)| init.is_procedure() && unassigned(name));

            let bind_complex_vars = |body: TermRef<'gc>| match complex.as_slice() {
                [] => body,
                _ => Term::let_(
                    &ctx,
                    LetStyle::Let,
                    complex.iter().map(|(name, _)| *name),
                    std::iter::repeat_n(constant(ctx, Value::undefined()), complex.len()),
                    body,
                    src,
                ),
            };

            let bind_lambdas = |body: TermRef<'gc>| match lambdas.as_slice() {
                [] => body,
                _ => Term::fix(
                    &ctx,
                    lambdas.iter().map(|(name, _)| *name),
                    lambdas.iter().map(|(_, init)| {
                        if let TermKind::Proc(proc) = init.kind {
                            proc
                        } else {
                            unreachable!()
                        }
                    }),
                    body,
                    src,
                ),
            };

            let initialize_complex = |body| {
                complex.iter().rfold(body, |body, (name, init)| {
                    Term::seq(&ctx, Term::lset(&ctx, *name, *init, src), body, src)
                })
            };
            bind_complex_vars(bind_lambdas(initialize_complex(body)))
        }
    }
}
struct Vertex<'gc> {
    lhs: LVarRef<'gc>,
    rhs: TermRef<'gc>,
}

struct ComputeFreeVariables<'gc, 'a> {
    fv_cache: Map<*const Term<'gc>, Rc<im::HashSet<u32>>>,
    sym_id: &'a Map<LVarRef<'gc>, u32>,
}

impl<'a, 'gc> ComputeFreeVariables<'gc, 'a> {
    fn new(sym_id: &'a Map<LVarRef<'gc>, u32>) -> Self {
        Self {
            fv_cache: Map::default(),
            sym_id,
        }
    }

    pub fn get(&mut self, t: TermRef<'gc>) -> Rc<im::HashSet<u32>> {
        if let Some(fv) = self.fv_cache.get(&(t.as_ref() as *const Term<'gc>)) {
            fv.clone()
        } else {
            let fv = Rc::new(self.visit(t));
            self.fv_cache
                .insert(t.as_ref() as *const Term<'gc>, fv.clone());
            fv
        }
    }

    fn union(set1: im::HashSet<u32>, set2: im::HashSet<u32>) -> im::HashSet<u32> {
        set1.union(set2)
    }

    fn difference(set1: im::HashSet<u32>, set2: im::HashSet<u32>) -> im::HashSet<u32> {
        set1.difference(set2)
    }

    fn empty() -> im::HashSet<u32> {
        im::HashSet::new()
    }

    fn recurse_many(&mut self, exprs: impl Iterator<Item = TermRef<'gc>>) -> im::HashSet<u32> {
        exprs.fold(Self::empty(), |acc, e| Self::union(acc, self.visit(e)))
    }

    fn recurse_many_procs(
        &mut self,
        procs: impl Iterator<Item = ProcRef<'gc>>,
    ) -> im::HashSet<u32> {
        procs.fold(Self::empty(), |acc, p| Self::union(acc, self.visit_proc(p)))
    }

    fn visit(&mut self, t: TermRef<'gc>) -> im::HashSet<u32> {
        let empty = || im::HashSet::new();
        let adjoin = |elt, set: im::HashSet<u32>| {
            let mut new_set = set;
            new_set.insert(elt);
            new_set
        };

        let union = |set1: im::HashSet<u32>, set2: im::HashSet<u32>| set1.union(set2);
        let difference = |set1: im::HashSet<u32>, set2: im::HashSet<u32>| set1.difference(set2);

        match &t.kind {
            TermKind::Const(_)
            | TermKind::ModuleRef(_, _, _)
            | TermKind::PrimRef(_)
            | TermKind::ToplevelRef(_, _) => empty(),

            TermKind::LRef(var) => adjoin(self.sym_id[var], empty()),

            TermKind::LSet(var, val) => {
                let val_fv = self.visit(*val);
                adjoin(self.sym_id[var], val_fv)
            }

            TermKind::ToplevelSet(_, _, val) => self.visit(*val),
            TermKind::ModuleSet(_, _, _, val) => self.visit(*val),
            TermKind::Call(func, args) => args
                .iter()
                .fold(self.visit(*func), |acc, arg| union(acc, self.visit(*arg))),
            TermKind::PrimCall(_, args) => args
                .iter()
                .fold(empty(), |acc, arg| union(acc, self.visit(*arg))),
            TermKind::Define(_, _, val) => self.visit(*val),
            TermKind::Seq(head, tail) => {
                let head_fv = self.visit(*head);
                let tail_fv = self.visit(*tail);
                union(head_fv, tail_fv)
            }
            TermKind::If(test, cons, alt) => {
                let test_fv = self.visit(*test);
                let cons_fv = self.visit(*cons);
                let alt_fv = self.visit(*alt);
                union(union(test_fv, cons_fv), alt_fv)
            }

            TermKind::Let(let_)
                if !matches!(let_.style, LetStyle::LetRec | LetStyle::LetRecStar) =>
            {
                union(
                    self.recurse_many(let_.rhs.iter().copied()),
                    difference(
                        self.visit(let_.body),
                        let_.lhs.iter().map(|var| self.sym_id[var]).collect(),
                    ),
                )
            }

            TermKind::Let(let_) => difference(
                union(
                    self.recurse_many(let_.rhs.iter().copied()),
                    self.visit(let_.body),
                ),
                let_.lhs.iter().map(|var| self.sym_id[var]).collect(),
            ),

            TermKind::Proc(proc) => difference(
                self.visit(proc.body),
                proc.args
                    .iter()
                    .map(|var| self.sym_id[var])
                    .chain(proc.variadic.iter().map(|var| self.sym_id[var]))
                    .collect(),
            ),

            TermKind::Fix(fix) => difference(
                union(
                    self.recurse_many_procs(fix.rhs.iter().copied()),
                    self.visit(fix.body),
                ),
                fix.lhs.iter().map(|var| self.sym_id[var]).collect(),
            ),

            TermKind::Receive(vars, variadic, producer, receiver) => union(
                self.visit(*producer),
                difference(
                    self.visit(*receiver),
                    vars.iter()
                        .map(|var| self.sym_id[var])
                        .chain(variadic.iter().map(|var| self.sym_id[var]))
                        .collect(),
                ),
            ),
            TermKind::Values(values) => self.recurse_many(values.iter().copied()),
            TermKind::WithContinuationMark(key, value, result) => {
                let key_fv = self.visit(*key);
                let value_fv = self.visit(*value);
                let result_fv = self.visit(*result);
                union(union(key_fv, value_fv), result_fv)
            }
        }
    }

    fn visit_proc(&mut self, proc: ProcRef<'gc>) -> im::HashSet<u32> {
        Self::difference(
            self.visit(proc.body),
            proc.args
                .iter()
                .map(|var| self.sym_id[var])
                .chain(proc.variadic.iter().map(|var| self.sym_id[var]))
                .collect(),
        )
    }
}

/// Convert let* to nested let
fn remove_letstar<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    t.post_order(ctx, |ctx, x| match &x.kind {
        TermKind::Let(l) if l.style == LetStyle::LetStar => {
            let mut body = l.body;
            for (var, init) in l.lhs.iter().zip(l.rhs.iter()).rev() {
                body = Term::let_(&ctx, LetStyle::Let, [*var], [*init], body, x.source());
            }
            body
        }

        _ => x,
    })
}
