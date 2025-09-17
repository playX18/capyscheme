use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use rsgc::{Gc, alloc::array::Array, cell::Lock};

use crate::{
    expander::core::{
        LVarRef, LetStyle, Proc, Term, TermKind, TermRef, constant, fix_term, let_term, lset, seq,
    },
    runtime::{Context, value::Value},
};

pub fn free_variables<'gc>(
    term: TermRef<'gc>,
    cache: &mut HashMap<*const Term<'gc>, Rc<HashSet<LVarRef<'gc>>>>,
) -> Rc<HashSet<LVarRef<'gc>>> {
    if let Some(vars) = cache.get(&term.as_ptr()) {
        return vars.clone();
    }

    fn rec<'gc>(expr: &Term<'gc>, set: &mut HashSet<LVarRef<'gc>>) {
        match &expr.kind {
            TermKind::ToplevelRef(_, _)
            | TermKind::ModuleRef(_, _, _)
            | TermKind::Const(_)
            | TermKind::PrimRef(..) => {}
            TermKind::Fix(fix) => {
                let mut set2 = HashSet::new();
                for var in fix.lhs.iter() {
                    set2.insert(*var);
                }

                for proc in fix.rhs.iter() {
                    rec(&proc.body, &mut set2);
                    for var in proc.args.iter().chain(proc.variadic.iter()) {
                        set2.remove(var);
                    }

                    //set.extend(set2);
                }

                rec(&fix.body, set);

                for var in fix.lhs.iter() {
                    set2.remove(var);
                }

                set.extend(set2);
            }

            TermKind::LRef(var) => {
                set.insert(var.clone());
            }

            TermKind::Call(proc, args) => {
                rec(&proc, set);
                args.iter().for_each(|arg| rec(arg, set));
            }

            TermKind::PrimCall(_, args) => {
                args.iter().for_each(|arg| rec(arg, set));
            }

            TermKind::Define(_, _, val)
            | TermKind::LSet(_, val)
            | TermKind::ToplevelSet(_, _, val)
            | TermKind::ModuleSet(_, _, _, val) => {
                rec(val, set);
            }

            TermKind::If(test, cons, alt) => {
                rec(test, set);
                rec(cons, set);
                rec(alt, set);
            }

            TermKind::Seq(seq) => seq.iter().for_each(|term| rec(term, set)),

            TermKind::Values(values) => {
                values.iter().for_each(|v| rec(v, set));
            }

            TermKind::Receive(formals, opt_formal, producer, consumer) => {
                rec(producer, set);

                let mut set2 = HashSet::new();
                rec(consumer, &mut set2);
                for var in formals.iter().chain(opt_formal.iter()) {
                    set2.remove(var);
                }

                set.extend(set2);
            }

            TermKind::Let(l) => {
                if let LetStyle::Let = l.style {
                    for init in l.rhs.iter() {
                        rec(init, set);
                    }

                    let mut set2 = HashSet::new();
                    rec(&l.body, &mut set2);

                    for var in l.lhs.iter() {
                        set2.remove(var);
                    }

                    set.extend(set2);
                } else {
                    let mut set2 = HashSet::new();
                    for init in l.rhs.iter() {
                        rec(init, &mut set2);
                    }

                    rec(&l.body, &mut set2);

                    for var in l.lhs.iter() {
                        set2.remove(var);
                    }

                    set.extend(set2);
                }
            }

            TermKind::Proc(proc) => {
                rec(&proc.body, set);
                for var in proc.args.iter().chain(proc.variadic.iter()) {
                    set.remove(var);
                }
            }
        }
    }

    let mut set = HashSet::new();
    rec(&term, &mut set);
    let set = Rc::new(set);
    cache.insert(term.as_ptr(), set.clone());
    set
}

struct Vertex<'gc> {
    lhs: Option<LVarRef<'gc>>,
    rhs: Option<TermRef<'gc>>,
    idx: usize,
    adjacency: Vec<VertexId>,
    number: isize,
    lowlink: isize,
    onstack: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct VertexId(u32);

struct Graph<'gc> {
    vertices: Vec<Vertex<'gc>>,
}

impl<'gc> std::ops::Index<VertexId> for Graph<'gc> {
    type Output = Vertex<'gc>;

    fn index(&self, index: VertexId) -> &Self::Output {
        &self.vertices[index.0 as usize]
    }
}

impl<'gc> std::ops::IndexMut<VertexId> for Graph<'gc> {
    fn index_mut(&mut self, index: VertexId) -> &mut Self::Output {
        &mut self.vertices[index.0 as usize]
    }
}

impl<'gc> Graph<'gc> {
    fn new() -> Self {
        Self {
            vertices: Vec::new(),
        }
    }

    fn add_edge(&mut self, v: VertexId, w: VertexId) {
        self[v].adjacency.push(w);
    }

    fn add_empty_vertex(&mut self) -> VertexId {
        let ix = self.vertices.len();

        self.vertices.push(Vertex {
            idx: 0,

            lhs: None,
            rhs: None,
            adjacency: Vec::new(),
            number: -1,
            lowlink: -1,
            onstack: false,
        });
        VertexId(ix as _)
    }

    fn add_vertex(&mut self, lhs: LVarRef<'gc>, rhs: TermRef<'gc>) -> VertexId {
        let vertex = self.add_empty_vertex();
        self[vertex].lhs = Some(lhs);
        self[vertex].rhs = Some(rhs);

        vertex
    }

    // Robert Tarjan's algorithm for finding strongly connected
    // components in a graph.
    fn tarjan_sccs(&mut self) -> Vec<Vec<VertexId>> {
        let mut index = 0;
        let mut stack = Vec::new();
        let mut sccs = Vec::new();

        fn strong_connect(
            graph: &mut Graph,
            v: VertexId,
            index: &mut isize,
            stack: &mut Vec<VertexId>,
            sccs: &mut Vec<Vec<VertexId>>,
        ) {
            graph[v].number = *index;
            graph[v].lowlink = *index;
            *index += 1;

            stack.push(v);

            graph[v].onstack = true;

            for j in 0..graph[v].adjacency.len() {
                let w = graph[v].adjacency[j];

                if graph[w].number == -1 {
                    strong_connect(graph, w, index, stack, sccs);
                    graph[w].lowlink = std::cmp::min(graph[v].lowlink, graph[w].lowlink);
                } else if graph[w].onstack {
                    graph[v].lowlink = std::cmp::min(graph[v].lowlink, graph[w].lowlink);
                }
            }

            if graph[v].number == graph[v].lowlink {
                let mut scc = Vec::new();

                loop {
                    let w = stack.pop().unwrap();

                    graph[w].onstack = false;
                    scc.push(w);
                    if w == v {
                        break;
                    }
                }
                sccs.push(scc);
            }
        }

        for i in 0..self.vertices.len() {
            let v = VertexId(i as u32);
            if self[v].number == -1 {
                strong_connect(self, v, &mut index, &mut stack, &mut sccs);
            }
        }

        sccs
    }

    fn unassigned_procedure(&self, v: VertexId) -> bool {
        let v = &self[v];
        let unassigned = !v.lhs.unwrap().is_mutated();
        let proc = matches!(v.rhs.unwrap().kind, TermKind::Proc(_));

        unassigned & proc
    }

    fn make_mutations(
        &self,
        ctx: Context<'gc>,
        vertices: &[VertexId],
        body: TermRef<'gc>,
    ) -> TermRef<'gc> {
        if vertices.is_empty() {
            return body;
        }

        let mut new_seq = Vec::new();

        for v in vertices.iter().copied() {
            let var = self[v].lhs.clone().unwrap();

            let init = self[v].rhs.clone();

            let mutation = lset(ctx, var, init.unwrap());

            new_seq.push(mutation);
        }

        if let TermKind::Seq(seq) = &body.kind {
            new_seq.extend_from_slice(seq);
        } else {
            new_seq.push(body.clone());
        }

        seq(ctx, Array::from_slice(&ctx, new_seq))
    }
}

struct FixPass<'gc> {
    fv_cache: HashMap<*const Term<'gc>, Rc<HashSet<LVarRef<'gc>>>>,
    graph: Graph<'gc>,
    ctx: Context<'gc>,
}

fn make_fixes<'gc>(
    ctx: Context<'gc>,
    _source: Value<'gc>,
    graph: &Graph<'gc>,
    vertices: &[VertexId],
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    if vertices.is_empty() {
        return body;
    }

    let fix_lhs = vertices
        .iter()
        .copied()
        .map(|v| graph[v].lhs.unwrap())
        .collect::<Vec<_>>();
    let fix_rhs = vertices
        .iter()
        .copied()
        .map(|v| {
            if let TermKind::Proc(proc) = &graph[v].rhs.unwrap().kind {
                proc.clone()
            } else {
                panic!("only procedures go into <fix> forms")
            }
        })
        .collect::<Vec<_>>();

    fix_term(
        ctx,
        Array::from_slice(&ctx, fix_lhs),
        Array::from_slice(&ctx, fix_rhs),
        body,
    )
}

fn fix1<'gc>(
    in_order: bool,
    pass: &mut FixPass<'gc>,
    scc: &[VertexId],
    fixes: &mut Vec<VertexId>,
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    let span = body.source.get();

    if scc.len() == 1 {
        let b = scc[0];

        let var = pass.graph[b].lhs.unwrap();
        let init = pass.graph[b].rhs.unwrap();

        if pass.graph.unassigned_procedure(b) {
            fixes.push(b);
            return body;
        } else if !free_variables(init, &mut pass.fv_cache).contains(&var) {
            // if var is not free in init consume
            // fixes previously saved up.

            let new_fixes = make_fixes(pass.ctx, span, &pass.graph, fixes, body);
            fixes.clear();

            let_term(
                pass.ctx,
                LetStyle::Let,
                Array::from_slice(&pass.ctx, [var]),
                Array::from_slice(&pass.ctx, [init]),
                new_fixes,
            )
        } else {
            // otherwise we resort to assignment. Also,
            // consumes any fixes saved up.

            let new_fixes = make_fixes(pass.ctx, span, &pass.graph, fixes, body);
            fixes.clear();
            let mutations = pass.graph.make_mutations(pass.ctx, &[b], new_fixes);

            let_term(
                pass.ctx,
                LetStyle::Let,
                Array::from_slice(&pass.ctx, [var]),
                Array::from_slice(
                    &pass.ctx,
                    [Gc::new(
                        &pass.ctx,
                        Term {
                            source: span.into(),
                            kind: TermKind::Const(Value::undefined()),
                        },
                    )],
                ),
                mutations,
            )
        }
    } else {
        // l - contains lambdas, c - contains complex expressions

        let (mut l, mut c): (Vec<VertexId>, Vec<VertexId>) = scc
            .iter()
            .copied()
            .partition(|v| pass.graph.unassigned_procedure(*v));

        if c.is_empty() {
            // <var_l,init_l> if init is a lambda expression and
            // var is unassigned
            fixes.append(&mut l);
            return body;
        } else {
            if in_order {
                c.sort_by(|a, b| pass.graph[*a].idx.cmp(&pass.graph[*b].idx));
            }

            // <var_c, init_c> otherwise.
            let body = {
                let mut fixes_ = fixes.clone();
                fixes_.append(&mut l);
                fixes.clear();
                let mutations = pass.graph.make_mutations(pass.ctx, &c, body);
                make_fixes(pass.ctx, span, &pass.graph, &fixes_, mutations)
            };

            let lhs = Array::from_slice(
                &pass.ctx,
                c.iter()
                    .map(|v| pass.graph[*v].lhs.unwrap())
                    .collect::<Vec<_>>(),
            );

            let rhs = Array::with(&pass.ctx, c.len(), |_, _| {
                constant(pass.ctx, Value::undefined())
            });

            let_term(pass.ctx, LetStyle::Let, lhs, rhs, body)
        }
    }
}

fn fixing<'gc>(
    pass: &mut FixPass<'gc>,
    sccs: &[Vec<VertexId>],
    body: TermRef<'gc>,
    in_order: bool,
) -> (Vec<VertexId>, TermRef<'gc>) {
    fn rec<'gc>(
        pass: &mut FixPass<'gc>,
        sccs: &[Vec<VertexId>],
        body: TermRef<'gc>,
        in_order: bool,
        fixes: &mut Vec<VertexId>,
    ) -> TermRef<'gc> {
        if sccs.is_empty() {
            return body;
        }

        let body = rec(pass, &sccs[1..], body, in_order, fixes);
        fix1(in_order, pass, &sccs[0], fixes, body)
    }

    let mut fixes = Vec::new();
    let body = rec(pass, sccs, body.clone(), in_order, &mut fixes);

    (fixes, body)
}

fn rec_deps(vs: &[VertexId], pass: &mut FixPass) {
    for &v in vs.iter() {
        for &w in vs.iter() {
            if w != v
                && free_variables(pass.graph[v].rhs.unwrap(), &mut pass.fv_cache)
                    .contains(&pass.graph[w].lhs.unwrap())
            {
                pass.graph.add_edge(w, v);
            }
        }
    }
}

fn rec_deps_in_order(vs: &[VertexId], pass: &mut FixPass) {
    let mut w = vs;

    while !w.is_empty() {
        if w.len() != 1 {
            let xj = w[1];
            let xi = w[0];

            // if vertex might have side-effects add dependency.
            if !pass.graph[xi].rhs.unwrap().is_transparent() {
                pass.graph.add_edge(xj, xi);
            }

            w = &w[1..];
        } else {
            break;
        }
    }
}

fn fixing_letrec<'gc>(
    pass: &mut FixPass<'gc>,
    in_order: bool,
    lhs: &[LVarRef<'gc>],
    rhs: &[TermRef<'gc>],
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    let vertices = lhs
        .iter()
        .zip(rhs.iter())
        .enumerate()
        .map(|(ix, (lhs, rhs))| {
            let v = pass.graph.add_vertex(lhs.clone(), rhs.clone());
            pass.graph[v].idx = ix;
            v
        })
        .collect::<Vec<_>>();

    rec_deps(&vertices, pass);
    if in_order {
        rec_deps_in_order(&vertices, pass);
    }

    let mut sccs = pass.graph.tarjan_sccs();
    sccs.reverse();

    let (fixes, body) = fixing(pass, &sccs, body, in_order);

    make_fixes(pass.ctx, body.source(), &pass.graph, &fixes, body)
}

/// Fixes letrec expressions in the given term.
///
/// "Fixing" is a process of transforming letrec expressions into
/// `let` for simple or complex variables, and `fix` for mutually recursive
/// functions.
///
/// - Complex variables are those that are not free in their initializers, and also might be mutated.
/// - Simple variables are those that are free in their initializers, and are not mutated.
/// - Mutually recursive functions are those that are defined in terms of each other, and are not free
///   in their initializers.
///
///
/// For a detailed discussion, see ["Fixing Letrec: A Faithful Yet
/// Efficient Implementation of Scheme's Recursive Binding Construct"](https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf), by
/// Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig, as well as
/// "Fixing Letrec (reloaded)", by Abdulaziz Ghuloum and R. Kent Dybvig.
pub fn fix_letrec<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let fv_cache = HashMap::new();

    let mut pass = FixPass {
        fv_cache,
        graph: Graph::new(),
        ctx,
    };

    fn post_order<'gc>(x: &TermRef<'gc>, pass: &mut FixPass<'gc>) -> TermRef<'gc> {
        match &x.kind {
            TermKind::ModuleRef(..)
            | TermKind::ToplevelRef(..)
            | TermKind::Const(..)
            | TermKind::PrimRef(..)
            | TermKind::LRef(..) => *x,
            TermKind::Seq(seq) => {
                let seq = seq
                    .iter()
                    .map(|term| post_order(term, pass))
                    .collect::<Vec<_>>();
                crate::expander::core::seq(pass.ctx, Array::from_slice(&pass.ctx, seq))
            }
            TermKind::PrimCall(prim, args) => {
                let args = args
                    .iter()
                    .map(|arg| post_order(arg, pass))
                    .collect::<Vec<_>>();
                let args = Array::from_slice(&pass.ctx, args);
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::PrimCall(prim.clone(), args),
                    },
                )
            }
            TermKind::Fix(fix) => {
                let procedures = fix
                    .rhs
                    .iter()
                    .map(|proc| {
                        let body = post_order(&proc.body, pass);

                        Gc::new(
                            &pass.ctx,
                            Proc {
                                name: proc.name,
                                body,
                                args: proc.args,
                                variadic: proc.variadic,
                                source: proc.source,
                            },
                        )
                    })
                    .collect::<Vec<_>>();

                let body = post_order(&fix.body, pass);

                fix_term(
                    pass.ctx,
                    fix.lhs,
                    Array::from_slice(&pass.ctx, procedures),
                    body,
                )
            }

            TermKind::Proc(proc) => {
                let body = post_order(&proc.body, pass);
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::Proc(Gc::new(
                            &pass.ctx,
                            Proc {
                                name: proc.name,
                                body,
                                args: proc.args,
                                variadic: proc.variadic,
                                source: proc.source,
                            },
                        )),
                    },
                )
            }

            TermKind::LSet(var, val) => {
                if var.is_referenced() {
                    let val = post_order(val, pass);
                    return Gc::new(
                        &pass.ctx,
                        Term {
                            source: Lock::new(x.source()),
                            kind: TermKind::LSet(var.clone(), val),
                        },
                    );
                }
                // sets to unreferenced variables may be replaced
                // by their expression for side effects
                let val = post_order(val, pass);
                let seq = seq(pass.ctx, Array::from_slice(&pass.ctx, [val]));

                seq
            }

            TermKind::Call(proc, args) => {
                let proc = post_order(proc, pass);
                let args = args
                    .iter()
                    .map(|arg| post_order(arg, pass))
                    .collect::<Vec<_>>();
                let args = Array::from_slice(&pass.ctx, args);
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::Call(proc, args),
                    },
                )
            }

            TermKind::Define(module, var, val) => {
                let val = post_order(val, pass);
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::Define(*module, var.clone(), val),
                    },
                )
            }

            TermKind::ToplevelSet(module, var, val) => {
                let val = post_order(val, pass);
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::ToplevelSet(*module, var.clone(), val),
                    },
                )
            }

            TermKind::ModuleSet(module, var, public, val) => {
                let val = post_order(val, pass);

                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::ModuleSet(module.clone(), var.clone(), *public, val),
                    },
                )
            }

            TermKind::If(test, cons, alt) => {
                let test = post_order(test, pass);
                let cons = post_order(cons, pass);
                let alt = post_order(alt, pass);

                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::If(test, cons, alt),
                    },
                )
            }

            TermKind::Values(values) => {
                let values = values
                    .iter()
                    .map(|v| post_order(v, pass))
                    .collect::<Vec<_>>();
                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::Values(Array::from_slice(&pass.ctx, values)),
                    },
                )
            }

            TermKind::Receive(formals, opt_formals, producer, consumer) => {
                let producer = post_order(producer, pass);
                let consumer = post_order(consumer, pass);

                Gc::new(
                    &pass.ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::Receive(
                            formals.clone(),
                            opt_formals.clone(),
                            producer,
                            consumer,
                        ),
                    },
                )
            }

            TermKind::Let(l) => match l.style {
                LetStyle::LetRec | LetStyle::LetRecStar => {
                    let res = fixing_letrec(
                        pass,
                        l.style == LetStyle::LetRecStar,
                        &l.lhs,
                        &l.rhs,
                        l.body,
                    );

                    post_order(&res, pass)
                }

                _ => {
                    let rhs = l
                        .rhs
                        .iter()
                        .map(|init| post_order(init, pass))
                        .collect::<Vec<_>>();
                    let body = post_order(&l.body, pass);

                    let_term(
                        pass.ctx,
                        l.style,
                        l.lhs,
                        Array::from_slice(&pass.ctx, rhs),
                        body,
                    )
                }
            },
        }
    }

    post_order(&term, &mut pass)
}
