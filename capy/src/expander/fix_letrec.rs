/*use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use petgraph::graph::NodeIndex;
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
            println!("mutating var={:?}", var.name);
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

struct Node<'gc> {
    lhs: LVarRef<'gc>,
    rhs: TermRef<'gc>,
}

type PGraph<'gc> = petgraph::Graph<Node<'gc>, ()>;

fn unassigned_procedure<'gc>(graph: &PGraph<'gc>, v: petgraph::prelude::NodeIndex) -> bool {
    let v = &graph[v];
    let unassigned = !v.lhs.is_mutated();
    let proc = matches!(v.rhs.kind, TermKind::Proc(_));

    unassigned && proc
}

fn make_mutations<'gc>(
    ctx: Context<'gc>,
    graph: &PGraph<'gc>,
    vertices: &[NodeIndex],
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    if vertices.is_empty() {
        return body;
    }

    let mut new_seq = Vec::new();

    for v in vertices.iter().copied() {
        let var = graph[v].lhs.clone();
        println!("mutating var={:?}", var.name);
        let init = graph[v].rhs.clone();

        let mutation = lset(ctx, var, init);

        new_seq.push(mutation);
    }

    if let TermKind::Seq(seq) = &body.kind {
        new_seq.extend_from_slice(seq);
    } else {
        new_seq.push(body.clone());
    }

    seq(ctx, Array::from_slice(&ctx, new_seq))
}

struct FixPass<'gc> {
    fv_cache: HashMap<*const Term<'gc>, Rc<HashSet<LVarRef<'gc>>>>,
    graph: PGraph<'gc>,
    ctx: Context<'gc>,
}

fn make_fixes<'gc>(
    ctx: Context<'gc>,
    _source: Value<'gc>,
    graph: &PGraph<'gc>,
    vertices: &[NodeIndex],
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    if vertices.is_empty() {
        return body;
    }

    let fix_lhs = vertices
        .iter()
        .copied()
        .map(|v| graph[v].lhs)
        .collect::<Vec<_>>();
    let fix_rhs = vertices
        .iter()
        .copied()
        .map(|v| {
            if let TermKind::Proc(proc) = &graph[v].rhs.kind {
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
    scc: &[NodeIndex],
    mut fixes: Vec<NodeIndex>,
    body: TermRef<'gc>,
) -> (Vec<NodeIndex>, TermRef<'gc>) {
    let span = body.source.get();

    if scc.len() == 1 {
        let b = scc[0];

        let var = pass.graph[b].lhs;
        let init = pass.graph[b].rhs;

        if unassigned_procedure(&pass.graph, b) {
            fixes.push(b);
            return (fixes, body);
        } else if !free_variables(init, &mut pass.fv_cache).contains(&var) {
            // if var is not free in init consume
            // fixes previously saved up.

            let new_fixes = make_fixes(pass.ctx, span, &pass.graph, &fixes, body);

            (
                vec![],
                let_term(
                    pass.ctx,
                    LetStyle::Let,
                    Array::from_slice(&pass.ctx, [var]),
                    Array::from_slice(&pass.ctx, [init]),
                    new_fixes,
                ),
            )
        } else {
            // otherwise we resort to assignment. Also,
            // consumes any fixes saved up.

            let new_fixes = make_fixes(pass.ctx, span, &pass.graph, &fixes, body);

            let mutations = make_mutations(pass.ctx, &pass.graph, &[b], new_fixes);

            (
                vec![],
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
                ),
            )
        }
    } else {
        // l - contains lambdas, c - contains complex expressions

        let (mut l, mut c): (Vec<NodeIndex>, Vec<NodeIndex>) = scc
            .iter()
            .copied()
            .partition(|v| unassigned_procedure(&pass.graph, *v));

        if c.is_empty() {
            // <var_l,init_l> if init is a lambda expression and
            // var is unassigned
            fixes.append(&mut l);
            return (fixes, body);
        } else {
            if in_order {
                c.sort_by(|a, b| a.cmp(b));
            }
            // <var_c, init_c> otherwise.
            let body = {
                fixes.append(&mut l);

                let mutations = make_mutations(pass.ctx, &pass.graph, &c, body);
                make_fixes(pass.ctx, span, &pass.graph, &fixes, mutations)
            };

            let lhs = Array::from_slice(
                &pass.ctx,
                c.iter().map(|v| pass.graph[*v].lhs).collect::<Vec<_>>(),
            );

            let rhs = Array::with(&pass.ctx, c.len(), |_, _| {
                constant(pass.ctx, Value::undefined())
            });

            (vec![], let_term(pass.ctx, LetStyle::Let, lhs, rhs, body))
        }
    }
}

fn fixing<'gc>(
    pass: &mut FixPass<'gc>,
    sccs: &[Vec<NodeIndex>],
    body: TermRef<'gc>,
    in_order: bool,
) -> (Vec<NodeIndex>, TermRef<'gc>) {
    fn rec<'gc>(
        pass: &mut FixPass<'gc>,
        sccs: &[Vec<NodeIndex>],
        body: TermRef<'gc>,
        in_order: bool,
    ) -> (Vec<NodeIndex>, TermRef<'gc>) {
        if sccs.is_empty() {
            return (vec![], body);
        }

        let (fixes, body) = rec(pass, &sccs[1..], body, in_order);
        fix1(in_order, pass, &sccs[0], fixes, body)
    }

    let (fixes, body) = rec(pass, sccs, body.clone(), in_order);

    (fixes, body)
}

fn rec_deps(vs: &[NodeIndex], pass: &mut FixPass) {
    for &v in vs.iter() {
        for &w in vs.iter() {
            if w != v
                && free_variables(pass.graph[v].rhs, &mut pass.fv_cache)
                    .contains(&pass.graph[w].lhs)
            {
                pass.graph.add_edge(w, v, ());
            }
        }
    }
}

fn rec_deps_in_order(vs: &[NodeIndex], pass: &mut FixPass) {
    let mut w = vs;

    while !w.is_empty() {
        if w.len() != 1 {
            let xj = w[1];
            let xi = w[0];

            // if vertex might have side-effects add dependency.
            if !pass.graph[xi].rhs.is_transparent() {
                pass.graph.add_edge(xj, xi, ());
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
        .map(|(_, (lhs, rhs))| {
            pass.graph.add_node(Node {
                lhs: *lhs,
                rhs: *rhs,
            })
        })
        .collect::<Vec<_>>();

    rec_deps(&vertices, pass);
    if in_order {
        rec_deps_in_order(&vertices, pass);
    }

    let sccs = petgraph::algo::tarjan_scc(&pass.graph);

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
        graph: PGraph::new(),
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

                    let doc = res.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
                    println!("fixed letrec at {}:\n", x.source());
                    doc.1.render(80, &mut std::io::stdout()).unwrap();
                    println!("\n---");

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
*/

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
