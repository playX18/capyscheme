use std::collections::{HashMap, HashSet};

use crate::source::Span;
use crate::{ast::*, il::term::*};
use tree_sitter::Point;

pub fn free_variables(
    expr: &P<IForm>,
    cache: &mut HashMap<P<IForm>, P<HashSet<P<LVar>>>>,
) -> P<HashSet<P<LVar>>> {
    if let Some(free) = cache.get(expr) {
        return free.clone();
    }

    fn rec(expr: &IForm, set: &mut HashSet<P<LVar>>) {
        match &expr.term {
            ITerm::PrimRef(_) | ITerm::GRef(_) | ITerm::Const(_) => (),

            ITerm::Fix(fix) => {
                for proc in fix.procedures.iter() {
                    for case in proc.cases.iter() {
                        let mut set2 = HashSet::new();
                        rec(&case.body, &mut set2);
                        for var in case.args.iter().chain(case.variadic.iter()) {
                            set2.remove(var);
                        }

                        set.extend(set2);
                    }
                }
            }
            ITerm::LRef(var) => {
                set.insert(var.clone());
            }
            ITerm::App(proc, args) => {
                rec(&proc, set);
                args.iter().for_each(|arg| rec(arg, set));
            }
            ITerm::PrimApp(_, args) => {
                args.iter().for_each(|arg| rec(arg, set));
            }
            ITerm::Define(_, val) | ITerm::LSet(_, val) | ITerm::GSet(_, val) => rec(val, set),

            ITerm::If(test, cons, alt) => {
                rec(test, set);
                rec(cons, set);
                rec(alt, set);
            }

            ITerm::Seq(seq) => {
                for x in seq.iter() {
                    rec(x, set);
                }
            }

            ITerm::Let(l) => {
                if let LetStyle::Let = l.style {
                    for init in l.initializers.iter() {
                        rec(&init, set);
                    }

                    let mut set2 = HashSet::new();
                    rec(&l.body, &mut set2);

                    for var in l.variables.iter() {
                        set2.remove(var);
                    }

                    set.extend(set2);
                } else {
                    let mut set2 = HashSet::new();
                    for init in l.initializers.iter() {
                        rec(&init, &mut set2);
                    }

                    rec(&l.body, &mut set2);

                    for var in l.variables.iter() {
                        set2.remove(var);
                    }

                    set.extend(set2);
                }
            }

            ITerm::Proc(proc) => {
                for case in proc.cases.iter() {
                    let mut set2 = HashSet::new();
                    rec(&case.body, &mut set2);
                    for var in case.args.iter().chain(case.variadic.iter()) {
                        set2.remove(var);
                    }
                    set.extend(set2);
                }
            }
        }
    }
    let mut set = HashSet::new();
    rec(expr, &mut set);
    let set = P(set);
    cache.insert(expr.clone(), set.clone());
    set
}

#[allow(dead_code)]
struct Vertex {
    ix: VertexId,
    lhs: P<LVar>,
    rhs: P<IForm>,
    idx: usize,
    adjacency: Vec<VertexId>,
    number: isize,
    lowlink: isize,
    onstack: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct VertexId(u32);

struct Graph {
    vertices: Vec<Vertex>,
}

impl std::ops::Index<VertexId> for Graph {
    type Output = Vertex;

    fn index(&self, index: VertexId) -> &Self::Output {
        &self.vertices[index.0 as usize]
    }
}

impl std::ops::IndexMut<VertexId> for Graph {
    fn index_mut(&mut self, index: VertexId) -> &mut Self::Output {
        &mut self.vertices[index.0 as usize]
    }
}

impl Graph {
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
            ix: VertexId(ix as _),
            lhs: LVar::new(Datum::new(DatumValue::Undefined), None),
            rhs: P(IForm {
                span: Span::new(
                    None,
                    0,
                    0,
                    Point { row: 0, column: 0 },
                    Point { row: 0, column: 0 },
                ),
                term: ITerm::Const(Datum::new(DatumValue::Undefined)),
            }),
            adjacency: Vec::new(),
            number: -1,
            lowlink: -1,
            onstack: false,
        });
        VertexId(ix as _)
    }

    fn add_vertex(&mut self, lhs: P<LVar>, rhs: P<IForm>) -> VertexId {
        let vertex = self.add_empty_vertex();
        self[vertex].lhs = lhs;
        self[vertex].rhs = rhs;
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
        let unassigned = !v.lhs.is_mutated();
        let proc = matches!(v.rhs.term, ITerm::Proc(_));

        unassigned & proc
    }

    fn make_mutations(&self, vertices: &[VertexId], body: P<IForm>) -> P<IForm> {
        if vertices.is_empty() {
            return body;
        }

        let mut new_seq = Vec::new();

        for v in vertices.iter().copied() {
            let var = self[v].lhs.clone();
            if !var.is_mutated() {
                var.set();
            }

            let init = self[v].rhs.clone();

            let mutation = P(IForm {
                span: init.span,
                term: ITerm::LSet(var, init),
            });

            new_seq.push(mutation);
        }

        if let ITerm::Seq(seq) = &body.term {
            new_seq.extend_from_slice(seq);
        } else {
            new_seq.push(body.clone());
        }

        P(IForm {
            span: body.span,
            term: ITerm::Seq(new_seq),
        })
    }
}

#[allow(dead_code)]
struct FixPass {

    fv_cache: HashMap<P<IForm>, P<HashSet<P<LVar>>>>,
    graph: Graph,
}

fn make_fixes(span: Span, graph: &Graph, vertices: &[VertexId], body: P<IForm>) -> P<IForm> {
    if vertices.is_empty() {
        return body;
    }

    let fix_lhs = vertices
        .iter()
        .copied()
        .map(|v| graph[v].lhs.clone())
        .collect();
    let fix_rhs = vertices
        .iter()
        .copied()
        .map(|v| {
            if let ITerm::Proc(proc) = &graph[v].rhs.term {
                proc.clone()
            } else {
                panic!("only procedures go into <fix> forms")
            }
        })
        .collect();

    P(IForm {
        span,
        term: ITerm::Fix(Fix {
            variables: fix_lhs,
            procedures: fix_rhs,
            body,
        }),
    })
}

fn fix1(
    in_order: bool,
    pass: &mut FixPass,
    scc: &[VertexId],
    fixes: &mut Vec<VertexId>,
    body: P<IForm>,
) -> P<IForm> {
    let span = body.span;

    if scc.len() == 1 {
        let b = scc[0];

        let var = pass.graph[b].lhs.clone();
        let init = pass.graph[b].rhs.clone();

        if pass.graph.unassigned_procedure(b) {
            fixes.push(b);
            return body;
        } else if !free_variables(&init, &mut pass.fv_cache).contains(&var) {
            // if var is not free in init consume
            // fixes previously saved up.

            let new_fixes = make_fixes(span, &pass.graph, fixes, body);
            fixes.clear();
            let bind = P(IForm {
                span,
                term: ITerm::Let(Let {
                    style: LetStyle::Let,
                    variables: vec![var.clone()],
                    initializers: vec![init],
                    body: new_fixes,
                }),
            });

            return bind;
        } else {
            // otherwise we resort to assignment. Also,
            // consumes any fixes saved up.

            let new_fixes = make_fixes(span, &pass.graph, fixes, body);
            fixes.clear();
            let mutations = pass.graph.make_mutations(&[b], new_fixes);

            return P(IForm {
                span,
                term: ITerm::Let(Let {
                    style: LetStyle::Let,
                    variables: vec![var],
                    initializers: vec![P(IForm {
                        span,
                        term: ITerm::Const(Datum::new(DatumValue::Undefined)),
                    })],
                    body: mutations,
                }),
            });
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
            fixes.extend(l);
            return body;
        } else {
            if in_order {
                c.sort_by(|a, b| pass.graph[*a].idx.cmp(&pass.graph[*b].idx));
            }

            // <var_c, init_c> otherwise.
            return P(IForm {
                span,
                term: ITerm::Let(Let {
                    style: LetStyle::Let,
                    variables: c.iter().map(|v| pass.graph[*v].lhs.clone()).collect(),
                    initializers: vec![
                        P(IForm {
                            span,
                            term: ITerm::Const(Datum::new(DatumValue::Undefined))
                        });
                        c.len()
                    ],
                    body: {
                        let mut fixes = fixes.clone();
                        fixes.append(&mut l);
                        let mutations = pass.graph.make_mutations(&c, body);
                        make_fixes(span, &pass.graph, &fixes, mutations)
                    },
                }),
            });
        }
    }
}

fn fixing(
    pass: &mut FixPass,
    sccs: &[Vec<VertexId>],
    body: &P<IForm>,
    in_order: bool,
) -> (Vec<VertexId>, P<IForm>) {
    fn rec(
        pass: &mut FixPass,
        sccs: &[Vec<VertexId>],
        body: P<IForm>,
        in_order: bool,
        fixes: &mut Vec<VertexId>,
    ) -> P<IForm> {
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
            pass.graph.add_edge(w, v);
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
            if !pass.graph[xi].rhs.is_transparent() {
                pass.graph.add_edge(xj, xi);
            }

            w = &w[1..];
        } else {
            break;
        }
    }
}

fn fixing_letrec(
    pass: &mut FixPass,
    in_order: bool,
    lhs: &[P<LVar>],
    rhs: &[P<IForm>],
    body: P<IForm>,
) -> P<IForm> {
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

    let sccs = pass.graph.tarjan_sccs();

    let (fixes, body) = fixing(pass, &sccs, &body, in_order);

    make_fixes(body.span, &pass.graph, &fixes, body)
}

/// Fixes letrec expressions in the given IForm.
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
pub fn fix_letrec(x: P<IForm>) -> P<IForm> {
    x.count_refs();
    let fv_cache = HashMap::new();

    let mut pass = FixPass {
        fv_cache,
        graph: Graph::new(),
    };

    fn post_order(x: &P<IForm>, pass: &mut FixPass) -> P<IForm> {
        match &x.term {
            ITerm::PrimApp(prim, args) => P(IForm {
                span: x.span,
                term: ITerm::PrimApp(
                    *prim,
                    args.iter().map(|arg| post_order(arg, pass)).collect(),
                ),
            }),
            ITerm::Fix(fix) => {
                let procedures = fix
                    .procedures
                    .iter()
                    .map(|proc| {
                        let cases = proc
                            .cases
                            .iter()
                            .map(|case| ProcCase {
                                loc: case.loc,
                                args: case.args.clone(),
                                variadic: case.variadic.clone(),
                                body: post_order(&case.body, pass),
                            })
                            .collect();
                        P(Proc {
                            loc: proc.loc,
                            name: proc.name.clone(),
                            cases,
                        })
                    })
                    .collect();
                P(IForm {
                    span: x.span,
                    term: ITerm::Fix(Fix {
                        variables: fix.variables.clone(),
                        procedures,
                        body: post_order(&fix.body, pass),
                    }),
                })
            }

            ITerm::Proc(proc) => {
                let cases = proc
                    .cases
                    .iter()
                    .map(|case| ProcCase {
                        loc: case.loc,
                        args: case.args.clone(),
                        variadic: case.variadic.clone(),
                        body: post_order(&case.body, pass),
                    })
                    .collect();
                let proc = P(Proc {
                    loc: proc.loc,
                    name: proc.name.clone(),
                    cases,
                });

                P(IForm {
                    span: x.span,
                    term: ITerm::Proc(proc),
                })
            }

            ITerm::LSet(var, val) => {
                println!("lset {} (refc: {}, setc: {})", var.name, var.ref_count(), var.set_count());
                if var.ref_count() != 0 {
                    return P(IForm {
                        span: x.span,
                        term: ITerm::LSet(var.clone(), post_order(val, pass)),
                    });
                }

                println!("removing unused LSet: {}", var.name);
                
                // sets to unreferenced variables may be replaced
                // by their expression for side effects.
                P(IForm {
                    span: x.span,
                    term: ITerm::Seq(vec![
                        post_order(val, pass),
                        IForm::constant(x.span, DatumValue::Undefined),
                    ]),
                })
            }

            ITerm::App(proc, args) => {
                let proc = post_order(proc, pass);
                let args = args.iter().map(|arg| post_order(arg, pass));

                P(IForm {
                    span: x.span,
                    term: ITerm::App(proc, args.collect()),
                })
            }

            ITerm::Define(var, val) => P(IForm {
                span: x.span,
                term: ITerm::Define(var.clone(), post_order(val, pass)),
            }),

            ITerm::GSet(var, val) => P(IForm {
                span: x.span,
                term: ITerm::GSet(var.clone(), post_order(val, pass)),
            }),
            ITerm::If(test, cons, alt) => P(IForm {
                span: x.span,
                term: ITerm::If(
                    post_order(test, pass),
                    post_order(cons, pass),
                    post_order(alt, pass),
                ),
            }),

            ITerm::Seq(seq) => P(IForm {
                span: x.span,
                term: ITerm::Seq(seq.iter().map(|x| post_order(x, pass)).collect()),
            }),

            ITerm::Let(l) => match l.style {
                LetStyle::LetRec | LetStyle::LetRecStar => {
                    let res = fixing_letrec(
                        pass,
                        l.style == LetStyle::LetRecStar,
                        &l.variables,
                        &l.initializers,
                        l.body.clone(),
                    );

                    post_order(&res, pass)
                }

                _ => {
                    let initializers = l.initializers.iter().map(|x| post_order(x, pass)).collect();
                    let body = post_order(&l.body, pass);

                    P(IForm {
                        span: x.span,
                        term: ITerm::Let(Let {
                            style: l.style,
                            variables: l.variables.clone(),
                            initializers,
                            body,
                        }),
                    })
                }
            },

            ITerm::Const(_) | ITerm::PrimRef(_) | ITerm::LRef(_) | ITerm::GRef(_) => x.clone(),
        }
    }

    post_order(&x, &mut pass)
}
