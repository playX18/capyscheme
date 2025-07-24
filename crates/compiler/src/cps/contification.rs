use std::{hash::Hash, rc::Rc};

use crate::{
    ast::{Datum, P},
    cps::term::{Atom, Cont, Expression, Func, Term},
    il::term::LVar,
    utils::{TreeEq, fixedpoint},
};
use hashlink::LinkedHashMap as HashMap;
use hashlink::LinkedHashSet as HashSet;
use petgraph::{Direction, graph::NodeIndex, visit::EdgeRef};

pub trait SetUtils<T> {
    fn minimize<F>(self, shrink: F) -> Self
    where
        F: Fn(&T, &T) -> Option<T>;
}

impl<T: TreeEq> TreeEq for HashSet<T> {
    fn tree_eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.iter().zip(other.iter()).all(|(a, b)| a.tree_eq(b))
    }
}

impl<T: Clone + TreeEq + Hash + PartialEq + Eq> SetUtils<T> for HashSet<T> {
    fn minimize<F>(self, shrink: F) -> Self
    where
        F: Fn(&T, &T) -> Option<T>,
    {
        fixedpoint(self, None)(move |set| {
            let mut remaining = HashSet::new();

            if set.len() <= 1 {
                return set.clone();
            }

            let elements: Vec<_> = set.into_iter().collect();

            for pair_slice in elements.chunks(2) {
                match pair_slice {
                    [f, s] => match shrink(f, s) {
                        Some(merged) => {
                            remaining.insert(merged);
                        }

                        None => {
                            remaining.insert((*f).clone());
                            remaining.insert((*s).clone());
                        }
                    },

                    [f] => {
                        remaining.insert((*f).clone());
                    }

                    _ => {}
                }
            }

            remaining
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CNode {
    Root,
    Cont(P<LVar>),
}

impl TreeEq for CNode {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CNode::Root, CNode::Root) => true,
            (CNode::Cont(a), CNode::Cont(b)) => a.tree_eq(b),
            _ => false,
        }
    }
}

pub type Graph = petgraph::Graph<CNode, ()>;

type NodeId = NodeIndex;

pub struct Analyze {
    root: NodeId,
    graph: Graph,
    fun_ret_conts: HashMap<P<LVar>, P<LVar>>,
    fun_bodies: HashMap<P<LVar>, Rc<Term>>,
    funs: HashMap<P<LVar>, P<Func>>,
    cont_bodies: HashMap<P<LVar>, Rc<Term>>,
    seen: HashSet<P<LVar>>,
    cont_to_node: HashMap<P<LVar>, NodeId>,
}

impl Analyze {
    pub fn new() -> Self {
        let mut graph = Graph::new();
        let root = graph.add_node(CNode::Root);
        Analyze {
            root,
            graph,
            fun_ret_conts: HashMap::new(),
            fun_bodies: HashMap::new(),
            funs: HashMap::new(),
            cont_bodies: HashMap::new(),
            seen: HashSet::new(),
            cont_to_node: HashMap::new(),
        }
    }

    fn with_edge(&mut self, from: CNode, to: CNode) {
        let from = match from {
            CNode::Root => self.root,
            CNode::Cont(c) => *self
                .cont_to_node
                .entry(c.clone())
                .or_insert_with(|| self.graph.add_node(CNode::Cont(c.clone()))),
        };
        let to = match to {
            CNode::Root => self.root,
            CNode::Cont(c) => *self
                .cont_to_node
                .entry(c.clone())
                .or_insert_with(|| self.graph.add_node(CNode::Cont(c.clone()))),
        };
        self.graph.add_edge(from, to, ());
    }

    fn used_as_value(&mut self, atom: &Atom) {
        match atom {
            Atom::Local(local) if self.fun_ret_conts.contains_key(local) => {
                self.with_edge(CNode::Root, CNode::Cont(self.fun_ret_conts[local].clone()));
                self.seen.insert(local.clone());
                let body = self.fun_bodies[local].clone();
                self.build_graph(&body);
            }

            _ => (),
        }
    }

    fn do_unseen(&mut self, conts: bool, n: &P<LVar>) {
        let map = if conts {
            &self.cont_bodies
        } else {
            &self.fun_bodies
        };

        if let Some(body) = map.get(n).cloned()
            && !self.seen.contains(n)
        {
            self.seen.insert(n.clone());
            self.build_graph(&body);
        }
    }

    fn build_graph(&mut self, term: &Term) {
        match term {
            Term::Let(_, expr, body) => {
                match expr {
                    Expression::PrimCall(_, args, ..) => {
                        for arg in args.iter() {
                            self.used_as_value(arg);
                        }
                    }
                }

                self.build_graph(body);
            }

            Term::Letk(cnts, body) => {
                for cont in cnts.iter() {
                    self.cont_bodies
                        .insert(cont.binding.clone(), cont.body.clone());
                    self.with_edge(CNode::Root, CNode::Cont(cont.binding.clone()));
                }

                self.build_graph(body);
            }

            Term::Fix(funs, body) => {
                for fun in funs.iter() {
                    self.fun_bodies
                        .insert(fun.binding.clone(), fun.body.clone());
                    self.fun_ret_conts
                        .insert(fun.binding.clone(), fun.return_cont.clone());
                    self.funs.insert(fun.binding.clone(), fun.clone());
                    self.with_edge(CNode::Root, CNode::Cont(fun.binding.clone()));
                }

                self.build_graph(body);
            }

            Term::Continue(k, args, ..) => {
                for arg in args.iter() {
                    self.used_as_value(arg);
                }

                self.do_unseen(true, k);
            }

            Term::App(Atom::Local(f), retc, args, _) => {
                if let Some(fretc) = self.fun_ret_conts.get(f).cloned()
                    && self.funs[f].args.len() == args.len()
                    && self.funs[f].variadic.is_none()
                {
                    self.with_edge(CNode::Cont(retc.clone()), CNode::Cont(fretc));
                }

                for arg in args.iter() {
                    self.used_as_value(arg);
                }

                self.do_unseen(false, f);
                self.do_unseen(true, retc);
            }

            Term::App(_, _, args, _) => {
                for arg in args.iter() {
                    self.used_as_value(arg);
                }
            }

            Term::If(test, cons, alt) => {
                self.used_as_value(test);
                self.do_unseen(true, cons);
                self.do_unseen(true, alt);
            }
        }
    }

    pub fn analyze(mut self, term: &Term) -> CFGraph {
        self.build_graph(term);
        let root = self.root;
        let retcs = self
            .fun_ret_conts
            .iter()
            .map(|(_, v)| v.clone())
            .collect::<HashSet<_>>();
        let sccs = petgraph::algo::tarjan_scc(&self.graph)
            .iter()
            .filter(|scc| {
                scc.iter().all(|v| {
                    // Functions with in edge of Root cannot be contified (used as value)
                    self
                        .graph
                        .edges_directed(*v, Direction::Incoming)
                        .all(|e| e.source() != root)
                })
                &&
                // Components including non-retC continuations are just wrong
                 scc.iter()
                    .all(|node| match &self.graph[*node] {
                        CNode::Cont(c) => retcs.contains(c),
                        _ => false,
                    })
                && // The component must all return to *the same* place (i.e. static jump)
                {
                    let incoming_targets: HashSet<NodeId> = scc.iter()
                        .flat_map(|node| {
                            self.graph
                                .edges_directed(*node, Direction::Incoming)
                                .map(|e| e.source())
                        })
                        .collect::<HashSet<_>>()
                        .difference(&scc.iter().cloned().collect::<HashSet<_>>())
                        .cloned()
                        .collect();
                    incoming_targets.len() == 1
                }
            })
            .map(|scc| {
                scc.iter()
                    .map(|node| match &self.graph[*node] {
                        CNode::Cont(c) => c.clone(),
                        _ => unreachable!(),
                    })
                    .collect::<HashSet<_>>()
            })
            .collect();

        CFGraph {
            sccs,
            graph: self.graph,
            fun_ret_conts: self.fun_ret_conts,
            seen: self.seen,
            c_subst: HashMap::new(),
            f_env: HashMap::new(),
            scc_env: HashSet::new(),
            var_to_node: self.cont_to_node.clone(),
            sym_count: 0,
        }
    }
}

pub struct CFGraph {
    pub graph: Graph,
    pub fun_ret_conts: HashMap<P<LVar>, P<LVar>>,
    pub seen: HashSet<P<LVar>>,
    pub sccs: Vec<HashSet<P<LVar>>>,
    pub c_subst: HashMap<P<LVar>, P<LVar>>,
    pub f_env: HashMap<P<LVar>, P<LVar>>,
    pub scc_env: HashSet<HashSet<P<Func>>>,
    pub var_to_node: HashMap<P<LVar>, NodeId>,
    pub sym_count: usize,
}

impl std::fmt::Display for CFGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Control Flow Graph SCCs:")?;
        if self.sccs.is_empty() {
            writeln!(f, "  No SCCs found")?;
        } else {
            for (i, scc) in self.sccs.iter().enumerate() {
                writeln!(f, "  SCC {}: {{", i)?;
                for cont in scc {
                    writeln!(f, "    {}", cont.name)?;
                }
                writeln!(f, "  }}")?;
            }
        }
        Ok(())
    }
}

impl CFGraph {
    pub fn uncalled(&self, name: &P<LVar>) -> bool {
        !self.seen.contains(name)
    }

    pub fn csub(&self, name: &P<LVar>) -> P<LVar> {
        let mut subst = self.c_subst.get(name);

        while let Some(replacement) = subst {
            if let Some(next_replacement) = self.c_subst.get(replacement) {
                subst = Some(next_replacement);
            } else {
                return replacement.clone();
            }
        }

        name.clone()
    }

    pub fn with_csubst(&mut self, from: P<LVar>, to: P<LVar>) -> &mut Self {
        self.c_subst.insert(from, to);
        self
    }

    pub fn with_scc(&mut self, scc: HashSet<P<Func>>) -> &mut Self {
        for func in &scc {
            let fresh_name = LVar::new(
                Datum::make_symbol(&format!("j{}", self.sym_count), None),
                None,
            );
            self.sym_count += 1;
            self.f_env.insert(func.binding.clone(), fresh_name);
        }
        self.scc_env.insert(scc);

        self
    }

    pub fn get_j(&self, func: &P<LVar>) -> P<LVar> {
        self.f_env
            .get(func)
            .expect("Function should have a fresh name")
            .clone()
    }

    pub fn with_ks(&mut self, scc: HashSet<P<Func>>) -> &mut Self {
        let scc_cnodes: HashSet<CNode> = scc
            .iter()
            .map(|f| CNode::Cont(f.return_cont.clone()))
            .collect();

        let incoming_edges: HashSet<NodeId> = scc_cnodes
            .iter()
            .flat_map(|cnode| {
                if let CNode::Cont(cont) = cnode {
                    if let Some(&node_id) = self.var_to_node.get(cont) {
                        self.graph
                            .edges_directed(node_id, Direction::Incoming)
                            .map(|edge| edge.source())
                            .collect::<Vec<_>>()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            })
            .collect();

        let scc_node_ids: HashSet<NodeId> = scc_cnodes
            .iter()
            .filter_map(|cnode| {
                if let CNode::Cont(cont) = cnode {
                    self.var_to_node.get(cont).copied()
                } else {
                    None
                }
            })
            .collect();

        let external_incoming: Vec<NodeId> =
            incoming_edges.difference(&scc_node_ids).cloned().collect();

        if let Some(&k_node_id) = external_incoming.first() {
            if let CNode::Cont(k) = &self.graph[k_node_id] {
                let k = k.clone();
                for func in &scc {
                    let ret_cont = self.fun_ret_conts[&func.binding].clone();

                    self.with_csubst(ret_cont, k.clone());
                }
            }
        }

        self
    }
}

pub fn push_cont_definition(cnts: &HashSet<P<Cont>>, term: &P<Term>) -> Option<P<Term>> {
    fn in_set(cnts: &HashSet<P<Cont>>, name: &P<LVar>) -> bool {
        cnts.iter().any(|c| c.binding.tree_eq(name))
    }

    fn wrap(t: P<Term>, cnts: &HashSet<P<Cont>>) -> P<Term> {
        P(Term::Letk(cnts.iter().cloned().collect(), t))
    }

    fn rec(term: &P<Term>, cnts: &HashSet<P<Cont>>) -> Option<P<Term>> {
        match &**term {
            Term::Let(name, exp, body) => push_cont_definition(cnts, &body)
                .map(|new_body| P(Term::Let(name.clone(), exp.clone(), new_body))),
            Term::Letk(conts, body) => {
                let new_cnts = conts
                    .iter()
                    .map(|k| match push_cont_definition(cnts, &k.body) {
                        None => Ok(k.clone()),
                        Some(merged) => Err(k.with_body(merged)),
                    })
                    .collect::<Vec<_>>();

                let in_cnt_bodies = new_cnts.iter().any(|c| c.is_err());

                let ks = new_cnts
                    .into_iter()
                    .map(|c| match c {
                        Ok(c) => c,
                        Err(c) => c,
                    })
                    .collect::<Vec<_>>();

                match push_cont_definition(cnts, body) {
                    None if !in_cnt_bodies => None,
                    None => Some(P(Term::Letk(ks, body.clone()))),
                    Some(merged) if !in_cnt_bodies => Some(P(Term::Letk(ks, merged))),
                    Some(_) => Some(wrap(term.clone(), &cnts)),
                }
            }

            Term::Fix(funs, body) => {
                let new_funs = funs
                    .iter()
                    .map(|f| match push_cont_definition(cnts, &f.body) {
                        None => Ok(f.clone()),
                        Some(merged) => Err(P(Func {
                            name: f.name.clone(),
                            binding: f.binding.clone(),
                            return_cont: f.return_cont.clone(),
                            args: f.args.clone(),
                            variadic: f.variadic.clone(),
                            body: merged,
                            span: f.span,
                            reified: f.reified,
                        })),
                    })
                    .collect::<Vec<_>>();

                let in_fun_bodies = new_funs.iter().any(|f| f.is_err());

                let fs = new_funs
                    .into_iter()
                    .map(|f| match f {
                        Ok(f) => f,
                        Err(f) => f,
                    })
                    .collect::<Vec<_>>();

                match push_cont_definition(cnts, body) {
                    None if !in_fun_bodies => None,
                    None => Some(P(Term::Fix(fs, body.clone()))),
                    Some(merged) if !in_fun_bodies => {
                        if funs.is_empty() {
                            Some(merged)
                        } else {
                            Some(P(Term::Fix(funs.clone(), merged)))
                        }
                    } // Some(P(Term::Fix(funs.clone(), merged))),
                    Some(_) => Some(wrap(term.clone(), &cnts)),
                }
            }
            Term::Continue(k, _, _) | Term::App(_, k, _, _) if in_set(cnts, k) => {
                Some(wrap(term.clone(), &cnts))
            }

            _ => None,
        }
    }

    rec(term, cnts)
}

impl CFGraph {
    fn rec(&mut self, term: &P<Term>) -> P<Term> {
        match &**term {
            Term::Let(name, exp, body) => P(Term::Let(name.clone(), exp.clone(), self.rec(body))),
            Term::Letk(cnts, body) => {
                let cnts = cnts
                    .iter()
                    .map(|k| {
                        let body = self.rec(&k.body);
                        k.with_body(body)
                    })
                    .collect::<Vec<_>>();
                let cnts = cnts
                    .into_iter()
                    .filter(|k| !self.uncalled(&k.binding))
                    .collect::<Vec<_>>();
                let body = self.rec(body);

                P(Term::Letk(cnts, body))
            }

            Term::Fix(funs_p, body) => {
                if funs_p.is_empty() {
                    return self.rec(body);
                }

                let funs = funs_p.iter().filter(|f| !self.uncalled(&f.binding));

                let retc_mapped = funs
                    .map(|f| (f.return_cont.clone(), f.clone()))
                    .collect::<HashMap<_, _>>();
                let (sccs, rem_fs_ns): (Vec<HashSet<P<Func>>>, HashSet<P<LVar>>) =
                    self.sccs.iter().fold(
                        (Vec::new(), retc_mapped.keys().cloned().collect()),
                        |(mut sccs, fs), scc| {
                            if scc.is_subset(&fs) {
                                let mapped_scc = scc
                                    .iter()
                                    .filter_map(|retc| retc_mapped.get(retc))
                                    .cloned()
                                    .collect::<HashSet<_>>();
                                sccs.insert(0, mapped_scc);
                                let remaining_fs = fs.difference(scc).cloned().collect();
                                (sccs, remaining_fs)
                            } else {
                                (sccs, fs)
                            }
                        },
                    );

                // The remaining functions: not contifiable nor dead
                let rem_fs_p: Vec<P<Func>> = rem_fs_ns
                    .iter()
                    .filter_map(|retc| retc_mapped.get(retc))
                    .cloned()
                    .collect();
                // We can use this new state for transforming and pushing all
                // tree nodes from here forward, as it does not define continuation
                // definitions, it will only rewrite usages which are guaranteed to
                // be in the right place.
                let state = sccs.iter().fold(self, |acc, scc| {
                    acc.with_scc(scc.clone());
                    acc
                });
                for scc in &sccs {
                    state.with_ks(scc.clone());
                }

                let new_state = state;
                let mut sccs_k = Vec::new();

                for scc in &sccs {
                    let mut scc_k = Vec::new();
                    for func in scc {
                        let j = new_state.get_j(&func.binding);
                        let body = new_state.rec(&func.body);
                        scc_k.push(P(Cont {
                            args: func.args.clone(),
                            binding: j,
                            body,
                            variadic: func.variadic.clone(),
                            span: func.span,
                            name: func.name.clone(),
                        }));
                    }
                    sccs_k.push(scc_k);
                }

                let rem_fs: Vec<P<Func>> = rem_fs_p
                    .iter()
                    .map(|func| {
                        let body = new_state.rec(&func.body);
                        P(Func {
                            name: func.name.clone(),
                            binding: func.binding.clone(),
                            return_cont: func.return_cont.clone(),
                            args: func.args.clone(),
                            variadic: func.variadic.clone(),
                            body,
                            span: func.span,
                            reified: func.reified,
                        })
                    })
                    .collect();
                /* A SCC usage can occur in one of three places:
                 * (1) The 'body' of this LetF.
                 * (2) The body of a [single] function in 'remFs'.
                 * (3) The body of a [single] continuation in 'sccs'
                 *     that is NOT the same scc.
                 *
                 * The set of mutually recursive continuations needs
                 * to be pushed as far down the respective body as
                 * possible.
                 *
                 * First, try to identify SCCS that are used in another
                 * SCC. This will reduce the list of SCCS to handle.
                 *
                 * Second, try to identify SCCS that are used in a body
                 * of 'remFs' and place them there.
                 *
                 * Third, the usage must be in the body of the LetF,
                 * push each one down the tree there.
                 *
                 */

                let collapsed_sccs = sccs_k
                    .iter()
                    .map(|scc| scc.iter().cloned().collect::<HashSet<_>>())
                    .collect::<HashSet<_>>()
                    .minimize(collapse);

                let new_body = new_state.rec(body);

                collapsed_sccs
                    .iter()
                    .rfold(
                        P(Term::Fix(rem_fs, new_body)),
                        |acc, scc| match push_cont_definition(scc, &acc) {
                            Some(good) => new_state.rewrite_all(&good),
                            None => unreachable!("contifiable component could not be merged"),
                        },
                    )
            }

            Term::App(Atom::Local(f), retc, args, span) => match self.f_env.get(f).cloned() {
                Some(k) => P(Term::Continue(k.clone(), args.clone(), *span)),
                None => P(Term::App(
                    Atom::Local(f.clone()),
                    self.csub(retc),
                    args.clone(),
                    *span,
                )),
            },

            Term::App(f, retc, args, span) => {
                let args = args.clone();
                let retc = self
                    .c_subst
                    .get(retc)
                    .cloned()
                    .unwrap_or_else(|| retc.clone());
                P(Term::App(f.clone(), retc, args, *span))
            }

            Term::Continue(k, args, span) => {
                let args = args.clone();

                let k = self.csub(k);

                P(Term::Continue(k, args, *span))
            }

            Term::If(test, cons, alt) => {
                let test = test.clone();
                let cons = self
                    .c_subst
                    .get(cons)
                    .cloned()
                    .unwrap_or_else(|| cons.clone());
                let alt = self
                    .c_subst
                    .get(alt)
                    .cloned()
                    .unwrap_or_else(|| alt.clone());
                P(Term::If(test, cons, alt))
            }
        }
    }

    fn rewrite_all(&mut self, term: &P<Term>) -> P<Term> {
        match &**term {
            Term::Let(name, exp, body) => {
                let body = self.rewrite_all(body);
                P(Term::Let(name.clone(), exp.clone(), body))
            }

            Term::Letk(cnts, body) => {
                let cnts = cnts
                    .iter()
                    .map(|k| {
                        let body = self.rewrite_all(&k.body);
                        k.with_body(body)
                    })
                    .collect::<Vec<_>>();

                let body = self.rewrite_all(body);

                P(Term::Letk(cnts, body))
            }

            Term::Fix(funs, body) => {
                let funs = funs
                    .iter()
                    .map(|f| {
                        let body = self.rewrite_all(&f.body);
                        P(Func {
                            name: f.name.clone(),
                            binding: f.binding.clone(),
                            return_cont: f.return_cont.clone(),
                            args: f.args.clone(),
                            variadic: f.variadic.clone(),
                            body,
                            span: f.span,
                            reified: f.reified,
                        })
                    })
                    .collect::<Vec<_>>();

                let body = self.rewrite_all(body);

                P(Term::Fix(funs, body))
            }

            Term::App(Atom::Local(f), retc, args, span) => match self.f_env.get(f).cloned() {
                Some(k) => {
                    let k = self.csub(&k);

                    P(Term::Continue(k, args.clone(), *span))
                }

                _ => {
                    let args = args.clone();
                    let retc = self
                        .c_subst
                        .get(retc)
                        .cloned()
                        .unwrap_or_else(|| retc.clone());
                    P(Term::App(Atom::Local(f.clone()), retc.clone(), args, *span))
                }
            },

            Term::App(f, retc, args, span) => {
                let args = args.clone();
                let retc = self
                    .c_subst
                    .get(retc)
                    .cloned()
                    .unwrap_or_else(|| retc.clone());
                P(Term::App(f.clone(), retc, args, *span))
            }

            Term::If(test, cons, alt) => {
                let test = test.clone();
                let cons = self
                    .c_subst
                    .get(cons)
                    .cloned()
                    .unwrap_or_else(|| cons.clone());
                let alt = self
                    .c_subst
                    .get(alt)
                    .cloned()
                    .unwrap_or_else(|| alt.clone());
                P(Term::If(test, cons, alt))
            }

            _ => term.clone(),
        }
    }
}

fn collapse(lhs: &HashSet<P<Cont>>, rhs: &HashSet<P<Cont>>) -> Option<HashSet<P<Cont>>> {
    fn collapse_scc(from: &HashSet<P<Cont>>, to: &HashSet<P<Cont>>) -> Option<HashSet<P<Cont>>> {
        let pushed = to
            .iter()
            .map(|c| match push_cont_definition(&from, &c.body) {
                None => Ok(c.clone()),
                Some(merged) => Err(c.with_body(merged)),
            })
            .collect::<HashSet<_>>();

        if pushed.iter().any(|c| c.is_err()) {
            Some(
                pushed
                    .into_iter()
                    .map(|c| match c {
                        Ok(c) => c,
                        Err(c) => c,
                    })
                    .collect(),
            )
        } else {
            None
        }
    }

    collapse_scc(lhs, rhs).or_else(|| collapse_scc(rhs, lhs))
}

pub fn contify(term: P<Term>) -> P<Term> {
    let analyzer = Analyze::new();
    let mut cfgraph = analyzer.analyze(&term);
    cfgraph.rec(&term)
}
