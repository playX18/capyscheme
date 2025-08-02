//! Contification of CPS program.
//!
//!
//! This pass optimizes the CPS program by converting functions into continuations
//! where possible, which can lead to more efficient code generation thus better
//! performance at runtime.

use crate::runtime::Context;
use core::fmt;
use std::collections::VecDeque;
use std::hash::Hash;

use super::term::*;
use hashlink::LinkedHashMap as HashMap;
use hashlink::LinkedHashSet as HashSet;
use petgraph::Directed;
use petgraph::Direction;
use petgraph::dot::Config;

use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use rsgc::Gc;
use rsgc::alloc::Array;

use crate::expander::core::LVarRef;
use crate::expander::core::fresh_lvar;
use crate::runtime::value::Symbol;
use crate::utils::TreeEq;
use crate::utils::fixedpoint;

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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum CNode<'gc> {
    Root,
    Cont(LVarRef<'gc>),
}

impl TreeEq for CNode<'_> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CNode::Root, CNode::Root) => true,
            (CNode::Cont(a), CNode::Cont(b)) => a.tree_eq(b),
            _ => false,
        }
    }
}

pub type Graph<'gc> = petgraph::Graph<CNode<'gc>, (), Directed>;

pub struct Analyze<'gc> {
    root: NodeIndex,
    graph: Graph<'gc>,
    fun_ret_conts: HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    fun_bodies: HashMap<LVarRef<'gc>, TermRef<'gc>>,
    funs: HashMap<LVarRef<'gc>, FuncRef<'gc>>,
    cont_bodies: HashMap<LVarRef<'gc>, TermRef<'gc>>,
    seen: HashSet<LVarRef<'gc>>,
    cont_to_node: HashMap<LVarRef<'gc>, NodeIndex>,

    workstack: VecDeque<TermRef<'gc>>,
}

impl<'gc> Analyze<'gc> {
    pub fn new() -> Self {
        let mut graph = Graph::with_capacity(128, 256);
        let root = graph.add_node(CNode::Root);
        Analyze {
            root,
            graph: Graph::new(),
            fun_ret_conts: HashMap::new(),
            fun_bodies: HashMap::new(),
            funs: HashMap::new(),
            cont_bodies: HashMap::new(),
            seen: HashSet::new(),
            cont_to_node: HashMap::new(),
            workstack: VecDeque::new(),
        }
    }

    pub fn root(&self) -> NodeIndex {
        self.root
    }

    pub fn graph(&self) -> &Graph<'gc> {
        &self.graph
    }

    pub fn with_edge(&mut self, from: CNode<'gc>, to: CNode<'gc>) {
        let from = match from {
            CNode::Root => self.root,
            CNode::Cont(cont) => *self
                .cont_to_node
                .entry(cont)
                .or_insert_with(|| self.graph.add_node(CNode::Cont(cont))),
        };

        let to = match to {
            CNode::Root => self.root,
            CNode::Cont(cont) => *self
                .cont_to_node
                .entry(cont)
                .or_insert_with(|| self.graph.add_node(CNode::Cont(cont))),
        };

        self.graph.add_edge(from, to, ());
    }

    pub fn used_as_value(&mut self, atom: Atom<'gc>) {
        let Atom::Local(local) = atom else {
            return;
        };

        if !self.fun_ret_conts.contains_key(&local) {
            return;
        }

        self.with_edge(CNode::Root, CNode::Cont(self.fun_ret_conts[&local]));
        self.seen.insert(local);
        if !self.seen.contains(&local) {
            self.seen.insert(local);
            let body = self.fun_bodies[&local];
            self.workstack.push_back(body);
            //self.build_graph(body);
        }
    }

    pub fn do_unseen(&mut self, conts: bool, n: LVarRef<'gc>) {
        let map = if conts {
            &self.cont_bodies
        } else {
            &self.fun_bodies
        };

        if let Some(body) = map.get(&n)
            && !self.seen.contains(&n)
        {
            self.seen.insert(n);
            self.workstack.push_back(*body);
            //self.build_graph(*body);
        }
    }

    fn build_graph(&mut self, term: TermRef<'gc>) {
        match *term {
            Term::Let(_, expr, body) => {
                match expr {
                    Expression::PrimCall(_, args, ..) => {
                        for &arg in args.iter() {
                            self.used_as_value(arg);
                        }
                    }
                }

                self.build_graph(body);
            }

            Term::Letk(conts, body) => {
                for cont in conts.iter() {
                    self.cont_bodies
                        .insert(cont.binding(), cont.body().unwrap());
                    self.with_edge(CNode::Root, CNode::Cont(cont.binding()));
                }

                self.build_graph(body);
            }

            Term::Fix(funs, body) => {
                for fun in funs.iter() {
                    self.fun_bodies.insert(fun.binding, fun.body);
                    self.funs.insert(fun.binding, fun.clone());
                    self.fun_ret_conts.insert(fun.binding, fun.return_cont);
                    self.with_edge(CNode::Root, CNode::Cont(fun.binding));
                }
                self.build_graph(body);
            }

            Term::Continue(k, args, ..) => {
                for &arg in args.iter() {
                    self.used_as_value(arg);
                }

                self.do_unseen(true, k);
            }

            Term::App(Atom::Local(f), retc, args, _) => {
                if let Some(&fretc) = self.fun_ret_conts.get(&f)
                    && self.funs[&f].args.len() == args.len()
                    && self.funs[&f].variadic.is_none()
                {
                    self.with_edge(CNode::Cont(retc), CNode::Cont(fretc));
                }
                for &arg in args.iter() {
                    self.used_as_value(arg);
                }

                self.do_unseen(false, f);
                self.do_unseen(true, retc);
            }

            Term::App(_, k, args, _) => {
                for arg in args.iter() {
                    self.used_as_value(*arg);
                }

                self.do_unseen(true, k);
            }

            Term::If(test, cons, alt, _) => {
                self.used_as_value(test);
                self.do_unseen(true, cons);
                self.do_unseen(true, alt);
            }

            Term::Throw(throw, _) => match throw {
                Throw::Value(key, args)
                | Throw::ValueAndData(key, args)
                | Throw::Throw(key, args) => {
                    self.used_as_value(key);
                    self.used_as_value(args);
                }
            },
        }
    }

    pub fn analyze(mut self, ctx: Context<'gc>, func: FuncRef<'gc>) -> CFGraph<'gc> {
        self.fun_bodies.insert(func.binding, func.body);
        self.fun_ret_conts.insert(func.binding, func.return_cont);
        // self.with_edge(CNode::Root, CNode::Cont(func.binding));
        self.workstack.push_back(func.body);

        while let Some(term) = self.workstack.pop_front() {
            self.build_graph(term);
        }

        let root = self.root;

        let retcs = self
            .fun_ret_conts
            .iter()
            .map(|(_, v)| *v)
            .collect::<HashSet<_>>();

        let sccs = petgraph::algo::tarjan_scc(&self.graph)
            .iter()
            .filter(|scc| {
                scc.iter().all(|&v| {
                    self.graph
                        .edges_directed(v, Direction::Incoming)
                        .all(|e| e.source() != root)
                }) && scc.iter().all(|&node| match &self.graph[node] {
                    CNode::Cont(c) => retcs.contains(c),
                    _ => false,
                }) && {
                    let incoming_targets: HashSet<NodeIndex> = scc
                        .iter()
                        .flat_map(|&node| {
                            self.graph
                                .edges_directed(node, Direction::Incoming)
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
                        CNode::Cont(c) => *c,
                        _ => unreachable!(),
                    })
                    .collect()
            })
            .collect();

        CFGraph {
            ctx,
            sccs,
            graph: self.graph,
            fun_ret_conts: self.fun_ret_conts,
            seen: self.seen,
            c_subst: HashMap::new(),
            f_env: HashMap::new(),
            scc_env: HashSet::new(),
            var_to_node: self.cont_to_node,
            sym_count: 0,
        }
    }
}

pub struct CFGraph<'gc> {
    pub ctx: Context<'gc>,
    pub graph: Graph<'gc>,
    pub fun_ret_conts: HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    pub seen: HashSet<LVarRef<'gc>>,
    pub sccs: Vec<HashSet<LVarRef<'gc>>>,
    pub c_subst: HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    pub f_env: HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    pub scc_env: HashSet<HashSet<FuncRef<'gc>>>,
    pub var_to_node: HashMap<LVarRef<'gc>, NodeIndex>,
    pub sym_count: usize,
}

impl fmt::Debug for CNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CNode::Root => write!(f, "Root"),
            CNode::Cont(c) => write!(f, "{}", c.name),
        }
    }
}

impl<'gc> CFGraph<'gc> {
    pub fn dot(&self) -> petgraph::dot::Dot<'_, &'_ Graph<'gc>> {
        let dot = petgraph::dot::Dot::with_config(&self.graph, &[Config::EdgeNoLabel]);
        dot
    }

    pub fn uncalled(&self, name: LVarRef<'gc>) -> bool {
        !self.seen.contains(&name)
    }

    pub fn csub(&self, name: LVarRef<'gc>) -> LVarRef<'gc> {
        let mut subst = self.c_subst.get(&name);
        while let Some(replacement) = subst {
            if let Some(next_replacement) = self.c_subst.get(replacement) {
                subst = Some(next_replacement);
            } else {
                return *replacement;
            }
        }

        name
    }

    pub fn with_csubst(&mut self, from: LVarRef<'gc>, to: LVarRef<'gc>) -> &mut Self {
        self.c_subst.insert(from, to);

        self
    }

    pub fn with_scc(&mut self, scc: HashSet<FuncRef<'gc>>) -> &mut Self {
        for &func in &scc {
            let fresh_name = fresh_lvar(
                self.ctx,
                Symbol::from_str(self.ctx, &format!("j{}", self.sym_count)).into(),
            );
            self.sym_count += 1;
            self.f_env.insert(func.binding, fresh_name);
        }

        self.scc_env.insert(scc);
        self
    }

    pub fn get_j(&self, func: LVarRef<'gc>) -> LVarRef<'gc> {
        self.f_env
            .get(&func)
            .copied()
            .expect("Function should have a fresh name in the environment")
    }

    pub fn with_ks(&mut self, scc: HashSet<FuncRef<'gc>>) -> &mut Self {
        let scc_cnodes: HashSet<CNode> = scc.iter().map(|f| CNode::Cont(f.return_cont)).collect();

        let incoming_edges: HashSet<NodeIndex> = scc_cnodes
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

        let scc_node_ids: HashSet<NodeIndex> = scc_cnodes
            .iter()
            .filter_map(|cnode| {
                if let CNode::Cont(cont) = cnode {
                    self.var_to_node.get(cont).copied()
                } else {
                    None
                }
            })
            .collect();

        let external_incoming: Vec<NodeIndex> =
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

pub fn push_cont_definition<'gc>(
    ctx: Context<'gc>,
    cnts: &HashSet<ContRef<'gc>>,
    term: TermRef<'gc>,
) -> Option<TermRef<'gc>> {
    fn in_set<'gc>(cnts: &HashSet<ContRef<'gc>>, name: LVarRef<'gc>) -> bool {
        cnts.iter().any(|c| c.binding() == name)
    }

    fn wrap<'gc>(ctx: Context<'gc>, t: TermRef<'gc>, cnts: &HashSet<ContRef<'gc>>) -> TermRef<'gc> {
        let cnts = cnts.iter().copied().collect::<Vec<_>>();
        let cnts = Array::from_array(&ctx, cnts);
        Gc::new(&ctx, Term::Letk(cnts, t))
    }

    fn rec<'gc>(
        ctx: Context<'gc>,
        term: TermRef<'gc>,
        cnts: &HashSet<ContRef<'gc>>,
    ) -> Option<TermRef<'gc>> {
        match *term {
            Term::Let(name, exp, body) => push_cont_definition(ctx, cnts, body)
                .map(|new_body| Gc::new(&ctx, Term::Let(name, exp, new_body))),

            Term::Letk(conts, body) => {
                let new_cnts = conts
                    .iter()
                    .map(
                        |k| match push_cont_definition(ctx, cnts, k.body().unwrap()) {
                            None => Ok(*k),
                            Some(merged) => Err(k.with_body(ctx, merged)),
                        },
                    )
                    .collect::<Vec<_>>();
                let in_cnt_bodies = new_cnts.iter().any(|c| c.is_err());

                let ks = new_cnts
                    .into_iter()
                    .map(|c| match c {
                        Ok(c) => c,
                        Err(c) => c,
                    })
                    .collect::<Vec<_>>();
                let ks = Array::from_array(&ctx, ks);
                match push_cont_definition(ctx, cnts, body) {
                    None if !in_cnt_bodies => None,
                    None => Some(Gc::new(&ctx, Term::Letk(ks, body))),
                    Some(merged) if !in_cnt_bodies => Some(Gc::new(&ctx, Term::Letk(ks, merged))),
                    Some(_) => Some(wrap(ctx, term, cnts)),
                }
            }

            Term::Fix(funs, body) => {
                let new_funs = funs
                    .iter()
                    .map(|f| match push_cont_definition(ctx, cnts, f.body) {
                        None => Ok(*f),
                        Some(merged) => Err(f.with_body(ctx, merged)),
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

                let fs = Array::from_array(&ctx, fs);

                match push_cont_definition(ctx, cnts, body) {
                    None if !in_fun_bodies => None,
                    None => Some(Gc::new(&ctx, Term::Fix(fs, body))),
                    Some(merged) if !in_fun_bodies => {
                        if funs.is_empty() {
                            Some(merged)
                        } else {
                            Some(Gc::new(&ctx, Term::Fix(fs, merged)))
                        }
                    }
                    Some(_) => Some(wrap(ctx, term, cnts)),
                }
            }

            Term::Continue(k, _, _) | Term::App(_, k, _, _) if in_set(cnts, k) => {
                Some(wrap(ctx, term, cnts))
            }

            _ => None,
        }
    }

    rec(ctx, term, cnts)
}

fn collapse<'gc>(
    ctx: Context<'gc>,
    lhs: &HashSet<ContRef<'gc>>,
    rhs: &HashSet<ContRef<'gc>>,
) -> Option<HashSet<ContRef<'gc>>> {
    fn collapse_scc<'gc>(
        ctx: Context<'gc>,
        from: &HashSet<ContRef<'gc>>,
        to: &HashSet<ContRef<'gc>>,
    ) -> Option<HashSet<ContRef<'gc>>> {
        let pushed = to
            .iter()
            .map(
                |c| match push_cont_definition(ctx, from, c.body().unwrap()) {
                    None => Ok(*c),
                    Some(merged) => Err(c.with_body(ctx, merged)),
                },
            )
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

    collapse_scc(ctx, lhs, rhs).or_else(|| collapse_scc(ctx, rhs, lhs))
}

impl<'gc> CFGraph<'gc> {
    fn rec(&mut self, term: TermRef<'gc>) -> TermRef<'gc> {
        match *term {
            Term::Let(bind, expr, body) => {
                let body = self.rec(body);

                Gc::new(&self.ctx, Term::Let(bind, expr, body))
            }

            Term::Letk(cnts, body) => {
                let cnts = cnts
                    .iter()
                    .map(|k| {
                        let body = self.rec(k.body().unwrap());
                        k.with_body(self.ctx, body)
                    })
                    .collect::<Vec<_>>();
                let cnts = cnts
                    .into_iter()
                    .filter(|k| !self.uncalled(k.binding()))
                    .collect::<Vec<_>>();
                let cnts = Array::from_array(&self.ctx, cnts);

                let body = self.rec(body);

                if cnts.is_empty() {
                    body
                } else {
                    Gc::new(&self.ctx, Term::Letk(cnts, body))
                }
            }

            Term::Fix(funs_p, body) => {
                if funs_p.is_empty() {
                    return self.rec(body);
                }

                let funs = funs_p.iter().filter(|f| !self.uncalled(f.binding));

                let retc_mapped = funs.map(|f| (f.return_cont, f)).collect::<HashMap<_, _>>();

                let (sccs, rem_fs_ns): (Vec<HashSet<FuncRef>>, HashSet<LVarRef>) =
                    self.sccs.iter().fold(
                        (Vec::new(), retc_mapped.keys().cloned().collect()),
                        |(mut sccs, fs), scc| {
                            if scc.is_subset(&fs) {
                                let mapped_scc = scc
                                    .iter()
                                    .filter_map(|retc| retc_mapped.get(retc).copied())
                                    .cloned()
                                    .collect::<HashSet<_>>();

                                sccs.insert(0, mapped_scc);
                                let remaining_fs =
                                    fs.difference(scc).cloned().collect::<HashSet<_>>();
                                (sccs, remaining_fs)
                            } else {
                                (sccs, fs)
                            }
                        },
                    );

                let rem_fs_p: Vec<FuncRef> = rem_fs_ns
                    .iter()
                    .filter_map(|retc| retc_mapped.get(retc).copied())
                    .copied()
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
                        let j = new_state.get_j(func.binding);
                        let body = new_state.rec(func.body);

                        scc_k.push(Gc::new(
                            &new_state.ctx,
                            Cont::Local {
                                args: func.args,
                                variadic: func.variadic,
                                binding: j,
                                body,
                                name: func.name.clone(),
                                source: func.source,
                                reified: false,
                            },
                        ));
                    }
                    sccs_k.push(scc_k);
                }

                let rem_fs: Vec<FuncRef> = rem_fs_p
                    .iter()
                    .map(|func| {
                        let body = new_state.rec(func.body);
                        func.with_body(new_state.ctx, body)
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
                let ctx = new_state.ctx;

                let collapsed_sccs = sccs_k
                    .iter()
                    .map(|scc| scc.iter().cloned().collect::<HashSet<_>>())
                    .collect::<HashSet<_>>()
                    .minimize(|a, b| collapse(ctx, a, b));

                let new_body = new_state.rec(body);

                let rem_fs = Array::from_array(&new_state.ctx, rem_fs);

                let fix = Gc::new(&new_state.ctx, Term::Fix(rem_fs, new_body));

                collapsed_sccs.iter().rfold(fix, |acc, scc| {
                    match push_cont_definition(new_state.ctx, scc, acc) {
                        Some(good) => new_state.rewrite_all(good),
                        None => unreachable!("BUG: Contifiable component could not be merged"),
                    }
                })
            }

            Term::App(Atom::Local(f), retc, args, span) => match self.f_env.get(&f).copied() {
                Some(k) => Gc::new(&self.ctx, Term::Continue(k, args, span)),
                None => {
                    let retc = self.csub(retc);

                    Gc::new(&self.ctx, Term::App(Atom::Local(f), retc, args, span))
                }
            },

            Term::App(f, retc, args, src) => {
                let retc = self.csub(retc);

                Gc::new(&self.ctx, Term::App(f, retc, args, src))
            }

            Term::Continue(k, args, src) => {
                let k = self.csub(k);

                Gc::new(&self.ctx, Term::Continue(k, args, src))
            }

            Term::If(test, cons, alt, hint) => {
                let cons = self.csub(cons);
                let alt = self.csub(alt);

                Gc::new(&self.ctx, Term::If(test, cons, alt, hint))
            }

            Term::Throw(throw, src) => Gc::new(&self.ctx, Term::Throw(throw, src)),
        }
    }

    fn rewrite_all(&mut self, term: TermRef<'gc>) -> TermRef<'gc> {
        match *term {
            Term::Let(bind, exp, body) => {
                let body = self.rewrite_all(body);

                Gc::new(&self.ctx, Term::Let(bind, exp, body))
            }

            Term::Letk(cnts, body) => {
                let cnts = cnts
                    .iter()
                    .map(|k| {
                        let body = self.rewrite_all(k.body().unwrap());
                        k.with_body(self.ctx, body)
                    })
                    .collect::<Vec<_>>();
                let cnts = Array::from_array(&self.ctx, cnts);

                let body = self.rewrite_all(body);

                if cnts.is_empty() {
                    body
                } else {
                    Gc::new(&self.ctx, Term::Letk(cnts, body))
                }
            }

            Term::Fix(funs, body) => {
                if funs.is_empty() {
                    return self.rewrite_all(body);
                }

                let funs = funs
                    .iter()
                    .map(|f| {
                        let body = self.rewrite_all(f.body);
                        f.with_body(self.ctx, body)
                    })
                    .collect::<Vec<_>>();

                let funs = Array::from_array(&self.ctx, funs);

                let body = self.rewrite_all(body);

                Gc::new(&self.ctx, Term::Fix(funs, body))
            }

            Term::App(Atom::Local(f), retc, args, src) => match self.f_env.get(&f).copied() {
                Some(k) => Gc::new(&self.ctx, Term::Continue(k, args, src)),
                None => {
                    let retc = self.csub(retc);

                    Gc::new(&self.ctx, Term::App(Atom::Local(f), retc, args, src))
                }
            },

            Term::App(f, retc, args, src) => {
                let retc = self.csub(retc);

                Gc::new(&self.ctx, Term::App(f, retc, args, src))
            }

            Term::Continue(k, args, src) => {
                let k = self.csub(k);

                Gc::new(&self.ctx, Term::Continue(k, args, src))
            }

            Term::If(test, cons, alt, hint) => {
                let cons = self.csub(cons);
                let alt = self.csub(alt);

                Gc::new(&self.ctx, Term::If(test, cons, alt, hint))
            }

            Term::Throw(throw, src) => Gc::new(&self.ctx, Term::Throw(throw, src)),
        }
    }
}

pub fn contify<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> FuncRef<'gc> {
    let analyzer = Analyze::new();
    let mut cfgraph = analyzer.analyze(ctx, func);
    let new = cfgraph.rec(func.body);
    func.with_body(ctx, new)
}
