#![allow(dead_code)]
use std::{rc::Rc, sync::LazyLock};

use im::{HashMap, HashSet};
use lasso::Spur;

use crate::{
    ast::{Datum, P},
    cps::term::{Atom, Cont, Expression, Func, Term},
    il::primitives::PRIMITIVE_SET,
    il::term::LVar,
};

pub struct CPSOptimizer;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Count {
    applied: u32,
    as_value: u32,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BlockUsage {
    in_app: u32,
    in_set: u32,
    in_get: u32,
}

impl std::ops::Add<BlockUsage> for BlockUsage {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        BlockUsage {
            in_app: self.in_app + other.in_app,
            in_set: self.in_set + other.in_set,
            in_get: self.in_get + other.in_get,
        }
    }
}

struct State {
    census: HashMap<P<LVar>, Count>,
    block_usage: HashMap<P<LVar>, BlockUsage>,
    atom_subst: HashMap<Atom, Atom>,
    var_subst: HashMap<P<LVar>, P<LVar>>,

    e_inv_env: HashMap<(Spur, Vec<Atom>), Atom>,
    cenv: HashMap<P<LVar>, Rc<Cont>>,
    fenv: HashMap<P<LVar>, Rc<Func>>,
    benv: HashSet<P<LVar>>,
}

impl State {
    fn new(census: HashMap<P<LVar>, Count>) -> Self {
        State {
            census,
            block_usage: HashMap::new(),
            atom_subst: HashMap::new(),
            var_subst: HashMap::new(),
            e_inv_env: HashMap::new(),
            cenv: HashMap::new(),
            fenv: HashMap::new(),
            benv: HashSet::new(),
        }
    }

    fn is_dead(&self, var: &P<LVar>) -> bool {
        !self.census.contains_key(var)
    }

    fn applied_once(&self, var: &P<LVar>) -> bool {
        self.census
            .get(var)
            .copied()
            .map_or(false, |count| count.applied == 1 && count.as_value == 0)
    }

    fn can_remove_block(&self, b: &P<LVar>) -> bool {
        let block = self.block_usage.get(b).expect("block not found");
        block.in_app == 0 && block.in_get == 0
    }
    fn with_atom_subst(&mut self, from: Atom, to: Atom) -> &mut Self {
        self.atom_subst.insert(from, to);
        self
    }

    fn with_var_to_atom_subst(&mut self, from: P<LVar>, to: Atom) -> &mut Self {
        self.with_atom_subst(Atom::Local(from), to)
    }

    fn with_vars_to_atoms(&mut self, from: &[P<LVar>], to: &[Atom]) -> &mut Self {
        assert_eq!(from.len(), to.len());
        for (f, t) in from.iter().zip(to.iter()) {
            self.with_var_to_atom_subst(f.clone(), t.clone());
        }
        self
    }

    fn with_var_to_literal_subst(&mut self, from: P<LVar>, to: P<Datum>) -> &mut Self {
        self.with_atom_subst(Atom::Local(from), Atom::Constant(to))
    }

    fn with_c_subst(&mut self, from: P<LVar>, to: P<LVar>) -> &mut Self {
        self.var_subst.insert(from, to);
        self
    }

    fn with_exp(&mut self, atom: Atom, prim: Spur, args: Vec<Atom>) -> &mut Self {
        self.e_inv_env.insert((prim, args), atom);
        self
    }

    fn with_continuations(&mut self, cnts: Vec<Rc<Cont>>) -> &mut Self {
        for cont in cnts {
            let name = cont.binding.clone();
            self.cenv.insert(name, cont);
        }
        self
    }

    fn with_functions(&mut self, funcs: Vec<Rc<Func>>) -> &mut Self {
        for func in funcs {
            let name = func.binding.clone();
            self.fenv.insert(name, func);
        }
        self
    }

    fn with_block(&mut self, b: P<LVar>) -> &mut Self {
        self.benv.insert(b);
        self
    }

    fn substitue_atoms<'a>(&self, atoms: impl Iterator<Item = &'a Atom> + 'a) -> Vec<Atom> {
        atoms
            .map(move |atom| {
                if let Some(subst) = self.atom_subst.get(&atom) {
                    subst.clone()
                } else {
                    atom.clone()
                }
            })
            .collect()
    }
}

fn census(tree: &Rc<Term>) -> HashMap<P<LVar>, Count> {
    let mut census: HashMap<P<LVar>, Count> = HashMap::new();
    let mut rhs: HashMap<P<LVar>, Rc<Term>> = HashMap::new();

    fn inc_app_use_n(
        name: P<LVar>,
        census: &mut HashMap<P<LVar>, Count>,
        rhs: &mut HashMap<P<LVar>, Rc<Term>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();
        census.insert(
            name.clone(),
            Count {
                applied: curr_count.applied + 1,
                as_value: curr_count.as_value,
            },
        );
        if let Some(tree) = rhs.remove(&name) {
            add_to_census(&tree, census, rhs);
        }
    }

    fn inc_app_use_a(
        atom: &Atom,
        census: &mut HashMap<P<LVar>, Count>,
        rhs: &mut HashMap<P<LVar>, Rc<Term>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_app_use_n(name.clone(), census, rhs);
        }
    }

    fn inc_val_use_n(
        name: P<LVar>,
        census: &mut HashMap<P<LVar>, Count>,
        rhs: &mut HashMap<P<LVar>, Rc<Term>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();
        census.insert(
            name.clone(),
            Count {
                applied: curr_count.applied,
                as_value: curr_count.as_value + 1,
            },
        );
        if let Some(tree) = rhs.remove(&name) {
            add_to_census(&tree, census, rhs);
        }
    }

    fn inc_val_use_a(
        atom: &Atom,
        census: &mut HashMap<P<LVar>, Count>,
        rhs: &mut HashMap<P<LVar>, Rc<Term>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_val_use_n(name.clone(), census, rhs);
        }
    }

    fn add_to_census(
        tree: &Rc<Term>,
        census: &mut HashMap<P<LVar>, Count>,
        rhs: &mut HashMap<P<LVar>, Rc<Term>>,
    ) {
        match &**tree {
            crate::cps::term::Term::Let(_, expr, body) => {
                /*for arg in args {
                    inc_val_use_a(arg, census, rhs, );
                }*/
                match expr {
                    Expression::PrimCall(_, args, _) => {
                        for arg in args.iter() {
                            inc_val_use_a(arg, census, rhs);
                        }
                    }
                }
                add_to_census(body, census, rhs);
            }
            crate::cps::term::Term::Letk(conts, body) => {
                for cont in conts {
                    rhs.insert(cont.binding.clone(), cont.body.clone());
                }
                add_to_census(body, census, rhs);
            }
            crate::cps::term::Term::Fix(funcs, body) => {
                for fun in funcs {
                    rhs.insert(fun.binding.clone(), fun.body.clone());
                }
                add_to_census(body, census, rhs);
            }
            crate::cps::term::Term::Continue(cnt, args, _) => {
                inc_app_use_n(cnt.clone(), census, rhs);
                for arg in args.iter() {
                    inc_val_use_a(arg, census, rhs);
                }
            }
            crate::cps::term::Term::App(fun, ret_c, args, _) => {
                inc_app_use_a(fun, census, rhs);
                inc_val_use_n(ret_c.clone(), census, rhs);
                for arg in args {
                    inc_val_use_a(arg, census, rhs);
                }
            }
            crate::cps::term::Term::If(test, then_c, else_c, ..) => {
                inc_val_use_a(test, census, rhs);
                inc_val_use_n(then_c.clone(), census, rhs);
                inc_val_use_n(else_c.clone(), census, rhs);
            }
        }
    }

    add_to_census(tree, &mut census, &mut rhs);
    census
}

pub fn shrink(tree: &Rc<Term>) -> Rc<Term> {
    let census = census(tree);

    let mut state = State::new(census);

    shrink_tree(tree, &mut state)
}

fn shrink_tree(tree: &Rc<Term>, state: &mut State) -> Rc<Term> {
    match &**tree {
        Term::Let(binding, expr, body) => match expr {
            Expression::PrimCall(prim, args, span) => {
                let prim_def = PRIMITIVE_SET[prim];
                let args = state.substitue_atoms(args.iter());

                if !prim_def.is_impure() && state.is_dead(binding) {
                    return shrink_tree(body, state);
                } else {
                    return Rc::new(Term::Let(
                        binding.clone(),
                        Expression::PrimCall(*prim, args, *span),
                        shrink_tree(body, state),
                    ));
                }
            }
        },

        Term::Letk(conts, body) => {
            let conts = conts
                .iter()
                .filter_map(|cont| {
                    if state.is_dead(&cont.binding) {
                        return None;
                    }
                    let body = shrink_tree(&cont.body, state);
                    Some(Rc::new(Cont {
                        binding: cont.binding.clone(),
                        name: cont.name.clone(),
                        args: cont.args.clone(),
                        variadic: cont.variadic.clone(),
                        body,
                        span: cont.span,
                    }))
                })
                .collect::<Vec<_>>();

            if conts.is_empty() {
                return shrink_tree(body, state);
            } else {
                return Rc::new(Term::Letk(conts, shrink_tree(body, state)));
            }
        }

        Term::Fix(funcs, body) => {
            let funcs = funcs
                .iter()
                .cloned()
                .filter_map(|func| {
                    if state.is_dead(&func.binding) {
                        return None;
                    }
                    let body = shrink_tree(&func.body, state);
                    Some(Rc::new(Func {
                        binding: func.binding.clone(),
                        name: func.name.clone(),
                        return_cont: func.return_cont.clone(),
                        args: func.args.clone(),
                        variadic: func.variadic.clone(),
                        body,
                        span: func.span,
                        reified: func.reified
                    }))
                })
                .collect::<Vec<_>>();

            if funcs.is_empty() {
                return shrink_tree(body, state);
            } else {
                return Rc::new(Term::Fix(funcs, shrink_tree(body, state)));
            }
        }

        Term::Continue(k_prev, args_prev, span) => {
            let k = state
                .var_subst
                .get(k_prev)
                .cloned()
                .unwrap_or_else(|| k_prev.clone());
            let args = state.substitue_atoms(args_prev.iter());

            let Some(k) = state.cenv.get(&k).cloned() else {
                return Rc::new(Term::Continue(k, args, *span));
            };

            shrink_tree(&k.body, state.with_vars_to_atoms(&k.args, &args))
        }

        Term::App(fun, retc_prev, args_prev, span) => {
            let retc = state
                .var_subst
                .get(retc_prev)
                .cloned()
                .unwrap_or_else(|| retc_prev.clone());

            let args = state.substitue_atoms(args_prev.iter());
            let fun = state
                .atom_subst
                .get(fun)
                .cloned()
                .unwrap_or_else(|| fun.clone());

            match fun {
                Atom::Local(ref fun) if state.fenv.contains_key(fun) => {
                    let fun = state.fenv[fun].clone();
                    if fun.args.len() == args.len() {
                        return shrink_tree(
                            &fun.body,
                            state
                                .with_vars_to_atoms(&fun.args, &args)
                                .with_c_subst(fun.return_cont.clone(), retc),
                        );
                    }
                }

                _ => (),
            }

            return Rc::new(Term::App(fun, retc, args, *span));
        }

        Term::If(test, cons, alt) => {
            let test = state
                .atom_subst
                .get(test)
                .cloned()
                .unwrap_or_else(|| test.clone());
            let cons = state
                .var_subst
                .get(cons)
                .cloned()
                .unwrap_or_else(|| cons.clone());
            let alt = state
                .var_subst
                .get(alt)
                .cloned()
                .unwrap_or_else(|| alt.clone());

            Rc::new(Term::If(test, cons, alt))
        }
    }
}

pub fn rewrite(tree: Rc<Term>) -> Rc<Term> {
    let mut simplified_tree = tree.clone();
    for _ in 0..2 {
        simplified_tree = shrink(&simplified_tree);
    }

    let _max_size = size(&simplified_tree) * 3 / 2;

    simplified_tree
}

fn size(tree: &Rc<Term>) -> usize {
    match &**tree {
        Term::Let(_, _, body) => size(body) + 1,
        Term::Letk(conts, body) => conts.iter().map(|c| size(&c.body)).sum::<usize>() + size(body),
        Term::Fix(funs, body) => funs.iter().map(|f| size(&f.body)).sum::<usize>() + size(body),
        _ => 1,
    }
}

pub struct Primitives {
    pub make_cell_tc8: Spur,
    pub make_cell_tc16: Spur,
    pub cell_size: Spur,
    pub cell_ref: Spur,
    pub cell_set: Spur,
    pub scm_is_cell: Spur,
    pub scm_typ8: Spur,
    pub scm_typ16: Spur,

    pub eq: Spur,

    pub u64: Spur,
    pub unsafe_u64plus: Spur,
    pub unsafe_u64minus: Spur,
    pub unsafe_u64times: Spur,
    pub unsafe_u64div: Spur,
    pub unsafe_u64eq: Spur,
    pub unsafe_u64lt: Spur,
    pub unsafe_u64gt: Spur,
    pub unsafe_u64le: Spur,
    pub unsafe_u64ge: Spur,
    pub unsafe_u64lshift: Spur,
    pub unsafe_u64rshift: Spur,
    pub unsafe_u64and: Spur,
    pub unsafe_u64or: Spur,
    pub unsafe_u64xor: Spur,
}

impl Primitives {
    pub fn new() -> Self {
        macro_rules! prims {
            ($($id: ident -> $s: literal),*) => {
                Self {
                    $(
                        $id: $crate::ast::INTERNER.get_or_intern($s)
                    ),*
                }
            };
        }

        prims!(
            make_cell_tc8 -> "make-cell/typ8",
            make_cell_tc16 -> "make-cell/typ16",
            cell_size -> "cell-size",
            cell_ref -> "cell-ref",
            cell_set -> "cell-set!",
            scm_is_cell -> "scm-cell?",
            scm_typ8 -> "scm-typ8",
            scm_typ16 -> "scm-typ16",
            eq -> "eq?",
            u64 -> "u64",
            unsafe_u64plus -> "unsafe-u64+",
            unsafe_u64minus -> "unsafe-u64-",
            unsafe_u64times -> "unsafe-u64*",
            unsafe_u64div -> "unsafe-u64/",
            unsafe_u64eq -> "unsafe-u64=",
            unsafe_u64lt -> "unsafe-u64<",
            unsafe_u64gt -> "unsafe-u64>",
            unsafe_u64le -> "unsafe-u64<=",
            unsafe_u64ge -> "unsafe-u64>=",

            unsafe_u64and -> "unsafe-u64and",
            unsafe_u64or -> "unsafe-u64ior",
            unsafe_u64xor -> "unsafe-u64xor",
            unsafe_u64lshift -> "unsafe-u64lshift",
            unsafe_u64rshift -> "unsafe-u64rshift"
        )
    }
}


pub static PRIMITIVES: LazyLock<Primitives> = LazyLock::new(Primitives::new);


