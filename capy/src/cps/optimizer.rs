#![allow(dead_code, unused_variables)]
use crate::{
    cps::{
        Map, Substitute,
        fold::folding_table,
        term::{Atom, Atoms, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
    },
    expander::{
        core::{LVarRef, fresh_lvar},
        primitives::sym_cons,
    },
    runtime::{Context, value::Value},
    utils::fixedpoint,
};

use crate::rsgc::{Gc, alloc::array::Array, barrier, cell::Lock, traits::IterGc};
use std::{cell::Cell, collections::HashMap};

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Count {
    applied: u32,
    as_value: u32,
}

#[derive(Clone)]
struct State<'gc> {
    ctx: Context<'gc>,
    census: im::HashMap<LVarRef<'gc>, Count>,
    atom_subst: im::HashMap<Atom<'gc>, Atom<'gc>>,
    var_subst: Map<LVarRef<'gc>, LVarRef<'gc>>,

    e_inv_env: im::HashMap<(Value<'gc>, Atoms<'gc>), Atom<'gc>>,
    cenv: im::HashMap<LVarRef<'gc>, ContRef<'gc>>,
    fenv: im::HashMap<LVarRef<'gc>, FuncRef<'gc>>,

    aenv: im::HashSet<LVarRef<'gc>>,
}

impl<'gc> State<'gc> {
    fn new(ctx: Context<'gc>, census: im::HashMap<LVarRef<'gc>, Count>) -> Self {
        Self {
            ctx,
            census,
            atom_subst: im::HashMap::new(),
            var_subst: Map::default(),
            e_inv_env: im::HashMap::new(),
            cenv: im::HashMap::new(),
            fenv: im::HashMap::new(),
            aenv: im::HashSet::new(),
        }
    }

    fn is_dead(&self, var: LVarRef<'gc>) -> bool {
        !self.census.contains_key(&var)
    }

    fn applied_once(&self, var: LVarRef<'gc>) -> bool {
        self.census
            .get(&var)
            .map_or(false, |count| count.applied == 1 && count.as_value == 0)
    }

    fn with_atom_subst(mut self, from: Atom<'gc>, to: Atom<'gc>) -> Self {
        self.atom_subst.insert(from, to);
        self
    }

    fn with_var_to_atom_subst(self, from: LVarRef<'gc>, to: Atom<'gc>) -> Self {
        self.with_atom_subst(Atom::Local(from), to)
    }

    fn with_c_subst(mut self, from: LVarRef<'gc>, to: LVarRef<'gc>) -> Self {
        self.var_subst.insert(from, to);
        self
    }

    fn with_vars_to_atoms(self, from: &[LVarRef<'gc>], to: &[Atom<'gc>]) -> Self {
        assert_eq!(from.len(), to.len());
        /*for (f, t) in from.iter().zip(to.iter()) {
            self.with_var_to_atom_subst(*f, *t);
        }*/
        from.iter()
            .zip(to.iter())
            .fold(self, |s, (f, t)| s.with_var_to_atom_subst(*f, *t))
    }

    fn with_var_to_literal_subst(self, from: LVarRef<'gc>, to: Value<'gc>) -> Self {
        self.with_atom_subst(Atom::Local(from), Atom::Constant(to))
    }

    fn with_exp(mut self, atom: Atom<'gc>, prim: Value<'gc>, atoms: Atoms<'gc>) -> Self {
        self.e_inv_env.insert((prim, atoms), atom);
        self
    }

    fn with_continuations(self, conts: &[ContRef<'gc>]) -> Self {
        /*for cont in conts {
            match &**cont {
                Cont { binding, .. } => {
                    self.cenv.insert(*binding, cont.clone());
                }
            }
        }
        self*/
        conts.iter().fold(self, |mut s, cont| {
            s.cenv.insert(cont.binding(), *cont);
            s
        })
    }

    fn with_functions(self, funcs: &[FuncRef<'gc>]) -> Self {
        funcs.iter().fold(self, |mut s, func| {
            s.fenv.insert(func.binding, *func);
            s
        })
    }

    fn substitute_atoms<'a>(
        &'a self,
        atoms: impl Iterator<Item = Atom<'gc>> + 'a,
    ) -> impl Iterator<Item = Atom<'gc>> + 'a {
        atoms.map(|atom| self.atom_subst(atom))
    }

    fn atom_subst(&self, mut atom: Atom<'gc>) -> Atom<'gc> {
        while let Some(new_atom) = self.atom_subst.get(&atom).cloned() {
            atom = new_atom;
        }
        atom
    }

    fn var_subst(&self, mut var: LVarRef<'gc>) -> LVarRef<'gc> {
        while let Some(new_var) = self.var_subst.get(&var).copied() {
            var = new_var;
        }
        var
    }
}

fn census<'gc>(term: TermRef<'gc>) -> im::HashMap<LVarRef<'gc>, Count> {
    let mut census = im::HashMap::new();
    let mut rhs = im::HashMap::new();

    fn inc_app_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut im::HashMap<LVarRef<'gc>, Count>,
        rhs: &mut im::HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();
        census.insert(
            name,
            Count {
                applied: curr_count.applied + 1,
                as_value: curr_count.as_value,
            },
        );

        if let Some(tree) = rhs.remove(&name) {
            add_to_census(tree, census, rhs);
        }
    }

    fn inc_app_use_a<'gc>(
        atom: Atom<'gc>,
        census: &mut im::HashMap<LVarRef<'gc>, Count>,
        rhs: &mut im::HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_app_use_n(name, census, rhs);
        }
    }

    fn inc_val_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut im::HashMap<LVarRef<'gc>, Count>,
        rhs: &mut im::HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();

        census.insert(
            name,
            Count {
                applied: curr_count.applied,
                as_value: curr_count.as_value + 1,
            },
        );

        if let Some(tree) = rhs.remove(&name) {
            add_to_census(tree, census, rhs);
        }
    }

    fn inc_val_use_a<'gc>(
        atom: Atom<'gc>,
        census: &mut im::HashMap<LVarRef<'gc>, Count>,
        rhs: &mut im::HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_val_use_n(name, census, rhs);
        }
    }

    fn add_to_census<'gc>(
        term: TermRef<'gc>,
        census: &mut im::HashMap<LVarRef<'gc>, Count>,
        rhs: &mut im::HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        match *term {
            Term::Let(_, expr, body) => {
                match expr {
                    Expression::PrimCall(_, args, h, _) => {
                        for arg in args.iter() {
                            inc_val_use_a(*arg, census, rhs);
                        }

                        inc_val_use_n(h, census, rhs);
                    }
                }

                add_to_census(body, census, rhs);
            }

            Term::Letk(ks, body) => {
                for k in ks.iter() {
                    rhs.insert(k.binding(), k.body());
                }

                add_to_census(body, census, rhs);
            }

            Term::Fix(funcs, body) => {
                for fun in funcs.iter() {
                    rhs.insert(fun.binding, fun.body());
                }

                add_to_census(body, census, rhs);
            }

            Term::App(fun, ret_c, err_c, args, _) => {
                inc_app_use_a(fun, census, rhs);
                inc_val_use_n(ret_c, census, rhs);
                inc_val_use_n(err_c, census, rhs);

                for arg in args.iter() {
                    inc_val_use_a(*arg, census, rhs);
                }
            }

            Term::Continue(k, args, _) => {
                inc_app_use_n(k, census, rhs);
                for arg in args.iter() {
                    inc_val_use_a(*arg, census, rhs);
                }
            }

            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                ..
            } => {
                inc_val_use_a(test, census, rhs);
                inc_val_use_n(consequent, census, rhs);
                inc_val_use_n(alternative, census, rhs);
                if let Some(args) = consequent_args {
                    for arg in args.iter() {
                        inc_val_use_a(*arg, census, rhs);
                    }
                }

                if let Some(args) = alternative_args {
                    for arg in args.iter() {
                        inc_val_use_a(*arg, census, rhs);
                    }
                }
            }
        }
    }

    add_to_census(term, &mut census, &mut rhs);

    census
}

fn shrink_tree<'gc>(term: TermRef<'gc>, state: State<'gc>) -> TermRef<'gc> {
    match *term {
        Term::Let(binding, expr, prev_body) => match expr {
            Expression::PrimCall(prim, prev_args, prev_h, source) => {
                let args = state
                    .substitute_atoms(prev_args.iter().copied())
                    .collect::<Vec<_>>();
                let h = state.var_subst(prev_h);

                if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
                    if let Some(atom) = folding_table(state.ctx).try_fold(state.ctx, prim, &args) {
                        let state =
                            state.with_atom_subst(Atom::Local(binding), Atom::Constant(atom));
                        return shrink_tree(prev_body, state);
                    }
                }
                let ctx = state.ctx;
                let atoms = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    prev_args
                } else {
                    Array::from_slice(*state.ctx, args)
                };

                let body = shrink_tree(prev_body, state);

                if Gc::ptr_eq(body, prev_body) && Gc::ptr_eq(atoms, prev_args) && h == prev_h {
                    return term;
                }

                Gc::new(
                    *ctx,
                    Term::Let(binding, Expression::PrimCall(prim, atoms, h, source), body),
                )
            }
        },

        Term::Letk(prev_conts, prev_body) => {
            let (inlined, not_inlined) = prev_conts
                .iter()
                .filter_map(|cont| {
                    if state.is_dead(cont.binding()) {
                        return None;
                    }
                    let body = shrink_tree(cont.body(), state.clone());

                    let newh = state.var_subst(cont.handler.get());

                    let wcont = Gc::write(*state.ctx, *cont);
                    barrier::field!(wcont, Cont, handler).unlock().set(newh);

                    Some(cont.with_body(state.ctx, body))
                })
                .collect::<Vec<_>>()
                .into_iter()
                .partition::<Vec<_>, _>(|&cont| {
                    !cont.noinline && state.applied_once(cont.binding())
                });

            let state = state.with_continuations(&inlined);
            if not_inlined.is_empty() {
                return shrink_tree(prev_body, state);
            }

            let ctx = state.ctx;

            let conts = if inlined.is_empty()
                && not_inlined
                    .iter()
                    .zip(prev_conts.iter())
                    .all(|(a, b)| Gc::ptr_eq(*a, *b))
            {
                prev_conts
            } else {
                Array::from_slice(*state.ctx, not_inlined)
            };
            let body = shrink_tree(prev_body, state);
            if Gc::ptr_eq(body, prev_body) && Gc::ptr_eq(conts, prev_conts) {
                return term;
            }
            Gc::new(*ctx, Term::Letk(conts, body))
        }

        Term::Fix(prev_funcs, prev_body) => {
            let funcs = prev_funcs
                .iter()
                .copied()
                .filter_map(|func| {
                    if state.is_dead(func.binding) {
                        return None;
                    }

                    let body = shrink_tree(func.body(), state.clone());
                    Some(func.with_body(state.ctx, body))
                })
                .collect::<Vec<_>>();

            if funcs.is_empty() {
                return shrink_tree(prev_body, state);
            }
            let ctx = state.ctx;
            let funcs = if funcs
                .iter()
                .zip(prev_funcs.iter())
                .all(|(a, b)| Gc::ptr_eq(*a, *b))
            {
                prev_funcs
            } else {
                Array::from_slice(*state.ctx, funcs)
            };
            let body = shrink_tree(prev_body, state);
            if Gc::ptr_eq(body, prev_body) && Gc::ptr_eq(funcs, prev_funcs) {
                return term;
            }
            Gc::new(*ctx, Term::Fix(funcs, body))
        }

        Term::Continue(k_prev, args_prev, src) => {
            let k = state.var_subst(k_prev);
            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();

            let args = if args.iter().zip(args_prev.iter()).all(|(a, b)| a == b) {
                args_prev
            } else {
                Array::from_slice(*state.ctx, args)
            };
            if let Some(k) = state.cenv.get(&k).copied()
                && !k.noinline
                && k.arity_matches(args.len())
            {
                if let Some(variadic) = k.variadic() {
                    let fixed_args = &args[0..k.args.len()];
                    if variadic.is_referenced() {
                        materialize_list(
                            state,
                            k.handler.get(),
                            &args[k.args.len()..],
                            Box::new(move |state, rest| {
                                let mut state = state;
                                for (arg, &var) in fixed_args
                                    .iter()
                                    .copied()
                                    .chain(std::iter::once(rest))
                                    .zip(k.args().iter().chain(std::iter::once(&variadic)))
                                {
                                    state = state.with_var_to_atom_subst(var, arg);
                                }

                                shrink_tree(k.body(), state)
                            }),
                        )
                    } else {
                        // if variadic is never referenced we don't need to materialize the list
                        let mut state = state.with_vars_to_atoms(&k.args(), fixed_args);
                        state =
                            state.with_var_to_atom_subst(variadic, Atom::Constant(Value::null()));
                        shrink_tree(k.body(), state)
                    }
                } else {
                    let state = state.with_vars_to_atoms(&k.args(), &args);
                    shrink_tree(k.body(), state)
                }
            } else {
                if Gc::ptr_eq(k, k_prev) && Gc::ptr_eq(args, args_prev) {
                    return term;
                }
                Gc::new(*state.ctx, Term::Continue(k, args, src))
            }
        }

        Term::App(fun_prev, retc_prev, rete_prev, args_prev, span) => {
            let retc = state.var_subst(retc_prev);
            let rete = state.var_subst(rete_prev);

            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();
            let args = if args.iter().zip(args_prev.iter()).all(|(a, b)| a == b) {
                args_prev
            } else {
                Array::from_slice(*state.ctx, args)
            };

            let fun = state.atom_subst(fun_prev);

            match fun {
                Atom::Local(fun) if state.fenv.contains_key(&fun) => {
                    let fun = state.fenv[&fun];
                    if fun.arity_matches(args.len()) {
                        let mut state = state
                            .with_c_subst(fun.return_cont, retc)
                            .with_c_subst(fun.handler_cont, rete);
                        if let Some(variadic) = fun.variadic {
                            return materialize_list(
                                state,
                                rete,
                                &args[fun.args.len()..],
                                Box::new(|state, rest| {
                                    let mut state = state;
                                    for (arg, &var) in args
                                        .iter()
                                        .copied()
                                        .take(fun.args.len())
                                        .chain(std::iter::once(rest))
                                        .zip(fun.args.iter().chain(std::iter::once(&variadic)))
                                    {
                                        state = state.with_var_to_atom_subst(var, arg);
                                    }

                                    shrink_tree(fun.body(), state)
                                }),
                            );
                        } else {
                            state = state.with_vars_to_atoms(&fun.args, &args);
                            return shrink_tree(fun.body(), state);
                        }
                    }
                }

                _ => {}
            }

            if Gc::ptr_eq(retc, retc_prev)
                && Gc::ptr_eq(rete, rete_prev)
                && Gc::ptr_eq(args, args_prev)
                && fun == fun_prev
            {
                return term;
            }

            return Gc::new(*state.ctx, Term::App(fun, retc, rete, args, span));
        }

        Term::If {
            test: test_prev,
            consequent: consequent_prev,
            consequent_args: consequent_args_prev,
            alternative: alternative_prev,
            alternative_args: alternative_args_prev,
            hints,
        } => {
            let test = state.atom_subst(test_prev);

            let consequent = state.var_subst(consequent_prev);
            let alternative = state.var_subst(alternative_prev);
            let consequent_args = consequent_args_prev.map(|prev_args| {
                let args = prev_args
                    .iter()
                    .map(|&arg| state.atom_subst(arg))
                    .collect::<Vec<_>>();
                if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    prev_args
                } else {
                    Array::from_slice(*state.ctx, args)
                }
            });
            let alternative_args = alternative_args_prev.map(|prev_args| {
                let args = prev_args
                    .iter()
                    .map(|&arg| state.atom_subst(arg))
                    .collect::<Vec<_>>();
                if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                    prev_args
                } else {
                    Array::from_slice(*state.ctx, args)
                }
            });

            if let Atom::Constant(test) = test {
                let (k, args) = if test != Value::new(false) {
                    (consequent, consequent_args)
                } else {
                    (alternative, alternative_args)
                };

                return Gc::new(
                    *state.ctx,
                    Term::Continue(
                        k,
                        args.unwrap_or_else(|| Array::from_slice(*state.ctx, [])),
                        Value::new(false),
                    ),
                );
            }

            if Gc::ptr_eq(consequent, consequent_prev)
                && Gc::ptr_eq(alternative, alternative_prev)
                && (consequent_args.is_some()
                    && consequent_args
                        .iter()
                        .zip(consequent_args_prev.iter())
                        .all(|(a, b)| a.iter().zip(b.iter()).all(|(x, y)| x == y)))
                && (alternative_args.is_some()
                    && alternative_args
                        .iter()
                        .zip(alternative_args_prev.iter())
                        .all(|(a, b)| a.iter().zip(b.iter()).all(|(x, y)| x == y)))
                && test == test_prev
            {
                return term;
            }

            Gc::new(
                *state.ctx,
                Term::If {
                    test,
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    hints,
                },
            )
        }
    }
}

pub fn shrink<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let census = census(term);
    let state = State::new(ctx, census);
    term.count_refs();
    shrink_tree(term, state)
}

pub fn rewrite<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let simplified_tree = fixedpoint(term, Some(3))(|term| shrink(ctx, *term));

    let max_size = size(simplified_tree) * 3 / 2;

    fixedpoint(simplified_tree, Some(2))(|term| inline(ctx, *term, max_size))
}

pub fn rewrite_func<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> FuncRef<'gc> {
    let body = rewrite(ctx, func.body());
    func.with_body(ctx, body)
}

const FIBONACCI: &[usize] = &[1, 2, 3, 5, 8, 13];

//const LOOP_UNROLL: &[usize] = &[1, 2, 4, 5, 6, 7];

const LOOP_UNROLL: &[usize] = &[1, 1, 1, 1, 1, 1];

fn copy_t<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> TermRef<'gc> {
    match *term {
        Term::Let(binding, expr, body) => {
            let binding1 = binding.copy(ctx);
            subv.insert(Atom::Local(binding), Atom::Local(binding1));
            let body1 = copy_t(ctx, body, subv, subc);
            let expr = match expr {
                Expression::PrimCall(prim, args, h, source) => {
                    let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
                    let atoms = Array::from_slice(*ctx, args);

                    let h = subc.subst(h);

                    Expression::PrimCall(prim, atoms, h, source)
                }
            };

            Gc::new(*ctx, Term::Let(binding1, expr, body1))
        }

        Term::Letk(conts, body) => {
            let names = conts.iter().map(|c| c.binding()).collect::<Vec<_>>();
            let names1 = conts
                .iter()
                .map(|c| c.binding().copy(ctx))
                .collect::<Vec<_>>();
            let mut subc1 = subc;

            for (old, new) in names.iter().zip(names1.iter()) {
                subc1.insert(*old, *new);
            }

            let conts = conts
                .iter()
                .map(|cont| copy_c(ctx, *cont, subv, &mut subc1))
                .collect::<Vec<_>>();

            let conts = Array::from_slice(*ctx, conts);

            let body = copy_t(ctx, body, subv, &mut subc1);

            Gc::new(*ctx, Term::Letk(conts, body))
        }

        Term::Fix(funcs, body) => {
            let names = funcs.iter().map(|f| f.binding).collect::<Vec<_>>();
            let names1 = funcs
                .iter()
                .map(|f| f.binding.copy(ctx))
                .collect::<Vec<_>>();

            let mut subv1 = subv.clone();

            for (old, new) in names.iter().zip(names1.iter()) {
                subv1.insert(Atom::Local(*old), Atom::Local(*new));
            }

            let funcs = funcs
                .iter()
                .map(|func| copy_f(ctx, *func, &mut subv1, subc))
                .collect::<Vec<_>>();
            let funcs = Array::from_slice(*ctx, funcs);

            let body = copy_t(ctx, body, &mut subv1, subc);

            Gc::new(*ctx, Term::Fix(funcs, body))
        }

        Term::Continue(cont, args, src) => {
            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
            let args = Array::from_slice(*ctx, args);

            let cont = subc.subst(cont);

            Gc::new(*ctx, Term::Continue(cont, args, src))
        }

        Term::App(fun, retc, rete, args, src) => {
            let fun = subv.subst(fun);

            let retc = subc.subst(retc);
            let rete = subc.subst(rete);

            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
            let args = Array::from_slice(*ctx, args);

            Gc::new(*ctx, Term::App(fun, retc, rete, args, src))
        }

        Term::If {
            test,
            consequent,
            consequent_args,
            alternative,
            alternative_args,
            hints,
        } => {
            let test = subv.subst(test);
            let consequent = subc.subst(consequent);
            let alternative = subc.subst(alternative);
            let consequent_args =
                consequent_args.map(|args| args.iter().map(|&a| subv.subst(a)).collect_gc(*ctx));
            let alternative_args =
                alternative_args.map(|args| args.iter().map(|&a| subv.subst(a)).collect_gc(*ctx));

            Gc::new(
                *ctx,
                Term::If {
                    test,
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    hints,
                },
            )
        }
    }
}

fn copy_c<'gc>(
    ctx: Context<'gc>,
    cont: ContRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> ContRef<'gc> {
    let args1 = cont.args().iter().map(|a| a.copy(ctx)).collect::<Vec<_>>();
    let var1 = cont.variadic().map(|v| v.copy(ctx));

    let subv1 = subv;
    for (new, prev) in args1
        .iter()
        .chain(var1.iter())
        .zip(cont.args().iter().chain(cont.variadic().iter()))
    {
        subv1.insert(Atom::Local(*prev), Atom::Local(*new));
    }

    let binding = subc.subst(cont.binding());
    let args1 = Array::from_slice(*ctx, args1);
    let body = copy_t(ctx, cont.body(), subv1, subc);
    let handler = subc.subst(cont.handler.get());
    Gc::new(
        *ctx,
        Cont {
            name: cont.name(),
            binding,

            body: Lock::new(body),
            args: args1,
            variadic: var1,
            source: cont.source(),
            noinline: cont.noinline,
            free_vars: Lock::new(cont.free_vars.get()),
            reified: Cell::new(cont.reified.get()),
            handler: Lock::new(handler),
            cold: cont.cold,
            meta: cont.meta,
        },
    )
}

fn copy_f<'gc>(
    ctx: Context<'gc>,
    fun: FuncRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> FuncRef<'gc> {
    let retc1 = fun.return_cont.copy(ctx);
    let rete1 = fun.handler_cont.copy(ctx);

    let subc1 = subc;
    subc1.insert(fun.return_cont, retc1);
    subc1.insert(fun.handler_cont, rete1);

    let args1 = fun.args.iter().map(|a| a.copy(ctx)).collect::<Vec<_>>();
    let var1 = fun.variadic.map(|v| v.copy(ctx));
    let subv1 = subv;
    for (new, prev) in args1
        .iter()
        .chain(var1.iter())
        .zip(fun.args.iter().chain(fun.variadic.iter()))
    {
        subv1.insert(Atom::Local(*prev), Atom::Local(*new));
    }

    let Atom::Local(binding) = subv1[&Atom::Local(fun.binding)] else {
        panic!("BUG: expected local binding for function");
    };

    let body = copy_t(ctx, fun.body(), subv1, subc1);

    Gc::new(
        *ctx,
        Func {
            meta: fun.meta,
            return_cont: retc1,
            handler_cont: rete1,

            name: fun.name,
            binding,

            args: Array::from_slice(*ctx, args1),
            variadic: var1,
            body: Lock::new(body),
            source: fun.source,
            free_vars: Lock::new(fun.free_vars.get()),
        },
    )
}

fn inline_t<'gc>(state: State<'gc>, term: TermRef<'gc>, cnt_limit: usize) -> TermRef<'gc> {
    let loop_limit = LOOP_UNROLL[cnt_limit];
    let fun_limit = FIBONACCI[cnt_limit];

    match *term {
        Term::Let(name, prev_exp, prev_body) => {
            let exp = match prev_exp {
                Expression::PrimCall(prim, prev_args, handler, src) => {
                    let args = prev_args
                        .iter()
                        .map(|&arg| state.atom_subst(arg))
                        .collect::<Vec<_>>();
                    let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                        prev_args
                    } else {
                        Array::from_slice(*state.ctx, args)
                    };

                    let handler = state.var_subst(handler);
                    Expression::PrimCall(prim, args, handler, src)
                }
            };
            let ctx = state.ctx;
            let body = inline_t(state, prev_body, cnt_limit);
            if exp == prev_exp && Gc::ptr_eq(body, prev_body) {
                return term;
            }
            Gc::new(*ctx, Term::Let(name, exp, body))
        }

        Term::Letk(conts, body) => {
            let conts = conts.iter().copied().map(|cnt| {
                let orig = cnt.body();
                let h = state.var_subst(cnt.handler.get());
                let wcont = Gc::write(*state.ctx, cnt);
                barrier::field!(wcont, Cont, handler).unlock().set(h);

                let body = inline_t(state.clone(), cnt.body(), cnt_limit);
                let newk = cnt.with_body(state.ctx, body);

                let my_size = size(orig);
                let dont = cnt.noinline
                    || my_size > cnt_limit
                    || (my_size > loop_limit && census(body).contains_key(&cnt.binding));
                (newk, if dont { None } else { Some(newk) })
            });

            let (i_ks, to_inline): (Vec<_>, Vec<_>) = conts.unzip();
            let ctx = state.ctx;
            let s =
                state.with_continuations(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, body, cnt_limit);
            let i_ks = Array::from_slice(*ctx, i_ks);
            Gc::new(*ctx, Term::Letk(i_ks, body))
        }

        Term::Fix(funcs, body) => {
            let funcs = funcs
                .iter()
                .copied()
                .map(|func| {
                    let nbody = inline_t(state.clone(), func.body(), cnt_limit);
                    let newf = func.with_body(state.ctx, nbody);

                    let my_size = size(func.body());
                    let dont = my_size > fun_limit
                        || (my_size > loop_limit && census(nbody).contains_key(&func.binding));

                    (newf, if dont { None } else { Some(newf) })
                })
                .collect::<Vec<_>>();

            let (i_fs, to_inline): (Vec<_>, Vec<_>) = funcs.into_iter().unzip();
            let ctx = state.ctx;
            let s = state.with_functions(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, body, cnt_limit);
            let ifs = Array::from_slice(*ctx, i_fs);
            Gc::new(*ctx, Term::Fix(ifs, body))
        }

        Term::Continue(prev_cnt, prev_args, src) => {
            let args = state
                .substitute_atoms(prev_args.iter().copied())
                .collect::<Vec<_>>();
            let args = if prev_args.iter().zip(args.iter()).all(|(a, b)| a == b) {
                prev_args
            } else {
                Array::from_slice(*state.ctx, args)
            };
            let cnt = state.var_subst(prev_cnt);

            if let Atom::Local(cnt) = state.atom_subst(Atom::Local(cnt)) {
                if let Some(new_cnt) = state.cenv.get(&cnt).cloned() {
                    if new_cnt.args().len() == args.len() && new_cnt.variadic().is_none() {
                        let mut subst = new_cnt
                            .args()
                            .iter()
                            .zip(args.iter())
                            .map(|(arg, val)| (Atom::Local(*arg), *val))
                            .collect::<HashMap<_, _>>();

                        return copy_t(state.ctx, new_cnt.body(), &mut subst, &mut HashMap::new());
                    }
                }
            }

            Gc::new(*state.ctx, Term::Continue(cnt, args, src))
        }

        Term::App(fun, ret_cp, ret_ep, args, src) => {
            let retc = state.var_subst(ret_cp);
            let rete = state.var_subst(ret_ep);

            let args = state
                .substitute_atoms(args.iter().copied())
                .collect::<Vec<_>>();
            let args = Array::from_slice(*state.ctx, args);

            if let Atom::Local(new_fun) = state.atom_subst(fun) {
                if let Some(func) = state.fenv.get(&new_fun).copied() {
                    if func.args.len() == args.len() && func.variadic.is_none() {
                        let mut subv = func
                            .args
                            .iter()
                            .zip(args.iter())
                            .map(|(arg, val)| (Atom::Local(*arg), *val))
                            .collect::<HashMap<_, _>>();

                        let mut subc = HashMap::new();
                        subc.insert(func.return_cont, retc);
                        subc.insert(func.handler_cont, rete);

                        return copy_t(state.ctx, func.body(), &mut subv, &mut subc);
                    }
                }
            }

            Gc::new(*state.ctx, Term::App(fun, retc, rete, args, src))
        }

        Term::If {
            test,
            consequent,
            consequent_args,
            alternative,
            alternative_args,
            hints,
        } => {
            let test = state.atom_subst(test);
            let consequent = state.var_subst(consequent);
            let alternative = state.var_subst(alternative);
            let consequent_args = consequent_args.map(|args| {
                args.iter()
                    .map(|&a| state.atom_subst(a))
                    .collect_gc(*state.ctx)
            });
            let alternative_args = alternative_args.map(|args| {
                args.iter()
                    .map(|&a| state.atom_subst(a))
                    .collect_gc(*state.ctx)
            });
            Gc::new(
                *state.ctx,
                Term::If {
                    test,
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    hints,
                },
            )
        }
    }
}

pub fn inline<'gc>(ctx: Context<'gc>, mut term: TermRef<'gc>, max_size: usize) -> TermRef<'gc> {
    for i in 0..FIBONACCI.len() {
        if size(term) > max_size {
            return term;
        }

        let state = State::new(ctx, census(term));

        term = fixedpoint(inline_t(state, term, i), Some(1))(|term| shrink(ctx, *term));
    }

    term
}

/// Given a term, returns the size of it.
pub fn size<'gc>(term: TermRef<'gc>) -> usize {
    match *term {
        Term::Let(_, _, body) => 1 + size(body),
        Term::Letk(conts, body) => conts.iter().map(|c| size(c.body())).sum::<usize>() + size(body),

        Term::Fix(funcs, body) => funcs.iter().map(|f| size(f.body())).sum::<usize>() + size(body),

        _ => 1,
    }
}

/// Given a list of elements, emit a CPS list construction.
fn materialize_list<'gc, 'a>(
    state: State<'gc>,
    h: LVarRef<'gc>,
    elements: &[Atom<'gc>],
    fk: Box<dyn FnOnce(State<'gc>, Atom<'gc>) -> TermRef<'gc> + 'a>,
) -> TermRef<'gc> {
    // empty list: just return null directly
    if elements.is_empty() {
        return fk(state, Atom::Constant(Value::null()));
    }

    let head = elements[0];
    materialize_list(
        state,
        h,
        &elements[1..],
        Box::new(move |state, tail| {
            let var = fresh_lvar(state.ctx, state.ctx.intern("&ls"));
            let ctx = state.ctx;
            let f = fk(state, Atom::Local(var));
            Gc::new(
                *ctx,
                Term::Let(
                    var,
                    Expression::PrimCall(
                        sym_cons(ctx).into(),
                        Array::from_slice(*ctx, [head, tail]),
                        h,
                        Value::new(false),
                    ),
                    f,
                ),
            )
        }),
    )
}
