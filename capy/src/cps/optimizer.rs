//! Tree rewriting optimization for CPS terms.
//!
//! Main entrypoints: `shrink` and `rewrite`.
//!
//! Included optimizations:
//! - Constant folding
//! - Beta reduction
//! - Rest argument optimization: if rest arguments do not escape a function they belong to,
//! we can make access to it more efficient by avoiding to allocate a list for them.

#![allow(dead_code, unused_variables)]

use crate::{
    cps::{
        HMap as HashMap, Map, Set, Substitute,
        fold::folding_table,
        term::{Atom, Atoms, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef},
    },
    expander::{
        core::{LVarRef, fresh_lvar},
        primitives::sym_cons,
    },
    runtime::{Context, value::Value},
    utils::pass_profile::ProfileScope,
};

use crate::rsgc::{Gc, alloc::array::Array, cell::Lock, traits::IterGc};
use std::{cell::Cell, sync::LazyLock};

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Count {
    applied: u32,
    as_value: u32,
    single_app_arity: Option<usize>,
}

#[derive(Clone, Copy)]
struct RewriteResult<'gc> {
    term: TermRef<'gc>,
    changed: bool,
    size: usize,
    mentions_tracked_binding: bool,
}

#[derive(Clone)]
struct State<'gc> {
    ctx: Context<'gc>,
    census: Map<LVarRef<'gc>, Count>,
    atom_subst: Map<Atom<'gc>, Atom<'gc>>,
    var_subst: Map<LVarRef<'gc>, LVarRef<'gc>>,

    e_inv_env: Map<(Value<'gc>, Atoms<'gc>), Atom<'gc>>, // expression-inverse environment: maps (primop, args) to previously computed results
    cenv: Map<LVarRef<'gc>, ContRef<'gc>>, // continuation environment: maps variables to their continuation definitions
    fenv: Map<LVarRef<'gc>, FuncRef<'gc>>, // function environment: maps variables to their function definitions

    aenv: Set<LVarRef<'gc>>, // alias environment: set of variables known to be aliased
}

impl<'gc> State<'gc> {
    fn new(ctx: Context<'gc>, census: Map<LVarRef<'gc>, Count>) -> Self {
        Self {
            ctx,
            census,
            atom_subst: Map::default(),
            var_subst: Map::default(),
            e_inv_env: Map::default(),
            cenv: Map::default(),
            fenv: Map::default(),
            aenv: Set::default(),
        }
    }

    fn is_dead(&self, var: LVarRef<'gc>) -> bool {
        self.census
            .get(&var)
            .map_or(true, |var| var.applied == 0 && var.as_value == 0)
    }

    fn applied_once_with_arity(&self, var: LVarRef<'gc>) -> Option<usize> {
        self.census.get(&var).and_then(|count| {
            (count.applied == 1 && count.as_value == 0)
                .then_some(count.single_app_arity)
                .flatten()
        })
    }

    fn is_referenced(&self, var: LVarRef<'gc>) -> bool {
        self.census
            .get(&var)
            .is_some_and(|count| count.applied > 0 || count.as_value > 0)
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

    /// Resolve a variable used in continuation position through both
    /// `var_subst` and `atom_subst`, since continuation parameters
    /// inlined via beta-reduction only appear in `atom_subst`.
    fn cont_subst(&self, var: LVarRef<'gc>) -> LVarRef<'gc> {
        let var = self.var_subst(var);
        match self.atom_subst(Atom::Local(var)) {
            Atom::Local(v) => v,
            _ => var,
        }
    }
}

fn census<'gc>(term: TermRef<'gc>) -> Map<LVarRef<'gc>, Count> {
    let mut census = HashMap::default();
    let mut rhs = HashMap::default();

    fn inc_app_use_n<'gc>(
        name: LVarRef<'gc>,
        arity: usize,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();
        census.insert(
            name,
            Count {
                applied: curr_count.applied + 1,
                as_value: curr_count.as_value,
                single_app_arity: if curr_count.applied == 0 {
                    Some(arity)
                } else {
                    None
                },
            },
        );

        if let Some(tree) = rhs.remove(&name) {
            add_to_census(tree, census, rhs);
        }
    }

    fn inc_app_use_a<'gc>(
        atom: Atom<'gc>,
        arity: usize,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_app_use_n(name, arity, census, rhs);
        }
    }

    fn inc_val_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        let curr_count = census.get(&name).copied().unwrap_or_default();

        census.insert(
            name,
            Count {
                applied: curr_count.applied,
                as_value: curr_count.as_value + 1,
                single_app_arity: curr_count.single_app_arity,
            },
        );

        if let Some(tree) = rhs.remove(&name) {
            add_to_census(tree, census, rhs);
        }
    }

    fn inc_val_use_a<'gc>(
        atom: Atom<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_val_use_n(name, census, rhs);
        }
    }

    fn add_to_census<'gc>(
        term: TermRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        match *term {
            Term::Let(_, expr, body) => {
                match expr {
                    Expression::PrimCall(_, args, _) => {
                        for arg in args.iter() {
                            inc_val_use_a(*arg, census, rhs);
                        }
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

            Term::App(fun, ret_c, args, _) => {
                inc_app_use_a(fun, args.len(), census, rhs);
                inc_val_use_n(ret_c, census, rhs);

                for arg in args.iter() {
                    inc_val_use_a(*arg, census, rhs);
                }
            }

            Term::Continue(k, args, _) => {
                inc_app_use_n(k, args.len(), census, rhs);
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

    census.into_iter().collect()
}

fn shrink_tree<'gc>(term: TermRef<'gc>, state: State<'gc>) -> (TermRef<'gc>, bool) {
    stacker::maybe_grow(4 * 1024 * 1024, 1 * 1024 * 1024, || match *term {
        Term::Let(binding, expr, prev_body) => match expr {
            Expression::PrimCall(prim, prev_args, source) => {
                /*if state.is_dead(binding)
                    && SIDE_EFFECT_FREE_OPS.contains(&*prim.downcast::<Symbol>().as_str())
                {
                    return shrink_tree(prev_body, state);
                }*/

                let args = state
                    .substitute_atoms(prev_args.iter().copied())
                    .collect::<Vec<_>>();

                if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
                    if let Some(atom) = folding_table(state.ctx).try_fold(state.ctx, prim, &args) {
                        let state =
                            state.with_atom_subst(Atom::Local(binding), Atom::Constant(atom));
                        let (body, _) = shrink_tree(prev_body, state);
                        return (body, true);
                    }
                }
                let ctx = state.ctx;
                let args_changed = !args.iter().zip(prev_args.iter()).all(|(a, b)| a == b);
                let atoms = if !args_changed {
                    prev_args
                } else {
                    Array::from_slice(*state.ctx, args)
                };

                let (body, body_changed) = shrink_tree(prev_body, state);

                if !body_changed && !args_changed {
                    return (term, false);
                }

                (
                    Gc::new(
                        *ctx,
                        Term::Let(binding, Expression::PrimCall(prim, atoms, source), body),
                    ),
                    true,
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
                    let (body, body_changed) = shrink_tree(cont.body(), state.clone());
                    Some(if body_changed {
                        cont.with_body(state.ctx, body)
                    } else {
                        *cont
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .partition::<Vec<_>, _>(|&cont| {
                    !cont.noinline
                        && state
                            .applied_once_with_arity(cont.binding())
                            .is_some_and(|arity| cont.arity_matches(arity))
                });

            let state = state.with_continuations(&inlined);
            let ctx = state.ctx;
            let (body, body_changed) = shrink_tree(prev_body, state);
            let conts_changed = prev_conts.len() != not_inlined.len()
                || not_inlined
                    .iter()
                    .zip(prev_conts.iter())
                    .any(|(a, b)| !Gc::ptr_eq(*a, *b));
            if not_inlined.is_empty() {
                return if conts_changed || body_changed {
                    (body, true)
                } else {
                    (term, false)
                };
            }

            let conts = if !conts_changed {
                prev_conts
            } else {
                Array::from_slice(*ctx, not_inlined)
            };
            if !conts_changed && !body_changed {
                return (term, false);
            }
            (Gc::new(*ctx, Term::Letk(conts, body)), true)
        }

        Term::Fix(prev_funcs, prev_body) => {
            let funcs = prev_funcs
                .iter()
                .copied()
                .filter_map(|func| {
                    if state.is_dead(func.binding) {
                        return None;
                    }

                    let (body, body_changed) = shrink_tree(func.body(), state.clone());
                    Some(if body_changed {
                        func.with_body(state.ctx, body)
                    } else {
                        func
                    })
                })
                .collect::<Vec<_>>();
            let funcs_changed = prev_funcs.len() != funcs.len()
                || funcs
                    .iter()
                    .zip(prev_funcs.iter())
                    .any(|(a, b)| !Gc::ptr_eq(*a, *b));

            if funcs.is_empty() {
                let (body, body_changed) = shrink_tree(prev_body, state);
                return if funcs_changed || body_changed {
                    (body, true)
                } else {
                    (term, false)
                };
            }
            let ctx = state.ctx;
            let funcs = if !funcs_changed {
                prev_funcs
            } else {
                Array::from_slice(*state.ctx, funcs)
            };
            let (body, body_changed) = shrink_tree(prev_body, state);
            if !funcs_changed && !body_changed {
                return (term, false);
            }
            (Gc::new(*ctx, Term::Fix(funcs, body)), true)
        }

        Term::Continue(k_prev, args_prev, src) => {
            let k = state.cont_subst(k_prev);
            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();

            let args_changed = !args.iter().zip(args_prev.iter()).all(|(a, b)| a == b);
            let args = if !args_changed {
                args_prev
            } else {
                Array::from_slice(*state.ctx, args)
            };
            if let Some(k) = state.cenv.get(&k).copied()
                && !k.noinline
                && k.arity_matches(args.len())
            {
                let body = if let Some(variadic) = k.variadic() {
                    let fixed_args = &args[0..k.args.len()];
                    if state.is_referenced(variadic) {
                        materialize_list(
                            state,
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

                                shrink_tree(k.body(), state).0
                            }),
                        )
                    } else {
                        // if variadic is never referenced we don't need to materialize the list
                        let mut state = state.with_vars_to_atoms(&k.args(), fixed_args);
                        state =
                            state.with_var_to_atom_subst(variadic, Atom::Constant(Value::null()));
                        shrink_tree(k.body(), state).0
                    }
                } else {
                    let state = state.with_vars_to_atoms(&k.args(), &args);
                    shrink_tree(k.body(), state).0
                };
                (body, true)
            } else {
                if Gc::ptr_eq(k, k_prev) && !args_changed {
                    return (term, false);
                }
                (Gc::new(*state.ctx, Term::Continue(k, args, src)), true)
            }
        }

        Term::App(fun_prev, retc_prev, args_prev, span) => {
            let retc = state.cont_subst(retc_prev);

            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();
            let args_changed = !args.iter().zip(args_prev.iter()).all(|(a, b)| a == b);
            let args = if !args_changed {
                args_prev
            } else {
                Array::from_slice(*state.ctx, args)
            };

            let fun = state.atom_subst(fun_prev);

            match fun {
                Atom::Local(fun) if state.fenv.contains_key(&fun) => {
                    let fun = state.fenv[&fun];
                    if fun.arity_matches(args.len()) {
                        let mut state = state.with_c_subst(fun.return_cont, retc);
                        if let Some(variadic) = fun.variadic {
                            let body = materialize_list(
                                state,
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

                                    shrink_tree(fun.body(), state).0
                                }),
                            );
                            return (body, true);
                        } else {
                            state = state.with_vars_to_atoms(&fun.args, &args);
                            return {
                                let (body, _) = shrink_tree(fun.body(), state);
                                (body, true)
                            };
                        }
                    }
                }

                _ => {}
            }

            if Gc::ptr_eq(retc, retc_prev) && !args_changed && fun == fun_prev {
                return (term, false);
            }

            return (Gc::new(*state.ctx, Term::App(fun, retc, args, span)), true);
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

            let consequent = state.cont_subst(consequent_prev);
            let alternative = state.cont_subst(alternative_prev);
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
            let consequent_args_changed = match (consequent_args, consequent_args_prev) {
                (Some(a), Some(b)) => !Gc::ptr_eq(a, b),
                (None, None) => false,
                _ => true,
            };
            let alternative_args_changed = match (alternative_args, alternative_args_prev) {
                (Some(a), Some(b)) => !Gc::ptr_eq(a, b),
                (None, None) => false,
                _ => true,
            };

            if let Atom::Constant(test) = test {
                let (k, args) = if test != Value::new(false) {
                    (consequent, consequent_args)
                } else {
                    (alternative, alternative_args)
                };

                return (
                    Gc::new(
                        *state.ctx,
                        Term::Continue(
                            k,
                            args.unwrap_or_else(|| Array::from_slice(*state.ctx, [])),
                            Value::new(false),
                        ),
                    ),
                    true,
                );
            }

            if Gc::ptr_eq(consequent, consequent_prev)
                && Gc::ptr_eq(alternative, alternative_prev)
                && !consequent_args_changed
                && !alternative_args_changed
                && test == test_prev
            {
                return (term, false);
            }

            (
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
                ),
                true,
            )
        }
    })
}

pub fn shrink<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> (TermRef<'gc>, bool) {
    let census = census(term);
    let state = State::new(ctx, census);
    shrink_tree(term, state)
}

pub fn rewrite<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let mut profile = ProfileScope::new("cps.rewrite.total");
    if profile.is_enabled() {
        profile.field("term_size_before", size(term));
    }

    let mut simplified_tree = term;
    for round in 0..3 {
        let mut round_profile = ProfileScope::new("cps.rewrite.shrink");
        if round_profile.is_enabled() {
            round_profile.field("round", round + 1);
            round_profile.field("term_size_before", size(simplified_tree));
        }

        let (next, changed) = shrink(ctx, simplified_tree);
        simplified_tree = next;

        if round_profile.is_enabled() {
            round_profile.field("term_size_after", size(simplified_tree));
            round_profile.field("changed", changed);
        }

        if !changed {
            break;
        }
    }

    let max_size = size(simplified_tree) * 3 / 2;
    let mut rewritten = simplified_tree;
    for _ in 0..2 {
        let (next, changed) = inline(ctx, rewritten, max_size);
        rewritten = next;
        if !changed {
            break;
        }
    }

    if profile.is_enabled() {
        profile.field("term_size_after", size(rewritten));
        profile.field("max_size", max_size);
    }

    rewritten
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
                Expression::PrimCall(prim, args, source) => {
                    let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
                    let atoms = Array::from_slice(*ctx, args);

                    Expression::PrimCall(prim, atoms, source)
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

        Term::App(fun, retc, args, src) => {
            let fun = subv.subst(fun);

            let retc = subc.subst(retc);

            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
            let args = Array::from_slice(*ctx, args);

            Gc::new(*ctx, Term::App(fun, retc, args, src))
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

    let subc1 = subc;
    subc1.insert(fun.return_cont, retc1);

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

fn mentions_tracked_atom<'gc>(tracked_binding: Option<LVarRef<'gc>>, atom: Atom<'gc>) -> bool {
    matches!(atom, Atom::Local(var) if Some(var) == tracked_binding)
}

fn mentions_tracked_var<'gc>(tracked_binding: Option<LVarRef<'gc>>, var: LVarRef<'gc>) -> bool {
    Some(var) == tracked_binding
}

fn term_metadata<'gc>(term: TermRef<'gc>, tracked_binding: Option<LVarRef<'gc>>) -> (usize, bool) {
    match *term {
        Term::Let(_, Expression::PrimCall(_, args, _), body) => {
            let (body_size, body_mentions) = term_metadata(body, tracked_binding);
            let mentions = body_mentions
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg));
            (1 + body_size, mentions)
        }

        Term::Letk(conts, body) => {
            let (body_size, body_mentions) = term_metadata(body, tracked_binding);
            let (conts_size, conts_mentions) = conts.iter().fold((0, false), |acc, cont| {
                let (size, mentions) = term_metadata(cont.body(), tracked_binding);
                (acc.0 + size, acc.1 || mentions)
            });
            (body_size + conts_size, body_mentions || conts_mentions)
        }

        Term::Fix(funcs, body) => {
            let (body_size, body_mentions) = term_metadata(body, tracked_binding);
            let (funcs_size, funcs_mentions) = funcs.iter().fold((0, false), |acc, func| {
                let (size, mentions) = term_metadata(func.body(), tracked_binding);
                (acc.0 + size, acc.1 || mentions)
            });
            (body_size + funcs_size, body_mentions || funcs_mentions)
        }

        Term::Continue(k, args, _) => (
            1,
            mentions_tracked_var(tracked_binding, k)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg)),
        ),

        Term::App(fun, retc, args, _) => (
            1,
            mentions_tracked_atom(tracked_binding, fun)
                || mentions_tracked_var(tracked_binding, retc)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg)),
        ),

        Term::If {
            test,
            consequent,
            consequent_args,
            alternative,
            alternative_args,
            ..
        } => (
            1,
            mentions_tracked_atom(tracked_binding, test)
                || mentions_tracked_var(tracked_binding, consequent)
                || mentions_tracked_var(tracked_binding, alternative)
                || consequent_args.iter().any(|args| {
                    args.iter()
                        .copied()
                        .any(|arg| mentions_tracked_atom(tracked_binding, arg))
                })
                || alternative_args.iter().any(|args| {
                    args.iter()
                        .copied()
                        .any(|arg| mentions_tracked_atom(tracked_binding, arg))
                }),
        ),
    }
}

fn inline_t<'gc>(
    state: State<'gc>,
    term: TermRef<'gc>,
    cnt_limit: usize,
    tracked_binding: Option<LVarRef<'gc>>,
) -> RewriteResult<'gc> {
    let loop_limit = LOOP_UNROLL[cnt_limit];
    let fun_limit = FIBONACCI[cnt_limit];

    match *term {
        Term::Let(name, prev_exp, prev_body) => {
            let exp = match prev_exp {
                Expression::PrimCall(prim, prev_args, src) => {
                    let args = prev_args
                        .iter()
                        .map(|&arg| state.atom_subst(arg))
                        .collect::<Vec<_>>();
                    let args = if args.iter().zip(prev_args.iter()).all(|(a, b)| a == b) {
                        prev_args
                    } else {
                        Array::from_slice(*state.ctx, args)
                    };

                    Expression::PrimCall(prim, args, src)
                }
            };
            let ctx = state.ctx;
            let exp_changed = exp != prev_exp;
            let body = inline_t(state, prev_body, cnt_limit, tracked_binding);
            let mentions = body.mentions_tracked_binding
                || match exp {
                    Expression::PrimCall(_, args, _) => args
                        .iter()
                        .copied()
                        .any(|arg| mentions_tracked_atom(tracked_binding, arg)),
                };
            let size = 1 + body.size;
            if !exp_changed && !body.changed {
                return RewriteResult {
                    term,
                    changed: false,
                    size,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(*ctx, Term::Let(name, exp, body.term)),
                changed: true,
                size,
                mentions_tracked_binding: mentions,
            }
        }

        Term::Letk(prev_conts, prev_body) => {
            let mut changed = false;
            let mut size = 0;
            let mut mentions = false;
            let conts = prev_conts
                .iter()
                .copied()
                .map(|cnt| {
                    let body = inline_t(state.clone(), cnt.body(), cnt_limit, Some(cnt.binding()));
                    changed |= body.changed;
                    size += body.size;
                    mentions |= body.mentions_tracked_binding;
                    let newk = if body.changed {
                        cnt.with_body(state.ctx, body.term)
                    } else {
                        cnt
                    };

                    let my_size = body.size;
                    let dont = cnt.noinline
                        || my_size > cnt_limit
                        || (my_size > loop_limit && body.mentions_tracked_binding);
                    (newk, if dont { None } else { Some(newk) })
                })
                .collect::<Vec<_>>();

            let (i_ks, to_inline): (Vec<_>, Vec<_>) = conts.into_iter().unzip();
            let ctx = state.ctx;
            let s =
                state.with_continuations(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, prev_body, cnt_limit, tracked_binding);
            changed |= body.changed;
            size += body.size;
            mentions |= body.mentions_tracked_binding;
            let ks_changed = !i_ks
                .iter()
                .zip(prev_conts.iter())
                .all(|(a, b)| Gc::ptr_eq(*a, *b));
            changed |= ks_changed;
            let i_ks = if ks_changed {
                Array::from_slice(*ctx, i_ks)
            } else {
                prev_conts
            };
            if !changed {
                return RewriteResult {
                    term,
                    changed: false,
                    size,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(*ctx, Term::Letk(i_ks, body.term)),
                changed: true,
                size,
                mentions_tracked_binding: mentions,
            }
        }

        Term::Fix(prev_funcs, prev_body) => {
            let mut changed = false;
            let mut size = 0;
            let mut mentions = false;
            let funcs = prev_funcs
                .iter()
                .copied()
                .map(|func| {
                    let body = inline_t(state.clone(), func.body(), cnt_limit, Some(func.binding));
                    changed |= body.changed;
                    size += body.size;
                    mentions |= body.mentions_tracked_binding;
                    let newf = func.with_body(state.ctx, body.term);

                    let my_size = body.size;
                    let dont = my_size > fun_limit
                        || (my_size > loop_limit && body.mentions_tracked_binding);

                    (newf, if dont { None } else { Some(newf) })
                })
                .collect::<Vec<_>>();

            let (i_fs, to_inline): (Vec<_>, Vec<_>) = funcs.into_iter().unzip();
            let ctx = state.ctx;
            let s = state.with_functions(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, prev_body, cnt_limit, tracked_binding);
            changed |= body.changed;
            size += body.size;
            mentions |= body.mentions_tracked_binding;
            let fs_changed = !i_fs
                .iter()
                .zip(prev_funcs.iter())
                .all(|(a, b)| Gc::ptr_eq(*a, *b));
            changed |= fs_changed;
            let ifs = if fs_changed {
                Array::from_slice(*ctx, i_fs)
            } else {
                prev_funcs
            };
            if !changed {
                return RewriteResult {
                    term,
                    changed: false,
                    size,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(*ctx, Term::Fix(ifs, body.term)),
                changed: true,
                size,
                mentions_tracked_binding: mentions,
            }
        }

        Term::Continue(prev_cnt, prev_args, src) => {
            let args = state
                .substitute_atoms(prev_args.iter().copied())
                .collect::<Vec<_>>();
            let args_changed = !prev_args.iter().zip(args.iter()).all(|(a, b)| a == b);
            let args = if !args_changed {
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

                        let term = copy_t(
                            state.ctx,
                            new_cnt.body(),
                            &mut subst,
                            &mut HashMap::default(),
                        );
                        let (size, mentions_tracked_binding) = term_metadata(term, tracked_binding);
                        return RewriteResult {
                            term,
                            changed: true,
                            size,
                            mentions_tracked_binding,
                        };
                    }
                }
            }

            let mentions = mentions_tracked_var(tracked_binding, cnt)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg));
            if Gc::ptr_eq(cnt, prev_cnt) && !args_changed {
                return RewriteResult {
                    term,
                    changed: false,
                    size: 1,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(*state.ctx, Term::Continue(cnt, args, src)),
                changed: true,
                size: 1,
                mentions_tracked_binding: mentions,
            }
        }

        Term::App(prev_fun, prev_retc, prev_args, src) => {
            let retc = state.var_subst(prev_retc);

            let args = state
                .substitute_atoms(prev_args.iter().copied())
                .collect::<Vec<_>>();
            let args_changed = !args.iter().zip(prev_args.iter()).all(|(a, b)| a == b);
            let args = if !args_changed {
                prev_args
            } else {
                Array::from_slice(*state.ctx, args)
            };

            let fun = state.atom_subst(prev_fun);
            if let Atom::Local(new_fun) = fun {
                if let Some(func) = state.fenv.get(&new_fun).copied() {
                    if func.args.len() == args.len() && func.variadic.is_none() {
                        let mut subv = func
                            .args
                            .iter()
                            .zip(args.iter())
                            .map(|(arg, val)| (Atom::Local(*arg), *val))
                            .collect::<HashMap<_, _>>();

                        let mut subc = HashMap::default();
                        subc.insert(func.return_cont, retc);

                        let term = copy_t(state.ctx, func.body(), &mut subv, &mut subc);
                        let (size, mentions_tracked_binding) = term_metadata(term, tracked_binding);
                        return RewriteResult {
                            term,
                            changed: true,
                            size,
                            mentions_tracked_binding,
                        };
                    }
                }
            }

            let mentions = mentions_tracked_atom(tracked_binding, fun)
                || mentions_tracked_var(tracked_binding, retc)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg));
            if Gc::ptr_eq(retc, prev_retc) && !args_changed && fun == prev_fun {
                return RewriteResult {
                    term,
                    changed: false,
                    size: 1,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(*state.ctx, Term::App(fun, retc, args, src)),
                changed: true,
                size: 1,
                mentions_tracked_binding: mentions,
            }
        }

        Term::If {
            test: prev_test,
            consequent: prev_consequent,
            consequent_args: prev_consequent_args,
            alternative: prev_alternative,
            alternative_args: prev_alternative_args,
            hints,
        } => {
            let test = state.atom_subst(prev_test);
            let consequent = state.var_subst(prev_consequent);
            let alternative = state.var_subst(prev_alternative);
            let consequent_args = prev_consequent_args.map(|args| {
                args.iter()
                    .map(|&a| state.atom_subst(a))
                    .collect_gc(*state.ctx)
            });
            let alternative_args = prev_alternative_args.map(|args| {
                args.iter()
                    .map(|&a| state.atom_subst(a))
                    .collect_gc(*state.ctx)
            });
            let consequent_args_changed = match (consequent_args, prev_consequent_args) {
                (Some(a), Some(b)) => !Gc::ptr_eq(a, b),
                (None, None) => false,
                _ => true,
            };
            let alternative_args_changed = match (alternative_args, prev_alternative_args) {
                (Some(a), Some(b)) => !Gc::ptr_eq(a, b),
                (None, None) => false,
                _ => true,
            };
            let changed = test != prev_test
                || !Gc::ptr_eq(consequent, prev_consequent)
                || !Gc::ptr_eq(alternative, prev_alternative)
                || consequent_args_changed
                || alternative_args_changed;
            let mentions = mentions_tracked_atom(tracked_binding, test)
                || mentions_tracked_var(tracked_binding, consequent)
                || mentions_tracked_var(tracked_binding, alternative)
                || consequent_args.iter().any(|args| {
                    args.iter()
                        .copied()
                        .any(|arg| mentions_tracked_atom(tracked_binding, arg))
                })
                || alternative_args.iter().any(|args| {
                    args.iter()
                        .copied()
                        .any(|arg| mentions_tracked_atom(tracked_binding, arg))
                });
            if !changed {
                return RewriteResult {
                    term,
                    changed: false,
                    size: 1,
                    mentions_tracked_binding: mentions,
                };
            }
            RewriteResult {
                term: Gc::new(
                    *state.ctx,
                    Term::If {
                        test,
                        consequent,
                        consequent_args,
                        alternative,
                        alternative_args,
                        hints,
                    },
                ),
                changed: true,
                size: 1,
                mentions_tracked_binding: mentions,
            }
        }
    }
}

pub fn inline<'gc>(
    ctx: Context<'gc>,
    mut term: TermRef<'gc>,
    max_size: usize,
) -> (TermRef<'gc>, bool) {
    let mut changed = false;
    let mut term_size = size(term);
    for i in 0..FIBONACCI.len() {
        let mut round_profile = ProfileScope::new("cps.rewrite.inline");
        if round_profile.is_enabled() {
            round_profile.field("round", i + 1);
            round_profile.field("term_size_before", term_size);
            round_profile.field("max_size", max_size);
        }

        if term_size > max_size {
            if round_profile.is_enabled() {
                round_profile.field("term_size_after", term_size);
                round_profile.field("inline_changed", false);
                round_profile.field("shrink_changed", false);
                round_profile.field("changed", false);
                round_profile.field("skipped", true);
            }
            return (term, changed);
        }

        let state = State::new(ctx, census(term));
        let next = inline_t(state, term, i, None);
        let (next_term, shrink_changed) = shrink(ctx, next.term);
        let inline_changed = next.changed;
        changed |= inline_changed || shrink_changed;
        term_size = if shrink_changed {
            size(next_term)
        } else {
            next.size
        };
        if round_profile.is_enabled() {
            round_profile.field("term_size_after", term_size);
            round_profile.field("inline_changed", inline_changed);
            round_profile.field("shrink_changed", shrink_changed);
            round_profile.field("changed", inline_changed || shrink_changed);
        }
        term = next_term;
    }

    (term, changed)
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
                        Value::new(false),
                    ),
                    f,
                ),
            )
        }),
    )
}

static SIDE_EFFECT_FREE_OPS: LazyLock<std::collections::HashSet<&'static str>> =
    LazyLock::new(|| {
        std::collections::HashSet::from(["variable-ref", "cons", "list", "make-variable", "vector"])
    });
