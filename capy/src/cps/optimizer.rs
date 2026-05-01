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
}

#[derive(Clone, Copy)]
struct RewriteResult<'gc> {
    term: TermRef<'gc>,
    changed: bool,
    size: usize,
    mentions_tracked_binding: bool,
}

#[derive(Clone, Copy)]
struct ShrinkResult<'gc> {
    term: TermRef<'gc>,
    changed: bool,
    size: usize,
}

#[derive(Clone, Copy)]
struct CopyResult<'gc> {
    term: TermRef<'gc>,
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

    fn applied_once(&self, var: LVarRef<'gc>) -> bool {
        self.census
            .get(&var)
            .map_or(false, |count| count.applied == 1 && count.as_value == 0)
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

fn substitute_atoms_array<'gc>(state: &State<'gc>, prev_args: Atoms<'gc>) -> (Atoms<'gc>, bool) {
    let mut args: Option<Vec<Atom<'gc>>> = None;

    for (index, prev_arg) in prev_args.iter().copied().enumerate() {
        let arg = state.atom_subst(prev_arg);
        match &mut args {
            Some(args) => args.push(arg),
            None if arg != prev_arg => {
                let mut changed_args = Vec::with_capacity(prev_args.len());
                changed_args.extend(prev_args[..index].iter().copied());
                changed_args.push(arg);
                args = Some(changed_args);
            }
            None => {}
        }
    }

    match args {
        Some(args) => (Array::from_slice(*state.ctx, args), true),
        None => (prev_args, false),
    }
}

fn substitute_atoms_vec<'gc>(state: &State<'gc>, prev_args: Atoms<'gc>) -> (Vec<Atom<'gc>>, bool) {
    let mut changed = false;
    let args = prev_args
        .iter()
        .copied()
        .map(|prev_arg| {
            let arg = state.atom_subst(prev_arg);
            changed |= arg != prev_arg;
            arg
        })
        .collect::<Vec<_>>();
    (args, changed)
}

fn atoms_mention_tracked_binding<'gc>(
    tracked_binding: Option<LVarRef<'gc>>,
    args: Atoms<'gc>,
) -> bool {
    args.iter()
        .copied()
        .any(|arg| mentions_tracked_atom(tracked_binding, arg))
}

fn census<'gc>(term: TermRef<'gc>) -> Map<LVarRef<'gc>, Count> {
    let mut census = HashMap::default();
    let mut rhs = HashMap::default();
    let mut worklist = vec![term];

    fn inc_app_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
        worklist: &mut Vec<TermRef<'gc>>,
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
            worklist.push(tree);
        }
    }

    fn inc_app_use_a<'gc>(
        atom: Atom<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
        worklist: &mut Vec<TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_app_use_n(name, census, rhs, worklist);
        }
    }

    fn inc_val_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
        worklist: &mut Vec<TermRef<'gc>>,
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
            worklist.push(tree);
        }
    }

    fn inc_val_use_a<'gc>(
        atom: Atom<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
        worklist: &mut Vec<TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_val_use_n(name, census, rhs, worklist);
        }
    }

    while let Some(mut term) = worklist.pop() {
        loop {
            match *term {
                Term::Let(_, expr, body) => {
                    match expr {
                        Expression::PrimCall(_, args, _) => {
                            for arg in args.iter() {
                                inc_val_use_a(*arg, &mut census, &mut rhs, &mut worklist);
                            }
                        }
                    }

                    term = body;
                }

                Term::Letk(ks, body) => {
                    for k in ks.iter() {
                        rhs.insert(k.binding(), k.body());
                    }

                    term = body;
                }

                Term::Fix(funcs, body) => {
                    for fun in funcs.iter() {
                        rhs.insert(fun.binding, fun.body());
                    }

                    term = body;
                }

                Term::App(fun, ret_c, args, _) => {
                    inc_app_use_a(fun, &mut census, &mut rhs, &mut worklist);
                    inc_val_use_n(ret_c, &mut census, &mut rhs, &mut worklist);

                    for arg in args.iter() {
                        inc_val_use_a(*arg, &mut census, &mut rhs, &mut worklist);
                    }
                    break;
                }

                Term::Continue(k, args, _) => {
                    inc_app_use_n(k, &mut census, &mut rhs, &mut worklist);
                    for arg in args.iter() {
                        inc_val_use_a(*arg, &mut census, &mut rhs, &mut worklist);
                    }
                    break;
                }

                Term::If {
                    test,
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    ..
                } => {
                    inc_val_use_a(test, &mut census, &mut rhs, &mut worklist);
                    inc_val_use_n(consequent, &mut census, &mut rhs, &mut worklist);
                    inc_val_use_n(alternative, &mut census, &mut rhs, &mut worklist);
                    if let Some(args) = consequent_args {
                        for arg in args.iter() {
                            inc_val_use_a(*arg, &mut census, &mut rhs, &mut worklist);
                        }
                    }

                    if let Some(args) = alternative_args {
                        for arg in args.iter() {
                            inc_val_use_a(*arg, &mut census, &mut rhs, &mut worklist);
                        }
                    }
                    break;
                }
            }
        }
    }

    census.into_iter().collect()
}

struct PendingShrinkLet<'gc> {
    original: TermRef<'gc>,
    binding: LVarRef<'gc>,
    expr: Expression<'gc>,
}

fn shrink_tree<'gc>(term: TermRef<'gc>, state: State<'gc>) -> ShrinkResult<'gc> {
    stacker::maybe_grow(4 * 1024 * 1024, 1 * 1024 * 1024, || {
        shrink_tree_inner(term, state)
    })
}

fn shrink_tree_inner<'gc>(mut term: TermRef<'gc>, mut state: State<'gc>) -> ShrinkResult<'gc> {
    let mut lets = Vec::new();
    let mut changed = false;

    loop {
        match *term {
            Term::Let(binding, Expression::PrimCall(prim, prev_args, source), prev_body) => {
                /*if state.is_dead(binding)
                    && SIDE_EFFECT_FREE_OPS.contains(&*prim.downcast::<Symbol>().as_str())
                {
                    changed = true;
                    term = prev_body;
                    continue;
                }*/

                let (args, args_changed) = substitute_atoms_vec(&state, prev_args);

                if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
                    if let Some(atom) = folding_table(state.ctx).try_fold(state.ctx, prim, &args) {
                        state = state.with_atom_subst(Atom::Local(binding), Atom::Constant(atom));
                        changed = true;
                        term = prev_body;
                        continue;
                    }
                }

                let atoms = if args_changed {
                    changed = true;
                    Array::from_slice(*state.ctx, args)
                } else {
                    prev_args
                };
                lets.push(PendingShrinkLet {
                    original: term,
                    binding,
                    expr: Expression::PrimCall(prim, atoms, source),
                });
                term = prev_body;
            }

            _ => {
                let ctx = state.ctx;
                let mut result = shrink_tree_non_let(term, state);
                result.changed |= changed;
                result.size += lets.len();

                if !result.changed {
                    if let Some(first) = lets.first() {
                        result.term = first.original;
                    }
                    return result;
                }

                let mut body = result.term;
                for pending in lets.into_iter().rev() {
                    body = Gc::new(*ctx, Term::Let(pending.binding, pending.expr, body));
                }
                result.term = body;
                return result;
            }
        }
    }
}

fn shrink_tree_non_let<'gc>(term: TermRef<'gc>, state: State<'gc>) -> ShrinkResult<'gc> {
    match *term {
        Term::Let(..) => shrink_tree(term, state),

        Term::Letk(prev_conts, prev_body) => {
            let mut inlined = Vec::new();
            let mut not_inlined = Vec::new();
            let mut not_inlined_size = 0;

            for cont in prev_conts.iter().copied() {
                if state.is_dead(cont.binding()) {
                    continue;
                }

                let body = shrink_tree(cont.body(), state.clone());
                let cont = if body.changed {
                    cont.with_body(state.ctx, body.term)
                } else {
                    cont
                };

                if !cont.noinline && state.applied_once(cont.binding()) {
                    inlined.push(cont);
                } else {
                    not_inlined_size += body.size;
                    not_inlined.push(cont);
                }
            }

            let ctx = state.ctx;
            let state = state.with_continuations(&inlined);
            let body = shrink_tree(prev_body, state);
            let conts_changed = prev_conts.len() != not_inlined.len()
                || not_inlined
                    .iter()
                    .zip(prev_conts.iter())
                    .any(|(a, b)| !Gc::ptr_eq(*a, *b));
            let size = body.size + not_inlined_size;

            if not_inlined.is_empty() {
                return ShrinkResult {
                    term: if conts_changed || body.changed {
                        body.term
                    } else {
                        term
                    },
                    changed: conts_changed || body.changed,
                    size,
                };
            }

            if !conts_changed && !body.changed {
                return ShrinkResult {
                    term,
                    changed: false,
                    size,
                };
            }

            ShrinkResult {
                term: Gc::new(
                    *ctx,
                    Term::Letk(Array::from_slice(*ctx, not_inlined), body.term),
                ),
                changed: true,
                size,
            }
        }

        Term::Fix(prev_funcs, prev_body) => {
            let mut funcs = Vec::new();
            let mut funcs_size = 0;
            let mut func_bodies_changed = false;

            for func in prev_funcs.iter().copied() {
                if state.is_dead(func.binding) {
                    continue;
                }

                let body = shrink_tree(func.body(), state.clone());
                funcs_size += body.size;
                func_bodies_changed |= body.changed;
                funcs.push(if body.changed {
                    func.with_body(state.ctx, body.term)
                } else {
                    func
                });
            }

            let funcs_changed = func_bodies_changed
                || prev_funcs.len() != funcs.len()
                || funcs
                    .iter()
                    .zip(prev_funcs.iter())
                    .any(|(a, b)| !Gc::ptr_eq(*a, *b));
            let ctx = state.ctx;
            let body = shrink_tree(prev_body, state);
            let size = body.size + funcs_size;

            if funcs.is_empty() {
                return ShrinkResult {
                    term: if funcs_changed || body.changed {
                        body.term
                    } else {
                        term
                    },
                    changed: funcs_changed || body.changed,
                    size,
                };
            }

            if !funcs_changed && !body.changed {
                return ShrinkResult {
                    term,
                    changed: false,
                    size,
                };
            }

            ShrinkResult {
                term: Gc::new(*ctx, Term::Fix(Array::from_slice(*ctx, funcs), body.term)),
                changed: true,
                size,
            }
        }

        Term::Continue(k_prev, args_prev, src) => {
            let k = state.cont_subst(k_prev);
            let (args, args_changed) = substitute_atoms_array(&state, args_prev);

            if let Some(k) = state.cenv.get(&k).copied()
                && !k.noinline
                && k.arity_matches(args.len())
            {
                let result = if let Some(variadic) = k.variadic() {
                    let fixed_args = &args[0..k.args.len()];
                    if state.is_referenced(variadic) {
                        let body = materialize_list(
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

                                shrink_tree(k.body(), state).term
                            }),
                        );
                        ShrinkResult {
                            term: body,
                            changed: true,
                            size: size(body),
                        }
                    } else {
                        let mut state = state.with_vars_to_atoms(&k.args(), fixed_args);
                        state =
                            state.with_var_to_atom_subst(variadic, Atom::Constant(Value::null()));
                        shrink_tree(k.body(), state)
                    }
                } else {
                    let state = state.with_vars_to_atoms(&k.args(), &args);
                    shrink_tree(k.body(), state)
                };

                return ShrinkResult {
                    changed: true,
                    ..result
                };
            }

            if Gc::ptr_eq(k, k_prev) && !args_changed {
                return ShrinkResult {
                    term,
                    changed: false,
                    size: 1,
                };
            }

            ShrinkResult {
                term: Gc::new(*state.ctx, Term::Continue(k, args, src)),
                changed: true,
                size: 1,
            }
        }

        Term::App(fun_prev, retc_prev, args_prev, span) => {
            let retc = state.cont_subst(retc_prev);
            let (args, args_changed) = substitute_atoms_array(&state, args_prev);
            let fun = state.atom_subst(fun_prev);

            if let Atom::Local(fun_name) = fun
                && let Some(fun) = state.fenv.get(&fun_name).copied()
                && fun.arity_matches(args.len())
            {
                let mut state = state.with_c_subst(fun.return_cont, retc);
                let result = if let Some(variadic) = fun.variadic {
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

                            shrink_tree(fun.body(), state).term
                        }),
                    );
                    ShrinkResult {
                        term: body,
                        changed: true,
                        size: size(body),
                    }
                } else {
                    state = state.with_vars_to_atoms(&fun.args, &args);
                    shrink_tree(fun.body(), state)
                };

                return ShrinkResult {
                    changed: true,
                    ..result
                };
            }

            if Gc::ptr_eq(retc, retc_prev) && !args_changed && fun == fun_prev {
                return ShrinkResult {
                    term,
                    changed: false,
                    size: 1,
                };
            }

            ShrinkResult {
                term: Gc::new(*state.ctx, Term::App(fun, retc, args, span)),
                changed: true,
                size: 1,
            }
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
            let (consequent_args, consequent_args_changed) = match consequent_args_prev {
                Some(args) => {
                    let (args, changed) = substitute_atoms_array(&state, args);
                    (Some(args), changed)
                }
                None => (None, false),
            };
            let (alternative_args, alternative_args_changed) = match alternative_args_prev {
                Some(args) => {
                    let (args, changed) = substitute_atoms_array(&state, args);
                    (Some(args), changed)
                }
                None => (None, false),
            };

            if let Atom::Constant(test) = test {
                let (k, args) = if test != Value::new(false) {
                    (consequent, consequent_args)
                } else {
                    (alternative, alternative_args)
                };

                return ShrinkResult {
                    term: Gc::new(
                        *state.ctx,
                        Term::Continue(
                            k,
                            args.unwrap_or_else(|| Array::from_slice(*state.ctx, [])),
                            Value::new(false),
                        ),
                    ),
                    changed: true,
                    size: 1,
                };
            }

            if Gc::ptr_eq(consequent, consequent_prev)
                && Gc::ptr_eq(alternative, alternative_prev)
                && !consequent_args_changed
                && !alternative_args_changed
                && test == test_prev
            {
                return ShrinkResult {
                    term,
                    changed: false,
                    size: 1,
                };
            }

            ShrinkResult {
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
            }
        }
    }
}

pub fn shrink<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> (TermRef<'gc>, bool) {
    let result = shrink_with_size(ctx, term);
    (result.term, result.changed)
}

fn shrink_with_size<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> ShrinkResult<'gc> {
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
    let mut simplified_size = size(simplified_tree);
    for round in 0..3 {
        let mut round_profile = ProfileScope::new("cps.rewrite.shrink");
        if round_profile.is_enabled() {
            round_profile.field("round", round + 1);
            round_profile.field("term_size_before", simplified_size);
        }

        let next = shrink_with_size(ctx, simplified_tree);
        simplified_tree = next.term;
        simplified_size = next.size;

        if round_profile.is_enabled() {
            round_profile.field("term_size_after", simplified_size);
            round_profile.field("changed", next.changed);
        }

        if !next.changed {
            break;
        }
    }

    let max_size = simplified_size * 3 / 2;
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
) -> CopyResult<'gc> {
    copy_t_with_metadata(ctx, term, subv, subc, None)
}

struct PendingCopyLet<'gc> {
    binding: LVarRef<'gc>,
    expr: Expression<'gc>,
}

fn copy_t_with_metadata<'gc>(
    ctx: Context<'gc>,
    mut term: TermRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    tracked_binding: Option<LVarRef<'gc>>,
) -> CopyResult<'gc> {
    let mut lets = Vec::new();

    loop {
        match *term {
            Term::Let(binding, expr, body) => {
                let binding1 = binding.copy(ctx);
                subv.insert(Atom::Local(binding), Atom::Local(binding1));
                lets.push(PendingCopyLet {
                    binding: binding1,
                    expr,
                });
                term = body;
            }

            _ => {
                let mut result = copy_non_let(ctx, term, subv, subc, tracked_binding);
                for pending in lets.into_iter().rev() {
                    let expr = match pending.expr {
                        Expression::PrimCall(prim, args, source) => {
                            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
                            let mentions = args
                                .iter()
                                .copied()
                                .any(|arg| mentions_tracked_atom(tracked_binding, arg));
                            result.mentions_tracked_binding |= mentions;
                            Expression::PrimCall(prim, Array::from_slice(*ctx, args), source)
                        }
                    };
                    result.term = Gc::new(*ctx, Term::Let(pending.binding, expr, result.term));
                    result.size += 1;
                }
                return result;
            }
        }
    }
}

fn copy_non_let<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    tracked_binding: Option<LVarRef<'gc>>,
) -> CopyResult<'gc> {
    match *term {
        Term::Let(..) => copy_t_with_metadata(ctx, term, subv, subc, tracked_binding),

        Term::Letk(conts, body) => {
            let names = conts.iter().map(|c| c.binding()).collect::<Vec<_>>();
            let names1 = conts
                .iter()
                .map(|c| c.binding().copy(ctx))
                .collect::<Vec<_>>();

            for (old, new) in names.iter().zip(names1.iter()) {
                subc.insert(*old, *new);
            }

            let mut size = 0;
            let mut mentions = false;
            let conts = conts
                .iter()
                .map(|cont| {
                    let (cont, body) = copy_c(ctx, *cont, subv, subc, tracked_binding);
                    size += body.size;
                    mentions |= body.mentions_tracked_binding;
                    cont
                })
                .collect::<Vec<_>>();

            let conts = Array::from_slice(*ctx, conts);

            let body = copy_t_with_metadata(ctx, body, subv, subc, tracked_binding);
            size += body.size;
            mentions |= body.mentions_tracked_binding;

            CopyResult {
                term: Gc::new(*ctx, Term::Letk(conts, body.term)),
                size,
                mentions_tracked_binding: mentions,
            }
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

            let mut size = 0;
            let mut mentions = false;
            let funcs = funcs
                .iter()
                .map(|func| {
                    let (func, body) = copy_f(ctx, *func, &mut subv1, subc, tracked_binding);
                    size += body.size;
                    mentions |= body.mentions_tracked_binding;
                    func
                })
                .collect::<Vec<_>>();
            let funcs = Array::from_slice(*ctx, funcs);

            let body = copy_t_with_metadata(ctx, body, &mut subv1, subc, tracked_binding);
            size += body.size;
            mentions |= body.mentions_tracked_binding;

            CopyResult {
                term: Gc::new(*ctx, Term::Fix(funcs, body.term)),
                size,
                mentions_tracked_binding: mentions,
            }
        }

        Term::Continue(cont, args, src) => {
            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
            let cont = subc.subst(cont);
            let mentions = mentions_tracked_var(tracked_binding, cont)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg));
            let args = Array::from_slice(*ctx, args);

            CopyResult {
                term: Gc::new(*ctx, Term::Continue(cont, args, src)),
                size: 1,
                mentions_tracked_binding: mentions,
            }
        }

        Term::App(fun, retc, args, src) => {
            let fun = subv.subst(fun);

            let retc = subc.subst(retc);

            let args = args.iter().map(|&a| subv.subst(a)).collect::<Vec<_>>();
            let mentions = mentions_tracked_atom(tracked_binding, fun)
                || mentions_tracked_var(tracked_binding, retc)
                || args
                    .iter()
                    .copied()
                    .any(|arg| mentions_tracked_atom(tracked_binding, arg));
            let args = Array::from_slice(*ctx, args);

            CopyResult {
                term: Gc::new(*ctx, Term::App(fun, retc, args, src)),
                size: 1,
                mentions_tracked_binding: mentions,
            }
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
            let mentions = mentions_tracked_atom(tracked_binding, test)
                || mentions_tracked_var(tracked_binding, consequent)
                || mentions_tracked_var(tracked_binding, alternative)
                || consequent_args
                    .iter()
                    .any(|args| atoms_mention_tracked_binding(tracked_binding, *args))
                || alternative_args
                    .iter()
                    .any(|args| atoms_mention_tracked_binding(tracked_binding, *args));

            CopyResult {
                term: Gc::new(
                    *ctx,
                    Term::If {
                        test,
                        consequent,
                        consequent_args,
                        alternative,
                        alternative_args,
                        hints,
                    },
                ),
                size: 1,
                mentions_tracked_binding: mentions,
            }
        }
    }
}

fn copy_c<'gc>(
    ctx: Context<'gc>,
    cont: ContRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    tracked_binding: Option<LVarRef<'gc>>,
) -> (ContRef<'gc>, CopyResult<'gc>) {
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
    let body = copy_t_with_metadata(ctx, cont.body(), subv1, subc, tracked_binding);
    let cont = Gc::new(
        *ctx,
        Cont {
            name: cont.name(),
            binding,

            body: Lock::new(body.term),
            args: args1,
            variadic: var1,
            source: cont.source(),
            noinline: cont.noinline,
            free_vars: Lock::new(cont.free_vars.get()),
            reified: Cell::new(cont.reified.get()),
            cold: cont.cold,
            meta: cont.meta,
        },
    );
    (cont, body)
}

fn copy_f<'gc>(
    ctx: Context<'gc>,
    fun: FuncRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
    tracked_binding: Option<LVarRef<'gc>>,
) -> (FuncRef<'gc>, CopyResult<'gc>) {
    let retc1 = fun.return_cont.copy(ctx);

    subc.insert(fun.return_cont, retc1);

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

    let body = copy_t_with_metadata(ctx, fun.body(), subv1, subc, tracked_binding);

    let func = Gc::new(
        *ctx,
        Func {
            meta: fun.meta,
            return_cont: retc1,

            name: fun.name,
            binding,

            args: Array::from_slice(*ctx, args1),
            variadic: var1,
            body: Lock::new(body.term),
            source: fun.source,
            free_vars: Lock::new(fun.free_vars.get()),
        },
    );
    (func, body)
}

fn mentions_tracked_atom<'gc>(tracked_binding: Option<LVarRef<'gc>>, atom: Atom<'gc>) -> bool {
    matches!(atom, Atom::Local(var) if Some(var) == tracked_binding)
}

fn mentions_tracked_var<'gc>(tracked_binding: Option<LVarRef<'gc>>, var: LVarRef<'gc>) -> bool {
    Some(var) == tracked_binding
}

fn term_metadata<'gc>(term: TermRef<'gc>, tracked_binding: Option<LVarRef<'gc>>) -> (usize, bool) {
    let mut size = 0;
    let mut mentions = false;
    let mut stack = vec![term];

    while let Some(mut term) = stack.pop() {
        loop {
            match *term {
                Term::Let(_, Expression::PrimCall(_, args, _), body) => {
                    size += 1;
                    mentions |= atoms_mention_tracked_binding(tracked_binding, args);
                    term = body;
                }

                Term::Letk(conts, body) => {
                    for cont in conts.iter() {
                        stack.push(cont.body());
                    }
                    term = body;
                }

                Term::Fix(funcs, body) => {
                    for func in funcs.iter() {
                        stack.push(func.body());
                    }
                    term = body;
                }

                Term::Continue(k, args, _) => {
                    size += 1;
                    mentions |= mentions_tracked_var(tracked_binding, k)
                        || atoms_mention_tracked_binding(tracked_binding, args);
                    break;
                }

                Term::App(fun, retc, args, _) => {
                    size += 1;
                    mentions |= mentions_tracked_atom(tracked_binding, fun)
                        || mentions_tracked_var(tracked_binding, retc)
                        || atoms_mention_tracked_binding(tracked_binding, args);
                    break;
                }

                Term::If {
                    test,
                    consequent,
                    consequent_args,
                    alternative,
                    alternative_args,
                    ..
                } => {
                    size += 1;
                    mentions |= mentions_tracked_atom(tracked_binding, test)
                        || mentions_tracked_var(tracked_binding, consequent)
                        || mentions_tracked_var(tracked_binding, alternative)
                        || consequent_args
                            .iter()
                            .any(|args| atoms_mention_tracked_binding(tracked_binding, *args))
                        || alternative_args
                            .iter()
                            .any(|args| atoms_mention_tracked_binding(tracked_binding, *args));
                    break;
                }
            }
        }
    }

    (size, mentions)
}

fn inline_t<'gc>(
    state: State<'gc>,
    term: TermRef<'gc>,
    cnt_limit: usize,
    tracked_binding: Option<LVarRef<'gc>>,
) -> RewriteResult<'gc> {
    inline_t_inner(state, term, cnt_limit, tracked_binding)
}

struct PendingInlineLet<'gc> {
    original: TermRef<'gc>,
    name: LVarRef<'gc>,
    exp: Expression<'gc>,
    mentions_tracked_binding: bool,
}

fn inline_t_inner<'gc>(
    state: State<'gc>,
    mut term: TermRef<'gc>,
    cnt_limit: usize,
    tracked_binding: Option<LVarRef<'gc>>,
) -> RewriteResult<'gc> {
    let mut lets = Vec::new();
    let mut changed = false;

    loop {
        match *term {
            Term::Let(name, prev_exp, prev_body) => {
                let exp = match prev_exp {
                    Expression::PrimCall(prim, prev_args, src) => {
                        let (args, args_changed) = substitute_atoms_array(&state, prev_args);
                        changed |= args_changed;
                        Expression::PrimCall(prim, args, src)
                    }
                };
                lets.push(PendingInlineLet {
                    original: term,
                    name,
                    mentions_tracked_binding: match exp {
                        Expression::PrimCall(_, args, _) => {
                            atoms_mention_tracked_binding(tracked_binding, args)
                        }
                    },
                    exp,
                });
                term = prev_body;
            }

            _ => {
                let ctx = state.ctx;
                let mut result = inline_non_let(state, term, cnt_limit, tracked_binding);
                result.changed |= changed;
                result.size += lets.len();
                for pending in lets.iter() {
                    result.mentions_tracked_binding |= pending.mentions_tracked_binding;
                }

                if !result.changed {
                    if let Some(first) = lets.first() {
                        result.term = first.original;
                    }
                    return result;
                }

                let mut body = result.term;
                for pending in lets.into_iter().rev() {
                    body = Gc::new(*ctx, Term::Let(pending.name, pending.exp, body));
                }
                result.term = body;
                return result;
            }
        }
    }
}

fn inline_non_let<'gc>(
    state: State<'gc>,
    term: TermRef<'gc>,
    cnt_limit: usize,
    tracked_binding: Option<LVarRef<'gc>>,
) -> RewriteResult<'gc> {
    let loop_limit = LOOP_UNROLL[cnt_limit];
    let fun_limit = FIBONACCI[cnt_limit];

    match *term {
        Term::Let(..) => inline_t(state, term, cnt_limit, tracked_binding),

        Term::Letk(prev_conts, prev_body) => {
            let mut changed = false;
            let mut size = 0;
            let mut mentions = false;
            let mut i_ks = Vec::with_capacity(prev_conts.len());
            let mut to_inline = Vec::new();

            for cnt in prev_conts.iter().copied() {
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
                if !dont {
                    to_inline.push(newk);
                }
                i_ks.push(newk);
            }

            let ctx = state.ctx;
            let s = state.with_continuations(&to_inline);
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
            let mut i_fs = Vec::with_capacity(prev_funcs.len());
            let mut to_inline = Vec::new();

            for func in prev_funcs.iter().copied() {
                let body = inline_t(state.clone(), func.body(), cnt_limit, Some(func.binding));
                changed |= body.changed;
                size += body.size;
                mentions |= body.mentions_tracked_binding;
                let newf = if body.changed {
                    func.with_body(state.ctx, body.term)
                } else {
                    func
                };

                let my_size = body.size;
                let dont =
                    my_size > fun_limit || (my_size > loop_limit && body.mentions_tracked_binding);

                if !dont {
                    to_inline.push(newf);
                }
                i_fs.push(newf);
            }

            let ctx = state.ctx;
            let s = state.with_functions(&to_inline);
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
            let (args, args_changed) = substitute_atoms_array(&state, prev_args);
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

                        let copy = copy_t_with_metadata(
                            state.ctx,
                            new_cnt.body(),
                            &mut subst,
                            &mut HashMap::default(),
                            tracked_binding,
                        );
                        return RewriteResult {
                            term: copy.term,
                            changed: true,
                            size: copy.size,
                            mentions_tracked_binding: copy.mentions_tracked_binding,
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

            let (args, args_changed) = substitute_atoms_array(&state, prev_args);

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

                        let copy = copy_t_with_metadata(
                            state.ctx,
                            func.body(),
                            &mut subv,
                            &mut subc,
                            tracked_binding,
                        );
                        return RewriteResult {
                            term: copy.term,
                            changed: true,
                            size: copy.size,
                            mentions_tracked_binding: copy.mentions_tracked_binding,
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
            let (consequent_args, consequent_args_changed) = match prev_consequent_args {
                Some(args) => {
                    let (args, changed) = substitute_atoms_array(&state, args);
                    (Some(args), changed)
                }
                None => (None, false),
            };
            let (alternative_args, alternative_args_changed) = match prev_alternative_args {
                Some(args) => {
                    let (args, changed) = substitute_atoms_array(&state, args);
                    (Some(args), changed)
                }
                None => (None, false),
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
        let shrink = shrink_with_size(ctx, next.term);
        let next_term = shrink.term;
        let shrink_changed = shrink.changed;
        let inline_changed = next.changed;
        changed |= inline_changed || shrink_changed;
        term_size = if shrink_changed {
            shrink.size
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
    let mut size = 0;
    let mut stack = vec![term];

    while let Some(mut term) = stack.pop() {
        loop {
            match *term {
                Term::Let(_, _, body) => {
                    size += 1;
                    term = body;
                }

                Term::Letk(conts, body) => {
                    for cont in conts.iter() {
                        stack.push(cont.body());
                    }
                    term = body;
                }

                Term::Fix(funcs, body) => {
                    for func in funcs.iter() {
                        stack.push(func.body());
                    }
                    term = body;
                }

                _ => {
                    size += 1;
                    break;
                }
            }
        }
    }

    size
}

/// Given a list of elements, emit a CPS list construction.
fn materialize_list<'gc, 'a>(
    state: State<'gc>,
    elements: &[Atom<'gc>],
    fk: Box<dyn FnOnce(State<'gc>, Atom<'gc>) -> TermRef<'gc> + 'a>,
) -> TermRef<'gc> {
    if elements.is_empty() {
        return fk(state, Atom::Constant(Value::null()));
    }

    let ctx = state.ctx;
    let vars = (0..elements.len())
        .map(|_| fresh_lvar(ctx, ctx.intern("&ls")))
        .collect::<Vec<_>>();
    let mut body = fk(state, Atom::Local(vars[0]));

    for (index, (&element, &var)) in elements.iter().zip(vars.iter()).enumerate() {
        let tail = vars
            .get(index + 1)
            .copied()
            .map(Atom::Local)
            .unwrap_or_else(|| Atom::Constant(Value::null()));
        body = Gc::new(
            *ctx,
            Term::Let(
                var,
                Expression::PrimCall(
                    sym_cons(ctx).into(),
                    Array::from_slice(*ctx, [element, tail]),
                    Value::new(false),
                ),
                body,
            ),
        );
    }

    body
}

#[cfg(test)]
pub(crate) fn copy_for_stress_test<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
) -> (TermRef<'gc>, usize, bool) {
    let copy = copy_t_with_metadata(
        ctx,
        term,
        &mut HashMap::default(),
        &mut HashMap::default(),
        None,
    );
    (copy.term, copy.size, copy.mentions_tracked_binding)
}

static SIDE_EFFECT_FREE_OPS: LazyLock<std::collections::HashSet<&'static str>> =
    LazyLock::new(|| {
        std::collections::HashSet::from(["variable-ref", "cons", "list", "make-variable", "vector"])
    });
