#![allow(dead_code, unused_variables)]
use crate::{
    cps::{
        fold::folding_table,
        term::{Atom, Atoms, Cont, ContRef, Expression, Func, FuncRef, Term, TermRef, Throw},
    },
    expander::{core::LVarRef, primitives::primitives},
    runtime::{Context, value::Value},
    utils::fixedpoint,
};
use rsgc::{Gc, alloc::array::Array};
use std::collections::{HashMap, HashSet};

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Count {
    applied: u32,
    as_value: u32,
}

struct State<'gc> {
    ctx: Context<'gc>,
    census: HashMap<LVarRef<'gc>, Count>,
    atom_subst: HashMap<Atom<'gc>, Atom<'gc>>,
    var_subst: HashMap<LVarRef<'gc>, LVarRef<'gc>>,

    e_inv_env: HashMap<(Value<'gc>, Atoms<'gc>), Atom<'gc>>,
    cenv: HashMap<LVarRef<'gc>, ContRef<'gc>>,
    fenv: HashMap<LVarRef<'gc>, FuncRef<'gc>>,

    aenv: HashSet<LVarRef<'gc>>,
}

impl<'gc> State<'gc> {
    fn new(ctx: Context<'gc>, census: HashMap<LVarRef<'gc>, Count>) -> Self {
        Self {
            ctx,
            census,
            atom_subst: HashMap::new(),
            var_subst: HashMap::new(),
            e_inv_env: HashMap::new(),
            cenv: HashMap::new(),
            fenv: HashMap::new(),
            aenv: HashSet::new(),
        }
    }

    fn is_dead(&self, var: LVarRef<'gc>) -> bool {
        !self.census.contains_key(&var)
    }

    fn applied_once(&self, var: LVarRef<'gc>) -> bool {
        self.census
            .get(&var)
            .map_or(false, |count| count.applied == 1)
    }

    fn with_atom_subst(&mut self, from: Atom<'gc>, to: Atom<'gc>) -> &mut Self {
        self.atom_subst.insert(from, to);
        self
    }

    fn with_var_to_atom_subst(&mut self, from: LVarRef<'gc>, to: Atom<'gc>) -> &mut Self {
        self.with_atom_subst(Atom::Local(from), to);
        self
    }

    fn with_c_subst(&mut self, from: LVarRef<'gc>, to: LVarRef<'gc>) -> &mut Self {
        self.var_subst.insert(from, to);
        self
    }

    fn with_vars_to_atoms(&mut self, from: &[LVarRef<'gc>], to: &[Atom<'gc>]) -> &mut Self {
        assert_eq!(from.len(), to.len());
        for (f, t) in from.iter().zip(to.iter()) {
            self.with_var_to_atom_subst(*f, *t);
        }
        self
    }

    fn with_var_to_literal_subst(&mut self, from: LVarRef<'gc>, to: Value<'gc>) -> &mut Self {
        self.with_atom_subst(Atom::Local(from), Atom::Constant(to));
        self
    }

    fn with_exp(&mut self, atom: Atom<'gc>, prim: Value<'gc>, atoms: Atoms<'gc>) -> &mut Self {
        self.e_inv_env.insert((prim, atoms), atom);
        self
    }

    fn with_continuations(&mut self, conts: &[ContRef<'gc>]) -> &mut Self {
        for cont in conts {
            match &**cont {
                Cont::Local { binding, .. } | Cont::Return(binding) => {
                    self.cenv.insert(*binding, cont.clone());
                }
            }
        }
        self
    }

    fn with_functions(&mut self, funcs: &[FuncRef<'gc>]) -> &mut Self {
        for func in funcs {
            self.fenv.insert(func.binding, *func);
        }
        self
    }

    fn substitute_atoms<'a>(
        &'a self,
        atoms: impl Iterator<Item = Atom<'gc>> + 'a,
    ) -> impl Iterator<Item = Atom<'gc>> + 'a {
        atoms.map(|atom| self.atom_subst.get(&atom).cloned().unwrap_or(atom))
    }
}

fn census<'gc>(term: TermRef<'gc>) -> HashMap<LVarRef<'gc>, Count> {
    let mut census = HashMap::new();
    let mut rhs = HashMap::new();

    fn inc_app_use_n<'gc>(
        name: LVarRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
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
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        if let Atom::Local(name) = atom {
            inc_app_use_n(name, census, rhs);
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
        } else if let Atom::Values(values) = atom {
            for &value in values.iter() {
                inc_val_use_a(value, census, rhs);
            }
        }
    }

    fn add_to_census<'gc>(
        term: TermRef<'gc>,
        census: &mut HashMap<LVarRef<'gc>, Count>,
        rhs: &mut HashMap<LVarRef<'gc>, TermRef<'gc>>,
    ) {
        match *term {
            Term::Throw(throw, _) => match throw {
                Throw::Throw(key, args) => {
                    inc_val_use_a(key, census, rhs);
                    inc_val_use_a(args, census, rhs);
                }

                Throw::Value(val, subr_and_message) => {
                    inc_val_use_a(val, census, rhs);
                    inc_val_use_a(subr_and_message, census, rhs);
                }

                Throw::ValueAndData(val, subr_and_message) => {
                    inc_val_use_a(val, census, rhs);
                    inc_val_use_a(subr_and_message, census, rhs);
                }
            },

            Term::Let(_, expr, body) => {
                match expr {
                    Expression::PrimCall(_, args, h, _) => {
                        for arg in args.iter() {
                            inc_val_use_a(*arg, census, rhs);
                        }

                        inc_val_use_n(h, census, rhs);
                    }

                    Expression::ValuesAt(atom, _) => {
                        inc_val_use_a(atom, census, rhs);
                    }

                    Expression::ValuesRest(atom, _) => {
                        inc_val_use_a(atom, census, rhs);
                    }
                }

                add_to_census(body, census, rhs);
            }

            Term::Letk(ks, body) => {
                for k in ks.iter() {
                    rhs.insert(k.binding(), k.body().expect("letk body"));
                }

                add_to_census(body, census, rhs);
            }

            Term::Fix(funcs, body) => {
                for fun in funcs.iter() {
                    rhs.insert(fun.binding, fun.body);
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

            Term::If(test, then_c, else_c, ..) => {
                inc_val_use_a(test, census, rhs);
                inc_val_use_n(then_c, census, rhs);
                inc_val_use_n(else_c, census, rhs);
            }
        }
    }

    add_to_census(term, &mut census, &mut rhs);

    census
}

fn shrink_tree<'gc>(term: TermRef<'gc>, state: &mut State<'gc>) -> TermRef<'gc> {
    match *term {
        Term::Let(binding, expr, body) => match expr {
            Expression::PrimCall(prim, args, h, source) => {
                let prim_def = primitives(state.ctx)
                    .set
                    .get(&prim)
                    .expect(&format!("BUG: unknown primitive {}", prim));

                let args = state
                    .substitute_atoms(args.iter().copied())
                    .collect::<Vec<_>>();
                let h = state.var_subst.get(&h).copied().unwrap_or(h);

                if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
                    if let Some(atom) = folding_table(state.ctx).try_fold(state.ctx, prim, &args) {
                        let state =
                            state.with_atom_subst(Atom::Local(binding), Atom::Constant(atom));
                        return shrink_tree(body, state);
                    } else {
                        println!("can't fold {}", prim);
                    }
                }

                let atoms = Array::from_array(&state.ctx, args);
                let body = shrink_tree(body, state);

                if !prim_def.is_impure() && state.is_dead(binding) {
                    return body;
                }

                Gc::new(
                    &state.ctx,
                    Term::Let(binding, Expression::PrimCall(prim, atoms, h, source), body),
                )
            }

            Expression::ValuesAt(atom, ix) => {
                let atom = state.atom_subst.get(&atom).cloned().unwrap_or(atom);
                if let Atom::Values(values) = atom {
                    if let Some(&value) = values.get(ix) {
                        let state = state.with_atom_subst(Atom::Local(binding), value);
                        return shrink_tree(body, state);
                    }
                }

                let body = shrink_tree(body, state);
                return Gc::new(
                    &state.ctx,
                    Term::Let(binding, Expression::ValuesAt(atom, ix), body),
                );
            }

            Expression::ValuesRest(atom, from) => {
                let atom = state.atom_subst.get(&atom).cloned().unwrap_or(atom);
                let body = shrink_tree(body, state);
                return Gc::new(
                    &state.ctx,
                    Term::Let(binding, Expression::ValuesRest(atom, from), body),
                );
            }
        },

        Term::Letk(conts, body) => {
            let conts = conts
                .iter()
                .filter_map(|cont| {
                    if state.is_dead(cont.binding()) {
                        return None;
                    }

                    let Cont::Local {
                        name,
                        binding,
                        body,
                        args,
                        variadic,
                        source,
                        reified,
                    } = **cont
                    else {
                        panic!("BUG: invalid letk")
                    };

                    let body = shrink_tree(body, state);

                    Some(Gc::new(
                        &state.ctx,
                        Cont::Local {
                            name,
                            binding,
                            body,
                            args,
                            variadic,
                            source,
                            reified,
                        },
                    ))
                })
                .collect::<Vec<_>>();
            if conts.is_empty() {
                return shrink_tree(body, state);
            }

            let conts = Array::from_array(&state.ctx, conts);
            let body = shrink_tree(body, state);
            Gc::new(&state.ctx, Term::Letk(conts, body))
        }

        Term::Fix(funcs, body) => {
            let funcs = funcs
                .iter()
                .copied()
                .filter_map(|func| {
                    if state.is_dead(func.binding) {
                        return None;
                    }

                    let body = shrink_tree(func.body, state);
                    Some(func.with_body(state.ctx, body))
                })
                .collect::<Vec<_>>();

            if funcs.is_empty() {
                return shrink_tree(body, state);
            }

            let funcs = Array::from_array(&state.ctx, funcs);
            let body = shrink_tree(body, state);
            Gc::new(&state.ctx, Term::Fix(funcs, body))
        }

        Term::Continue(k_prev, args_prev, src) => {
            let k = state.var_subst.get(&k_prev).copied().unwrap_or(k_prev);
            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();

            let args = Array::from_array(&state.ctx, args);
            if let Some(k) = state.cenv.get(&k).copied()
                && k.args().len() == args.len()
                && k.variadic().is_none()
            {
                let state = state.with_vars_to_atoms(&k.args(), &args);

                shrink_tree(k.body().unwrap(), state)
            } else {
                Gc::new(&state.ctx, Term::Continue(k, args, src))
            }
        }

        Term::App(fun, retc_prev, rete_prev, args_prev, span) => {
            let retc = state
                .var_subst
                .get(&retc_prev)
                .copied()
                .unwrap_or(retc_prev);
            let rete = state
                .var_subst
                .get(&rete_prev)
                .copied()
                .unwrap_or(rete_prev);

            let args = state
                .substitute_atoms(args_prev.iter().copied())
                .collect::<Vec<_>>();
            let args = Array::from_array(&state.ctx, args);

            let fun = state.atom_subst.get(&fun).cloned().unwrap_or(fun);

            match fun {
                Atom::Local(fun) if state.fenv.contains_key(&fun) => {
                    let fun = state.fenv[&fun];
                    if fun.args.len() == args.len() && fun.variadic.is_none() {
                        let state = state
                            .with_vars_to_atoms(&fun.args, &args)
                            .with_c_subst(fun.return_cont, retc);

                        return shrink_tree(fun.body, state);
                    }
                }

                _ => {}
            }

            return Gc::new(&state.ctx, Term::App(fun, retc, rete, args, span));
        }

        Term::If(test, kcons, kalt, hints) => {
            let test = state.atom_subst.get(&test).cloned().unwrap_or(test);

            if let Atom::Constant(test) = test {
                let k = if test.as_bool() { kcons } else { kalt };

                return Gc::new(
                    &state.ctx,
                    Term::Continue(k, Array::from_array(&state.ctx, []), Value::new(false)),
                );
            }

            let kcons = state.var_subst.get(&kcons).copied().unwrap_or(kcons);
            let kalt = state.var_subst.get(&kalt).copied().unwrap_or(kalt);

            Gc::new(&state.ctx, Term::If(test, kcons, kalt, hints))
        }

        Term::Throw(throw, src) => match throw {
            Throw::Throw(key, args) => {
                let key = state.atom_subst.get(&key).cloned().unwrap_or(key);
                let args = state.atom_subst.get(&args).cloned().unwrap_or(args);
                Gc::new(&state.ctx, Term::Throw(Throw::Throw(key, args), src))
            }

            Throw::Value(val, subr_and_message) => {
                let val = state.atom_subst.get(&val).cloned().unwrap_or(val);
                let subr_and_message = state
                    .atom_subst
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(
                    &state.ctx,
                    Term::Throw(Throw::Value(val, subr_and_message), src),
                )
            }

            Throw::ValueAndData(val, subr_and_message) => {
                let val = state.atom_subst.get(&val).cloned().unwrap_or(val);
                let subr_and_message = state
                    .atom_subst
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(
                    &state.ctx,
                    Term::Throw(Throw::ValueAndData(val, subr_and_message), src),
                )
            }
        },
    }
}

pub fn shrink<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let census = census(term);
    let mut state = State::new(ctx, census);

    shrink_tree(term, &mut state)
}

pub fn rewrite<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    let simplified_tree = fixedpoint(term, None)(|term| shrink(ctx, *term));
    let max_size = size(simplified_tree) * 3 / 2;
    fixedpoint(simplified_tree, Some(8))(|term| inline(ctx, *term, max_size))
}

pub fn rewrite_func<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> FuncRef<'gc> {
    let body = rewrite(ctx, func.body);
    func.with_body(ctx, body)
}

const FIBONACCI: &[usize] = &[1, 2, 3, 5, 8, 13, 1000];

const LOOP_UNROLL: &[usize] = &[1, 2, 4, 5, 6, 7, 1000];

fn copy_t<'gc>(
    ctx: Context<'gc>,
    term: TermRef<'gc>,
    subv: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    subc: &mut HashMap<LVarRef<'gc>, LVarRef<'gc>>,
) -> TermRef<'gc> {
    match *term {
        Term::Let(binding, expr, body) => match expr {
            Expression::PrimCall(prim, args, h, source) => {
                let binding1 = binding.copy(ctx);
                let args = args
                    .iter()
                    .map(|a| subv.get(a).cloned().unwrap_or(*a))
                    .collect::<Vec<_>>();
                let atoms = Array::from_array(&ctx, args);
                subv.insert(Atom::Local(binding), Atom::Local(binding1));
                let h = subc.get(&h).copied().unwrap_or_else(|| h);
                let body = copy_t(ctx, body, subv, subc);
                Gc::new(
                    &ctx,
                    Term::Let(binding1, Expression::PrimCall(prim, atoms, h, source), body),
                )
            }

            Expression::ValuesAt(atom, from) => {
                let binding1 = binding.copy(ctx);
                let atom = subv.get(&atom).cloned().unwrap_or(atom);
                subv.insert(Atom::Local(binding), Atom::Local(binding1));
                let body = copy_t(ctx, body, subv, subc);

                Gc::new(
                    &ctx,
                    Term::Let(binding1, Expression::ValuesAt(atom, from), body),
                )
            }

            Expression::ValuesRest(atom, from) => {
                let binding1 = binding.copy(ctx);
                let atom = subv.get(&atom).cloned().unwrap_or(atom);
                subv.insert(Atom::Local(binding), Atom::Local(binding1));
                let body = copy_t(ctx, body, subv, subc);

                Gc::new(
                    &ctx,
                    Term::Let(binding1, Expression::ValuesRest(atom, from), body),
                )
            }
        },

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

            let conts = Array::from_array(&ctx, conts);

            let body = copy_t(ctx, body, subv, &mut subc1);

            Gc::new(&ctx, Term::Letk(conts, body))
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
            let funcs = Array::from_array(&ctx, funcs);

            let body = copy_t(ctx, body, &mut subv1, subc);

            Gc::new(&ctx, Term::Fix(funcs, body))
        }

        Term::Continue(cont, args, src) => {
            let args = args
                .iter()
                .map(|a| subv.get(a).cloned().unwrap_or(*a))
                .collect::<Vec<_>>();
            let args = Array::from_array(&ctx, args);

            let cont = subc.get(&cont).copied().unwrap_or(cont);

            Gc::new(&ctx, Term::Continue(cont, args, src))
        }

        Term::Throw(throw, src) => match throw {
            Throw::Throw(key, args) => {
                let key = subv.get(&key).cloned().unwrap_or(key);
                let args = subv.get(&args).cloned().unwrap_or(args);

                Gc::new(&ctx, Term::Throw(Throw::Throw(key, args), src))
            }

            Throw::Value(val, subr_and_message) => {
                let val = subv.get(&val).cloned().unwrap_or(val);
                let subr_and_message = subv
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(&ctx, Term::Throw(Throw::Value(val, subr_and_message), src))
            }

            Throw::ValueAndData(val, subr_and_message) => {
                let val = subv.get(&val).cloned().unwrap_or(val);
                let subr_and_message = subv
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(
                    &ctx,
                    Term::Throw(Throw::ValueAndData(val, subr_and_message), src),
                )
            }
        },

        Term::App(fun, retc, rete, args, src) => {
            let fun = subv.get(&fun).cloned().unwrap_or_else(|| fun);

            let retc = subc.get(&retc).copied().unwrap_or(retc);
            let rete = subc.get(&rete).copied().unwrap_or(rete);
            let args = args
                .iter()
                .map(|a| subv.get(a).cloned().unwrap_or(*a))
                .collect::<Vec<_>>();
            let args = Array::from_array(&ctx, args);

            Gc::new(&ctx, Term::App(fun, retc, rete, args, src))
        }

        Term::If(test, kcons, kalt, hints) => {
            let test = subv.get(&test).cloned().unwrap_or(test);
            let kcons = subc.get(&kcons).copied().unwrap_or(kcons);
            let kalt = subc.get(&kalt).copied().unwrap_or(kalt);

            Gc::new(&ctx, Term::If(test, kcons, kalt, hints))
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

    let binding = subc[&cont.binding()];
    let args1 = Array::from_array(&ctx, args1);
    let body = copy_t(ctx, cont.body().unwrap(), subv1, subc);
    Gc::new(
        &ctx,
        Cont::Local {
            name: cont.name(),
            binding,
            body,
            args: args1,
            variadic: var1,
            source: cont.source(),
            reified: cont.is_reified(),
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

    let body = copy_t(ctx, fun.body, subv1, subc1);

    Gc::new(
        &ctx,
        Func {
            return_cont: retc1,
            handler_cont: rete1,
            name: fun.name,
            binding,
            args: Array::from_array(&ctx, args1),
            variadic: var1,
            body,
            source: fun.source,
        },
    )
}

fn inline_t<'gc>(state: &mut State<'gc>, term: TermRef<'gc>, cnt_limit: usize) -> TermRef<'gc> {
    let loop_limit = LOOP_UNROLL[cnt_limit];
    let fun_limit = FIBONACCI[cnt_limit];

    match *term {
        Term::Let(name, exp, body) => {
            let body = inline_t(state, body, cnt_limit);
            Gc::new(&state.ctx, Term::Let(name, exp, body))
        }

        Term::Letk(conts, body) => {
            let conts = conts.iter().copied().map(|cnt| {
                let Cont::Local {
                    name,
                    binding,
                    body,
                    args,
                    variadic,
                    source,
                    reified,
                } = *cnt
                else {
                    panic!("BUG: expected local continuation");
                };
                let orig = body;
                let body = inline_t(state, body, cnt_limit);
                let newk = Gc::new(
                    &state.ctx,
                    Cont::Local {
                        name,
                        binding,
                        body,
                        args,
                        variadic,
                        source,
                        reified,
                    },
                );

                let my_size = size(orig);
                let dont = my_size > cnt_limit
                    || (my_size > loop_limit && census(body).contains_key(&binding));
                (newk, if dont { None } else { Some(newk) })
            });

            let (i_ks, to_inline): (Vec<_>, Vec<_>) = conts.unzip();

            let s =
                state.with_continuations(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, body, cnt_limit);
            let i_ks = Array::from_array(&state.ctx, i_ks);
            Gc::new(&state.ctx, Term::Letk(i_ks, body))
        }

        Term::Fix(funcs, body) => {
            let funcs = funcs
                .iter()
                .copied()
                .map(|func| {
                    let nbody = inline_t(state, func.body, cnt_limit);
                    let newf = Gc::new(
                        &state.ctx,
                        Func {
                            return_cont: func.return_cont,
                            handler_cont: func.handler_cont,
                            name: func.name,
                            binding: func.binding,
                            args: func.args,
                            variadic: func.variadic,
                            body: nbody,
                            source: func.source,
                        },
                    );

                    let my_size = size(func.body);
                    let dont = my_size > fun_limit
                        || (my_size > loop_limit && census(nbody).contains_key(&func.binding));

                    (newf, if dont { None } else { Some(newf) })
                })
                .collect::<Vec<_>>();

            let (i_fs, to_inline): (Vec<_>, Vec<_>) = funcs.into_iter().unzip();
            let s = state.with_functions(&to_inline.iter().copied().flatten().collect::<Vec<_>>());
            let body = inline_t(s, body, cnt_limit);
            let ifs = Array::from_array(&state.ctx, i_fs);
            Gc::new(&state.ctx, Term::Fix(ifs, body))
        }

        Term::Continue(cnt, args, src) => {
            let args = state
                .substitute_atoms(args.iter().copied())
                .collect::<Vec<_>>();
            let args = Array::from_array(&state.ctx, args);

            if let Atom::Local(cnt) = state
                .atom_subst
                .get(&Atom::Local(cnt))
                .cloned()
                .unwrap_or(Atom::Local(cnt))
            {
                if let Some(new_cnt) = state.cenv.get(&cnt).cloned() {
                    if new_cnt.args().len() == args.len() && new_cnt.variadic().is_none() {
                        let mut subst = new_cnt
                            .args()
                            .iter()
                            .zip(args.iter())
                            .map(|(arg, val)| (Atom::Local(*arg), *val))
                            .collect::<HashMap<_, _>>();
                        assert!(!matches!(*new_cnt, Cont::Return(_)));
                        return copy_t(
                            state.ctx,
                            new_cnt.body().unwrap(),
                            &mut subst,
                            &mut HashMap::new(),
                        );
                    }
                }
            }

            Gc::new(&state.ctx, Term::Continue(cnt, args, src))
        }
        Term::Throw(throw, src) => match throw {
            Throw::Throw(key, args) => {
                let key = state.atom_subst.get(&key).cloned().unwrap_or(key);
                let args = state.atom_subst.get(&args).cloned().unwrap_or(args);
                Gc::new(&state.ctx, Term::Throw(Throw::Throw(key, args), src))
            }

            Throw::Value(val, subr_and_message) => {
                let val = state.atom_subst.get(&val).cloned().unwrap_or(val);
                let subr_and_message = state
                    .atom_subst
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(
                    &state.ctx,
                    Term::Throw(Throw::Value(val, subr_and_message), src),
                )
            }

            Throw::ValueAndData(val, subr_and_message) => {
                let val = state.atom_subst.get(&val).cloned().unwrap_or(val);
                let subr_and_message = state
                    .atom_subst
                    .get(&subr_and_message)
                    .cloned()
                    .unwrap_or(subr_and_message);
                Gc::new(
                    &state.ctx,
                    Term::Throw(Throw::ValueAndData(val, subr_and_message), src),
                )
            }
        },

        Term::App(fun, ret_cp, ret_ep, args, src) => {
            let retc = state.var_subst.get(&ret_cp).copied().unwrap_or(ret_cp);
            let rete = state.var_subst.get(&ret_ep).copied().unwrap_or(ret_ep);
            let args = state
                .substitute_atoms(args.iter().copied())
                .collect::<Vec<_>>();
            let args = Array::from_array(&state.ctx, args);

            if let Atom::Local(new_fun) = state.atom_subst.get(&fun).copied().unwrap_or(fun) {
                if let Some(func) = state.fenv.get(&new_fun) {
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

                        return copy_t(state.ctx, func.body, &mut subv, &mut subc);
                    }
                }
            }

            Gc::new(&state.ctx, Term::App(fun, retc, rete, args, src))
        }

        Term::If(test, kcons, kalt, hints) => {
            let test = state.atom_subst.get(&test).cloned().unwrap_or(test);
            let kcons = state.var_subst.get(&kcons).copied().unwrap_or(kcons);
            let kalt = state.var_subst.get(&kalt).copied().unwrap_or(kalt);

            Gc::new(&state.ctx, Term::If(test, kcons, kalt, hints))
        }
    }
}

pub fn inline<'gc>(ctx: Context<'gc>, mut term: TermRef<'gc>, max_size: usize) -> TermRef<'gc> {
    for i in 0..FIBONACCI.len() {
        if size(term) > max_size {
            return term;
        }

        let mut state = State::new(ctx, census(term));

        term = fixedpoint(inline_t(&mut state, term, i), None)(|term| shrink(ctx, *term));
    }

    term
}

/// Given a term, returns the size of it.
pub fn size<'gc>(term: TermRef<'gc>) -> usize {
    match *term {
        Term::Let(_, _, body) => 1 + size(body),
        Term::Letk(conts, body) => {
            conts.iter().map(|c| size(c.body().unwrap())).sum::<usize>() + size(body)
        }

        Term::Fix(funcs, body) => funcs.iter().map(|f| size(f.body)).sum::<usize>() + size(body),

        _ => 1,
    }
}
