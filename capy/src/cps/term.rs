use std::{cell::Cell, hash::Hash};

use crate::rsgc::{
    Gc, Trace,
    alloc::{Array, array::ArrayRef},
    barrier,
    cell::Lock,
};

use crate::{
    expander::core::{LVar, LVarRef},
    runtime::{Context, value::Value},
    utils::TreeEq,
};

pub type Atoms<'gc> = ArrayRef<'gc, Atom<'gc>>;
pub type Vars<'gc> = ArrayRef<'gc, LVarRef<'gc>>;
pub type Conts<'gc> = ArrayRef<'gc, ContRef<'gc>>;
pub type Funcs<'gc> = ArrayRef<'gc, FuncRef<'gc>>;

#[derive(Debug, Clone, Trace, Copy, PartialEq, Eq, Hash)]
#[collect(no_drop)]
pub enum Atom<'gc> {
    Constant(Value<'gc>),
    Local(LVarRef<'gc>),
}

impl<'gc> std::fmt::Display for Atom<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Local(l) => write!(f, "{}:{}", l.name, l.id),
        }
    }
}

#[derive(Debug, Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum Term<'gc> {
    Continue(LVarRef<'gc>, Atoms<'gc>, Value<'gc>),

    App(
        Atom<'gc>,
        LVarRef<'gc>,
        LVarRef<'gc>,
        Atoms<'gc>,
        Value<'gc>,
    ),

    If {
        test: Atom<'gc>,
        consequent: LVarRef<'gc>,
        consequent_args: Option<Atoms<'gc>>,
        alternative: LVarRef<'gc>,
        alternative_args: Option<Atoms<'gc>>,
        hints: [BranchHint; 2],
    },

    Letk(Conts<'gc>, TermRef<'gc>),
    Fix(Funcs<'gc>, TermRef<'gc>),
    Let(LVarRef<'gc>, Expression<'gc>, TermRef<'gc>),
}

impl<'gc, T: Into<Value<'gc>>> From<T> for Atom<'gc> {
    fn from(value: T) -> Self {
        Atom::Constant(Value::new(value))
    }
}

impl<'gc> Into<Atom<'gc>> for LVarRef<'gc> {
    fn into(self) -> Atom<'gc> {
        Atom::Local(self)
    }
}

#[derive(Debug, Clone, Trace, Copy, PartialEq, Eq)]
#[collect(no_drop)]
pub enum Expression<'gc> {
    PrimCall(Value<'gc>, Atoms<'gc>, LVarRef<'gc>, Value<'gc>),
}

pub type TermRef<'gc> = Gc<'gc, Term<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Func<'gc> {
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub binding: LVarRef<'gc>,
    /// Continuation to return to
    pub return_cont: LVarRef<'gc>,
    /// Exception handler continuation
    pub handler_cont: LVarRef<'gc>,

    pub args: Vars<'gc>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: Lock<TermRef<'gc>>,

    pub free_vars: Lock<Option<Vars<'gc>>>,
    pub meta: Value<'gc>,
}

impl<'gc> Func<'gc> {
    pub fn with_body(self: FuncRef<'gc>, ctx: Context<'gc>, body: TermRef<'gc>) -> FuncRef<'gc> {
        let wfunc = Gc::write(&ctx, self);
        barrier::field!(wfunc, Self, body).unlock().set(body);
        self
    }

    pub fn body(&self) -> TermRef<'gc> {
        self.body.get()
    }
}

impl<'gc> PartialEq for Func<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl<'gc> Eq for Func<'gc> {}

impl<'gc> Hash for Func<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            let hashcode = Gc::ptr_hash(Gc::from_ptr(self));
            hashcode.hash(state);
        }
    }
}

pub type FuncRef<'gc> = Gc<'gc, Func<'gc>>;

#[derive(Debug, Clone, Trace)]
#[collect(no_drop)]
pub struct Cont<'gc> {
    pub name: Value<'gc>,
    pub binding: LVarRef<'gc>,
    pub args: Vars<'gc>,
    pub variadic: Option<LVarRef<'gc>>,

    pub body: Lock<TermRef<'gc>>,
    pub source: Value<'gc>,
    pub free_vars: Lock<Option<Vars<'gc>>>,
    pub reified: Cell<bool>,
    pub handler: Lock<LVarRef<'gc>>,
    pub cold: bool,
    pub noinline: bool,
    pub meta: Value<'gc>,
}

impl<'gc> PartialEq for Cont<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl<'gc> Eq for Cont<'gc> {}

impl<'gc> Hash for Cont<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            let hashcode = Gc::ptr_hash(Gc::from_ptr(self));
            hashcode.hash(state);
        }
    }
}

impl<'gc> Cont<'gc> {
    pub fn args(&self) -> Vars<'gc> {
        self.args
    }

    pub fn variadic(&self) -> Option<LVarRef<'gc>> {
        self.variadic
    }

    pub fn binding(&self) -> LVarRef<'gc> {
        self.binding
    }

    pub fn name(&self) -> Value<'gc> {
        self.name
    }

    pub fn body(&self) -> TermRef<'gc> {
        self.body.get()
    }

    pub fn with_body(
        self: ContRef<'gc>,
        ctx: Context<'gc>,
        body: TermRef<'gc>,
    ) -> Gc<'gc, Cont<'gc>> {
        if Gc::ptr_eq(self.body.get(), body) {
            return self;
        }

        let this = Gc::new(
            &ctx,
            Self {
                meta: self.meta,
                name: self.name,
                binding: self.binding,

                args: self.args,
                variadic: self.variadic,
                body: Lock::new(body),
                source: self.source,
                noinline: self.noinline,
                free_vars: Lock::new(self.free_vars.get()),
                reified: Cell::new(self.reified.get()),
                handler: Lock::new(self.handler.get()),
                cold: self.cold,
            },
        );

        this
    }

    pub fn arity_matches(&self, arg_count: usize) -> bool {
        if let Some(_) = self.variadic {
            arg_count >= self.args.len()
        } else {
            arg_count == self.args.len()
        }
    }
}

pub type ContRef<'gc> = Gc<'gc, Cont<'gc>>;

/// A branch hint given to `if` term in CPS code.
///
/// This is mainly used to generate efficient code for type-checks.
#[derive(Debug, Clone, Trace, Copy, PartialEq, Eq)]
pub enum BranchHint {
    /// Normal branch, block will be reordered as needed.
    Normal,
    /// Hot branch. This is equivalent to `likely` in C. At the moment
    /// is equivalent to `Normal`, but may be used in the future
    /// to generate more efficient code.
    Hot,
    /// Cold branch. This will try to move resulting block
    /// of code to the end of the function.
    Cold,
}

impl<'gc> TreeEq for Atom<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Constant(a), Atom::Constant(b)) => a.tree_eq(b),

            (Atom::Local(a), Atom::Local(b)) => Gc::ptr_eq(*a, *b),

            _ => false,
        }
    }
}

impl<'gc> TreeEq for LVar<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<'gc> TreeEq for Cont<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        self.binding().name == other.binding().name
            && self.args().tree_eq(&other.args())
            && self.variadic().tree_eq(&other.variadic())
            && self.body().tree_eq(&other.body())
    }
}

impl<'gc> TreeEq for Func<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.source == other.source
            && Gc::ptr_eq(self.binding, other.binding)
            && Gc::ptr_eq(self.return_cont, other.return_cont)
            && self.args.tree_eq(&other.args)
            && self.variadic.tree_eq(&other.variadic)
            && self.body().tree_eq(&other.body())
    }
}

impl<'gc> TreeEq for Term<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::App(f, k, h, args, _), Term::App(g, l, lh, bargs, _)) => {
                f.tree_eq(g) && Gc::ptr_eq(*k, *l) && Gc::ptr_eq(*h, *lh) && args.tree_eq(bargs)
            }
            (Term::Continue(k, args, _), Term::Continue(l, bargs, _)) => {
                Gc::ptr_eq(*k, *l) && args.tree_eq(bargs)
            }

            (
                Term::If {
                    test: f,
                    consequent: k,
                    consequent_args: args,
                    alternative: l,
                    alternative_args: bargs,
                    hints,
                },
                Term::If {
                    test: g,
                    consequent: m,
                    consequent_args: cargs,
                    alternative: n,
                    alternative_args: dargs,
                    hints: bhints,
                },
            ) => {
                f.tree_eq(g)
                    && Gc::ptr_eq(*k, *m)
                    && Gc::ptr_eq(*l, *n)
                    && args.tree_eq(cargs)
                    && bargs.tree_eq(dargs)
                    && hints == bhints
            }
            (Term::Letk(ks, body), Term::Letk(ls, bbody)) => ks.tree_eq(ls) && body.tree_eq(bbody),
            (Term::Fix(fs, body), Term::Fix(ls, bbody)) => fs.tree_eq(ls) && body.tree_eq(bbody),
            (Term::Let(k, expr, body), Term::Let(l, bexpr, bbody)) => {
                Gc::ptr_eq(*k, *l) && expr.tree_eq(bexpr) && body.tree_eq(bbody)
            }

            _ => false,
        }
    }
}

impl<'gc> TreeEq for Expression<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::PrimCall(f, args, h, _), Expression::PrimCall(g, bargs, gh, _)) => {
                f.tree_eq(g) && args.tree_eq(bargs) && Gc::ptr_eq(*h, *gh)
            }
        }
    }
}

impl<'gc> Term<'gc> {
    pub fn source(&self) -> Value<'gc> {
        match self {
            Term::Continue(_, _, source) | Term::App(_, _, _, _, source) => *source,
            Term::If { .. } => Value::new(false),
            Term::Letk(_, body) | Term::Fix(_, body) | Term::Let(_, _, body) => body.source(),
        }
    }
}

impl<'gc> Cont<'gc> {
    pub fn source(&self) -> Value<'gc> {
        self.source
    }

    pub fn make_meta(&self, ctx: Context<'gc>) -> Value<'gc> {
        Value::cons(ctx, self.name, self.source)
    }
}

impl<'gc> Func<'gc> {
    pub fn source(&self) -> Value<'gc> {
        self.source
    }

    pub fn name(&self) -> Value<'gc> {
        self.name
    }

    pub fn binding(&self) -> LVarRef<'gc> {
        self.binding
    }

    pub fn return_cont(&self) -> LVarRef<'gc> {
        self.return_cont
    }

    pub fn make_meta(&self, ctx: Context<'gc>) -> Value<'gc> {
        Value::cons(ctx, self.name, self.source)
    }

    pub fn arity_matches(&self, arg_count: usize) -> bool {
        if let Some(_) = self.variadic {
            arg_count >= self.args.len()
        } else {
            arg_count == self.args.len()
        }
    }
}

impl<'gc> Term<'gc> {
    pub fn count_refs(&self) {
        match self {
            Term::Let(binding, exp, body) => {
                binding.ref_count.set(0);
                match exp {
                    Expression::PrimCall(_, args, _, _) => {
                        for arg in args.iter() {
                            arg.count_refs();
                        }
                    }
                }
                body.count_refs();
            }

            Term::Fix(funcs, body) => {
                for func in funcs.iter() {
                    func.binding.ref_count.set(0);
                }

                for func in funcs.iter() {
                    func.return_cont.ref_count.set(0);
                    func.handler_cont.ref_count.set(0);
                    for arg in func.args.iter().chain(func.variadic.iter()) {
                        arg.ref_count.set(0);
                    }
                    func.body().count_refs();
                }
                body.count_refs();
            }

            Term::Letk(conts, body) => {
                for cont in conts.iter() {
                    cont.binding.ref_count.set(0);
                }
                for cont in conts.iter() {
                    for arg in cont.args.iter().chain(cont.variadic.iter()) {
                        arg.ref_count.set(0);
                    }
                    cont.body().count_refs();
                }
                body.count_refs();
            }

            Term::App(func, k, h, args, _) => {
                func.count_refs();
                k.ref_count.set(k.ref_count.get() + 1);
                h.ref_count.set(h.ref_count.get() + 1);

                for arg in args.iter() {
                    arg.count_refs();
                }
            }

            Term::Continue(k, args, _) => {
                k.ref_count.set(k.ref_count.get() + 1);
                for arg in args.iter() {
                    arg.count_refs();
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
                test.count_refs();
                consequent.ref_count.set(consequent.ref_count.get() + 1);
                if let Some(args) = consequent_args {
                    for arg in args.iter() {
                        arg.count_refs();
                    }
                }
                alternative.ref_count.set(alternative.ref_count.get() + 1);
                if let Some(args) = alternative_args {
                    for arg in args.iter() {
                        arg.count_refs();
                    }
                }
                let _ = hints;
            }
        }
    }
}

impl<'gc> Atom<'gc> {
    pub fn count_refs(&self) {
        match self {
            Atom::Local(lvar) => {
                lvar.ref_count.set(lvar.ref_count.get() + 1);
            }
            Atom::Constant(_) => {}
        }
    }
}

pub fn pre_post_order<'gc, PRE, POST>(
    ctx: Context<'gc>,
    mut pre: PRE,
    mut post: POST,
    term: TermRef<'gc>,
) -> TermRef<'gc>
where
    PRE: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    POST: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    fn rec<'gc>(
        ctx: Context<'gc>,
        pre: &mut impl FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
        post: &mut impl FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
        term: TermRef<'gc>,
    ) -> TermRef<'gc> {
        let x = pre(ctx, term);

        match &*x {
            Term::App(..) | Term::Continue(..) | Term::If { .. } => post(ctx, x),
            Term::Let(binding, exp, body) => {
                let nbody = rec(ctx, pre, post, *body);
                if Gc::ptr_eq(nbody, *body) {
                    post(ctx, x)
                } else {
                    let nterm = Term::Let(*binding, exp.clone(), nbody);
                    post(ctx, Gc::new(&ctx, nterm))
                }
            }

            Term::Fix(procs, body) => {
                let mut rhs = Vec::with_capacity(procs.len());
                let mut changed = false;

                for func in procs.iter() {
                    let nbody = rec(ctx, pre, post, func.body());
                    if !Gc::ptr_eq(nbody, func.body()) {
                        changed = true;
                        rhs.push(func.with_body(ctx, nbody));
                    } else {
                        rhs.push(*func);
                        rhs.push(func.with_body(ctx, nbody));
                    }
                }

                let nbody = rec(ctx, pre, post, *body);

                if !changed && Gc::ptr_eq(nbody, *body) {
                    post(ctx, x)
                } else {
                    let nterm = Term::Fix(Array::from_slice(&ctx, &rhs), nbody);
                    post(ctx, Gc::new(&ctx, nterm))
                }
            }

            Term::Letk(conts, body) => {
                let mut nconts = Vec::with_capacity(conts.len());
                let mut changed = false;

                for cont in conts.iter() {
                    let nbody = rec(ctx, pre, post, cont.body());
                    if !Gc::ptr_eq(nbody, cont.body()) {
                        changed = true;
                        nconts.push(cont.with_body(ctx, nbody));
                    } else {
                        nconts.push(*cont);
                    }
                }

                let nbody = rec(ctx, pre, post, *body);

                if !changed && Gc::ptr_eq(nbody, *body) {
                    post(ctx, x)
                } else {
                    let nterm = Term::Letk(Array::from_slice(&ctx, &nconts), nbody);
                    post(ctx, Gc::new(&ctx, nterm))
                }
            }
        }
    }

    rec(ctx, &mut pre, &mut post, term)
}

pub fn post_order<'gc, F>(ctx: Context<'gc>, f: F, term: TermRef<'gc>) -> TermRef<'gc>
where
    F: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    pre_post_order(ctx, |_, t| t, f, term)
}

pub fn pre_order<'gc, F>(ctx: Context<'gc>, f: F, term: TermRef<'gc>) -> TermRef<'gc>
where
    F: Fn(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
{
    pre_post_order(ctx, f, |_, t| t, term)
}

pub fn fold_cps<'gc, ACC>(
    ctx: Context<'gc>,
    down: &mut impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
    up: &mut impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
    acc: ACC,
    term: TermRef<'gc>,
) -> ACC {
    fn foldts<'gc, ACC>(
        ctx: Context<'gc>,
        down: &mut impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
        up: &mut impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
        acc: ACC,
        term: TermRef<'gc>,
    ) -> ACC {
        let acc = down(ctx, acc, term);

        let acc = match &*term {
            Term::App(..) | Term::Continue(..) | Term::If { .. } => acc,

            Term::Let(_, _exp, body) => foldts(ctx, down, up, acc, *body),

            Term::Fix(funcs, body) => {
                let acc = funcs
                    .iter()
                    .fold(acc, |acc, func| foldts(ctx, down, up, acc, func.body()));
                foldts(ctx, down, up, acc, *body)
            }

            Term::Letk(conts, body) => {
                let acc = conts
                    .iter()
                    .fold(acc, |acc, cont| foldts(ctx, down, up, acc, cont.body()));
                foldts(ctx, down, up, acc, *body)
            }
        };

        up(ctx, acc, term)
    }

    foldts(ctx, down, up, acc, term)
}

impl<'gc> Term<'gc> {
    pub fn post_order<F>(self: TermRef<'gc>, ctx: Context<'gc>, f: F) -> TermRef<'gc>
    where
        F: FnMut(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    {
        post_order(ctx, f, self)
    }

    pub fn pre_order<F>(self: TermRef<'gc>, ctx: Context<'gc>, f: F) -> TermRef<'gc>
    where
        F: Fn(Context<'gc>, TermRef<'gc>) -> TermRef<'gc>,
    {
        pre_order(ctx, f, self)
    }

    pub fn fold<ACC>(
        self: TermRef<'gc>,
        ctx: Context<'gc>,
        mut down: impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
        mut up: impl FnMut(Context<'gc>, ACC, TermRef<'gc>) -> ACC,
        acc: ACC,
    ) -> ACC {
        fold_cps(ctx, &mut down, &mut up, acc, self)
    }
}
