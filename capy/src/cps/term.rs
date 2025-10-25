use std::{cell::Cell, hash::Hash};

use rsgc::{Gc, Trace, alloc::array::ArrayRef, cell::Lock};

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

#[derive(Debug, Clone, Trace, Copy)]
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
    pub return_cont: LVarRef<'gc>,
    pub handler_cont: LVarRef<'gc>,

    pub args: Vars<'gc>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: TermRef<'gc>,

    pub free_vars: Lock<Option<Vars<'gc>>>,
    pub meta: Value<'gc>,
}

impl<'gc> Func<'gc> {
    pub fn with_body(&self, ctx: Context<'gc>, body: TermRef<'gc>) -> FuncRef<'gc> {
        Gc::new(
            &ctx,
            Func {
                meta: self.meta,
                name: self.name,
                source: self.source,
                binding: self.binding,
                return_cont: self.return_cont,
                handler_cont: self.handler_cont,
                args: self.args,
                variadic: self.variadic,
                body,
                free_vars: Lock::new(self.free_vars.get()),
            },
        )
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
    pub ignore_args: bool,
    pub body: TermRef<'gc>,
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
        self.body
    }

    pub fn with_body(&self, ctx: Context<'gc>, body: TermRef<'gc>) -> Gc<'gc, Cont<'gc>> {
        Gc::new(
            &ctx,
            Self {
                meta: self.meta,
                name: self.name,
                binding: self.binding,
                ignore_args: self.ignore_args,
                args: self.args,
                variadic: self.variadic,
                body,
                source: self.source,
                noinline: self.noinline,
                free_vars: Lock::new(self.free_vars.get()),
                reified: Cell::new(self.reified.get()),
                handler: Lock::new(self.handler.get()),
                cold: self.cold,
            },
        )
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
            && self.body.tree_eq(&other.body)
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
                    func.body.count_refs();
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
                    cont.body.count_refs();
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
