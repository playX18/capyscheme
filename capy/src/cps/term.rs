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
    Values(ArrayRef<'gc, Atom<'gc>>),
}

impl<'gc> std::fmt::Display for Atom<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Local(l) => write!(f, "{}", l.name),
            Self::Values(_) => write!(f, "(values ...)"),
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

    Throw(Throw<'gc>, Value<'gc>),
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
pub enum Throw<'gc> {
    /// Throw to KEY and ARGS. ARGS should be a list
    Throw(Atom<'gc>, Atom<'gc>),
    /// Raise error indicating VAL as bad value, KEY-SUBR-AND-MESSAGE
    /// should be a vector, where first element is the symbol
    /// to which to throw, the second is the procedure in which to signal
    /// the error (a string) or #f, and the third is the format string
    /// with one template argument.
    Value(Atom<'gc>, Atom<'gc>),
    ValueAndData(Atom<'gc>, Atom<'gc>),
}

#[derive(Debug, Clone, Trace, Copy)]
#[collect(no_drop)]
pub enum Expression<'gc> {
    PrimCall(Value<'gc>, Atoms<'gc>, LVarRef<'gc>, Value<'gc>),
    PrimRef(Value<'gc>),
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
}

impl<'gc> Func<'gc> {
    pub fn with_body(&self, ctx: Context<'gc>, body: TermRef<'gc>) -> FuncRef<'gc> {
        Gc::new(
            &ctx,
            Func {
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
    pub body: TermRef<'gc>,
    pub source: Value<'gc>,
    pub free_vars: Lock<Option<Vars<'gc>>>,
    pub reified: Cell<bool>,
    pub handler: Lock<LVarRef<'gc>>,
    pub cold: bool,
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
                name: self.name,
                binding: self.binding,
                args: self.args,
                variadic: self.variadic,
                body,
                source: self.source,
                free_vars: Lock::new(self.free_vars.get()),
                reified: Cell::new(self.reified.get()),
                handler: Lock::new(self.handler.get()),
                cold: self.cold,
            },
        )
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
            (Atom::Values(a), Atom::Values(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(x, y)| x.tree_eq(y))
            }
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

impl<'gc> TreeEq for Throw<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Throw::Throw(a_key, a_args), Throw::Throw(b_key, b_args)) => {
                a_key.tree_eq(b_key) && a_args.tree_eq(b_args)
            }
            (Throw::Value(a_key, a_subr_and_message), Throw::Value(b_key, b_subr_and_message)) => {
                a_key.tree_eq(b_key) && a_subr_and_message.tree_eq(b_subr_and_message)
            }
            (Throw::ValueAndData(a_key, a_data), Throw::ValueAndData(b_key, b_data)) => {
                a_key.tree_eq(b_key) && a_data.tree_eq(b_data)
            }
            _ => false,
        }
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
            /*(Term::If(f, k, l, hints), Term::If(g, m, n, bhints)) => {
                f.tree_eq(g) && Gc::ptr_eq(*k, *m) && Gc::ptr_eq(*l, *n) && hints == bhints
            }*/
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
            (Term::Throw(a, _), Term::Throw(b, _)) => a.tree_eq(b),

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
            (Expression::PrimRef(f), Expression::PrimRef(g)) => f.tree_eq(g),

            _ => false,
        }
    }
}

impl<'gc> Term<'gc> {
    pub fn source(&self) -> Value<'gc> {
        match self {
            Term::Continue(_, _, source) | Term::App(_, _, _, _, source) => *source,
            Term::If { .. } => Value::new(false),
            Term::Letk(_, body) | Term::Fix(_, body) | Term::Let(_, _, body) => body.source(),
            Term::Throw(_, src) => *src,
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
}
