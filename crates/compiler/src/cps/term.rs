use lasso::Spur;
use std::{hash::Hash, rc::Rc};

use crate::{
    utils::TreeEq,
    ast::{Datum, P, Symbol},
    il::term::LVar,
    source::Span,
};

#[derive(Debug, Clone)]
pub enum Term {
    Continue(P<LVar>, Vec<Atom>, Span),
    App(Atom, P<LVar>, Vec<Atom>, Span),
    If(Atom, P<LVar>, P<LVar>),

    Letk(Vec<Rc<Cont>>, Rc<Term>),
    Fix(Vec<Rc<Func>>, Rc<Term>),
    Let(P<LVar>, Expression, Rc<Term>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Atom {
    Constant(P<Datum>),
    Global(P<Symbol>, Span),
    Local(P<LVar>),
    Prim(Spur, Span),
    Func(Rc<Func>, Span),
}

impl From<Rc<Symbol>> for Atom {
    fn from(symbol: Rc<Symbol>) -> Self {
        Atom::Global(symbol, Span::default())
    }
}

impl From<Spur> for Atom {
    fn from(spur: Spur) -> Self {
        Atom::Prim(spur, Span::default())
    }
}

impl From<P<LVar>> for Atom {
    fn from(var: P<LVar>) -> Self {
        Atom::Local(var)
    }
}

impl From<P<Datum>> for Atom {
    fn from(datum: P<Datum>) -> Self {
        Atom::Constant(datum)
    }
}

impl From<Datum> for Atom {
    fn from(datum: Datum) -> Self {
        Atom::Constant(P::new(datum))
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Option<Spur>,
    pub binding: P<LVar>,
    pub return_cont: P<LVar>,
    pub args: Vec<P<LVar>>,
    pub variadic: Option<P<LVar>>,
    pub body: Rc<Term>,
    pub span: Span,
    pub reified: bool,
}

impl Func {
    pub fn with_body(&self, body: Rc<Term>) -> P<Self> {
        P(Func {
            body,
            ..self.clone()
        })
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Func {}

#[derive(Debug, Clone)]
pub struct Cont {
    pub name: Option<Spur>,
    pub binding: P<LVar>,
    pub args: Vec<P<LVar>>,
    pub variadic: Option<P<LVar>>,
    pub body: Rc<Term>,
    pub span: Span,
}

impl Cont {
    pub fn with_body(&self, body: Rc<Term>) -> P<Self> {
        P(Cont {
            body,
            ..self.clone()
        })
    }
}

impl PartialEq for Cont {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Cont {}

impl Hash for Func {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Func).hash(state);
    }
}

impl Hash for Cont {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Cont).hash(state);
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    PrimCall(Spur, Vec<Atom>, Span),
}

impl TreeEq for Term {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Continue(a1, a2, s1), Term::Continue(b1, b2, s2)) => {
                a1.tree_eq(b1) && a2 == b2 && s1 == s2
            }
            (Term::App(a1, a2, a3, s1), Term::App(b1, b2, b3, s2)) => {
                a1.tree_eq(b1) && a2.tree_eq(b2) && a3.tree_eq(b3) && s1 == s2
            }
            (Term::If(a1, a2, a3), Term::If(b1, b2, b3)) => {
                a1 == b1 && a2.tree_eq(b2) && a3.tree_eq(b3)
            }
            (Term::Letk(a1, a2), Term::Letk(b1, b2)) => a1.tree_eq(b1) && Rc::ptr_eq(&a2, &b2),
            (Term::Fix(a1, a2), Term::Fix(b1, b2)) => a1.tree_eq(b1) && a2.tree_eq(b2),
            (Term::Let(a1, a2, a3), Term::Let(b1, b2, b3)) => {
                a1.tree_eq(b1) && a2.tree_eq(b2) && a3.tree_eq(b3)
            }
            _ => false,
        }
    }
}

impl TreeEq for Atom {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Constant(a), Atom::Constant(b)) => a == b,
            (Atom::Global(a, _), Atom::Global(b, _)) => a == b,
            (Atom::Local(a), Atom::Local(b)) => a.tree_eq(b),
            (Atom::Prim(a, _), Atom::Prim(b, _)) => a == b,
            (Atom::Func(a, _), Atom::Func(b, _)) => a.tree_eq(b),
            _ => false,
        }
    }
}

impl TreeEq for Func {
    fn tree_eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.binding.tree_eq(&other.binding)
            && self.return_cont.tree_eq(&other.return_cont)
            && self.args == other.args
            && self.variadic == other.variadic
            && self.body.tree_eq(&other.body)
            && self.span == other.span
    }
}

impl TreeEq for Cont {
    fn tree_eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.binding.tree_eq(&other.binding)
            && self.args == other.args
            && self.variadic == other.variadic
            && self.body.tree_eq(&other.body)
            && self.span == other.span
    }
}

impl TreeEq for LVar {
    fn tree_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl TreeEq for Expression {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::PrimCall(a1, a2, s1), Expression::PrimCall(b1, b2, s2)) => {
                a1 == b1 && a2 == b2 && s1 == s2
            }
        }
    }
}

pub struct Closure {
    pub func: P<Func>,
    /// The captured variables that are used in the closure.
    pub captured: Vec<P<LVar>>,
}