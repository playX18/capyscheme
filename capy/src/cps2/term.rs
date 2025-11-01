use std::ops::{Index, IndexMut};

use im::*;

use crate::{expander::core::LVarRef, prelude::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Atom<'gc> {
    Constant(Value<'gc>),
    Variable(LVarRef<'gc>),
}

#[derive(Clone)]
pub enum Expression<'gc> {
    PrimCall(Value<'gc>, Vector<Atom<'gc>>, LVarRef<'gc>, Value<'gc>),
}

#[derive(Clone)]
pub enum Term<'gc> {
    Let(LVarRef<'gc>, Expression<'gc>, TermId),

    Fix {
        funcs: Vector<FuncId>,
        body: TermId,
    },

    Letk {
        conts: Vector<ContId>,
        body: TermId,
    },

    Call {
        func: Atom<'gc>,
        args: Vector<Atom<'gc>>,
        retk: LVarRef<'gc>,
        rete: LVarRef<'gc>,
        src: Value<'gc>,
    },

    Continue {
        cont: LVarRef<'gc>,
        args: Vector<Atom<'gc>>,
    },

    If {
        test: Atom<'gc>,
        consequent: LVarRef<'gc>,
        consequent_args: Vector<Atom<'gc>>,
        alternative: LVarRef<'gc>,
        alternative_args: Vector<Atom<'gc>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct TermId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct FuncId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ContId(pub u32);

#[derive(Clone)]
pub struct Func<'gc> {
    pub binding: LVarRef<'gc>,
    pub params: Vector<LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub retk: LVarRef<'gc>,
    pub rete: LVarRef<'gc>,
    pub body: TermId,
    pub meta: Value<'gc>,
    pub opt_flags: OptFlags,
}

#[derive(Clone)]
pub struct Cont<'gc> {
    pub binding: LVarRef<'gc>,
    /// Continuation for throwing exceptions from this continuation.
    ///
    /// Only used by reified continuations when argument count does not match.
    pub rete: LVarRef<'gc>,
    pub params: Vector<LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: TermId,
    pub meta: Value<'gc>,
    pub opt_flags: OptFlags,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct OptFlags: u8 {
        /// No optimization flags.
        const NONE = 0;
        /// Mark continuation or function as cold and tries to minimize code size when inlining.
        const COLD = 1 << 0;
        /// Prevent inlining of this continuation or function.
        const NOINLINE = 1 << 1;
        /// Always inline this continuation or function.
        const INLINE_ALWAYS = 1 << 2;
    }
}

pub struct CPS<'gc> {
    pub funcs: OrdMap<FuncId, Func<'gc>>,
    pub conts: OrdMap<ContId, Cont<'gc>>,
    pub terms: OrdMap<TermId, Term<'gc>>,
}

impl<'gc> CPS<'gc> {
    pub fn new() -> Self {
        Self {
            funcs: OrdMap::new(),
            conts: OrdMap::new(),
            terms: OrdMap::new(),
        }
    }

    pub fn replace_term(&mut self, id: TermId, term: Term<'gc>) {
        self.terms.insert(id, term);
    }
}

impl<'gc> Index<TermId> for CPS<'gc> {
    type Output = Term<'gc>;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.terms[&index]
    }
}

impl<'gc> Index<FuncId> for CPS<'gc> {
    type Output = Func<'gc>;

    fn index(&self, index: FuncId) -> &Self::Output {
        &self.funcs[&index]
    }
}

impl<'gc> Index<ContId> for CPS<'gc> {
    type Output = Cont<'gc>;

    fn index(&self, index: ContId) -> &Self::Output {
        &self.conts[&index]
    }
}

impl<'gc> IndexMut<TermId> for CPS<'gc> {
    fn index_mut(&mut self, index: TermId) -> &mut Self::Output {
        self.terms.get_mut(&index).unwrap()
    }
}

impl<'gc> IndexMut<FuncId> for CPS<'gc> {
    fn index_mut(&mut self, index: FuncId) -> &mut Self::Output {
        self.funcs.get_mut(&index).unwrap()
    }
}

impl<'gc> IndexMut<ContId> for CPS<'gc> {
    fn index_mut(&mut self, index: ContId) -> &mut Self::Output {
        self.conts.get_mut(&index).unwrap()
    }
}
