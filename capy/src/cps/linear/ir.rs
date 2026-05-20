use crate::{
    compiler::ssa::primitive::Primitive,
    cps::term::{BranchHint, ContRef, FuncRef},
    expander::core::LVarRef,
    runtime::{value::Value, vm::exceptions::RaiseKind},
};
use smallvec::{SmallVec, smallvec};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinearAtom<'gc> {
    Constant(Value<'gc>),
    Local(ValueId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcedureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeId<'gc> {
    Function(FuncRef<'gc>),
    Continuation(ContRef<'gc>),
}

#[derive(Debug, Clone)]
pub struct LinearProgram<'gc> {
    pub entry: FuncRef<'gc>,
    pub procedures: Vec<Procedure<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Procedure<'gc> {
    pub code: CodeId<'gc>,
    pub kind: ProcedureKind,
    pub binding: ValueId,
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub meta: Value<'gc>,
    pub return_cont: Option<ValueId>,
    pub params: Vec<ValueId>,
    pub variadic: Option<ValueId>,
    pub free_vars: Vec<ValueId>,
    pub sources: HashMap<ValueId, LVarRef<'gc>>,
    pub entry: BlockId,
    pub blocks: Vec<Block<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub params: Vec<ValueId>,
    pub variadic: Option<ValueId>,
    pub instructions: Vec<Instruction<'gc>>,
    pub terminator: Terminator<'gc>,
    pub source: Value<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestPredicate {
    Null,
    Pair,
    List,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<'gc> {
    Const {
        dst: ValueId,
        value: Value<'gc>,
    },
    MakeClosure {
        dst: ValueId,
        code: CodeId<'gc>,
        kind: ClosureKind,
        free_count: usize,
    },
    ClosureRef {
        dst: ValueId,
        closure: LinearAtom<'gc>,
        index: usize,
    },
    ClosureSet {
        closure: LinearAtom<'gc>,
        index: usize,
        value: LinearAtom<'gc>,
    },
    CacheRef {
        dst: ValueId,
        cache_key: LinearAtom<'gc>,
        source: Value<'gc>,
    },
    CacheSet {
        dst: ValueId,
        cache_key: LinearAtom<'gc>,
        value: LinearAtom<'gc>,
        source: Value<'gc>,
    },
    PrimCall {
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    RestToList {
        dst: ValueId,
        rest: ValueId,
        source: Value<'gc>,
    },
    RestRef {
        dst: ValueId,
        rest: ValueId,
        index: usize,
        source: Value<'gc>,
    },
    RestLength {
        dst: ValueId,
        rest: ValueId,
        skip: usize,
        source: Value<'gc>,
    },
    RestPredicate {
        dst: ValueId,
        rest: ValueId,
        predicate: RestPredicate,
        skip: usize,
        source: Value<'gc>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchTarget<'gc> {
    Local {
        block: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Reified {
        continuation: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase<'gc> {
    pub value: SwitchCaseValue<'gc>,
    pub target: BranchTarget<'gc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SwitchCaseValue<'gc> {
    Integer(i32),
    Symbol { hash: u64, value: Value<'gc> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwitchKind {
    Eq,
    Fixnum,
    Numeric,
    Char,
    CharEq,
    SymbolEq { mask: u64 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator<'gc> {
    Call {
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    TailCall {
        callee: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    Raise {
        kind: RaiseKind,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    Jump {
        target: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Branch {
        test: LinearAtom<'gc>,
        consequent: BranchTarget<'gc>,
        alternative: BranchTarget<'gc>,
        hints: [BranchHint; 2],
    },
    Switch {
        kind: SwitchKind,
        scrutinee: LinearAtom<'gc>,
        cases: Vec<SwitchCase<'gc>>,
        default: BranchTarget<'gc>,
    },
}

impl<'gc> Instruction<'gc> {
    pub fn def(&self) -> Option<ValueId> {
        match self {
            Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. } => Some(*dst),
            Self::ClosureSet { .. } => None,
        }
    }

    pub fn defs(&self) -> SmallVec<[ValueId; 2]> {
        match self {
            Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. } => smallvec![*dst],
            Self::ClosureSet { .. } => SmallVec::new(),
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Const { .. } => vec![],
            Self::MakeClosure { .. } => vec![],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::CacheRef { cache_key, .. } => vec![*cache_key],
            Self::CacheSet {
                cache_key, value, ..
            } => vec![*cache_key, *value],
            Self::PrimCall { args, .. } => args.clone(),
            Self::RestToList { .. } => vec![],
            Self::RestRef { rest, .. }
            | Self::RestLength { rest, .. }
            | Self::RestPredicate { rest, .. } => vec![LinearAtom::Local(*rest)],
        }
    }
}

impl<'gc> BranchTarget<'gc> {
    pub fn local_successor(&self) -> Option<BlockId> {
        match self {
            Self::Local { block, .. } => Some(*block),
            Self::Reified { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Local { args, .. } => args.clone(),
            Self::Reified { continuation, args } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*continuation);
                uses.extend(args.iter().copied());
                uses
            }
        }
    }
}

impl<'gc> Terminator<'gc> {
    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Call {
                callee, retk, args, ..
            } => {
                let mut uses = Vec::with_capacity(args.len() + 2);
                uses.push(*callee);
                uses.push(*retk);
                uses.extend(args.iter().copied());
                uses
            }
            Self::TailCall { callee, args, .. } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*callee);
                uses.extend(args.iter().copied());
                uses
            }
            Self::Raise { args, .. } => args.clone(),
            Self::Jump { args, .. } => args.clone(),
            Self::Branch {
                test,
                consequent,
                alternative,
                ..
            } => {
                let mut uses =
                    Vec::with_capacity(1 + consequent.uses().len() + alternative.uses().len());
                uses.push(*test);
                uses.extend(consequent.uses());
                uses.extend(alternative.uses());
                uses
            }
            Self::Switch {
                kind: _,
                scrutinee,
                cases,
                default,
            } => {
                let mut uses = Vec::with_capacity(
                    1 + default.uses().len()
                        + cases
                            .iter()
                            .map(|case| case.target.uses().len())
                            .sum::<usize>(),
                );
                uses.push(*scrutinee);
                for case in cases {
                    uses.extend(case.target.uses());
                }
                uses.extend(default.uses());
                uses
            }
        }
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Self::Call { .. } | Self::TailCall { .. } | Self::Raise { .. } => vec![],
            Self::Jump { target, .. } => vec![*target],
            Self::Branch {
                consequent,
                alternative,
                ..
            } => [consequent.local_successor(), alternative.local_successor()]
                .into_iter()
                .flatten()
                .collect(),
            Self::Switch { cases, default, .. } => cases
                .iter()
                .filter_map(|case| case.target.local_successor())
                .chain(default.local_successor())
                .collect(),
        }
    }
}
