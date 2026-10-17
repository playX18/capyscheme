use crate::{
    compiler::cps::graph::BranchHint,
    expander::core::LVarRef,
    runtime::{prims::def::Primitive, value::Value, vm::exceptions::RaiseKind},
};
use smallvec::{SmallVec, smallvec};
use std::collections::HashMap;

use super::effects::{EffectFlags, terminator_effects};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

/// Mutable local home. May be written multiple times.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UVar(pub u32);

pub use UVar as ValueId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operand<'gc> {
    Constant(Value<'gc>),
    Local(UVar),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphCodeId(pub u32);

/// Identity of a compiled procedure. Continuations are never procedures of
/// their own: a `letk` is a block of the function that binds it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeId {
    GraphFunction(GraphCodeId),
}

#[derive(Debug, Clone)]
pub struct Program<'gc> {
    pub entry: CodeId,
    pub procedures: Vec<Procedure<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Procedure<'gc> {
    pub code: CodeId,
    pub binding: UVar,
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub meta: Value<'gc>,
    pub params: Vec<UVar>,
    pub variadic: Option<UVar>,
    pub free_vars: Vec<UVar>,
    /// True when slot 0 is a shared `EnvRecord` pointer rather than a captured
    /// value.
    pub env_shared: bool,
    pub sources: HashMap<UVar, LVarRef<'gc>>,
    pub entry: BlockId,
    pub blocks: Vec<Block<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub instructions: Vec<Instruction<'gc>>,
    pub terminator: Terminator<'gc>,
    pub source: Value<'gc>,
}

impl<'gc> Block<'gc> {
    /// Whether `id` appears as a use in this block's body or terminator.
    pub fn uses_local(&self, id: UVar) -> bool {
        let is_use = |atom: &Operand<'gc>| matches!(atom, Operand::Local(v) if *v == id);
        self.instructions
            .iter()
            .any(|instruction| instruction.uses().iter().any(is_use))
            || self.terminator.uses().iter().any(is_use)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestPredicate {
    Null,
    Pair,
    List,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<'gc> {
    /// Copy `src` into mutable home `dst`.
    Assign {
        dst: UVar,
        src: Operand<'gc>,
    },
    Const {
        dst: UVar,
        value: Value<'gc>,
    },
    MakeClosure {
        dst: UVar,
        code: CodeId,
        /// Physical free-var slot count (including the env pointer at slot 0
        /// when `env` is `Some`).
        free_count: usize,
        /// Shared `EnvRecord` operand: holds the free vars common to all
        /// closures at this site, stored in slot 0, private captures in
        /// slots 1.. `free_count`.
        env: Option<Operand<'gc>>,
    },
    /// Allocate a shared closure environment record with `size` value slots.
    MakeEnv {
        dst: UVar,
        size: usize,
    },
    /// Read slot `index` of the `EnvRecord` in `env`.
    EnvRef {
        dst: UVar,
        env: Operand<'gc>,
        index: usize,
    },
    /// Write slot `index` of the `EnvRecord` in `env`.
    EnvSet {
        env: Operand<'gc>,
        index: usize,
        value: Operand<'gc>,
    },
    ClosureRef {
        dst: UVar,
        closure: Operand<'gc>,
        index: usize,
    },
    ClosureSet {
        closure: Operand<'gc>,
        index: usize,
        value: Operand<'gc>,
    },
    CacheRef {
        dst: UVar,
        cache_key: Operand<'gc>,
        source: Value<'gc>,
    },
    CacheSet {
        dst: UVar,
        cache_key: Operand<'gc>,
        value: Operand<'gc>,
        source: Value<'gc>,
    },
    PrimCall {
        dst: UVar,
        prim: Primitive,
        args: Vec<Operand<'gc>>,
        source: Value<'gc>,
    },
    RestToList {
        dst: UVar,
        rest: UVar,
        source: Value<'gc>,
    },
    RestRef {
        dst: UVar,
        rest: UVar,
        index: usize,
        source: Value<'gc>,
    },
    RestLength {
        dst: UVar,
        rest: UVar,
        skip: usize,
        source: Value<'gc>,
    },
    RestPredicate {
        dst: UVar,
        rest: UVar,
        predicate: RestPredicate,
        skip: usize,
        source: Value<'gc>,
    },
    /// Bind value `index` returned by the call this block is the return point
    /// of. Only valid in a return-point block, conceptually it runs on the
    /// edge from the call.
    Receive {
        dst: UVar,
        index: usize,
    },
    /// Bind the list of returned values from `from` on. Same placement rules
    /// as `Receive`.
    ReceiveRest {
        dst: UVar,
        from: usize,
    },
}

/// Where a non-tail call returns to: a block of the same procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReturnPoint {
    pub block: BlockId,
    /// Number of values the block binds with `Receive`.
    pub nvals: usize,
    /// Whether extra values are accepted (and bound with `ReceiveRest`).
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchTarget<'gc> {
    Local {
        block: BlockId,
        /// Instructions to run on this edge before entering `block` (contified
        /// parallel assigns / rest cons). Kept on the edge instead of a helper
        /// block so there is one fewer jump per continue.
        edge_assigns: Vec<Instruction<'gc>>,
    },
    /// Return `args` to the caller of this procedure.
    Return { args: Vec<Operand<'gc>> },
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
    /// Non-tail call. Control continues at `ret.block` with the returned
    /// values.
    Call {
        callee: Operand<'gc>,
        ret: ReturnPoint,
        args: Vec<Operand<'gc>>,
        source: Value<'gc>,
    },
    /// Call in tail position: the callee returns to this procedure's caller.
    TailCall {
        callee: Operand<'gc>,
        args: Vec<Operand<'gc>>,
        source: Value<'gc>,
    },
    /// Return `args` to the caller.
    Return {
        args: Vec<Operand<'gc>>,
        source: Value<'gc>,
    },
    Raise {
        kind: RaiseKind,
        args: Vec<Operand<'gc>>,
        source: Value<'gc>,
    },
    Jump {
        target: BlockId,
    },
    Branch {
        test: Operand<'gc>,
        consequent: BranchTarget<'gc>,
        alternative: BranchTarget<'gc>,
        hints: [BranchHint; 2],
    },
    /// Predicate/compare fused with the branch that tests its result.
    ///
    /// Formed during CPS conversion (`convert_branch_prim`) from
    /// `(if (prim args...) ...)` with a branch-fusable primitive, so the
    /// backend can emit the test and branch without materializing a
    /// Scheme bool.
    BranchPrim {
        prim: Primitive,
        args: Vec<Operand<'gc>>,
        consequent: BranchTarget<'gc>,
        alternative: BranchTarget<'gc>,
        hints: [BranchHint; 2],
    },
    Switch {
        kind: SwitchKind,
        scrutinee: Operand<'gc>,
        cases: Vec<SwitchCase<'gc>>,
        default: BranchTarget<'gc>,
    },
}

impl<'gc> Instruction<'gc> {
    pub fn def(&self) -> Option<UVar> {
        match self {
            Self::Assign { dst, .. }
            | Self::Const { dst, .. }
            | Self::MakeClosure { dst, .. }
            | Self::MakeEnv { dst, .. }
            | Self::EnvRef { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::CacheRef { dst, .. }
            | Self::CacheSet { dst, .. }
            | Self::PrimCall { dst, .. }
            | Self::RestToList { dst, .. }
            | Self::RestRef { dst, .. }
            | Self::RestLength { dst, .. }
            | Self::RestPredicate { dst, .. }
            | Self::Receive { dst, .. }
            | Self::ReceiveRest { dst, .. } => Some(*dst),
            Self::ClosureSet { .. } | Self::EnvSet { .. } => None,
        }
    }

    pub fn defs(&self) -> SmallVec<[UVar; 2]> {
        match self.def() {
            Some(dst) => smallvec![dst],
            None => SmallVec::new(),
        }
    }

    pub fn uses(&self) -> Vec<Operand<'gc>> {
        match self {
            Self::Assign { src, .. } => vec![*src],
            Self::Const { .. } => vec![],
            Self::MakeClosure { env, .. } => env.map_or(vec![], |env| vec![env]),
            Self::MakeEnv { .. } => vec![],
            Self::EnvRef { env, .. } => vec![*env],
            Self::EnvSet { env, value, .. } => vec![*env, *value],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::CacheRef { cache_key, .. } => vec![*cache_key],
            Self::CacheSet {
                cache_key, value, ..
            } => vec![*cache_key, *value],
            Self::PrimCall { args, .. } => args.clone(),
            Self::RestToList { rest, .. } => vec![Operand::Local(*rest)],
            Self::RestRef { rest, .. }
            | Self::RestLength { rest, .. }
            | Self::RestPredicate { rest, .. } => vec![Operand::Local(*rest)],
            Self::Receive { .. } | Self::ReceiveRest { .. } => vec![],
        }
    }
}

impl<'gc> BranchTarget<'gc> {
    pub fn local_successor(&self) -> Option<BlockId> {
        match self {
            Self::Local { block, .. } => Some(*block),
            Self::Return { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<Operand<'gc>> {
        match self {
            Self::Local { edge_assigns, .. } => {
                let mut uses = Vec::new();
                for instruction in edge_assigns {
                    uses.extend(instruction.uses());
                }
                uses
            }
            Self::Return { args } => args.clone(),
        }
    }
}

impl<'gc> Terminator<'gc> {
    pub fn is_terminal(&self) -> bool {
        terminator_effects(self)
            .flags
            .contains(EffectFlags::TERMINAL)
    }

    pub fn uses(&self) -> Vec<Operand<'gc>> {
        match self {
            Self::Call { callee, args, .. } | Self::TailCall { callee, args, .. } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*callee);
                uses.extend(args.iter().copied());
                uses
            }
            Self::Raise { args, .. } | Self::Return { args, .. } => args.clone(),
            Self::Jump { .. } => vec![],
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
            Self::BranchPrim {
                args,
                consequent,
                alternative,
                ..
            } => {
                let mut uses = Vec::with_capacity(
                    args.len() + consequent.uses().len() + alternative.uses().len(),
                );
                uses.extend(args.iter().copied());
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
            Self::Call { ret, .. } => vec![ret.block],
            Self::TailCall { .. } | Self::Raise { .. } | Self::Return { .. } => vec![],
            Self::Jump { target } => vec![*target],
            Self::Branch {
                consequent,
                alternative,
                ..
            }
            | Self::BranchPrim {
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
