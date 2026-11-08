//! Tiny IR builders shared by the pass tests.

use cranelift_entity::EntityRef;

use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Constant, Edge, Inst, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::{CodeRef, Type};

/// An empty, unreachable block. The caller sets its terminator.
pub(crate) fn block(func: &mut Func<'_>) -> Block {
    func.new_block()
}

/// Pushes an instruction with a result.
pub(crate) fn push(func: &mut Func<'_>, block: Block, op: Op, args: &[Value], ty: Type) -> Value {
    func.push_inst(block, op, args, Some(ty), None)
        .expect("the operation has a result")
}

/// A tagged immediate constant.
pub(crate) fn const_any(func: &mut Func<'_>, block: Block, bits: u64) -> Value {
    push(func, block, Op::Const(Constant::Bits(bits)), &[], Type::Any)
}

/// A raw int32 constant.
pub(crate) fn const_i32(func: &mut Func<'_>, block: Block, n: i32) -> Value {
    push(func, block, Op::Const(Constant::Int32(n)), &[], Type::Int32)
}

/// A raw Bool flag.
pub(crate) fn const_bool(func: &mut Func<'_>, block: Block, value: bool) -> Value {
    push(
        func,
        block,
        Op::Const(Constant::Bits(u64::from(value))),
        &[],
        Type::Bool,
    )
}

/// An empty frame state whose only value is `closure`.
pub(crate) fn frame(func: &mut Func<'_>, closure: Value) -> FrameState {
    func.push_frame(FrameStateData {
        code: CodeRef::new(0),
        pc: 0,
        closure,
        registers: Vec::new(),
        parent: None,
        call_base: None,
        returned: None,
        rest: None,
        inline_site: None,
    })
}

/// Terminates `from` with a jump to `to`.
pub(crate) fn jump(func: &mut Func<'_>, from: Block, to: Block, args: &[Value]) {
    let edge = func.edge(to, args);
    func.set_terminator(from, Terminator::Jump(edge));
}

/// Terminates `from` with a branch on `cond`.
pub(crate) fn branch(
    func: &mut Func<'_>,
    from: Block,
    cond: Value,
    then: Block,
    then_args: &[Value],
    otherwise: Block,
    otherwise_args: &[Value],
) {
    let then = func.edge(then, then_args);
    let otherwise = func.edge(otherwise, otherwise_args);
    func.set_terminator(
        from,
        Terminator::Branch {
            cond,
            then,
            otherwise,
        },
    );
}

/// Terminates `from` by returning `value`.
pub(crate) fn ret(func: &mut Func<'_>, from: Block, value: Value) {
    func.set_terminator(from, Terminator::Return(value));
}

/// Whether `inst` is still placed in a reachable block.
pub(crate) fn placed(func: &Func<'_>, inst: Inst) -> bool {
    func.blocks.values().any(|data| data.insts.contains(&inst))
}

/// The edge `from -> to`, when the terminator has exactly one.
pub(crate) fn edge_to(func: &Func<'_>, from: Block, to: Block) -> Edge {
    let term = func.blocks[from].term.as_ref().expect("a terminator");
    term.edges()
        .into_iter()
        .find(|edge| edge.block == to)
        .expect("an edge to the target")
}
