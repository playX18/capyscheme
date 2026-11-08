//! Natural-loop discovery and the induction-value helpers shared by
//! [`licm`](super::licm), [`unroll`](super::unroll) and
//! [`strength`](super::strength).

use std::collections::HashSet;

use cranelift_entity::SecondaryMap;

use super::forward::def_inst;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, DomTree, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Edge, Inst, Op, Repr, Terminator};
use crate::runtime::objectmodel::Value as SchemeValue;

/// A natural loop.
pub(super) struct Loop {
    pub(super) header: Block,
    /// The blocks of the loop in reverse postorder, the header first.
    pub(super) blocks: Vec<Block>,
    pub(super) member: SecondaryMap<Block, bool>,
}

impl Loop {
    /// The blocks of the loop other than its header.
    pub(super) fn body(&self) -> Vec<Block> {
        self.blocks
            .iter()
            .copied()
            .filter(|&block| block != self.header)
            .collect()
    }
}

/// The natural loops of `func`, one per header.
pub(super) fn find_loops(func: &Func, dom: &DomTree) -> Vec<Loop> {
    let rpo = dom.rpo();
    let mut preds: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    let mut latches: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    let mut headers = Vec::new();
    for &block in rpo {
        for succ in func.successors(block) {
            preds[succ].push(block);
            if dom.dominates(succ, block) {
                if latches[succ].is_empty() {
                    headers.push(succ);
                }
                if !latches[succ].contains(&block) {
                    latches[succ].push(block);
                }
            }
        }
    }

    let mut loops = Vec::new();
    for header in headers {
        // The entry has no outside predecessor to hang a preheader on. It is
        // only a loop header after a self tail call, which is left alone.
        if header == func.entry {
            continue;
        }
        let mut member: SecondaryMap<Block, bool> = SecondaryMap::new();
        member[header] = true;
        let mut stack = latches[header].clone();
        while let Some(block) = stack.pop() {
            if member[block] {
                continue;
            }
            member[block] = true;
            stack.extend(preds[block].iter().copied());
        }
        let blocks = rpo.iter().copied().filter(|&b| member[b]).collect();
        loops.push(Loop {
            header,
            blocks,
            member,
        });
    }
    loops
}

/// The block that holds each instruction, for reachable blocks.
pub(super) fn inst_blocks(func: &Func) -> SecondaryMap<Inst, Option<Block>> {
    let mut map: SecondaryMap<Inst, Option<Block>> = SecondaryMap::new();
    for (block, data) in func.blocks.iter() {
        for &inst in &data.insts {
            map[inst] = Some(block);
        }
    }
    map
}

/// Whether `value` is a parameter of `lp`'s header whose value is the same on
/// every iteration: every in-loop edge into the header at its position passes
/// the parameter itself. A parameter updated from outside only (an OSR edge,
/// say) also qualifies, because nothing inside the loop changes it.
///
/// This is the parameter half of [`unroll`](super::unroll)'s
/// `bound_invariant`; the instruction half is handled by the caller, which
/// must only accept a value whose definition it has already chosen to move.
pub(super) fn header_param_invariant(func: &Func, lp: &Loop, value: Value) -> bool {
    let ValueDef::Param { block, index } = func.value_def(value) else {
        return false;
    };
    if block != lp.header {
        return false;
    }
    let slot = index as usize;
    for &pred in &lp.blocks {
        let Some(term) = &func.blocks[pred].term else {
            continue;
        };
        for edge in term.edges() {
            if edge.block != lp.header {
                continue;
            }
            let Some(&arg) = func.edge_args(&edge).get(slot) else {
                return false;
            };
            if arg != value {
                return false;
            }
        }
    }
    true
}

/// The preheader of `lp`: a block whose only successor is the header and
/// which is the only way into the loop from outside. An existing single-jump
/// predecessor is reused; otherwise a new block with one parameter per header
/// parameter is created and every outside edge into the header is retargeted
/// at it. Returns the block and the values that enter the header from it.
///
/// `None` when the header has no reachable edge from outside the loop.
pub(super) fn ensure_preheader(func: &mut Func, lp: &Loop) -> Option<(Block, Vec<Value>)> {
    let header = lp.header;
    let live = super::reachable(func);
    let mut entering: Vec<Block> = Vec::new();
    for block in func.blocks.keys().collect::<Vec<_>>() {
        if !live[block] || lp.member[block] {
            continue;
        }
        if let Some(term) = &func.blocks[block].term {
            for edge in term.edges() {
                if edge.block == header && !entering.contains(&block) {
                    entering.push(block);
                }
            }
        }
    }
    if entering.is_empty() {
        return None;
    }
    let header_params: Vec<Value> = func.params(header).to_vec();
    match entering.as_slice() {
        [only] if matches!(func.blocks[*only].term, Some(Terminator::Jump(_))) => {
            let Some(Terminator::Jump(edge)) = &func.blocks[*only].term else {
                unreachable!("matched a jump above")
            };
            Some((*only, func.edge_args(edge).to_vec()))
        }
        _ => {
            let pre = func.new_block();
            let params: Vec<Value> = header_params
                .iter()
                .map(|&param| {
                    let ty = func.value_type(param);
                    func.append_param(pre, ty)
                })
                .collect();
            for &block in &entering {
                if let Some(term) = func.blocks[block].term.as_mut() {
                    retarget(term, header, pre);
                }
            }
            let edge = func.edge(header, &params);
            func.set_terminator(pre, Terminator::Jump(edge));
            Some((pre, params))
        }
    }
}

/// Points every edge of `term` into `from` at `to`, keeping its arguments.
pub(super) fn retarget(term: &mut Terminator, from: Block, to: Block) {
    let fix = |edge: &mut Edge| {
        if edge.block == from {
            edge.block = to;
        }
    };
    match term {
        Terminator::Jump(edge) => fix(edge),
        Terminator::Branch {
            then, otherwise, ..
        } => {
            fix(then);
            fix(otherwise);
        }
        Terminator::Switch { cases, default, .. } => {
            for (_, edge) in cases.iter_mut() {
                fix(edge);
            }
            fix(default);
        }
        Terminator::Return(_)
        | Terminator::TailCall { .. }
        | Terminator::Deopt { .. }
        | Terminator::Unreachable => {}
    }
}

/// Every value defined inside `lp` (block parameters and instruction results).
pub(super) fn loop_defined(func: &Func, lp: &Loop) -> HashSet<Value> {
    let mut defined: HashSet<Value> = HashSet::new();
    for &block in &lp.blocks {
        defined.extend(func.params(block).iter().copied());
        for &inst in &func.blocks[block].insts {
            if let Some(result) = func.insts[inst].result {
                defined.insert(result);
            }
        }
    }
    defined
}

/// Calls `f` on every value `block` reads: instruction operands, frame
/// states (with their parent chains), terminator operands and edge arguments.
pub(super) fn for_each_block_use(func: &Func, block: Block, f: &mut dyn FnMut(Value)) {
    fn frame(func: &Func, fs: FrameState, f: &mut dyn FnMut(Value)) {
        let data = &func.frames[fs];
        f(data.closure);
        for &(_, value) in &data.registers {
            f(value);
        }
        if let Some(parent) = data.parent {
            frame(func, parent, f);
        }
    }
    let data = &func.blocks[block];
    for &inst in &data.insts {
        for &arg in func.args(inst) {
            f(arg);
        }
        if let Some(fs) = func.insts[inst].frame {
            frame(func, fs, f);
        }
    }
    if let Some(term) = &data.term {
        for value in func.terminator_uses(term) {
            f(value);
        }
        if let Terminator::TailCall { frame: fs, .. } | Terminator::Deopt { frame: fs } = term {
            frame(func, *fs, f);
        }
    }
}

/// The values defined inside `lp` that a reachable block outside it reads,
/// in discovery order. Edge arguments of loop-to-outside edges belong to
/// loop blocks and are not counted: cloning re-points them.
pub(super) fn outside_used_values(func: &Func, lp: &Loop) -> Vec<Value> {
    let defined = loop_defined(func, lp);
    let mut found: Vec<Value> = Vec::new();
    if defined.is_empty() {
        return found;
    }
    let live = super::reachable(func);
    let mut seen: HashSet<Value> = HashSet::new();
    for block in func.blocks.keys() {
        if lp.member[block] || !live[block] {
            continue;
        }
        for_each_block_use(func, block, &mut |value| {
            if defined.contains(&value) && seen.insert(value) {
                found.push(value);
            }
        });
    }
    found
}

/// Whether any block outside `lp` uses a value defined inside it directly.
/// [`lcssa::form_lcssa`](super::lcssa::form_lcssa) repairs such loops.
pub(super) fn outside_uses_loop_values(func: &Func, lp: &Loop) -> bool {
    !outside_used_values(func, lp).is_empty()
}

/// Whether a frame state names a value the check rejects, including the
/// parent chain of an inlined body.
pub(super) fn frame_uses(func: &Func, frame: FrameState, check: &dyn Fn(Value) -> bool) -> bool {
    let data = &func.frames[frame];
    if check(data.closure) || data.registers.iter().any(|&(_, value)| check(value)) {
        return true;
    }
    data.parent
        .is_some_and(|parent| frame_uses(func, parent, check))
}

/// The canonical numeric value: guards and unboxing do not change the number.
pub(super) fn canon(func: &Func, mut value: Value) -> Value {
    for _ in 0..4096 {
        let Some(inst) = def_inst(func, value) else {
            break;
        };
        match func.insts[inst].op {
            Op::CheckInt32 | Op::CheckInt64 | Op::Unbox(Repr::Int32) | Op::Unbox(Repr::Int64) => {
                value = func.args(inst)[0]
            }
            _ => break,
        }
    }
    value
}

/// The immediate value of an integer constant, in either raw lane. A tagged
/// fixnum constant counts too.
pub(super) fn int_const(func: &Func, value: Value) -> Option<i64> {
    let inst = def_inst(func, value)?;
    match func.insts[inst].op {
        Op::Const(Constant::Int32(n)) => Some(n as i64),
        Op::Const(Constant::Int64(n)) => Some(n),
        Op::Const(Constant::Bits(bits)) => SchemeValue::from_raw(bits).int32().map(|n| n as i64),
        _ => None,
    }
}

/// How a loop-carried counter changes on one back edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Step {
    /// The amount added to the counter each iteration.
    pub(super) delta: i64,
}

/// The step of parameter `index` of `header` over the loop, when every back
/// edge adds the same constant to it.
///
/// A back edge argument is unwrapped through boxes and fixnum guards. What
/// remains must be `add.x(raw, c)` or `sub.x(raw, c)`, where `raw`
/// canonicalizes to the parameter itself. The parameter may be a tagged
/// `Any` value whose counter is unboxed in the header.
pub(super) fn induction_step(func: &Func, lp: &Loop, header: Block, index: usize) -> Option<Step> {
    let param = func.params(header).get(index).copied()?;
    let mut delta: Option<i64> = None;
    let mut any = false;
    for pred in func.blocks.keys().collect::<Vec<_>>() {
        if !lp.member[pred] || pred == header {
            continue;
        }
        let Some(term) = &func.blocks[pred].term else {
            continue;
        };
        for edge in term.edges() {
            if edge.block != header {
                continue;
            }
            let arg = func.edge_args(&edge).get(index).copied()?;
            let delta_here = step_of(func, param, arg)?;
            any = true;
            match delta {
                None => delta = Some(delta_here),
                Some(previous) if previous == delta_here => {}
                Some(_) => return None,
            }
        }
    }
    if !any {
        return None;
    }
    Some(Step { delta: delta? })
}

/// The constant a back-edge argument adds to `param`.
fn step_of(func: &Func, param: Value, mut value: Value) -> Option<i64> {
    // Unwrap the boxes and guards that retag the updated counter.
    for _ in 0..64 {
        let Some(inst) = def_inst(func, value) else {
            break;
        };
        match func.insts[inst].op {
            Op::Box(_) | Op::Unbox(_) | Op::CheckInt32 | Op::CheckInt64 | Op::CheckBigIntFits64 => {
                value = func.args(inst)[0]
            }
            _ => break,
        }
    }
    let inst = def_inst(func, value)?;
    let args = func.args(inst);
    let (delta, base) = match func.insts[inst].op {
        Op::AddI64 | Op::AddI64Ovf | Op::AddI32Ovf => match (
            int_const(func, *args.get(1)?),
            int_const(func, *args.get(0)?),
        ) {
            (Some(c), _) => (c, args[0]),
            (None, Some(c)) => (c, args[1]),
            _ => return None,
        },
        Op::SubI64 | Op::SubI64Ovf | Op::SubI32Ovf => {
            let c = int_const(func, *args.get(1)?)?;
            (c.checked_neg()?, args[0])
        }
        _ => return None,
    };
    // The increment is relative to the parameter: directly for a raw counter,
    // or through its guard chain for a tagged one. `base` is the raw value the
    // header's comparison reads.
    if canon(func, base) != param {
        return None;
    }
    Some(delta)
}
