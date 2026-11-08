//! Strength reduction
//!
//! Three rewrites:
//!
//! 1. Multiply by a constant. A total `mul.i64` by `2^k` (or `-2^k`) becomes
//!    `k` doublings with `add.i64`, which is one add for the common `*2`; by
//!    `0`, `1` or `-1` it becomes a constant, the operand itself, or a
//!    subtraction. A checked `mul.i32.ovf` takes the same rewrites only when
//!    the operand's range keeps every intermediate inside `i32`, so the guard
//!    can never fire and its frame stays on the replacement.
//!
//! 2. Quotient, remainder and modulo by a constant. `quot` by `1` is the
//!    operand; by `-1` is a negation when the range excludes `MIN`; by a
//!    constant larger than the operand's magnitude it is zero. `rem` by `+-1`
//!    is zero and by a larger constant it is the operand. `mod` by `+-1` is
//!    zero, and when the operand's range lies inside one period
//!    `[k*d, (k+1)*d)` the modulo is `x + (-k*d)`, a single add.
//!
//! 3. Induction-variable products. In a natural loop, a header parameter that
//!    every back edge updates by the same constant is a counter. A
//!    `mul.i64(counter, C)` in the loop becomes a new header parameter holding
//!    `counter * C`, fed once on every entry and advanced by `step * C` on
//!    every back edge. The multiply then runs once per entry instead of once
//!    per iteration. Index arithmetic such as `(* i 8)` in a vector loop is
//!    the target.

use std::collections::HashMap;

use super::loops::Loop;
use super::loops::{canon, find_loops, induction_step, int_const};
use super::range;
use super::simplify::edges_mut;
use super::{PassOutcome, Subst, replace_uses};
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Inst, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;
use cranelift_entity::{EntityList, SecondaryMap};

/// How an instruction's result is recomputed.
#[derive(Clone, Copy)]
enum Rep {
    /// The result is an existing value.
    Value(Value),
    /// The result is an i32 immediate.
    Int32(i32),
    /// The result is an i64 immediate.
    Int64(i64),
    /// The result is `base + delta`, or `0 - base` when `op` is a subtraction.
    AddConst {
        base: Value,
        delta: i64,
        op: Op,
        frame: Option<FrameState>,
    },
    /// The result is `base * 2^k`, optionally negated, built by doubling
    /// with `op`.
    Double {
        base: Value,
        times: u32,
        negate: bool,
        op: Op,
        frame: Option<FrameState>,
    },
}

/// A planned rewrite.
struct Plan {
    inst: Inst,
    block: Block,
    rep: Rep,
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    // Induction-variable products first: a local power-of-two rewrite would
    // otherwise replace the multiply with doublings before the loop param is
    // available, even though one loop-carried add per iteration is cheaper.
    let mut changed = induction_reduce(func);
    let analysis = range::analyze(func);
    let rpo = func.reverse_postorder();
    let mut plans: Vec<Plan> = Vec::new();
    let mut work = func.blocks.len();
    for &block in &rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            if !func.insts[inst].op.has_result() {
                continue;
            }
            let Some(rep) = classify(func, &analysis, block, inst) else {
                continue;
            };
            plans.push(Plan { inst, block, rep });
        }
    }
    drop(analysis);

    let mut subst = Subst::default();
    let mut removed: Vec<(Block, Inst)> = Vec::new();
    for plan in &plans {
        let data = func.insts[plan.inst];
        let Some(result) = data.result else {
            continue;
        };
        let ty = func.value_type(result);
        if let Some(replaced) = apply(func, plan, ty, &mut subst) {
            if replaced {
                removed.push((plan.block, plan.inst));
                changed = true;
            }
        }
    }
    replace_uses(func, &subst);
    for (block, inst) in removed {
        work += 1;
        func.blocks[block]
            .insts
            .retain(|&candidate| candidate != inst);
    }
    PassOutcome { changed, work }
}

/// The rewrite for one instruction, if its operands make one exact.
fn classify(func: &Func, analysis: &range::Analysis<'_>, block: Block, inst: Inst) -> Option<Rep> {
    let data = &func.insts[inst];
    let args = func.args(inst);
    let result = data.result?;
    let ty = func.value_type(result);
    let left = *args.first()?;
    let right = *args.get(1)?;

    match data.op {
        Op::AddI64 | Op::AddI32Ovf => {
            if int_const(func, right) == Some(0) {
                Some(Rep::Value(left))
            } else if int_const(func, left) == Some(0) {
                Some(Rep::Value(right))
            } else {
                None
            }
        }
        Op::SubI64 | Op::SubI32Ovf => {
            (int_const(func, right) == Some(0)).then_some(Rep::Value(left))
        }
        Op::MulI64 => {
            let (value, constant) = constant_operand(func, left, right)?;
            mul_rep(ty, value, constant)
        }
        Op::MulI32Ovf => {
            let (value, constant) = constant_operand(func, left, right)?;
            mul32_rep(func, analysis, block, value, constant, data.frame)
        }
        Op::QuotI64 | Op::RemI64 | Op::ModI64 | Op::QuotI32 | Op::RemI32 | Op::ModI32 => {
            // A divisor is never commutative: it must be the right operand.
            let constant = int_const(func, right)?;
            division_rep(
                func, analysis, block, ty, data.op, left, constant, data.frame,
            )
        }
        _ => None,
    }
}

/// The value and the constant when one operand of a binary op is an integer
/// constant.
fn constant_operand(func: &Func, left: Value, right: Value) -> Option<(Value, i64)> {
    if let Some(c) = int_const(func, right) {
        Some((left, c))
    } else {
        int_const(func, left).map(|c| (right, c))
    }
}

/// The multiply rewrites for an `i64` operation.
fn mul_rep(ty: Type, value: Value, constant: i64) -> Option<Rep> {
    if constant == 0 {
        return Some(match ty {
            Type::Int32 => Rep::Int32(0),
            _ => Rep::Int64(0),
        });
    }
    if constant == 1 {
        return Some(Rep::Value(value));
    }
    let magnitude = constant.unsigned_abs();
    if !magnitude.is_power_of_two() {
        return None;
    }
    // Doubling is `x + x`, never `x * x`: the latter would be squaring.
    Some(Rep::Double {
        base: value,
        times: magnitude.trailing_zeros(),
        negate: constant < 0,
        op: Op::AddI64,
        frame: None,
    })
}

/// The multiply rewrites for `mul.i32.ovf`, guarded by a range proof that no
/// doubling overflows.
fn mul32_rep(
    func: &Func,
    analysis: &range::Analysis<'_>,
    block: Block,
    value: Value,
    constant: i64,
    frame: Option<FrameState>,
) -> Option<Rep> {
    let _ = func;
    if constant == 0 {
        return Some(Rep::Int32(0));
    }
    if constant == 1 {
        return Some(Rep::Value(value));
    }
    let (lo, hi) = analysis.range_of(block, value)?;
    let magnitude = constant.unsigned_abs();
    if !magnitude.is_power_of_two() {
        return None;
    }
    let times = magnitude.trailing_zeros();
    // Every intermediate and the final negation must fit i32.
    let mut scale: i128 = 1;
    for _ in 0..times {
        scale *= 2;
        if lo as i128 * scale < i32::MIN as i128 || hi as i128 * scale > i32::MAX as i128 {
            return None;
        }
    }
    if constant < 0 && lo as i128 * scale == i32::MIN as i128 {
        return None;
    }
    Some(Rep::Double {
        base: value,
        times,
        negate: constant < 0,
        op: Op::AddI32Ovf,
        frame,
    })
}

/// The quotient, remainder and modulo rewrites.
fn division_rep(
    func: &Func,
    analysis: &range::Analysis<'_>,
    block: Block,
    ty: Type,
    op: Op,
    value: Value,
    constant: i64,
    frame: Option<FrameState>,
) -> Option<Rep> {
    if constant == 0 {
        // Always deopts; leave the original guard alone.
        return None;
    }
    let int64 = matches!(ty, Type::Int64);
    let zero = || if int64 { Rep::Int64(0) } else { Rep::Int32(0) };

    match op {
        Op::QuotI64 | Op::QuotI32 => {
            if constant == 1 {
                return Some(Rep::Value(value));
            }
            if constant == -1 && excludes_min(analysis, func, block, value, ty) {
                return Some(negate_rep(value, ty, frame));
            }
            if magnitude_lt(analysis, block, value, constant) {
                return Some(zero());
            }
            None
        }
        Op::RemI64 | Op::RemI32 => {
            if constant == 1 || constant == -1 {
                return Some(zero());
            }
            if magnitude_lt(analysis, block, value, constant) {
                // The remainder of a value smaller than the divisor is the
                // value itself.
                return Some(Rep::Value(value));
            }
            None
        }
        Op::ModI64 | Op::ModI32 => {
            if constant == 1 || constant == -1 {
                return Some(zero());
            }
            let delta = period_offset(analysis, block, value, constant, int64)?;
            if delta == 0 {
                return Some(Rep::Value(value));
            }
            if int64 {
                Some(Rep::AddConst {
                    base: value,
                    delta,
                    op: Op::AddI64,
                    frame: None,
                })
            } else {
                let delta32 = i32::try_from(delta).ok()?;
                Some(Rep::AddConst {
                    base: value,
                    delta: delta32 as i64,
                    op: Op::AddI32Ovf,
                    frame,
                })
            }
        }
        _ => None,
    }
}

/// `0 - value`, as an `i64` total subtraction or a proved-safe `i32` one.
fn negate_rep(value: Value, ty: Type, frame: Option<FrameState>) -> Rep {
    Rep::AddConst {
        base: value,
        delta: 0,
        op: if matches!(ty, Type::Int64) {
            Op::SubI64
        } else {
            Op::SubI32Ovf
        },
        frame,
    }
}

/// Whether the range of `value` excludes `MIN` for its type.
fn excludes_min(
    analysis: &range::Analysis<'_>,
    _func: &Func,
    block: Block,
    value: Value,
    ty: Type,
) -> bool {
    let min = if matches!(ty, Type::Int32) {
        i32::MIN as i64
    } else {
        i64::MIN
    };
    matches!(analysis.range_of(block, value), Some((lo, hi)) if lo > min || hi < min)
}

/// Whether the range of `value` is strictly inside `(-|d|, |d|)`.
fn magnitude_lt(analysis: &range::Analysis<'_>, block: Block, value: Value, constant: i64) -> bool {
    let Some((lo, hi)) = analysis.range_of(block, value) else {
        return false;
    };
    let d = (constant as i128).abs();
    (lo as i128) > -d && (hi as i128) < d
}

/// The constant `delta` with `x mod d == x + delta` when the range of `x`
/// lies in one period of the modulo.
fn period_offset(
    analysis: &range::Analysis<'_>,
    block: Block,
    value: Value,
    constant: i64,
    int64: bool,
) -> Option<i64> {
    let (lo, hi) = analysis.range_of(block, value)?;
    let d = constant as i128;
    let k = floor_div(lo as i128, d);
    if floor_div(hi as i128, d) != k {
        return None;
    }
    let delta = i64::try_from(-k * d).ok()?;
    if !int64 {
        i32::try_from(delta).ok()?;
    }
    Some(delta)
}

/// Floor division of `a` by `b`, for a nonzero `b`.
fn floor_div(a: i128, b: i128) -> i128 {
    let q = a / b;
    let r = a % b;
    if r != 0 && (r < 0) != (b < 0) {
        q - 1
    } else {
        q
    }
}

/// Applies one planned rewrite. Returns `Some(true)` when the original
/// instruction was replaced and can be removed.
fn apply(func: &mut Func, plan: &Plan, ty: Type, subst: &mut Subst) -> Option<bool> {
    let result = func.insts[plan.inst].result?;
    match plan.rep {
        Rep::Value(value) => {
            subst.add(result, value);
            Some(true)
        }
        Rep::Int32(constant) => {
            if !matches!(ty, Type::Int32) {
                return None;
            }
            let data = &mut func.insts[plan.inst];
            data.op = Op::Const(Constant::Int32(constant));
            data.args = EntityList::new();
            data.frame = None;
            Some(false)
        }
        Rep::Int64(constant) => {
            if !matches!(ty, Type::Int64) {
                return None;
            }
            let data = &mut func.insts[plan.inst];
            data.op = Op::Const(Constant::Int64(constant));
            data.args = EntityList::new();
            data.frame = None;
            Some(false)
        }
        Rep::AddConst {
            base,
            delta,
            op,
            frame,
        } => {
            let block = plan.block;
            let start = func.blocks[block].insts.len();
            let constant = func.push_inst(
                block,
                Op::Const(constant_for(ty, delta)),
                &[],
                Some(ty),
                None,
            )?;
            let value = if matches!(op, Op::SubI64 | Op::SubI32Ovf) {
                // `0 - base`: the constant pushed above is unused.
                let zero =
                    func.push_inst(block, Op::Const(constant_for(ty, 0)), &[], Some(ty), None)?;
                let _ = constant;
                func.push_inst(block, op, &[zero, base], Some(ty), frame)?
            } else {
                func.push_inst(block, op, &[base, constant], Some(ty), frame)?
            };
            move_new_insts(func, block, start, plan.inst);
            subst.add(result, value);
            Some(true)
        }
        Rep::Double {
            base,
            times,
            negate,
            op,
            frame,
        } => {
            let block = plan.block;
            let start = func.blocks[block].insts.len();
            let mut current = base;
            for _ in 0..times {
                current = func.push_inst(block, op, &[current, current], Some(ty), frame)?;
            }
            if negate {
                let zero =
                    func.push_inst(block, Op::Const(constant_for(ty, 0)), &[], Some(ty), None)?;
                let sub = if matches!(ty, Type::Int32) {
                    Op::SubI32Ovf
                } else {
                    Op::SubI64
                };
                current = func.push_inst(block, sub, &[zero, current], Some(ty), frame)?;
            }
            move_new_insts(func, block, start, plan.inst);
            if current == result {
                return None;
            }
            subst.add(result, current);
            Some(true)
        }
    }
}

/// The constant operand matching a result of type `ty`.
fn constant_for(ty: Type, value: i64) -> Constant {
    if matches!(ty, Type::Int32) {
        Constant::Int32(value as i32)
    } else {
        Constant::Int64(value)
    }
}

/// Moves the instructions appended after `start` to just before `anchor`, so
/// a rewritten value still dominates every later use in the block.
fn move_new_insts(func: &mut Func, block: Block, start: usize, anchor: Inst) {
    let insts = &mut func.blocks[block].insts;
    if start >= insts.len() {
        return;
    }
    let appended: Vec<Inst> = insts.split_off(start);
    if let Some(position) = insts.iter().position(|&inst| inst == anchor) {
        insts.splice(position..position, appended);
    } else {
        insts.extend(appended);
    }
}

/// Strength-reduces one induction-variable product in one loop.
fn induction_reduce(func: &mut Func) -> bool {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    for lp in &loops {
        let Some(action) = plan_induction(func, lp) else {
            continue;
        };
        apply_induction(func, lp, action);
        return true;
    }
    false
}

/// A planned induction-variable product.
struct Induction {
    header: Block,
    /// `step * constant`, already checked to fit `i64`.
    delta: i64,
    mul: Inst,
    mul_block: Block,
    constant: i64,
    /// The counter parameter and its index.
    counter_index: usize,
    counter_type: Type,
    /// The edges into the header from outside the loop: predecessor and edge
    /// index in its terminator.
    outside: Vec<(Block, usize)>,
    /// The header frame state the seeded guard resumes at.
    frame: Option<FrameState>,
}

/// Finds a total `mul.i64` of a header counter with a constant.
fn plan_induction(func: &Func, lp: &Loop) -> Option<Induction> {
    let header = lp.header;
    let params = func.params(header);
    let inst_block = super::loops::inst_blocks(func);
    let header_frame = func.blocks[header]
        .insts
        .iter()
        .find_map(|&inst| func.insts[inst].frame);
    for index in 0..params.len() {
        let Some(step) = induction_step(func, lp, header, index) else {
            continue;
        };
        if step.delta == 0 {
            continue;
        }
        let param = params[index];
        let counter_type = func.value_type(param);
        if counter_type != Type::Int64 && counter_type != Type::Any {
            continue;
        }
        // A tagged counter is unboxed in the seed block, which needs the
        // header's frame state and a frame whose values all map onto the
        // edge's arguments.
        if counter_type == Type::Any {
            let Some(frame) = header_frame else {
                continue;
            };
            if !entry_frame_ok(func, lp, &inst_block, frame, header) {
                continue;
            }
        }
        // Edges into the header from outside the loop.
        let mut outside: Vec<(Block, usize)> = Vec::new();
        for (pred, data) in func.blocks.iter() {
            if lp.member[pred] {
                continue;
            }
            let Some(term) = &data.term else {
                continue;
            };
            for (k, edge) in term.edges().iter().enumerate() {
                if edge.block == header {
                    outside.push((pred, k));
                }
            }
        }
        if outside.is_empty() {
            continue;
        }
        for &block in &lp.blocks {
            for &inst in &func.blocks[block].insts {
                if !matches!(func.insts[inst].op, Op::MulI64) {
                    continue;
                }
                let args = func.args(inst);
                if args.len() != 2 {
                    continue;
                }
                let Some((value, constant)) = constant_operand(func, args[0], args[1]) else {
                    continue;
                };
                if canon(func, value) != param
                    || func.value_type(value) != Type::Int64
                    || constant == 0
                    || constant == 1
                {
                    continue;
                }
                let Some(delta) = (step.delta as i128)
                    .checked_mul(constant as i128)
                    .and_then(|product| i64::try_from(product).ok())
                else {
                    continue;
                };
                return Some(Induction {
                    header,
                    delta,
                    mul: inst,
                    mul_block: block,
                    constant,
                    counter_index: index,
                    counter_type,
                    outside,
                    frame: if counter_type == Type::Any {
                        header_frame
                    } else {
                        None
                    },
                });
            }
        }
    }
    None
}

/// Whether every value of `frame` can be read on an edge into `header`: it is
/// a header parameter, or it is defined outside the loop (and so dominates
/// the header).
fn entry_frame_ok(
    func: &Func,
    lp: &Loop,
    inst_block: &SecondaryMap<Inst, Option<Block>>,
    frame: FrameState,
    header: Block,
) -> bool {
    let params = func.params(header);
    for value in func.frame_values(frame) {
        if params.contains(&value) {
            continue;
        }
        match func.value_def(value) {
            ValueDef::Param { block, .. } => {
                if lp.member[block] {
                    return false;
                }
            }
            ValueDef::Inst(inst) => {
                if inst_block[inst].is_some_and(|block| lp.member[block]) {
                    return false;
                }
            }
        }
    }
    true
}

/// Applies one induction-variable product.
fn apply_induction(func: &mut Func, lp: &Loop, action: Induction) {
    let Induction {
        header,
        delta,
        mul,
        mul_block,
        constant,
        counter_index,
        counter_type,
        outside,
        frame,
    } = action;
    let Some(mul_result) = func.insts[mul].result else {
        return;
    };
    let old_params: Vec<Value> = func.params(header).to_vec();
    let product = func.append_param(header, Type::Int64);

    // Every outside edge enters through a seed block that computes the
    // product once, then jumps to the header with the extra argument.
    for (pred, k) in outside {
        let seed = func.new_block();
        let seed_params: Vec<Value> = old_params
            .iter()
            .map(|&param| {
                let ty = func.value_type(param);
                func.append_param(seed, ty)
            })
            .collect();
        let counter = seed_params[counter_index];
        let raw = if counter_type == Type::Int64 {
            counter
        } else {
            let frame = clone_entry_frame(
                func,
                frame.expect("a tagged counter has a frame"),
                &old_params,
                &seed_params,
            );
            func.push_inst(
                seed,
                Op::CheckInt64,
                &[counter],
                Some(Type::Int64),
                Some(frame),
            )
            .expect("a check has a result")
        };
        let c = func
            .push_inst(
                seed,
                Op::Const(Constant::Int64(constant)),
                &[],
                Some(Type::Int64),
                None,
            )
            .expect("a constant has a result");
        let seeded = func
            .push_inst(seed, Op::MulI64, &[raw, c], Some(Type::Int64), None)
            .expect("a multiply has a result");
        let mut args = seed_params.clone();
        args.push(seeded);
        let jump = func.edge(header, &args);
        func.set_terminator(seed, Terminator::Jump(jump));
        retarget_edge(func, pred, k, seed);
    }

    // Every back edge advances the product by `step * C`.
    let mut backs: Vec<(Block, usize)> = Vec::new();
    for (pred, data) in func.blocks.iter() {
        if !lp.member[pred] {
            continue;
        }
        let Some(term) = &data.term else {
            continue;
        };
        for (k, edge) in term.edges().iter().enumerate() {
            if edge.block == header {
                backs.push((pred, k));
            }
        }
    }
    for (pred, k) in backs {
        let constant = func
            .push_inst(
                pred,
                Op::Const(Constant::Int64(delta)),
                &[],
                Some(Type::Int64),
                None,
            )
            .expect("a constant has a result");
        let advanced = func
            .push_inst(
                pred,
                Op::AddI64,
                &[product, constant],
                Some(Type::Int64),
                None,
            )
            .expect("an add has a result");
        push_edge_arg(func, pred, k, advanced);
    }

    let mut subst = Subst::default();
    subst.add(mul_result, product);
    replace_uses(func, &subst);
    func.blocks[mul_block]
        .insts
        .retain(|&candidate| candidate != mul);
}

/// Points edge `k` of `block` at `target`, keeping its arguments.
fn retarget_edge(func: &mut Func, block: Block, k: usize, target: Block) {
    let Some(mut term) = func.blocks[block].term.take() else {
        return;
    };
    {
        let mut edges = edges_mut(&mut term);
        if let Some(edge) = edges.get_mut(k) {
            edge.block = target;
        }
    }
    func.blocks[block].term = Some(term);
}

/// Appends `arg` to edge `k` of `block`'s terminator.
fn push_edge_arg(func: &mut Func, block: Block, k: usize, arg: Value) {
    let Some(mut term) = func.blocks[block].term.take() else {
        return;
    };
    {
        let mut edges = edges_mut(&mut term);
        let Some(edge) = edges.get_mut(k) else {
            func.blocks[block].term = Some(term);
            return;
        };
        let mut args = func.edge_args(edge).to_vec();
        args.push(arg);
        edge.args = EntityList::from_slice(&args, &mut func.pool);
    }
    func.blocks[block].term = Some(term);
}

/// Clones a header frame state with each header parameter read as the matching
/// seed-block parameter.
fn clone_entry_frame(
    func: &mut Func,
    frame: FrameState,
    header_params: &[Value],
    seed_params: &[Value],
) -> FrameState {
    fn clone(
        func: &mut Func,
        frame: FrameState,
        header_params: &[Value],
        seed_params: &[Value],
        memo: &mut HashMap<FrameState, FrameState>,
    ) -> FrameState {
        if let Some(&cloned) = memo.get(&frame) {
            return cloned;
        }
        let data = func.frames[frame].clone();
        let resolve = |value: Value| {
            header_params
                .iter()
                .position(|&param| param == value)
                .map(|index| seed_params[index])
                .unwrap_or(value)
        };
        let closure = resolve(data.closure);
        let registers: Vec<(u32, Value)> = data
            .registers
            .iter()
            .map(|&(reg, value)| (reg, resolve(value)))
            .collect();
        let parent = data
            .parent
            .map(|parent| clone(func, parent, header_params, seed_params, memo));
        let cloned = func.push_frame(FrameStateData {
            code: data.code,
            pc: data.pc,
            closure,
            registers,
            parent,
            call_base: data.call_base,
            returned: data.returned,
            rest: data.rest,
            inline_site: data.inline_site,
        });
        memo.insert(frame, cloned);
        cloned
    }
    let mut memo = HashMap::new();
    clone(func, frame, header_params, seed_params, &mut memo)
}
