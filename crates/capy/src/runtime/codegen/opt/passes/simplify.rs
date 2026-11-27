//! Constant folding on raw value bits, branch folding, removal of
//! trivial block parameters, block merging, and moving bitwise operations on
//! u64 words from the signed lane to the u64 lane.

use std::iter::once;

use cranelift_entity::{EntityList, SecondaryMap};

use super::{PassOutcome, Subst, reachable, replace_uses};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{
    BitOp, Cmp, Constant, Edge, Inst, Op, Repr, ShiftKind, Terminator,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = false;
    let mut work = 0;
    let rpo = func.reverse_postorder();
    for &block in &rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            changed |= fold_inst(func, inst);
        }
    }
    for &block in &rpo {
        changed |= fold_terminator(func, block);
    }

    let mut subst = Subst::default();
    // A comparison against a known-false Bool is the other Bool itself. This
    // is the consumer side of constant comparison folding: the producing
    // comparison is already decided, but a Bool has no constant form.
    changed |= fold_bool_ops(func, &mut subst);
    changed |= fold_box_compares(func, &rpo);
    changed |= fold_box_unbox(func, &mut subst);
    changed |= remove_trivial_params(func, &mut subst);
    changed |= merge_blocks(func, &mut subst);
    replace_uses(func, &subst);
    changed |= !subst.is_empty();
    changed |= widen_u64_bits(func, &rpo);

    work += func.blocks.len();
    PassOutcome { changed, work }
}

pub(super) fn insert_before(
    func: &mut Func,
    block: Block,
    before: Inst,
    op: Op,
    args: &[Value],
    ty: Type,
) -> Value {
    let value = func
        .push_inst(block, op, args, Some(ty), None)
        .expect("the operation has a result");
    let created = func.blocks[block]
        .insts
        .pop()
        .expect("push_inst appended the instruction");
    let insts = &mut func.blocks[block].insts;
    match insts.iter().position(|&inst| inst == before) {
        Some(position) => insts.insert(position, created),
        None => insts.push(created),
    }
    value
}

/// The constant an operand is, if it is a `Const` instruction.
fn constant_of(func: &Func, value: Value) -> Option<Constant> {
    match func.value_def(value) {
        ValueDef::Inst(inst) => match func.insts[inst].op {
            Op::Const(constant) => Some(constant),
            _ => None,
        },
        ValueDef::Param { .. } => None,
    }
}

fn int32_const(func: &Func, value: Value) -> Option<i32> {
    match constant_of(func, value)? {
        Constant::Int32(n) => Some(n),
        _ => None,
    }
}

fn int64_const(func: &Func, value: Value) -> Option<i64> {
    match constant_of(func, value)? {
        Constant::Int64(n) => Some(n),
        _ => None,
    }
}

/// The raw bits of an immediate constant. Heap constants are not immediates.
fn bits_const(func: &Func, value: Value) -> Option<u64> {
    match constant_of(func, value)? {
        Constant::Bits(bits) => Some(bits),
        _ => None,
    }
}

fn compare<T: PartialEq + PartialOrd>(cmp: Cmp, a: T, b: T) -> bool {
    match cmp {
        Cmp::Eq => a == b,
        Cmp::Ne => a != b,
        Cmp::Lt => a < b,
        Cmp::Le => a <= b,
        Cmp::Gt => a > b,
        Cmp::Ge => a >= b,
    }
}

/// A value's int32 value when it is a constant, or a Bool whose 0/1 flag is
/// known from constants.
fn eval_i32(func: &Func, value: Value) -> Option<i32> {
    if let Some(n) = int32_const(func, value) {
        return Some(n);
    }
    if func.value_type(value) == Type::Bool {
        return eval_bool(func, value).map(i32::from);
    }
    None
}

/// A value's int64 value when it is a constant, or a Bool whose 0/1 flag is
/// known from constants.
fn eval_i64(func: &Func, value: Value) -> Option<i64> {
    if let Some(n) = int64_const(func, value) {
        return Some(n);
    }
    if func.value_type(value) == Type::Bool {
        return eval_bool(func, value).map(i64::from);
    }
    None
}

/// The value of a Bool computed from constants, if it is one.
fn eval_bool(func: &Func, value: Value) -> Option<bool> {
    let ValueDef::Inst(inst) = func.value_def(value) else {
        return None;
    };
    let args = func.args(inst);
    match func.insts[inst].op {
        // A raw 0/1 flag is a Bool value, not a tagged Scheme boolean.
        Op::Const(Constant::Bits(bits)) if func.value_type(value) == Type::Bool => match bits {
            0 => Some(false),
            1 => Some(true),
            _ => None,
        },
        Op::CmpI32(cmp) => {
            let a = eval_i32(func, args[0])?;
            let b = eval_i32(func, args[1])?;
            Some(compare(cmp, a, b))
        }
        Op::EqBits => {
            let a = bits_const(func, args[0])?;
            let b = bits_const(func, args[1])?;
            Some(a == b)
        }
        Op::CmpI64(cmp) => {
            let a = eval_i64(func, args[0])?;
            let b = eval_i64(func, args[1])?;
            Some(compare(cmp, a, b))
        }
        Op::TestInt32 | Op::TestInt64 => {
            let bits = bits_const(func, args[0])?;
            Some(SchemeValue::from_raw(bits).int32().is_some())
        }
        // An immediate is never a pair.
        Op::TestPair | Op::TestClass(_) | Op::TestHeap => {
            if bits_const(func, args[0]).is_some() {
                return Some(false);
            }
            known_test(func, func.insts[inst].op, args[0])
        }
        _ => None,
    }
}

fn known_test(func: &Func, op: Op, value: Value) -> Option<bool> {
    let class = match op {
        Op::TestPair => ClassRef::PAIR,
        Op::TestClass(class) => class,
        _ => return None,
    };
    let ValueDef::Inst(def) = func.value_def(value) else {
        return None;
    };
    match func.insts[def].op {
        Op::Box(Repr::Int32 | Repr::Double) | Op::BoolToValue => Some(false),
        Op::Box(Repr::Int64 | Repr::Int128) | Op::BoxU64 => {
            (class != ClassRef::BIGINT).then_some(false)
        }
        Op::AllocPair => Some(class == ClassRef::PAIR),
        Op::AllocClosure(_) => Some(class == ClassRef::CLOSURE),
        Op::AllocVector => Some(class == ClassRef::VECTOR),
        Op::AllocInstance { class: id, .. } => Some(class.class_id() == id),
        _ => None,
    }
}

/// Rewrites `ne false, x` (and its mirror) to `x` when `x` is a Bool flag,
/// and a `select` of one value or on a known condition to that value.
/// The false operand is a comparison whose constant operands decide it; a
/// Bool has no constant form, so the producer stays until DCE sees it unused.
fn fold_bool_ops(func: &Func, subst: &mut Subst) -> bool {
    let mut changed = false;
    for (_, block) in func.blocks.iter() {
        for &inst in &block.insts {
            let Some(result) = func.insts[inst].result else {
                continue;
            };
            // A select of one value, or on a known condition, is that value.
            if func.insts[inst].op == Op::Select {
                let args = func.args(inst);
                let pick = if args[1] == args[2] {
                    Some(args[1])
                } else {
                    eval_bool(func, args[0]).map(|c| if c { args[1] } else { args[2] })
                };
                if let Some(v) = pick {
                    subst.add(result, v);
                    changed = true;
                }
                continue;
            }
            let ne = match func.insts[inst].op {
                Op::CmpI32(Cmp::Ne) | Op::CmpI64(Cmp::Ne) => true,
                _ => false,
            };
            if !ne {
                continue;
            }
            let args = func.args(inst);
            let (a, b) = (args[0], args[1]);
            if eval_i64(func, a) == Some(0) && func.value_type(b) == Type::Bool {
                subst.add(result, b);
                changed = true;
            } else if eval_i64(func, b) == Some(0) && func.value_type(a) == Type::Bool {
                subst.add(result, a);
                changed = true;
            }
        }
    }
    changed
}

fn fold_box_unbox(func: &Func, subst: &mut Subst) -> bool {
    let mut changed = false;
    for (_, block) in func.blocks.iter() {
        for &inst in &block.insts {
            let want = match func.insts[inst].op {
                Op::Unbox(r @ (Repr::Int32 | Repr::Int64 | Repr::Double)) => r,
                _ => continue,
            };
            let Some(result) = func.insts[inst].result else {
                continue;
            };
            let args = func.args(inst);
            let [arg] = args else { continue };
            let ValueDef::Inst(def) = func.value_def(*arg) else {
                continue;
            };
            if !matches!(func.insts[def].op, Op::Box(r2) if r2 == want) {
                continue;
            }
            let inner = func.args(def);
            let [source] = inner else { continue };
            subst.add(result, *source);
            changed = true;
        }
    }
    changed
}

fn fold_box_compares(func: &mut Func, rpo: &[Block]) -> bool {
    let mut changed = false;
    for &block in rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            if func.insts[inst].op != Op::EqBits {
                continue;
            }
            let args = func.args(inst).to_vec();
            let boxed = |value: Value| -> Option<(Repr, Value)> {
                let ValueDef::Inst(def) = func.value_def(value) else {
                    return None;
                };
                match func.insts[def].op {
                    Op::Box(repr @ (Repr::Int32 | Repr::Int64)) => Some((repr, func.args(def)[0])),
                    _ => None,
                }
            };
            let fixnum = |value: Value| -> Option<i32> {
                SchemeValue::from_raw(bits_const(func, value)?).int32()
            };
            // The compare, its left operand and its right operand: a value,
            // or an immediate to create.
            let rewrite = match (boxed(args[0]), boxed(args[1])) {
                (Some((Repr::Int32, a)), Some((Repr::Int32, b))) => {
                    Some((Op::CmpI32(Cmp::Eq), a, Err(b)))
                }
                (Some((repr, a)), None) => fixnum(args[1]).map(|k| payload_compare(repr, a, k)),
                (None, Some((repr, b))) => fixnum(args[0]).map(|k| payload_compare(repr, b, k)),
                _ => None,
            };
            let Some((op, lhs, rhs)) = rewrite else {
                continue;
            };
            let rhs = match rhs {
                Ok((constant, ty)) => {
                    insert_before(func, block, inst, Op::Const(constant), &[], ty)
                }
                Err(value) => value,
            };
            let data = &mut func.insts[inst];
            data.op = op;
            data.args = EntityList::from_slice(&[lhs, rhs], &mut func.pool);
            data.frame = None;
            changed = true;
        }
    }
    changed
}

/// The compare of a boxed integer's raw operand with a fixnum's payload.
fn payload_compare(
    repr: Repr,
    raw: Value,
    payload: i32,
) -> (Op, Value, Result<(Constant, Type), Value>) {
    match repr {
        Repr::Int32 => (
            Op::CmpI32(Cmp::Eq),
            raw,
            Ok((Constant::Int32(payload), Type::Int32)),
        ),
        _ => (
            Op::CmpI64(Cmp::Eq),
            raw,
            Ok((Constant::Int64(i64::from(payload)), Type::Int64)),
        ),
    }
}

/// Folds the instruction into a constant when its operands are constants.
/// Returns whether it changed.
fn fold_inst(func: &mut Func, inst: Inst) -> bool {
    let op = func.insts[inst].op;
    let args: Vec<Value> = func.args(inst).to_vec();
    let folded = match op {
        // A checked fixnum from an immediate is known. Anything else keeps the
        // guard, which must deopt.
        Op::CheckInt32 | Op::Unbox(Repr::Int32) => bits_const(func, args[0])
            .and_then(|bits| SchemeValue::from_raw(bits).int32())
            .map(Constant::Int32),
        Op::CheckInt64 => bits_const(func, args[0])
            .and_then(|bits| SchemeValue::from_raw(bits).int32())
            .map(|n| Constant::Int64(n as i64)),
        Op::CheckDouble | Op::Unbox(Repr::Double) => bits_const(func, args[0])
            .and_then(|bits| SchemeValue::from_raw(bits).flonum())
            .map(|f| Constant::Double(f.to_bits())),
        Op::Box(Repr::Int32) => {
            int32_const(func, args[0]).map(|n| Constant::Bits(SchemeValue::from_i32(n).bits()))
        }
        // Only an in-range value folds: a wider one makes the box deopt so
        // that baseline builds the bignum.
        Op::Box(Repr::Int64) => int64_const(func, args[0])
            .and_then(|n| i32::try_from(n).ok())
            .map(|n| Constant::Bits(SchemeValue::from_i32(n).bits())),
        Op::BoxU64 => int64_const(func, args[0])
            .and_then(|n| i32::try_from(n).ok())
            .filter(|n| *n >= 0)
            .map(|n| Constant::Bits(SchemeValue::from_i32(n).bits())),
        Op::CheckU64 => bits_const(func, args[0])
            .and_then(|bits| SchemeValue::from_raw(bits).int32())
            .filter(|n| *n >= 0)
            .map(|n| Constant::Int64(i64::from(n))),
        Op::Int64ToDouble => {
            int64_const(func, args[0]).map(|n| Constant::Double((n as f64).to_bits()))
        }
        // An overflowing operation deopts, so only the non-overflowing case
        // folds.
        Op::AddI32Ovf | Op::SubI32Ovf | Op::MulI32Ovf => {
            let a = int32_const(func, args[0]);
            let b = int32_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) => match op {
                    Op::AddI32Ovf => a.checked_add(b),
                    Op::SubI32Ovf => a.checked_sub(b),
                    _ => a.checked_mul(b),
                }
                .map(Constant::Int32),
                _ => None,
            }
        }
        Op::AddI32 | Op::SubI32 | Op::MulI32 | Op::BitI32(_) => {
            let a = int32_const(func, args[0]);
            let b = int32_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) => Some(Constant::Int32(match op {
                    Op::AddI32 => a.wrapping_add(b),
                    Op::SubI32 => a.wrapping_sub(b),
                    Op::MulI32 => a.wrapping_mul(b),
                    Op::BitI32(BitOp::And) => a & b,
                    Op::BitI32(BitOp::Or) => a | b,
                    _ => a ^ b,
                })),
                _ => None,
            }
        }
        Op::BitI64(bit) => {
            let a = int64_const(func, args[0]);
            let b = int64_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) => Some(Constant::Int64(match bit {
                    BitOp::And => a & b,
                    BitOp::Or => a | b,
                    BitOp::Xor => a ^ b,
                })),
                _ => None,
            }
        }
        Op::ShiftI32(kind, n) => {
            int32_const(func, args[0]).map(|a| Constant::Int32(shift_i32(kind, n, a)))
        }
        Op::ShiftI64(kind, n) => {
            int64_const(func, args[0]).map(|a| Constant::Int64(shift_i64(kind, n, a)))
        }
        // An overflowing shift deopts, so only the exact case folds.
        Op::ShlI32Ovf(n) => int32_const(func, args[0])
            .and_then(|a| i32::try_from((a as i64) << n).ok())
            .map(Constant::Int32),
        Op::ShiftI32Var(kind) => match (int32_const(func, args[0]), int32_const(func, args[1])) {
            (Some(a), Some(n)) => Some(Constant::Int32(shift_i32(kind, (n & 31) as u8, a))),
            _ => None,
        },
        // A count outside `0..32` deopts, as does an overflow: only the exact
        // case folds.
        Op::ShlI32OvfVar => match (int32_const(func, args[0]), int32_const(func, args[1])) {
            (Some(a), Some(n)) if (0..32).contains(&n) => {
                i32::try_from((a as i64) << n).ok().map(Constant::Int32)
            }
            _ => None,
        },
        Op::CharFromCode => int32_const(func, args[0])
            .and_then(|n| u32::try_from(n).ok())
            .and_then(char::from_u32)
            .map(|c| Constant::Bits(SchemeValue::from_char(c).bits())),
        // The total 64-bit operations cannot fail for any operands, so they
        // fold with wrapping semantics (the machine add/sub/mul).
        Op::AddI64 | Op::SubI64 | Op::MulI64 => {
            let a = int64_const(func, args[0]);
            let b = int64_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) => Some(Constant::Int64(match op {
                    Op::AddI64 => a.wrapping_add(b),
                    Op::SubI64 => a.wrapping_sub(b),
                    _ => a.wrapping_mul(b),
                })),
                _ => None,
            }
        }
        // A checked 64-bit operation deopts on overflow, so only the
        // non-overflowing case folds.
        Op::AddI64Ovf | Op::SubI64Ovf | Op::MulI64Ovf => {
            let a = int64_const(func, args[0]);
            let b = int64_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) => match op {
                    Op::AddI64Ovf => a.checked_add(b),
                    Op::SubI64Ovf => a.checked_sub(b),
                    _ => a.checked_mul(b),
                }
                .map(Constant::Int64),
                _ => None,
            }
        }
        Op::QuotI64 | Op::RemI64 | Op::ModI64 => {
            let a = int64_const(func, args[0]);
            let b = int64_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) if b != 0 && !(a == i64::MIN && b == -1) => {
                    let r = a % b;
                    let value = match op {
                        Op::QuotI64 => a / b,
                        Op::RemI64 => r,
                        _ if r != 0 && (r < 0) != (b < 0) => r + b,
                        _ => r,
                    };
                    Some(Constant::Int64(value))
                }
                _ => None,
            }
        }
        // A division deopts on a zero divisor and on `MIN / -1`, so only the
        // other cases fold.
        Op::QuotI32 | Op::RemI32 | Op::ModI32 => {
            let a = int32_const(func, args[0]);
            let b = int32_const(func, args[1]);
            match (a, b) {
                (Some(a), Some(b)) if b != 0 && !(a == i32::MIN && b == -1) => {
                    let r = a % b;
                    let value = match op {
                        Op::QuotI32 => a / b,
                        Op::RemI32 => r,
                        // The modulo takes the sign of the divisor.
                        _ if r != 0 && (r < 0) != (b < 0) => r + b,
                        _ => r,
                    };
                    Some(Constant::Int32(value))
                }
                _ => None,
            }
        }

        Op::TestPair | Op::TestClass(_) => {
            known_test(func, op, args[0]).map(|known| Constant::Bits(u64::from(known)))
        }
        Op::BoolToValue => eval_bool(func, args[0]).map(|b| {
            let bits = if b {
                SchemeValue::VALUE_TRUE
            } else {
                SchemeValue::VALUE_FALSE
            };
            Constant::Bits(bits as u64)
        }),
        _ => None,
    };
    let Some(constant) = folded else {
        return false;
    };
    let data = &mut func.insts[inst];
    data.op = Op::Const(constant);
    data.args = EntityList::new();
    data.frame = None;
    true
}

/// The flag `b` when `value` is `eq.bits (box.bool b) #f`, which is `!b`.
fn negated_flag(func: &Func, value: Value) -> Option<Value> {
    let ValueDef::Inst(inst) = func.value_def(value) else {
        return None;
    };
    if func.insts[inst].op != Op::EqBits {
        return None;
    }
    let args = func.args(inst);
    if bits_const(func, args[1])? != SchemeValue::VALUE_FALSE as u64 {
        return None;
    }
    let ValueDef::Inst(boxed) = func.value_def(args[0]) else {
        return None;
    };
    if func.insts[boxed].op != Op::BoolToValue {
        return None;
    }
    Some(func.args(boxed)[0])
}

/// Folds a branch or switch on a constant into a jump, and a branch whose
/// arms agree into a jump. Returns whether it changed.
fn fold_terminator(func: &mut Func, block: Block) -> bool {
    let Some(term) = func.blocks[block].term.clone() else {
        return false;
    };
    let jump = match &term {
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => {
            // A branch on `(bits == #f)` of a boxed flag tests the flag
            // itself, with the arms swapped.
            if let Some(flag) = negated_flag(func, *cond) {
                func.set_terminator(
                    block,
                    Terminator::Branch {
                        cond: flag,
                        then: *otherwise,
                        otherwise: *then,
                    },
                );
                return true;
            }
            if let Some(taken) = eval_bool(func, *cond) {
                Some(if taken { *then } else { *otherwise })
            } else if then.block == otherwise.block
                && func.edge_args(then) == func.edge_args(otherwise)
            {
                Some(*then)
            } else {
                None
            }
        }
        Terminator::Switch {
            value,
            cases,
            default,
            ..
        } => int32_const(func, *value).map(|n| {
            cases
                .iter()
                .find(|(case, _)| *case == n)
                .map_or(*default, |(_, edge)| *edge)
        }),
        _ => None,
    };
    match jump {
        Some(edge) => {
            func.set_terminator(block, Terminator::Jump(edge));
            true
        }
        None => false,
    }
}

pub(super) fn edges_mut(term: &mut Terminator) -> Vec<&mut Edge> {
    match term {
        Terminator::Jump(edge) => vec![edge],
        Terminator::Branch {
            then, otherwise, ..
        } => vec![then, otherwise],
        Terminator::Switch { cases, default, .. } => cases
            .iter_mut()
            .map(|(_, edge)| edge)
            .chain(once(default))
            .collect(),
        Terminator::Return(_)
        | Terminator::ReturnMany { .. }
        | Terminator::TailCall { .. }
        | Terminator::Deopt { .. }
        | Terminator::Unreachable => vec![],
    }
}

/// Removes a block parameter that receives one value on every reachable
/// incoming edge, apart from the block's own back edges. Its uses take that
/// value. The entry's parameters stay, since they are bound to registers.
fn remove_trivial_params(func: &mut Func, subst: &mut Subst) -> bool {
    let live = reachable(func);
    // The argument lists of the reachable edges into each block.
    let mut incoming: SecondaryMap<Block, Vec<Vec<Value>>> = SecondaryMap::new();
    for (block, data) in func.blocks.iter() {
        if !live[block] {
            continue;
        }
        if let Some(term) = &data.term {
            for edge in term.edges() {
                incoming[edge.block].push(func.edge_args(&edge).to_vec());
            }
        }
    }

    let mut removed: SecondaryMap<Block, Vec<bool>> = SecondaryMap::new();
    let mut any = false;
    for block in func.blocks.keys() {
        if block == func.entry || !live[block] || incoming[block].is_empty() {
            continue;
        }
        let params = func.params(block).to_vec();
        let mut flags = vec![false; params.len()];
        for (index, &param) in params.iter().enumerate() {
            let mut same: Option<Value> = None;
            let mut trivial = true;
            for args in &incoming[block] {
                let value = subst.resolve(args[index]);
                if value == param {
                    continue;
                }
                match same {
                    None => same = Some(value),
                    Some(prev) if prev == value => {}
                    Some(_) => {
                        trivial = false;
                        break;
                    }
                }
            }
            if let (true, Some(value)) = (trivial, same) {
                subst.add(param, value);
                flags[index] = true;
            }
        }
        if flags.iter().any(|&flag| flag) {
            removed[block] = flags;
            any = true;
        }
    }
    if !any {
        return false;
    }

    // Drop the removed arguments from every edge, reachable or not, so every
    // edge still matches its target's parameters.
    for block in func.blocks.keys().collect::<Vec<_>>() {
        let Some(mut term) = func.blocks[block].term.take() else {
            continue;
        };
        for edge in edges_mut(&mut term) {
            let flags = &removed[edge.block];
            if flags.is_empty() {
                continue;
            }
            let kept: Vec<Value> = func
                .edge_args(edge)
                .iter()
                .zip(flags)
                .filter(|(_, flag)| !**flag)
                .map(|(&value, _)| value)
                .collect();
            edge.args = EntityList::from_slice(&kept, &mut func.pool);
        }
        func.blocks[block].term = Some(term);
    }
    for block in func.blocks.keys().collect::<Vec<_>>() {
        let flags = &removed[block];
        if flags.is_empty() {
            continue;
        }
        let kept: Vec<Value> = func
            .params(block)
            .iter()
            .zip(flags)
            .filter(|(_, flag)| !**flag)
            .map(|(&value, _)| value)
            .collect();
        for (index, &value) in kept.iter().enumerate() {
            func.values[value].def = ValueDef::Param {
                block,
                index: index as u32,
            };
        }
        func.blocks[block].params = EntityList::from_slice(&kept, &mut func.pool);
    }
    true
}

/// Appends a block to its only predecessor when that predecessor jumps to
/// it. The merged block's parameters become the jump's arguments. A merged
/// block is left unreachable, with no instructions, for the DCE pass.
fn merge_blocks(func: &mut Func, subst: &mut Subst) -> bool {
    // Every edge into a block counts, reachable or not. An unreachable edge
    // would otherwise be left aimed at a block whose parameters were removed.
    let mut pred_edges: SecondaryMap<Block, u32> = SecondaryMap::new();
    for (_, data) in func.blocks.iter() {
        if let Some(term) = &data.term {
            for edge in term.edges() {
                pred_edges[edge.block] += 1;
            }
        }
    }

    let mut changed = false;
    let mut merged: SecondaryMap<Block, bool> = SecondaryMap::new();
    for block in func.reverse_postorder() {
        if merged[block] {
            continue;
        }
        loop {
            let Some(Terminator::Jump(edge)) = func.blocks[block].term.clone() else {
                break;
            };
            let target = edge.block;
            if target == block || target == func.entry || pred_edges[target] != 1 || merged[target]
            {
                break;
            }
            let params = func.params(target).to_vec();
            let args = func.edge_args(&edge).to_vec();
            for (&param, &arg) in params.iter().zip(&args) {
                subst.add(param, arg);
            }
            let moved = std::mem::take(&mut func.blocks[target].insts);
            func.blocks[block].insts.extend(moved);
            let next = func.blocks[target].term.take();
            func.blocks[block].term = next;
            func.blocks[target].params = EntityList::new();
            func.blocks[target].term = Some(Terminator::Unreachable);
            merged[target] = true;
            changed = true;
        }
    }
    changed
}

/// The value of [`Op::ShiftI32`] on a constant.
pub(crate) fn shift_i32(kind: ShiftKind, n: u8, a: i32) -> i32 {
    let n = u32::from(n) & 31;
    match kind {
        ShiftKind::Shl => a.wrapping_shl(n),
        ShiftKind::Sar => a >> n,
        ShiftKind::Shr => ((a as u32) >> n) as i32,
        ShiftKind::Rotr => (a as u32).rotate_right(n) as i32,
    }
}

/// The value of [`Op::ShiftI64`] on a constant.
pub(crate) fn shift_i64(kind: ShiftKind, n: u8, a: i64) -> i64 {
    let n = u32::from(n) & 63;
    match kind {
        ShiftKind::Shl => a.wrapping_shl(n),
        ShiftKind::Sar => a >> n,
        ShiftKind::Shr => ((a as u64) >> n) as i64,
        ShiftKind::Rotr => (a as u64).rotate_right(n) as i64,
    }
}

/// Moves a signed-lane bitwise operation on u64 words to the u64 lane.
fn widen_u64_bits(func: &mut Func, rpo: &[Block]) -> bool {
    let mut changed = false;
    while widen_u64_bits_once(func, rpo) {
        changed = true;
    }
    changed
}

fn widen_u64_bits_once(func: &mut Func, rpo: &[Block]) -> bool {
    let mut uses: std::collections::HashMap<Value, u32> = std::collections::HashMap::new();
    for &block in rpo {
        for &inst in &func.blocks[block].insts {
            for &arg in func.args(inst) {
                *uses.entry(arg).or_default() += 1;
            }
            if let Some(frame) = func.insts[inst].frame {
                for value in func.frame_values(frame) {
                    *uses.entry(value).or_default() += 1;
                }
            }
        }
        if let Some(term) = &func.blocks[block].term {
            for value in func.terminator_uses(term) {
                *uses.entry(value).or_default() += 1;
            }
            if let Terminator::TailCall { frame, .. } | Terminator::Deopt { frame } = term {
                for value in func.frame_values(*frame) {
                    *uses.entry(value).or_default() += 1;
                }
            }
        }
    }
    let used_once = |v: Value| uses.get(&v).copied() == Some(1);
    let def = |func: &Func, v: Value| match func.value_def(v) {
        ValueDef::Inst(inst) => Some(inst),
        _ => None,
    };
    // The raw word behind operand `v`, with whether it may be at or above
    // 2^63, and the guard to drop.
    let word = |func: &Func, v: Value| -> Option<(Value, bool, Option<Inst>)> {
        let inst = def(func, v)?;
        match func.insts[inst].op {
            Op::Const(Constant::Int64(n)) if n >= 0 => Some((v, false, None)),
            Op::CheckBigIntFits64 => {
                let boxed = func.args(inst)[0];
                let source = def(func, boxed)?;
                (func.insts[source].op == Op::BoxU64)
                    .then(|| (func.args(source)[0], true, Some(inst)))
            }
            _ => None,
        }
    };
    let mut rewrites = Vec::new();
    for &block in rpo {
        for &inst in &func.blocks[block].insts {
            if func.insts[inst].op != Op::Box(Repr::Int64) {
                continue;
            }
            let raw = func.args(inst)[0];
            let Some(bit) = def(func, raw) else { continue };
            if !matches!(func.insts[bit].op, Op::BitI64(_)) || !used_once(raw) {
                continue;
            }
            let args = func.args(bit);
            let (Some(a), Some(b)) = (word(func, args[0]), word(func, args[1])) else {
                continue;
            };
            if !a.1 && !b.1 {
                continue;
            }
            rewrites.push((inst, bit, [a.0, b.0], [a.2, b.2]));
        }
    }
    // A guard goes once every use of its result is a rewritten operand;
    // another user still reads the signed value it checks.
    let mut dropped = std::collections::HashSet::new();
    for (boxed, bit, operands, guards) in &rewrites {
        func.insts[*bit].args = EntityList::from_slice(operands, &mut func.pool);
        func.insts[*boxed].op = Op::BoxU64;
        for guard in guards.iter().flatten() {
            let result = func.insts[*guard].result.expect("a check has a result");
            let count = uses.get_mut(&result).expect("the operand is a use");
            *count -= 1;
            if *count == 0 {
                dropped.insert(*guard);
            }
        }
    }
    if !dropped.is_empty() {
        for &block in rpo {
            func.blocks[block]
                .insts
                .retain(|inst| !dropped.contains(inst));
        }
    }
    !rewrites.is_empty()
}
