//! Loop predication of bounds checks.
//!
//! A counted loop that indexes an invariant vector with its counter checks
//! `index < length` on every iteration. When the counter runs from `start` up
//! to a bound the loop tests, the checks hold for all iterations if two facts
//! hold at loop entry: `0 <= start` and `bound <= length`. This pass proves
//! them once in the preheader and drops the per-iteration checks. It is the
//! range-check elimination of Graal's `LoopPredication`, with HotSpot's
//! `RangeCheckElimination` shape: one test before the loop in place of one
//! check in it.
//!
//! The loop must test its counter in the header, against an invariant bound,
//! and advance the counter by `+1` ([`unroll::counter`]). It continues on
//! `counter < bound`, or on `counter != bound`, which also needs
//! `start <= bound` so the counter reaches the bound instead of passing it.
//! Only a check in a body block is rewritten: it runs after the header's test
//! of the same iteration, so the counter is below the bound there. The index
//! must be the counter parameter itself, and the vector a value defined
//! outside the loop with a known vector class (its length never changes).
//!
//! A failing test deopts to the state at loop entry (the header's first
//! frame, see [`licm::entry_frame_of`](super::licm::entry_frame_of)). Baseline then runs the loop and
//! raises its own error at the iteration that fails, so error timing is kept.
//! The cost is a deopt the original loop would not take when the loop stops
//! early before reaching a bad index. The exit profile of the loop's pc backs
//! the speculation off after a few such exits.

use std::collections::HashSet;

use cranelift_entity::EntityList;

use super::PassOutcome;
use super::forward::def_inst;
use super::licm::{entry_frame_in, entry_frame_of};
use super::loops::{ensure_preheader, find_loops, header_param_invariant, inst_blocks, int_const};
use super::unroll::{Counter, counter};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Constant, Inst, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};

pub(super) fn run(func: &mut Func) -> PassOutcome {
    if !opt().predicate {
        return PassOutcome::default();
    }
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    for lp in &loops {
        work += lp.blocks.len();
        if predicate_loop(func, lp) {
            return PassOutcome {
                changed: true,
                work: work + func.insts.len(),
            };
        }
    }
    PassOutcome {
        changed: false,
        work,
    }
}

/// How the loop continues, as a relation `counter ? bound`.
fn continue_relation(counter: &Counter) -> Cmp {
    let mut cmp = counter.cmp;
    if !counter.counter_is_lhs {
        cmp = match cmp {
            Cmp::Lt => Cmp::Gt,
            Cmp::Le => Cmp::Ge,
            Cmp::Gt => Cmp::Lt,
            Cmp::Ge => Cmp::Le,
            other => other,
        };
    }
    if !counter.continue_is_then {
        cmp = match cmp {
            Cmp::Eq => Cmp::Ne,
            Cmp::Ne => Cmp::Eq,
            Cmp::Lt => Cmp::Ge,
            Cmp::Le => Cmp::Gt,
            Cmp::Gt => Cmp::Le,
            Cmp::Ge => Cmp::Lt,
        };
    }
    cmp
}

/// What a check reads: the invariant object that must be a vector, and
/// whether the check itself is the tagged or the raw form.
struct Candidate {
    inst: Inst,
    /// The value that is checked to be a vector, defined outside the loop or
    /// an invariant header parameter.
    base: Value,
    raw: bool,
}

fn predicate_loop(func: &mut Func, lp: &super::loops::Loop) -> bool {
    let Some(counter) = counter(func, lp) else {
        return false;
    };
    if counter.step != 1 {
        return false;
    }
    let relation = continue_relation(&counter);
    if !matches!(relation, Cmp::Lt | Cmp::Ne) {
        return false;
    }
    let inst_block = inst_blocks(func);
    // A value the preheader can read: defined outside the loop, or a header
    // parameter that every back edge passes through unchanged.
    let readable = |value: Value| match func.value_def(value) {
        ValueDef::Param { block, .. } => {
            !lp.member[block] || header_param_invariant(func, lp, value)
        }
        ValueDef::Inst(def) => !inst_block[def].is_some_and(|block| lp.member[block]),
    };
    if !readable(counter.bound) {
        return false;
    }

    let mut checks: Vec<Candidate> = Vec::new();
    for &block in &lp.blocks {
        if block == lp.header {
            continue;
        }
        for &inst in &func.blocks[block].insts {
            let op = func.insts[inst].op;
            let raw = match op {
                Op::CheckIndex => false,
                Op::CheckIndexRaw => true,
                _ => continue,
            };
            let args = func.args(inst);
            let (vector, index) = (args[0], args[1]);
            let wanted = if raw { Type::Int64 } else { Type::Any };
            if index != counter.param || func.value_type(index) != wanted {
                continue;
            }
            // The vector is an invariant value, or the result of a vector
            // class check on one.
            let base = match def_inst(func, vector) {
                Some(def) if func.insts[def].op == Op::CheckClass(ClassRef::VECTOR) => {
                    func.args(def)[0]
                }
                _ if func.value_type(vector) == Type::Heap(ClassRef::VECTOR) => vector,
                _ => continue,
            };
            if readable(base) {
                checks.push(Candidate { inst, base, raw });
            }
        }
    }
    if checks.is_empty() {
        return false;
    }
    let header_params = func.params(lp.header).to_vec();
    let Some(position) = header_params.iter().position(|&p| p == counter.param) else {
        return false;
    };
    let Some(entry) = entry_frame_of(func, lp) else {
        return false;
    };
    let Some((pre, entry_args)) = ensure_preheader(func, lp) else {
        return false;
    };
    let frame = entry_frame_in(func, lp, pre, &entry_args, entry);
    // The value `value` has in the preheader.
    let in_pre = |value: Value| match header_params.iter().position(|&p| p == value) {
        Some(index) => entry_args[index],
        None => value,
    };

    // The tests, in the preheader. Each is a raw int64 comparison.
    let start_entry = entry_args[position];
    let start = if func.value_type(start_entry) == Type::Int64 {
        start_entry
    } else {
        match int_const(func, start_entry) {
            Some(n) => emit_const(func, pre, n),
            None => emit(
                func,
                pre,
                Op::CheckInt64,
                &[start_entry],
                Type::Int64,
                Some(frame),
            ),
        }
    };
    let bound = in_pre(counter.bound);
    let zero = emit_const(func, pre, 0);
    let mut tests: Vec<Value> = Vec::new();
    tests.push(emit(
        func,
        pre,
        Op::CmpI64(Cmp::Ge),
        &[start, zero],
        Type::Bool,
        None,
    ));
    if relation == Cmp::Ne {
        tests.push(emit(
            func,
            pre,
            Op::CmpI64(Cmp::Le),
            &[start, bound],
            Type::Bool,
            None,
        ));
    }
    let mut seen: HashSet<Value> = HashSet::new();
    for check in &checks {
        if !seen.insert(check.base) {
            continue;
        }
        let base = in_pre(check.base);
        let vector = if func.value_type(base) == Type::Heap(ClassRef::VECTOR) {
            base
        } else {
            emit(
                func,
                pre,
                Op::CheckClass(ClassRef::VECTOR),
                &[base],
                Type::Heap(ClassRef::VECTOR),
                Some(frame),
            )
        };
        let length = emit(
            func,
            pre,
            Op::VectorLength,
            &[vector],
            Type::Int32,
            Some(frame),
        );
        let tagged = emit(func, pre, Op::Box(Repr::Int32), &[length], Type::Any, None);
        let length = emit(
            func,
            pre,
            Op::CheckInt64,
            &[tagged],
            Type::Int64,
            Some(frame),
        );
        tests.push(emit(
            func,
            pre,
            Op::CmpI64(Cmp::Le),
            &[bound, length],
            Type::Bool,
            None,
        ));
    }

    // pre: test1 -> test2 -> ... -> header; any failure deopts at entry.
    let deopt = func.new_block();
    func.set_terminator(deopt, Terminator::Deopt { frame });
    let mut current: Block = pre;
    let last = tests.len() - 1;
    for (index, &test) in tests.iter().enumerate() {
        let (next, args): (Block, &[Value]) = if index == last {
            (lp.header, &entry_args)
        } else {
            (func.new_block(), &[])
        };
        let then = func.edge(next, args);
        let otherwise = func.edge(deopt, &[]);
        func.set_terminator(
            current,
            Terminator::Branch {
                cond: test,
                then,
                otherwise,
            },
        );
        current = next;
    }

    for check in checks {
        let index = func.args(check.inst)[1];
        let data = &mut func.insts[check.inst];
        data.op = if check.raw {
            Op::Int64ToInt32
        } else {
            Op::Unbox(Repr::Int32)
        };
        data.args = EntityList::from_slice(&[index], &mut func.pool);
        data.frame = None;
    }
    true
}

fn emit(
    func: &mut Func,
    block: Block,
    op: Op,
    args: &[Value],
    ty: Type,
    frame: Option<FrameState>,
) -> Value {
    func.push_inst(block, op, args, Some(ty), frame)
        .expect("the operation has a result")
}

fn emit_const(func: &mut Func, block: Block, n: i64) -> Value {
    emit(
        func,
        block,
        Op::Const(Constant::Int64(n)),
        &[],
        Type::Int64,
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{block, branch, frame, jump, push, ret};

    /// `for (i = 0; i < n; i++) { check.index.raw v (i or k) }` with `v`
    /// and `n` parameters of the function. Returns the function and the check.
    fn build(index_is_counter: bool) -> (Func<'static>, Inst) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let vector = func.append_param(entry, Type::Heap(ClassRef::VECTOR));
        let n = func.append_param(entry, Type::Int64);
        let start = push(
            &mut func,
            entry,
            Op::Const(Constant::Int64(0)),
            &[],
            Type::Int64,
        );
        let key = push(
            &mut func,
            entry,
            Op::Const(Constant::Int64(3)),
            &[],
            Type::Int64,
        );
        let header = block(&mut func);
        let i = func.append_param(header, Type::Int64);
        let body = block(&mut func);
        let exit = block(&mut func);
        jump(&mut func, entry, header, &[start]);
        let header_frame = frame(&mut func, vector);
        func.push_inst(header, Op::Safepoint, &[], None, Some(header_frame));
        let cond = push(&mut func, header, Op::CmpI64(Cmp::Lt), &[i, n], Type::Bool);
        branch(&mut func, header, cond, body, &[], exit, &[]);
        let check_frame = frame(&mut func, vector);
        let index = if index_is_counter { i } else { key };
        let check = func
            .push_inst(
                body,
                Op::CheckIndexRaw,
                &[vector, index],
                Some(Type::Int32),
                Some(check_frame),
            )
            .expect("a check has a result");
        let one = push(
            &mut func,
            body,
            Op::Const(Constant::Int64(1)),
            &[],
            Type::Int64,
        );
        let next = push(&mut func, body, Op::AddI64, &[i, one], Type::Int64);
        jump(&mut func, body, header, &[next]);
        ret(&mut func, exit, n);
        let crate::runtime::codegen::opt::ir::func::ValueDef::Inst(inst) = func.value_def(check)
        else {
            unreachable!("a check is an instruction result")
        };
        (func, inst)
    }

    fn loop_of(func: &Func) -> super::super::loops::Loop {
        let dom = func.dom_tree();
        find_loops(func, &dom)
            .pop()
            .expect("the function has a loop")
    }

    #[test]
    fn counter_index_check_is_predicated() {
        let (mut func, check) = build(true);
        verify(&func).expect("the test function is valid");
        let lp = loop_of(&func);
        assert!(predicate_loop(&mut func, &lp));
        verify(&func).expect("the predicated function is valid");
        assert_eq!(func.insts[check].op, Op::Int64ToInt32);
        assert!(func.insts[check].frame.is_none());
        // The preheader now ends in a test, not a jump.
        assert!(matches!(
            func.blocks[func.entry].term,
            Some(Terminator::Branch { .. })
        ));
    }

    #[test]
    fn other_index_check_stays() {
        let (mut func, check) = build(false);
        let lp = loop_of(&func);
        assert!(!predicate_loop(&mut func, &lp));
        assert_eq!(func.insts[check].op, Op::CheckIndexRaw);
    }
}
