//! Loop versioning on invariant guards.
//!
//! A deopting guard cannot simply move out of a loop body: unlike a header
//! guard it may not run on the first iteration, and deopting into its frame
//! state (in particular an inlined callee frame) would resume a call the
//! caller had not made. Versioning avoids motion entirely. It duplicates the
//! loop and puts a pure *test* on the preheader:
//!
//! ```text
//!   preheader: t = test(operand)          ; total, cannot fail
//!              branch t -> fast, slow
//!   fast:      clone of the loop without the guard
//!   slow:      the original loop, guards untouched
//! ```
//!
//! The test never errors and never deopts, so evaluating it on paths that
//! never reach the guard changes nothing; the slow loop keeps the original
//! error timing. When the operand is loop-invariant the test's answer is the
//! guard's answer on every iteration, so the fast clone can drop the guard.
//!

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::PassOutcome;
use super::lcssa::form_lcssa;
use super::loops::{
    Loop, ensure_preheader, find_loops, header_param_invariant, inst_blocks, outside_used_values,
};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Edge, Inst, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;

/// The largest loop, in instructions, that one run will duplicate.
const MAX_LOOP_INSTS: usize = 96;
/// The most guard predicates one preheader carries.
const MAX_GUARDS: usize = 8;

/// A guard that the fast clone drops, with the total test that justifies it.
struct Guard {
    inst: Inst,
    test: Op,
    operand: Value,
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    if !opt().version {
        return PassOutcome::default();
    }
    run_enabled(func)
}

fn run_enabled(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    for lp in &loops {
        work += lp.blocks.len();
        // The slow loop keeps its guards; marking it stops this pass from
        // cloning it again next round.
        if func.versioned.contains(&lp.header) {
            continue;
        }
        let live_outs: HashSet<Value> = outside_used_values(func, lp).into_iter().collect();
        let Some(guards) = plan(func, lp, &live_outs) else {
            continue;
        };
        // The clone gives every loop value a second definition. Thread the
        // live-outs through exit-block parameters first.
        if !live_outs.is_empty() && !form_lcssa(func, lp) {
            continue;
        }
        let work_now = func.insts.len();
        apply(func, lp, &guards);
        func.versioned.insert(lp.header);
        return PassOutcome {
            changed: true,
            work: work + work_now,
        };
    }
    PassOutcome {
        changed: false,
        work,
    }
}

/// The guards of `lp` that a versioning clone can drop. The loop's guard
/// operands must be invariant; otherwise the test would not decide the
/// guard's answer on every iteration.
fn plan(func: &Func, lp: &Loop, live_outs: &HashSet<Value>) -> Option<Vec<Guard>> {
    let size: usize = lp
        .blocks
        .iter()
        .map(|&block| func.blocks[block].insts.len())
        .sum();
    if size > MAX_LOOP_INSTS {
        return None;
    }
    let inst_block = inst_blocks(func);
    let mut guards: Vec<Guard> = Vec::new();
    for &block in &lp.blocks {
        for &inst in &func.blocks[block].insts {
            let op = func.insts[inst].op;
            let (test, operand) = match op {
                Op::CheckInt32 | Op::CheckInt64 | Op::CheckClass(_) | Op::CheckCode(_) => {
                    let Some(&operand) = func.args(inst).first() else {
                        continue;
                    };
                    let test = match op {
                        Op::CheckInt32 => Op::TestInt32,
                        Op::CheckInt64 => Op::TestInt64,
                        Op::CheckClass(class) => Op::TestClass(class),
                        _ => Op::TestCode(match op {
                            Op::CheckCode(code) => code,
                            _ => unreachable!("matched a code check above"),
                        }),
                    };
                    (test, operand)
                }
                _ => continue,
            };
            if !invariant(func, lp, &inst_block, operand) {
                continue;
            }
            // A class or code check's result may have a narrower type than its
            // operand. Substituting the operand is fine everywhere except an
            // edge argument, where the target parameter's type is checked.
            if matches!(op, Op::CheckClass(_) | Op::CheckCode(_))
                && func.insts[inst].result.is_some_and(|result| {
                    passed_on_an_edge(func, result) || live_outs.contains(&result)
                })
            {
                continue;
            }
            guards.push(Guard {
                inst,
                test,
                operand,
            });
            if guards.len() >= MAX_GUARDS {
                return Some(guards);
            }
        }
    }
    if guards.is_empty() {
        None
    } else {
        Some(guards)
    }
}

/// Whether `value` is the same on every iteration of `lp`.
pub(super) fn invariant(
    func: &Func,
    lp: &Loop,
    inst_block: &SecondaryMap<Inst, Option<Block>>,
    value: Value,
) -> bool {
    match func.value_def(value) {
        ValueDef::Param { block, .. } => {
            !lp.member[block] || header_param_invariant(func, lp, value)
        }
        ValueDef::Inst(def) => match inst_block[def] {
            _ if matches!(func.insts[def].op, Op::Const(_)) => true,
            Some(block) => !lp.member[block],
            None => true,
        },
    }
}

/// Whether `value` is passed as a block-parameter argument anywhere.
fn passed_on_an_edge(func: &Func, value: Value) -> bool {
    for (_, data) in func.blocks.iter() {
        let Some(term) = &data.term else {
            continue;
        };
        for edge in term.edges() {
            if func.edge_args(&edge).iter().any(|&arg| arg == value) {
                return true;
            }
        }
    }
    false
}

/// Clones `lp` and puts its guard tests on the preheader.
fn apply(func: &mut Func, lp: &Loop, guards: &[Guard]) {
    let header = lp.header;
    let header_params: Vec<Value> = func.params(header).to_vec();
    let Some((pre, entry_args)) = ensure_preheader(func, lp) else {
        return;
    };
    let inst_block = inst_blocks(func);

    let mut consts: HashMap<Value, Value> = HashMap::new();
    let mut chain: Vec<(Op, Value)> = Vec::new();
    let mut seen: HashSet<(Op, Value)> = HashSet::new();
    for guard in guards {
        let operand = preheader_value(
            func,
            pre,
            lp,
            &header_params,
            &entry_args,
            &inst_block,
            &mut consts,
            guard.operand,
        );
        if seen.insert((guard.test, operand)) {
            chain.push((guard.test, operand));
        }
    }

    let mut removed: HashSet<Inst> = HashSet::new();
    let mut untagged: HashSet<Inst> = HashSet::new();
    for guard in guards {
        match func.insts[guard.inst].op {
            Op::CheckClass(_) | Op::CheckCode(_) => {
                removed.insert(guard.inst);
            }
            Op::CheckInt32 | Op::CheckInt64 => {
                untagged.insert(guard.inst);
            }
            _ => {}
        }
    }
    let clone = clone_loop(func, lp, &removed, &untagged, &HashMap::new());

    // The test chain: each false arm runs the original loop, the last true
    // arm enters the clone.
    let fast_header = clone.blocks[&header];
    let mut current = pre;
    for (index, &(test, operand)) in chain.iter().enumerate() {
        let cond = func
            .push_inst(current, test, &[operand], Some(Type::Bool), None)
            .expect("a test has a result");
        let otherwise = func.edge(header, &entry_args);
        if index + 1 == chain.len() {
            let then = func.edge(fast_header, &entry_args);
            func.set_terminator(
                current,
                Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                },
            );
        } else {
            let next = func.new_block();
            let then = func.edge(next, &[]);
            func.set_terminator(
                current,
                Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                },
            );
            current = next;
        }
    }
}

/// The value `operand` has in the preheader.
#[allow(clippy::too_many_arguments)]
pub(super) fn preheader_value(
    func: &mut Func,
    pre: Block,
    lp: &Loop,
    header_params: &[Value],
    entry_args: &[Value],
    inst_block: &SecondaryMap<Inst, Option<Block>>,
    consts: &mut HashMap<Value, Value>,
    operand: Value,
) -> Value {
    if let Some(index) = header_params.iter().position(|&param| param == operand) {
        return entry_args[index];
    }
    let ValueDef::Inst(def) = func.value_def(operand) else {
        return operand;
    };
    let op = func.insts[def].op;
    if !matches!(op, Op::Const(_)) || !inst_block[def].is_some_and(|block| lp.member[block]) {
        return operand;
    }
    if let Some(&copy) = consts.get(&operand) {
        return copy;
    }
    let ty = func.value_type(operand);
    let copy = func
        .push_inst(pre, op, &[], Some(ty), None)
        .expect("a constant has a result");
    consts.insert(operand, copy);
    copy
}

/// A cloned loop region.
pub(super) struct LoopClone {
    pub(super) blocks: HashMap<Block, Block>,
    pub(super) values: HashMap<Value, Value>,
}

/// Clones every block of `lp`, dropping the guards in `removed` (whose results
/// stand for their operands) and turning a planned integer check into an
/// `Unbox`.
pub(super) fn clone_loop(
    func: &mut Func,
    lp: &Loop,
    removed: &HashSet<Inst>,
    untagged: &HashSet<Inst>,
    folds: &HashMap<Block, bool>,
) -> LoopClone {
    let mut clone = LoopClone {
        blocks: HashMap::new(),
        values: HashMap::new(),
    };
    // Blocks and their parameters.
    for &block in &lp.blocks {
        let new = func.new_block();
        clone.blocks.insert(block, new);
        let params = func.params(block).to_vec();
        for param in params {
            let ty = func.value_type(param);
            let copy = func.append_param(new, ty);
            clone.values.insert(param, copy);
        }
    }
    // Instructions first, so every value a frame names is mapped.
    let mut pending: Vec<(Inst, Inst)> = Vec::new();
    for &block in &lp.blocks {
        let target = clone.blocks[&block];
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            let data = func.insts[inst];
            if removed.contains(&inst) {
                let (Some(result), Some(&operand)) = (data.result, func.args(inst).first()) else {
                    continue;
                };
                let replacement = resolve(&clone.values, operand);
                clone.values.insert(result, replacement);
                continue;
            }
            let op = match data.op {
                Op::CheckInt32 if untagged.contains(&inst) => Op::Unbox(Repr::Int32),
                Op::CheckInt64 if untagged.contains(&inst) => Op::Unbox(Repr::Int64),
                other => other,
            };
            let ty = data.result.map(|result| func.value_type(result));
            let new = func.push_inst(target, op, &[], ty, None);
            if let (Some(old_result), Some(new_result)) = (data.result, new) {
                clone.values.insert(old_result, new_result);
            }
            let placed = *func.blocks[target]
                .insts
                .last()
                .expect("the instruction was just pushed");
            pending.push((inst, placed));
        }
    }
    // Operands and frame states.
    let mut cloner = FrameCloner::default();
    for (old, new) in pending {
        let args: Vec<Value> = func
            .args(old)
            .iter()
            .map(|&value| resolve(&clone.values, value))
            .collect();
        func.insts[new].args = EntityList::from_slice(&args, &mut func.pool);
        if let Some(frame) = func.insts[old].frame {
            let cloned = cloner.clone_frame(func, &clone.values, frame);
            func.insts[new].frame = Some(cloned);
        }
    }
    // Terminators last.
    for &block in &lp.blocks {
        let target = clone.blocks[&block];
        let Some(term) = func.blocks[block].term.clone() else {
            continue;
        };
        let mut term = clone_term(func, &clone, &mut cloner, &term);
        if let (
            Some(&keep_then),
            Terminator::Branch {
                then, otherwise, ..
            },
        ) = (folds.get(&block), &term)
        {
            term = Terminator::Jump(if keep_then { *then } else { *otherwise });
        }
        func.blocks[target].term = Some(term);
    }
    clone
}

pub(super) fn resolve(values: &HashMap<Value, Value>, mut value: Value) -> Value {
    for _ in 0..64 {
        match values.get(&value) {
            Some(&next) if next != value => value = next,
            _ => break,
        }
    }
    value
}

pub(super) fn clone_term(
    func: &mut Func,
    clone: &LoopClone,
    cloner: &mut FrameCloner,
    term: &Terminator,
) -> Terminator {
    fn edge(func: &mut Func, clone: &LoopClone, edge: &Edge) -> Edge {
        let block = clone.blocks.get(&edge.block).copied().unwrap_or(edge.block);
        let args: Vec<Value> = func
            .edge_args(edge)
            .iter()
            .map(|&value| resolve(&clone.values, value))
            .collect();
        func.edge(block, &args)
    }
    match term {
        Terminator::Jump(e) => Terminator::Jump(edge(func, clone, e)),
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => Terminator::Branch {
            cond: resolve(&clone.values, *cond),
            then: edge(func, clone, then),
            otherwise: edge(func, clone, otherwise),
        },
        Terminator::Switch {
            value,
            cases,
            default,
            density,
        } => {
            let mut mapped = Vec::with_capacity(cases.len());
            for (key, e) in cases {
                mapped.push((*key, edge(func, clone, e)));
            }
            Terminator::Switch {
                value: resolve(&clone.values, *value),
                cases: mapped,
                default: edge(func, clone, default),
                density: *density,
            }
        }
        Terminator::Return(value) => Terminator::Return(resolve(&clone.values, *value)),
        Terminator::TailCall {
            callee,
            args,
            frame,
            check,
        } => {
            let mapped: Vec<Value> = args
                .as_slice(&func.pool)
                .iter()
                .map(|&value| resolve(&clone.values, value))
                .collect();
            let callee = resolve(&clone.values, *callee);
            let frame = cloner.clone_frame(func, &clone.values, *frame);
            Terminator::TailCall {
                callee,
                args: EntityList::from_slice(&mapped, &mut func.pool),
                frame,
                check: *check,
            }
        }
        Terminator::Deopt { frame } => Terminator::Deopt {
            frame: cloner.clone_frame(func, &clone.values, *frame),
        },
        Terminator::Unreachable => Terminator::Unreachable,
    }
}

/// Clones frame states through a value map, sharing one clone per original.
#[derive(Default)]
pub(super) struct FrameCloner {
    memo: HashMap<FrameState, FrameState>,
}

impl FrameCloner {
    pub(super) fn clone_frame(
        &mut self,
        func: &mut Func,
        values: &HashMap<Value, Value>,
        frame: FrameState,
    ) -> FrameState {
        if let Some(&cloned) = self.memo.get(&frame) {
            return cloned;
        }
        let data = func.frames[frame].clone();
        let closure = resolve(values, data.closure);
        let registers: Vec<(u32, Value)> = data
            .registers
            .iter()
            .map(|&(reg, value)| (reg, resolve(values, value)))
            .collect();
        let parent = data
            .parent
            .map(|parent| self.clone_frame(func, values, parent));
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
        self.memo.insert(frame, cloned);
        cloned
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::func::ValueDef;
    use crate::runtime::codegen::opt::ir::inst::MemoryRegion;
    use crate::runtime::codegen::opt::ir::types::ClassRef;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, frame, jump, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    fn count(func: &Func, wanted: impl Fn(Op) -> bool) -> usize {
        func.blocks
            .values()
            .flat_map(|data| data.insts.iter())
            .filter(|&&inst| wanted(func.insts[inst].op))
            .count()
    }

    #[test]
    fn invariant_guard_is_versioned() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let cond0 = const_bool(&mut func, entry, true);

        let header = block(&mut func);
        let x_param = func.append_param(header, Type::Any);
        let cond_param = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let latch = block(&mut func);
        let exit = block(&mut func);
        let exit_param = func.append_param(exit, Type::Any);
        jump(&mut func, entry, header, &[x0, cond0]);
        branch(&mut func, header, cond_param, body, &[], exit, &[x_param]);
        jump(&mut func, body, latch, &[]);
        // The back edge passes `x` through unchanged: the guard is invariant.
        branch(
            &mut func,
            latch,
            cond_param,
            header,
            &[x_param, cond_param],
            exit,
            &[x_param],
        );
        ret(&mut func, exit, exit_param);

        let guard_frame = frame(&mut func, x_param);
        let guard = func
            .push_inst(
                body,
                Op::CheckInt32,
                &[x_param],
                Some(Type::Int32),
                Some(guard_frame),
            )
            .expect("a guard has a result");
        let ValueDef::Inst(guard_inst) = func.value_def(guard) else {
            panic!("the guard is an instruction result");
        };

        verify(&func).expect("the test function is valid");
        let outcome = run_enabled(&mut func);
        assert!(outcome.changed, "the loop is versioned");
        assert!(func.versioned.contains(&header));
        assert_eq!(
            count(&func, |op| op == Op::CheckInt32),
            1,
            "the slow loop keeps the guard"
        );
        assert_eq!(
            count(&func, |op| op == Op::Unbox(Repr::Int32)),
            1,
            "the fast clone only untags"
        );
        assert_eq!(
            count(&func, |op| op == Op::TestInt32),
            1,
            "one test on the preheader"
        );
        assert!(func.blocks[body].insts.contains(&guard_inst));
        verify(&func).expect("the versioned function is valid");
    }

    /// The same loop with a variant guard operand is left alone.
    #[test]
    fn variant_guard_is_not_versioned() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let cond0 = const_bool(&mut func, entry, true);

        let header = block(&mut func);
        let x_param = func.append_param(header, Type::Any);
        let cond_param = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let latch = block(&mut func);
        let exit = block(&mut func);
        let exit_param = func.append_param(exit, Type::Any);
        jump(&mut func, entry, header, &[x0, cond0]);
        branch(&mut func, header, cond_param, body, &[], exit, &[x_param]);
        jump(&mut func, body, latch, &[]);
        // The back edge replaces `x`, so it is not invariant.
        branch(
            &mut func,
            latch,
            cond_param,
            header,
            &[x0, cond_param],
            exit,
            &[x_param],
        );
        ret(&mut func, exit, exit_param);

        let guard_frame = frame(&mut func, x_param);
        let guard = func
            .push_inst(
                body,
                Op::CheckInt32,
                &[x_param],
                Some(Type::Int32),
                Some(guard_frame),
            )
            .expect("a guard has a result");
        let ValueDef::Inst(guard_inst) = func.value_def(guard) else {
            panic!("the guard is an instruction result");
        };

        verify(&func).expect("the test function is valid");
        let outcome = run_enabled(&mut func);
        assert!(!outcome.changed, "the guard operand is not invariant");
        assert!(!func.versioned.contains(&header));
        assert_eq!(count(&func, |op| op == Op::CheckInt32), 1);
        assert!(func.blocks[body].insts.contains(&guard_inst));
    }

    /// A class check whose result feeds a load is dropped from the clone by
    /// forwarding the operand.
    #[test]
    fn class_guard_is_versioned() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let cond0 = const_bool(&mut func, entry, true);

        let header = block(&mut func);
        let x_param = func.append_param(header, Type::Any);
        let cond_param = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let latch = block(&mut func);
        let exit = block(&mut func);
        let exit_param = func.append_param(exit, Type::Any);
        jump(&mut func, entry, header, &[x0, cond0]);
        branch(&mut func, header, cond_param, body, &[], exit, &[x_param]);
        jump(&mut func, body, latch, &[]);
        branch(
            &mut func,
            latch,
            cond_param,
            header,
            &[x_param, cond_param],
            exit,
            &[x_param],
        );
        ret(&mut func, exit, exit_param);

        let guard_frame = frame(&mut func, x_param);
        let checked = func
            .push_inst(
                body,
                Op::CheckClass(ClassRef::PAIR),
                &[x_param],
                Some(Type::Heap(ClassRef::PAIR)),
                Some(guard_frame),
            )
            .expect("a guard has a result");
        func.push_inst(
            body,
            Op::Load {
                field: 0,
                region: MemoryRegion::Heap,
            },
            &[checked],
            Some(Type::Any),
            None,
        )
        .expect("a load has a result");

        verify(&func).expect("the test function is valid");
        let outcome = run_enabled(&mut func);
        assert!(outcome.changed, "the loop is versioned");
        assert_eq!(
            count(&func, |op| matches!(op, Op::CheckClass(_))),
            1,
            "the slow loop keeps the class check"
        );
        assert_eq!(
            count(&func, |op| matches!(op, Op::TestClass(_))),
            1,
            "one test on the preheader"
        );
        verify(&func).expect("the versioned function is valid");
    }

    /// A loop whose guarded value is also read after the loop is versioned:
    /// the exit block becomes the join of the two loops.
    #[test]
    fn live_out_loop_is_versioned() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let cond0 = const_bool(&mut func, entry, true);
        let header = block(&mut func);
        let x_param = func.append_param(header, Type::Any);
        let cond_param = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let exit = block(&mut func);
        jump(&mut func, entry, header, &[x0, cond0]);
        branch(&mut func, header, cond_param, body, &[], exit, &[]);
        jump(&mut func, body, header, &[x_param, cond_param]);
        // `x` is read directly after the loop.
        ret(&mut func, exit, x_param);
        let guard_frame = frame(&mut func, x_param);
        func.push_inst(
            body,
            Op::CheckInt32,
            &[x_param],
            Some(Type::Int32),
            Some(guard_frame),
        );
        verify(&func).expect("the test function is valid");
        let outcome = run_enabled(&mut func);
        assert!(outcome.changed, "LCSSA lets the loop be versioned");
        assert_eq!(count(&func, |op| op == Op::TestInt32), 1);
        verify(&func).expect("the versioned function is valid");
    }
}
