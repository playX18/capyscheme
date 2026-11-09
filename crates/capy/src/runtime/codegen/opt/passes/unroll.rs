//! Unrolling of canonical natural loops.
//!
//! A loop whose header is a `Branch` on a comparison of a counter with an
//! invariant, where every back edge advances the counter by the same `±1`, is
//! duplicated once. The new hot header `H'` tests the *same* comparison on
//! the current counter and on the next one (`i + step`) and branches to:
//!
//! - the original exit edge when the current iteration would not run;
//! - a copy of the header and the body when only one iteration remains;
//! - a copy of the body followed by an unconditional copy of the header and
//!   a second body copy when two iterations remain.
//!
//! The original header stays in place as the one-iteration tail, so an entry
//! into the middle of a pair (an OSR entry, or a pair cut short by an early
//! exit) runs the right number of iterations. Each iteration still executes a
//! copy of the header's instructions, so guards, safepoints and deopt frame
//! states are preserved; only the second comparison of a full pair is dropped.
//!
//! Edges into the header from outside the loop are redirected to `H'`, and an
//! OSR entry names `H'` from then on. The `i + step` comparison uses the
//! total `add.i64` when the range analysis proves the counter fits `i32`;
//! otherwise it uses `add.i64.ovf` with the header's own frame state, so an
//! overflowing counter simply deopts to the loop header in baseline.
//!
//! Loops with calls are not unrolled, and a loop larger than
//! [`MAX_LOOP_INSTS`] instructions is left alone.

use std::collections::HashMap;

use cranelift_entity::{EntityList, SecondaryMap};

use super::PassOutcome;
use super::forward::def_inst;
use super::lcssa::form_lcssa;
use super::loops::{Loop, canon, find_loops, induction_step, outside_uses_loop_values};
use super::range;
use super::simplify::edges_mut;
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Constant, Edge, Inst, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;

/// The largest loop, in instructions, that one run will duplicate.
const MAX_LOOP_INSTS: usize = 96;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    for lp in &loops {
        if let Some(mut plan) = plan_loop(func, lp) {
            // Duplicating the header replaces it on every path into the
            // loop, so a loop-defined value read outside the loop (`return
            // sum`) must first go through exit-block parameters. That
            // retargets the exit edge, so plan again afterwards.
            if outside_uses_loop_values(func, lp) {
                if !form_lcssa(func, lp) {
                    work += lp.blocks.len();
                    continue;
                }
                match plan_loop(func, lp) {
                    Some(replanned) => plan = replanned,
                    None => {
                        return PassOutcome {
                            changed: true,
                            work: work + func.insts.len(),
                        };
                    }
                }
            }
            apply(func, lp, plan);
            return PassOutcome {
                changed: true,
                work: work + func.insts.len(),
            };
        }
        work += lp.blocks.len();
    }
    PassOutcome {
        changed: false,
        work,
    }
}

/// The canonical loop shape this pass can duplicate.
struct Plan {
    cond: Value,
    step: i64,
    raw: Value,
    cmp: Cmp,
    counter_is_lhs: bool,
    bound: Value,
    /// The arm that stays in the loop.
    continue_edge: Edge,
    /// The arm that leaves the loop.
    exit_edge: Edge,
    /// Whether the header's continuation is the branch's `then` arm.
    continue_is_then: bool,
    /// The first deopting instruction's frame in the header, for the checked
    /// `i + step`.
    header_frame: Option<FrameState>,
    /// Whether the range proves `raw + step` fits `i64`, so the total add is
    /// enough.
    small: bool,
}

/// Whether the counted 2x unroller can take `lp`.
pub(super) fn counted(func: &Func, lp: &Loop) -> bool {
    plan_loop(func, lp).is_some()
}

/// The counter of a loop whose header branches on a comparison of a ±1
/// counter with an invariant bound.
pub(super) struct Counter {
    pub(super) cond: Value,
    pub(super) cmp: Cmp,
    /// The header parameter that counts.
    pub(super) param: Value,
    /// The int64 view of the counter that the comparison reads.
    pub(super) raw: Value,
    pub(super) counter_is_lhs: bool,
    pub(super) bound: Value,
    pub(super) step: i64,
    pub(super) continue_edge: Edge,
    pub(super) exit_edge: Edge,
    pub(super) continue_is_then: bool,
}

/// Finds the [`Counter`] of `lp`, if its header has that shape.
pub(super) fn counter(func: &Func, lp: &Loop) -> Option<Counter> {
    let header = lp.header;
    let Some(Terminator::Branch {
        cond,
        then,
        otherwise,
    }) = func.blocks[header].term.clone()
    else {
        return None;
    };
    let then_in = lp.member[then.block];
    let otherwise_in = lp.member[otherwise.block];
    if then_in == otherwise_in {
        return None;
    }
    let continue_is_then = then_in;
    let (continue_edge, exit_edge) = if continue_is_then {
        (then, otherwise)
    } else {
        (otherwise, then)
    };

    let cond_inst = def_inst(func, cond)?;
    let Op::CmpI64(cmp) = func.insts[cond_inst].op else {
        return None;
    };
    let inst_block = super::loops::inst_blocks(func);
    if inst_block[cond_inst] != Some(header) {
        return None;
    }
    let args = func.args(cond_inst);
    if args.len() != 2 {
        return None;
    }
    let params = func.params(header);
    let mut chosen = None;
    for index in 0..params.len() {
        let Some(step) = induction_step(func, lp, header, index) else {
            continue;
        };
        if step.delta != 1 && step.delta != -1 {
            continue;
        }
        let candidate = if canon(func, args[0]) == params[index]
            && func.value_type(args[0]) == Type::Int64
        {
            Some((true, args[0], args[1]))
        } else if canon(func, args[1]) == params[index] && func.value_type(args[1]) == Type::Int64 {
            Some((false, args[1], args[0]))
        } else {
            None
        };
        let Some((counter_is_lhs, raw, bound)) = candidate else {
            continue;
        };
        if !bound_invariant(func, lp, &inst_block, bound) {
            continue;
        }
        chosen = Some((counter_is_lhs, raw, bound, step.delta, params[index]));
        break;
    }
    let (counter_is_lhs, raw, bound, step, param) = chosen?;

    Some(Counter {
        cond,
        cmp,
        param,
        raw,
        counter_is_lhs,
        bound,
        step,
        continue_edge,
        exit_edge,
        continue_is_then,
    })
}

fn plan_loop(func: &Func, lp: &Loop) -> Option<Plan> {
    let header = lp.header;
    let body = lp.body();
    if body.is_empty() {
        return None;
    }
    let size: usize = lp
        .blocks
        .iter()
        .map(|&block| func.blocks[block].insts.len())
        .sum();
    if size > MAX_LOOP_INSTS {
        return None;
    }
    // Duplicating calls would change their number, so leave those loops
    // alone. Safepoints are fine: every iteration still runs one.
    for &block in &lp.blocks {
        if matches!(func.blocks[block].term, Some(Terminator::TailCall { .. })) {
            return None;
        }
        for &inst in &func.blocks[block].insts {
            let op = func.insts[inst].op;
            if op.is_call() && op != Op::Safepoint {
                return None;
            }
        }
    }

    let Counter {
        cond,
        cmp,
        raw,
        counter_is_lhs,
        bound,
        step,
        continue_edge,
        exit_edge,
        continue_is_then,
        ..
    } = counter(func, lp)?;

    let analysis = range::analyze(func);
    let small = analysis.fits_i32(header, raw);
    drop(analysis);
    let header_frame = func.blocks[header]
        .insts
        .iter()
        .find_map(|&inst| func.insts[inst].frame);
    if !small && header_frame.is_none() {
        return None;
    }

    Some(Plan {
        cond,
        step,
        raw,
        cmp,
        counter_is_lhs,
        bound,
        continue_edge,
        exit_edge,
        continue_is_then,
        header_frame,
        small,
    })
}

/// Whether `value` has the same value on every iteration of `lp`.
pub(super) fn bound_invariant(
    func: &Func,
    lp: &Loop,
    inst_block: &SecondaryMap<Inst, Option<Block>>,
    value: Value,
) -> bool {
    fn check(
        func: &Func,
        lp: &Loop,
        inst_block: &SecondaryMap<Inst, Option<Block>>,
        value: Value,
        seen: &mut HashMap<Value, bool>,
    ) -> bool {
        if let Some(&known) = seen.get(&value) {
            return known;
        }
        seen.insert(value, false);
        let result = match func.value_def(value) {
            ValueDef::Param { block, index } => {
                if !lp.member[block] {
                    true
                } else {
                    // A header parameter is invariant when every back edge
                    // passes it through unchanged.
                    block == lp.header
                        && lp.blocks.iter().all(|&pred| {
                            if pred == lp.header {
                                return true;
                            }
                            let Some(term) = &func.blocks[pred].term else {
                                return true;
                            };
                            term.edges().iter().all(|edge| {
                                edge.block != lp.header
                                    || func.edge_args(edge).get(index as usize).copied()
                                        == Some(value)
                            })
                        })
                }
            }
            ValueDef::Inst(inst) => match inst_block[inst] {
                Some(block) if lp.member[block] => {
                    let data = &func.insts[inst];
                    let effects = data.op.effects();
                    // A pure operation of invariant operands is invariant;
                    // so is a deterministic guard on them (it cannot fail on
                    // some iterations and not others). Anything that reads
                    // memory or calls is not.
                    matches!(data.op, Op::Const(_))
                        || (!data.op.is_call()
                            && effects.writes.is_empty()
                            && effects.reads.is_empty()
                            && func
                                .args(inst)
                                .iter()
                                .all(|&arg| check(func, lp, inst_block, arg, seen)))
                }
                _ => true,
            },
        };
        seen.insert(value, result);
        result
    }
    check(func, lp, inst_block, value, &mut HashMap::new())
}

fn apply(func: &mut Func, lp: &Loop, plan: Plan) {
    let header = lp.header;
    let body = lp.body();
    let header_params: Vec<Value> = func.params(header).to_vec();

    // The two header copies and the second-check block.
    let (hot, hot_map, hot_frames) = clone_header(func, header);
    let (mid, mid_map, _mid_frames) = clone_header(func, header);
    let check2 = func.new_block();

    // The first body copy runs iteration 1 of a pair and continues to `mid`;
    // the second runs iteration 2 and continues to `hot`.
    let (body1_entry, _) = clone_body(func, &body, header, &hot_map, mid);
    let (body2_entry, _) = clone_body(func, &body, header, &mid_map, hot);

    // `hot`: the original test. A false test leaves the loop; a true test
    // moves on to the second check.
    let cond_hot = resolve(&hot_map, plan.cond);
    let exit_args = remap_args(func, &plan.exit_edge, |value| resolve(&hot_map, value));
    let exit_hot = Edge {
        block: plan.exit_edge.block,
        args: EntityList::from_slice(&exit_args, &mut func.pool),
    };
    let check2_edge = func.edge(check2, &[]);
    let hot_term = if plan.continue_is_then {
        Terminator::Branch {
            cond: cond_hot,
            then: check2_edge,
            otherwise: exit_hot,
        }
    } else {
        Terminator::Branch {
            cond: cond_hot,
            then: exit_hot,
            otherwise: check2_edge,
        }
    };
    func.set_terminator(hot, hot_term);

    // `check2`: does `i + step` also continue? When it does not, the
    // one-iteration path enters `mid`, which runs the header once and then
    // the second body copy.
    let raw_hot = resolve(&hot_map, plan.raw);
    let bound_hot = resolve(&hot_map, plan.bound);
    let step_const = func
        .push_inst(
            check2,
            Op::Const(Constant::Int64(plan.step)),
            &[],
            Some(Type::Int64),
            None,
        )
        .expect("a constant has a result");
    let frame = plan
        .header_frame
        .and_then(|frame| hot_frames.get(&frame).copied());
    let next = if plan.small {
        func.push_inst(
            check2,
            Op::AddI64,
            &[raw_hot, step_const],
            Some(Type::Int64),
            None,
        )
    } else {
        func.push_inst(
            check2,
            Op::AddI64Ovf,
            &[raw_hot, step_const],
            Some(Type::Int64),
            frame,
        )
    }
    .expect("an add has a result");
    let cond2 = if plan.counter_is_lhs {
        func.push_inst(
            check2,
            Op::CmpI64(plan.cmp),
            &[next, bound_hot],
            Some(Type::Bool),
            None,
        )
    } else {
        func.push_inst(
            check2,
            Op::CmpI64(plan.cmp),
            &[bound_hot, next],
            Some(Type::Bool),
            None,
        )
    }
    .expect("a comparison has a result");
    let body1_args = remap_args(func, &plan.continue_edge, |value| resolve(&hot_map, value));
    let body1_edge = func.edge(body1_entry, &body1_args);
    let mid_params: Vec<Value> = header_params
        .iter()
        .map(|&param| resolve(&hot_map, param))
        .collect();
    let mid_edge = func.edge(mid, &mid_params);
    let check_term = if plan.continue_is_then {
        Terminator::Branch {
            cond: cond2,
            then: body1_edge,
            otherwise: mid_edge,
        }
    } else {
        Terminator::Branch {
            cond: cond2,
            then: mid_edge,
            otherwise: body1_edge,
        }
    };
    func.set_terminator(check2, check_term);

    // `mid`: the header runs, then the second body copy unconditionally.
    let continue_args = remap_args(func, &plan.continue_edge, |value| resolve(&mid_map, value));
    let continue_mid = Edge {
        block: body2_entry,
        args: EntityList::from_slice(&continue_args, &mut func.pool),
    };
    func.set_terminator(mid, Terminator::Jump(continue_mid));

    // Outside edges (the preheader and the OSR entry) now enter `hot`.
    for block in func.blocks.keys().collect::<Vec<_>>() {
        if lp.member[block] {
            continue;
        }
        let Some(mut term) = func.blocks[block].term.take() else {
            continue;
        };
        for edge in edges_mut(&mut term) {
            if edge.block == header {
                edge.block = hot;
            }
        }
        func.blocks[block].term = Some(term);
    }
    for entry in &mut func.osr {
        if entry.header == header {
            entry.header = hot;
        }
    }
}

/// Remaps an edge's arguments, keeping its target.
fn remap_args(func: &Func, edge: &Edge, resolve: impl Fn(Value) -> Value) -> Vec<Value> {
    func.edge_args(edge).iter().map(|&v| resolve(v)).collect()
}

fn resolve(map: &HashMap<Value, Value>, value: Value) -> Value {
    map.get(&value).copied().unwrap_or(value)
}

/// Clones a header block without its terminator. Returns the new block, the
/// value map, and the cloned frame states.
fn clone_header(
    func: &mut Func,
    old: Block,
) -> (
    Block,
    HashMap<Value, Value>,
    HashMap<FrameState, FrameState>,
) {
    let new = func.new_block();
    let mut map: HashMap<Value, Value> = HashMap::new();
    for &param in func.params(old).to_vec().iter() {
        let ty = func.value_type(param);
        let copy = func.append_param(new, ty);
        map.insert(param, copy);
    }
    let insts = func.blocks[old].insts.clone();
    let mut pending: Vec<(Inst, FrameState)> = Vec::new();
    for inst in insts {
        let data = func.insts[inst];
        let args: Vec<Value> = func.args(inst).iter().map(|&v| resolve(&map, v)).collect();
        let ty = data.result.map(|result| func.value_type(result));
        let copy = func.push_inst(new, data.op, &args, ty, None);
        if let (Some(result), Some(original)) = (copy, data.result) {
            map.insert(original, result);
        }
        if let Some(frame) = data.frame {
            pending.push((
                func.blocks[new].insts.last().copied().expect("just pushed"),
                frame,
            ));
        }
    }
    let mut cloner = FrameCloner::default();
    let mut frames = HashMap::new();
    for (inst, old_frame) in pending {
        let cloned = cloner.clone_frame(func, &map, old_frame);
        func.insts[inst].frame = Some(cloned);
        frames.insert(old_frame, cloned);
    }
    (new, map, frames)
}

/// Clones every block of `blocks`, redirecting edges into `header` at
/// `latch_target`. Returns the copy of the block `header`'s continue edge
/// names, which is the clone region's entry.
fn clone_body(
    func: &mut Func,
    blocks: &[Block],
    header: Block,
    base: &HashMap<Value, Value>,
    latch_target: Block,
) -> (Block, HashMap<Value, Value>) {
    let mut map: HashMap<Value, Value> = base.clone();
    let mut block_map: HashMap<Block, Block> = HashMap::new();
    for &block in blocks {
        let new = func.new_block();
        block_map.insert(block, new);
        for &param in func.params(block).to_vec().iter() {
            let ty = func.value_type(param);
            let copy = func.append_param(new, ty);
            map.insert(param, copy);
        }
    }
    // Instructions first, so every value a frame names is mapped.
    let mut pending: Vec<(Inst, FrameState)> = Vec::new();
    for &block in blocks {
        let new = block_map[&block];
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            let data = func.insts[inst];
            let args: Vec<Value> = func.args(inst).iter().map(|&v| resolve(&map, v)).collect();
            let ty = data.result.map(|result| func.value_type(result));
            let copy = func.push_inst(new, data.op, &args, ty, None);
            if let (Some(result), Some(original)) = (copy, data.result) {
                map.insert(original, result);
            }
            if let Some(frame) = data.frame {
                pending.push((
                    func.blocks[new].insts.last().copied().expect("just pushed"),
                    frame,
                ));
            }
        }
    }
    let mut cloner = FrameCloner::default();
    for (inst, old_frame) in pending {
        let cloned = cloner.clone_frame(func, &map, old_frame);
        func.insts[inst].frame = Some(cloned);
    }
    // Terminators last.
    for &block in blocks {
        let new = block_map[&block];
        let Some(mut term) = func.blocks[block].term.clone() else {
            continue;
        };
        remap_term(
            func,
            &mut term,
            &map,
            &block_map,
            header,
            latch_target,
            &mut cloner,
        );
        func.blocks[new].term = Some(term);
    }
    // The entry the header's continue edge names.
    let entry = func.blocks[header]
        .term
        .as_ref()
        .and_then(|term| {
            term.edges()
                .into_iter()
                .map(|edge| edge.block)
                .find(|&target| block_map.contains_key(&target))
        })
        .and_then(|target| block_map.get(&target).copied())
        .expect("a planned loop has a body edge");
    (entry, map)
}

/// Remaps a cloned block's terminator: values through `map`, targets through
/// `block_map`, and edges into `header` onto `latch_target`.
fn remap_term(
    func: &mut Func,
    term: &mut Terminator,
    map: &HashMap<Value, Value>,
    block_map: &HashMap<Block, Block>,
    header: Block,
    latch_target: Block,
    cloner: &mut FrameCloner,
) {
    match term {
        Terminator::Jump(edge) => remap_one(func, edge, map, block_map, header, latch_target),
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => {
            *cond = resolve(map, *cond);
            remap_one(func, then, map, block_map, header, latch_target);
            remap_one(func, otherwise, map, block_map, header, latch_target);
        }
        Terminator::Switch {
            value,
            cases,
            default,
            ..
        } => {
            *value = resolve(map, *value);
            for (_, edge) in cases.iter_mut() {
                remap_one(func, edge, map, block_map, header, latch_target);
            }
            remap_one(func, default, map, block_map, header, latch_target);
        }
        Terminator::Return(value) => *value = resolve(map, *value),
        Terminator::TailCall {
            callee,
            args,
            frame,
            ..
        } => {
            *callee = resolve(map, *callee);
            let mapped: Vec<Value> = args
                .as_slice(&func.pool)
                .iter()
                .map(|&v| resolve(map, v))
                .collect();
            *args = EntityList::from_slice(&mapped, &mut func.pool);
            *frame = cloner.clone_frame(func, map, *frame);
        }
        Terminator::Deopt { frame } => {
            *frame = cloner.clone_frame(func, map, *frame);
        }
        Terminator::ReturnMany { values } => {
            for value in values.iter_mut() {
                *value = resolve(map, *value);
            }
        }
        Terminator::Unreachable => {}
    }
}

fn remap_one(
    func: &mut Func,
    edge: &mut Edge,
    map: &HashMap<Value, Value>,
    block_map: &HashMap<Block, Block>,
    header: Block,
    latch_target: Block,
) {
    let target = if edge.block == header {
        latch_target
    } else {
        block_map.get(&edge.block).copied().unwrap_or(edge.block)
    };
    let args: Vec<Value> = func
        .edge_args(edge)
        .iter()
        .map(|&v| resolve(map, v))
        .collect();
    edge.block = target;
    edge.args = EntityList::from_slice(&args, &mut func.pool);
}

/// Clones frame states through a value map, sharing one clone per original
/// within a region.
#[derive(Default)]
struct FrameCloner {
    memo: HashMap<FrameState, FrameState>,
}

impl FrameCloner {
    fn clone_frame(
        &mut self,
        func: &mut Func,
        map: &HashMap<Value, Value>,
        frame: FrameState,
    ) -> FrameState {
        if let Some(&cloned) = self.memo.get(&frame) {
            return cloned;
        }
        let data = func.frames[frame].clone();
        let closure = resolve(map, data.closure);
        let registers: Vec<(u32, Value)> = data
            .registers
            .iter()
            .map(|&(reg, value)| (reg, resolve(map, value)))
            .collect();
        let parent = data
            .parent
            .map(|parent| self.clone_frame(func, map, parent));
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
