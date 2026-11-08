//! Loop-invariant code motion.
//!
//! A natural loop is found for each header `h` that dominates a latch `l`
//! with an edge `l -> h`. Its body holds every reachable block that reaches a
//! latch without passing `h`. Invariant instructions move to a preheader `p`,
//! a block whose only successor is `h` and which is the only way into the loop
//! from outside. One loop is handled per step, innermost first, and the
//! analysis is rebuilt after each move, so outer loops see moved instructions
//! as outside the inner loop. A step always takes at least one instruction out
//! of a loop body, and a run stops after a fixed number of moves. The pipeline
//! runs the pass again while it changes the function.
//!
//! An instruction is invariant when every operand is defined outside the loop,
//! is a constant, or is an instruction already chosen to move. A constant of
//! the loop that a moved instruction (or a moved guard's frame) reads is copied
//! into the preheader; the loop keeps its own copy for its other uses. The rules for which invariant
//! instructions move:
//!
//! 1. Pure operations that cannot deopt (see [`pure_hoistable`]) move from any
//!    block of the loop. Running them early is harmless because they cannot
//!    fail or write.
//! 2. A load (see [`is_load`]) moves from any block when nothing in the loop
//!    clobbers its region and the loop has no call or safepoint, since
//!    [`clobbers`] reports a barrier as the whole memory. Its object was
//!    checked before the loop, because the load reads a checked value, so the
//!    early read is memory-safe. The read has no other effect.
//! 3. A deopting guard moves only from a block that runs on every path
//!    through the loop, under the guard rule below. That is the header, or a
//!    block that dominates every latch and every block with an edge leaving
//!    the loop (a post-tested loop body, or an inlined callee's entry block
//!    when the call is unconditional). `CheckNotInvalid` never moves:
//!    invalidation can change during a call. It does not block a later guard
//!    from moving, since it can only deopt to baseline.
//!
//! The guard rule. A block that runs on every path through the loop does so on
//! the first iteration, before any effect of the loop (every block on every
//! path from the header to it must be neutral). A guard on invariant operands
//! gives the same answer on every iteration, so if it would fail on some
//! iteration it fails on the first. Moving it to the preheader therefore
//! deopts at the same point with no effect skipped, provided:
//!
//! - every instruction before it in its block is neutral: it cannot deopt,
//!   writes nothing and does not call, or it is a `Safepoint` or a
//!   `CheckNotInvalid`, or it moved too. A store, an allocation, a call or a
//!   guard that stays blocks the rest of the block. The one exception is a
//!   `Safepoint`. A yield there that precedes the deopt is skipped on the
//!   deopt path, which changes only the scheduling of the failing run, never
//!   the values it computes.
//! - its frame state can be rebuilt in the preheader. Each register or closure
//!   value that is a parameter of the header becomes the value that enters the
//!   header from the preheader, which is the value the first iteration sees.
//!   Every other value in the frame must be defined outside the loop or be an
//!   instruction that moved. Otherwise the guard stays. Frames of inlined
//!   bodies are copied whole, including their parent chain and call-site
//!   metadata, so a guard inside a spliced callee moves like any other.
//!
//! A guard in any other block stays, since it may not run on the first
//! iteration and moving it would deopt on a path the program never takes
//! (with an inlined frame, baseline would even resume inside a call the
//! caller had not made yet).

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::loops::{Loop, ensure_preheader, find_loops, header_param_invariant, inst_blocks};
use super::{PassOutcome, clobbers, is_load, writes_existing};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, DomTree, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Inst, MemoryRanges, Op, Repr};
use crate::runtime::codegen::opt::ir::types::CodeRef;
use cranelift_entity::EntityRef as _;

/// Moves allowed in one run. Each step moves at least one instruction, so this
/// bounds the work; a later round of the pipeline continues.
const MAX_MOVES_PER_RUN: usize = 1024;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = false;
    let mut work = 0;
    let mut moves = 0;
    while moves < MAX_MOVES_PER_RUN {
        let dom = func.dom_tree();
        let mut loops = find_loops(func, &dom);
        loops.sort_by_key(|lp| lp.blocks.len());
        let inst_block = inst_blocks(func);
        work += func.blocks.len() + func.insts.len();

        let mut step = None;
        for lp in &loops {
            let found = plan(func, &dom, lp, &inst_block);
            if !found.order.is_empty() {
                step = Some((lp, found));
                break;
            }
        }
        let Some((lp, found)) = step else { break };
        moves += found.order.len();
        apply(func, lp, &found);
        changed = true;
    }
    PassOutcome { changed, work }
}

/// Operations that compute a value from their operands and cannot fail.
fn pure_hoistable(op: Op) -> bool {
    let effects = op.effects();
    op.has_result()
        && !matches!(op, Op::Const(_))
        && !op.can_deopt()
        && !op.is_call()
        && effects.writes.is_empty()
        && effects.reads.is_empty()
        && effects.causes.is_empty()
}

fn guard_hoistable(op: Op) -> bool {
    op.can_deopt()
        && op.effects().writes.is_empty()
        && op != Op::CheckNotInvalid
        && op != Op::Box(Repr::Int64)
        && !matches!(op, Op::SlotGet { .. } | Op::SlotPut { .. })
}

/// Whether an instruction may precede a hoisted guard without blocking it
fn neutral(op: Op) -> bool {
    op == Op::Safepoint
        || op == Op::CheckNotInvalid
        || (clobbers(op).is_empty() && !op.can_deopt() && !op.is_call())
}

/// The state needed to answer invariance questions about one loop.
struct Scan<'a> {
    func: &'a Func<'a>,
    lp: &'a Loop,
    inst_block: &'a SecondaryMap<Inst, Option<Block>>,
}

impl Scan<'_> {
    /// Whether `v` has the same value on every iteration, given the
    /// instructions already chosen (`hoisted`).
    fn invariant(&self, hoisted: &HashSet<Inst>, v: Value) -> bool {
        match self.func.value_def(v) {
            ValueDef::Param { block, .. } => {
                !self.lp.member[block] || header_param_invariant(self.func, self.lp, v)
            }
            ValueDef::Inst(def) => match self.inst_block[def] {
                _ if matches!(self.func.insts[def].op, Op::Const(_)) => true,
                Some(block) if self.lp.member[block] => hoisted.contains(&def),
                _ => true,
            },
        }
    }

    /// Whether every value of `frame` can be read in the preheader.
    fn frame_ok(
        &self,
        hoisted: &HashSet<Inst>,
        header_params: &[Value],
        frame: FrameState,
    ) -> bool {
        self.func
            .frame_values(frame)
            .into_iter()
            .all(|v| self.rematerializable(hoisted, header_params, v, REMAT_DEPTH))
    }

    fn entry_frame(&self, header_params: &[Value]) -> Option<FrameState> {
        let mut frame = None;
        for &inst in &self.func.blocks[self.lp.header].insts {
            let data = &self.func.insts[inst];
            if data.op == Op::Safepoint || data.op.can_deopt() {
                frame = data.frame;
                break;
            }
            if !neutral(data.op) {
                return None;
            }
        }
        let frame = frame?;
        let data = &self.func.frames[frame];
        if data.parent.is_some()
            || data.returned.is_some()
            || data.code != CodeRef::new(0)
            || self.func.spec_blocked.contains(&data.pc)
        {
            return None;
        }
        self.frame_ok(&HashSet::new(), header_params, frame)
            .then_some(frame)
    }

    /// Whether `v` can be read in the preheader: a header parameter (its entry
    /// value), an invariant value, or a pure computation of such values.
    ///
    /// A frame register that holds `i + 1`, computed in the iteration before
    /// the guard, is the last kind. On the first iteration it equals the same
    /// computation on the values that enter the header, so the preheader can
    /// compute it for the frame. Only pure, frameless operations qualify: a
    /// load or a call would read state the preheader may not see, and an
    /// allocation would make a fresh object per loop entry.
    fn rematerializable(
        &self,
        hoisted: &HashSet<Inst>,
        header_params: &[Value],
        v: Value,
        depth: usize,
    ) -> bool {
        if header_params.contains(&v) || self.invariant(hoisted, v) {
            return true;
        }
        let ValueDef::Inst(def) = self.func.value_def(v) else {
            return false;
        };
        let data = &self.func.insts[def];
        depth > 0
            && data.frame.is_none()
            && data.result.is_some()
            && pure_hoistable(data.op)
            && self
                .func
                .args(def)
                .iter()
                .all(|&arg| self.rematerializable(hoisted, header_params, arg, depth - 1))
    }
}

/// How deep a frame value's pure computation may be and still be copied into
/// the preheader.
const REMAT_DEPTH: usize = 4;

/// The member blocks whose execution ends an iteration (a latch: an edge back
/// to the header) or leaves the loop. A guard may move out of another block
/// only when that block dominates all of them.
fn loop_terminals(func: &Func, lp: &Loop) -> Vec<Block> {
    let header = lp.header;
    let mut out: Vec<Block> = Vec::new();
    for &block in &lp.blocks {
        let Some(term) = &func.blocks[block].term else {
            continue;
        };
        let terminal = term
            .edges()
            .iter()
            .any(|edge| edge.block == header || !lp.member[edge.block]);
        if terminal && !out.contains(&block) {
            out.push(block);
        }
    }
    out
}

/// Whether every block on every path from the header to `target` (excluding
/// `target`) is neutral.
fn neutral_path_to(func: &Func, lp: &Loop, target: Block) -> bool {
    let header = lp.header;
    if !func.blocks[header]
        .insts
        .iter()
        .all(|&inst| neutral(func.insts[inst].op))
    {
        return false;
    }
    let mut stack: Vec<Block> = func
        .successors(header)
        .into_iter()
        .filter(|&block| lp.member[block] && block != target)
        .collect();
    let mut seen: HashSet<Block> = HashSet::new();
    while let Some(block) = stack.pop() {
        if block == target || !seen.insert(block) {
            continue;
        }
        if !func.blocks[block]
            .insts
            .iter()
            .all(|&inst| neutral(func.insts[inst].op))
        {
            return false;
        }
        for next in func.successors(block) {
            if lp.member[next] && next != target {
                stack.push(next);
            }
        }
    }
    true
}

/// The instructions of `lp` that move to its preheader, in an order where
/// every operand comes before its use.
fn plan(
    func: &Func,
    dom: &DomTree,
    lp: &Loop,
    inst_block: &SecondaryMap<Inst, Option<Block>>,
) -> Plan {
    let scan = Scan {
        func,
        lp,
        inst_block,
    };
    let header = lp.header;
    let header_params = func.params(header);

    // What the loop may write to *existing* objects. An allocation only
    // creates a fresh object, so it cannot invalidate a load of one that
    // already exists; a call or safepoint still writes everything.
    let mut clobber = MemoryRanges::NONE;
    for &block in &lp.blocks {
        for &inst in &func.blocks[block].insts {
            clobber = clobber.union(writes_existing(func.insts[inst].op));
        }
    }

    let terminals = loop_terminals(func, lp);
    let entry = if opt().spec_hoist {
        scan.entry_frame(header_params)
    } else {
        None
    };
    let mut hoisted: HashSet<Inst> = HashSet::new();
    let mut order = Vec::new();
    let mut spec = HashSet::new();
    // Whether every instruction of the header so far is neutral or moved.
    let mut prefix_clean = true;
    for &block in &lp.blocks {
        let is_header = block == header;
        // A non-header block may host a hoisted guard only when it runs on
        // every path through the loop (it dominates every latch and every
        // block with an edge leaving the loop), and every block on the way to
        // it is neutral. Otherwise a first iteration that skips it (and exits)
        // would deopt into a state that never existed.
        let always = !is_header
            && terminals
                .iter()
                .all(|&terminal| dom.dominates(block, terminal));
        let path_clean = always && neutral_path_to(func, lp, block);
        let mut local_clean = true;
        for &inst in &func.blocks[block].insts {
            let data = &func.insts[inst];
            let op = data.op;
            let operands_ok = func.args(inst).iter().all(|&v| scan.invariant(&hoisted, v));
            let guard_here = if is_header {
                prefix_clean
            } else {
                path_clean && local_clean
            };
            let movable = data.result.is_some()
                && operands_ok
                && (pure_hoistable(op)
                    || (is_load(op) && !clobber.overlaps(op.effects().reads))
                    || (guard_here
                        && guard_hoistable(op)
                        && data
                            .frame
                            .is_some_and(|frame| scan.frame_ok(&hoisted, header_params, frame))));
            if movable {
                hoisted.insert(inst);
                order.push(inst);
                continue;
            }

            if entry.is_some()
                && !guard_here
                && data.result.is_some()
                && operands_ok
                && guard_hoistable(op)
                && data.frame.is_some()
            {
                hoisted.insert(inst);
                order.push(inst);
                spec.insert(inst);
                continue;
            }
            if !neutral(op) {
                if is_header {
                    prefix_clean = false;
                }
                local_clean = false;
            }
        }
    }
    Plan { order, spec, entry }
}

/// What one step moves.
struct Plan {
    /// The instructions to move, definitions before uses.
    order: Vec<Inst>,
    /// The guards of `order` that move speculatively: they get `entry` as
    /// their frame.
    spec: HashSet<Inst>,
    entry: Option<FrameState>,
}

/// Moves the planned instructions into the preheader of `lp`, creating it
/// when needed.
fn apply(func: &mut Func, lp: &Loop, plan: &Plan) {
    let order = &plan.order;
    let header = lp.header;
    let header_params: Vec<Value> = func.params(header).to_vec();
    let Some((pre, entry_args)) = ensure_preheader(func, lp) else {
        return;
    };

    let mut hoist = Hoist {
        header_params,
        entry_args,
        pre,
        in_loop: lp
            .blocks
            .iter()
            .flat_map(|&b| func.blocks[b].insts.iter().copied())
            .collect(),
        moving: order.iter().copied().collect(),
        copies: HashMap::new(),
    };
    // Each moved instruction is appended to the preheader after the copies its
    // operands and frame needed, which are made while it is mapped, so every
    // copy follows the moved instructions it reads and precedes its user.
    for &inst in order {
        let args: Vec<Value> = func.args(inst).to_vec();
        let mapped: Vec<Value> = args.iter().map(|&v| hoist.value(func, v)).collect();
        if mapped != args {
            func.insts[inst].args = EntityList::from_slice(&mapped, &mut func.pool);
        }
        let frame = if plan.spec.contains(&inst) {
            plan.entry
        } else {
            func.insts[inst].frame
        };
        if let Some(frame) = frame {
            let mapped = hoist.frame(func, frame);
            func.insts[inst].frame = Some(mapped);
        }
        func.blocks[pre].insts.push(inst);
    }
    for &block in &lp.blocks {
        func.blocks[block]
            .insts
            .retain(|inst| !hoist.moving.contains(inst));
    }
}

/// The frame a test in the preheader of `lp` deopts with, if the loop has one
/// (see `Scan::entry_frame`). The frame is still in terms of the loop's own
/// values; [`entry_frame_in`] maps it into the preheader.
pub(super) fn entry_frame_of(func: &Func, lp: &Loop) -> Option<FrameState> {
    let inst_block = inst_blocks(func);
    let scan = Scan {
        func,
        lp,
        inst_block: &inst_block,
    };
    scan.entry_frame(func.params(lp.header))
}

/// A copy of `frame` (from [`entry_frame_of`]) that reads the values entering
/// the header from the preheader `pre`. Pure computations the frame needs are
/// copied into `pre`.
pub(super) fn entry_frame_in(
    func: &mut Func,
    lp: &Loop,
    pre: Block,
    entry_args: &[Value],
    frame: FrameState,
) -> FrameState {
    let mut hoist = Hoist {
        header_params: func.params(lp.header).to_vec(),
        entry_args: entry_args.to_vec(),
        pre,
        in_loop: lp
            .blocks
            .iter()
            .flat_map(|&b| func.blocks[b].insts.iter().copied())
            .collect(),
        moving: HashSet::new(),
        copies: HashMap::new(),
    };
    hoist.frame(func, frame)
}

/// How moved instructions and their frames are read in the preheader.
struct Hoist {
    /// The parameters of the header, and the values that enter it from the
    /// preheader: its registers on the first iteration.
    header_params: Vec<Value>,
    entry_args: Vec<Value>,
    pre: Block,
    /// Every instruction of the loop, before any move.
    in_loop: HashSet<Inst>,
    /// The instructions that move; their results keep their values.
    moving: HashSet<Inst>,
    /// The copy of each in-loop constant or pure computation, made in the
    /// preheader.
    copies: HashMap<Value, Value>,
}

impl Hoist {
    /// The value `v` stands for in the preheader.
    fn value(&mut self, func: &mut Func, v: Value) -> Value {
        if let Some(index) = self.header_params.iter().position(|&p| p == v) {
            return self.entry_args[index];
        }
        let ValueDef::Inst(def) = func.value_def(v) else {
            return v;
        };
        if !self.in_loop.contains(&def) || self.moving.contains(&def) {
            return v;
        }
        if let Some(&copy) = self.copies.get(&v) {
            return copy;
        }
        let op = func.insts[def].op;
        let args: Vec<Value> = func.args(def).to_vec();
        let mapped: Vec<Value> = args.iter().map(|&arg| self.value(func, arg)).collect();
        let ty = func.value_type(v);
        let copy = func
            .push_inst(self.pre, op, &mapped, Some(ty), None)
            .expect("a rematerialized operation has a result");
        self.copies.insert(v, copy);
        copy
    }

    /// A copy of `frame` and its parents, with each value read in the
    /// preheader.
    fn frame(&mut self, func: &mut Func, frame: FrameState) -> FrameState {
        let data = func.frames[frame].clone();
        let closure = self.value(func, data.closure);
        let mut registers = Vec::with_capacity(data.registers.len());
        for &(reg, v) in &data.registers {
            registers.push((reg, self.value(func, v)));
        }
        let parent = data.parent.map(|parent| self.frame(func, parent));
        func.push_frame(FrameStateData {
            code: data.code,
            pc: data.pc,
            closure,
            registers,
            parent,
            call_base: data.call_base,
            returned: data.returned,
            rest: data.rest,
            inline_site: data.inline_site,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::frame_state::{CallSite, FrameStateData};
    use crate::runtime::codegen::opt::ir::func::ValueDef;
    use crate::runtime::codegen::opt::ir::inst::Constant;
    use crate::runtime::codegen::opt::ir::types::{CodeRef, Type};
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, frame, jump, placed, push, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;
    use cranelift_entity::EntityRef;

    /// An allocation is not a guard: moving it out of the loop would merge
    /// per-iteration objects into one.
    #[test]
    fn alloc_rational_is_not_hoisted() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let closure = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let cond = const_bool(&mut func, entry, false);

        // Header parameters: the closure and the loop condition.
        let header = block(&mut func);
        let closure_param = func.append_param(header, Type::Any);
        let cond_param = func.append_param(header, Type::Bool);
        jump(&mut func, entry, header, &[closure, cond]);

        // An allocation with loop-invariant operands, first in the header,
        // which is exactly where the old guard rule moved it from.
        let one = const_any(&mut func, header, SchemeValue::from_i32(1).bits());
        let three = const_any(&mut func, header, SchemeValue::from_i32(3).bits());
        let alloc_frame = frame(&mut func, closure_param);
        let alloc = func
            .push_inst(
                header,
                Op::AllocRational,
                &[one, three],
                Some(Type::Rational),
                Some(alloc_frame),
            )
            .expect("an allocation has a result");

        let body = block(&mut func);
        let exit = block(&mut func);
        branch(&mut func, header, cond_param, body, &[], exit, &[]);
        jump(&mut func, body, header, &[closure_param, cond_param]);
        ret(&mut func, exit, closure_param);

        verify(&func).expect("the test function is valid");
        let ValueDef::Inst(alloc_inst) = func.value_def(alloc) else {
            panic!("the allocation is an instruction result");
        };
        let outcome = run(&mut func);
        assert!(!outcome.changed, "the allocation must not move");
        assert!(
            placed(&func, alloc_inst),
            "AllocRational stays in the header"
        );
    }

    /// A guard in an inlined body that runs on every path through the loop
    /// moves to the preheader with its inline frame chain intact. The loop is
    /// post-tested: the header always enters the body.
    #[test]
    fn guard_in_an_inlined_body_is_hoisted() {
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
        jump(&mut func, entry, header, &[x0, cond0]);
        jump(&mut func, header, body, &[]);
        jump(&mut func, body, latch, &[]);
        branch(
            &mut func,
            latch,
            cond_param,
            header,
            &[x_param, cond_param],
            exit,
            &[],
        );
        ret(&mut func, exit, x_param);

        // The guard's frame is the spliced callee's, with the caller's frame
        // as its parent.
        let site = func.push_frame(FrameStateData {
            code: CodeRef::new(0),
            pc: 0,
            closure: x_param,
            registers: vec![(1, x_param)],
            parent: None,
            call_base: None,
            returned: None,
            rest: None,
            inline_site: None,
        });
        let inlined = func.push_frame(FrameStateData {
            code: CodeRef::new(0),
            pc: 0,
            closure: x_param,
            registers: vec![(1, x_param)],
            parent: Some(site),
            call_base: Some(0),
            returned: None,
            rest: None,
            inline_site: Some(CallSite {
                call_pc: 0,
                caller: CodeRef::new(0),
            }),
        });
        let guard = func
            .push_inst(
                body,
                Op::CheckInt32,
                &[x_param],
                Some(Type::Int32),
                Some(inlined),
            )
            .expect("a guard has a result");
        let ValueDef::Inst(guard_inst) = func.value_def(guard) else {
            panic!("the guard is an instruction result");
        };

        verify(&func).expect("the test function is valid");
        let outcome = run(&mut func);
        assert!(outcome.changed, "the invariant guard moves");
        assert!(
            !func.blocks[body].insts.contains(&guard_inst),
            "the guard left the inlined body"
        );
        assert!(
            func.blocks[entry].insts.contains(&guard_inst),
            "the guard is in the preheader"
        );
        let moved = func.insts[guard_inst]
            .frame
            .expect("the guard keeps a frame");
        assert!(
            func.frames[moved].inline_site.is_some(),
            "the inline site is preserved"
        );
        assert!(
            func.frames[moved].parent.is_some(),
            "the caller frame is preserved"
        );
    }

    /// A guard whose frame holds a per-iteration pure value (`i + 1`) still
    /// moves: the preheader recomputes the value from the entry arguments.
    /// A frame holding a load result keeps the guard in the loop.
    #[test]
    fn guard_with_a_pure_frame_chain_is_hoisted() {
        for (pure, hoisted) in [(true, true), (false, false)] {
            let mut func: Func<'static> = Func::new();
            let entry = func.entry;
            let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
            let i0 = push(
                &mut func,
                entry,
                Op::Const(Constant::Int64(0)),
                &[],
                Type::Int64,
            );
            let cond0 = const_bool(&mut func, entry, true);

            let header = block(&mut func);
            let x = func.append_param(header, Type::Any);
            let i = func.append_param(header, Type::Int64);
            let cond = func.append_param(header, Type::Bool);
            let body = block(&mut func);
            let latch = block(&mut func);
            let exit = block(&mut func);
            jump(&mut func, entry, header, &[x0, i0, cond0]);
            jump(&mut func, header, body, &[]);

            // The value in the guard's frame, defined in the iteration.
            let one = push(
                &mut func,
                body,
                Op::Const(Constant::Int64(1)),
                &[],
                Type::Int64,
            );
            let next = if pure {
                push(&mut func, body, Op::AddI64, &[i, one], Type::Int64)
            } else {
                {
                    let add_frame = frame(&mut func, x);
                    func.push_inst(
                        body,
                        Op::AddI64Ovf,
                        &[i, one],
                        Some(Type::Int64),
                        Some(add_frame),
                    )
                    .expect("an add has a result")
                }
            };
            let guard_frame = func.push_frame(FrameStateData {
                code: CodeRef::new(0),
                pc: 0,
                closure: x,
                registers: vec![(1, next)],
                parent: None,
                call_base: None,
                returned: None,
                rest: None,
                inline_site: None,
            });
            let guard = func
                .push_inst(
                    body,
                    Op::CheckInt32,
                    &[x],
                    Some(Type::Int32),
                    Some(guard_frame),
                )
                .expect("a guard has a result");
            jump(&mut func, body, latch, &[]);
            branch(&mut func, latch, cond, header, &[x, next, cond], exit, &[]);
            ret(&mut func, exit, x);
            let ValueDef::Inst(guard_inst) = func.value_def(guard) else {
                panic!("the guard is an instruction result");
            };

            verify(&func).expect("the test function is valid");
            run(&mut func);
            verify(&func).expect("the hoisted function is valid");
            assert_eq!(
                func.blocks[entry].insts.contains(&guard_inst),
                hoisted,
                "pure frame chain: {pure}"
            );
        }
    }

    /// A guard in a block the first iteration may skip (the header exits
    /// before reaching the body) must stay: moving it would deopt into a
    /// state that never existed.
    #[test]
    fn guard_in_a_skippable_body_stays() {
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
        jump(&mut func, entry, header, &[x0, cond0]);
        branch(&mut func, header, cond_param, body, &[], exit, &[]);
        jump(&mut func, body, latch, &[]);
        branch(
            &mut func,
            latch,
            cond_param,
            header,
            &[x_param, cond_param],
            exit,
            &[],
        );
        ret(&mut func, exit, x_param);

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
        let outcome = run(&mut func);
        assert!(!outcome.changed, "the guard must not move");
        assert!(func.blocks[body].insts.contains(&guard_inst));
    }
}
