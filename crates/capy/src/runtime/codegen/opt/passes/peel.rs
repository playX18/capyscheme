//! Loop peeling and Generic replication of small loops.
//!
//! **Peeling** copies the first iteration of a loop out of it:
//!
//! ```text
//!   before:  pre -> H -> body -> H ...        (guards run every iteration)
//!   after:   pre -> H' -> body' -> H -> body -> H ...
//! ```
//!
//! `H'`/`body'` are a clone of the loop whose back edges lead into the
//! original header, so the clone is exactly the first iteration and the
//! original loop runs the rest. Every guard that tests a loop-invariant value
//! now runs in the clone first, and the clone dominates the loop proper, so
//! [`typeprop`](super::typeprop) removes the loop's copy of the guard, and
//! the loads that it protected become invariant for [`licm`](super::licm).
//! V8's loop peeling (`loop-peeling.cc`) is where we take algorithm from.
//!
//! Unlike [`version`](super::version), peeling needs no test and no second
//! loop, and it keeps the original error timing: the first iteration deopts
//! at the same point it always did. It only helps guards that the first
//! iteration reaches, which is why the profitability test asks for a guard
//! whose operands are invariant or computed from invariants.
//!
//! OSR entries are redirected to the peeled header, so every way into the loop
//! passes the peeled guards.
//!
//! **Replication** ([`run_unroll`]) is the same cloning with `n` copies
//! chained back to the original header, for small innermost loops that the
//! counted 2x [`unroll`](super::unroll) leaves alone. Every copy keeps its
//! exit test, so no trip count is needed; the win is fewer back edges and
//! guards the copies make redundant for one another.
//!
//! A value defined in the loop and read after it goes through exit-block
//! parameters first ([`lcssa`](super::lcssa)).

use std::collections::{HashMap, HashSet};

use super::lcssa::{edges_mut, form_lcssa};
use super::loops::{Loop, find_loops, inst_blocks};
use super::unroll;
use super::version::{LoopClone, clone_loop, invariant};
use super::{PassOutcome, is_load};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Op, Terminator};

/// The largest loop, in instructions, that peeling will copy.
const MAX_PEEL_INSTS: usize = 64;
/// The largest unrolled body, in instructions, that replication produces.
const MAX_UNROLLED_INSTS: usize = 96;
/// The most copies replication makes.
const MAX_FACTOR: usize = 4;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    run_peel(func)
}

pub(super) fn run_unroll(func: &mut Func) -> PassOutcome {
    run_replicate(func)
}

fn loop_size(func: &Func, lp: &Loop) -> usize {
    lp.blocks
        .iter()
        .map(|&block| func.blocks[block].insts.len())
        .sum()
}

fn run_peel(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    for lp in &loops {
        work += lp.blocks.len();
        if func.peeled.contains(&lp.header) {
            continue;
        }
        // A loop is judged once, whether or not it is peeled.
        if loop_size(func, lp) > MAX_PEEL_INSTS || !profitable(func, lp) {
            func.peeled.insert(lp.header);
            continue;
        }
        if !form_lcssa(func, lp) {
            func.peeled.insert(lp.header);
            continue;
        }
        func.peeled.insert(lp.header);
        replicate(func, lp, 1, true);
        return PassOutcome {
            changed: true,
            work: work + func.insts.len(),
        };
    }
    PassOutcome {
        changed: false,
        work,
    }
}

fn run_replicate(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    let headers: Vec<Block> = loops.iter().map(|lp| lp.header).collect();
    for lp in &loops {
        work += lp.blocks.len();
        if func.unrolled.contains(&lp.header) {
            continue;
        }
        func.unrolled.insert(lp.header);
        let size = loop_size(func, lp);
        let factor = (MAX_UNROLLED_INSTS / size.max(1)).min(MAX_FACTOR);
        if factor < 2 || !replicable(func, lp, &headers) {
            continue;
        }
        if !form_lcssa(func, lp) {
            continue;
        }
        replicate(func, lp, factor - 1, false);
        return PassOutcome {
            changed: true,
            work: work + func.insts.len(),
        };
    }
    PassOutcome {
        changed: false,
        work,
    }
}

/// Whether `lp` holds a deopting guard that peeling would make redundant:
/// one whose operands are loop-invariant, or computed from loop-invariant
/// values by guards, loads and pure operations.
fn profitable(func: &Func, lp: &Loop) -> bool {
    let inst_block = inst_blocks(func);
    let mut chain: HashSet<Value> = HashSet::new();
    let known = |chain: &HashSet<Value>, value: Value| {
        chain.contains(&value) || invariant(func, lp, &inst_block, value)
    };
    // Reverse postorder visits a definition before its uses.
    for &block in &lp.blocks {
        for &inst in &func.blocks[block].insts {
            let data = &func.insts[inst];
            let op = data.op;
            if !func.args(inst).iter().all(|&arg| known(&chain, arg)) {
                continue;
            }
            let guard = op.can_deopt() && op.effects().writes.is_empty() && !op.is_call();
            if guard && data.result.is_some() && !func.args(inst).is_empty() {
                // A guard on an operand the first iteration can establish.
                if is_peel_guard(op) {
                    return true;
                }
            }
            if let Some(result) = data.result {
                if guard || is_load(op) || is_pure(op) {
                    chain.insert(result);
                }
            }
        }
    }
    false
}

/// The guards that a dominating copy makes redundant.
fn is_peel_guard(op: Op) -> bool {
    matches!(
        op,
        Op::CheckInt32 | Op::CheckInt64 | Op::CheckClass(_) | Op::CheckCode(_) | Op::CheckDouble
    )
}

fn is_pure(op: Op) -> bool {
    let effects = op.effects();
    op.has_result()
        && !op.can_deopt()
        && !op.is_call()
        && effects.writes.is_empty()
        && effects.reads.is_empty()
        && effects.causes.is_empty()
}

/// Whether replication may copy `lp`: an innermost loop without calls that
/// the counted unroller does not already take.
fn replicable(func: &Func, lp: &Loop, headers: &[Block]) -> bool {
    if headers
        .iter()
        .any(|&other| other != lp.header && lp.member[other])
    {
        return false;
    }
    for &block in &lp.blocks {
        if matches!(func.blocks[block].term, Some(Terminator::TailCall { .. })) {
            return false;
        }
        for &inst in &func.blocks[block].insts {
            let op = func.insts[inst].op;
            if op.is_call() && op != Op::Safepoint {
                return false;
            }
        }
    }
    !unroll::counted(func, lp)
}

/// Makes `copies` clones of `lp` and chains them with the original.
fn replicate(func: &mut Func, lp: &Loop, copies: usize, leading: bool) {
    let header = lp.header;
    let none = HashSet::new();
    let clones: Vec<LoopClone> = (0..copies)
        .map(|_| clone_loop(func, lp, &none, &none, &HashMap::new()))
        .collect();
    let heads: Vec<Block> = clones.iter().map(|clone| clone.blocks[&header]).collect();

    // The back edges of each copy go to the next one.
    let next_of = |index: usize| heads.get(index + 1).copied().unwrap_or(header);
    for (index, clone) in clones.iter().enumerate() {
        let target = next_of(index);
        for &block in clone.blocks.values() {
            retarget(func, block, heads[index], target);
        }
    }
    if leading {
        // Enter through the first clone instead of the original header.
        let first = heads[0];
        let blocks: Vec<Block> = func.blocks.keys().collect();
        let inside: HashSet<Block> = clones
            .iter()
            .flat_map(|clone| clone.blocks.values().copied())
            .collect();
        for block in blocks {
            if lp.member[block] || inside.contains(&block) {
                continue;
            }
            retarget(func, block, header, first);
        }
        for entry in &mut func.osr {
            if entry.header == header {
                entry.header = first;
            }
        }
    } else {
        for &block in &lp.blocks {
            retarget(func, block, header, heads[0]);
        }
    }
}

/// Points every edge of `block` into `from` at `to`, keeping its arguments.
fn retarget(func: &mut Func, block: Block, from: Block, to: Block) {
    let Some(mut term) = func.blocks[block].term.take() else {
        return;
    };
    for edge in edges_mut(&mut term) {
        if edge.block == from {
            edge.block = to;
        }
    }
    func.blocks[block].term = Some(term);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::types::Type;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, frame, jump, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    fn guards(func: &Func) -> usize {
        let live = super::super::reachable(func);
        func.blocks
            .iter()
            .filter(|(block, _)| live[*block])
            .flat_map(|(_, data)| data.insts.iter())
            .filter(|&&inst| func.insts[inst].op == Op::CheckInt32)
            .count()
    }

    /// `while (c) { check.int32 x }` with `x` invariant and read after.
    fn build(guarded: bool) -> (Func<'static>, Block) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let c0 = const_bool(&mut func, entry, true);
        let header = block(&mut func);
        let x = func.append_param(header, Type::Any);
        let c = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let exit = block(&mut func);
        jump(&mut func, entry, header, &[x0, c0]);
        branch(&mut func, header, c, body, &[], exit, &[]);
        jump(&mut func, body, header, &[x, c]);
        ret(&mut func, exit, x);
        if guarded {
            let guard_frame = frame(&mut func, x);
            func.push_inst(
                body,
                Op::CheckInt32,
                &[x],
                Some(Type::Int32),
                Some(guard_frame),
            );
        }
        (func, header)
    }

    #[test]
    fn invariant_guard_loop_is_peeled() {
        let (mut func, header) = build(true);
        verify(&func).expect("the test function is valid");
        let outcome = run_peel(&mut func);
        assert!(outcome.changed);
        assert!(func.peeled.contains(&header));
        assert_eq!(guards(&func), 2, "one in the peeled copy, one in the loop");
        verify(&func).expect("the peeled function is valid");
        // A second run does nothing: the loop is marked.
        assert!(!run_peel(&mut func).changed);
    }

    #[test]
    fn loop_without_guards_is_not_peeled() {
        let (mut func, header) = build(false);
        let outcome = run_peel(&mut func);
        assert!(!outcome.changed);
        assert!(func.peeled.contains(&header));
    }

    #[test]
    fn small_loop_is_replicated() {
        let (mut func, header) = build(true);
        verify(&func).expect("the test function is valid");
        let outcome = run_replicate(&mut func);
        assert!(outcome.changed);
        assert!(func.unrolled.contains(&header));
        assert!(guards(&func) >= 2, "each copy keeps its guard");
        verify(&func).expect("the replicated function is valid");
    }
}
