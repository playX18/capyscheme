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

use super::forward::def_inst;
use super::lcssa::form_lcssa;
use super::loops::{Loop, canon, find_loops, induction_step, inst_blocks, int_const};
use super::unroll;
use super::version::{LoopClone, clone_loop, invariant};
use super::{PassOutcome, is_load};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Op, Terminator};

/// The largest loop, in instructions, that peeling will copy.
const MAX_PEEL_INSTS: usize = 64;
/// The largest unrolled body, in instructions, that replication produces.
const MAX_UNROLLED_INSTS: usize = 96;
/// The most copies replication makes.
const MAX_FACTOR: usize = 4;
/// The largest fully unrolled loop, in instructions, before folding.
const MAX_FULL_UNROLL_INSTS: usize = 6144;
/// The most iterations a full unroll copies out.
const MAX_FULL_TRIP: i64 = 96;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    run_peel(func)
}

/// The `unroll_all` pass: replicate every loop body, not only the ones the
/// `unroll` pass picks. Off unless `CAPY_OPT_UNROLL_ALL` is set.
pub(super) fn run_unroll(func: &mut Func) -> PassOutcome {
    if !crate::runtime::codegen::jit::config::opt().unroll_all {
        return PassOutcome::default();
    }
    run_replicate(func)
}

/// The `unroll_full` pass: a loop that runs a constant number of times
/// (`(do ([t 0 (fx+ t 1)]) ((fx=? t 64)) ...)`) is replaced by that many
/// copies of its body. The copies are peeled iterations, each still with its
/// exit test, so the result stays correct for any trip count; the count only
/// decides profitability. With the counter a constant in every copy, the
/// tests, the index arithmetic and the bounds checks fold away, and the
/// original loop, entered with the counter past the bound, folds to its exit.
pub(super) fn run_full(func: &mut Func) -> PassOutcome {
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
        let Some(trip) = trip_count(func, lp) else {
            continue;
        };
        // The loop is judged once, whether or not it is unrolled.
        func.unrolled.insert(lp.header);
        let size = loop_size(func, lp);
        if trip == 0
            || trip > MAX_FULL_TRIP
            || size.saturating_mul(trip as usize) > MAX_FULL_UNROLL_INSTS
            || !innermost_without_calls(func, lp, &headers)
        {
            continue;
        }
        if !form_lcssa(func, lp) {
            continue;
        }
        replicate(func, lp, trip as usize, Entry::Normal);
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

/// How many times the header of `lp` takes its in-loop edge, when the
/// header branches on comparing a counter that starts at a constant and
/// moves by a constant step with a constant bound.
fn trip_count(func: &Func, lp: &Loop) -> Option<i64> {
    let header = lp.header;
    let Some(Terminator::Branch {
        cond,
        then,
        otherwise,
    }) = func.blocks[header].term.clone()
    else {
        return None;
    };
    let continue_is_then = lp.member[then.block];
    if continue_is_then == lp.member[otherwise.block] {
        return None;
    }
    let cond_inst = def_inst(func, cond)?;
    let cmp = match func.insts[cond_inst].op {
        Op::CmpI32(cmp) | Op::CmpI64(cmp) => cmp,
        _ => return None,
    };
    let args = func.args(cond_inst);
    if args.len() != 2 {
        return None;
    }
    let params = func.params(header);
    let (index, counter_is_lhs, bound) = (0..params.len()).find_map(|index| {
        if canon(func, args[0]) == params[index] {
            Some((index, true, int_const(func, args[1])?))
        } else if canon(func, args[1]) == params[index] {
            Some((index, false, int_const(func, args[0])?))
        } else {
            None
        }
    })?;
    let step = induction_step(func, lp, header, index)?.delta;
    if step == 0 {
        return None;
    }
    // Every edge into the loop passes the same constant start. An OSR entry
    // starts the counter anywhere; it keeps entering the original loop.
    let mut start = None;
    for pred in func.blocks.keys() {
        if lp.member[pred] || is_osr_block(func, pred) {
            continue;
        }
        let Some(term) = &func.blocks[pred].term else {
            continue;
        };
        for edge in term.edges() {
            if edge.block != header {
                continue;
            }
            let arg = *func.edge_args(&edge).get(index)?;
            let value = int_const(func, canon(func, arg))?;
            match start {
                None => start = Some(value),
                Some(previous) if previous == value => {}
                Some(_) => return None,
            }
        }
    }
    let mut counter = start?;
    let mut trip = 0;
    loop {
        let (lhs, rhs) = if counter_is_lhs {
            (counter, bound)
        } else {
            (bound, counter)
        };
        let holds = match cmp {
            Cmp::Eq => lhs == rhs,
            Cmp::Ne => lhs != rhs,
            Cmp::Lt => lhs < rhs,
            Cmp::Le => lhs <= rhs,
            Cmp::Gt => lhs > rhs,
            Cmp::Ge => lhs >= rhs,
        };
        if holds != continue_is_then {
            return Some(trip);
        }
        trip += 1;
        if trip > MAX_FULL_TRIP {
            return None;
        }
        counter = counter.checked_add(step)?;
    }
}

/// Whether `lp` holds no other loop and makes no calls.
fn innermost_without_calls(func: &Func, lp: &Loop, headers: &[Block]) -> bool {
    if headers
        .iter()
        .any(|&other| other != lp.header && lp.member[other])
    {
        return false;
    }
    lp.blocks.iter().all(|&block| {
        !matches!(func.blocks[block].term, Some(Terminator::TailCall { .. }))
            && func.blocks[block].insts.iter().all(|&inst| {
                let op = func.insts[inst].op;
                !op.is_call() || op == Op::Safepoint
            })
    })
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
        replicate(func, lp, 1, Entry::All);
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
        replicate(func, lp, factor - 1, Entry::Original);
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

/// Whether `block` is the block an OSR entry starts in.
fn is_osr_block(func: &Func, block: Block) -> bool {
    func.osr.iter().any(|entry| entry.block == block)
}

/// Which way into a replicated loop enters the copies.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Entry {
    /// Every entry, OSR ones included, enters the first copy (peeling).
    All,
    /// Entries other than OSR enter the first copy; an OSR entry, which
    /// starts the loop at any iteration, keeps entering the original
    /// (full unrolling).
    Normal,
    /// The entries keep the original header, whose back edges enter the
    /// copies (replication).
    Original,
}

/// Makes `copies` clones of `lp` and chains them with the original.
fn replicate(func: &mut Func, lp: &Loop, copies: usize, entry: Entry) {
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
            func.retarget(block, heads[index], target);
        }
    }

    if entry != Entry::Original {
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
            if entry == Entry::Normal && is_osr_block(func, block) {
                continue;
            }
            func.retarget(block, header, first);
        }
        if entry == Entry::All {
            for osr in &mut func.osr {
                if osr.header == header {
                    osr.header = first;
                }
            }
        }
    } else {
        for &block in &lp.blocks {
            func.retarget(block, header, heads[0]);
        }
    }
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
