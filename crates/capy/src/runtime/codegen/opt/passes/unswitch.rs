//! Loop unswitching on loop-invariant branches.
//!
//! A branch inside a loop whose condition is the same on every iteration
//! decides the same way each time. Unswitching tests it once:
//!
//! ```text
//!   preheader: branch c -> loop_t, loop_f
//!   loop_t:    clone of the loop with the branch replaced by `then`
//!   loop_f:    clone of the loop with the branch replaced by `otherwise`
//! ```
//!
//! Evaluating `c` on the preheader is safe because it is already defined
//! outside the loop (or is a header parameter that the loop never changes),
//! and a branch condition is a plain `Bool` that cannot fail. Moving the
//! decision earlier therefore changes no effect: both clones run the same
//! instructions as the original, minus a branch that always went one way.
//!
//! The original loop stays in the function. Nothing but OSR entries names it
//! afterwards, so an OSR entry is never wrong and ordinary control flow runs
//! the specialised clones. A value defined in the loop and read after it is
//! first routed through exit-block parameters ([`lcssa`](super::lcssa)),
//! whose parameters then join the three loops.
//!
//! Each loop is split at most once ([`Func::unswitched`] holds the headers of
//! split loops and of both clones), which bounds code growth to 3x per loop.

use std::collections::{HashMap, HashSet};

use super::PassOutcome;
use super::lcssa::form_lcssa;
use super::loops::{Loop, ensure_preheader, find_loops, inst_blocks};
use super::version::{clone_loop, invariant, preheader_value};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Op, Terminator};

/// The largest loop, in instructions, that one run will triplicate.
const MAX_LOOP_INSTS: usize = 64;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    if !opt().unswitch {
        return PassOutcome::default();
    }
    run_enabled(func)
}

/// The pass proper, without the configuration gate, so tests exercise it
/// regardless of the environment.
fn run_enabled(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let mut loops = find_loops(func, &dom);
    loops.sort_by_key(|lp| lp.blocks.len());
    let mut work = func.blocks.len();
    for lp in &loops {
        work += lp.blocks.len();
        if func.unswitched.contains(&lp.header) {
            continue;
        }
        let Some((block, cond)) = plan(func, lp) else {
            continue;
        };
        if !form_lcssa(func, lp) {
            continue;
        }
        if !apply(func, lp, block, cond) {
            continue;
        }
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

/// A loop block that ends in a branch on a loop-invariant condition, with
/// that condition.
fn plan(func: &Func, lp: &Loop) -> Option<(Block, Value)> {
    let size: usize = lp
        .blocks
        .iter()
        .map(|&block| func.blocks[block].insts.len())
        .sum();
    if size > MAX_LOOP_INSTS {
        return None;
    }
    let inst_block = inst_blocks(func);
    for &block in &lp.blocks {
        let Some(Terminator::Branch {
            cond,
            then,
            otherwise,
        }) = &func.blocks[block].term
        else {
            continue;
        };
        if then.block == otherwise.block {
            continue;
        }
        // A constant condition is the business of `simplify`.
        if let ValueDef::Inst(def) = func.value_def(*cond) {
            if matches!(func.insts[def].op, Op::Const(_)) {
                continue;
            }
        }
        if invariant(func, lp, &inst_block, *cond) {
            return Some((block, *cond));
        }
    }
    None
}

/// Splits `lp` on the branch that ends `block`. Returns whether it did.
fn apply(func: &mut Func, lp: &Loop, block: Block, cond: Value) -> bool {
    let header = lp.header;
    let header_params: Vec<Value> = func.params(header).to_vec();
    let Some((pre, entry_args)) = ensure_preheader(func, lp) else {
        return false;
    };
    let inst_block = inst_blocks(func);
    let mut consts: HashMap<Value, Value> = HashMap::new();
    let cond_pre = preheader_value(
        func,
        pre,
        lp,
        &header_params,
        &entry_args,
        &inst_block,
        &mut consts,
        cond,
    );

    let none = HashSet::new();
    let taken = clone_loop(func, lp, &none, &none, &HashMap::from([(block, true)]));
    let skipped = clone_loop(func, lp, &none, &none, &HashMap::from([(block, false)]));
    let then = func.edge(taken.blocks[&header], &entry_args);
    let otherwise = func.edge(skipped.blocks[&header], &entry_args);
    func.set_terminator(
        pre,
        Terminator::Branch {
            cond: cond_pre,
            then,
            otherwise,
        },
    );
    func.unswitched.insert(header);
    func.unswitched.insert(taken.blocks[&header]);
    func.unswitched.insert(skipped.blocks[&header]);
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::types::Type;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, jump, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    /// Builds `while (c) { if (flag) A else B }` with `flag` invariant and
    /// the loop value `x` read after the loop.
    fn build(flag_invariant: bool) -> (Func<'static>, Block) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let c0 = const_bool(&mut func, entry, true);
        let flag0 = const_bool(&mut func, entry, true);
        let header = block(&mut func);
        let x = func.append_param(header, Type::Any);
        let c = func.append_param(header, Type::Bool);
        let flag = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let left = block(&mut func);
        let right = block(&mut func);
        let latch = block(&mut func);
        let exit = block(&mut func);
        jump(&mut func, entry, header, &[x0, c0, flag0]);
        branch(&mut func, header, c, body, &[], exit, &[]);
        branch(&mut func, body, flag, left, &[], right, &[]);
        jump(&mut func, left, latch, &[]);
        jump(&mut func, right, latch, &[]);
        // The variant loop swaps the two flags every iteration.
        let back = if flag_invariant {
            [x, c, flag]
        } else {
            [x, flag, c]
        };
        jump(&mut func, latch, header, &back);
        ret(&mut func, exit, x);
        (func, header)
    }

    fn branches(func: &Func) -> usize {
        let live = super::super::reachable(func);
        func.blocks
            .iter()
            .filter(|(block, data)| {
                live[*block] && matches!(data.term, Some(Terminator::Branch { .. }))
            })
            .count()
    }

    #[test]
    fn invariant_branch_is_unswitched() {
        let (mut func, header) = build(true);
        verify(&func).expect("the test function is valid");
        let before = branches(&func);
        let outcome = run_enabled(&mut func);
        assert!(outcome.changed);
        assert!(func.unswitched.contains(&header));
        verify(&func).expect("the unswitched function is valid");
        // The preheader's test plus each clone's header test; the original
        // loop's two branches are unreachable.
        assert!(branches(&func) <= before + 2);
    }

    #[test]
    fn variant_branch_is_not_unswitched() {
        let (mut func, header) = build(false);
        let outcome = run_enabled(&mut func);
        assert!(!outcome.changed);
        assert!(!func.unswitched.contains(&header));
    }
}
