//! # Global Value Numbering
//!
//! ## Pure values
//!
//! An instruction in [`numbered`] computes its result from its
//! operands alone. A dominating instruction with the same operation and
//! operands already computed it, and SSA values never change, so its result
//! replaces this one. The guards in the list (overflow, char, length) are
//! numbered too: a dominating guard on the same operands either passed, so
//! this one passes, or deopted before anything here ran. The checks that
//! typeprop already dedupes (`CheckInt32`, `CheckDouble`, `CheckClass`,
//! `CheckCode`, `CheckIndex`) are left to it. Immediate constants are
//! numbered too: lowering rebuilds each one in the block that uses it, so
//! sharing one does not lengthen a live range.
//!
//! ## Known branches
//!
//! A block whose only incoming edge is an arm of a branch
//! knows the branch condition. A branch on the same condition in a block
//! that block dominates takes the known arm.
//!
//! ## Loads
//!
//! A load ([`is_load`]) reuses an earlier load of the same operation
//! and operands when nothing in between may write its region. The scope is an
//! extended basic block: a block whose only incoming edge comes from block
//! `p` inherits the loads of the end of `p`. Every path to such a block passes
//! through `p`, so the path between the two loads is unique and its writes can
//! be checked exactly. A block with several predecessors starts with no loads.
//!
//! Inside a scope, an instruction that clobbers memory drops
//! the loads whose region it overlaps. A call or a safepoint clobbers every
//! region. Allocations are never merged, and neither are instructions with
//! other effects.

use std::collections::{HashMap, HashSet};

use cranelift_entity::SecondaryMap;

use super::{PassOutcome, Subst, is_load, replace_uses, writes_existing};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Constant, Inst, MemoryRanges, Op, Repr, Terminator};

/// A pure operation and its operands, after replacements.
type PureKey = (Op, Vec<Value>);

/// A load and its operands, after replacements.
type LoadKey = (Op, Vec<Value>);

/// A step of a walk over a tree of blocks.
#[derive(Clone, Copy)]
enum Visit {
    Enter(Block),
    Exit(Block),
}

/// Whether `op` computes a value from its operands alone.
fn numbered(op: Op) -> bool {
    matches!(
        op,
        Op::Const(
            Constant::Int32(_) | Constant::Int64(_) | Constant::Bits(_) | Constant::Double(_)
        )
            | Op::Box(Repr::Int32)
            | Op::Box(Repr::Double)
            | Op::Unbox(Repr::Int32)
            // `Box(Repr::Int64)` allocates a one-limb bignum for a value
            // wider than i32, so it stays out of value numbering. The total
            // i64 ops are pure and can be numbered.
            | Op::AddI64
            | Op::SubI64
            | Op::MulI64
            | Op::CmpI64(_)
            | Op::TestInt64
            | Op::Int64ToDouble
            | Op::FloorF64
            | Op::CeilingF64
            | Op::TruncateF64
            | Op::AddF64
            | Op::SubF64
            | Op::MulF64
            | Op::DivF64
            | Op::CmpI32(_)
            | Op::CmpF64(_)
            | Op::EqBits
            | Op::TestInt32
            | Op::TestPair
            | Op::TestChar
            | Op::TestBoolean
            | Op::TestNumber
            | Op::TestClass(_)
            | Op::TestHeap
            | Op::BoolToValue
            | Op::AddI32Ovf
            | Op::SubI32Ovf
            | Op::MulI32Ovf
            | Op::QuotI32
            | Op::RemI32
            | Op::ModI32
            | Op::CheckChar
            | Op::VectorLength
    )
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut subst = Subst::default();
    let mut dead: HashSet<Inst> = HashSet::new();
    let mut work = number_pure(func, &mut subst, &mut dead);
    work += number_loads(func, &mut subst, &mut dead);

    if !dead.is_empty() {
        for (_, data) in func.blocks.iter_mut() {
            data.insts.retain(|inst| !dead.contains(inst));
        }
        replace_uses(func, &subst);
    }
    let folded = fold_known_branches(func);
    PassOutcome {
        changed: !dead.is_empty() || folded,
        work,
    }
}

/// Replaces a branch on a condition that a dominating branch already decided
/// with a jump to the decided arm.
fn fold_known_branches(func: &mut Func) -> bool {
    let dom = func.dom_tree();
    let reach = func.reverse_postorder();
    let mut incoming: SecondaryMap<Block, u32> = SecondaryMap::new();
    let mut source: SecondaryMap<Block, Option<Block>> = SecondaryMap::new();
    for &block in &reach {
        if let Some(term) = &func.blocks[block].term {
            for edge in term.edges() {
                incoming[edge.block] += 1;
                source[edge.block] = Some(block);
            }
        }
    }
    let mut children: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    for &block in dom.rpo() {
        if let Some(parent) = dom.idom(block) {
            children[parent].push(block);
        }
    }
    // What the edge into `block` says about a condition, when it is the only one.
    let learned = |func: &Func, block: Block| -> Option<(Value, bool)> {
        if block == func.entry || incoming[block] != 1 {
            return None;
        }
        let parent = source[block]?;
        match func.blocks[parent].term.as_ref()? {
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } if then.block != otherwise.block => {
                if then.block == block {
                    Some((*cond, true))
                } else if otherwise.block == block {
                    Some((*cond, false))
                } else {
                    None
                }
            }
            _ => None,
        }
    };
    let mut known: HashMap<Value, bool> = HashMap::new();
    let mut rewrites: Vec<(Block, Terminator)> = Vec::new();
    let mut stack = vec![Visit::Enter(func.entry)];
    let mut pushed: SecondaryMap<Block, Option<Value>> = SecondaryMap::new();
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                if let Some((cond, value)) = learned(func, block) {
                    // A dominator's fact wins. A contradicting edge comes from an arm that is
                    // already decided the other way, so this block is dead. Overwriting would
                    // leak past this block's exit, because only fresh entries are undone.
                    if let std::collections::hash_map::Entry::Vacant(slot) = known.entry(cond) {
                        slot.insert(value);
                        pushed[block] = Some(cond);
                    }
                }
                if let Some(Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                }) = &func.blocks[block].term
                {
                    if let Some(&taken) = known.get(cond) {
                        let edge = if taken { *then } else { *otherwise };
                        rewrites.push((block, Terminator::Jump(edge)));
                    }
                }
                stack.push(Visit::Exit(block));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(block) => {
                if let Some(cond) = pushed[block].take() {
                    known.remove(&cond);
                }
            }
        }
    }
    let any = !rewrites.is_empty();
    for (block, term) in rewrites {
        func.set_terminator(block, term);
    }
    any
}

/// Dominator-scoped numbering of [`numbered`] instructions.
fn number_pure(func: &Func, subst: &mut Subst, dead: &mut HashSet<Inst>) -> usize {
    let dom = func.dom_tree();
    let mut children: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    for &block in dom.rpo() {
        if let Some(parent) = dom.idom(block) {
            children[parent].push(block);
        }
    }

    let mut avail: HashMap<PureKey, Value> = HashMap::new();
    let mut added: SecondaryMap<Block, Vec<PureKey>> = SecondaryMap::new();
    let mut work = 0;
    let mut stack = vec![Visit::Enter(func.entry)];
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                for &inst in &func.blocks[block].insts {
                    work += 1;
                    let data = &func.insts[inst];
                    let (Some(result), true) = (data.result, numbered(data.op)) else {
                        continue;
                    };
                    let args: Vec<Value> =
                        func.args(inst).iter().map(|&v| subst.resolve(v)).collect();
                    let key = (data.op, args);
                    match avail.get(&key) {
                        Some(&prev) => {
                            subst.add(result, prev);
                            dead.insert(inst);
                        }
                        None => {
                            avail.insert(key.clone(), result);
                            added[block].push(key);
                        }
                    }
                }
                stack.push(Visit::Exit(block));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(block) => {
                for key in added[block].drain(..) {
                    avail.remove(&key);
                }
            }
        }
    }
    work + func.blocks.len()
}

/// A change to the load table, undone when its block is left.
enum Undo {
    /// A load was added: remove it.
    Insert(LoadKey),
    /// A load was dropped by a clobber: put it back.
    Restore(LoadKey, Value, MemoryRanges),
}

/// Extended-basic-block numbering of loads.
fn number_loads(func: &Func, subst: &mut Subst, dead: &mut HashSet<Inst>) -> usize {
    let reach = func.reverse_postorder();

    // Incoming edges from reachable blocks, and the block of the last one.
    let mut incoming: SecondaryMap<Block, u32> = SecondaryMap::new();
    let mut source: SecondaryMap<Block, Option<Block>> = SecondaryMap::new();
    for &block in &reach {
        if let Some(term) = &func.blocks[block].term {
            for edge in term.edges() {
                incoming[edge.block] += 1;
                source[edge.block] = Some(block);
            }
        }
    }
    // A block with one incoming edge, not the entry, continues the chain of
    // its predecessor. Every other reachable block starts a new chain.
    let mut children: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    let mut roots = Vec::new();
    for &block in &reach {
        match source[block] {
            Some(parent) if block != func.entry && incoming[block] == 1 && parent != block => {
                children[parent].push(block);
            }
            _ => roots.push(block),
        }
    }

    let mut avail: HashMap<LoadKey, (Value, MemoryRanges)> = HashMap::new();
    let mut undo: Vec<Undo> = Vec::new();
    let mut marks: SecondaryMap<Block, usize> = SecondaryMap::new();
    let mut work = 0;
    let mut stack: Vec<Visit> = roots.iter().rev().map(|&b| Visit::Enter(b)).collect();
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                marks[block] = undo.len();
                for &inst in &func.blocks[block].insts {
                    work += 1;
                    let data = &func.insts[inst];
                    let op = data.op;
                    if is_load(op) {
                        let Some(result) = data.result else { continue };
                        let args: Vec<Value> =
                            func.args(inst).iter().map(|&v| subst.resolve(v)).collect();
                        let key = (op, args);
                        match avail.get(&key) {
                            Some(&(prev, _)) => {
                                subst.add(result, prev);
                                dead.insert(inst);
                            }
                            None => {
                                avail.insert(key.clone(), (result, op.effects().reads));
                                undo.push(Undo::Insert(key));
                            }
                        }
                        continue;
                    }
                    // Allocations write only fresh objects, so they do not
                    // invalidate loads of objects that already exist.
                    let writes = writes_existing(op);
                    if writes.is_empty() {
                        continue;
                    }
                    let victims: Vec<LoadKey> = avail
                        .iter()
                        .filter(|(_, (_, reads))| reads.overlaps(writes))
                        .map(|(key, _)| key.clone())
                        .collect();
                    for key in victims {
                        if let Some((value, reads)) = avail.remove(&key) {
                            undo.push(Undo::Restore(key, value, reads));
                        }
                    }
                }
                stack.push(Visit::Exit(block));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(block) => {
                while undo.len() > marks[block] {
                    match undo.pop().expect("the log is longer than the mark") {
                        Undo::Insert(key) => {
                            avail.remove(&key);
                        }
                        Undo::Restore(key, value, reads) => {
                            avail.insert(key, (value, reads));
                        }
                    }
                }
            }
        }
    }
    work + func.blocks.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::inst::Terminator;
    use crate::runtime::codegen::opt::ir::types::Type;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, push, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    /// The arm a block's terminator was folded to, when it became a jump.
    fn jump_target(func: &Func<'_>, block: Block) -> Option<Block> {
        match func.blocks[block].term.as_ref()? {
            Terminator::Jump(edge) => Some(edge.block),
            _ => None,
        }
    }

    /// The shape that miscompiled `pretty-print.scm`'s printer loop:
    ///
    /// ```text
    /// entry: c = (x == K); branch c -> done else outer
    /// outer: branch f1 -> inner else far
    /// inner: c' = (x == K); branch c' -> yes else no     ; must fold to `no`
    /// far:   branch f2 -> dead_head else exit
    /// dead_head: c'' = (x == K); branch c'' -> dead else live
    /// ```
    ///
    /// `dead_head` is reached only on the false arm of `c`, so its then-arm
    /// `dead` is unreachable, yet its edge says `c` is true. That fact used to
    /// overwrite the dominating `c == false` without being undone, and then
    /// leaked into `inner`, folding it to `yes`.
    ///
    /// `inner_first` picks whether `inner` is created before `far`, which
    /// decides the order the dominator tree visits them in.
    fn leaking_fact(inner_first: bool) -> (Func<'static>, Block, Block, Block) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x = func.append_param(entry, Type::Any);
        let f1 = func.append_param(entry, Type::Bool);
        let f2 = func.append_param(entry, Type::Bool);
        let key = SchemeValue::from_i32(6).bits();

        let (done, outer) = (block(&mut func), block(&mut func));
        let (inner, far) = if inner_first {
            let inner = block(&mut func);
            (inner, block(&mut func))
        } else {
            let far = block(&mut func);
            (block(&mut func), far)
        };
        let (yes, no) = (block(&mut func), block(&mut func));
        let (dead_head, exit) = (block(&mut func), block(&mut func));
        let (dead, live) = (block(&mut func), block(&mut func));

        let k = const_any(&mut func, entry, key);
        let c = push(&mut func, entry, Op::EqBits, &[x, k], Type::Bool);
        branch(&mut func, entry, c, done, &[], outer, &[]);
        branch(&mut func, outer, f1, inner, &[], far, &[]);

        let k = const_any(&mut func, inner, key);
        let c = push(&mut func, inner, Op::EqBits, &[x, k], Type::Bool);
        branch(&mut func, inner, c, yes, &[], no, &[]);

        branch(&mut func, far, f2, dead_head, &[], exit, &[]);
        let k = const_any(&mut func, dead_head, key);
        let c = push(&mut func, dead_head, Op::EqBits, &[x, k], Type::Bool);
        branch(&mut func, dead_head, c, dead, &[], live, &[]);

        for leaf in [done, yes, no, exit, dead, live] {
            ret(&mut func, leaf, x);
        }
        (func, inner, yes, no)
    }

    #[test]
    fn a_dead_arm_does_not_leak_its_fact_into_later_blocks() {
        for inner_first in [true, false] {
            let (mut func, inner, yes, no) = leaking_fact(inner_first);
            verify(&func).expect("the test function is valid");
            assert!(run(&mut func).changed);
            assert_ne!(
                jump_target(&func, inner),
                Some(yes),
                "`x == K` is false on every path to `inner` (inner_first = {inner_first})"
            );
            assert_eq!(jump_target(&func, inner), Some(no));
            verify(&func).expect("the folded function is valid");
        }
    }

    #[test]
    fn a_branch_on_a_condition_known_by_a_dominator_folds() {
        // entry: branch c -> a else b; a: branch c -> y else n  (c is true in a)
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x = func.append_param(entry, Type::Any);
        let (a, b, yes, no) = (
            block(&mut func),
            block(&mut func),
            block(&mut func),
            block(&mut func),
        );
        let k = const_any(&mut func, entry, SchemeValue::from_i32(6).bits());
        let c = push(&mut func, entry, Op::EqBits, &[x, k], Type::Bool);
        branch(&mut func, entry, c, a, &[], b, &[]);
        branch(&mut func, a, c, yes, &[], no, &[]);
        for leaf in [b, yes, no] {
            ret(&mut func, leaf, x);
        }
        verify(&func).expect("the test function is valid");
        assert!(run(&mut func).changed);
        assert_eq!(jump_target(&func, a), Some(yes));
        verify(&func).expect("the folded function is valid");
    }
}
