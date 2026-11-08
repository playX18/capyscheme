//! Store-to-load forwarding and write-barrier elision.
//!
//! ## Forwarding
//! A store and a later load of the same slot in the same
//! extended basic block give the load the stored value, which is then used
//! in place of the load. A slot is a field of an object, a free variable of a
//! closure, a cell, or an element of a vector.
//!
//! An operation that writes a region drops every table entry of that region,
//! whatever the object, since two SSA values may name one object. A call or a
//! safepoint drops all entries. A write barrier and an allocation write no
//! existing slot, so they drop nothing. The store itself stays. Only the load
//! goes away.
//!
//! Objects are compared after looking through [`Op::CheckClass`], which
//! returns the object it checks. Indices are compared by value, so a
//! `CheckIndex` on a constant and on an equal index value meet in one slot.
//!
//! ## Barrier elision
//! [`Op::WriteBarrier`] logs an object that now refers to a
//! value. An object allocated in the same block, with no call, safepoint or
//! other allocation between the allocation and the barrier, is still young
//! and cannot have been promoted, so its barrier is dropped. Nothing in the
//! block between the two can run the collector.
//!
//! This relies on the allocation being in the nursery state. Every
//! [`Op::AllocPair`], [`Op::AllocClosure`], [`Op::AllocEnv`] and
//! [`Op::AllocVector`] takes the default semantics.

use std::collections::{HashMap, HashSet};

use cranelift_entity::SecondaryMap;

use super::{PassOutcome, Subst, replace_uses, writes_existing};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{
    Constant, EffectFlags, Inst, MemoryRanges, MemoryRegion, Op, Repr,
};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::codegen::opt::lir::VRepr;
use crate::runtime::objectmodel::Value as SchemeValue;

/// An index into a vector, as a value both `CheckIndex` and the constant it
/// was built from agree on.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum IndexKey {
    /// A fixnum or int32 constant.
    Num(i32),
    /// Any other value, by identity.
    Value(Value),
}

/// The object `value` refers to, looking through class checks and block
/// parameters that carry one object.
pub(super) fn object(func: &Func, subst: &Subst, value: Value) -> Value {
    object_at(func, subst, value, 0)
}

fn object_at(func: &Func, subst: &Subst, value: Value, depth: usize) -> Value {
    let value = subst.resolve(value);
    if depth >= 8 {
        return value;
    }
    if let Some(inst) = def_inst(func, value) {
        return match func.insts[inst].op {
            Op::CheckClass(_) => object_at(func, subst, func.args(inst)[0], depth + 1),
            _ => value,
        };
    }
    let ValueDef::Param { block, index } = func.value_def(value) else {
        return value;
    };
    let index = index as usize;
    let mut unique: Option<Value> = None;
    for (_, data) in func.blocks.iter() {
        let Some(term) = &data.term else {
            continue;
        };
        for edge in term.edges() {
            if edge.block != block {
                continue;
            }
            let Some(&arg) = func.edge_args(&edge).get(index) else {
                return value;
            };
            let arg = subst.resolve(arg);
            // A self reference (a loop back edge) is the parameter's own
            // value; it adds no information.
            if arg == value {
                continue;
            }
            let resolved = object_at(func, subst, arg, depth + 1);
            match unique {
                None => unique = Some(resolved),
                Some(previous) if previous == resolved => {}
                Some(_) => return value,
            }
        }
    }
    unique.unwrap_or(value)
}

/// The index that `value` names. An `Int32` and the tagged fixnum it was
/// unboxed or checked from name the same index.
pub(super) fn index_key(func: &Func, subst: &Subst, value: Value) -> IndexKey {
    let value = subst.resolve(value);
    if let Some(inst) = def_inst(func, value) {
        let args = func.args(inst);
        match func.insts[inst].op {
            Op::Const(Constant::Int32(n)) => return IndexKey::Num(n),
            Op::Const(Constant::Int64(n)) if i32::try_from(n).is_ok() => {
                return IndexKey::Num(n as i32);
            }
            Op::Const(Constant::Bits(bits)) => {
                if let Some(n) = SchemeValue::from_raw(bits).int32() {
                    return IndexKey::Num(n);
                }
            }
            Op::CheckIndex => return index_key(func, subst, args[1]),
            Op::CheckInt32
            | Op::CheckInt64
            | Op::Unbox(Repr::Int32)
            | Op::Unbox(Repr::Int64)
            | Op::Box(Repr::Int32)
            | Op::Box(Repr::Int64) => {
                return index_key(func, subst, args[0]);
            }
            _ => {}
        }
    }
    IndexKey::Value(value)
}

/// The instruction that defines `value`, if an instruction does.
pub(super) fn def_inst(func: &Func, value: Value) -> Option<Inst> {
    match func.value_def(value) {
        ValueDef::Inst(inst) => Some(inst),
        ValueDef::Param { .. } => None,
    }
}

/// A memory slot that a load or a store names.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Slot {
    Field {
        obj: Value,
        field: u32,
        region: MemoryRegion,
    },
    Free {
        closure: Value,
        index: u32,
    },
    Cell {
        cell: Value,
    },
    Element {
        vector: Value,
        index: IndexKey,
    },
}

impl Slot {
    fn region(self) -> MemoryRegion {
        match self {
            Slot::Field { region, .. } => region,
            Slot::Free { .. } | Slot::Element { .. } => MemoryRegion::Heap,
            Slot::Cell { .. } => MemoryRegion::Variables,
        }
    }
}

/// The slot an instruction reads or writes, with the stored value for a
/// store (`None` for a load).
fn access(func: &Func, subst: &Subst, inst: Inst) -> Option<(Slot, Option<Value>)> {
    let data = &func.insts[inst];
    let args = func.args(inst);
    let obj = |v| object(func, subst, v);
    let slot = match data.op {
        Op::Load { field, region } => (
            Slot::Field {
                obj: obj(args[0]),
                field,
                region,
            },
            None,
        ),
        Op::Store { field, region } => (
            Slot::Field {
                obj: obj(args[0]),
                field,
                region,
            },
            Some(args[1]),
        ),
        Op::LoadFree { index } => (
            Slot::Free {
                closure: obj(args[0]),
                index,
            },
            None,
        ),
        Op::StoreFree { index } => (
            Slot::Free {
                closure: obj(args[0]),
                index,
            },
            Some(args[1]),
        ),
        Op::LoadCell => (
            Slot::Cell {
                cell: subst.resolve(args[0]),
            },
            None,
        ),
        Op::StoreCell => (
            Slot::Cell {
                cell: subst.resolve(args[0]),
            },
            Some(args[1]),
        ),
        Op::LoadElement => (
            Slot::Element {
                vector: obj(args[0]),
                index: index_key(func, subst, args[1]),
            },
            None,
        ),
        Op::StoreElement => (
            Slot::Element {
                vector: obj(args[0]),
                index: index_key(func, subst, args[1]),
            },
            Some(args[2]),
        ),
        _ => return None,
    };
    Some(slot)
}

fn is_alloc(op: Op) -> bool {
    matches!(
        op,
        Op::AllocPair
            | Op::AllocClosure(_)
            | Op::AllocEnv
            | Op::AllocVector
            | Op::AllocVectorFill
            | Op::AllocTuple
            | Op::AllocInstance { .. }
    )
}

/// The memory an instruction may write to an existing object. A write
/// barrier and an allocation write none.
fn written(op: Op) -> MemoryRanges {
    writes_existing(op)
}

/// Removes the write barrier of each store into an object that the same block
/// allocated with nothing in between that could run the collector.
fn elide_barriers(func: &Func, dead: &mut HashSet<Inst>) -> usize {
    let mut work = 0;
    for (_, data) in func.blocks.iter() {
        let mut fresh: Vec<Value> = Vec::new();
        for &inst in &data.insts {
            work += 1;
            let op = func.insts[inst].op;
            if is_alloc(op) {
                fresh.clear();
                if let Some(result) = func.insts[inst].result {
                    fresh.push(result);
                }
            } else if op.is_call() || op.effects().flags.contains(EffectFlags::ALLOCATES) {
                fresh.clear();
            } else if op == Op::WriteBarrier {
                let obj = object(func, &Subst::default(), func.args(inst)[0]);
                if fresh.contains(&obj) {
                    dead.insert(inst);
                }
            }
        }
    }
    work
}

/// A change to the slot table, undone when its block is left: the slot's
/// value before the change, or `None` if it had none.
#[derive(Clone, Copy)]
struct Undo {
    slot: Slot,
    before: Option<Value>,
}

/// A step of the walk over the chains of blocks.
#[derive(Clone, Copy)]
enum Visit {
    Enter(Block),
    Exit(Block),
}

fn forward_loads(func: &Func, subst: &mut Subst, dead: &mut HashSet<Inst>) -> usize {
    let reach = func.reverse_postorder();

    // A block whose only incoming edge comes from `source` continues the chain
    // of `source`, as in gvn. Every other reachable block starts a chain.
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
    let mut roots = Vec::new();
    for &block in &reach {
        match source[block] {
            Some(parent) if block != func.entry && incoming[block] == 1 && parent != block => {
                children[parent].push(block);
            }
            _ => roots.push(block),
        }
    }

    let mut avail: HashMap<Slot, Value> = HashMap::new();
    let mut undo: Vec<Undo> = Vec::new();
    let mut marks: SecondaryMap<Block, usize> = SecondaryMap::new();
    let mut work = 0;
    // Global search state (see `find_global`).
    let dom = func.dom_tree();
    let preds = func.predecessors();
    let mut global = Global {
        tails: SecondaryMap::new(),
        writes: SecondaryMap::new(),
        chain_end: SecondaryMap::new(),
        chain_root: SecondaryMap::new(),
    };
    for &block in &reach {
        let mut writes = MemoryRanges::NONE;
        for &inst in &func.blocks[block].insts {
            writes = writes.union(written(func.insts[inst].op));
        }
        global.writes[block] = writes;
    }
    let mut stack: Vec<Visit> = roots.iter().rev().map(|&b| Visit::Enter(b)).collect();
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                marks[block] = undo.len();
                // The chain continues its parent's, or starts here.
                let (root, mut so_far) = match source[block] {
                    Some(parent) if children[parent].contains(&block) => (
                        global.chain_root[parent].unwrap_or(parent),
                        global.chain_end[parent],
                    ),
                    _ => (block, MemoryRanges::NONE),
                };
                global.chain_root[block] = Some(root);
                for &inst in &func.blocks[block].insts {
                    work += 1;
                    let op = func.insts[inst].op;
                    let accessed = access(func, subst, inst);
                    let writes = written(op);
                    if !writes.is_empty() {
                        kill(&mut avail, &mut undo, writes);
                        so_far = so_far.union(writes);
                    }
                    let Some((slot, stored)) = accessed else {
                        continue;
                    };
                    match stored {
                        Some(value) => {
                            let value = subst.resolve(value);
                            if func.value_type(value) == Type::Bool {
                                continue;
                            }
                            insert(&mut avail, &mut undo, slot, value);
                        }
                        None => {
                            let Some(result) = func.insts[inst].result else {
                                continue;
                            };
                            let found = match avail.get(&slot) {
                                Some(&prev) => Some(prev),
                                None if globalizable(slot)
                                    && !so_far.overlaps(MemoryRanges::region(slot.region())) =>
                                {
                                    find_global(&global, &dom, &preds, root, slot, &mut work)
                                }
                                None => None,
                            };
                            match found {
                                Some(prev) if usable(func, prev, result) => {
                                    subst.add(result, prev);
                                    dead.insert(inst);
                                }
                                _ => insert(&mut avail, &mut undo, slot, result),
                            }
                        }
                    }
                }
                global.chain_end[block] = so_far;
                global.tails[block] = avail
                    .iter()
                    .filter(|(slot, _)| globalizable(**slot))
                    .map(|(slot, value)| (*slot, *value))
                    .collect();
                stack.push(Visit::Exit(block));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(block) => {
                while undo.len() > marks[block] {
                    let entry = undo.pop().expect("the log is longer than the mark");
                    match entry.before {
                        Some(value) => {
                            avail.insert(entry.slot, value);
                        }
                        None => {
                            avail.remove(&entry.slot);
                        }
                    }
                }
            }
        }
    }
    work + func.blocks.len()
}

/// What the search for a load in another block knows about each block.
struct Global {
    /// The globalizable slots available at the end of each visited block.
    tails: SecondaryMap<Block, Vec<(Slot, Value)>>,
    /// Everything a block may write.
    writes: SecondaryMap<Block, MemoryRanges>,
    /// What the chain of single-predecessor blocks down to and including each
    /// block has written.
    chain_end: SecondaryMap<Block, MemoryRanges>,
    /// The block each chain starts at.
    chain_root: SecondaryMap<Block, Option<Block>>,
}

/// The slots that the search across blocks handles: every kind. A slot is
/// named by an SSA value that is fixed once it exists (the cell, the closure,
/// the object, the vector and its index), so two loads of one slot differ only
/// if something wrote the slot's region in between. The search checks exactly
/// that, by region, so an object reached through another name is covered too.
fn globalizable(_slot: Slot) -> bool {
    true
}

/// A value of `slot` that is available at the start of `root` along every
/// path: a definition in a block that strictly dominates `root`, with no
/// write to the slot's region in any block between the two.
///
/// This is the backwards search of WebKit's `GlobalCSEPhase::findReplacement`
/// (`DFGCSEPhase.cpp`). It starts at the predecessors of `root` and walks
/// back. A block that strictly dominates `root` and has the slot available at
/// its end is the definition, and its predecessors are not walked. Any other
/// block is on a path from the definition to `root` (the definition
/// dominates `root`, so every block the walk sees before reaching it lies on
/// such a path), so a write to the slot's region in it ends the search.
fn find_global(
    global: &Global,
    dom: &crate::runtime::codegen::opt::ir::func::DomTree,
    preds: &SecondaryMap<Block, Vec<Block>>,
    root: Block,
    slot: Slot,
    work: &mut usize,
) -> Option<Value> {
    let region = MemoryRanges::region(slot.region());
    let mut seen: HashSet<Block> = HashSet::new();
    let mut worklist: Vec<Block> = Vec::new();
    for &pred in &preds[root] {
        if seen.insert(pred) {
            worklist.push(pred);
        }
    }
    let mut found: Option<Value> = None;
    while let Some(block) = worklist.pop() {
        *work += 1;
        if block != root && dom.dominates(block, root) {
            if let Some(&(_, value)) = global.tails[block].iter().find(|(s, _)| *s == slot) {
                match found {
                    None => found = Some(value),
                    // Two dominating definitions cannot both lie on the
                    // search; the nearer one was found first on this path.
                    Some(previous) if previous == value => {}
                    Some(_) => return None,
                }
                continue;
            }
        }
        if global.writes[block].overlaps(region) {
            return None;
        }
        for &pred in &preds[block] {
            if seen.insert(pred) {
                worklist.push(pred);
            }
        }
    }
    found
}

/// Whether the value `prev` can stand for a load's `result`.
fn usable(func: &Func, prev: Value, result: Value) -> bool {
    let (from, to) = (func.value_type(prev), func.value_type(result));

    from != Type::Bool && from.is_subtype_of(to) && VRepr::of_type(from) == VRepr::of_type(to)
}

/// Drops every table entry whose region `writes` overlaps.
fn kill(avail: &mut HashMap<Slot, Value>, undo: &mut Vec<Undo>, writes: MemoryRanges) {
    let victims: Vec<Slot> = avail
        .keys()
        .copied()
        .filter(|slot| writes.overlaps(MemoryRanges::region(slot.region())))
        .collect();
    for slot in victims {
        let before = avail.remove(&slot);
        undo.push(Undo { slot, before });
    }
}

fn insert(avail: &mut HashMap<Slot, Value>, undo: &mut Vec<Undo>, slot: Slot, value: Value) {
    let before = avail.insert(slot, value);
    undo.push(Undo { slot, before });
}

pub(crate) fn run(func: &mut Func) -> PassOutcome {
    let mut subst = Subst::default();
    let mut dead: HashSet<Inst> = HashSet::new();
    let mut work = elide_barriers(func, &mut dead);
    work += forward_loads(func, &mut subst, &mut dead);

    if dead.is_empty() {
        return PassOutcome {
            changed: false,
            work,
        };
    }
    for (_, data) in func.blocks.iter_mut() {
        data.insts.retain(|inst| !dead.contains(inst));
    }
    replace_uses(func, &subst);
    PassOutcome {
        changed: true,
        work,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, jump, placed, push, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    /// `x = load.cell c; if (flag) {A} else {B}; y = load.cell c`, where the
    /// else arm runs `arm` first. Returns the function and both loads.
    fn diamond(arm: Option<Op>) -> (Func<'static>, Inst, Inst) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let cell = func.append_param(entry, Type::Any);
        let flag = const_bool(&mut func, entry, true);
        let first = push(&mut func, entry, Op::LoadCell, &[cell], Type::Any);
        let left = block(&mut func);
        let right = block(&mut func);
        let join = block(&mut func);
        branch(&mut func, entry, flag, left, &[], right, &[]);
        jump(&mut func, left, join, &[]);
        if let Some(op) = arm {
            let value = const_any(&mut func, right, SchemeValue::from_i32(7).bits());
            func.push_inst(right, op, &[cell, value], None, None);
        }
        jump(&mut func, right, join, &[]);
        let second = push(&mut func, join, Op::LoadCell, &[cell], Type::Any);
        ret(&mut func, join, second);
        let def = |value| match func.value_def(value) {
            ValueDef::Inst(inst) => inst,
            ValueDef::Param { .. } => unreachable!("a load"),
        };
        let (a, b) = (def(first), def(second));
        (func, a, b)
    }

    #[test]
    fn load_after_a_diamond_reuses_the_dominating_load() {
        let (mut func, first, second) = diamond(None);
        verify(&func).expect("the test function is valid");
        assert!(run(&mut func).changed);
        assert!(placed(&func, first));
        assert!(!placed(&func, second), "the join's load is redundant");
        verify(&func).expect("the forwarded function is valid");
    }

    /// `x = load.element v i; if (flag) {A} else {B}; y = load.element v i`,
    /// where the else arm runs `arm` (an element or a cell store) first.
    fn element_diamond(arm: Option<Op>) -> (Func<'static>, Inst, Inst) {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let vector = func.append_param(entry, Type::Any);
        let index = func.append_param(entry, Type::Int32);
        let flag = const_bool(&mut func, entry, true);
        let first = push(
            &mut func,
            entry,
            Op::LoadElement,
            &[vector, index],
            Type::Any,
        );
        let left = block(&mut func);
        let right = block(&mut func);
        let join = block(&mut func);
        branch(&mut func, entry, flag, left, &[], right, &[]);
        jump(&mut func, left, join, &[]);
        if let Some(op) = arm {
            let value = const_any(&mut func, right, SchemeValue::from_i32(7).bits());
            match op {
                Op::StoreElement => func.push_inst(right, op, &[vector, index, value], None, None),
                _ => func.push_inst(right, op, &[vector, value], None, None),
            };
        }
        jump(&mut func, right, join, &[]);
        let second = push(
            &mut func,
            join,
            Op::LoadElement,
            &[vector, index],
            Type::Any,
        );
        ret(&mut func, join, second);
        let def = |value| match func.value_def(value) {
            ValueDef::Inst(inst) => inst,
            ValueDef::Param { .. } => unreachable!("a load"),
        };
        let (a, b) = (def(first), def(second));
        (func, a, b)
    }

    #[test]
    fn element_load_after_a_diamond_reuses_the_dominating_load() {
        let (mut func, first, second) = element_diamond(None);
        verify(&func).expect("the test function is valid");
        assert!(run(&mut func).changed);
        assert!(placed(&func, first));
        assert!(!placed(&func, second), "the join's load is redundant");
        verify(&func).expect("the forwarded function is valid");
    }

    #[test]
    fn an_element_store_on_one_arm_keeps_the_later_element_load() {
        let (mut func, _, second) = element_diamond(Some(Op::StoreElement));
        run(&mut func);
        assert!(placed(&func, second), "an arm wrote the heap");
        verify(&func).expect("the function is valid");
    }

    #[test]
    fn a_store_on_one_arm_keeps_the_later_load() {
        let (mut func, _, second) = diamond(Some(Op::StoreCell));
        run(&mut func);
        assert!(placed(&func, second), "an arm wrote the cell");
        verify(&func).expect("the function is valid");
    }
}
