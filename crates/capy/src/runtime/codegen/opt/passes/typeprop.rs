//! Type propagation and redundant guard elimination.
//!
//! The pass walks the dominator tree and keeps a scope of facts. A guard is
//! redundant when a dominating instruction already proved the same thing:
//!
//! - a dominating guard with the same operands (its result is reused);
//! - the operand was boxed from a raw value of the same kind (the raw value
//!   is the result);
//! - the operand's type already satisfies the guard's class or code (the
//!   operand is the result, which has the same type);
//! - a dominating branch on `TestInt32` proved the operand is a fixnum. Then
//!   `CheckInt32` becomes a pure `Unbox`, with no frame.
//!
use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::forward::{IndexKey, index_key, object};
use super::{PassOutcome, Subst, replace_uses};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Edge, Inst, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};

/// A fact a dominating branch's taken arm proves about one operand.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Fact {
    /// The operand is a fixnum.
    Fixnum,
    /// The operand is an instance of exactly this class.
    Class(ClassRef),
    /// The operand is a closure that runs this code block.
    Code(CodeRef),
}

/// A fact and the operand it is about.
#[derive(Clone, Copy)]
struct Taken {
    value: Value,
    fact: Fact,
}

/// A guard and its operands, with the second operand present for the index
/// checks.
type Key = (Op, Value, Option<IndexKey>);

/// A step of the dominator-tree walk.
#[derive(Clone, Copy)]
enum Visit {
    Enter(Block),
    Exit(Block),
}

#[derive(Default)]
struct Scope {
    subst: Subst,
    /// The result of the guard that proved each key on the current path.
    avail: HashMap<Key, Value>,
    /// Values known to be fixnums on the current path.
    int32_known: HashSet<Value>,
    /// Values known to be instances of exactly one class on the current path.
    class_known: HashMap<Value, ClassRef>,
    /// Values known to be closures running one code block on the current path.
    code_known: HashMap<Value, CodeRef>,
    /// Guards removed from their blocks.
    dead: HashSet<Inst>,
    /// Guards turned into `Unbox`.
    unboxed: usize,
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let dom = func.dom_tree();
    let rpo = dom.rpo().to_vec();
    let work = rpo.len();

    // The reachable edges into each block. A block with one edge inherits
    // the facts of that edge.
    let mut pred_edges: SecondaryMap<Block, u32> = SecondaryMap::new();
    for &block in &rpo {
        if let Some(term) = &func.blocks[block].term {
            for edge in term.edges() {
                pred_edges[edge.block] += 1;
            }
        }
    }
    let mut children: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    for &block in &rpo {
        if let Some(parent) = dom.idom(block) {
            children[parent].push(block);
        }
    }
    // The fact a branch's taken arm proves. The arm must be entered only
    // through this branch, so every path to it carries the fact.
    let mut taken: SecondaryMap<Block, Option<Taken>> = SecondaryMap::new();
    for &block in &rpo {
        let Some(Terminator::Branch { cond, then, .. }) = &func.blocks[block].term else {
            continue;
        };
        let ValueDef::Inst(test) = func.value_def(*cond) else {
            continue;
        };
        let fact = match func.insts[test].op {
            Op::TestInt32 | Op::TestInt64 => Some(Fact::Fixnum),
            Op::TestPair => Some(Fact::Class(ClassRef::PAIR)),
            Op::TestClass(class) => Some(Fact::Class(class)),
            Op::TestCode(code) => Some(Fact::Code(code)),
            _ => None,
        };
        let Some(fact) = fact else {
            continue;
        };
        if then.block == block || then.block == func.entry {
            continue;
        }
        if pred_edges[then.block] == 1 {
            taken[then.block] = Some(Taken {
                value: func.args(test)[0],
                fact,
            });
        }
    }

    // A loop parameter's class is proved by every edge into its block
    // except the OSR entries, whose arguments are frame loads. Guarding each
    // OSR edge once makes the fact hold there too (a mismatch deopts to
    // baseline), so the loop body drops its per-iteration class checks.
    let osr_blocks: HashSet<Block> = func.osr.iter().map(|entry| entry.block).collect();
    let inferred = param_facts(func, &rpo, &osr_blocks);
    let guarded = guard_osr_edges(func, &inferred);
    let mut scope = Scope::default();
    let mut keys_added: SecondaryMap<Block, Vec<Key>> = SecondaryMap::new();
    let mut fact_added: SecondaryMap<Block, Vec<Taken>> = SecondaryMap::new();
    let mut stack = vec![Visit::Enter(func.entry)];
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                if let Some(Taken { value, fact }) = taken[block] {
                    // Canonicalize through class checks and single-incoming
                    // parameters, so an alias and the object it guards share
                    // the fact.
                    let value = object(func, &scope.subst, value);
                    let fresh = match fact {
                        Fact::Fixnum => scope.int32_known.insert(value),
                        Fact::Class(class) => scope.class_known.insert(value, class).is_none(),
                        Fact::Code(code) => scope.code_known.insert(value, code).is_none(),
                    };
                    if fresh {
                        fact_added[block].push(Taken { value, fact });
                    }
                }
                // Facts that hold for a block parameter on every incoming
                // path (the OSR edges were guarded above).
                for &param in func.params(block) {
                    let Some(fact) = inferred.get(&param).copied() else {
                        continue;
                    };
                    let fresh = match fact {
                        Fact::Fixnum => scope.int32_known.insert(param),
                        Fact::Class(class) => scope.class_known.insert(param, class).is_none(),
                        Fact::Code(code) => scope.code_known.insert(param, code).is_none(),
                    };
                    if fresh {
                        fact_added[block].push(Taken { value: param, fact });
                    }
                }
                let insts = func.blocks[block].insts.clone();
                for inst in insts {
                    process(func, &mut scope, inst, &mut keys_added[block]);
                }
                stack.push(Visit::Exit(block));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(block) => {
                for key in keys_added[block].drain(..) {
                    scope.avail.remove(&key);
                }
                for Taken { value, fact } in fact_added[block].drain(..) {
                    match fact {
                        Fact::Fixnum => {
                            scope.int32_known.remove(&value);
                        }
                        Fact::Class(_) => {
                            scope.class_known.remove(&value);
                        }
                        Fact::Code(_) => {
                            scope.code_known.remove(&value);
                        }
                    }
                }
            }
        }
    }

    let changed = guarded || !scope.subst.is_empty() || !scope.dead.is_empty() || scope.unboxed > 0;
    if !scope.dead.is_empty() {
        for (_, data) in func.blocks.iter_mut() {
            data.insts.retain(|inst| !scope.dead.contains(inst));
        }
    }
    replace_uses(func, &scope.subst);
    PassOutcome {
        changed,
        work: work + func.insts.len(),
    }
}

/// Handles one instruction of the current block, recording its key when it
/// is a guard that stays.
fn process(func: &mut Func, scope: &mut Scope, inst: Inst, added: &mut Vec<Key>) {
    let op = func.insts[inst].op;
    let Some(result) = func.insts[inst].result else {
        return;
    };
    let args: Vec<Value> = func
        .args(inst)
        .iter()
        .map(|&v| scope.subst.resolve(v))
        .collect();
    match op {
        Op::CheckInt32 | Op::CheckInt64 | Op::CheckDouble => {
            let source = args[0];
            // Facts are keyed by the canonical value (a class-check alias
            // names the same object); the replacement keeps `source`'s type.
            let canonical = object(func, &scope.subst, source);
            let raw_op = match op {
                Op::CheckInt32 => Op::Box(Repr::Int32),
                Op::CheckInt64 => Op::Box(Repr::Int64),
                _ => Op::Box(Repr::Double),
            };
            if let Some(raw) = boxed_from(func, scope, source, raw_op) {
                return remove(scope, inst, result, raw);
            }
            let key = (op, canonical, None);
            if let Some(&prev) = scope.avail.get(&key) {
                return remove(scope, inst, result, prev);
            }
            if matches!(op, Op::CheckInt32 | Op::CheckInt64)
                && scope.int32_known.contains(&canonical)
            {
                // A branch on a fixnum test already proved the tag, so only
                // the untag remains. Both checks test the same fixnum tag.
                let data = &mut func.insts[inst];
                data.op = match op {
                    Op::CheckInt32 => Op::Unbox(Repr::Int32),
                    _ => Op::Unbox(Repr::Int64),
                };
                data.frame = None;
                scope.unboxed += 1;
            }
            record(scope, key, result, added);
        }
        // `Box(Int64)` yields a fixnum, or a bignum when the value leaves the
        // fixnum range. Either way it fits 64 bits, and the check returns the
        // same number, so it cannot fail and its result is the raw value.
        Op::CheckBigIntFits64 => {
            if let Some(raw) = boxed_from(func, scope, args[0], Op::Box(Repr::Int64)) {
                remove(scope, inst, result, raw);
            }
        }
        Op::CheckClass(class) => {
            let source = args[0];
            let canonical = object(func, &scope.subst, source);
            // A dominating `pair?`/`vector?`/`string?` (or any class test)
            // proved the object's class on this path, so the class check is
            // redundant.
            if (scope.class_known.get(&canonical) == Some(&class)
                // A closure whose code block is known is a closure.
                || (class == ClassRef::CLOSURE
                    && scope.code_known.contains_key(&canonical)))
                && !passed_on_an_edge(func, result)
            {
                return remove(scope, inst, result, source);
            }
            let key = (op, canonical, None);
            if let Some(&prev) = scope.avail.get(&key) {
                return remove(scope, inst, result, prev);
            }
            if func
                .value_type(source)
                .is_subtype_of(func.value_type(result))
            {
                return remove(scope, inst, result, source);
            }
            record(scope, key, result, added);
        }
        Op::CheckCode(code) => {
            let source = args[0];
            let canonical = object(func, &scope.subst, source);
            // A dominating `procedure?` test on this code block, or a type
            // that already names it, makes the check redundant.
            if scope.code_known.get(&canonical) == Some(&code) && !passed_on_an_edge(func, result) {
                return remove(scope, inst, result, source);
            }
            if func
                .value_type(source)
                .is_subtype_of(func.value_type(result))
            {
                return remove(scope, inst, result, source);
            }
            let key = (op, canonical, None);
            if let Some(&prev) = scope.avail.get(&key) {
                return remove(scope, inst, result, prev);
            }
            record(scope, key, result, added);
        }
        Op::CheckIndex | Op::CheckStringIndex => {
            // The canonical object and index: a class-check alias names the
            // same object, and a boxed/checked index names the same number.
            let key = (
                op,
                object(func, &scope.subst, args[0]),
                Some(index_key(func, &scope.subst, args[1])),
            );
            if let Some(&prev) = scope.avail.get(&key) {
                return remove(scope, inst, result, prev);
            }
            record(scope, key, result, added);
        }

        Op::Box(Repr::Int32) | Op::Box(Repr::Int64) | Op::Box(Repr::Double) => {
            let source = args[0];
            let ValueDef::Inst(def) = func.value_def(source) else {
                return;
            };
            let (check, unbox) = match op {
                Op::Box(Repr::Int32) => (Op::CheckInt32, Op::Unbox(Repr::Int32)),
                Op::Box(Repr::Int64) => (Op::CheckInt64, Op::Unbox(Repr::Int64)),
                _ => (Op::CheckDouble, Op::Unbox(Repr::Double)),
            };
            let defined = func.insts[def].op;
            if defined == check || defined == unbox {
                let original = scope.subst.resolve(func.args(def)[0]);
                remove(scope, inst, result, original);
            }
        }
        _ => {}
    }
}

/// The exact fact a value's static type states, if any.
fn static_fact(func: &Func, value: Value) -> Option<Fact> {
    match func.value_type(value) {
        Type::Pair => Some(Fact::Class(ClassRef::PAIR)),
        Type::Vector => Some(Fact::Class(ClassRef::VECTOR)),
        Type::Cell => Some(Fact::Class(ClassRef::VARIABLE)),
        Type::Heap(class) => Some(Fact::Class(class)),
        Type::Rational => Some(Fact::Class(ClassRef::RATIONAL)),
        Type::Closure(Some(code)) => Some(Fact::Code(code)),
        _ => None,
    }
}

/// Facts that hold for a block parameter on every incoming path.
fn param_facts(func: &Func, rpo: &[Block], osr_blocks: &HashSet<Block>) -> HashMap<Value, Fact> {
    // Optimistic pass: a parameter argument with no fact yet is ignored, so
    // two loops whose parameters flow into each other can both inherit the
    // fact their shared preheader argument proves.
    let mut facts: HashMap<Value, Fact> = HashMap::new();
    loop {
        let mut changed = false;
        for &block in rpo {
            for (index, &param) in func.params(block).iter().enumerate() {
                if facts.contains_key(&param) {
                    continue;
                }
                let mut agreed: Option<Fact> = None;
                let mut consistent = true;
                for &source in rpo {
                    if osr_blocks.contains(&source) {
                        continue;
                    }
                    let Some(term) = &func.blocks[source].term else {
                        continue;
                    };
                    for edge in term.edges() {
                        if edge.block != block {
                            continue;
                        }
                        let Some(&arg) = func.edge_args(&edge).get(index) else {
                            consistent = false;
                            continue;
                        };
                        if arg == param {
                            continue;
                        }
                        let known = static_fact(func, arg).or_else(|| facts.get(&arg).copied());
                        let Some(fact) = known else {
                            // An undetermined parameter is pending; anything
                            // else is a concrete counterexample.
                            if matches!(func.value_def(arg), ValueDef::Param { .. }) {
                                continue;
                            }
                            consistent = false;
                            continue;
                        };
                        match agreed {
                            None => agreed = Some(fact),
                            Some(f) if f == fact => {}
                            Some(_) => consistent = false,
                        }
                    }
                }
                if let (true, Some(fact)) = (consistent, agreed) {
                    facts.insert(param, fact);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    // Strict pass: a fact is only real if every incoming argument states it
    // or carries the same verified fact (including through the other loop).
    loop {
        let snapshot = facts.clone();
        let mut removed = false;
        for (&param, &fact) in &snapshot {
            let ValueDef::Param { block, index } = func.value_def(param) else {
                continue;
            };
            let index = index as usize;
            let mut valid = true;
            'edges: for &source in rpo {
                if osr_blocks.contains(&source) {
                    continue;
                }
                let Some(term) = &func.blocks[source].term else {
                    continue;
                };
                for edge in term.edges() {
                    if edge.block != block {
                        continue;
                    }
                    let Some(&arg) = func.edge_args(&edge).get(index) else {
                        valid = false;
                        break 'edges;
                    };
                    if arg == param {
                        continue;
                    }
                    if static_fact(func, arg) == Some(fact) {
                        continue;
                    }
                    if snapshot.get(&arg) == Some(&fact) {
                        continue;
                    }
                    valid = false;
                    break 'edges;
                }
            }
            if !valid {
                facts.remove(&param);
                removed = true;
            }
        }
        if !removed {
            break;
        }
    }
    facts
}

fn guard_osr_edges(func: &mut Func, facts: &HashMap<Value, Fact>) -> bool {
    let mut changed = false;
    for entry in func.osr.clone() {
        let Some(Terminator::Jump(edge)) = func.blocks[entry.block].term.clone() else {
            continue;
        };
        if edge.block != entry.header {
            continue;
        }
        let frame = func.blocks[entry.block].insts.iter().find_map(|&inst| {
            (func.insts[inst].op == Op::CheckNotInvalid)
                .then(|| func.insts[inst].frame)
                .flatten()
        });
        let Some(frame) = frame else {
            continue;
        };
        let args = func.edge_args(&edge).to_vec();
        let params = func.params(entry.header).to_vec();
        let mut new_args = args.clone();
        let mut edge_changed = false;
        for (index, &param) in params.iter().enumerate() {
            let Some(&fact) = facts.get(&param) else {
                continue;
            };
            let Some(&arg) = args.get(index) else {
                continue;
            };
            if static_fact(func, arg) == Some(fact) {
                continue;
            }
            let (op, ty) = match fact {
                Fact::Class(class) => (Op::CheckClass(class), Type::Heap(class)),
                Fact::Code(code) => (Op::CheckCode(code), Type::Closure(Some(code))),
                Fact::Fixnum => continue,
            };
            if let Some(checked) = func.push_inst(entry.block, op, &[arg], Some(ty), Some(frame)) {
                new_args[index] = checked;
                edge_changed = true;
            }
        }
        if edge_changed {
            let edge = Edge {
                block: entry.header,
                args: EntityList::from_slice(&new_args, &mut func.pool),
            };
            func.blocks[entry.block].term = Some(Terminator::Jump(edge));
            changed = true;
        }
    }
    changed
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

/// The raw value a `box` of `box_op` produced `value` from, if `value` is one.
fn boxed_from(func: &Func, scope: &Scope, value: Value, box_op: Op) -> Option<Value> {
    let ValueDef::Inst(def) = func.value_def(value) else {
        return None;
    };
    if func.insts[def].op != box_op {
        return None;
    }
    Some(scope.subst.resolve(func.args(def)[0]))
}

fn remove(scope: &mut Scope, inst: Inst, result: Value, replacement: Value) {
    scope.subst.add(result, replacement);
    scope.dead.insert(inst);
}

fn record(scope: &mut Scope, key: Key, result: Value, added: &mut Vec<Key>) {
    scope.avail.insert(key, result);
    added.push(key);
}
