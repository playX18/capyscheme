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

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::forward::{Incoming, IndexKey, index_key, object_with};
use super::{PassOutcome, Subst, replace_uses};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Edge, Inst, Op, Repr, Terminator};
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

/// How to take back a fact a block learned when the walk leaves the block's
/// dominator subtree: the value's previous entry, which a dominating block may
/// have set to a different class or code (an infeasible arm states the
/// contrary of a dominating fact).
#[derive(Clone, Copy)]
enum Undo {
    Fixnum(Value),
    Class(Value, Option<ClassRef>),
    Code(Value, Option<CodeRef>),
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
    /// Guards turned into `Unbox` or `Int64ToInt32`, and tests turned into
    /// constants.
    unboxed: usize,
    /// The arguments of the edges into each block, built once the OSR edges
    /// are guarded (no edge changes during the walk).
    incoming: Option<Incoming>,
    /// The values passed as a block-parameter argument on some edge.
    passed: SecondaryMap<Value, bool>,
}

impl Scope {
    /// The object `value` refers to, see [`super::forward::object`].
    fn object(&self, func: &Func, value: Value) -> Value {
        let incoming = self.incoming.as_ref().expect("built before the walk");
        object_with(func, &self.subst, incoming, value)
    }

    /// Learns `fact` about `value`, returning how to take it back, or `None`
    /// when the scope already held it.
    fn learn(&mut self, value: Value, fact: Fact) -> Option<Undo> {
        match fact {
            Fact::Fixnum => self.int32_known.insert(value).then_some(Undo::Fixnum(value)),
            Fact::Class(class) => match self.class_known.insert(value, class) {
                Some(previous) if previous == class => None,
                previous => Some(Undo::Class(value, previous)),
            },
            Fact::Code(code) => match self.code_known.insert(value, code) {
                Some(previous) if previous == code => None,
                previous => Some(Undo::Code(value, previous)),
            },
        }
    }

    fn undo(&mut self, undo: Undo) {
        match undo {
            Undo::Fixnum(value) => {
                self.int32_known.remove(&value);
            }
            Undo::Class(value, None) => {
                self.class_known.remove(&value);
            }
            Undo::Class(value, Some(class)) => {
                self.class_known.insert(value, class);
            }
            Undo::Code(value, None) => {
                self.code_known.remove(&value);
            }
            Undo::Code(value, Some(code)) => {
                self.code_known.insert(value, code);
            }
        }
    }
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
    let mut scope = Scope {
        incoming: Some(Incoming::build(func)),
        passed: passed_on_edges(func),
        ..Scope::default()
    };
    let mut keys_added: SecondaryMap<Block, Vec<Key>> = SecondaryMap::new();
    let mut fact_added: SecondaryMap<Block, Vec<Undo>> = SecondaryMap::new();
    let mut stack = vec![Visit::Enter(func.entry)];
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                if let Some(Taken { value, fact }) = taken[block] {
                    // Canonicalize through class checks and single-incoming
                    // parameters, so an alias and the object it guards share
                    // the fact.
                    let value = scope.object(func, value);
                    if let Some(undo) = scope.learn(value, fact) {
                        fact_added[block].push(undo);
                    }
                }
                // Facts that hold for a block parameter on every incoming
                // path (the OSR edges were guarded above).
                for &param in func.params(block) {
                    let Some(fact) = inferred.get(&param).copied() else {
                        continue;
                    };
                    if let Some(undo) = scope.learn(param, fact) {
                        fact_added[block].push(undo);
                    }
                }
                let insts = func.blocks[block].insts.clone();
                for inst in insts {
                    process(
                        func,
                        &mut scope,
                        inst,
                        &mut keys_added[block],
                        &mut fact_added[block],
                    );
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
                // Latest first, so a value learned twice in the block gets
                // back the entry it had before the block.
                for undo in fact_added[block].drain(..).rev() {
                    scope.undo(undo);
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
fn process(
    func: &mut Func,
    scope: &mut Scope,
    inst: Inst,
    added: &mut Vec<Key>,
    learned: &mut Vec<Undo>,
) {
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
        Op::CheckInt32
        | Op::CheckInt64
        | Op::CheckDouble
        | Op::Unbox(Repr::Int32)
        | Op::Unbox(Repr::Int64)
        | Op::Unbox(Repr::Double) => {
            let source = args[0];
            let (repr, is_check) = match op {
                Op::CheckInt32 => (Repr::Int32, true),
                Op::CheckInt64 => (Repr::Int64, true),
                Op::CheckDouble => (Repr::Double, true),
                Op::Unbox(repr) => (repr, false),
                _ => return,
            };

            if !is_check && func.value_type(source) != Type::Any {
                return;
            }

            let canonical = scope.object(func, source);
            // `Box(Int64)` is a bignum outside the fixnum range, which
            // `CheckInt64` must reject, so it cannot be forwarded as is.
            if repr != Repr::Int64 {
                if let Some(raw) = boxed_from(func, scope, source, Op::Box(repr)) {
                    return remove(scope, inst, result, raw);
                }
            }
            // A check and an unbox of one representation share a key.
            let key = (Op::Unbox(repr), canonical, None);
            if let Some(&prev) = scope.avail.get(&key) {
                return remove(scope, inst, result, prev);
            }
            let fixnum = matches!(repr, Repr::Int32 | Repr::Int64);
            if repr == Repr::Int32 {
                let wide = (Op::Unbox(Repr::Int64), canonical, None);

                if let Some(&wide) = scope.avail.get(&wide) {
                    let data = &mut func.insts[inst];
                    data.op = Op::Int64ToInt32;
                    data.args = EntityList::from_slice(&[wide], &mut func.pool);
                    data.frame = None;
                    scope.unboxed += 1;
                    return record(scope, key, result, added);
                }
            }

            if is_check && fixnum && scope.int32_known.contains(&canonical) {
                let data = &mut func.insts[inst];
                data.op = Op::Unbox(repr);
                data.frame = None;
                scope.unboxed += 1;
            }

            if fixnum {
                learned.extend(scope.learn(canonical, Fact::Fixnum));
            }
            record(scope, key, result, added);
        }
        Op::TestInt32 | Op::TestInt64 => {
            let canonical = scope.object(func, args[0]);
            let proven = scope.int32_known.contains(&canonical)
                || [Repr::Int32, Repr::Int64].iter().any(|&repr| {
                    scope
                        .avail
                        .contains_key(&(Op::Unbox(repr), canonical, None))
                });
            if proven {
                fold_true(func, scope, inst);
            }
        }
        Op::TestPair | Op::TestClass(_) => {
            let class = match op {
                Op::TestClass(class) => class,
                _ => ClassRef::PAIR,
            };
            let canonical = scope.object(func, args[0]);
            let proven = scope.class_known.get(&canonical) == Some(&class)
                || scope
                    .avail
                    .contains_key(&(Op::CheckClass(class), canonical, None))
                || static_fact(func, args[0]) == Some(Fact::Class(class));
            if proven {
                fold_true(func, scope, inst);
            }
        }

        Op::CheckBigIntFits64 => {
            if let Some(raw) = boxed_from(func, scope, args[0], Op::Box(Repr::Int64)) {
                remove(scope, inst, result, raw);
            }
        }
        Op::CheckU64 => {
            if let Some(raw) = boxed_from(func, scope, args[0], Op::BoxU64) {
                remove(scope, inst, result, raw);
            }
        }
        Op::CheckClass(class) => {
            let source = args[0];
            let canonical = scope.object(func, source);
            // A dominating `pair?`/`vector?`/`string?` (or any class test)
            // proved the object's class on this path, so the class check is
            // redundant.
            if (scope.class_known.get(&canonical) == Some(&class)
                // A closure whose code block is known is a closure.
                || (class == ClassRef::CLOSURE
                    && scope.code_known.contains_key(&canonical)))
                && !scope.passed[result]
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
            let canonical = scope.object(func, source);
            // A dominating `procedure?` test on this code block, or a type
            // that already names it, makes the check redundant.
            if scope.code_known.get(&canonical) == Some(&code) && !scope.passed[result] {
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
                scope.object(func, args[0]),
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
    // The arguments of the edges into each block from the non-OSR blocks, in
    // reverse postorder and edge order.
    let mut incoming: SecondaryMap<Block, Vec<&[Value]>> = SecondaryMap::new();
    for &source in rpo {
        if osr_blocks.contains(&source) {
            continue;
        }
        let Some(term) = &func.blocks[source].term else {
            continue;
        };
        for edge in term.edges() {
            incoming[edge.block].push(func.edge_args(&edge));
        }
    }
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
                for args in &incoming[block] {
                    let Some(&arg) = args.get(index) else {
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
            let valid = incoming[block].iter().all(|args| {
                let Some(&arg) = args.get(index) else {
                    return false;
                };
                arg == param
                    || static_fact(func, arg) == Some(fact)
                    || snapshot.get(&arg) == Some(&fact)
            });
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

/// The values passed as a block-parameter argument anywhere.
fn passed_on_edges(func: &Func) -> SecondaryMap<Value, bool> {
    let mut passed = SecondaryMap::new();
    for (_, data) in func.blocks.iter() {
        let Some(term) = &data.term else {
            continue;
        };
        for edge in term.edges() {
            for &arg in func.edge_args(&edge) {
                passed[arg] = true;
            }
        }
    }
    passed
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

fn fold_true(func: &mut Func, scope: &mut Scope, inst: Inst) {
    let data = &mut func.insts[inst];
    data.op = Op::Const(Constant::Bits(1));
    data.args = EntityList::new();
    data.frame = None;
    scope.unboxed += 1;
}

fn remove(scope: &mut Scope, inst: Inst, result: Value, replacement: Value) {
    scope.subst.add(result, replacement);
    scope.dead.insert(inst);
}

fn record(scope: &mut Scope, key: Key, result: Value, added: &mut Vec<Key>) {
    scope.avail.insert(key, result);
    added.push(key);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{block, branch, frame, placed, push, ret};

    /// A class fact that a nested arm overwrites is restored when the walk
    /// leaves that arm, so a sibling visited later sees the dominating fact.
    ///
    /// ```text
    /// entry: t0 = test.class VECTOR x; branch t0 -> a else z
    /// a:     branch c -> p else q          ; x is a vector here
    /// q:     t1 = test.pair x; branch t1 -> r else s
    /// r:     return x                      ; infeasible: states x is a pair
    /// p:     check.class PAIR x            ; must stay
    ///        check.class VECTOR x          ; redundant
    /// ```
    ///
    /// The walk visits `q` (and `r`) before `p`: `r`'s fact must not leak.
    #[test]
    fn an_overwritten_class_fact_is_restored_on_exit() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x = func.append_param(entry, Type::Any);
        let c = func.append_param(entry, Type::Bool);
        let a = block(&mut func);
        let z = block(&mut func);
        let p = block(&mut func);
        let q = block(&mut func);
        let r = block(&mut func);
        let s = block(&mut func);

        let t0 = push(&mut func, entry, Op::TestClass(ClassRef::VECTOR), &[x], Type::Bool);
        branch(&mut func, entry, t0, a, &[], z, &[]);
        ret(&mut func, z, x);
        branch(&mut func, a, c, p, &[], q, &[]);
        let t1 = push(&mut func, q, Op::TestPair, &[x], Type::Bool);
        branch(&mut func, q, t1, r, &[], s, &[]);
        ret(&mut func, r, x);
        ret(&mut func, s, x);
        let fs = frame(&mut func, x);
        let as_pair = func
            .push_inst(p, Op::CheckClass(ClassRef::PAIR), &[x], Some(Type::Pair), Some(fs))
            .unwrap();
        let fs = frame(&mut func, x);
        let as_vector = func
            .push_inst(p, Op::CheckClass(ClassRef::VECTOR), &[as_pair], Some(Type::Vector), Some(fs))
            .unwrap();
        ret(&mut func, p, as_vector);
        verify(&func).unwrap_or_else(|errors| panic!("{errors:?}"));

        let ValueDef::Inst(pair_check) = func.value_def(as_pair) else {
            unreachable!()
        };
        let ValueDef::Inst(vector_check) = func.value_def(as_vector) else {
            unreachable!()
        };
        run(&mut func);
        verify(&func).unwrap_or_else(|errors| panic!("{errors:?}"));
        assert!(placed(&func, pair_check), "x is a vector in p, so its pair check stays");
        assert!(!placed(&func, vector_check), "the vector check is redundant in p");
    }
}
