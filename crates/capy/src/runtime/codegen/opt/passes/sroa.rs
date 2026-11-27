//! Scalar replacement of aggregates (SROA) and escape analysis.

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::simplify::edges_mut;

use super::{PassOutcome, Subst, replace_uses};
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, DomTree, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{
    Constant, Inst, MemoryRegion, Op, RematKind, Terminator,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;
use crate::runtime::prims::def::Primitive;

fn eq_foldable(func: &Func, args: &[Value]) -> bool {
    if args.len() != 2 {
        return false;
    }
    let rooted = args
        .iter()
        .filter(|&&arg| object_of(func, arg).is_some())
        .count();
    let immediates = args
        .iter()
        .filter(|&&arg| func.const_bits_of(arg).is_some())
        .count();
    rooted == 2 || (rooted == 1 && immediates == 1)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Shape {
    Pair,
    Closure(CodeRef),
    Env { slots: u32 },
    Vector { len: u32 },
    Tuple { len: u32 },
    Instance { class: u32, slots: u32 },
}

impl Shape {
    fn class_id(self) -> u32 {
        match self {
            Shape::Pair => ClassRef::PAIR.class_id(),
            Shape::Closure(_) => ClassRef::CLOSURE.class_id(),
            Shape::Env { .. } => ClassRef::ENV_RECORD.class_id(),
            Shape::Vector { .. } => ClassRef::VECTOR.class_id(),
            Shape::Tuple { .. } => ClassRef::TUPLE.class_id(),
            Shape::Instance { class, .. } => class,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FieldPos {
    /// A data field, indexed from zero.
    Data(usize),
    /// The vector/tuple length word.
    Length,
}

/// The escape classification of a candidate.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Escape {
    NoEscape,
    ArgEscape,
    GlobalEscape,
}

/// One fresh allocation and what the analysis learned about it.
struct Candidate {
    alloc: Inst,
    block: Block,
    value: Value,
    shape: Shape,
    kind: RematKind,
    fields: Vec<Value>,
    frame_referenced: bool,
    mutated: bool,
    unsupported: bool,
    parents: Vec<Inst>,
    escape: Escape,
}

struct Flow {
    in_state: HashMap<Block, Vec<Option<Value>>>,
}

/// A planned rewrite of one object's uses.
#[derive(Default)]
struct Rewrite {
    forwards: Vec<(Value, Value)>,
    dead: Vec<Inst>,
    consts: Vec<(Inst, Constant)>,
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut work = 0;
    let dom = func.dom_tree();
    let rpo: Vec<Block> = dom.rpo().to_vec();
    if rpo.is_empty() {
        return PassOutcome::default();
    }
    let users = collect_users(func);
    let referenced_by_frame = frame_referenced_values(func);
    let reserved = reserved_blocks(func);

    // 1. Discover candidates.
    let mut candidates: Vec<Candidate> = Vec::new();
    for &block in &rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            let Some((shape, kind)) = shape_and_kind(func, inst) else {
                continue;
            };
            let Some(value) = func.insts[inst].result else {
                continue;
            };

            let all = func.args(inst).to_vec();
            let fields = match shape {
                Shape::Instance { .. } => all[3..].to_vec(),
                _ => all,
            };
            candidates.push(Candidate {
                alloc: inst,
                block,
                value,
                shape,
                kind,
                fields,
                frame_referenced: false,
                mutated: false,
                unsupported: false,
                parents: Vec::new(),
                escape: Escape::NoEscape,
            });
        }
    }
    if candidates.is_empty() {
        return PassOutcome::default();
    }

    for candidate in &mut candidates {
        if candidate
            .fields
            .iter()
            .any(|&field| matches!(func.value_type(field), Type::Bool | Type::Int128))
        {
            candidate.unsupported = true;
            candidate.escape = Escape::GlobalEscape;
        }
    }

    // 2. Escape analysis: classify every use of every candidate.
    for candidate in &mut candidates {
        analyze_uses(func, &dom, &rpo, &users, &referenced_by_frame, candidate);
        work += func.insts.len();
    }

    for candidate in &mut candidates {
        if !candidate.parents.is_empty() {
            candidate.unsupported = true;
            if candidate.escape == Escape::NoEscape {
                candidate.escape = Escape::GlobalEscape;
            }
        }
    }

    // 3. Decide what each candidate becomes.
    let mut rewrite: Vec<usize> = Vec::new();
    let mut remat = vec![false; candidates.len()];
    let mut state_at: Vec<Option<HashMap<Inst, Vec<Option<Value>>>>> = vec![None; candidates.len()];
    let mut failed: Vec<usize> = Vec::new();
    for i in 0..candidates.len() {
        if candidates[i].unsupported {
            continue;
        }
        let frame_referenced = candidates[i].frame_referenced;
        let mutated = candidates[i].mutated;
        if frame_referenced {
            if !mutated {
                remat[i] = true;
                rewrite.push(i);
            }
            continue;
        }
        if !mutated {
            rewrite.push(i);
            continue;
        }

        match solve_flow(func, &dom, &reserved, &candidates[i]) {
            Some(flow) => match state_map(func, &dom, &rpo, &flow, &candidates[i]) {
                Some(map) => {
                    state_at[i] = Some(map);
                    rewrite.push(i);
                }
                None => failed.push(i),
            },
            // The flow could not be expressed: leave object on the heap
            None => failed.push(i),
        }
    }
    for i in failed {
        candidates[i].escape = Escape::GlobalEscape;
    }

    if rewrite.is_empty() {
        return PassOutcome::default();
    }

    let mut subst = Subst::default();
    let mut dead: HashSet<Inst> = HashSet::new();
    let mut consts: Vec<(Inst, Constant)> = Vec::new();
    let mut changed = false;
    let mut applied: Vec<usize> = Vec::new();
    for &i in &rewrite {
        let candidate = &candidates[i];
        let plan = match plan_rewrite(
            func,
            candidate,
            if remat[i] || !candidate.mutated {
                None
            } else {
                state_at[i].as_ref()
            },
            &rpo,
        ) {
            Some(plan) => plan,
            None => continue,
        };
        changed = true;
        applied.push(i);
        if remat[i] {
            func.insts[candidate.alloc].op = Op::Remat(candidate.kind);
            func.insts[candidate.alloc].frame = None;
        } else {
            dead.insert(candidate.alloc);
        }
        for &(result, value) in &plan.forwards {
            if !subst.contains(result) && subst.resolve(result) == result {
                subst.add(result, value);
            }
        }
        for &inst in &plan.dead {
            dead.insert(inst);
        }
        consts.extend(plan.consts);
    }

    // 4. Fold reads of an object that no longer exists: `eq?`, type tests and length ops.
    let mut sroa_values: HashMap<Value, usize> = HashMap::new();
    for &i in &applied {
        sroa_values.insert(candidates[i].value, i);
    }
    let folds_before = dead.len();
    fold_reads(func, &rpo, &mut subst, &mut dead, &sroa_values, &candidates);
    changed |= dead.len() != folds_before;

    let cleanup_before = dead.len();
    cleanup_checks(func, &rpo, &mut subst, &mut dead);
    changed |= dead.len() != cleanup_before;

    if !changed {
        return PassOutcome::default();
    }

    // 5. Apply the const rewrites and one substitution sweep.
    for (inst, constant) in consts {
        if dead.contains(&inst) {
            continue;
        }
        let data = &mut func.insts[inst];
        data.op = Op::Const(constant);
        data.args = EntityList::new();
        data.frame = None;
    }
    replace_uses(func, &subst);
    for (_, data) in func.blocks.iter_mut() {
        data.insts.retain(|inst| !dead.contains(inst));
    }

    let no_escape = candidates
        .iter()
        .filter(|c| c.escape == Escape::NoEscape)
        .count();
    let arg = candidates
        .iter()
        .filter(|c| c.escape == Escape::ArgEscape)
        .count();
    let global = candidates
        .iter()
        .filter(|c| c.escape == Escape::GlobalEscape)
        .count();
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        " sroa: {no_escape} no-escape, {arg} arg-escape, {global} global-escape, {} scalarized",
        applied.len()
    );

    PassOutcome {
        changed: true,
        work: work + func.insts.len(),
    }
}

fn shape_and_kind(func: &Func, inst: Inst) -> Option<(Shape, RematKind)> {
    let args = func.args(inst).len() as u32;
    match func.insts[inst].op {
        Op::AllocPair => Some((Shape::Pair, RematKind::Pair)),
        Op::AllocClosure(code) => Some((
            Shape::Closure(code),
            RematKind::Closure { code, free: args },
        )),
        Op::AllocEnv => Some((Shape::Env { slots: args }, RematKind::Env { slots: args })),
        Op::AllocVector => Some((Shape::Vector { len: args }, RematKind::Vector { len: args })),
        Op::AllocTuple => Some((Shape::Tuple { len: args }, RematKind::Tuple { len: args })),
        Op::AllocInstance { class, slots } if slots + 3 == args => Some((
            Shape::Instance { class, slots },
            RematKind::Instance { class, slots },
        )),
        _ => None,
    }
}

fn object_of(func: &Func, value: Value) -> Option<Inst> {
    let mut cur = value;
    for _ in 0..64 {
        let ValueDef::Inst(inst) = func.value_def(cur) else {
            return None;
        };
        match func.insts[inst].op {
            Op::CheckClass(_) | Op::CheckCode(_) => cur = func.args(inst)[0],
            Op::AllocPair
            | Op::AllocClosure(_)
            | Op::AllocEnv
            | Op::AllocVector
            | Op::AllocTuple
            | Op::AllocInstance { .. } => return Some(inst),
            _ => return None,
        }
    }
    None
}

fn frame_referenced_values(func: &Func) -> HashSet<Value> {
    let mut out = HashSet::new();
    for (_, frame) in func.frames.iter() {
        out.insert(frame.closure);
        out.extend(frame.registers.iter().map(|&(_, value)| value));
    }
    out
}

/// Every instruction operand, by value.
fn collect_users(func: &Func) -> HashMap<Value, Vec<Inst>> {
    let mut users: HashMap<Value, Vec<Inst>> = HashMap::new();
    for (_, data) in func.blocks.iter() {
        for &inst in &data.insts {
            for &arg in func.args(inst) {
                users.entry(arg).or_default().push(inst);
            }
        }
    }
    users
}

/// Blocks whose parameter list is part of an entry ABI: the function entry
/// and the OSR blocks and headers. A new parameter at one of them would not
/// be passed by the entry glue.
fn reserved_blocks(func: &Func) -> HashSet<Block> {
    let mut out = HashSet::from([func.entry]);
    for entry in &func.osr {
        out.insert(entry.block);
        out.insert(entry.header);
    }
    out
}

fn field_pos(shape: Shape, field: u32) -> Option<FieldPos> {
    match shape {
        Shape::Pair => (field < 2).then_some(FieldPos::Data(field as usize)),
        Shape::Env { slots } => {
            (field >= 1 && field - 1 < slots).then_some(FieldPos::Data(field as usize - 1))
        }
        Shape::Vector { .. } if field == 0 => Some(FieldPos::Length),
        Shape::Vector { .. } => None,
        Shape::Tuple { .. } if field == 0 => Some(FieldPos::Length),
        Shape::Tuple { len } => (field - 1 < len).then_some(FieldPos::Data(field as usize - 1)),
        Shape::Closure(_) | Shape::Instance { .. } => None,
    }
}

fn checked_index(func: &Func, value: Value) -> Option<i32> {
    if let Some(n) = const_fixnum(func, value) {
        return Some(n);
    }
    let ValueDef::Inst(inst) = func.value_def(value) else {
        return None;
    };
    if func.insts[inst].op != Op::CheckIndex {
        return None;
    }
    const_fixnum(func, func.args(inst)[1])
}

fn const_fixnum(func: &Func, value: Value) -> Option<i32> {
    let ValueDef::Inst(inst) = func.value_def(value) else {
        return None;
    };
    match func.insts[inst].op {
        Op::Const(Constant::Bits(bits)) => SchemeValue::from_raw(bits).int32(),
        Op::Const(Constant::Int32(n)) => Some(n),
        _ => None,
    }
}

fn check_index_users_ok(
    func: &Func,
    users: &HashMap<Value, Vec<Inst>>,
    result: Value,
    alloc: Inst,
    len: u32,
) -> bool {
    let Some(list) = users.get(&result) else {
        return true;
    };
    !list.is_empty()
        && list.iter().all(|&user| {
            let data = &func.insts[user];
            let args = func.args(user);
            matches!(data.op, Op::LoadElement | Op::StoreElement)
                && object_of(func, args[0]) == Some(alloc)
                && checked_index(func, args[1]).is_some_and(|n| n >= 0 && (n as u32) < len)
        })
}

fn frame_refs(func: &Func, frame: Option<FrameState>, alloc: Inst) -> bool {
    let mut cur = frame;
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        if object_of(func, data.closure) == Some(alloc)
            || data
                .registers
                .iter()
                .any(|&(_, value)| object_of(func, value) == Some(alloc))
        {
            return true;
        }
        cur = data.parent;
    }
    false
}

fn analyze_uses(
    func: &Func,
    dom: &DomTree,
    rpo: &[Block],
    users: &HashMap<Value, Vec<Inst>>,
    referenced_by_frame: &HashSet<Value>,
    candidate: &mut Candidate,
) {
    let alloc = candidate.alloc;
    let mut unsupported = |candidate: &mut Candidate, escape: Escape| {
        candidate.unsupported = true;
        if candidate.escape == Escape::NoEscape {
            candidate.escape = escape;
        }
    };

    for &block in rpo {
        let data = &func.blocks[block];
        for &inst in &data.insts {
            let inst_data = &func.insts[inst];
            let args = func.args(inst);
            for pos in 0..args.len() {
                if object_of(func, args[pos]) != Some(alloc) {
                    continue;
                }
                if !dom.dominates(candidate.block, block) {
                    unsupported(candidate, Escape::GlobalEscape);
                    continue;
                }
                match pos {
                    0 => match inst_data.op {
                        Op::Load {
                            field,
                            region: MemoryRegion::Heap,
                        } => match field_pos(candidate.shape, field) {
                            Some(FieldPos::Data(_)) => {}
                            Some(FieldPos::Length)
                                if inst_data
                                    .result
                                    .is_some_and(|r| func.value_type(r) == Type::Int32) => {}
                            _ => unsupported(candidate, Escape::GlobalEscape),
                        },
                        Op::Store {
                            field,
                            region: MemoryRegion::Heap,
                        } => {
                            candidate.mutated = true;
                            let inside = matches!(
                                field_pos(candidate.shape, field),
                                Some(FieldPos::Data(_))
                            );
                            if !inside || object_of(func, args[1]) == Some(alloc) {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::WriteBarrier => {}
                        Op::LoadFree { index } => match candidate.shape {
                            Shape::Closure(_) if (index as usize) < candidate.fields.len() => {}
                            _ => unsupported(candidate, Escape::GlobalEscape),
                        },
                        Op::StoreFree { index } => {
                            candidate.mutated = true;
                            let inside = match candidate.shape {
                                Shape::Closure(_) => (index as usize) < candidate.fields.len(),
                                _ => false,
                            };
                            if !inside || object_of(func, args[1]) == Some(alloc) {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::LoadElement => match candidate.shape {
                            Shape::Vector { len } => {
                                if checked_index(func, args[1])
                                    .is_none_or(|n| n < 0 || n as u32 >= len)
                                {
                                    unsupported(candidate, Escape::GlobalEscape);
                                }
                            }
                            _ => unsupported(candidate, Escape::GlobalEscape),
                        },
                        Op::StoreElement => {
                            candidate.mutated = true;
                            let inside = match candidate.shape {
                                Shape::Vector { len } => checked_index(func, args[1])
                                    .is_some_and(|n| n >= 0 && (n as u32) < len),
                                _ => false,
                            };
                            if !inside || object_of(func, args[2]) == Some(alloc) {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::VectorLength => {
                            if !matches!(candidate.shape, Shape::Vector { .. }) {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::CheckIndex => {
                            let ok = match candidate.shape {
                                Shape::Vector { len } => {
                                    checked_index(func, args[1])
                                        .is_some_and(|n| n >= 0 && (n as u32) < len)
                                        && inst_data.result.is_some_and(|result| {
                                            !referenced_by_frame.contains(&result)
                                                && check_index_users_ok(
                                                    func, users, result, alloc, len,
                                                )
                                        })
                                }
                                _ => false,
                            };
                            if !ok {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::CheckClass(class) => {
                            if candidate.shape.class_id() != class.class_id() {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::CheckCode(code) => match candidate.shape {
                            Shape::Closure(c) if c == code => {}
                            _ => unsupported(candidate, Escape::GlobalEscape),
                        },
                        Op::TestPair
                        | Op::TestClass(_)
                        | Op::TestHeap
                        | Op::TestCode(_)
                        | Op::EqBits => {}
                        Op::CallPrim { prim, .. } if prim.is(Primitive::IsEq) => {}
                        Op::CallPrim { .. } | Op::CallOp(_) => {
                            unsupported(candidate, Escape::ArgEscape)
                        }
                        Op::AllocPair
                        | Op::AllocClosure(_)
                        | Op::AllocEnv
                        | Op::AllocVector
                        | Op::AllocTuple
                        | Op::AllocInstance { .. } => candidate.parents.push(inst),
                        _ => unsupported(candidate, Escape::GlobalEscape),
                    },
                    _ => match inst_data.op {
                        Op::Store { .. }
                        | Op::StoreFree { .. }
                        | Op::StoreElement
                        | Op::WriteBarrier => {
                            if let Some(outer) = object_of(func, args[0]) {
                                candidate.parents.push(outer);
                            } else {
                                unsupported(candidate, Escape::GlobalEscape);
                            }
                        }
                        Op::AllocPair
                        | Op::AllocClosure(_)
                        | Op::AllocEnv
                        | Op::AllocVector
                        | Op::AllocTuple
                        | Op::AllocInstance { .. } => candidate.parents.push(inst),
                        Op::EqBits => {}
                        Op::CallPrim { prim, .. } if prim.is(Primitive::IsEq) => {}
                        _ => unsupported(candidate, Escape::ArgEscape),
                    },
                }
            }
            if let Some(frame) = inst_data.frame
                && frame_refs(func, Some(frame), alloc) {
                    candidate.frame_referenced = true;
                    if !dom.dominates(candidate.block, block) {
                        unsupported(candidate, Escape::GlobalEscape);
                    }
                }
        }

        let Some(term) = &data.term else {
            continue;
        };
        match term {
            Terminator::Return(value) => {
                if object_of(func, *value) == Some(alloc) {
                    unsupported(candidate, Escape::GlobalEscape);
                }
            }
            Terminator::TailCall { callee, args, .. } => {
                if object_of(func, *callee) == Some(alloc)
                    || args
                        .as_slice(&func.pool)
                        .iter()
                        .any(|&arg| object_of(func, arg) == Some(alloc))
                {
                    unsupported(candidate, Escape::ArgEscape);
                }
            }
            Terminator::Branch { cond, .. } => {
                if object_of(func, *cond) == Some(alloc) {
                    unsupported(candidate, Escape::GlobalEscape);
                }
            }
            Terminator::Switch { value, .. } => {
                if object_of(func, *value) == Some(alloc) {
                    unsupported(candidate, Escape::GlobalEscape);
                }
            }
            Terminator::ReturnMany { values, .. } => {
                if values
                    .iter()
                    .any(|&value| object_of(func, value) == Some(alloc))
                {
                    unsupported(candidate, Escape::GlobalEscape);
                }
            }
            Terminator::Jump(_) | Terminator::Deopt { .. } | Terminator::Unreachable => {}
        }

        for edge in term.edges() {
            if func
                .edge_args(&edge)
                .iter()
                .any(|&arg| object_of(func, arg) == Some(alloc))
            {
                unsupported(candidate, Escape::GlobalEscape);
            }
        }
        if let Terminator::TailCall { frame, .. } | Terminator::Deopt { frame } = term
            && frame_refs(func, Some(*frame), alloc) {
                candidate.frame_referenced = true;
                if !dom.dominates(candidate.block, block) {
                    unsupported(candidate, Escape::GlobalEscape);
                }
            }
    }
}

fn store_to(func: &Func, inst: Inst, candidate: &Candidate) -> Option<(usize, Value)> {
    let data = &func.insts[inst];
    let args = func.args(inst);
    match data.op {
        Op::Store {
            field,
            region: MemoryRegion::Heap,
        } if object_of(func, args[0]) == Some(candidate.alloc) => {
            match field_pos(candidate.shape, field)? {
                FieldPos::Data(index) => Some((index, args[1])),
                FieldPos::Length => None,
            }
        }
        Op::StoreFree { index }
            if matches!(candidate.shape, Shape::Closure(_))
                && object_of(func, args[0]) == Some(candidate.alloc) =>
        {
            Some((index as usize, args[1]))
        }
        Op::StoreElement
            if matches!(candidate.shape, Shape::Vector { .. })
                && object_of(func, args[0]) == Some(candidate.alloc) =>
        {
            let index = checked_index(func, args[1])? as usize;
            Some((index, args[2]))
        }
        _ => None,
    }
}

/// The field version dataflow over the region the allocation dominates.
fn solve_flow(
    func: &mut Func,
    dom: &DomTree,
    reserved: &HashSet<Block>,
    candidate: &Candidate,
) -> Option<Flow> {
    let n = candidate.fields.len();
    let initial: Vec<Option<Value>> = candidate.fields.iter().copied().map(Some).collect();
    let blocks: Vec<Block> = dom
        .rpo()
        .iter()
        .copied()
        .filter(|&block| dom.dominates(candidate.block, block))
        .collect();
    if blocks.is_empty() {
        return None;
    }
    let preds = func.predecessors();

    let has_join = blocks.iter().any(|&block| {
        block != candidate.block
            && preds[block]
                .iter()
                .filter(|&&p| dom.is_reachable(p))
                .count()
                > 1
    });

    for &block in &blocks {
        if block == candidate.block {
            continue;
        }
        let reachable: Vec<Block> = preds[block]
            .iter()
            .copied()
            .filter(|&p| dom.is_reachable(p))
            .collect();
        if reachable
            .iter()
            .any(|&p| !dom.dominates(candidate.block, p))
        {
            return None;
        }
    }

    if has_join {
        let reserved_join = blocks.iter().any(|&block| {
            block != candidate.block
                && reserved.contains(&block)
                && preds[block]
                    .iter()
                    .filter(|&&p| dom.is_reachable(p))
                    .count()
                    > 1
        });
        if reserved_join {
            return None;
        }

        let raw = candidate
            .fields
            .iter()
            .any(|&v| !func.value_type(v).is_tagged())
            || blocks.iter().any(|&block| {
                func.blocks[block].insts.iter().any(|&inst| {
                    store_to(func, inst, candidate)
                        .is_some_and(|(_, v)| !func.value_type(v).is_tagged())
                })
            });

        if raw {
            // TODO: build mutable box
            return None;
        }
    }

    let mut in_state: HashMap<Block, Vec<Option<Value>>> = HashMap::new();
    let mut out_state: HashMap<Block, Vec<Option<Value>>> = HashMap::new();
    let mut phi: HashMap<(Block, usize), Value> = HashMap::new();
    let mut phi_pos: HashMap<(Block, usize), usize> = HashMap::new();
    let bound = (blocks.len() + 1) * (n + 2) * 4 + 16;
    let mut converged = false;
    for _ in 0..bound {
        let mut changed = false;
        for &block in &blocks {
            let incoming = if block == candidate.block {
                vec![None; n]
            } else {
                let reachable: Vec<Block> = preds[block]
                    .iter()
                    .copied()
                    .filter(|&p| dom.is_reachable(p))
                    .collect();
                if reachable.is_empty() {
                    continue;
                }
                let mut known = Vec::with_capacity(reachable.len());
                let mut all_known = true;
                for &p in &reachable {
                    match out_state.get(&p) {
                        Some(state) => known.push(state.clone()),
                        None => {
                            all_known = false;
                            break;
                        }
                    }
                }
                if !all_known {
                    continue;
                }
                let mut merged = vec![None; n];
                for field in 0..n {
                    let mut values: Vec<Value> = Vec::with_capacity(known.len());
                    let mut ok = true;
                    for state in &known {
                        match state[field] {
                            Some(value) => values.push(value),
                            None => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    if !ok {
                        return None;
                    }

                    let first = values[0];

                    if values.iter().all(|&v| v == first) {
                        merged[field] = Some(first);
                        continue;
                    }

                    if let Some(&param) = phi.get(&(block, field)) {
                        merged[field] = Some(param);
                        continue;
                    }

                    if reserved.contains(&block) {
                        return None;
                    }

                    if values.iter().any(|&v| !func.value_type(v).is_tagged()) {
                        return None;
                    }

                    let param = func.append_param(block, Type::Any);
                    let pos = func.params(block).len() - 1;
                    phi.insert((block, field), param);
                    phi_pos.insert((block, field), pos);
                    extend_edges(func, block, &preds, pos);
                    merged[field] = Some(param);
                    changed = true;
                }
                merged
            };

            if in_state.get(&block) != Some(&incoming) {
                in_state.insert(block, incoming.clone());
                changed = true;
            }

            let mut state = incoming;
            let insts = func.blocks[block].insts.clone();
            for inst in insts {
                if inst == candidate.alloc {
                    state = initial.clone();
                } else if let Some((field, value)) = store_to(func, inst, candidate)
                    && field < n {
                        state[field] = Some(value);
                    }
            }
            if out_state.get(&block) != Some(&state) {
                out_state.insert(block, state);
                changed = true;
            }
        }
        if !changed {
            converged = true;
            break;
        }
    }
    if !converged {
        return None;
    }

    for (&(block, field), &_param) in &phi {
        let pos = phi_pos[&(block, field)];
        for &pred in &preds[block] {
            let value = out_state
                .get(&pred)
                .and_then(|state| state[field])
                .or_else(|| Some(placeholder(func, pred)));
            if let Some(value) = value {
                set_edge_arg(func, pred, block, pos, value);
            }
        }
    }
    Some(Flow { in_state })
}

fn extend_edges(
    func: &mut Func,
    block: Block,
    preds: &SecondaryMap<Block, Vec<Block>>,
    pos: usize,
) {
    for &pred in &preds[block] {
        let Some(mut term) = func.blocks[pred].term.take() else {
            continue;
        };
        let mut changed = false;
        {
            let edges = edges_mut(&mut term);
            for edge in edges {
                if edge.block != block {
                    continue;
                }
                let mut args: Vec<Value> = edge.args.as_slice(&func.pool).to_vec();
                while args.len() <= pos {
                    args.push(placeholder(func, pred));
                }
                edge.args = EntityList::from_slice(&args, &mut func.pool);
                changed = true;
            }
        }
        func.blocks[pred].term = Some(term);
        let _ = changed;
    }
}

fn placeholder(func: &mut Func, block: Block) -> Value {
    func.push_inst(
        block,
        Op::Const(Constant::Bits(0)),
        &[],
        Some(Type::Any),
        None,
    )
    .expect("a constant has a result")
}

fn set_edge_arg(func: &mut Func, pred: Block, block: Block, pos: usize, value: Value) {
    let Some(mut term) = func.blocks[pred].term.take() else {
        return;
    };
    {
        let edges = edges_mut(&mut term);
        for edge in edges {
            if edge.block != block {
                continue;
            }
            let mut args: Vec<Value> = edge.args.as_slice(&func.pool).to_vec();
            if args.len() <= pos {
                continue;
            }
            args[pos] = value;
            edge.args = EntityList::from_slice(&args, &mut func.pool);
        }
    }
    func.blocks[pred].term = Some(term);
}

fn state_map(
    func: &Func,
    dom: &DomTree,
    rpo: &[Block],
    flow: &Flow,
    candidate: &Candidate,
) -> Option<HashMap<Inst, Vec<Option<Value>>>> {
    let n = candidate.fields.len();
    let initial: Vec<Option<Value>> = candidate.fields.iter().copied().map(Some).collect();
    let mut out = HashMap::new();
    for &block in rpo {
        if !dom.dominates(candidate.block, block) {
            continue;
        }
        let mut state = if block == candidate.block {
            vec![None; n]
        } else {
            {
                let state = flow.in_state.get(&block)?;
                state.clone()
            }
        };
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            let uses = func
                .args(inst)
                .iter()
                .any(|&arg| object_of(func, arg) == Some(candidate.alloc));
            if uses {
                out.insert(inst, state.clone());
            }
            if inst == candidate.alloc {
                state = initial.clone();
            } else if let Some((field, value)) = store_to(func, inst, candidate)
                && field < n {
                    state[field] = Some(value);
                }
        }
    }
    Some(out)
}

fn plan_rewrite(
    func: &Func,
    candidate: &Candidate,
    state_at: Option<&HashMap<Inst, Vec<Option<Value>>>>,
    rpo: &[Block],
) -> Option<Rewrite> {
    let initial: Vec<Option<Value>> = candidate.fields.iter().copied().map(Some).collect();
    let mut plan = Rewrite::default();
    let fields = |inst: Inst| -> Option<Vec<Option<Value>>> {
        match state_at {
            None => Some(initial.clone()),
            Some(map) => map.get(&inst).cloned(),
        }
    };
    for &block in rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            if inst == candidate.alloc {
                continue;
            }
            let data = &func.insts[inst];
            let args = func.args(inst);
            if !args
                .iter()
                .any(|&arg| object_of(func, arg) == Some(candidate.alloc))
            {
                continue;
            }
            let state =
                |index: usize| -> Option<Value> { fields(inst)?.get(index).copied().flatten() };
            let mut ok = true;
            for pos in 0..args.len() {
                if object_of(func, args[pos]) != Some(candidate.alloc) {
                    continue;
                }
                match pos {
                    0 => match data.op {
                        Op::Load {
                            field,
                            region: MemoryRegion::Heap,
                        } => match field_pos(candidate.shape, field) {
                            Some(FieldPos::Data(index)) => match (state(index), data.result) {
                                (Some(value), Some(result)) => {
                                    plan.forwards.push((result, value));
                                    plan.dead.push(inst);
                                }
                                _ => ok = false,
                            },
                            Some(FieldPos::Length) => {
                                if let Shape::Vector { len } | Shape::Tuple { len } =
                                    candidate.shape
                                {
                                    let value = Constant::Int32(len as i32);
                                    plan.consts.push((inst, value));
                                } else {
                                    ok = false;
                                }
                            }
                            None => ok = false,
                        },
                        Op::LoadFree { index } => match (state(index as usize), data.result) {
                            (Some(value), Some(result)) => {
                                plan.forwards.push((result, value));
                                plan.dead.push(inst);
                            }
                            _ => ok = false,
                        },
                        Op::LoadElement => {
                            let Some(index) = checked_index(func, args[1]) else {
                                ok = false;
                                continue;
                            };
                            match (state(index as usize), data.result) {
                                (Some(value), Some(result)) => {
                                    plan.forwards.push((result, value));
                                    plan.dead.push(inst);
                                }
                                _ => ok = false,
                            }
                        }
                        Op::VectorLength => {
                            if let Shape::Vector { len } = candidate.shape {
                                plan.consts.push((inst, Constant::Int32(len as i32)));
                            } else {
                                ok = false;
                            }
                        }
                        Op::CheckIndex => plan.dead.push(inst),
                        Op::CheckClass(_) | Op::CheckCode(_) => {
                            if let Some(result) = data.result {
                                plan.forwards.push((result, candidate.value));
                            }
                            plan.dead.push(inst);
                        }
                        Op::Store { .. } | Op::WriteBarrier => plan.dead.push(inst),
                        Op::TestPair
                        | Op::TestClass(_)
                        | Op::TestHeap
                        | Op::TestCode(_)
                        | Op::EqBits => {}
                        Op::CallPrim { prim, .. }
                            if prim.is(Primitive::IsEq) && eq_foldable(func, args) => {}
                        Op::CallPrim { .. } | Op::CallOp(_) => ok = false,
                        _ => ok = false,
                    },
                    _ => match data.op {
                        Op::EqBits => {}
                        Op::CallPrim { prim, .. }
                            if prim.is(Primitive::IsEq) && eq_foldable(func, args) => {}
                        _ => ok = false,
                    },
                }
            }
            if !ok {
                return None;
            }
        }
    }
    Some(plan)
}

fn fold_reads(
    func: &mut Func,
    rpo: &[Block],
    subst: &mut Subst,
    dead: &mut HashSet<Inst>,
    sroa_values: &HashMap<Value, usize>,
    candidates: &[Candidate],
) {
    for &block in rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            if dead.contains(&inst) {
                continue;
            }
            let data = &func.insts[inst];
            let args = func.args(inst);
            let resolved: Vec<Value> = args.iter().map(|&v| subst.resolve(v)).collect();
            let known: Vec<usize> = resolved
                .iter()
                .filter_map(|v| sroa_values.get(v).copied())
                .collect();
            match data.op {
                Op::EqBits => {
                    if known.is_empty() {
                        continue;
                    }
                    let truth = match resolved.as_slice() {
                        [a, b] => a == b,
                        _ => false,
                    };
                    let result = match data.result {
                        Some(result) if !subst.contains(result) => result,
                        _ => continue,
                    };
                    let value = fold_bool(func, block, inst, truth);
                    subst.add(result, value);
                    dead.insert(inst);
                }
                Op::CallPrim { prim, .. } if prim.is(Primitive::IsEq) => {
                    let [a, b] = resolved.as_slice() else {
                        continue;
                    };
                    let truth = match (object_of(func, *a), object_of(func, *b)) {
                        (Some(ia), Some(ib)) => ia == ib,
                        (Some(_), None) if func.const_bits_of(*b).is_some() => false,
                        (None, Some(_)) if func.const_bits_of(*a).is_some() => false,
                        _ => continue,
                    };
                    let result = match data.result {
                        Some(result) if !subst.contains(result) => result,
                        _ => continue,
                    };
                    let bits = if truth {
                        SchemeValue::VALUE_TRUE as u64
                    } else {
                        SchemeValue::VALUE_FALSE as u64
                    };
                    let value = insert_before(
                        func,
                        block,
                        inst,
                        Op::Const(Constant::Bits(bits)),
                        &[],
                        Type::Any,
                    );
                    subst.add(result, value);
                    dead.insert(inst);
                }
                Op::TestPair | Op::TestClass(_) | Op::TestHeap | Op::TestCode(_) => {
                    let Some(&index) = known.first() else {
                        continue;
                    };
                    let truth = match (data.op, candidates[index].shape) {
                        (Op::TestPair, Shape::Pair) => true,
                        (Op::TestHeap, _) => true,
                        (Op::TestClass(class), shape) => shape.class_id() == class.class_id(),
                        (Op::TestCode(code), Shape::Closure(c)) => c == code,
                        _ => false,
                    };
                    let result = match data.result {
                        Some(result) if !subst.contains(result) => result,
                        _ => continue,
                    };
                    let value = fold_bool(func, block, inst, truth);
                    subst.add(result, value);
                    dead.insert(inst);
                }
                Op::VectorLength => {
                    let Some(&index) = known.first() else {
                        continue;
                    };
                    if let Shape::Vector { len } = candidates[index].shape {
                        let data = &mut func.insts[inst];
                        data.op = Op::Const(Constant::Int32(len as i32));
                        data.args = EntityList::new();
                        data.frame = None;
                    }
                }
                _ => {}
            }
        }
    }
}

fn cleanup_checks(func: &mut Func, rpo: &[Block], subst: &mut Subst, dead: &mut HashSet<Inst>) {
    let mut fresh: HashMap<Inst, Shape> = HashMap::new();
    let mut edge_values: HashSet<Value> = HashSet::new();
    for &block in rpo {
        for &inst in &func.blocks[block].insts {
            if let Some((shape, _)) = shape_and_kind(func, inst) {
                fresh.insert(inst, shape);
            }
        }
        if let Some(term) = &func.blocks[block].term {
            for edge in term.edges() {
                edge_values.extend(func.edge_args(&edge).iter().copied());
            }
        }
    }
    for &block in rpo {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            if dead.contains(&inst) {
                continue;
            }
            let Some(result) = func.insts[inst].result else {
                continue;
            };
            if subst.contains(result) || edge_values.contains(&result) {
                continue;
            }
            let args = func.args(inst);
            if args.is_empty() {
                continue;
            }
            let Some(alloc) = object_of(func, args[0]) else {
                continue;
            };
            let Some(&shape) = fresh.get(&alloc) else {
                continue;
            };
            let passes = match func.insts[inst].op {
                Op::CheckClass(class) => shape.class_id() == class.class_id(),
                Op::CheckCode(code) => matches!(shape, Shape::Closure(c) if c == code),
                _ => false,
            };
            if passes {
                subst.add(result, args[0]);
                dead.insert(inst);
            }
        }
    }
}

/// A Bool value with the given truth, inserted before `before`.
fn fold_bool(func: &mut Func, block: Block, before: Inst, truth: bool) -> Value {
    let bits = if truth {
        SchemeValue::from_i32(0).bits()
    } else {
        SchemeValue::VALUE_FALSE as u64
    };
    let c = insert_before(
        func,
        block,
        before,
        Op::Const(Constant::Bits(bits)),
        &[],
        Type::Any,
    );
    insert_before(func, block, before, Op::TestInt32, &[c], Type::Bool)
}

fn insert_before(
    func: &mut Func,
    block: Block,
    before: Inst,
    op: Op,
    args: &[Value],
    ty: Type,
) -> Value {
    let value = func
        .push_inst(block, op, args, Some(ty), None)
        .expect("an operation with a result returns one");
    let list = &mut func.blocks[block].insts;
    let new = list.pop().expect("the instruction was just pushed");
    let at = list
        .iter()
        .position(|&inst| inst == before)
        .expect("the folded instruction is in its block");
    list.insert(at, new);
    value
}
