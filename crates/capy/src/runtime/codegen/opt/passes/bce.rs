//! Bounds-check elimination.
//!
//! A `CheckIndex` on `vector` and `index` proves two things: `index` is a
//! fixnum, and `0 <= index < length(vector)`. This pass drops the bounds part
//! when both halves follow from facts the IR already has. The check becomes
//! an `Unbox(Int32)`, which only untags, so the fixnum part must hold too.
//!
//! The upper bound comes from a branch. When a block has one predecessor
//! whose branch takes the edge into it on `a < length(v)` (a `CmpI32` `Lt`
//! taken, or `Ge` not taken, and their mirror images), every path into the
//! block passes that test, so `a < length(v)` holds in it and in every block
//! it dominates. `length(v)` is fixed for the life of the vector.
//!
//! The lower bound holds for a constant that is not negative, or for a block
//! parameter `p` that is a counter. Each entry into the block of `p` either
//! passes a non-negative constant, or passes `box(check(p) + c)` along a back
//! edge with `c > 0`. The addition deopts on overflow, so `p` never wraps and
//! stays at least its start.
//!
//! The range analysis supplies a second, independent
//! proof: a tagged index the domain already knows is a fixnum in
//! `[0, length)` needs no bounds check, and a vector whose length the
//! analysis can name (a constant-length `alloc.vector`, or a `vector-length`
//! whose own result is refined by a dominating test) bounds it directly.

use std::mem::offset_of;

use cranelift_entity::{EntityList, EntityRef, SecondaryMap};

use super::forward::{IndexKey, def_inst, index_key, object};
use super::{PassOutcome, Subst, range};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Inst, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::Vector;
use crate::runtime::prims::def::Primitive;

/// A fact `index < length(vector)` that holds in the current block.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Fact {
    index: IndexKey,
    /// The vector, looking through class checks.
    vector: Value,
}

/// A step of the walk over the dominator tree.
#[derive(Clone, Copy)]
enum Visit {
    Enter(Block),
    /// Leaves a block, dropping the facts it added.
    Exit(usize),
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let range_analysis = range::analyze(func);

    let subst = Subst::default();
    let dom = func.dom_tree();
    let preds = func.predecessors();
    let mut children: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    for &block in dom.rpo() {
        if let Some(parent) = dom.idom(block) {
            children[parent].push(block);
        }
    }

    let mut facts: Vec<Fact> = Vec::new();
    let mut rewrites: Vec<Inst> = Vec::new();
    let mut work = 0;
    let mut stack = vec![Visit::Enter(func.entry)];
    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(block) => {
                let mark = facts.len();
                if let Some(fact) = edge_fact(func, &subst, &preds, block) {
                    facts.push(fact);
                }
                for &inst in &func.blocks[block].insts {
                    work += 1;
                    if !matches!(func.insts[inst].op, Op::CheckIndex | Op::CheckStringIndex) {
                        continue;
                    }
                    let args = func.args(inst);
                    let (vector, index) = (args[0], args[1]);
                    // Only a tagged index is a fixnum to unbox. An Int32
                    // index is already unboxed, so there is nothing to drop.
                    if func.value_type(index) != Type::Any {
                        continue;
                    }
                    if range_analysis.bounds_proof(block, vector, index) {
                        rewrites.push(inst);
                        continue;
                    }
                    let key = index_key(func, &subst, index);
                    let upper = facts
                        .iter()
                        .any(|f| f.index == key && f.vector == object(func, &subst, vector));

                    if upper {
                        rewrites.push(inst);
                    }
                }
                stack.push(Visit::Exit(mark));
                for &child in &children[block] {
                    stack.push(Visit::Enter(child));
                }
            }
            Visit::Exit(mark) => facts.truncate(mark),
        }
    }
    drop(range_analysis);

    if rewrites.is_empty() {
        return PassOutcome {
            changed: false,
            work,
        };
    }
    for inst in rewrites.iter().copied() {
        let index = func.args(inst)[1];
        let data = &mut func.insts[inst];
        data.op = Op::Unbox(Repr::Int32);
        data.args = EntityList::from_slice(&[index], &mut func.pool);
        data.frame = None;
    }
    PassOutcome {
        changed: true,
        work,
    }
}

/// The fact that holds in `block` because its only predecessor branches to it
/// on a comparison, if the comparison is `index < length`.
fn edge_fact(
    func: &Func,
    subst: &Subst,
    preds: &SecondaryMap<Block, Vec<Block>>,
    block: Block,
) -> Option<Fact> {
    let [pred] = preds[block].as_slice() else {
        return None;
    };
    let Some(Terminator::Branch {
        cond,
        then,
        otherwise,
    }) = &func.blocks[*pred].term
    else {
        return None;
    };
    let positive = match (then.block == block, otherwise.block == block) {
        (true, false) => true,
        (false, true) => false,
        _ => return None,
    };
    let cond = def_inst(func, subst.resolve(*cond))?;
    let data = &func.insts[cond];
    let (Op::CmpI32(cmp) | Op::CmpI64(cmp)) = data.op else {
        return None;
    };
    let args = func.args(cond);
    // `lhs < rhs` on the edge, after the comparison is read with the edge's
    // polarity.
    let (lhs, rhs) = match (cmp, positive) {
        (Cmp::Lt, true) | (Cmp::Ge, false) => (args[0], args[1]),
        (Cmp::Gt, true) | (Cmp::Le, false) => (args[1], args[0]),
        _ => return None,
    };
    let vector = length_of(func, subst, rhs)?;
    Some(Fact {
        index: index_key(func, subst, lhs),
        vector,
    })
}

/// The vector whose length `value` is, for an int32 that holds a length.
fn length_of(func: &Func, subst: &Subst, value: Value) -> Option<Value> {
    let inst = def_inst(func, subst.resolve(value))?;
    let args = func.args(inst);
    match func.insts[inst].op {
        Op::VectorLength => Some(object(func, subst, args[0])),
        // `string-length` inlines to a raw load of the length word from a
        // class-checked string.
        Op::Load { field, .. }
            if field == (crate::runtime::sabi::STRING_LENGTH / 8) as u32
                && func.value_type(args[0]) == Type::Heap(ClassRef::STRING) =>
        {
            Some(object(func, subst, args[0]))
        }
        Op::CheckInt32
        | Op::CheckInt64
        | Op::CheckBigIntFits64
        | Op::Unbox(Repr::Int32)
        | Op::Unbox(Repr::Int64)
        | Op::Box(Repr::Int32)
        | Op::Box(Repr::Int64) => length_of(func, subst, args[0]),
        Op::CallPrim { prim, .. } => match Primitive::from_id(prim.index() as u16)? {
            Primitive::UsizeToValue => length_of(func, subst, args[0]),
            Primitive::Refptr => {
                let offset = index_key(func, subst, args[1]);
                let length_offset = IndexKey::Num(offset_of!(Vector, length) as i32);
                let string_offset = IndexKey::Num(crate::runtime::sabi::STRING_LENGTH as i32);
                (offset == length_offset || offset == string_offset)
                    .then(|| object(func, subst, args[0]))
            }
            _ => None,
        },
        _ => None,
    }
}
