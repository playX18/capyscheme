//! Loop-closed SSA (LCSSA) repair, shared by the loop-cloning passes
//! ([`version`](super::version), [`unswitch`](super::unswitch),
//! [`unroll`](super::unroll)).
//!
//! Cloning a loop gives its values a second definition, so a block outside
//! the loop that reads a loop-defined value directly is no longer dominated
//! by a single definition. [`form_lcssa`] removes such reads. Every edge
//! that leaves the loop is split by a fresh *exit block* that takes the
//! live-out values as block parameters; every outside use is rewritten to
//! the parameter that reaches it, with join parameters inserted where
//! several exits meet (the on-the-fly construction of Braun et al.).
//!
//! After the repair a loop value leaves the loop only as an argument on an
//! exit edge. A clone then re-points those arguments at its own values and
//! jumps to the same exit block, whose parameters are the join of the two
//! loops.

use std::collections::{HashMap, HashSet};

use cranelift_entity::SecondaryMap;

use super::loops::{
    Loop, for_each_block_use, frame_uses, inst_blocks, loop_defined, outside_used_values,
};
use super::version::{FrameCloner, LoopClone, clone_term};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Edge, Terminator};

/// The most live-out values one loop may carry through its exits.
const MAX_LIVE_OUTS: usize = 32;

/// Makes `lp` loop-closed. Returns `false`, leaving `func` untouched, when
/// the loop cannot be repaired; returns `true` when it is loop-closed on
/// return (including when it already was).
pub(super) fn form_lcssa(func: &mut Func, lp: &Loop) -> bool {
    let live_outs = outside_used_values(func, lp);
    if live_outs.is_empty() {
        return true;
    }
    if live_outs.len() > MAX_LIVE_OUTS {
        return false;
    }
    let dom = func.dom_tree();
    let inst_block = inst_blocks(func);
    let mut def_block: HashMap<Value, Block> = HashMap::new();
    for &value in &live_outs {
        let block = match func.value_def(value) {
            ValueDef::Param { block, .. } => block,
            ValueDef::Inst(inst) => match inst_block[inst] {
                Some(block) => block,
                None => return false,
            },
        };
        def_block.insert(value, block);
    }
    let defined = loop_defined(func, lp);

    // Split every exit edge. `defs` maps (exit block, live-out) to the exit
    // block's parameter.
    let mut defs: HashMap<(Block, Value), Value> = HashMap::new();
    let mut exits: HashSet<Block> = HashSet::new();
    for &src in &lp.blocks {
        let Some(mut term) = func.blocks[src].term.take() else {
            continue;
        };
        for edge in edges_mut(&mut term) {
            if lp.member[edge.block] {
                continue;
            }
            let target = edge.block;
            let original = func.edge_args(edge).to_vec();
            let mut carried: Vec<Value> = live_outs
                .iter()
                .copied()
                .filter(|value| dom.dominates(def_block[value], src))
                .collect();
            for &arg in &original {
                if defined.contains(&arg) && !carried.contains(&arg) {
                    carried.push(arg);
                }
            }
            let exit = func.new_block();
            let params: Vec<Value> = carried
                .iter()
                .map(|&value| {
                    let ty = func.value_type(value);
                    func.append_param(exit, ty)
                })
                .collect();
            let renamed: HashMap<Value, Value> = carried
                .iter()
                .copied()
                .zip(params.iter().copied())
                .collect();
            let forwarded: Vec<Value> = original
                .iter()
                .map(|arg| renamed.get(arg).copied().unwrap_or(*arg))
                .collect();
            let out = func.edge(target, &forwarded);
            func.set_terminator(exit, Terminator::Jump(out));
            *edge = func.edge(exit, &carried);
            for (&value, &param) in carried.iter().zip(&params) {
                defs.insert((exit, value), param);
            }
            exits.insert(exit);
        }
        func.blocks[src].term = Some(term);
    }

    // Rewrite the outside uses.
    let dom = func.dom_tree();
    let mut reach: SecondaryMap<Block, bool> = SecondaryMap::new();
    for &block in dom.rpo() {
        reach[block] = true;
    }
    let mut updater = Updater {
        preds: func.predecessors(),
        reach,
        defs,
        memo: HashMap::new(),
    };
    let live_set: HashSet<Value> = live_outs.iter().copied().collect();
    let blocks: Vec<Block> = func.blocks.keys().collect();
    for block in blocks {
        if lp.member[block] || exits.contains(&block) || !updater.reach[block] {
            continue;
        }
        let mut used: Vec<Value> = Vec::new();
        for_each_block_use(func, block, &mut |value| {
            if live_set.contains(&value) && !used.contains(&value) {
                used.push(value);
            }
        });
        if used.is_empty() {
            continue;
        }
        let mut values: HashMap<Value, Value> = HashMap::new();
        for value in used {
            let reaching = updater.read(func, block, value);
            values.insert(value, reaching);
        }
        rewrite_block(func, block, values);
    }
    true
}

/// Whether no reachable block outside `lp` reads a loop-defined value.
#[cfg(test)]
pub(super) fn is_lcssa(func: &Func, lp: &Loop) -> bool {
    outside_used_values(func, lp).is_empty()
}

/// Replaces the values of `values` in everything `block` reads.
fn rewrite_block(func: &mut Func, block: Block, values: HashMap<Value, Value>) {
    let check = |value: Value| values.contains_key(&value);
    let mut cloner = FrameCloner::default();
    let insts = func.blocks[block].insts.clone();
    for inst in insts {
        let args: Vec<Value> = func
            .args(inst)
            .iter()
            .map(|arg| values.get(arg).copied().unwrap_or(*arg))
            .collect();
        func.insts[inst].args = cranelift_entity::EntityList::from_slice(&args, &mut func.pool);
        if let Some(frame) = func.insts[inst].frame {
            if frame_uses(func, frame, &check) {
                let rebuilt = cloner.clone_frame(func, &values, frame);
                func.insts[inst].frame = Some(rebuilt);
            }
        }
    }
    if let Some(term) = func.blocks[block].term.clone() {
        let map = LoopClone {
            blocks: HashMap::new(),
            values: values.clone(),
        };
        let rebuilt = clone_term(func, &map, &mut cloner, &term);
        func.blocks[block].term = Some(rebuilt);
    }
}

/// Every outgoing edge of `term`, mutably.
pub(super) fn edges_mut(term: &mut Terminator) -> Vec<&mut Edge> {
    match term {
        Terminator::Jump(edge) => vec![edge],
        Terminator::Branch {
            then, otherwise, ..
        } => vec![then, otherwise],
        Terminator::Switch { cases, default, .. } => cases
            .iter_mut()
            .map(|(_, edge)| edge)
            .chain(std::iter::once(default))
            .collect(),
        Terminator::Return(_)
        | Terminator::TailCall { .. }
        | Terminator::Deopt { .. }
        | Terminator::Unreachable => vec![],
    }
}

/// Appends `value` to every edge from `pred` into `block`.
fn add_edge_arg(func: &mut Func, pred: Block, block: Block, value: Value) {
    let Some(mut term) = func.blocks[pred].term.take() else {
        return;
    };
    for edge in edges_mut(&mut term) {
        if edge.block == block {
            edge.args.push(value, &mut func.pool);
        }
    }
    func.blocks[pred].term = Some(term);
}

/// On-the-fly SSA construction over the blocks outside the loop. The only
/// definitions are the exit blocks' parameters.
struct Updater {
    preds: SecondaryMap<Block, Vec<Block>>,
    reach: SecondaryMap<Block, bool>,
    defs: HashMap<(Block, Value), Value>,
    memo: HashMap<(Block, Value), Value>,
}

impl Updater {
    /// The definition of `value` that reaches the start of `block`.
    fn read(&mut self, func: &mut Func, block: Block, value: Value) -> Value {
        if let Some(&def) = self.defs.get(&(block, value)) {
            return def;
        }
        if let Some(&def) = self.memo.get(&(block, value)) {
            return def;
        }
        let mut preds: Vec<Block> = Vec::new();
        for &pred in &self.preds[block] {
            if self.reach[pred] && !preds.contains(&pred) {
                preds.push(pred);
            }
        }
        match preds.as_slice() {
            // Cannot happen for valid SSA: the definition dominates the use.
            [] => value,
            [only] => {
                let def = self.read(func, *only, value);
                self.memo.insert((block, value), def);
                def
            }
            _ => {
                let ty = func.value_type(value);
                let param = func.append_param(block, ty);
                // Memoize first so a cycle through `block` finds the param.
                self.memo.insert((block, value), param);
                for pred in preds {
                    let def = self.read(func, pred, value);
                    add_edge_arg(func, pred, block, def);
                }
                param
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::types::Type;
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::loops::find_loops;
    use crate::runtime::codegen::opt::passes::testutil::{
        block, branch, const_any, const_bool, jump, ret,
    };
    use crate::runtime::objectmodel::Value as SchemeValue;

    /// A loop whose header parameter is returned directly by the exit block.
    #[test]
    fn header_param_used_after_loop() {
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
        // `x` is read directly, not through an edge argument.
        ret(&mut func, exit, x);
        verify(&func).expect("valid input");

        let dom = func.dom_tree();
        let loops = find_loops(&func, &dom);
        let lp = &loops[0];
        assert!(!is_lcssa(&func, lp));
        assert!(form_lcssa(&mut func, lp));
        assert!(is_lcssa(&func, lp));
        verify(&func).expect("LCSSA keeps the IR valid");
    }

    /// Two exits that both reach one block: the block gets a join parameter.
    #[test]
    fn two_exits_join() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let c0 = const_bool(&mut func, entry, true);
        let header = block(&mut func);
        let x = func.append_param(header, Type::Any);
        let c = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let join = block(&mut func);
        jump(&mut func, entry, header, &[x0, c0]);
        branch(&mut func, header, c, body, &[], join, &[]);
        branch(&mut func, body, c, header, &[x, c], join, &[]);
        ret(&mut func, join, x);
        verify(&func).expect("valid input");

        let dom = func.dom_tree();
        let loops = find_loops(&func, &dom);
        let lp = &loops[0];
        assert!(form_lcssa(&mut func, lp));
        assert!(is_lcssa(&func, lp));
        assert_eq!(func.params(join).len(), 1, "one join parameter");
        verify(&func).expect("LCSSA keeps the IR valid");
    }

    /// A loop that is already loop-closed is not touched.
    #[test]
    fn closed_loop_is_unchanged() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x0 = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let c0 = const_bool(&mut func, entry, true);
        let header = block(&mut func);
        let x = func.append_param(header, Type::Any);
        let c = func.append_param(header, Type::Bool);
        let body = block(&mut func);
        let exit = block(&mut func);
        let p = func.append_param(exit, Type::Any);
        jump(&mut func, entry, header, &[x0, c0]);
        branch(&mut func, header, c, body, &[], exit, &[x]);
        jump(&mut func, body, header, &[x, c]);
        ret(&mut func, exit, p);
        let blocks = func.blocks.len();
        let dom = func.dom_tree();
        let loops = find_loops(&func, &dom);
        assert!(form_lcssa(&mut func, &loops[0]));
        assert_eq!(func.blocks.len(), blocks);
    }
}
