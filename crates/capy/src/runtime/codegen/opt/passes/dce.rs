//! Dead code elimination.

use cranelift_entity::{EntityList, SecondaryMap};

use super::{PassOutcome, reachable};
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Inst, Terminator};

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = false;
    let live_block = reachable(func);
    let blocks: Vec<Block> = func.blocks.keys().collect();
    for &block in &blocks {
        if live_block[block] {
            continue;
        }
        let data = &mut func.blocks[block];
        let dirty = !data.insts.is_empty()
            || data.params.len(&func.pool) > 0
            || !matches!(data.term, Some(Terminator::Unreachable));
        if dirty {
            data.insts.clear();
            data.params = EntityList::new();
            data.term = Some(Terminator::Unreachable);
            changed = true;
        }
    }

    let mut marks = Marks::new();
    let mut work = 0;
    for &block in &blocks {
        if !live_block[block] {
            continue;
        }
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            if !removable(func, inst) {
                marks.mark_inst(inst);
            }
        }
        if let Some(term) = &func.blocks[block].term {
            for value in func.terminator_uses(term) {
                marks.mark_value(value);
            }
            if let Terminator::TailCall { frame, .. } | Terminator::Deopt { frame } = term {
                for value in func.frame_values(*frame) {
                    marks.mark_value(value);
                }
            }
        }
    }
    marks.drain(func);

    for &block in &blocks {
        if !live_block[block] {
            continue;
        }
        let before = func.blocks[block].insts.len();
        let live = &marks.live;
        func.blocks[block].insts.retain(|&inst| live[inst]);
        changed |= func.blocks[block].insts.len() != before;
    }
    PassOutcome {
        changed,
        work: work + func.blocks.len(),
    }
}

/// Whether `inst` may be dropped when its result is unused. Only a pure
/// operation or a fresh allocation qualifies. A guard, a call and an effect
/// always stay.
fn removable(func: &Func, inst: Inst) -> bool {
    let data = &func.insts[inst];
    data.result.is_some()
        && !data.op.can_deopt()
        && !data.op.is_call()
        && (data.op.effects().effect_free() || data.op.effects().allocation_only())
}

/// The live instructions, found by a worklist from the roots.
struct Marks {
    live: SecondaryMap<Inst, bool>,
    /// Values whose defining instruction may still need marking.
    pending: Vec<Value>,
    /// Live instructions whose operands and frame still need marking.
    queue: Vec<Inst>,
}

impl Marks {
    fn new() -> Self {
        Self {
            live: SecondaryMap::new(),
            pending: Vec::new(),
            queue: Vec::new(),
        }
    }

    fn mark_inst(&mut self, inst: Inst) {
        if !self.live[inst] {
            self.live[inst] = true;
            self.queue.push(inst);
        }
    }

    fn mark_value(&mut self, value: Value) {
        self.pending.push(value);
    }

    /// Propagates liveness from the roots to every operand and frame value.
    fn drain(&mut self, func: &Func) {
        loop {
            if let Some(inst) = self.queue.pop() {
                self.pending.extend_from_slice(func.args(inst));
                if let Some(frame) = func.insts[inst].frame {
                    self.pending.extend(func.frame_values(frame));
                }
            } else if let Some(value) = self.pending.pop() {
                if let ValueDef::Inst(def) = func.value_def(value) {
                    self.mark_inst(def);
                }
            } else {
                break;
            }
        }
    }
}
