//! Placement of floating values.

use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::SecondaryMap;

use crate::runtime::codegen::opt::ir::func::{Block, DomTree, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Inst, Op, Terminator};

/// Whether an operation may be placed freely. Such an operation has no
/// effect, cannot deopt, is not a call, and reads and writes no memory.
pub fn is_floating(op: Op) -> bool {
    let effects = op.effects();
    op.has_result()
        && !op.can_deopt()
        && !op.is_call()
        && effects.effect_free()
        && effects.reads.is_empty()
        && effects.writes.is_empty()
}

/// The result of scheduling.
pub struct Schedule {
    /// The block of each placed floating value. Unplaced values have no use
    /// and are never lowered.
    placed: SecondaryMap<Value, Option<Block>>,
    /// The floating values placed in each block, in value order. Value order
    /// is a topological order of the definitions.
    at_block: SecondaryMap<Block, Vec<Value>>,
}

impl Schedule {
    pub fn block_of(&self, value: Value) -> Option<Block> {
        self.placed[value]
    }

    pub fn values_in(&self, block: Block) -> &[Value] {
        &self.at_block[block]
    }
}

/// Places every floating value of `func` that has a use.
pub fn schedule(func: &Func<'_>) -> Schedule {
    let dom = func.dom_tree();
    let depth = loop_depths(func, &dom);

    let mut def_block: SecondaryMap<Inst, Option<Block>> = SecondaryMap::new();
    let mut is_floating_value: SecondaryMap<Value, bool> = SecondaryMap::new();
    let mut floating: Vec<Value> = Vec::new();
    for &block in dom.rpo() {
        for &inst in &func.blocks[block].insts {
            def_block[inst] = Some(block);
            let data = &func.insts[inst];
            if let Some(result) = data.result {
                if is_floating(data.op) {
                    is_floating_value[result] = true;
                    floating.push(result);
                }
            }
        }
    }

    let mut scheduler = Scheduler {
        func,
        dom: &dom,
        depth: &depth,
        def_block: &def_block,
        is_floating: &is_floating_value,
        early: SecondaryMap::new(),
        uses: SecondaryMap::new(),
    };

    scheduler.compute_early(&floating);
    scheduler.collect_pinned_uses();
    let (placed, mut at_block) = scheduler.place(&floating);

    // Floating values in value order, so lowering sees them in a stable order.
    for &block in dom.rpo() {
        at_block[block].sort();
    }
    Schedule { placed, at_block }
}

struct Scheduler<'a> {
    func: &'a Func<'a>,
    dom: &'a DomTree,
    depth: &'a SecondaryMap<Block, u32>,
    def_block: &'a SecondaryMap<Inst, Option<Block>>,
    is_floating: &'a SecondaryMap<Value, bool>,
    /// The earliest legal block of each floating value.
    early: SecondaryMap<Value, Option<Block>>,
    /// Blocks where each value is read, with one entry per use.
    uses: SecondaryMap<Value, Vec<Block>>,
}

impl Scheduler<'_> {
    /// The block where `value` becomes available: its definition for a
    /// pinned value, or its earliest block for a floating one.
    fn home(&self, value: Value) -> Option<Block> {
        if self.is_floating[value] {
            return self.early[value];
        }
        match self.func.value_def(value) {
            ValueDef::Param { block, .. } => Some(block),
            ValueDef::Inst(inst) => self.def_block[inst],
        }
    }

    /// The deepest of the operands' homes. Every home dominates the others,
    /// because they are all definitions that reach this instruction.
    fn compute_early(&mut self, floating: &[Value]) {
        for &value in floating {
            let inst = self.def_inst(value);
            let mut best = self.func.entry;
            for &arg in self.func.args(inst) {
                let Some(home) = self.home(arg) else { continue };
                if self.dom.dominates(best, home) {
                    best = home;
                }
            }
            self.early[value] = Some(best);
        }
    }

    /// Records the uses that do not come from floating values: pinned
    /// instructions, frame states, and terminators.
    fn collect_pinned_uses(&mut self) {
        let func = self.func;
        for &block in self.dom.rpo() {
            for &inst in &func.blocks[block].insts {
                let data = &func.insts[inst];
                if is_floating(data.op) {
                    continue;
                }
                for &arg in func.args(inst) {
                    self.uses[arg].push(block);
                }
                if let Some(frame) = data.frame {
                    for value in func.frame_values(frame) {
                        self.uses[value].push(block);
                    }
                }
            }
            if let Some(term) = &func.blocks[block].term {
                self.add_terminator_uses(block, term);
            }
        }
    }

    /// A tail call replaces the frame, so its frame state is never rebuilt
    /// and is not a use.
    fn add_terminator_uses(&mut self, block: Block, term: &Terminator) {
        for value in self.func.terminator_uses(term) {
            self.uses[value].push(block);
        }
        if let Terminator::Deopt { frame } = term {
            for value in self.func.frame_values(*frame) {
                self.uses[value].push(block);
            }
        }
    }

    fn def_inst(&self, value: Value) -> Inst {
        match self.func.value_def(value) {
            ValueDef::Inst(inst) => inst,
            ValueDef::Param { .. } => unreachable!("a parameter is never floating"),
        }
    }

    /// Late placement, in reverse program order so that every user is placed
    /// before the values it reads.
    fn place(
        &mut self,
        floating: &[Value],
    ) -> (
        SecondaryMap<Value, Option<Block>>,
        SecondaryMap<Block, Vec<Value>>,
    ) {
        let mut placed: SecondaryMap<Value, Option<Block>> = SecondaryMap::new();
        let mut at_block: SecondaryMap<Block, Vec<Value>> = SecondaryMap::new();
        for &value in floating.iter().rev() {
            if self.uses[value].is_empty() {
                continue;
            }
            let users = std::mem::take(&mut self.uses[value]);
            let mut late = users[0];
            for &block in &users[1..] {
                late = lca(self.dom, late, block);
            }

            let early = self.early[value].expect("floating values have an early block");
            debug_assert!(self.dom.dominates(early, late));
            let mut best = late;
            let mut cur = late;
            loop {
                if self.depth[cur] < self.depth[best] {
                    best = cur;
                }
                if cur == early {
                    break;
                }
                match self.dom.idom(cur) {
                    Some(parent) => cur = parent,
                    None => break,
                }
            }

            placed[value] = Some(best);
            at_block[best].push(value);
            let inst = self.def_inst(value);
            for &arg in self.func.args(inst) {
                self.uses[arg].push(best);
            }
        }
        (placed, at_block)
    }
}

/// The lowest common dominator of two reachable blocks.
fn lca(dom: &DomTree, mut a: Block, b: Block) -> Block {
    while !dom.dominates(a, b) {
        a = dom
            .idom(a)
            .expect("a reachable block is dominated by the entry");
    }
    a
}

/// The loop nesting depth of each reachable block, from natural loops.
///
/// A back edge `b -> h` is one whose target dominates its source. The loop
/// body of `h` is every block that reaches `b` without passing through `h`.
/// Back edges to one header share a body. Irreducible cycles have no header
/// that dominates them, so they count for nothing.
fn loop_depths(func: &Func<'_>, dom: &DomTree) -> SecondaryMap<Block, u32> {
    let preds = func.predecessors();
    let mut bodies: BTreeMap<Block, BTreeSet<Block>> = BTreeMap::new();
    for &block in dom.rpo() {
        for header in func.successors(block) {
            if !dom.dominates(header, block) {
                continue;
            }
            let body = bodies.entry(header).or_default();
            body.insert(header);
            let mut stack = vec![block];
            while let Some(cur) = stack.pop() {
                if body.insert(cur) {
                    stack.extend(preds[cur].iter().copied().filter(|&p| dom.is_reachable(p)));
                }
            }
        }
    }

    let mut depth: SecondaryMap<Block, u32> = SecondaryMap::new();
    for body in bodies.values() {
        for &block in body {
            depth[block] += 1;
        }
    }
    depth
}
