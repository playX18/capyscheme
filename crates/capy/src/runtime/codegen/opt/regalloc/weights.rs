//! Spill weights: how much a piece costs to keep in a slot.
//!
//! The weight of a vreg is the weighted cost of its reads and writes divided by
//! the length of the piece, so a value that is read once in a hot loop outranks
//! a value that lives a long time for a single cold use.
//!
use cranelift_entity::SecondaryMap;

use super::liveness::Numbering;
use super::pos;
use crate::runtime::codegen::opt::lir::weights::block_weights;
use crate::runtime::codegen::opt::lir::{Constraint, LirFunc, LirTerm, Role, VReg};

const BIAS: u64 = 100_000;
const SCALE: u128 = 1 << 24;

pub(crate) struct Weights {
    /// Every read and write of a vreg, frame reads included, with the weight
    /// of its block, sorted by position.
    accesses: SecondaryMap<VReg, Vec<(u32, u64)>>,
    /// The accesses a register helps: those whose operand is not pinned to a
    /// fixed register. A pinned operand costs one move whether the value
    /// sits in a slot or in a register, so keeping the value in a register
    /// does not pay for it.
    free_accesses: SecondaryMap<VReg, Vec<(u32, u64)>>,
    /// The first position and weight of each block, sorted by position.
    blocks: Vec<(u32, u64)>,
}

impl Weights {
    pub(crate) fn compute(func: &LirFunc, num: &Numbering) -> Self {
        let weights = block_weights(func);
        let mut accesses: SecondaryMap<VReg, Vec<(u32, u64)>> = SecondaryMap::new();
        let mut free_accesses: SecondaryMap<VReg, Vec<(u32, u64)>> = SecondaryMap::new();
        let mut blocks: Vec<(u32, u64)> = Vec::with_capacity(num.order.len());
        for &block in &num.order {
            let data = &func.blocks[block];
            let w = weights[block];
            blocks.push((num.from(block), w));
            for (k, inst) in data.insts.iter().enumerate() {
                let g = num.base[block] + k as u32;

                for op in inst.operands(func) {
                    let at = if op.role == Role::Def {
                        pos::late(g)
                    } else {
                        pos::early(g)
                    };
                    accesses[op.vreg].push((at, w));
                    if !matches!(op.constraint, Constraint::Fixed(_)) {
                        free_accesses[op.vreg].push((at, w));
                    }
                }

                // A deopt exit or a safepoint reads its frame values here, not
                // through the instruction's own operands. They still occupy a
                // location at this position, so a split decision must see them.
                if let Some(exit) = inst.deopt_exit() {
                    for op in func.frame_ops(&func.exits[exit].values) {
                        accesses[op.vreg].push((pos::early(g), w));
                    }
                }

                if let Some(sp) = inst.safepoint() {
                    for op in func.frame_ops(&func.safepoints[sp].values) {
                        accesses[op.vreg].push((pos::early(g), w));
                    }
                }
            }
            if let Some(term) = &data.term {
                let g = num.term[block];
                for op in term.operands(func) {
                    let at = if op.role == Role::Def {
                        pos::late(g)
                    } else {
                        pos::early(g)
                    };
                    accesses[op.vreg].push((at, w));
                    if !matches!(op.constraint, Constraint::Fixed(_)) {
                        free_accesses[op.vreg].push((at, w));
                    }
                }
                if let LirTerm::Deopt { exit } = term {
                    for op in func.frame_ops(&func.exits[*exit].values) {
                        accesses[op.vreg].push((pos::early(g), w));
                    }
                }
            }
        }
        for list in accesses.values_mut().chain(free_accesses.values_mut()) {
            list.sort_unstable();
        }
        blocks.sort_unstable();
        Self {
            accesses,
            free_accesses,
            blocks,
        }
    }

    /// The weighted read/write cost of `vreg` inside `ranges`: what keeping
    /// that part of the vreg in a slot costs in memory operations.
    pub(crate) fn piece_cost(&self, vreg: VReg, ranges: &[(u32, u32)]) -> u64 {
        let list = &self.accesses[vreg];
        let mut total = 0u64;
        for &(lo, hi) in ranges {
            total = total.saturating_add(self.cost_between(list, lo, hi));
        }
        total
    }

    /// The access cost of `vreg` in `[lo, hi)`.
    pub(crate) fn cost_in(&self, vreg: VReg, lo: u32, hi: u32) -> u64 {
        self.cost_between(&self.accesses[vreg], lo, hi)
    }

    fn cost_between(&self, list: &[(u32, u64)], lo: u32, hi: u32) -> u64 {
        let start = list.partition_point(|&(p, _)| p < lo);
        list[start..]
            .iter()
            .take_while(|&&(p, _)| p < hi)
            .fold(0u64, |acc, &(_, w)| acc.saturating_add(w))
    }

    /// The accesses of `vreg` in `[lo, hi)`, as `(position, block weight)`.
    /// The accesses inside `[lo, hi)` that a register helps.
    pub(crate) fn free_accesses_in(&self, vreg: VReg, lo: u32, hi: u32) -> &[(u32, u64)] {
        let list = &self.free_accesses[vreg];
        let start = list.partition_point(|&(p, _)| p < lo);
        let end = list.partition_point(|&(p, _)| p < hi);
        &list[start..end]
    }

    pub(crate) fn accesses_in(&self, vreg: VReg, lo: u32, hi: u32) -> &[(u32, u64)] {
        let list = &self.accesses[vreg];
        let start = list.partition_point(|&(p, _)| p < lo);
        let end = list.partition_point(|&(p, _)| p < hi);
        &list[start..end]
    }

    /// The first and last access of `vreg` inside `[lo, hi)`.
    pub(crate) fn access_span(&self, vreg: VReg, lo: u32, hi: u32) -> Option<(u32, u32)> {
        let list = &self.accesses[vreg];
        let start = list.partition_point(|&(p, _)| p < lo);
        let first = list.get(start).filter(|&&(p, _)| p < hi)?.0;
        let end = list.partition_point(|&(p, _)| p < hi);
        Some((first, list[end - 1].0))
    }

    /// The cost of one move at position `p`: the weight of its block.
    pub(crate) fn boundary_cost(&self, p: u32) -> u64 {
        let i = self.blocks.partition_point(|&(start, _)| start <= p);
        self.blocks.get(i.wrapping_sub(1)).map_or(1, |&(_, w)| w)
    }

    /// The cost of spilling `ranges` of `vreg`: higher means more expensive.
    pub(crate) fn spill_weight(&self, vreg: VReg, ranges: &[(u32, u32)]) -> u64 {
        let cost = u128::from(self.piece_cost(vreg, ranges));
        let denominator = u128::from(live_len(ranges).saturating_add(BIAS));
        u64::try_from(cost * SCALE / denominator).unwrap_or(u64::MAX)
    }
}

/// The number of points in `ranges`.
pub(crate) fn live_len(ranges: &[(u32, u32)]) -> u64 {
    ranges.iter().map(|&(lo, hi)| u64::from(hi - lo)).sum()
}
