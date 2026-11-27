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
use crate::runtime::codegen::opt::lir::{
    Clobbers, Constraint, DeoptExit, LirFunc, LirTerm, Operand, Role, SafePoint, VReg,
};

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
    calls: Vec<u32>,
}

impl Weights {
    pub(crate) fn compute(func: &LirFunc, num: &Numbering) -> Self {
        let weights = block_weights(func);
        let mut accesses: SecondaryMap<VReg, Vec<(u32, u64)>> = SecondaryMap::new();
        let mut free_accesses: SecondaryMap<VReg, Vec<(u32, u64)>> = SecondaryMap::new();
        let mut blocks: Vec<(u32, u64)> = Vec::with_capacity(num.order.len());
        let mut calls: Vec<u32> = Vec::new();
        for &block in &num.order {
            let data = &func.blocks[block];
            let w = weights[block];
            blocks.push((num.from(block), w));
            for (k, inst) in data.insts.iter().enumerate() {
                let g = num.base[block] + k as u32;
                if inst.clobbers() == Clobbers::RustCall {
                    calls.push(pos::late(g));
                }

                let exit = if inst.safepoint().is_some() {
                    None
                } else {
                    inst.deopt_exit()
                };

                let mut frame = frame_regs_of(func, exit, inst.safepoint());
                frame.extend(super::intervals::poll_lazy_frame(func, inst));

                record(
                    &mut frame,
                    inst.operands(func),
                    (pos::early(g), pos::late(g)),
                    w,
                    &mut accesses,
                    &mut free_accesses,
                );
            }

            if let Some(term) = &data.term {
                let g = num.term[block];
                let exit = match term {
                    LirTerm::Deopt { exit } => Some(*exit),
                    _ => None,
                };
                let mut frame = frame_regs_of(func, exit, None);

                record(
                    &mut frame,
                    term.operands(func),
                    (pos::early(g), pos::late(g)),
                    w,
                    &mut accesses,
                    &mut free_accesses,
                );
            }
        }
        for list in accesses.values_mut().chain(free_accesses.values_mut()) {
            list.sort_unstable();
        }
        blocks.sort_unstable();
        calls.sort_unstable();

        Self {
            accesses,
            free_accesses,
            blocks,
            calls,
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

    fn crosses_call(&self, ranges: &[(u32, u32)]) -> bool {
        for &c in &self.calls {
            for &(lo, hi) in ranges {
                if lo < c && c < hi {
                    return true;
                }
            }
        }
        false
    }

    /// The cost of spilling `ranges` of `vreg`: higher means more expensive.
    pub(crate) fn spill_weight(&self, vreg: VReg, ranges: &[(u32, u32)]) -> u64 {
        let cost = u128::from(self.piece_cost(vreg, ranges));
        let cost = if self.crosses_call(ranges) {
            cost.saturating_mul(2)
        } else {
            cost
        };

        let denominator = u128::from(live_len(ranges).saturating_add(BIAS));

        u64::try_from(cost * SCALE / denominator).unwrap_or(u64::MAX)
    }
}

/// The frame values a deopt exit or a safepoint reads at an instruction.
fn frame_regs_of(
    func: &LirFunc,
    exit: Option<DeoptExit>,
    safepoint: Option<SafePoint>,
) -> Vec<VReg> {
    let mut frame = Vec::new();
    if let Some(e) = exit {
        frame.extend(func.frame_regs(&func.exits[e].values));
    }
    if let Some(sp) = safepoint {
        frame.extend(func.frame_regs(&func.safepoints[sp].values));
    }
    frame
}

/// Records the accesses of one instruction, leaving out its frame reads.
fn record(
    frame: &mut Vec<VReg>,
    operands: Vec<Operand>,
    (early, late): (u32, u32),
    w: u64,
    accesses: &mut SecondaryMap<VReg, Vec<(u32, u64)>>,
    free_accesses: &mut SecondaryMap<VReg, Vec<(u32, u64)>>,
) {
    for op in operands.into_iter().rev() {
        if op.role == Role::Use {
            if let Some(i) = frame.iter().position(|&v| v == op.vreg) {
                frame.swap_remove(i);
                continue;
            }
        }
        let at = if op.role == Role::Def { late } else { early };
        accesses[op.vreg].push((at, w));
        if !matches!(op.constraint, Constraint::Fixed(_)) {
            free_accesses[op.vreg].push((at, w));
        }
    }
}

/// The number of points in `ranges`.
pub(crate) fn live_len(ranges: &[(u32, u32)]) -> u64 {
    ranges.iter().map(|&(lo, hi)| u64::from(hi - lo)).sum()
}
