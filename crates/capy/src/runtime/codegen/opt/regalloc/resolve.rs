//! Moves that connect the pieces of a vreg.
//!
//! A move point is a global instruction index with a rank that orders the
//! moves within it:
//!
//! - rank 0: the moves that enter a block (a single-predecessor target), and
//!   the reloads at an odd boundary that starts the next instruction;
//! - rank 1: moves at an even boundary, before the instruction;
//! - rank 2: spills at an odd boundary, which run before a clobber;
//! - rank 3: the moves on the edges that leave a block with one successor.
//!
//! Moves with the same key form one parallel move and are sequentialized
//! together. Keys run in order, so a later group sees the results of an
//! earlier one.

use std::collections::BTreeMap;

use cranelift_entity::{EntityRef, SecondaryMap};

use super::linear_scan::{Assignment, end_of};
use super::liveness::{Liveness, Numbering};
use super::{Loc, Move, Scratch};
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, RegClass, VReg};

/// Orders the moves of one parallel move so that no source is overwritten
/// before it is read. A cycle is broken through the scratch register of its
/// class. The scratch only holds one value at a time, because a cycle's
/// chain drains before another cycle is broken.
pub(crate) fn sequentialize(moves: Vec<Move>, scratch: Scratch) -> Vec<Move> {
    let mut pending: Vec<Move> = moves.into_iter().filter(|m| m.src != m.dst).collect();

    // One destination cannot need the same value twice, and coalesced
    // edges can name the same pair from two live-in spellings.
    let mut seen: Vec<Move> = Vec::with_capacity(pending.len());
    pending.retain(|m| {
        if seen.contains(m) {
            false
        } else {
            seen.push(*m);
            true
        }
    });

    let mut out = Vec::with_capacity(pending.len());
    while !pending.is_empty() {
        let ready = (0..pending.len()).find(|&i| {
            !pending
                .iter()
                .enumerate()
                .any(|(j, m)| j != i && m.src == pending[i].dst)
        });
        match ready {
            Some(i) => out.push(pending.remove(i)),
            None => {
                // Break the cycle by saving a destination. Prefer a register
                // destination: the scratch save is then a register move, not
                // a memory access through a slot.
                let at = {
                    pending
                        .iter()
                        .position(|m| {
                            matches!(m.dst, Loc::Reg(_)) && pending.iter().any(|n| n.src == m.dst)
                        })
                        .unwrap_or(0)
                };
                let d = pending[at].dst;
                let class = pending
                    .iter()
                    .find(|m| m.src == d)
                    .map_or(pending[at].class, |m| m.class);
                let reg = match class {
                    RegClass::Gpr => scratch.gpr,
                    RegClass::Fpr => scratch.fpr,
                };
                let tmp = Loc::Reg(reg);
                out.push(Move {
                    class,
                    src: d,
                    dst: tmp,
                });
                for m in pending.iter_mut() {
                    if m.src == d {
                        m.src = tmp;
                    }
                }
            }
        }
    }
    out
}

/// The moves before each instruction, indexed by global instruction index.
pub(crate) fn resolve(
    func: &LirFunc,
    num: &Numbering,
    live: &Liveness,
    asg: &Assignment,
    scratch: Scratch,
) -> Result<Vec<Vec<Move>>, String> {
    let mut groups: BTreeMap<(u32, u8), Vec<Move>> = BTreeMap::new();

    // Child boundaries inside a block.
    for v in func.vregs.keys() {
        let mut ids = asg.vreg_children[v].clone();
        if ids.len() < 2 {
            continue;
        }
        ids.sort_by_key(|&id| asg.children[id].ranges[0].0);
        for pair in ids.windows(2) {
            let a = &asg.children[pair[0]];
            let b = &asg.children[pair[1]];
            let p = end_of(&a.ranges);
            if p != b.ranges[0].0 {
                continue;
            }
            let g = p / 2;
            let blk = num.block_at[g as usize];
            // A block boundary is an edge, handled below.
            if p == num.from(blk) {
                continue;
            }
            let src = asg
                .loc_of_child(a)
                .ok_or_else(|| format!("{v} has an unplaced piece"))?;
            let dst = asg
                .loc_of_child(b)
                .ok_or_else(|| format!("{v} has an unplaced piece"))?;
            if src == dst {
                continue;
            }
            let key = if p % 2 == 0 {
                (g, 1)
            } else if matches!((src, dst), (Loc::Reg(_), Loc::Slot(_))) {
                (g, 2)
            } else {
                if g + 1 >= num.count || num.block_at[(g + 1) as usize] != blk {
                    return Err(format!("a reload of {v} falls past its block"));
                }
                (g + 1, 0)
            };
            groups.entry(key).or_default().push(Move {
                class: func.class(v),
                src,
                dst,
            });
        }
    }

    // Edges. Each target with several predecessors has single-successor
    // predecessors only, so an edge's moves go at the end of its source or
    // at the start of its target.
    let mut preds: SecondaryMap<LBlock, u32> = SecondaryMap::new();
    for (_, data) in func.blocks.iter() {
        if let Some(term) = &data.term {
            for e in term.edges() {
                preds[e.block] += 1;
            }
        }
    }
    for &pb in &num.order {
        let Some(term) = &func.blocks[pb].term else {
            continue;
        };
        let edges = term.edges();
        let tpos = num.term_pos(pb);
        for e in &edges {
            let s = e.block;
            let key = if edges.len() == 1 {
                (num.term[pb], 3)
            } else {
                if preds[s] != 1 {
                    return Err(format!("the edge {pb} -> {s} is critical"));
                }
                (num.base[s], 0)
            };
            let spos = num.from(s);
            let params = &func.blocks[s].params;
            if e.args.len() != params.len() {
                return Err(format!("the edge {pb} -> {s} has the wrong arity"));
            }
            let mut pairs: Vec<(VReg, VReg)> = Vec::new();
            for (i, &arg) in e.args.iter().enumerate() {
                let param = params[i];
                if !asg.vreg_children[param].is_empty() {
                    pairs.push((arg, param));
                }
            }
            for v in live.live_in[s].iter() {
                let v = VReg::new(v);
                if !params.contains(&v) {
                    pairs.push((v, v));
                }
            }
            for (sv, dv) in pairs {
                let src = asg
                    .loc_at(sv, tpos)
                    .ok_or_else(|| format!("{sv} is not live on {pb} -> {s}"))?;
                let dst = asg
                    .loc_at(dv, spos)
                    .ok_or_else(|| format!("{dv} is not live at the start of {s}"))?;
                if src != dst {
                    groups.entry(key).or_default().push(Move {
                        class: func.class(dv),
                        src,
                        dst,
                    });
                }
            }
        }
    }

    let mut out: Vec<Vec<Move>> = vec![Vec::new(); num.count as usize];
    for ((g, _), moves) in groups {
        out[g as usize].extend(sequentialize(moves, scratch));
    }
    Ok(out)
}
