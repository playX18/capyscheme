//! Linear scan
//!
//! Wimmer–Franz with lifetime holes and interval splitting, plus spill-weight
//! eviction: when no register is free the scan spills whichever of the incoming
//! piece and the blocking pieces is cheapest to spill. The pieces, their
//! locations and the spill slots are described by [`super::assign`]

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use super::assign::{
    Assignment, Pieces, covers, end_of, finish, next_intersection, placed_reg, placed_reg_before,
};
use super::intervals::{Assign, Built};
use super::liveness::Numbering;
use super::pos;
use super::weights::Weights;
use super::{PhysReg, RegFile};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::lir::{LirFunc, RegClass};

/// Runs linear scan and assigns spill slots.
pub(crate) fn run(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
    regs: &RegFile,
    weights: &Weights,
) -> Result<Assignment, String> {
    let mut events: Vec<u32> = built
        .fixed
        .values()
        .flatten()
        .flat_map(|&(lo, hi)| [lo, hi])
        .collect();
    events.sort_unstable();
    events.dedup();

    let evict = opt().evict;
    let mut pieces = Pieces::new(built);
    scan_class(
        func,
        built,
        &mut pieces,
        RegClass::Gpr,
        regs,
        weights,
        evict,
        &events,
    )?;
    scan_class(
        func,
        built,
        &mut pieces,
        RegClass::Fpr,
        regs,
        weights,
        evict,
        &events,
    )?;

    if let Some(c) = pieces.children.iter().find(|c| c.assign.is_none()) {
        return Err(format!("a piece of {} was left unplaced", c.vreg));
    }
    Ok(finish(func, num, pieces))
}

fn scan_class(
    func: &LirFunc,
    built: &Built,
    pieces: &mut Pieces,
    class: RegClass,
    regs: &RegFile,
    weights: &Weights,
    evict: bool,
    events: &[u32],
) -> Result<(), String> {
    let pool = regs.pool(class);

    let mut unhandled = BinaryHeap::new();
    for (id, c) in pieces.children.iter().enumerate() {
        if c.assign.is_none() && func.class(c.vreg) == class {
            unhandled.push(Reverse((c.ranges[0].0, id)));
        }
    }

    let mut active: Vec<usize> = Vec::new();
    let mut inactive: Vec<usize> = Vec::new();
    // Pieces that were evicted at least once. They may be allocated again, but
    // they never evict: that bounds an eviction chain and makes it terminate.
    let mut evicted: Vec<bool> = Vec::new();
    while let Some(Reverse((s, cur))) = unhandled.pop() {
        // Move pieces between the active and inactive lists for position s.
        let mut kept = Vec::with_capacity(active.len());
        for a in std::mem::take(&mut active) {
            let c = &pieces.children[a];
            if covers(&c.ranges, s) {
                kept.push(a);
            } else if end_of(&c.ranges) > s {
                inactive.push(a);
            }
        }
        active = kept;
        let mut kept = Vec::new();
        for i in std::mem::take(&mut inactive) {
            let c = &pieces.children[i];
            if covers(&c.ranges, s) {
                active.push(i);
            } else if end_of(&c.ranges) > s {
                kept.push(i);
            }
        }
        inactive = kept;

        let mut ranges = pieces.children[cur].ranges.clone();
        let mut e = end_of(&ranges);

        let at_event = events.binary_search(&s).is_ok();
        let arrives_in_slot = at_event
            && pieces.vreg_children[pieces.children[cur].vreg]
                .iter()
                .any(|&id| {
                    let c = &pieces.children[id];
                    c.assign == Some(Assign::Stack) && end_of(&c.ranges) == s
                });

        if at_event && arrives_in_slot {
            let vreg = pieces.children[cur].vreg;
            let first = built.uses[vreg]
                .iter()
                .copied()
                .find(|&u| u >= s && covers(&ranges, u));
            // The reload runs at the gap before the reading instruction.
            let cut = first.map_or(e, pos::gap_floor);
            if cut > s {
                if let Some(id) = pieces.split(cur, cut) {
                    pieces.children[cur].assign = Some(Assign::Stack);
                    unhandled.push(Reverse((pieces.children[id].ranges[0].0, id)));
                } else {
                    // The first use is the end of the piece: no reload follows.
                    pieces.children[cur].assign = Some(Assign::Stack);
                }
                continue;
            }
            ranges = pieces.children[cur].ranges.clone();
            e = end_of(&ranges);
        }
        let reg_index = |r: PhysReg| pool.iter().position(|&p| p == r);

        let mut free = vec![u32::MAX; pool.len()];
        let mut holder: Vec<Option<usize>> = vec![None; pool.len()];
        for &i in &inactive {
            if let Some(Assign::Reg(r)) = pieces.children[i].assign {
                if let Some(k) = reg_index(r) {
                    if let Some(p) = next_intersection(&pieces.children[i].ranges, &ranges, s) {
                        free[k] = free[k].min(p);
                    }
                }
            }
        }
        for (k, r) in pool.iter().enumerate() {
            if let Some(f) = built.fixed.get(r) {
                if let Some(p) = next_intersection(f, &ranges, s) {
                    free[k] = free[k].min(p);
                }
            }
        }
        let free_static = free.clone();
        for &a in &active {
            if let Some(Assign::Reg(r)) = pieces.children[a].assign {
                if let Some(k) = reg_index(r) {
                    free[k] = 0;
                    holder[k] = Some(a);
                }
            }
        }

        let vreg = pieces.children[cur].vreg;

        let call_safe = built.call_safe[vreg].iter().any(|&p| covers(&ranges, p));
        let allowed = |r: PhysReg| !call_safe || regs.call_preserved(r);

        // Coalescing: take the copy partner's register when it stays free for
        // the whole piece, so the edge or `Move` disappears. A piece split at
        // a boundary also resumes in its own previous register when possible.
        let mut full_hint = None;

        let mut candidates: Vec<usize> = Vec::new();
        if let Some(r) = placed_reg_before(pieces, vreg, s) {
            if let Some(k) = reg_index(r) {
                if allowed(r) {
                    candidates.push(k);
                }
            }
        }
        for hint in &built.hints[vreg] {
            if !covers(&ranges, hint.pos) {
                continue;
            }
            let Some(r) = placed_reg(pieces, hint.partner, hint.partner_pos) else {
                continue;
            };
            let Some(k) = reg_index(r) else {
                continue;
            };
            if allowed(r) && !candidates.contains(&k) {
                candidates.push(k);
            }
        }
        for k in candidates {
            if free[k] >= e {
                full_hint = Some(k);
                break;
            }
        }

        let best = (0..pool.len())
            .filter(|&k| allowed(pool[k]))
            .max_by_key(|&k| (free[k], Reverse(k)));
        // A full-free hint beats every other choice.
        let best = full_hint.or(best);
        let mut assigned = false;
        if let Some(k) = best {
            let bf = free[k];
            if bf > s {
                if bf >= e {
                    pieces.children[cur].assign = Some(Assign::Reg(pool[k]));
                    assigned = true;
                } else {
                    // Free only up to bf. Split at the last gap at or before
                    // it: a conflict that starts mid-instruction is reached by
                    // a move at the Pre gap of that instruction.
                    let p = pos::gap_floor(bf);
                    if p > s {
                        if let Some(id) = pieces.split(cur, p) {
                            pieces.children[cur].assign = Some(Assign::Reg(pool[k]));
                            assigned = true;
                            unhandled.push(Reverse((pieces.children[id].ranges[0].0, id)));
                        }
                    }
                }
            }
        }

        if assigned {
            active.push(cur);
        } else {
            // Nothing is free. Before spilling this piece, look for an active
            // piece that is cheaper to spill: taking its register is what makes
            // the scan prefer splitting a long cold live range over evicting a
            // hot short one (Traub et al., and `RegAllocBasic::spillInterferences`
            // in LLVM). A piece that has already been evicted never evicts, so
            // eviction chains strictly shrink and always terminate.
            evicted.resize(pieces.children.len(), false);
            let cur_weight = weights.spill_weight(vreg, &ranges);
            let mut victim: Option<(u64, usize, usize)> = None;
            if evict && !evicted[cur] {
                for (k, r) in pool.iter().enumerate() {
                    let Some(a) = holder[k] else { continue };
                    // The register must stay free of fixed occupancy and of
                    // pieces live elsewhere for the whole range.
                    if !allowed(*r) || free_static[k] < e {
                        continue;
                    }
                    let child = &pieces.children[a];
                    let w = weights.spill_weight(child.vreg, &child.ranges);
                    // A split costs a move at each end, so only a clearly
                    // cheaper piece is worth displacing.
                    if w.saturating_mul(4) > cur_weight {
                        continue;
                    }
                    if victim.as_ref().map_or(true, |&(bw, _, _)| w < bw) {
                        victim = Some((w, a, k));
                    }
                }
            }
            let mut moved = false;
            if let Some((_, a, k)) = victim {
                let start = pieces.children[a].ranges[0].0;
                let requeued = if start < s {
                    // The victim keeps its register up to the last gap at
                    // or before `s`; the move out of it runs there.
                    let cut = pos::gap_floor(s);
                    if cut > start {
                        pieces.split(a, cut)
                    } else {
                        None
                    }
                } else {
                    // The piece begins here, so all of it is requeued and no
                    // boundary move is created.
                    pieces.children[a].assign = None;
                    Some(a)
                };
                if let Some(id) = requeued {
                    evicted.resize(id + 1, false);
                    evicted[id] = true;
                    active.retain(|&x| x != a);
                    pieces.children[cur].assign = Some(Assign::Reg(pool[k]));
                    active.push(cur);
                    unhandled.push(Reverse((pieces.children[id].ranges[0].0, id)));
                    moved = true;
                }
            }
            if !moved {
                // Spill up to the next fixed event after s, so a register can
                // be reloaded there.
                let mut idx = events.partition_point(|&x| x <= s);
                let mut q = e;
                while idx < events.len() {
                    let cut = pos::gap_floor(events[idx]);
                    if cut > s {
                        q = cut.min(e);
                        break;
                    }
                    idx += 1;
                }
                let right = if q < e { pieces.split(cur, q) } else { None };
                pieces.children[cur].assign = Some(Assign::Stack);
                if let Some(id) = right {
                    unhandled.push(Reverse((pieces.children[id].ranges[0].0, id)));
                }
            }
        }
    }
    Ok(())
}
