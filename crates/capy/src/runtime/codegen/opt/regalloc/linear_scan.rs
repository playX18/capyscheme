//! Linear scan

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use cranelift_entity::SecondaryMap;

use super::intervals::{Assign, Built, Child};
use super::liveness::Numbering;
use super::{Loc, PhysReg, RegFile};
use crate::runtime::codegen::opt::lir::{LirFunc, RegClass, VReg};

/// The placement of every piece, and the spill slot of every vreg.
pub(crate) struct Assignment {
    pub children: Vec<Child>,
    pub vreg_children: SecondaryMap<VReg, Vec<usize>>,
    pub spill_slots: u32,
    pub slot_of: SecondaryMap<VReg, Option<u32>>,
}

pub(crate) fn covers(ranges: &[(u32, u32)], pos: u32) -> bool {
    ranges.iter().any(|&(lo, hi)| lo <= pos && pos < hi)
}

pub(crate) fn end_of(ranges: &[(u32, u32)]) -> u32 {
    ranges.last().map_or(0, |r| r.1)
}

impl Assignment {
    /// The location of a placed piece.
    pub(crate) fn loc_of_child(&self, child: &Child) -> Option<Loc> {
        match child.assign? {
            Assign::Reg(r) => Some(Loc::Reg(r)),
            Assign::Stack => match self.slot_of[child.vreg] {
                Some(s) => Some(Loc::Slot(s)),
                None => None,
            },
        }
    }

    /// The location of `vreg` at `pos`, if it is live there.
    pub(crate) fn loc_at(&self, vreg: VReg, pos: u32) -> Option<Loc> {
        for &id in &self.vreg_children[vreg] {
            let child = &self.children[id];
            if covers(&child.ranges, pos) {
                return self.loc_of_child(child);
            }
        }
        None
    }
}

/// The first position `>= from` where both range lists cover a position.
pub(crate) fn next_intersection(a: &[(u32, u32)], b: &[(u32, u32)], from: u32) -> Option<u32> {
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        let lo = a[i].0.max(b[j].0).max(from);
        let hi = a[i].1.min(b[j].1);
        if lo < hi {
            return Some(lo);
        }
        if a[i].1 < b[j].1 {
            i += 1;
        } else {
            j += 1;
        }
    }
    None
}

/// Splits sorted ranges at `p`. Ranges that straddle `p` are cut in two.
fn split(ranges: &[(u32, u32)], p: u32) -> (Vec<(u32, u32)>, Vec<(u32, u32)>) {
    let mut left = Vec::new();
    let mut right = Vec::new();
    for &(lo, hi) in ranges {
        if hi <= p {
            left.push((lo, hi));
        } else if lo >= p {
            right.push((lo, hi));
        } else {
            left.push((lo, p));
            right.push((p, hi));
        }
    }
    (left, right)
}

/// Runs linear scan and assigns spill slots.
pub(crate) fn run(
    func: &LirFunc,
    num: &Numbering,
    mut built: Built,
    regs: &RegFile,
) -> Result<Assignment, String> {
    let mut events: Vec<u32> = built
        .fixed
        .values()
        .flatten()
        .flat_map(|&(lo, hi)| [lo, hi])
        .collect();
    events.sort_unstable();
    events.dedup();

    scan_class(func, &mut built, RegClass::Gpr, regs, &events)?;
    scan_class(func, &mut built, RegClass::Fpr, regs, &events)?;

    if let Some(c) = built.children.iter().find(|c| c.assign.is_none()) {
        return Err(format!("a piece of {} was left unplaced", c.vreg));
    }
    let (spill_slots, slot_of) = assign_slots(func, num, &built);
    Ok(Assignment {
        children: built.children,
        vreg_children: built.vreg_children,
        spill_slots,
        slot_of,
    })
}

/// The register a placed piece of `vreg` occupies at `pos`.
fn placed_reg(built: &Built, vreg: VReg, pos: u32) -> Option<PhysReg> {
    for &id in &built.vreg_children[vreg] {
        let child = &built.children[id];
        if let Some(Assign::Reg(r)) = child.assign {
            if covers(&child.ranges, pos) {
                return Some(r);
            }
        }
    }
    None
}

/// The register of the piece of `vreg` that ends exactly at `pos`, if any.
/// A piece split at a call or constraint boundary prefers to resume in the
/// register it had before the boundary, which keeps a loop-carried value in
/// one register instead of rotating it on every split.
fn placed_reg_before(built: &Built, vreg: VReg, pos: u32) -> Option<PhysReg> {
    for &id in &built.vreg_children[vreg] {
        let child = &built.children[id];
        if let Some(Assign::Reg(r)) = child.assign {
            if end_of(&child.ranges) == pos {
                return Some(r);
            }
        }
    }
    None
}

fn scan_class(
    func: &LirFunc,
    built: &mut Built,
    class: RegClass,
    regs: &RegFile,
    events: &[u32],
) -> Result<(), String> {
    let pool = regs.pool(class);

    let mut unhandled = BinaryHeap::new();
    for (id, c) in built.children.iter().enumerate() {
        if c.assign.is_none() && func.class(c.vreg) == class {
            unhandled.push(Reverse((c.ranges[0].0, id)));
        }
    }

    let mut active: Vec<usize> = Vec::new();
    let mut inactive: Vec<usize> = Vec::new();
    while let Some(Reverse((s, cur))) = unhandled.pop() {
        // Move pieces between the active and inactive lists for position s.
        let mut kept = Vec::with_capacity(active.len());
        for a in std::mem::take(&mut active) {
            let c = &built.children[a];
            if covers(&c.ranges, s) {
                kept.push(a);
            } else if end_of(&c.ranges) > s {
                inactive.push(a);
            }
        }
        active = kept;
        let mut kept = Vec::new();
        for i in std::mem::take(&mut inactive) {
            let c = &built.children[i];
            if covers(&c.ranges, s) {
                active.push(i);
            } else if end_of(&c.ranges) > s {
                kept.push(i);
            }
        }
        inactive = kept;

        let mut ranges = built.children[cur].ranges.clone();
        let mut e = end_of(&ranges);
        // A piece that starts where a call ends stays in its slot until the
        // first instruction that reads it, so a value that the next call
        // needs in its slot anyway is not reloaded here. With the regalloc
        // flag on, only a piece that actually arrives in a slot (its previous
        // piece was spilled) does this: a fresh definition at a boundary, such
        // as a call result, is allocated normally instead of being stored and
        // immediately reloaded.
        let at_event = events.binary_search(&s).is_ok();
        let arrives_in_slot = at_event
            && built.vreg_children[built.children[cur].vreg]
                .iter()
                .any(|&id| {
                    let c = &built.children[id];
                    c.assign == Some(Assign::Stack) && end_of(&c.ranges) == s
                });
        if at_event && arrives_in_slot {
            let vreg = built.children[cur].vreg;
            let first = built.uses[vreg]
                .iter()
                .copied()
                .find(|&u| u >= s && covers(&ranges, u));
            let cut = first.unwrap_or(e);
            if cut > s {
                let (left, right) = split(&ranges, cut);
                if !left.is_empty() {
                    built.children[cur].ranges = left;
                    built.children[cur].assign = Some(Assign::Stack);
                    if !right.is_empty() {
                        let id = built.children.len();
                        built.children.push(Child {
                            vreg,
                            ranges: right.clone(),
                            assign: None,
                        });
                        built.vreg_children[vreg].push(id);
                        unhandled.push(Reverse((right[0].0, id)));
                    }
                    continue;
                }
            }
            ranges = built.children[cur].ranges.clone();
            e = end_of(&ranges);
        }
        let reg_index = |r: PhysReg| pool.iter().position(|&p| p == r);
        let mut free = vec![u32::MAX; pool.len()];
        for &a in &active {
            if let Some(Assign::Reg(r)) = built.children[a].assign {
                if let Some(k) = reg_index(r) {
                    free[k] = 0;
                }
            }
        }
        for &i in &inactive {
            if let Some(Assign::Reg(r)) = built.children[i].assign {
                if let Some(k) = reg_index(r) {
                    if let Some(p) = next_intersection(&built.children[i].ranges, &ranges, s) {
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

        let vreg = built.children[cur].vreg;

        // A frame value of a non-collecting call must be intact after the call
        // for the deopt checked there, so a piece covering one is confined to
        // the call-preserved registers, or spills.
        let call_safe = built.call_safe[vreg].iter().any(|&p| covers(&ranges, p));
        let allowed = |r: PhysReg| {
            if call_safe {
                regs.call_preserved(r)
            } else {
                !regs.call_preserved(r)
            }
        };

        // Coalescing: take the copy partner's register when it stays free for
        // the whole piece, so the edge or `Move` disappears. A piece split at
        // a boundary also resumes in its own previous register when possible.
        let mut full_hint = None;

        let mut candidates: Vec<usize> = Vec::new();
        if let Some(r) = placed_reg_before(built, vreg, s) {
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
            let Some(r) = placed_reg(built, hint.partner, hint.partner_pos) else {
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
                    built.children[cur].assign = Some(Assign::Reg(pool[k]));
                    assigned = true;
                } else {
                    // Free only up to bf. Split at the even position below it.
                    let p = bf & !1;
                    if p > s {
                        let (left, right) = split(&ranges, p);
                        built.children[cur].ranges = left;
                        built.children[cur].assign = Some(Assign::Reg(pool[k]));
                        assigned = true;
                        if !right.is_empty() {
                            let vreg = built.children[cur].vreg;
                            let id = built.children.len();
                            built.children.push(Child {
                                vreg,
                                ranges: right.clone(),
                                assign: None,
                            });
                            built.vreg_children[vreg].push(id);
                            unhandled.push(Reverse((right[0].0, id)));
                        }
                    }
                }
            }
        }

        if assigned {
            active.push(cur);
        } else {
            // Spill up to the next fixed event after s, so a register can be
            // reloaded there.
            let idx = events.partition_point(|&x| x <= s);
            let q = if idx < events.len() {
                events[idx].min(e)
            } else {
                e
            };
            let (left, right) = split(&ranges, q);
            built.children[cur].ranges = left;
            built.children[cur].assign = Some(Assign::Stack);
            if !right.is_empty() {
                let vreg = built.children[cur].vreg;
                let id = built.children.len();
                built.children.push(Child {
                    vreg,
                    ranges: right.clone(),
                    assign: None,
                });
                built.vreg_children[vreg].push(id);
                unhandled.push(Reverse((right[0].0, id)));
            }
        }
    }
    Ok(())
}

/// Gives every vreg with stack pieces one spill slot. The slot is reused by
/// vregs whose hulls do not overlap.
///
/// A vreg also prefers the slot of a copy
/// partner when the two hulls do not overlap. A block-parameter edge or a
/// `Move` whose halves are both spilled then needs no slot-to-slot copy at
/// all: the value is already in the shared slot.
fn assign_slots(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
) -> (u32, SecondaryMap<VReg, Option<u32>>) {
    // graph coloring for slots is better

    return super::slots::assign_slots(func, num, built);
    /*
    let mut hull: SecondaryMap<VReg, Option<(u32, u32)>> = SecondaryMap::new();
    for c in &built.children {
        if c.assign == Some(Assign::Stack) {
            let lo = c.ranges[0].0;
            let hi = end_of(&c.ranges);
            let h = &mut hull[c.vreg];
            *h = Some(match *h {
                None => (lo, hi),
                Some((a, b)) => (a.min(lo), b.max(hi)),
            });
        }
    }
    let coalesce = opt().regalloc && opt().regalloc_coalesce;
    // The copy partners whose two halves are both in stack pieces.
    let mut prefs: SecondaryMap<VReg, Vec<VReg>> = SecondaryMap::new();
    if coalesce {
        let stack_at = |v: VReg, pos: u32| {
            built.vreg_children[v].iter().any(|&id| {
                let c = &built.children[id];
                c.assign == Some(Assign::Stack) && covers(&c.ranges, pos)
            })
        };
        for v in func.vregs.keys() {
            for hint in &built.hints[v] {
                if stack_at(v, hint.pos) && stack_at(hint.partner, hint.partner_pos) {
                    prefs[v].push(hint.partner);
                }
            }
        }
    }
    let mut list: Vec<(u32, u32, VReg)> = func
        .vregs
        .keys()
        .filter_map(|v| hull[v].map(|(a, b)| (a, b, v)))
        .collect();
    list.sort();
    let mut ends: Vec<u32> = Vec::new();
    let mut slot_of: SecondaryMap<VReg, Option<u32>> = SecondaryMap::new();
    for (lo, hi, v) in list {
        let mut preferred = None;
        for &p in &prefs[v] {
            if let Some(s) = slot_of[p] {
                if ends[s as usize] <= lo {
                    preferred = Some(s as usize);
                    break;
                }
            }
        }
        if let Some(i) = preferred {
            ends[i] = hi;
            slot_of[v] = Some(i as u32);
            continue;
        }
        match ends.iter().position(|&e| e <= lo) {
            Some(i) => {
                ends[i] = hi;
                slot_of[v] = Some(i as u32);
            }
            None => {
                ends.push(hi);
                slot_of[v] = Some((ends.len() - 1) as u32);
            }
        }
    }
    (ends.len() as u32, slot_of)*/
}
