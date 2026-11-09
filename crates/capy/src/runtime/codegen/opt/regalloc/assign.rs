//! The allocator output.
//!
//! Every allocator is a function from [`Built`] to [`Assignment`]: it decides
//! the location of each piece, splitting pieces where a single location is not
//! enough. [`finish`] colors the spill slots and produces the result that
//! `resolve`, `stackmap` and the emitter consume.

use cranelift_entity::SecondaryMap;

use super::intervals::{Assign, Built, Child};
use super::liveness::Numbering;
use super::pos;
use super::slots;
use super::{Loc, PhysReg};
use crate::runtime::codegen::opt::lir::{LirFunc, VReg};

/// The placement of every piece, and the spill slot of every vreg.
pub(crate) struct Assignment {
    pub children: Vec<Child>,
    pub vreg_children: SecondaryMap<VReg, Vec<usize>>,
    pub spill_slots: u32,
    pub slot_of: SecondaryMap<VReg, Option<u32>>,
    pub coherence: Coherence,
    /// The representative of each coalesced affinity group (see greedy's
    /// `coalesce_tmps`): members of one group hold the same value wherever
    /// they are live together, so they may share a register.
    pub groups: SecondaryMap<VReg, Option<VReg>>,
}

impl Assignment {
    /// The location of a placed piece.
    pub(crate) fn loc_of_child(&self, child: &Child) -> Option<Loc> {
        match child.assign? {
            Assign::Reg(r) => Some(Loc::Reg(r)),
            Assign::Stack => self.slot_of[child.vreg].map(Loc::Slot),
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

/// The pieces an allocator is placing. Starts as a copy of the mandatory
/// pieces of `built` (fixed roles and forced slots) and grows as the allocator
/// splits.
pub(crate) struct Pieces {
    pub children: Vec<Child>,
    pub vreg_children: SecondaryMap<VReg, Vec<usize>>,
}

impl Pieces {
    pub(crate) fn new(built: &Built) -> Self {
        Self {
            children: built.children.clone(),
            vreg_children: built.vreg_children.clone(),
        }
    }

    /// The pieces of `built` with the unplaced pieces of each vreg merged
    /// into one, which is JSC's tmp: the whole vreg minus the holes that the
    /// pre-placed pieces (fixed registers, frame slots) make. The pre-placed
    /// pieces come first; the count is returned.
    pub(crate) fn merged(built: &Built) -> (Self, usize) {
        let mut children: Vec<Child> = Vec::new();
        let mut vreg_children: SecondaryMap<VReg, Vec<usize>> = SecondaryMap::new();
        for c in &built.children {
            if c.assign.is_some() {
                vreg_children[c.vreg].push(children.len());
                children.push(c.clone());
            }
        }
        let fixed = children.len();
        let mut unplaced: SecondaryMap<VReg, Vec<(u32, u32)>> = SecondaryMap::new();
        for c in &built.children {
            if c.assign.is_none() {
                unplaced[c.vreg].extend_from_slice(&c.ranges);
            }
        }
        for (vreg, ranges) in unplaced.iter_mut() {
            if ranges.is_empty() {
                continue;
            }
            ranges.sort_unstable();
            vreg_children[vreg].push(children.len());
            children.push(Child {
                vreg,
                ranges: std::mem::take(ranges),
                assign: None,
            });
        }
        (
            Self {
                children,
                vreg_children,
            },
            fixed,
        )
    }

    /// Repacks a colouring for greedy's second round: every placed piece
    /// first (the count is returned, with the new ids of the pieces in
    /// `colored`), then the unplaced pieces of each vreg merged into one.
    pub(crate) fn repack(self, colored: &[usize]) -> (Self, usize, Vec<usize>) {
        let mut children: Vec<Child> = Vec::new();
        let mut vreg_children: SecondaryMap<VReg, Vec<usize>> = SecondaryMap::new();
        let mut now_colored: Vec<usize> = Vec::new();
        let mut unplaced: SecondaryMap<VReg, Vec<(u32, u32)>> = SecondaryMap::new();
        for (id, c) in self.children.iter().enumerate() {
            if c.assign.is_some() {
                if colored.contains(&id) {
                    now_colored.push(children.len());
                }
                vreg_children[c.vreg].push(children.len());
                children.push(c.clone());
            } else {
                unplaced[c.vreg].extend_from_slice(&c.ranges);
            }
        }
        let fixed = children.len();
        for (vreg, ranges) in unplaced.iter_mut() {
            if ranges.is_empty() {
                continue;
            }
            vreg_children[vreg].push(children.len());
            children.push(Child {
                vreg,
                ranges: super::intervals::merge(std::mem::take(ranges)),
                assign: None,
            });
        }
        (
            Self {
                children,
                vreg_children,
            },
            fixed,
            now_colored,
        )
    }

    pub(crate) fn push(
        &mut self,
        vreg: VReg,
        ranges: Vec<(u32, u32)>,
        assign: Option<Assign>,
    ) -> usize {
        let id = self.children.len();
        self.children.push(Child {
            vreg,
            ranges,
            assign,
        });
        self.vreg_children[vreg].push(id);
        id
    }

    /// Splits child `id` at `p`, which must lie strictly inside one of its
    /// ranges. The child keeps the left part and a new piece takes the right
    /// part, so the two stay contiguous and `resolve` emits the move.
    ///
    /// Returns `None` when there is nothing to split off: `p` at or past the
    /// end of the piece, before its start, or inside a lifetime hole. A split
    /// in a hole would leave the two halves non-adjacent, and `resolve` only
    /// inserts a move between adjacent pieces, so the reload would be missing —
    /// the caller must spill the whole piece instead.
    pub(crate) fn split(&mut self, id: usize, p: u32) -> Option<usize> {
        let child = &mut self.children[id];
        let (left, right) = split_ranges(&child.ranges, p);
        assert!(!left.is_empty(), "a split before the start of a piece");
        if right.is_empty() || end_of(&left) != right[0].0 {
            return None;
        }
        child.ranges = left;
        let vreg = child.vreg;
        Some(self.push(vreg, right, None))
    }
}

impl Pieces {
    /// Removes `holes` (sorted, disjoint) from the ranges of child `id` and
    /// gives each non-empty intersection to a new unplaced piece of the same
    /// vreg. Returns whether the child keeps any range, and the new pieces.
    ///
    /// When the holes cover the whole child, the first hole's part stays in
    /// `id` and the child keeps nothing else, so a piece never has no range.
    /// Boundaries between the parts are the caller's to place on gaps. Two
    /// parts separated by a lifetime hole need no move: the later one starts
    /// at a definition or at a block start, where the edge moves read the
    /// location of the vreg directly.
    pub(crate) fn punch(&mut self, id: usize, holes: &[(u32, u32)]) -> (bool, Vec<usize>) {
        let vreg = self.children[id].vreg;
        let ranges = std::mem::take(&mut self.children[id].ranges);
        let mut rest = ranges.clone();
        let mut parts: Vec<Vec<(u32, u32)>> = Vec::new();
        for &(lo, hi) in holes {
            let part: Vec<(u32, u32)> = ranges
                .iter()
                .filter_map(|&(a, b)| {
                    let (a, b) = (a.max(lo), b.min(hi));
                    (a < b).then_some((a, b))
                })
                .collect();
            if part.is_empty() {
                continue;
            }
            rest = subtract_ranges(&rest, &[(lo, hi)]);
            parts.push(part);
        }
        let mut ids = Vec::with_capacity(parts.len());
        let keeps = !rest.is_empty();
        if keeps {
            self.children[id].ranges = rest;
        } else {
            self.children[id].ranges = parts.remove(0);
            ids.push(id);
        }
        for part in parts {
            ids.push(self.push(vreg, part, None));
        }
        (keeps, ids)
    }
}

impl Pieces {
    /// Moves the part of child `id` inside `holes` (sorted, disjoint) into one
    /// new unplaced piece of the same vreg, which may have several ranges.
    /// Returns the new piece, or `None` when the child would keep nothing or
    /// has nothing inside the holes.
    pub(crate) fn punch_one(&mut self, id: usize, holes: &[(u32, u32)]) -> Option<usize> {
        let vreg = self.children[id].vreg;
        let ranges = &self.children[id].ranges;
        let mut part: Vec<(u32, u32)> = Vec::new();
        for &(lo, hi) in holes {
            part.extend(ranges.iter().filter_map(|&(a, b)| {
                let (a, b) = (a.max(lo), b.min(hi));
                (a < b).then_some((a, b))
            }));
        }
        let rest = subtract_ranges(ranges, holes);
        if part.is_empty() || rest.is_empty() {
            return None;
        }
        self.children[id].ranges = rest;
        Some(self.push(vreg, part, None))
    }
}

/// `a` without the positions of `b`; both sorted and disjoint.
pub(crate) fn subtract_ranges(a: &[(u32, u32)], b: &[(u32, u32)]) -> Vec<(u32, u32)> {
    let mut out = Vec::new();
    for &(mut lo, hi) in a {
        for &(blo, bhi) in b {
            if bhi <= lo || blo >= hi {
                continue;
            }
            if blo > lo {
                out.push((lo, blo));
            }
            lo = lo.max(bhi);
            if lo >= hi {
                break;
            }
        }
        if lo < hi {
            out.push((lo, hi));
        }
    }
    out
}

/// Where a vreg's spill slot already holds its value, so that leaving a
/// register for the slot needs no store.
///
/// A vreg is defined once, so after the first store (or a definition straight
/// into the slot) the slot holds the value for the rest of the vreg's life.
/// LLVM's `InlineSpiller` relies on the same fact: it stores a single-def
/// value once and every later reload reads the slot. Whether the slot is
/// written on every path to a point is a forward must-analysis over the
/// blocks; whether a later piece still reads it is a backward one.
#[derive(Default)]
pub(crate) struct Coherence {
    /// Register-to-slot boundaries `(vreg, position)` inside a block that
    /// need no store.
    pub skip: std::collections::HashSet<(VReg, u32)>,
    /// Register-to-slot edge moves `(vreg, predecessor)` that need no store.
    pub skip_edge: std::collections::HashSet<(VReg, crate::runtime::codegen::opt::lir::LBlock)>,
    /// Positions where the slot holds the value and a later piece reads it,
    /// beyond the stack pieces themselves: the slot is reserved there.
    pub extents: SecondaryMap<VReg, Vec<(u32, u32)>>,
}

pub(crate) fn coherence(func: &LirFunc, num: &Numbering, children: &[Child]) -> Coherence {
    use crate::runtime::codegen::opt::lir::LBlock;
    use std::collections::HashMap;

    let mut out = Coherence::default();
    let mut segs: SecondaryMap<VReg, Vec<(u32, u32, bool)>> = SecondaryMap::new();
    for c in children {
        let Some(a) = c.assign else { continue };
        for &(lo, hi) in &c.ranges {
            segs[c.vreg].push((lo, hi, a == Assign::Stack));
        }
    }
    let mut index: SecondaryMap<LBlock, usize> = SecondaryMap::new();
    for (i, &b) in num.order.iter().enumerate() {
        index[b] = i;
    }
    let mut preds: SecondaryMap<LBlock, Vec<LBlock>> = SecondaryMap::new();
    let mut succs: SecondaryMap<LBlock, Vec<LBlock>> = SecondaryMap::new();
    for &b in &num.order {
        if let Some(term) = &func.blocks[b].term {
            for e in term.edges() {
                if !succs[b].contains(&e.block) {
                    succs[b].push(e.block);
                    preds[e.block].push(b);
                }
            }
        }
    }

    // Positions of the instructions that may move objects. A register that
    // lives across one is updated by the collector but the vreg's own slot is
    // not, so the slot is stale from there until it is stored again.
    let mut kills: Vec<u32> = Vec::new();
    for &b in &num.order {
        for (k, inst) in func.blocks[b].insts.iter().enumerate() {
            if inst.safepoint().is_some() {
                kills.push(pos::early(num.base[b] + k as u32));
            }
        }
    }
    kills.sort_unstable();

    struct Info {
        segs: Vec<(u32, u32, bool)>,
        first_stack: Option<u32>,
        last_stack_end: u32,
        start: u32,
        end: u32,
        entry_valid: bool,
        live_in: bool,
        need_out: bool,
    }

    /// Walks a block: whether the slot is valid at its end, the intervals
    /// where it is valid, and the register-to-slot boundaries that need no
    /// store.
    fn scan(info: &Info, kills: &[u32]) -> (bool, Vec<(u32, u32)>, Vec<u32>) {
        let mut valid = info.entry_valid;
        let mut from = info.start;
        let mut ivals: Vec<(u32, u32)> = Vec::new();
        let mut skips: Vec<u32> = Vec::new();
        let mut prev: Option<(u32, u32, bool)> = None;
        for &(lo, hi, stack) in &info.segs {
            // A segment that does not continue the previous one is a new
            // definition (a coalesced vreg is defined more than once): the
            // slot holds the old value until it is stored again.
            if let Some(p) = prev {
                if p.1 != lo && valid && !stack {
                    ivals.push((from, p.1));
                    valid = false;
                }
            }
            if let Some(p) = prev {
                if p.1 == lo && !p.2 && stack && valid {
                    skips.push(lo);
                }
            }
            if stack {
                if !valid {
                    valid = true;
                    from = lo;
                }
            } else if valid {
                // A safepoint inside the register run kills the slot.
                let i = kills.partition_point(|&k| k < lo);
                if i < kills.len() && kills[i] < hi {
                    ivals.push((from, kills[i]));
                    valid = false;
                }
            }
            prev = Some((lo, hi, stack));
        }
        if valid {
            ivals.push((from, info.end));
        }
        (valid, ivals, skips)
    }

    for v in func.vregs.keys() {
        if !segs[v].iter().any(|s| s.2) {
            continue;
        }
        let mut list = std::mem::take(&mut segs[v]);
        list.sort_unstable();
        // The segments of the vreg, split at block boundaries.
        let mut blocks: HashMap<LBlock, Info> = HashMap::new();
        for &(lo, hi, stack) in &list {
            let mut at = lo;
            while at < hi {
                let b = num.block_at[pos::inst_of(at) as usize];
                let e = hi.min(num.to(b));
                let info = blocks.entry(b).or_insert_with(|| Info {
                    segs: Vec::new(),
                    first_stack: None,
                    last_stack_end: 0,
                    start: u32::MAX,
                    end: 0,
                    entry_valid: true,
                    live_in: false,
                    need_out: false,
                });
                info.segs.push((at, e, stack));
                info.start = info.start.min(at);
                info.end = info.end.max(e);
                if stack {
                    info.first_stack = Some(info.first_stack.map_or(at, |f| f.min(at)));
                    info.last_stack_end = info.last_stack_end.max(e);
                }
                at = e;
            }
        }
        let mut order: Vec<LBlock> = blocks.keys().copied().collect();
        order.sort_by_key(|&b| index[b]);
        for &b in &order {
            let info = blocks.get_mut(&b).unwrap();
            info.live_in = info.start == num.from(b) && !func.blocks[b].params.contains(&v);
            info.entry_valid = info.live_in;
        }
        // Forward: the slot is written on every path to the block start.
        let mut changed = true;
        while changed {
            changed = false;
            for &b in &order {
                if !blocks[&b].live_in || !blocks[&b].entry_valid {
                    continue;
                }
                let ok = preds[b]
                    .iter()
                    .all(|p| blocks.get(p).is_some_and(|i| scan(i, &kills).0));
                if !ok {
                    blocks.get_mut(&b).unwrap().entry_valid = false;
                    changed = true;
                }
            }
        }
        // Backward: a later piece reads the slot.
        let mut changed = true;
        while changed {
            changed = false;
            for &b in order.iter().rev() {
                if blocks[&b].need_out {
                    continue;
                }
                let need = succs[b].iter().any(|s| {
                    blocks
                        .get(s)
                        // A loop-carried param passes itself on the back edge, so
                        // a segment that starts at the block counts, param or not.
                        .is_some_and(|i| {
                            i.start == num.from(*s) && (i.first_stack.is_some() || i.need_out)
                        })
                });
                if need {
                    blocks.get_mut(&b).unwrap().need_out = true;
                    changed = true;
                }
            }
        }
        for &b in &order {
            let info = &blocks[&b];
            let (exit_valid, ivals, skips) = scan(info, &kills);
            if exit_valid {
                out.skip_edge.insert((v, b));
            }
            let limit = if info.need_out {
                info.end
            } else {
                info.last_stack_end
            };
            for (lo, hi) in ivals {
                let hi = hi.min(limit);
                if lo < hi {
                    out.extents[v].push((lo, hi));
                }
            }
            for p in skips {
                out.skip.insert((v, p));
            }
        }
    }
    out
}

/// Colors the spill slots of `pieces` and returns the assignment.
pub(crate) fn finish(func: &LirFunc, num: &Numbering, pieces: Pieces) -> Assignment {
    let coherence = coherence(func, num, &pieces.children);
    let (spill_slots, slot_of) = slots::assign_slots(func, num, &pieces.children, &coherence);
    Assignment {
        coherence,
        groups: SecondaryMap::new(),
        children: pieces.children,
        vreg_children: pieces.vreg_children,
        spill_slots,
        slot_of,
    }
}

/// Whether any of `ranges` covers `pos`.
pub(crate) fn covers(ranges: &[(u32, u32)], pos: u32) -> bool {
    ranges.iter().any(|&(lo, hi)| lo <= pos && pos < hi)
}

/// One past the last position of `ranges`.
pub(crate) fn end_of(ranges: &[(u32, u32)]) -> u32 {
    ranges.last().map_or(0, |r| r.1)
}

/// Whether any range of `a` intersects any range of `b`.
///
/// Unlike [`next_intersection`], neither list has to be sorted: register
/// occupancy is built by appending each placed piece, so it is in allocation
/// order rather than position order.
pub(crate) fn intersects(a: &[(u32, u32)], b: &[(u32, u32)]) -> bool {
    a.iter()
        .any(|&(alo, ahi)| b.iter().any(|&(blo, bhi)| alo < bhi && blo < ahi))
}

/// The first position `>= from` where `a` and `b` both cover, ignoring order.
pub(crate) fn first_conflict(a: &[(u32, u32)], b: &[(u32, u32)], from: u32) -> Option<u32> {
    let mut best: Option<u32> = None;
    for &(alo, ahi) in a {
        for &(blo, bhi) in b {
            let lo = alo.max(blo).max(from);
            let hi = ahi.min(bhi);
            if lo < hi {
                best = Some(best.map_or(lo, |b: u32| b.min(lo)));
            }
        }
    }
    best
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
pub(crate) fn split_ranges(ranges: &[(u32, u32)], p: u32) -> (Vec<(u32, u32)>, Vec<(u32, u32)>) {
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

/// The register a placed piece of `vreg` occupies at `pos`.
pub(crate) fn placed_reg(pieces: &Pieces, vreg: VReg, pos: u32) -> Option<PhysReg> {
    for &id in &pieces.vreg_children[vreg] {
        let child = &pieces.children[id];
        if let Some(Assign::Reg(r)) = child.assign {
            if covers(&child.ranges, pos) {
                return Some(r);
            }
        }
    }
    None
}

/// The register of the piece of `vreg` that ends exactly at `pos`, if any.
///
/// A piece split at a call or constraint boundary prefers to resume in the
/// register it had before the boundary, which keeps a loop-carried value in one
/// register instead of rotating it on every split.
pub(crate) fn placed_reg_before(pieces: &Pieces, vreg: VReg, pos: u32) -> Option<PhysReg> {
    for &id in &pieces.vreg_children[vreg] {
        let child = &pieces.children[id];
        if let Some(Assign::Reg(r)) = child.assign {
            if end_of(&child.ranges) == pos {
                return Some(r);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use cranelift_entity::{EntityRef, SecondaryMap};

    use super::super::intervals::Child;
    use super::super::weights::live_len;
    use super::{
        Pieces, covers, end_of, first_conflict, intersects, next_intersection, split_ranges,
    };
    use crate::runtime::codegen::opt::lir::VReg;

    fn pieces(ranges: Vec<(u32, u32)>) -> Pieces {
        let vreg = VReg::new(0);
        let mut vreg_children: SecondaryMap<VReg, Vec<usize>> = SecondaryMap::new();
        vreg_children[vreg] = vec![0];
        Pieces {
            children: vec![Child {
                vreg,
                ranges,
                assign: None,
            }],
            vreg_children,
        }
    }

    #[test]
    fn split_inside_a_range_keeps_the_halves_adjacent() {
        let mut p = pieces(vec![(2, 10)]);
        let right = p.split(0, 5).expect("an interior split");
        assert_eq!(p.children[0].ranges, vec![(2, 5)]);
        assert_eq!(p.children[right].ranges, vec![(5, 10)]);
        assert_eq!(end_of(&p.children[0].ranges), p.children[right].ranges[0].0);
        assert_eq!(p.vreg_children[VReg::new(0)], vec![0, right]);
    }

    #[test]
    fn split_in_a_hole_is_refused() {
        // Splitting between two live segments would leave a gap, and `resolve`
        // only emits a move between adjacent pieces.
        let mut p = pieces(vec![(0, 4), (8, 12)]);
        assert_eq!(p.split(0, 6), None);
        assert_eq!(p.children.len(), 1);
        assert_eq!(p.children[0].ranges, vec![(0, 4), (8, 12)]);
    }

    #[test]
    fn split_at_the_end_is_refused() {
        let mut p = pieces(vec![(2, 10)]);
        assert_eq!(p.split(0, 10), None);
    }

    #[test]
    fn split_ranges_cuts_a_straddling_range() {
        let (left, right) = split_ranges(&[(0, 4), (8, 12)], 10);
        assert_eq!(left, vec![(0, 4), (8, 10)]);
        assert_eq!(right, vec![(10, 12)]);
    }

    #[test]
    fn holes_are_not_live() {
        let ranges = [(0, 4), (8, 12)];
        assert!(covers(&ranges, 0));
        assert!(!covers(&ranges, 4));
        assert!(!covers(&ranges, 6));
        assert!(covers(&ranges, 8));
        assert!(!covers(&ranges, 12));
        assert_eq!(live_len(&ranges), 8);
    }

    #[test]
    fn order_independent_queries_handle_unsorted_occupancy() {
        // Register occupancy is appended in allocation order, so it is not
        // sorted. `next_intersection` would miss these conflicts.
        let occ = [(20, 30), (0, 5)];
        let ranges = [(4, 6)];
        assert!(intersects(&occ, &ranges));
        assert_eq!(first_conflict(&occ, &ranges, 0), Some(4));
        assert!(!intersects(&occ, &[(10, 15)]));
        assert_eq!(first_conflict(&occ, &[(10, 15)], 0), None);
    }

    #[test]
    fn intersection_skips_holes() {
        let a = [(0, 4), (8, 12)];
        let b = [(2, 10)];
        assert_eq!(next_intersection(&a, &b, 0), Some(2));
        assert_eq!(next_intersection(&a, &[(9, 10)], 0), Some(9));
        assert_eq!(next_intersection(&[(0, 2)], &[(4, 6)], 0), None);
    }
}
