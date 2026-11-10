//! Greedy allocation, based on WebKit's `AirAllocateRegistersByGreedy.cpp` and LLVM's Greedy allocator.

use super::assign::{
    Assignment, Pieces, covers, finish, placed_reg, placed_reg_before, subtract_ranges,
};
use crate::runtime::codegen::opt::lir::{LirInst, Role};
use cranelift_entity::EntityRef;
use std::collections::{BTreeMap, BinaryHeap, HashMap, HashSet};

use super::intervals::{Assign, Built};
use super::liveness::Numbering;
use super::loops::Loops;
use super::pos;
use super::weights::{Weights, live_len};
use super::{PhysReg, RegFile};
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, RegClass, VReg};

/// The splitting knobs, `(by region, around loops)`. Read once: every
/// allocation builds a fresh `Greedy`, and the environment does not change
/// during a run.
fn split_knobs() -> (bool, bool) {
    static KNOBS: std::sync::OnceLock<(bool, bool)> = std::sync::OnceLock::new();
    *KNOBS.get_or_init(|| {
        (
            std::env::var("CAPY_GREEDY_REGION").map_or(true, |v| v != "0"),
            std::env::var("CAPY_GREEDY_LOOPS").is_ok_and(|v| v != "0"),
        )
    })
}

const SPILL_COST_SIZE_BIAS: f64 = 100_000.0;
const SPLIT_MIN_RANGE_SIZE: u64 = 4 * pos::POINTS as u64;
const SPLIT_MULTIPLIER: f64 = 2.0;
const MAX_LOOP_SPLIT_DEPTH: u32 = 32;
const ZERO_COST_EPSILON: f64 = 1e-6;
const LOOP_SPLIT_MAX_LOOP_FRACTION: f64 = 0.75;
const MAX_SPILLABLE_SPILL_COST: f64 = f32::MAX as f64;
const FIXED: usize = usize::MAX;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Stage {
    New,
    TryAllocate,
    TrySplit,
    Spill,
    Assigned,
    Spilled,
    Replaced,
}

/// Intervals allocated to one register.
#[derive(Default)]
struct RegisterRange {
    map: BTreeMap<u32, (u32, usize)>,
}

impl RegisterRange {
    fn add(&mut self, id: usize, ranges: &[(u32, u32)]) {
        for &(lo, hi) in ranges {
            self.map.insert(lo, (hi, id));
        }
    }

    fn evict(&mut self, ranges: &[(u32, u32)]) {
        for &(lo, _) in ranges {
            self.map.remove(&lo);
        }
    }

    fn has_conflict(&self, ranges: &[(u32, u32)]) -> bool {
        ranges
            .iter()
            .any(|&(lo, hi)| self.overlaps(lo, hi).next().is_some())
    }

    /// Calls `func` with each allocated interval `(lo, hi, owner)` that
    /// overlaps `ranges`
    fn for_each_conflict(
        &self,
        ranges: &[(u32, u32)],
        mut func: impl FnMut(u32, u32, usize) -> bool,
    ) {
        for &(lo, hi) in ranges {
            for (a, b, owner) in self.overlaps(lo, hi) {
                if func(a, b, owner) {
                    return;
                }
            }
        }
    }

    fn overlaps(&self, lo: u32, hi: u32) -> impl Iterator<Item = (u32, u32, usize)> + '_ {
        let before = self.map.range(..lo).next_back().filter(|(_, v)| v.0 > lo);
        before
            .into_iter()
            .chain(self.map.range(lo..hi))
            .map(|(&s, &(e, o))| (s, e, o))
    }
}

struct TmpData {
    stage: Stage,
    use_def_cost: f64,
    cumulative_fixup_cost: f64,
    split_around_clobbers_metadata_index: usize,
    /// The tmp crosses frame-slot holes and splitting around them did not
    /// pay (see [`Greedy::init_unit`]).
    crosses_holes: bool,
    /// An affinity group: the member units, which share this unit's
    /// register
    members: Vec<usize>,
    /// The unit is a group representative, not a piece of any vreg.
    is_group: bool,
    /// pieces evicted in one chain share
    /// a number, and a piece can only evict pieces with a smaller one.
    cascade: u32,
    /// the piece came out of a region split and is not
    /// region-split again.
    region_done: bool,
}

struct Greedy<'a> {
    func: &'a LirFunc,
    num: &'a Numbering,
    built: &'a Built,
    regs: &'a RegFile,
    weights: &'a Weights,
    pieces: Pieces,
    /// The pieces below this index are pre-placed.
    num_fixed: usize,
    map: Vec<TmpData>,
    reg_ranges: HashMap<PhysReg, RegisterRange>,
    next_cascade: u32,
    /// [`Self::try_region_split`]; `CAPY_GREEDY_REGION=0` turns it off. It
    /// adds a few allocator moves but removes more slot operands
    split_by_region: bool,
    queue: BinaryHeap<(u8, bool, bool, u64, std::cmp::Reverse<usize>)>,
    /// the gap tmps of each split.
    around_clobbers_metadata: Vec<Vec<usize>>,
    /// Natural loops of LIR, built on first use.
    loops: Option<Loops>,
    /// Split around loops, off by default.
    split_around_loops: bool,
}

pub(crate) fn run(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
    regs: &RegFile,
    weights: &Weights,
) -> Result<Assignment, String> {
    let (pieces, num_fixed) = Pieces::merged(built);
    solve(
        func,
        num,
        built,
        regs,
        weights,
        pieces,
        num_fixed,
        &[],
        true,
    )
}

pub(crate) fn refine(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
    regs: &RegFile,
    weights: &Weights,
    pieces: Pieces,
    colored: &[usize],
) -> Result<Assignment, String> {
    let (pieces, num_fixed, colored) = pieces.repack(colored);
    solve(
        func, num, built, regs, weights, pieces, num_fixed, &colored, false,
    )
}

fn solve(
    func: &LirFunc,
    num: &Numbering,
    built: &Built,
    regs: &RegFile,
    weights: &Weights,
    pieces: Pieces,
    num_fixed: usize,
    colored: &[usize],
    coalesce: bool,
) -> Result<Assignment, String> {
    let mut reg_ranges: HashMap<PhysReg, RegisterRange> = HashMap::new();

    for (&r, ranges) in &built.fixed {
        reg_ranges.entry(r).or_default().add(FIXED, ranges);
    }

    for &id in colored {
        if let Some(Assign::Reg(r)) = pieces.children[id].assign {
            reg_ranges
                .entry(r)
                .or_default()
                .add(FIXED, &pieces.children[id].ranges);
        }
    }

    let mut greedy = Greedy {
        func,
        num,
        built,
        regs,
        weights,
        pieces,
        num_fixed,
        map: Vec::new(),
        reg_ranges,
        next_cascade: 1,
        split_by_region: split_knobs().0,
        queue: BinaryHeap::new(),
        around_clobbers_metadata: vec![Vec::new()],
        loops: None,
        split_around_loops: split_knobs().1,
    };

    for id in 0..greedy.pieces.children.len() {
        greedy.sync_map(id);
    }

    for id in 0..greedy.pieces.children.len() {
        if greedy.pieces.children[id].assign.is_none() {
            let cost = greedy.initial_cost(id);
            greedy.map[id].use_def_cost = cost;
        }
    }

    if coalesce {
        greedy.coalesce_tmps();
    }

    for id in 0..greedy.pieces.children.len() {
        if greedy.pieces.children[id].assign.is_none() && greedy.map[id].stage == Stage::New {
            greedy.init_unit(id);
        }
    }

    greedy.allocate_registers();

    let Greedy {
        mut pieces, map, ..
    } = greedy;

    // The groups that stayed whole.
    let mut groups: Vec<(VReg, VReg)> = Vec::new();

    for d in map.iter() {
        if d.is_group && !d.members.is_empty() {
            let rep = pieces.children[d.members[0]].vreg;
            groups.extend(d.members.iter().map(|&m| (pieces.children[m].vreg, rep)));
        }
    }

    let mut new_id: Vec<usize> = Vec::with_capacity(pieces.children.len());
    let mut kept: Vec<super::intervals::Child> = Vec::new();

    for (id, c) in std::mem::take(&mut pieces.children).into_iter().enumerate() {
        new_id.push(kept.len());
        if !map.get(id).is_some_and(|d| d.is_group) {
            kept.push(c);
        }
    }

    pieces.children = kept;

    for (_, ids) in pieces.vreg_children.iter_mut() {
        for id in ids.iter_mut() {
            *id = new_id[*id];
        }
    }

    if let Some(c) = pieces.children.iter().find(|c| c.assign.is_none()) {
        return Err(format!("a piece of {} was left unplaced", c.vreg));
    }

    let mut asg = finish(func, num, pieces);

    for (v, rep) in groups {
        asg.groups[v] = Some(rep);
    }
    Ok(asg)
}

impl Greedy<'_> {
    fn sync_map(&mut self, id: usize) {
        while self.map.len() <= id {
            self.map.push(TmpData {
                stage: Stage::New,
                use_def_cost: 0.0,
                cumulative_fixup_cost: 0.0,
                split_around_clobbers_metadata_index: 0,
                crosses_holes: false,
                members: Vec::new(),
                is_group: false,
                cascade: 0,
                region_done: false,
            });
        }
    }

    fn init_unit(&mut self, id: usize) {
        let owners: Vec<usize> = if self.map[id].members.is_empty() {
            vec![id]
        } else {
            self.map[id].members.clone()
        };
        let mut split_cost = 0.0f64;
        let mut holes = false;
        for owner in owners {
            let vreg = self.pieces.children[owner].vreg;
            for &s in &self.pieces.vreg_children[vreg] {
                if s >= self.num_fixed || self.pieces.children[s].assign != Some(Assign::Stack) {
                    continue;
                }
                for &(lo, hi) in &self.pieces.children[s].ranges {
                    let before = self.pieces.children[id].ranges.iter().any(|r| r.1 == lo);
                    let after = self.pieces.children[id].ranges.iter().any(|r| r.0 == hi);
                    let moves = u32::from(before) + u32::from(after);
                    if moves > 0 {
                        holes = true;
                        split_cost += f64::from(moves) * self.block_frequency(lo);
                    }
                }
            }
        }
        if !holes {
            self.set_stage_and_enqueue(id, Stage::TryAllocate);
        } else if split_cost < self.map[id].use_def_cost / SPLIT_MULTIPLIER {
            // Already split around the clobbers.
            self.map[id].split_around_clobbers_metadata_index = self.around_clobbers_metadata.len();
            self.around_clobbers_metadata.push(Vec::new());
            self.set_stage_and_enqueue(id, Stage::TryAllocate);
        } else {
            self.map[id].crosses_holes = true;
            self.set_stage_and_enqueue(id, Stage::TrySplit);
        }
    }

    /// the block-frequency-weighted count of uses and defs.
    fn initial_cost(&self, id: usize) -> f64 {
        let c = &self.pieces.children[id];
        self.weights.piece_cost(c.vreg, &c.ranges) as f64
    }

    fn live_size(&self, id: usize) -> u64 {
        live_len(&self.pieces.children[id].ranges)
    }

    fn spill_cost(&self, id: usize) -> f64 {
        let cost = self.map[id].use_def_cost / (self.live_size(id) as f64 + SPILL_COST_SIZE_BIAS);
        cost.min(MAX_SPILLABLE_SPILL_COST)
    }

    fn block_frequency(&self, p: u32) -> f64 {
        self.weights.boundary_cost(p) as f64
    }

    fn class_of(&self, id: usize) -> RegClass {
        self.func.class(self.pieces.children[id].vreg)
    }

    fn is_live_range_block_local(&self, id: usize) -> bool {
        let ranges = &self.pieces.children[id].ranges;
        let first = self.num.block_at[pos::inst_of(ranges[0].0) as usize];
        let last = ranges.last().unwrap().1 - 1;
        last < self.num.to(first)
    }

    fn set_stage_and_enqueue(&mut self, id: usize, stage: Stage) {
        self.sync_map(id);
        self.map[id].stage = stage;
        let ranges = &self.pieces.children[id].ranges;
        let mut is_local = false;
        let mut range_size_or_start = live_len(ranges);
        if ranges.len() == 1 {
            let (lo, hi) = ranges[0];
            let block = self.num.block_at[pos::inst_of(lo) as usize];
            if hi - 1 < self.num.to(block) {
                is_local = true;
                range_size_or_start = u64::from(u32::MAX - lo);
            }
        }
        let vreg = self.pieces.children[id].vreg;
        let maybe_coalescable = self.built.hints[vreg]
            .iter()
            .any(|h| ranges.iter().any(|&(lo, hi)| lo <= h.pos && h.pos < hi));

        // Earlier stages are higher priority.
        let stage_rank = u8::MAX - stage as u8;
        self.queue.push((
            stage_rank,
            maybe_coalescable,
            !is_local,
            range_size_or_start,
            std::cmp::Reverse(id),
        ));
    }

    /// The registers `id` may take. A piece that covers a frame value of a
    /// non-collecting call must be in a call-preserved register.
    fn allowed(&self, id: usize, r: PhysReg) -> bool {
        let owners: &[usize] = if self.map[id].members.is_empty() {
            std::slice::from_ref(&id)
        } else {
            &self.map[id].members
        };
        let call_safe = owners.iter().any(|&m| {
            let c = &self.pieces.children[m];
            self.built.call_safe[c.vreg]
                .iter()
                .any(|&p| c.ranges.iter().any(|&(lo, hi)| lo <= p && p < hi))
        });
        !call_safe || self.regs.call_preserved(r)
    }

    fn registers(&self, id: usize) -> Vec<PhysReg> {
        self.regs
            .pool(self.class_of(id))
            .iter()
            .copied()
            .filter(|&r| self.allowed(id, r))
            .collect()
    }

    /// Preferred register where the neighbours of the piece already are. A
    /// piece resumes in the register it had before a split boundary, and
    /// takes a copy partner's register
    fn preferred(&self, id: usize) -> Vec<PhysReg> {
        if !self.map[id].members.is_empty() {
            let mut out: Vec<PhysReg> = Vec::new();
            for &m in &self.map[id].members {
                for r in self.preferred(m) {
                    if self.allowed(id, r) && !out.contains(&r) {
                        out.push(r);
                    }
                }
            }
            return out;
        }

        let c = &self.pieces.children[id];
        let mut out: Vec<PhysReg> = Vec::new();

        for &(lo, _) in &c.ranges {
            if let Some(r) = placed_reg_before(&self.pieces, c.vreg, lo) {
                out.push(r);
            }
        }

        for hint in &self.built.hints[c.vreg] {
            if !c
                .ranges
                .iter()
                .any(|&(lo, hi)| lo <= hint.pos && hint.pos < hi)
            {
                continue;
            }
            if let Some(r) = placed_reg(&self.pieces, hint.partner, hint.partner_pos) {
                out.push(r);
            }
        }

        let mut fixed_regs: Vec<(f64, PhysReg)> = Vec::new();
        for &s in &self.pieces.vreg_children[c.vreg] {
            if s >= self.num_fixed {
                continue;
            }
            if let Some(Assign::Reg(r)) = self.pieces.children[s].assign {
                let at = self.pieces.children[s].ranges[0].0;
                fixed_regs.push((self.block_frequency(at), r));
            }
        }
        fixed_regs.sort_by(|a, b| b.0.total_cmp(&a.0));
        out.extend(fixed_regs.into_iter().map(|(_, r)| r));
        // A fixed operand can sit in a register outside the pool (a helper
        // argument register is also an emitter scratch), which a free piece
        // must never take.
        let pool = self.regs.pool(self.class_of(id));
        out.retain(|&r| pool.contains(&r) && self.allowed(id, r));
        let mut seen = HashSet::new();
        out.retain(|r| seen.insert(*r));
        out
    }

    fn allocate_registers(&mut self) {
        while let Some((_, _, _, _, std::cmp::Reverse(id))) = self.queue.pop() {
            match self.map[id].stage {
                Stage::TryAllocate => self.do_stage_try_allocate(id),
                Stage::TrySplit => self.do_stage_try_split(id),
                Stage::Spill => self.do_stage_spill(id),
                // A split piece that is no longer needed: skip the stale entry.
                Stage::Replaced | Stage::Assigned | Stage::Spilled => {}
                Stage::New => unreachable!("an unstaged piece was queued"),
            }
        }
    }

    fn do_stage_try_allocate(&mut self, id: usize) {
        if self.try_allocate(id) {
            return;
        }

        if self.try_evict(id) {
            return;
        }

        // If we couldn't allocate, allow it to split next time.
        let mut next_stage = Stage::TrySplit;

        // If we already know splitting won't be profitable, skip it.
        if self.live_size(id) < SPLIT_MIN_RANGE_SIZE {
            next_stage = Stage::Spill;
        }

        self.set_stage_and_enqueue(id, next_stage);
    }

    fn do_stage_try_split(&mut self, id: usize) {
        if self.try_allocate(id) {
            return;
        }
        if !self.map[id].members.is_empty() {
            self.dissolve(id);
            return;
        }
        if self.try_split(id) {
            return;
        }
        self.set_stage_and_enqueue(id, Stage::Spill);
    }

    fn do_stage_spill(&mut self, id: usize) {
        if self.try_allocate(id) {
            return;
        }
        if self.try_evict(id) {
            return;
        }
        if !self.map[id].members.is_empty() {
            self.dissolve(id);
            return;
        }
        self.spill(id);
    }

    fn try_allocate(&mut self, id: usize) -> bool {
        if self.map[id].crosses_holes {
            return false;
        }
        let regs = self.registers(id);
        let preferred = self.preferred(id);
        // The search only reads the ranges, so it borrows them in place. The
        // assignment after it is what needs `&mut self`.
        let ranges = &self.pieces.children[id].ranges;
        let free = preferred
            .iter()
            .chain(regs.iter().filter(|r| !preferred.contains(r)))
            .copied()
            .find(|&r| {
                self.reg_ranges
                    .get(&r)
                    .map_or(true, |rr| !rr.has_conflict(ranges))
            });

        match free {
            Some(r) => {
                self.assign(id, r);
                true
            }
            None => false,
        }
    }

    fn try_evict(&mut self, id: usize) -> bool {
        if self.map[id].crosses_holes {
            return false;
        }

        let ranges = self.pieces.children[id].ranges.clone();
        let mut best_evict_reg: Option<PhysReg> = None;
        let mut min_spill_cost = self.spill_cost(id);
        let own_cost = min_spill_cost;
        // only pieces of an older cascade (or none)
        // may be evicted, which stops two pieces evicting each other forever.
        let cascade = if self.map[id].cascade != 0 {
            self.map[id].cascade
        } else {
            self.next_cascade
        };
        // should evict with a hint: the hinted registers are tried first and
        // win a tie, since taking one saves the copy the hint stands for.
        let preferred = self.preferred(id);
        let regs = self.registers(id);
        let order: Vec<PhysReg> = preferred
            .iter()
            .copied()
            .chain(regs.iter().copied().filter(|r| !preferred.contains(r)))
            .collect();

        for r in order {
            let mut conflicts_spill_cost = 0.0f64;
            let mut visited: HashSet<usize> = HashSet::new();
            if let Some(rr) = self.reg_ranges.get(&r) {
                rr.for_each_conflict(&ranges, |_, _, owner| {
                    if owner == FIXED {
                        // Conflicts with a fixed register use/def, cannot evict.
                        conflicts_spill_cost = f64::INFINITY;
                        return true;
                    }

                    if !visited.insert(owner) {
                        return false;
                    }

                    if cascade <= self.map[owner].cascade {
                        conflicts_spill_cost = f64::INFINITY;
                        return true;
                    }
                    let cost = self.spill_cost(owner);

                    if cost == MAX_SPILLABLE_SPILL_COST
                        || conflicts_spill_cost == MAX_SPILLABLE_SPILL_COST
                    {
                        conflicts_spill_cost = MAX_SPILLABLE_SPILL_COST;
                    } else {
                        conflicts_spill_cost += cost;
                    }
                    conflicts_spill_cost >= min_spill_cost
                });
            }

            if conflicts_spill_cost < min_spill_cost {
                min_spill_cost = conflicts_spill_cost;
                best_evict_reg = Some(r);
            }
        }
        let Some(best) = best_evict_reg else {
            return false;
        };
        if min_spill_cost >= own_cost {
            return false;
        }

        // It's cheaper to spill all the already-assigned conflicting tmps, so
        // evict them in favor of assigning this one.
        let mut conflicts: Vec<usize> = Vec::new();
        if let Some(rr) = self.reg_ranges.get(&best) {
            rr.for_each_conflict(&ranges, |_, _, owner| {
                conflicts.push(owner);
                false
            });
        }
        conflicts.sort_unstable();
        conflicts.dedup();

        if self.map[id].cascade == 0 {
            self.map[id].cascade = self.next_cascade;
            self.next_cascade += 1;
        }

        let cascade = self.map[id].cascade;

        for conflict in conflicts {
            self.map[conflict].cascade = cascade;
            self.evict(conflict, best);
            self.set_stage_and_enqueue(conflict, Stage::TryAllocate);
        }

        self.assign(id, best);
        true
    }

    fn assign(&mut self, id: usize, r: PhysReg) {
        // Disjoint fields: the ranges are read while `reg_ranges` is written.
        let ranges = &self.pieces.children[id].ranges;
        self.reg_ranges.entry(r).or_default().add(id, ranges);
        self.map[id].stage = Stage::Assigned;
        self.pieces.children[id].assign = Some(Assign::Reg(r));

        for m in self.map[id].members.clone() {
            self.pieces.children[m].assign = Some(Assign::Reg(r));
            self.map[m].stage = Stage::Assigned;
        }
    }

    fn evict(&mut self, id: usize, r: PhysReg) {
        let ranges = self.pieces.children[id].ranges.clone();
        if let Some(rr) = self.reg_ranges.get_mut(&r) {
            rr.evict(&ranges);
        }
        self.map[id].stage = Stage::New;
        self.pieces.children[id].assign = None;
        for m in self.map[id].members.clone() {
            self.pieces.children[m].assign = None;
            self.map[m].stage = Stage::Replaced;
        }
    }

    /// A group that found no register falls apart into its members, which
    /// are allocated (and split) one by one.
    fn dissolve(&mut self, id: usize) {
        let members = std::mem::take(&mut self.map[id].members);
        self.map[id].stage = Stage::Replaced;
        self.pieces.children[id].assign = None;
        for m in members {
            self.map[m].stage = Stage::New;
            self.init_unit(m);
        }
    }

    fn try_split(&mut self, id: usize) -> bool {
        if self.try_split_around_clobbers(id) {
            return true;
        }
        if self.try_split_around_loop(id) {
            return true;
        }
        if self.try_region_split(id) {
            return true;
        }
        self.try_split_intra_block(id)
    }

    /// Whether the instruction `g` reads or writes the vreg of `id` inside
    /// the piece
    fn uses_or_defs(&self, id: usize, g: u32) -> bool {
        let c = &self.pieces.children[id];
        self.weights
            .accesses_in(c.vreg, pos::pre(g), pos::post(g) + 1)
            .iter()
            .any(|&(p, _)| c.ranges.iter().any(|&(lo, hi)| lo <= p && p < hi))
    }

    fn ensure_loop_analysis(&mut self) {
        if self.loops.is_none() {
            self.loops = Some(Loops::new(self.func, self.num));
        }
    }

    /// The outermost loop that meaningfully reduces
    /// the live range when split out, and the non-loop remainder.
    fn choose_loop_for_split(&self, ranges: &[(u32, u32)]) -> Option<(usize, Vec<(u32, u32)>)> {
        let loops = self.loops.as_ref().unwrap();
        let size = live_len(ranges);
        let mut visited = vec![false; loops.loops.len()];

        for &(lo, hi) in ranges {
            let mut g = pos::inst_of(lo);
            let last = pos::inst_of(hi - 1);
            while g <= last {
                let block = self.num.block_at[g as usize];
                let mut nested: Vec<usize> = Vec::new();
                let mut l = loops.innermost[block];
                while let Some(i) = l {
                    if visited[i] {
                        break;
                    }
                    nested.push(i);
                    l = loops.loops[i].outer;
                }
                // Iterate from outer-most to inner-most
                for &i in nested.iter().rev() {
                    visited[i] = true;
                    let lp = &loops.loops[i];
                    if lp.boundary.iter().any(|&p| covers(ranges, p)) {
                        // Skip loops that are too large relative to the live
                        // range since they are unlikely to improve allocation
                        // progress.
                        if live_len(&lp.range) as f64 > size as f64 * LOOP_SPLIT_MAX_LOOP_FRACTION {
                            continue;
                        }
                        return Some((i, subtract_ranges(ranges, &lp.range)));
                    }
                }
                g = self.num.term[block] + 1;
            }
        }
        None
    }

    fn try_split_around_loop(&mut self, id: usize) -> bool {
        if !self.split_around_loops {
            return false;
        }
        // Clobber fixup references the tmp which would be stale after loop
        // splitting.
        if self.map[id].split_around_clobbers_metadata_index != 0 {
            return false;
        }

        if self.live_size(id) < SPLIT_MIN_RANGE_SIZE {
            return false;
        }

        if self.is_live_range_block_local(id) {
            return false;
        }

        self.ensure_loop_analysis();

        if self.loops.as_ref().unwrap().loops.is_empty() {
            return false;
        }

        let ranges = self.pieces.children[id].ranges.clone();
        let Some((loop_index, _non_loop_range)) = self.choose_loop_for_split(&ranges) else {
            return false;
        };
        let (depth, loop_range, entry) = {
            let lp = &self.loops.as_ref().unwrap().loops[loop_index];
            (lp.depth, lp.range.clone(), lp.entries.first().copied())
        };

        if depth > MAX_LOOP_SPLIT_DEPTH {
            return false;
        }

        // Estimate the fixup cost: each loop invocation pays one entry + one
        // exit move. If the cumulative fixup cost across sibling loop splits of
        // this tmp exceeds useDefCost, bail.
        let entry_freq = entry.map_or(1.0, |b| self.block_frequency(self.num.term_pos(b)));
        let fixup_cost = 2.0 * entry_freq;
        let total_fixup_cost = self.map[id].cumulative_fixup_cost + fixup_cost;
        if total_fixup_cost > self.map[id].use_def_cost {
            return false;
        }
        self.map[id].cumulative_fixup_cost += fixup_cost;

        let Some(loop_tmp) = self.pieces.punch_one(id, &loop_range) else {
            return false;
        };
        self.sync_map(loop_tmp);
        let vreg = self.pieces.children[id].vreg;
        let loop_cost =
            self.weights
                .piece_cost(vreg, &self.pieces.children[loop_tmp].ranges) as f64;
        self.map[loop_tmp].use_def_cost = loop_cost;
        // The original is no longer live within the loop.
        self.map[id].use_def_cost = (self.map[id].use_def_cost - loop_cost).max(0.0);

        // Spill zero-cost sides directly: a register would just carry a value
        // nothing reads, while still requiring fixup moves at loop boundaries.

        for t in [loop_tmp, id] {
            if self.map[t].use_def_cost < ZERO_COST_EPSILON {
                self.spill(t);
            } else {
                self.init_unit(t);
            }
        }
        true
    }

    fn try_split_around_clobbers(&mut self, id: usize) -> bool {
        if self.map[id].split_around_clobbers_metadata_index != 0 || self.map[id].crosses_holes {
            return false; // Already split around clobbers
        }
        if self.live_size(id) < SPLIT_MIN_RANGE_SIZE {
            return false; // Not enough instructions to be worthwhile
        }
        let ranges = self.pieces.children[id].ranges.clone();

        // The clobber at an instruction is reached once per candidate
        // register, so what depends only on the instruction is cached.
        let mut sites: HashMap<u32, bool> = HashMap::new();

        let mut best_split_reg: Option<PhysReg> = None;
        let split_cost_limit = if SPLIT_MULTIPLIER > 0.0 {
            self.map[id].use_def_cost / SPLIT_MULTIPLIER
        } else {
            f64::INFINITY
        };
        let mut min_split_cost = split_cost_limit;
        for r in self.registers(id) {
            let mut split_cost = 0.0f64;
            let mut conflicts: Vec<(u32, u32, usize)> = Vec::new();
            if let Some(rr) = self.reg_ranges.get(&r) {
                rr.for_each_conflict(&ranges, |lo, hi, owner| {
                    conflicts.push((lo, hi, owner));
                    false
                });
            }
            for (lo, hi, owner) in conflicts {
                // A clobber site is an interval inside one instruction.
                if owner == FIXED && pos::inst_of(lo) == pos::inst_of(hi - 1) {
                    let g = pos::inst_of(lo);
                    // The hole spans the instruction, between two gaps, and
                    // the move back runs at the Post gap.
                    if !self.num.can_split_at(pos::pre(g)) || !self.num.can_split_at(pos::post(g)) {
                        split_cost = f64::INFINITY;
                        break;
                    }
                    let touches = *sites.entry(g).or_insert_with(|| self.uses_or_defs(id, g));
                    if touches {
                        // If the inst that clobbers regs also use/def the tmp
                        // trying to be split, then can't split the tmp around
                        // this clobber.
                        split_cost = f64::INFINITY;
                        break;
                    }
                    // Times 2 for 'MOV tmp, gapTmp' and 'MOV gapTmp, tmp'
                    split_cost += self.block_frequency(pos::pre(g)) * 2.0;
                    if split_cost >= min_split_cost {
                        break; // Not the best or already over limit, exit early.
                    }
                } else {
                    // Conflicts with another Tmp already assigned to this
                    // register so splitting around the clobbers won't help.
                    split_cost = f64::INFINITY;
                    break;
                }
            }
            if split_cost < min_split_cost {
                min_split_cost = split_cost;
                best_split_reg = Some(r);
            }
        }
        if min_split_cost >= split_cost_limit {
            return false; // No register found or better to spill than to split
        }
        let best = best_split_reg.expect("a register under the limit");

        let mut hole_gs: Vec<u32> = Vec::new();
        if let Some(rr) = self.reg_ranges.get(&best) {
            rr.for_each_conflict(&ranges, |lo, _, _| {
                hole_gs.push(pos::inst_of(lo));
                false
            });
        }
        hole_gs.sort_unstable();
        hole_gs.dedup();
        // Punched hole should always include the instruction's late point so
        // the split tmp won't be modeled as conflicting with late defs.
        let holes: Vec<(u32, u32)> = hole_gs
            .iter()
            .map(|&g| (pos::pre(g), pos::post(g)))
            .collect();

        let (keeps, mut new_ids) = self.pieces.punch(id, &holes);
        debug_assert!(keeps, "the hole covered every range of the piece");
        if !keeps {
            // Cannot happen: the piece was live across, not inside, the hole.
            new_ids.retain(|&n| n != id);
        }
        self.map[id].split_around_clobbers_metadata_index = self.around_clobbers_metadata.len();
        self.set_stage_and_enqueue(id, Stage::TryAllocate);

        // Create tmps to carry the value across register clobbering
        // instructions. These tmps might spill or be assigned another register.
        let mut gap_tmps = Vec::with_capacity(new_ids.len());
        for gap in new_ids {
            self.sync_map(gap);
            let freq = 2.0 * self.block_frequency(self.pieces.children[gap].ranges[0].0);
            self.map[gap].use_def_cost = freq;
            gap_tmps.push(gap);
            self.set_stage_and_enqueue(gap, Stage::TryAllocate);
        }
        self.around_clobbers_metadata.push(gap_tmps);
        true
    }

    /// LLVM's `tryRegionSplit`. For each candidate register, `SpillPlacement`
    /// decides block by block whether the value should live in the register:
    /// a block where the register is taken must spill, a block that uses the
    /// value wants the register (by its use weight), and the other blocks
    /// follow their neighbours. The blocks that settle on the register
    /// become a new piece; the rest stays behind.
    fn try_region_split(&mut self, id: usize) -> bool {
        if !self.split_by_region || self.map[id].region_done {
            return false;
        }
        if self.map[id].crosses_holes || !self.map[id].members.is_empty() {
            return false;
        }
        if self.is_live_range_block_local(id) {
            return false;
        }
        let ranges = self.pieces.children[id].ranges.clone();
        let vreg = self.pieces.children[id].vreg;

        // The blocks the piece is live in, and its ranges inside each.
        let mut blocks: Vec<LBlock> = Vec::new();
        let mut in_block: Vec<Vec<(u32, u32)>> = Vec::new();
        for &(lo, hi) in &ranges {
            let mut p = lo;
            while p < hi {
                let b = self.num.block_at[pos::inst_of(p) as usize];
                let end = self.num.to(b).min(hi);
                if blocks.last() == Some(&b) {
                    in_block.last_mut().unwrap().push((p, end));
                } else {
                    blocks.push(b);
                    in_block.push(vec![(p, end)]);
                }
                p = end;
            }
        }
        let n = blocks.len();
        if n < 2 {
            return false;
        }
        let index_of: HashMap<LBlock, usize> = blocks
            .iter()
            .copied()
            .enumerate()
            .map(|(i, b)| (b, i))
            .collect();
        let mut links: Vec<(usize, usize)> = Vec::new();
        for (i, &b) in blocks.iter().enumerate() {
            if let Some(term) = &self.func.blocks[b].term {
                for e in term.edges() {
                    if let Some(&j) = index_of.get(&e.block) {
                        links.push((i, j));
                    }
                }
            }
        }
        let freq: Vec<f64> = blocks
            .iter()
            .map(|&b| self.block_frequency(self.num.from(b)))
            .collect();
        let want: Vec<f64> = in_block
            .iter()
            .map(|rs| {
                rs.iter()
                    .map(|&(lo, hi)| {
                        self.weights
                            .free_accesses_in(vreg, lo, hi)
                            .iter()
                            .map(|&(_, w)| w as f64)
                            .sum::<f64>()
                    })
                    .sum()
            })
            .collect();
        let mut best: Option<(f64, Vec<bool>)> = None;
        for r in self.registers(id) {
            // Blocks where the register is taken (or clobbered) must spill.
            let blocked: Vec<bool> = in_block
                .iter()
                .map(|rs| {
                    self.reg_ranges
                        .get(&r)
                        .is_some_and(|rr| rr.has_conflict(rs))
                })
                .collect();
            if blocked.iter().all(|&x| x) || !blocked.iter().any(|&x| x) {
                continue;
            }

            // `SpillPlacement::iterate`: +1 is the register, -1 the stack.
            let mut value: Vec<f64> = (0..n)
                .map(|i| {
                    if blocked[i] {
                        -1.0
                    } else if want[i] > 0.0 {
                        1.0
                    } else {
                        -1.0
                    }
                })
                .collect();
            for _ in 0..16 {
                let mut changed = false;
                let mut sum: Vec<f64> = vec![0.0; n];
                for &(i, j) in &links {
                    let w = freq[i].min(freq[j]);
                    sum[i] += w * value[j];
                    sum[j] += w * value[i];
                }
                for i in 0..n {
                    if blocked[i] {
                        continue;
                    }
                    let field = want[i] + sum[i];
                    let new = if field > 0.0 {
                        1.0
                    } else if field < 0.0 {
                        -1.0
                    } else {
                        value[i]
                    };
                    if new != value[i] {
                        value[i] = new;
                        changed = true;
                    }
                }
                if !changed {
                    break;
                }
            }
            let in_reg: Vec<bool> = value.iter().map(|&v| v > 0.0).collect();
            if !in_reg.iter().any(|&x| x) {
                continue;
            }
            // The moves on the border of the region and the uses it keeps.
            let mut border = 0.0f64;
            for &(i, j) in &links {
                if in_reg[i] != in_reg[j] {
                    border += freq[i].min(freq[j]);
                }
            }
            for i in 0..n {
                if in_reg[i] {
                    // The ends of the piece that are not in the region's
                    // neighbours (live-in or live-out through a hole).
                    let (lo, hi) = (in_block[i][0].0, in_block[i].last().unwrap().1);
                    if lo == self.num.from(blocks[i])
                        && !links.iter().any(|&(a, b)| b == i && in_reg[a])
                    {
                        border += freq[i];
                    }
                    if hi == self.num.to(blocks[i])
                        && !links.iter().any(|&(a, b)| a == i && in_reg[b])
                    {
                        border += freq[i];
                    }
                }
            }
            let kept: f64 = (0..n).filter(|&i| in_reg[i]).map(|i| want[i]).sum();
            let gain = kept - 2.0 * border;
            if gain > 0.0 && best.as_ref().map_or(true, |(g, _)| gain > *g) {
                best = Some((gain, in_reg));
            }
        }
        let Some((_, in_reg)) = best else {
            return false;
        };

        let runs = (0..n)
            .filter(|&i| in_reg[i] && (i == 0 || !in_reg[i - 1]))
            .count();
        if runs > 2 {
            return false;
        }

        let mut region: Vec<(u32, u32)> = Vec::new();
        for i in 0..n {
            if in_reg[i] {
                region.extend_from_slice(&in_block[i]);
            }
        }
        let Some(region_tmp) = self.pieces.punch_one(id, &region) else {
            return false;
        };
        self.sync_map(region_tmp);
        self.map[region_tmp].region_done = true;
        self.map[id].region_done = true;
        let region_cost =
            self.weights
                .piece_cost(vreg, &self.pieces.children[region_tmp].ranges) as f64;
        self.map[region_tmp].use_def_cost = region_cost;
        self.map[id].use_def_cost = (self.map[id].use_def_cost - region_cost).max(0.0);
        for t in [region_tmp, id] {
            if self.map[t].use_def_cost < ZERO_COST_EPSILON {
                self.spill(t);
            } else {
                self.init_unit(t);
            }
        }
        true
    }

    fn try_split_intra_block(&mut self, id: usize) -> bool {
        if self.map[id].split_around_clobbers_metadata_index != 0 {
            return false;
        }

        // Don't split an already intra-block tmp. Otherwise, we might
        // recursively try to split a cluster tmp that couldn't be allocated.
        if self.is_live_range_block_local(id) {
            return false;
        }

        let ranges = self.pieces.children[id].ranges.clone();
        let vreg = self.pieces.children[id].vreg;
        // The uses and defs inside the piece, in position order.
        let mut accesses: Vec<u32> = Vec::new();
        for &(lo, hi) in &ranges {
            for &(p, _) in self.weights.accesses_in(vreg, lo, hi) {
                accesses.push(p);
            }
        }

        let mut clusters: Vec<((u32, u32), usize)> = Vec::new();
        let mut i = 0;
        while i < accesses.len() {
            let block = self.num.block_at[pos::inst_of(accesses[i]) as usize];
            let end = self.num.to(block);
            let mut j = i;
            while j < accesses.len() && accesses[j] < end {
                j += 1;
            }
            let count = j - i;
            if count > 1 {
                let first = pos::inst_of(accesses[i]);
                let last = pos::inst_of(accesses[j - 1]);
                let lo = pos::pre(first);

                let hi = if self.num.can_split_at(pos::post(last)) {
                    pos::post(last)
                } else {
                    end
                };
                clusters.push(((lo, hi), count));
            }
            i = j;
        }
        if clusters.is_empty() {
            return false;
        }

        let holes: Vec<(u32, u32)> = clusters.iter().map(|&(h, _)| h).collect();
        let (keeps, new_ids) = self.pieces.punch(id, &holes);
        for &n in &new_ids {
            self.sync_map(n);
        }
        // Costs go to the clusters in the same order `punch` created them.
        // A cluster that meets no range is dropped, so match by position.
        for &n in &new_ids {
            let lo = self.pieces.children[n].ranges[0].0;
            let (cost_count, freq_at) = clusters
                .iter()
                .find(|&&((clo, chi), _)| clo <= lo && lo < chi)
                .map(|&((clo, _), count)| (count, clo))
                .expect("a cluster for the piece");
            self.map[n].use_def_cost = cost_count as f64 * self.block_frequency(freq_at);
            self.init_unit(n);
        }

        // The original is spilled, but the cluster tmps will hopefully carry
        // the value in registers within the cluster intervals.
        if keeps {
            self.spill(id);
        }
        true
    }

    fn spill(&mut self, id: usize) {
        self.map[id].stage = Stage::Spilled;
        self.pieces.children[id].assign = Some(Assign::Stack);

        let meta = self.map[id].split_around_clobbers_metadata_index;
        if meta != 0 {
            // Splitting didn't prevent the tmp from spilling after all, so no
            // point assigning registers or stack slots to the gap tmps.
            let gaps = self.around_clobbers_metadata[meta].clone();
            for gap in gaps {
                if let Some(Assign::Reg(r)) = self.pieces.children[gap].assign {
                    self.evict(gap, r);
                }
                self.map[gap].stage = Stage::Replaced;
                self.pieces.children[gap].assign = Some(Assign::Stack);
            }
        }
    }
}

/// A coalescable pair: two vregs joined by a copy.
type Pair = (usize, usize);

impl Greedy<'_> {
    /// Builds the affinity groups and gives each group of two
    /// or more units one representative unit.
    fn coalesce_tmps(&mut self) {
        // The live range of each vreg, and the unit that is its unplaced piece.
        let mut live: HashMap<usize, Vec<(u32, u32)>> = HashMap::new();
        let mut unit_of: HashMap<usize, usize> = HashMap::new();
        for (id, c) in self.pieces.children.iter().enumerate() {
            live.entry(c.vreg.index())
                .or_default()
                .extend_from_slice(&c.ranges);
            if c.assign.is_none() {
                unit_of.insert(c.vreg.index(), id);
            }
        }
        for ranges in live.values_mut() {
            *ranges = super::intervals::merge(std::mem::take(ranges));
        }

        let mut cost: BTreeMap<Pair, f64> = BTreeMap::new();
        let mut co: HashMap<usize, HashSet<usize>> = HashMap::new();
        for (v, hints) in self.built.hints.iter() {
            for h in hints {
                let (a, b) = (v.index(), h.partner.index());
                if a >= b
                    || !unit_of.contains_key(&a)
                    || !unit_of.contains_key(&b)
                    || self.func.class(v) != self.func.class(h.partner)
                    || self.func.repr(v) != self.func.repr(h.partner)
                    || self.func.is_gc_root(v) != self.func.is_gc_root(h.partner)
                {
                    continue;
                }
                *cost.entry((a, b)).or_insert(0.0) += self.block_frequency(h.pos);
                co.entry(a).or_default().insert(b);
                co.entry(b).or_default().insert(a);
            }
        }
        if cost.is_empty() {
            return;
        }

        let covers_at = |v: usize, p: u32| live.get(&v).is_some_and(|r| covers(r, p));
        let mut prune: Vec<Pair> = Vec::new();
        let mut check = |co: &HashMap<usize, HashSet<usize>>,
                         d: usize,
                         q: u32,
                         exempt: &[usize],
                         prune: &mut Vec<Pair>| {
            if let Some(partners) = co.get(&d) {
                for &l in partners {
                    if !exempt.contains(&l) && covers_at(l, q) {
                        prune.push((d.min(l), d.max(l)));
                    }
                }
            }
        };
        for &b in &self.num.order {
            for (k, inst) in self.func.blocks[b].insts.iter().enumerate() {
                let q = pos::late(self.num.base[b] + k as u32);
                let exempt: Vec<usize> = match inst {
                    LirInst::Move { src, .. } => vec![src.index()],
                    _ => Vec::new(),
                };
                for op in inst.operands(self.func) {
                    if op.role == Role::Def {
                        check(&co, op.vreg.index(), q, &exempt, &mut prune);
                    }
                }
            }

            // The copies of an edge run at the end of its source.
            if let Some(term) = &self.func.blocks[b].term {
                let q = self.num.to(b) - 1;
                for edge in term.edges() {
                    let params = &self.func.blocks[edge.block].params;
                    for (i, &arg) in edge.args.iter().enumerate() {
                        if let Some(&param) = params.get(i) {
                            check(&co, param.index(), q, &[arg.index()], &mut prune);
                        }
                    }

                    for (i, &x) in edge.args.iter().enumerate() {
                        for (j, &y) in edge.args.iter().enumerate().skip(i + 1) {
                            if x == y {
                                continue;
                            }
                            if let (Some(&pi), Some(&pj)) = (params.get(i), params.get(j)) {
                                let (pi, pj) = (pi.index(), pj.index());
                                if co.get(&pi).is_some_and(|s| s.contains(&pj)) {
                                    prune.push((pi.min(pj), pi.max(pj)));
                                }
                            }
                        }
                    }
                }
            }
        }
        for (a, b) in prune {
            cost.remove(&(a, b));
            if let Some(s) = co.get_mut(&a) {
                s.remove(&b);
            }
            if let Some(s) = co.get_mut(&b) {
                s.remove(&a);
            }
        }

        let mut moves: Vec<(Pair, f64)> = cost.into_iter().collect();
        moves.sort_by(|x, y| y.1.total_cmp(&x.1).then(x.0.cmp(&y.0)));
        let overlap = |a: usize, b: usize| super::assign::intersects(&live[&a], &live[&b]);
        let compatible = |co: &HashMap<usize, HashSet<usize>>, xs: &[usize], ys: &[usize]| {
            xs.iter().all(|&x| {
                ys.iter()
                    .all(|&y| !overlap(x, y) || co.get(&x).is_some_and(|s| s.contains(&y)))
            })
        };
        let mut group_of: HashMap<usize, usize> = HashMap::new();
        let mut groups: Vec<Vec<usize>> = Vec::new();
        for ((a, b), _) in moves {
            match (group_of.get(&a).copied(), group_of.get(&b).copied()) {
                (Some(x), Some(y)) if x == y => {}
                (None, None) => {
                    group_of.insert(a, groups.len());
                    group_of.insert(b, groups.len());
                    groups.push(vec![a, b]);
                }
                (Some(x), Some(y)) => {
                    if compatible(&co, &groups[x], &groups[y]) {
                        let moved = std::mem::take(&mut groups[y]);
                        for &m in &moved {
                            group_of.insert(m, x);
                        }
                        groups[x].extend(moved);
                    }
                }
                (Some(g), None) | (None, Some(g)) => {
                    let single = if group_of.contains_key(&a) { b } else { a };
                    if compatible(&co, &[single], &groups[g]) {
                        group_of.insert(single, g);
                        groups[g].push(single);
                    }
                }
            }
        }

        for members in groups.into_iter().filter(|g| g.len() > 1) {
            let units: Vec<usize> = members.iter().map(|m| unit_of[m]).collect();
            let mut ranges: Vec<(u32, u32)> = Vec::new();
            let mut use_def_cost = 0.0;
            for &u in &units {
                ranges.extend_from_slice(&self.pieces.children[u].ranges);
                use_def_cost += self.map[u].use_def_cost;
            }
            let vreg = self.pieces.children[units[0]].vreg;
            let id = self.pieces.children.len();
            self.pieces.children.push(super::intervals::Child {
                vreg,
                ranges: super::intervals::merge(ranges),
                assign: None,
            });
            self.sync_map(id);
            self.map[id].use_def_cost = use_def_cost;
            self.map[id].is_group = true;
            for &u in &units {
                self.map[u].stage = Stage::Replaced;
            }
            self.map[id].members = units;
        }
    }
}
