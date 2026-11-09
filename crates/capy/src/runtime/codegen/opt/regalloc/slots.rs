//! Spill-slot coloring and coalescing.
//!
//! The pass:
//!
//! 1. builds the exact interference relation over stack pieces of different
//!    vregs, so a vreg that is only spilled in some regions
//!    lets another vreg use its slot in the gaps;
//! 2. coalesces vreg pairs connected by a slot-to-slot copy (an explicit
//!    `Move` or an edge argument/parameter pair) when their stack pieces do
//!    not interfere, merging their interference edges;
//! 3. assigns the lowest free slot index to each group in first-fit order.

use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::SecondaryMap;

use super::assign::covers;
use super::intervals::{Assign, Child, merge};
use super::liveness::Numbering;
use super::pos;
use crate::runtime::codegen::opt::lir::{LirFunc, LirInst, VReg};

fn stack_at(stack: &SecondaryMap<VReg, Vec<(u32, u32)>>, vreg: VReg, pos: u32) -> bool {
    stack.get(vreg).is_some_and(|ranges| covers(ranges, pos))
}

/// Whether two slot live-range sets conflict.
fn conflict(a: &[(u32, u32)], b: &[(u32, u32)], allowed: Option<&BTreeSet<u32>>) -> bool {
    for &(alo, ahi) in a {
        for &(blo, bhi) in b {
            if alo < bhi && blo < ahi {
                return true;
            }
            let touch = if ahi == blo {
                ahi
            } else if bhi == alo {
                bhi
            } else {
                continue;
            };
            if !pos::is_gap(touch) && !allowed.is_some_and(|set| set.contains(&touch)) {
                return true;
            }
        }
    }
    false
}

fn find(parent: &mut [usize], mut x: usize) -> usize {
    while parent[x] != x {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    x
}

/// Assigns every vreg with stack pieces one slot, reusing slots exactly where
/// the stack pieces of two vregs never coexist.
pub(crate) fn assign_slots(
    func: &LirFunc,
    num: &Numbering,
    children: &[Child],
    coherence: &super::assign::Coherence,
) -> (u32, SecondaryMap<VReg, Option<u32>>) {
    let mut stack: SecondaryMap<VReg, Vec<(u32, u32)>> = SecondaryMap::new();
    for c in children {
        if c.assign == Some(Assign::Stack) {
            stack[c.vreg].extend_from_slice(&c.ranges);
        }
    }
    // Where the slot holds the value for a later read, a store is skipped.
    for v in func.vregs.keys() {
        if !stack[v].is_empty() {
            stack[v].extend_from_slice(&coherence.extents[v]);
        }
    }
    let mut vregs: Vec<VReg> = Vec::new();
    for v in func.vregs.keys() {
        if !stack[v].is_empty() {
            stack[v] = merge(std::mem::take(&mut stack[v]));
            vregs.push(v);
        }
    }
    let mut index: SecondaryMap<VReg, Option<usize>> = SecondaryMap::new();
    for (i, &v) in vregs.iter().enumerate() {
        index[v] = Some(i);
    }

    // Copy sites whose two halves are both in stack pieces. `allowed` names
    // the odd boundary of an explicit `Move`, where the touch is the copy
    // itself and is safe.
    let mut candidates: BTreeMap<(usize, usize), (u32, BTreeSet<u32>)> = BTreeMap::new();
    let mut add = |a: VReg, b: VReg, allowed_pos: Option<u32>| {
        if a == b {
            return;
        }
        let (Some(ia), Some(ib)) = (index[a], index[b]) else {
            return;
        };
        let key = if ia < ib { (ia, ib) } else { (ib, ia) };
        let entry = candidates
            .entry(key)
            .or_insert_with(|| (0, BTreeSet::new()));
        entry.0 += 1;
        if let Some(p) = allowed_pos {
            entry.1.insert(p);
        }
    };
    for &b in &num.order {
        let data = &func.blocks[b];
        let base = num.base[b];
        for (k, inst) in data.insts.iter().enumerate() {
            let LirInst::Move { dst, src } = inst else {
                continue;
            };
            if func.class(*dst) != func.class(*src) {
                continue;
            }
            let use_pos = pos::early(base + k as u32);
            let def_pos = pos::late(base + k as u32);
            if stack_at(&stack, *src, use_pos) && stack_at(&stack, *dst, def_pos) {
                add(*src, *dst, Some(def_pos));
            }
        }
        let Some(term) = &data.term else {
            continue;
        };
        let tpos = num.term_pos(b);
        for edge in term.edges() {
            let target = edge.block;
            let spos = num.from(target);
            let params = &func.blocks[target].params;
            for (i, &arg) in edge.args.iter().enumerate() {
                let Some(&param) = params.get(i) else {
                    break;
                };
                if func.class(arg) != func.class(param) {
                    continue;
                }
                if stack_at(&stack, arg, tpos) && stack_at(&stack, param, spos) {
                    add(arg, param, None);
                }
            }
        }
    }

    // Exact interference. `n` is the number of spilled vregs (tens), so the
    // quadratic pair scan is cheaper than an interval tree.
    let n = vregs.len();
    let mut adj: Vec<BTreeSet<usize>> = vec![BTreeSet::new(); n];
    for i in 0..n {
        let a = &stack[vregs[i]];
        for j in (i + 1)..n {
            let allowed = candidates.get(&(i, j)).map(|(_, set)| set);
            if conflict(a, &stack[vregs[j]], allowed) {
                adj[i].insert(j);
                adj[j].insert(i);
            }
        }
    }

    // Coalesce in copy-weight order. A merge is legal when the two groups are
    // not adjacent; the merged group inherits the killed group's edges.
    let mut parent: Vec<usize> = (0..n).collect();
    let mut order: Vec<(usize, usize, u32)> = candidates
        .into_iter()
        .map(|((a, b), (weight, _))| (a, b, weight))
        .collect();
    order.sort_by(|x, y| y.2.cmp(&x.2).then_with(|| (x.0, x.1).cmp(&(y.0, y.1))));
    for (i, j, _) in order {
        let ri = find(&mut parent, i);
        let rj = find(&mut parent, j);
        if ri == rj || adj[ri].contains(&rj) {
            continue;
        }
        let (root, dead) = if ri < rj { (ri, rj) } else { (rj, ri) };
        let dead_edges: Vec<usize> = adj[dead].iter().copied().collect();
        for nb in dead_edges {
            adj[nb].remove(&dead);
            if nb != root {
                adj[nb].insert(root);
                adj[root].insert(nb);
            }
        }
        adj[root].remove(&dead);
        adj[dead].clear();
        parent[dead] = root;
    }

    // First-fit coloring, in order of each group's earliest stack position so
    // that range-like groups reuse the low slots before they are taken.
    let mut lo: Vec<u32> = vec![u32::MAX; n];
    for (i, &v) in vregs.iter().enumerate() {
        let r = find(&mut parent, i);
        lo[r] = lo[r].min(stack[v][0].0);
    }
    let mut roots: Vec<usize> = (0..n).filter(|&i| find(&mut parent, i) == i).collect();
    roots.sort_by_key(|&r| (lo[r], r));
    let mut color: Vec<Option<u32>> = vec![None; n];
    for r in roots {
        let mut used: BTreeSet<u32> = BTreeSet::new();
        for nb in &adj[r] {
            if let Some(c) = color[*nb] {
                used.insert(c);
            }
        }
        let mut c = 0u32;
        while used.contains(&c) {
            c += 1;
        }
        color[r] = Some(c);
    }
    let spill_slots = color.iter().filter_map(|c| *c).max().map_or(0, |c| c + 1);

    let mut slot_of: SecondaryMap<VReg, Option<u32>> = SecondaryMap::new();
    for (i, &v) in vregs.iter().enumerate() {
        let r = find(&mut parent, i);
        slot_of[v] = color[r];
    }
    (spill_slots, slot_of)
}
