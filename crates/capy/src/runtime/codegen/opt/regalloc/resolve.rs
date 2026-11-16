//! Move resolver

use std::collections::BTreeMap;

use cranelift_entity::{EntityRef, SecondaryMap};

use super::assign::Assignment;
use super::liveness::{Liveness, Numbering};
use super::pos;
use super::{Loc, Move, Scratch, Untag};
use crate::runtime::codegen::opt::ir::inst::Constant;
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, LirInst, RegClass, Role, VReg};

/// Orders the moves of one parallel move so that no source is overwritten
/// before it is read. A cycle is broken through the scratch register of its
/// class. The scratch only holds one value at a time, because a cycle's
/// chain drains before another cycle is broken.
pub(crate) fn sequentialize(moves: Vec<Move>, scratch: Scratch) -> Vec<Move> {
    // An untag reload has a placeholder `src` (its raw value is never
    // stored), so it is never a no-op.
    let mut pending: Vec<Move> = moves
        .into_iter()
        .filter(|m| m.untag.is_some() || m.src != m.dst)
        .collect();

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

    // Two different values for one destination would be a lost move.
    debug_assert!(
        pending
            .iter()
            .enumerate()
            .all(|(i, m)| pending[i + 1..].iter().all(|n| n.dst != m.dst)),
        "a parallel move writes one destination twice: {pending:?}"
    );

    let mut out = Vec::with_capacity(pending.len());
    while !pending.is_empty() {
        let ready = (0..pending.len()).find(|&i| {
            !pending
                .iter()
                .enumerate()
                .any(|(j, m)| j != i && m.reads() == pending[i].dst)
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
                            matches!(m.dst, Loc::Reg(_))
                                && pending.iter().any(|n| n.reads() == m.dst)
                        })
                        .unwrap_or(0)
                };
                let d = pending[at].dst;
                let class = pending
                    .iter()
                    .find(|m| m.reads() == d)
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
                    konst: None,
                    untag: None,
                });
                for m in pending.iter_mut() {
                    match m.untag.as_mut() {
                        Some(u) if u.from == d => u.from = tmp,
                        Some(_) => {}
                        None if m.src == d => m.src = tmp,
                        None => {}
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
) -> Result<(Vec<Vec<Move>>, Vec<Constant>), String> {
    let mut remat = Remat::new(func, num, asg);
    let mut groups: BTreeMap<(u32, u8), Vec<Move>> = BTreeMap::new();

    // Piece boundaries inside a block. A piece may have several ranges, so
    // a boundary is a range of one piece that ends where a range of another
    // piece of the same vreg begins.
    for v in func.vregs.keys() {
        let ids = &asg.vreg_children[v];
        if ids.len() < 2 {
            continue;
        }
        let mut starts: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
        for &id in ids {
            for &(lo, _) in &asg.children[id].ranges {
                starts.insert(lo, id);
            }
        }
        for &a_id in ids {
            let a = &asg.children[a_id];
            for &(_, p) in &a.ranges {
                let Some(&b_id) = starts.get(&p) else {
                    continue;
                };
                if b_id == a_id {
                    continue;
                }
                let b = &asg.children[b_id];
                let g = pos::inst_of(p);
                let blk = num.block_at[g as usize];
                // A block boundary is an edge, handled below.
                if p == num.from(blk) {
                    continue;
                }
                if !num.can_split_at(p) {
                    return Err(format!(
                        "a piece of {v} changes location at {p}, which is not a gap"
                    ));
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
                // The slot already holds the value: no store. A raw int that
                // is only rebuilt from its tagged source is never stored.
                if matches!(src, Loc::Reg(_))
                    && matches!(dst, Loc::Slot(_))
                    && (asg.coherence.skip.contains(&(v, p)) || remat.slot_free[v])
                {
                    continue;
                }
                let key = if p % pos::POINTS == 0 {
                    (g, 2)
                } else {
                    (g + 1, 1)
                };
                let untag = remat.untag_reload(v, src, dst, p)?;
                groups.entry(key).or_default().push(Move {
                    class: func.class(v),
                    src,
                    dst,
                    konst: remat.reload(v, src, dst),
                    untag,
                });
            }
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
                if src != dst
                    && !(sv == dv
                        && matches!(src, Loc::Reg(_))
                        && matches!(dst, Loc::Slot(_))
                        && asg.coherence.skip_edge.contains(&(sv, pb)))
                {
                    groups.entry(key).or_default().push(Move {
                        class: func.class(dv),
                        src,
                        dst,
                        konst: remat.reload(sv, src, dst),
                        untag: None,
                    });
                }
            }
        }
    }

    let mut out: Vec<Vec<Move>> = vec![Vec::new(); num.count as usize];
    for ((g, _), moves) in groups {
        out[g as usize].extend(sequentialize(moves, scratch));
    }
    Ok((out, remat.consts))
}

/// Rematerialization at reloads.
///
/// Constants: a vreg with one definition,
/// a constant, is rebuilt from its immediate where it would be reloaded from
/// its slot. The store stays, so the slot is still valid for operands that
/// read it directly. Only integer immediates in a GPR qualify; the others
/// need a scratch register or the constant table.
///
/// Untags: a raw int defined once by `UntagInt`/`UntagInt64` of a tagged
/// fixnum is rebuilt as `untag(load source slot)` where it would be reloaded
/// from its own slot, when the source's slot holds the source there. If every
/// access to the raw value's slot is such a reload, the slot is never read, so
/// the spill stores are dropped too (`slot_free`).
struct Remat<'a> {
    of: SecondaryMap<VReg, Option<Constant>>,
    index: std::collections::HashMap<VReg, u32>,
    consts: Vec<Constant>,
    asg: &'a Assignment,
    /// The tagged source of each raw int with one `UntagInt`/`UntagInt64`
    /// definition.
    untag_of: SecondaryMap<VReg, Option<VReg>>,
    /// Raw ints whose slot is only ever reloaded by an untag remat.
    slot_free: SecondaryMap<VReg, bool>,
}

impl<'a> Remat<'a> {
    fn new(func: &LirFunc, num: &Numbering, asg: &'a Assignment) -> Self {
        let mut of: SecondaryMap<VReg, Option<Constant>> = SecondaryMap::new();
        let mut defs: SecondaryMap<VReg, u32> = SecondaryMap::new();
        let mut untag_of: SecondaryMap<VReg, Option<VReg>> = SecondaryMap::new();
        for (_, data) in func.blocks.iter() {
            for inst in &data.insts {
                if let LirInst::Const { dst, value } = inst {
                    if matches!(
                        value,
                        Constant::Int32(_) | Constant::Int64(_) | Constant::Bits(_)
                    ) && func.class(*dst) == RegClass::Gpr
                    {
                        of[*dst] = Some(value.clone());
                    }
                }
                if let LirInst::UntagInt { dst, src } | LirInst::UntagInt64 { dst, src } = inst {
                    untag_of[*dst] = Some(*src);
                }
                for op in inst.operands(func) {
                    if op.role == Role::Def {
                        defs[op.vreg] += 1;
                    }
                }
            }
            if let Some(term) = &data.term {
                for op in term.operands(func) {
                    if op.role == Role::Def {
                        defs[op.vreg] += 1;
                    }
                }
            }
            for &p in &data.params {
                defs[p] += 1;
            }
        }
        for v in func.vregs.keys() {
            if defs[v] != 1 {
                of[v] = None;
                untag_of[v] = None;
            }
        }
        let mut remat = Self {
            of,
            index: Default::default(),
            consts: Vec::new(),
            asg,
            untag_of,
            slot_free: SecondaryMap::new(),
        };
        remat.find_slot_free(func, num);
        remat
    }

    fn find_slot_free(&mut self, func: &LirFunc, num: &Numbering) {
        let asg = self.asg;
        let mut operand_at: SecondaryMap<VReg, Vec<u32>> = SecondaryMap::new();
        for &b in &num.order {
            let data = &func.blocks[b];
            let base = num.base[b];
            for (k, inst) in data.insts.iter().enumerate() {
                let g = base + k as u32;
                for op in super::intervals::inst_ops(func, inst) {
                    if self.untag_of[op.vreg].is_some() {
                        operand_at[op.vreg].push(op_pos(op.role, g));
                    }
                }
            }
            if let Some(term) = &data.term {
                let g = num.term[b];
                for op in super::intervals::term_ops(func, term) {
                    if self.untag_of[op.vreg].is_some() {
                        operand_at[op.vreg].push(op_pos(op.role, g));
                    }
                }
            }
        }
        for v in func.vregs.keys() {
            let Some(src) = self.untag_of[v] else {
                continue;
            };
            if func.class(v) != RegClass::Gpr {
                continue;
            }
            let ids = &asg.vreg_children[v];
            let stacks: Vec<&Vec<(u32, u32)>> = ids
                .iter()
                .map(|&id| &asg.children[id])
                .filter(|c| c.assign == Some(super::intervals::Assign::Stack))
                .map(|c| &c.ranges)
                .collect();
            if stacks.is_empty() {
                continue;
            }
            let mut free = true;
            'ranges: for &(lo, hi) in stacks.iter().flat_map(|r| r.iter()) {
                let b = num.block_at[pos::inst_of(lo) as usize];
                if lo <= num.from(b)
                    || hi > num.term_pos(b)
                    || operand_at[v].iter().any(|&q| lo <= q && q < hi)
                {
                    free = false;
                    break 'ranges;
                }
                // The next piece, if any, starts where this range ends.
                let next = ids.iter().map(|&id| &asg.children[id]).find(|c| {
                    c.ranges.iter().any(|r| r.0 == hi)
                        && c.assign != Some(super::intervals::Assign::Stack)
                });
                if next.is_some() && self.source_slot(src, hi).is_none() {
                    free = false;
                    break 'ranges;
                }
            }
            self.slot_free[v] = free;
        }
    }

    fn source_slot(&self, src: VReg, p: u32) -> Option<Loc> {
        let asg = self.asg;
        let slot = asg.slot_of[src]?;
        if super::home_word(slot).is_some() {
            return Some(Loc::Slot(slot));
        }
        if asg.groups[src].is_some() {
            return None;
        }
        asg.vreg_children[src]
            .iter()
            .map(|&id| &asg.children[id])
            .any(|c| {
                c.assign == Some(super::intervals::Assign::Stack)
                    && c.ranges.iter().any(|&(lo, hi)| lo < p && p < hi)
            })
            .then_some(Loc::Slot(slot))
    }

    fn untag_reload(&self, v: VReg, src: Loc, dst: Loc, p: u32) -> Result<Option<Untag>, String> {
        if !self.slot_free[v] || !matches!((src, dst), (Loc::Slot(_), Loc::Reg(_))) {
            return Ok(None);
        }
        let source = self.untag_of[v].ok_or_else(|| format!("{v} is not an untag"))?;
        let from = self
            .source_slot(source, p)
            .ok_or_else(|| format!("the source {source} of {v} is not in a slot at {p}"))?;
        Ok(Some(Untag {
            from,
            src: source,
            dst: v,
        }))
    }

    /// The constant index when moving `v` from `src` to `dst` is a reload.
    fn reload(&mut self, v: VReg, src: Loc, dst: Loc) -> Option<u32> {
        if !matches!((src, dst), (Loc::Slot(_), Loc::Reg(_))) {
            return None;
        }
        let value = self.of[v].clone()?;
        let consts = &mut self.consts;
        Some(*self.index.entry(v).or_insert_with(|| {
            consts.push(value);
            consts.len() as u32 - 1
        }))
    }
}

/// Where an operand of instruction `g` is read or written.
fn op_pos(role: Role, g: u32) -> u32 {
    if role == Role::Use {
        pos::early(g)
    } else {
        pos::late(g)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::regalloc::PhysReg;

    fn reg(n: u16) -> Loc {
        Loc::Reg(PhysReg(n))
    }

    fn mv(src: Loc, dst: Loc) -> Move {
        Move {
            class: RegClass::Gpr,
            src,
            dst,
            konst: None,
            untag: None,
        }
    }

    fn scratch() -> Scratch {
        Scratch {
            gpr: PhysReg(90),
            fpr: PhysReg(91),
        }
    }

    /// Applies `moves` to a register file and returns it.
    fn run(moves: &[Move], file: &mut std::collections::HashMap<Loc, i32>) {
        for m in moves {
            let v = file[&m.src];
            file.insert(m.dst, v);
        }
    }

    #[test]
    fn swap_goes_through_the_scratch() {
        let moves = vec![mv(reg(1), reg(2)), mv(reg(2), reg(1))];
        let out = sequentialize(moves, scratch());
        let mut file: std::collections::HashMap<Loc, i32> =
            [(reg(1), 10), (reg(2), 20)].into_iter().collect();
        run(&out, &mut file);
        assert_eq!((file[&reg(1)], file[&reg(2)]), (20, 10));
    }

    #[test]
    fn chain_is_ordered_without_scratch() {
        let moves = vec![mv(reg(1), reg(2)), mv(reg(2), reg(3))];
        let out = sequentialize(moves, scratch());
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].dst, reg(3));
    }

    #[test]
    fn identical_moves_collapse() {
        let moves = vec![mv(reg(1), reg(2)), mv(reg(1), reg(2)), mv(reg(3), reg(3))];
        assert_eq!(sequentialize(moves, scratch()).len(), 1);
    }
}
