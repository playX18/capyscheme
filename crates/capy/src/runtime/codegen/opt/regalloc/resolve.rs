//! Move resolver

use std::collections::BTreeMap;

use cranelift_entity::{EntityRef, SecondaryMap};

use super::assign::Assignment;
use super::liveness::{Liveness, Numbering};
use super::pos;
use super::{Loc, Move, Scratch};
use crate::runtime::codegen::opt::ir::inst::Constant;
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, LirInst, RegClass, Role, VReg};

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
                    konst: None,
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
) -> Result<(Vec<Vec<Move>>, Vec<Constant>), String> {
    let mut remat = Remat::new(func);
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
                // The slot already holds the value: no store.
                if matches!(src, Loc::Reg(_))
                    && matches!(dst, Loc::Slot(_))
                    && asg.coherence.skip.contains(&(v, p))
                {
                    continue;
                }
                let key = if p % pos::POINTS == 0 {
                    (g, 2)
                } else {
                    (g + 1, 1)
                };
                groups.entry(key).or_default().push(Move {
                    class: func.class(v),
                    src,
                    dst,
                    konst: remat.reload(v, src, dst),
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

/// Constant rematerialization, JSC's reload of a constant tmp: a vreg with
/// one definition, a constant, is rebuilt from its immediate where it would
/// be reloaded from its slot. The store stays, so the slot is still valid
/// for operands that read it directly. Only integer immediates in a GPR
/// qualify; the others need a scratch register or the constant table.
struct Remat {
    of: SecondaryMap<VReg, Option<Constant>>,
    index: std::collections::HashMap<VReg, u32>,
    consts: Vec<Constant>,
}

impl Remat {
    fn new(func: &LirFunc) -> Self {
        let mut of: SecondaryMap<VReg, Option<Constant>> = SecondaryMap::new();
        let mut defs: SecondaryMap<VReg, u32> = SecondaryMap::new();
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
            }
        }
        Self {
            of,
            index: Default::default(),
            consts: Vec::new(),
        }
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
