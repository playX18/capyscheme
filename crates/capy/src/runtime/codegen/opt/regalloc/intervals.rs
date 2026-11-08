//! Live segments, fixed constraints and the initial pieces of each vreg.

use std::collections::{BTreeMap, HashMap, HashSet};

use cranelift_entity::{EntityRef, SecondaryMap};

use super::liveness::{Liveness, Numbering};
use super::{PhysReg, RegFile};
use crate::runtime::codegen::opt::lir::{
    CallTarget, Clobbers, Constraint, LBlock, LEdge, LirBlock, LirFunc, LirInst, LirTerm, PhysRole,
    Role, VReg,
};

/// Where a piece lives. A `Stack` piece uses the spill slot of its vreg.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Assign {
    Reg(PhysReg),
    Stack,
}

/// A piece of a vreg's lifetime: a set of half-open position ranges with one
/// location. Pieces of one vreg never overlap.
#[derive(Clone, Debug)]
pub(crate) struct Child {
    pub vreg: VReg,
    pub ranges: Vec<(u32, u32)>,
    /// `None` while the piece still has to be placed by linear scan.
    pub assign: Option<Assign>,
}

/// The input to linear scan.
pub(crate) struct Built {
    pub children: Vec<Child>,
    pub vreg_children: SecondaryMap<VReg, Vec<usize>>,
    /// Fixed occupancy of each physical register, sorted and merged.
    pub fixed: BTreeMap<PhysReg, Vec<(u32, u32)>>,
    /// The use positions of each vreg by an instruction or edge proper, sorted.
    /// A frame value of an exit or a safepoint is not one: it is only read
    /// from wherever the vreg lives.
    pub uses: SecondaryMap<VReg, Vec<u32>>,
    /// Coalescing hints: the copy partners of a vreg, with the position where
    /// each side holds the value.
    pub hints: SecondaryMap<VReg, Vec<Hint>>,
    /// Positions at which a vreg is a frame value of a non-collecting call:
    /// a piece that covers one may only use a call-preserved register, or be
    /// spilled.
    pub call_safe: SecondaryMap<VReg, Vec<u32>>,
}

/// One coalescing hint. `vreg` holds at `pos` the value its `partner` holds
/// at `partner_pos`, because an edge or an explicit move copies one to the
/// other. If the partner is already placed in a register that stays free for
/// the whole piece, the allocator prefers it and the copy disappears.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Hint {
    pub pos: u32,
    pub partner: VReg,
    pub partner_pos: u32,
}

/// An operand as the allocator sees it.
#[derive(Clone, Copy, Debug)]
pub(crate) struct OpInfo {
    pub vreg: VReg,
    pub role: Role,
    pub constraint: Constraint,
    /// A frame value of a call or poll: it must be in a spill slot there.
    pub slot: bool,
    /// A frame value of a helper or primitive call, which never collects:
    /// it may stay in an ABI callee-saved register across the call, which
    /// preserves it for the deopt checked after the call returns.
    pub call_safe: bool,
}

fn plain_ops(ops: Vec<crate::runtime::codegen::opt::lir::Operand>) -> Vec<OpInfo> {
    ops.into_iter()
        .map(|op| OpInfo {
            vreg: op.vreg,
            role: op.role,
            constraint: op.constraint,
            slot: false,
            call_safe: false,
        })
        .collect()
}

/// Whether `inst` is a call that cannot collect.
fn is_non_collecting_call(inst: &LirInst) -> bool {
    matches!(
        inst,
        LirInst::Call {
            target: CallTarget::Helper(_) | CallTarget::Prim(_) | CallTarget::PrimN(_),
            ..
        }
    )
}

/// The operands of an instruction, with the frame values of a safepoint
/// marked as slot or call-safe uses.
pub(crate) fn inst_ops(func: &LirFunc, inst: &LirInst) -> Vec<OpInfo> {
    let mut out = plain_ops(inst.operands(func));

    let keep_in_regs = matches!(inst, LirInst::Poll { .. });
    if let (Some(sp), false) = (inst.safepoint(), keep_in_regs) {
        let call_safe = is_non_collecting_call(inst);
        let first = match inst {
            LirInst::Call { args, .. } => args.len(),
            _ => 0,
        };
        let n = func.frame_regs(&func.safepoints[sp].values).len();
        for op in &mut out[first..first + n] {
            if call_safe && func.is_gc_root(op.vreg) {
                op.call_safe = true;
            } else {
                op.slot = true;
            }
        }
    }
    out
}

/// The operands of a terminator.
pub(crate) fn term_ops(func: &LirFunc, term: &LirTerm) -> Vec<OpInfo> {
    plain_ops(term.operands(func))
}

fn edges_mut(term: &mut LirTerm) -> Vec<&mut LEdge> {
    match term {
        LirTerm::Jump(edge) => vec![edge],
        LirTerm::Branch {
            then, otherwise, ..
        } => vec![then, otherwise],
        LirTerm::Switch { cases, default, .. } => cases
            .iter_mut()
            .map(|(_, edge)| edge)
            .chain(std::iter::once(default))
            .collect(),
        _ => Vec::new(),
    }
}

pub(crate) fn legalize(func: &mut LirFunc) {
    let blocks: Vec<LBlock> = func.blocks.keys().collect();
    for b in blocks {
        let insts = std::mem::take(&mut func.blocks[b].insts);
        let mut out = Vec::with_capacity(insts.len());
        for mut inst in insts {
            match &mut inst {
                LirInst::Call {
                    target: CallTarget::Helper(_) | CallTarget::Prim(_),
                    args,
                    ..
                } => {
                    for arg in args.iter_mut() {
                        let repr = func.repr(*arg);
                        let value = func.vregs[*arg].value;
                        let copy = func.new_vreg(repr, value);
                        out.push(LirInst::Move {
                            dst: copy,
                            src: *arg,
                        });
                        *arg = copy;
                    }
                }
                // `IntArith` computes through the emitter's scratch register,
                // so `dst` is an ordinary definition when slot coloring is on.
                // `FloatArith` really does accumulate in the left operand's
                // register and keeps its copy.
                LirInst::IntArith { .. } => {}
                LirInst::FloatArith { dst, lhs, .. } => {
                    out.push(LirInst::Move {
                        dst: *dst,
                        src: *lhs,
                    });
                    *lhs = *dst;
                }
                _ => {}
            }
            out.push(inst);
        }
        func.blocks[b].insts = out;
        legalize_tail_call(func, b);
    }
}

fn legalize_tail_call(func: &mut LirFunc, b: LBlock) {
    let args = match &func.blocks[b].term {
        Some(LirTerm::TailCall {
            target: CallTarget::Helper(_),
            args,
            ..
        }) => args.clone(),
        _ => return,
    };
    let mut seen = HashSet::new();
    for (i, &arg) in args.iter().enumerate() {
        if seen.insert(arg) {
            continue;
        }
        let repr = func.repr(arg);
        let value = func.vregs[arg].value;
        let copy = func.new_vreg(repr, value);
        func.blocks[b].insts.push(LirInst::Move {
            dst: copy,
            src: arg,
        });
        if let Some(LirTerm::TailCall { args, .. }) = &mut func.blocks[b].term {
            args[i] = copy;
        }
    }
}

pub(crate) fn split_critical_edges(func: &mut LirFunc) {
    let mut preds: SecondaryMap<LBlock, u32> = SecondaryMap::new();
    for (_, data) in func.blocks.iter() {
        if let Some(term) = &data.term {
            for e in term.edges() {
                preds[e.block] += 1;
            }
        }
    }
    let blocks: Vec<LBlock> = func.blocks.keys().collect();
    let mut splits = Vec::new();
    for &b in &blocks {
        let Some(term) = &func.blocks[b].term else {
            continue;
        };
        let edges = term.edges();
        if edges.len() < 2 {
            continue;
        }
        for (i, e) in edges.iter().enumerate() {
            if preds[e.block] > 1 {
                splits.push((b, i, e.block, e.args.clone()));
            }
        }
    }
    for (b, i, target, args) in splits {
        let mid = func.blocks.push(LirBlock {
            params: Vec::new(),
            insts: Vec::new(),
            term: Some(LirTerm::Jump(LEdge {
                block: target,
                args,
            })),
            source: None,
        });
        let term = func.blocks[b].term.as_mut().expect("checked above");
        let mut edges = edges_mut(term);
        *edges[i] = LEdge {
            block: mid,
            args: Vec::new(),
        };
    }
}

/// A fixed piece of a vreg: it must be in `assign` throughout `[lo, hi)`.
#[derive(Clone, Copy)]
struct Special {
    lo: u32,
    hi: u32,
    assign: Assign,
}

fn phys_for(regs: &RegFile, role: PhysRole) -> Result<PhysReg, String> {
    match role {
        PhysRole::Rv => Ok(regs.rv),
        PhysRole::OpArg(n) => regs
            .op_args
            .get(n as usize)
            .copied()
            .ok_or_else(|| format!("no register for helper argument {n}")),
    }
}

pub(crate) fn merge(mut ranges: Vec<(u32, u32)>) -> Vec<(u32, u32)> {
    ranges.sort_unstable();
    let mut out: Vec<(u32, u32)> = Vec::with_capacity(ranges.len());
    for (lo, hi) in ranges {
        if let Some(last) = out.last_mut() {
            if lo <= last.1 {
                last.1 = last.1.max(hi);
                continue;
            }
        }
        out.push((lo, hi));
    }
    out
}

fn push_child(built: &mut Built, vreg: VReg, ranges: Vec<(u32, u32)>, assign: Option<Assign>) {
    let id = built.children.len();
    built.children.push(Child {
        vreg,
        ranges,
        assign,
    });
    built.vreg_children[vreg].push(id);
}

pub(crate) fn copy_hints(
    func: &LirFunc,
    num: &Numbering,
    live: &Liveness,
) -> SecondaryMap<VReg, Vec<Hint>> {
    let mut hints: SecondaryMap<VReg, Vec<Hint>> = SecondaryMap::new();
    for &pb in &num.order {
        let Some(term) = &func.blocks[pb].term else {
            continue;
        };
        let tpos = num.term_pos(pb);
        for edge in term.edges() {
            let s = edge.block;
            let spos = num.from(s);
            let params = &func.blocks[s].params;
            for (i, &arg) in edge.args.iter().enumerate() {
                let Some(&param) = params.get(i) else {
                    break;
                };
                if func.class(arg) != func.class(param) {
                    continue;
                }
                hints[arg].push(Hint {
                    pos: tpos,
                    partner: param,
                    partner_pos: spos,
                });
                hints[param].push(Hint {
                    pos: spos,
                    partner: arg,
                    partner_pos: tpos,
                });
            }
            // A value that is live into the target without being one of its
            // parameters is copied from its predecessor piece to its target
            // piece. Bias both halves to one register, so a loop-carried value
            // does not rotate on every back edge.
            for i in live.live_in[s].iter() {
                let v = VReg::new(i);
                if params.contains(&v) {
                    continue;
                }
                hints[v].push(Hint {
                    pos: spos,
                    partner: v,
                    partner_pos: tpos,
                });
                hints[v].push(Hint {
                    pos: tpos,
                    partner: v,
                    partner_pos: spos,
                });
            }
        }
    }
    for &b in &num.order {
        let base = num.base[b];
        for (k, inst) in func.blocks[b].insts.iter().enumerate() {
            let LirInst::Move { dst, src } = inst else {
                continue;
            };
            if func.class(*dst) != func.class(*src) {
                continue;
            }
            let use_pos = 2 * (base + k as u32);
            let def_pos = use_pos + 1;
            hints[*dst].push(Hint {
                pos: def_pos,
                partner: *src,
                partner_pos: use_pos,
            });
            hints[*src].push(Hint {
                pos: use_pos,
                partner: *dst,
                partner_pos: def_pos,
            });
        }
    }
    hints
}

/// Computes live segments and the initial pieces.
pub(crate) fn build(
    func: &LirFunc,
    num: &Numbering,
    live: &Liveness,
    regs: &RegFile,
) -> Result<Built, String> {
    let mut segs: SecondaryMap<VReg, Vec<(u32, u32)>> = SecondaryMap::new();
    let mut specials: SecondaryMap<VReg, Vec<Special>> = SecondaryMap::new();
    let mut fixed: BTreeMap<PhysReg, Vec<(u32, u32)>> = BTreeMap::new();
    let mut uses: SecondaryMap<VReg, Vec<u32>> = SecondaryMap::new();
    let mut call_safe: SecondaryMap<VReg, Vec<u32>> = SecondaryMap::new();
    let call_clobbered = regs.call_clobbered();

    for &b in &num.order {
        let data = &func.blocks[b];
        let from = num.from(b);
        let to = num.to(b);
        // Vregs live at the current point, with the end of their open segment.
        let mut open: HashMap<VReg, u32> = live.live_out[b]
            .iter()
            .map(|i| (VReg::new(i), to))
            .collect();

        let tpos = num.term_pos(b);
        let Some(term) = &data.term else {
            return Err(format!("{b} has no terminator"));
        };
        {
            let mut frame: Vec<VReg> = Vec::new();
            if let LirTerm::Deopt { exit } = term {
                frame.extend(func.frame_regs(&func.exits[*exit].values));
            }
            for op in term_ops(func, term) {
                if op.role != Role::Use {
                    continue;
                }
                if let Some(i) = frame.iter().position(|&v| v == op.vreg) {
                    frame.swap_remove(i);
                } else {
                    uses[op.vreg].push(tpos);
                }
            }
        }
        for op in term_ops(func, term) {
            if op.role == Role::Use {
                if let Constraint::Fixed(role) = op.constraint {
                    let reg = phys_for(regs, role)?;
                    specials[op.vreg].push(Special {
                        lo: tpos,
                        hi: tpos + 1,
                        assign: Assign::Reg(reg),
                    });
                    // Linear scan only sees a pre-assigned piece through
                    // `fixed`. Without this, a dead block parameter defined
                    // at the same position can take the same register, and
                    // its edge move then overwrites the returned value.
                    fixed.entry(reg).or_default().push((tpos, tpos + 1));
                }
                open.entry(op.vreg).or_insert(tpos + 1);
            }
        }

        let base = num.base[b];
        for (k, inst) in data.insts.iter().enumerate().rev() {
            let g = base + k as u32;
            let use_pos = 2 * g;
            let def_pos = use_pos + 1;
            let ops = inst_ops(func, inst);
            {
                let mut frame: Vec<VReg> = Vec::new();
                if let Some(e) = inst.deopt_exit() {
                    frame.extend(func.frame_regs(&func.exits[e].values));
                }
                if let Some(sp) = inst.safepoint() {
                    frame.extend(func.frame_regs(&func.safepoints[sp].values));
                }
                for op in ops.iter().filter(|o| o.role == Role::Use) {
                    if let Some(i) = frame.iter().position(|&v| v == op.vreg) {
                        frame.swap_remove(i);
                    } else {
                        uses[op.vreg].push(use_pos);
                    }
                }
            }
            for op in ops.iter().filter(|o| o.role == Role::Def) {
                let hi = open.remove(&op.vreg).unwrap_or(def_pos + 1);
                segs[op.vreg].push((def_pos, hi));
                if let Constraint::Fixed(role) = op.constraint {
                    let reg = phys_for(regs, role)?;
                    specials[op.vreg].push(Special {
                        lo: def_pos,
                        hi: def_pos + 1,
                        assign: Assign::Reg(reg),
                    });
                    fixed.entry(reg).or_default().push((def_pos, def_pos + 1));
                }
            }
            for op in ops.iter().filter(|o| o.role == Role::Use) {
                open.entry(op.vreg).or_insert(use_pos + 1);
                if op.call_safe {
                    call_safe[op.vreg].push(use_pos);
                }
                if op.slot {
                    specials[op.vreg].push(Special {
                        lo: use_pos,
                        hi: use_pos + 1,
                        assign: Assign::Stack,
                    });
                } else if let Constraint::Fixed(role) = op.constraint {
                    let reg = phys_for(regs, role)?;
                    specials[op.vreg].push(Special {
                        lo: use_pos,
                        hi: use_pos + 1,
                        assign: Assign::Reg(reg),
                    });
                    fixed.entry(reg).or_default().push((use_pos, use_pos + 1));
                }
            }
            match inst.clobbers() {
                Clobbers::All => {
                    for &r in regs.gprs.iter().chain(&regs.fprs) {
                        fixed.entry(r).or_default().push((def_pos, def_pos + 1));
                    }
                }
                Clobbers::RustCall => {
                    for &r in &call_clobbered {
                        fixed.entry(r).or_default().push((def_pos, def_pos + 1));
                    }
                }
                Clobbers::Slow => {
                    for &r in &regs.slow_clobbers {
                        fixed.entry(r).or_default().push((use_pos, def_pos + 1));
                    }
                }
                Clobbers::None => {}
            }
        }

        // Parameters are defined at the block start.
        let mut flushed = HashSet::new();
        for &p in &data.params {
            let hi = open.remove(&p).unwrap_or(from + 1);
            segs[p].push((from, hi));
            flushed.insert(p);
        }
        for (v, hi) in open.drain() {
            if !flushed.contains(&v) {
                segs[v].push((from, hi));
            }
        }
    }

    for ranges in fixed.values_mut() {
        *ranges = merge(std::mem::take(ranges));
    }

    let mut built = Built {
        children: Vec::new(),
        vreg_children: SecondaryMap::new(),
        fixed,
        uses: SecondaryMap::new(),
        hints: copy_hints(func, num, live),
        call_safe,
    };
    for v in func.vregs.keys() {
        built.call_safe[v].sort_unstable();
    }
    for v in func.vregs.keys() {
        let mut u = std::mem::take(&mut uses[v]);
        u.sort_unstable();
        u.dedup();
        built.uses[v] = u;
    }
    for v in func.vregs.keys() {
        let ss = merge(std::mem::take(&mut segs[v]));
        let mut sps = std::mem::take(&mut specials[v]);
        if ss.is_empty() && sps.is_empty() {
            continue;
        }
        sps.sort_by_key(|s| s.lo);
        // A safepoint can list one vreg in several frame slots. Each listing is
        // the same constraint at the same position, so one is enough.
        sps.dedup_by(|later, kept| {
            later.lo == kept.lo && later.hi == kept.hi && later.assign == kept.assign
        });
        let mut si = 0;
        for &(lo, hi) in &ss {
            let mut cur = lo;
            while si < sps.len() && sps[si].lo < hi {
                let sp = sps[si];
                if sp.lo < cur || sp.hi > hi {
                    return Err(format!("conflicting constraints on {v} at {}", sp.lo));
                }
                if sp.lo > cur {
                    push_child(&mut built, v, vec![(cur, sp.lo)], None);
                }
                push_child(&mut built, v, vec![(sp.lo, sp.hi)], Some(sp.assign));
                cur = sp.hi;
                si += 1;
            }
            if cur < hi {
                push_child(&mut built, v, vec![(cur, hi)], None);
            }
        }
        if si != sps.len() {
            return Err(format!("a constraint on {v} lies outside its live range"));
        }
    }
    Ok(built)
}
