//! Stack maps: the tagged spill slots live at each safepoint, and where each
//! deopt value lives at its exit.

use std::collections::{HashMap, HashSet};

use cranelift_entity::{PrimaryMap, SecondaryMap};

use super::assign::{Assignment, covers};
use super::liveness::Numbering;
use super::pos;
use super::{DeoptLoc, Loc, PhysReg, RegFile, RematDesc};
use crate::runtime::codegen::opt::ir::inst::Constant;
use crate::runtime::codegen::opt::lir::{
    DeoptExit, LirFunc, LirInst, LirTerm, LiveMask, RegClass, RegMask, Role, SafePoint, VReg, VRepr,
};

/// A general-purpose register that a yieldpoint keeps its value in across the
/// yield. The slow path stores the register to `slot` before the call and
/// reloads it after, so the mask of the yieldpoint names `slot` when the value
/// is a root (tagged), and the slot is not scanned for a raw value.
#[derive(Clone, Copy, Debug)]
pub struct PollSave {
    pub vreg: VReg,
    pub reg: PhysReg,
    pub slot: u32,
    pub root: bool,
}

/// Reserves the save slots of every yieldpoint.
pub(crate) fn reserve_poll_saves(
    func: &LirFunc,
    num: &Numbering,
    asg: &mut Assignment,
) -> SecondaryMap<SafePoint, Vec<PollSave>> {
    let mut found: Vec<(SafePoint, Vec<PollSave>)> = Vec::new();
    for &b in &num.order {
        let data = &func.blocks[b];
        let base = num.base[b];
        for (k, inst) in data.insts.iter().enumerate() {
            let LirInst::Poll { safepoint } = inst else {
                continue;
            };
            let pos = pos::early(base + k as u32);
            let mut saves = Vec::new();
            for v in func.vregs.keys() {
                if func.class(v) != RegClass::Gpr {
                    continue;
                }
                if let Some(Loc::Reg(reg)) = asg.loc_at(v, pos) {
                    saves.push(PollSave {
                        vreg: v,
                        reg,
                        slot: 0,
                        root: func.is_gc_root(v),
                    });
                }
            }
            found.push((*safepoint, saves));
        }
    }
    let width = found.iter().map(|(_, s)| s.len()).max().unwrap_or(0) as u32;
    let first = asg.spill_slots;
    asg.spill_slots += width;
    let mut out: SecondaryMap<SafePoint, Vec<PollSave>> = SecondaryMap::new();
    for (sp, mut saves) in found {
        for (i, save) in saves.iter_mut().enumerate() {
            save.slot = first + i as u32;
        }
        out[sp] = saves;
    }
    out
}

/// Fills `livemask` of every safepoint and returns the deopt locations and
/// remat descriptors of every exit.
pub(crate) fn build(
    func: &mut LirFunc,
    num: &Numbering,
    asg: &Assignment,
    regs: &RegFile,
    polls: &SecondaryMap<SafePoint, Vec<PollSave>>,
) -> Result<
    (
        PrimaryMap<DeoptExit, Vec<DeoptLoc>>,
        PrimaryMap<DeoptExit, Vec<RematDesc>>,
    ),
    String,
> {
    /// What kind of instruction a safepoint belongs to.
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Kind {
        Poll,
        FrameCall,
        RustCall,
    }

    let mut sp_at: Vec<(SafePoint, u32, Kind)> = Vec::new();
    let mut exit_at: SecondaryMap<DeoptExit, Option<u32>> = SecondaryMap::new();
    let mut defs: SecondaryMap<VReg, u32> = SecondaryMap::new();
    let mut konst: SecondaryMap<VReg, Option<Constant>> = SecondaryMap::new();
    for &b in &num.order {
        let data = &func.blocks[b];
        let base = num.base[b];
        for (k, inst) in data.insts.iter().enumerate() {
            let g = base + k as u32;
            if let Some(sp) = inst.safepoint() {
                let kind = match inst {
                    LirInst::Poll { .. } => Kind::Poll,
                    LirInst::Call {
                        target:
                            crate::runtime::codegen::opt::lir::CallTarget::Helper(_)
                            | crate::runtime::codegen::opt::lir::CallTarget::Prim(_)
                            | crate::runtime::codegen::opt::lir::CallTarget::PrimN(_),
                        ..
                    } => Kind::RustCall,
                    LirInst::Call { .. } => Kind::FrameCall,
                    other => {
                        return Err(format!(
                            "safepoint {sp} belongs to an instruction that is not a call or poll: {other}"
                        ));
                    }
                };
                sp_at.push((sp, g, kind));
            }
            if let Some(e) = inst.deopt_exit() {
                exit_at[e] = Some(g);
            }
            if let LirInst::Const { dst, value } = inst {
                konst[*dst] = Some(value.clone());
            }
            for op in inst.operands(func) {
                if op.role == Role::Def {
                    defs[op.vreg] += 1;
                }
            }
        }
        if let Some(term) = &data.term {
            if let LirTerm::Deopt { exit } = term {
                exit_at[*exit] = Some(num.term[b]);
            }
            for op in term.operands(func) {
                if op.role == Role::Def {
                    defs[op.vreg] += 1;
                }
            }
        }
    }
    // A constant is rebuilt from its immediate when it has one definition.
    let const_of =
        |v: VReg| -> Option<Constant> { if defs[v] == 1 { konst[v].clone() } else { None } };

    let words = (asg.spill_slots as usize).div_ceil(64).max(1);
    let mut masks: Vec<(SafePoint, LiveMask, RegMask)> = Vec::with_capacity(sp_at.len());
    for &(sp, g, kind) in &sp_at {
        let mut w = vec![0u64; words];
        let set = |w: &mut Vec<u64>, s: u32| w[s as usize / 64] |= 1u64 << (s % 64);
        let mut regmask = RegMask::default();
        for v in func.vregs.keys() {
            if func.class(v) != crate::runtime::codegen::opt::lir::RegClass::Gpr
                || !func.is_gc_root(v)
            {
                continue;
            }
            for &id in &asg.vreg_children[v] {
                let c = &asg.children[id];
                if c.assign == Some(super::intervals::Assign::Stack) && covers(&c.ranges, pos::late(g))
                {
                    if let Some(s) = asg.slot_of[v] {
                        set(&mut w, s);
                    }
                }
            }

            match kind {
                Kind::Poll => {
                    if let Some(Loc::Reg(r)) = asg.loc_at(v, pos::late(g)) {
                        regmask.set(r.0);
                    }
                }

                Kind::RustCall => {
                    if let Some(Loc::Reg(r)) = asg.loc_at(v, pos::early(g)) {
                        if regs.call_preserved(r) {
                            regmask.set(r.0);
                        }
                    }
                }

                Kind::FrameCall => {}
            }
        }
        // The root saves of a yieldpoint are in its mask: the slow path keeps
        // them current before the yield.
        for save in polls[sp].iter().filter(|s| s.root) {
            set(&mut w, save.slot);
        }

        for vreg in func.frame_regs(&func.safepoints[sp].values) {
            if !func.is_gc_root(vreg) {
                continue;
            }
            match asg.loc_at(vreg, pos::early(g)) {
                Some(Loc::Slot(s)) => set(&mut w, s),
                Some(Loc::Reg(_)) if polls[sp].iter().any(|s| s.vreg == vreg) => {
                    let save = polls[sp].iter().find(|s| s.vreg == vreg).unwrap();
                    set(&mut w, save.slot)
                }
                Some(Loc::Reg(r)) if kind == Kind::RustCall && regs.call_preserved(r) => {}
                _ => return Err(format!("frame value {vreg} of {sp} is not in a spill slot")),
            }
        }
        masks.push((sp, LiveMask { words: w }, regmask));
    }
    for (sp, mask, regmask) in masks {
        func.safepoints[sp].livemask = Some(mask);
        func.safepoints[sp].regmask = Some(regmask);
    }

    let mut out: PrimaryMap<DeoptExit, Vec<DeoptLoc>> = PrimaryMap::new();
    let mut remat_out: PrimaryMap<DeoptExit, Vec<RematDesc>> = PrimaryMap::new();
    for e in func.exits.keys() {
        let g = exit_at[e].ok_or_else(|| format!("{e} is not attached to an instruction"))?;
        let mut builder = ExitLocs {
            func,
            asg,
            const_of: &const_of,
            pos: pos::early(g),
            exit: e,
            table: Vec::new(),
            memo: HashMap::new(),
            visiting: HashSet::new(),
        };
        let mut locs = Vec::with_capacity(func.exits[e].values.len());
        for fv in &func.exits[e].values {
            locs.push(builder.loc(fv.vreg)?);
        }
        out.push(locs);
        remat_out.push(builder.table);
    }
    Ok((out, remat_out))
}

/// Builds one exit's locations, expanding remat markers into descriptors.
struct ExitLocs<'a> {
    func: &'a LirFunc,
    asg: &'a Assignment,
    const_of: &'a dyn Fn(VReg) -> Option<Constant>,
    pos: u32,
    exit: DeoptExit,
    /// The descriptors of this exit. `Materialize(i)` names entry `i`.
    table: Vec<RematDesc>,
    /// The descriptor id of each marker vreg already built at this exit.
    memo: HashMap<VReg, u32>,
    /// Markers currently being expanded, to reject a descriptor cycle.
    visiting: HashSet<VReg>,
}

impl ExitLocs<'_> {
    /// Where the value of `vreg` lives, or the descriptor that rebuilds it.
    fn loc(&mut self, vreg: VReg) -> Result<DeoptLoc, String> {
        if self.func.vregs[vreg].remat.is_some() {
            return Ok(DeoptLoc::Materialize(self.materialize(vreg)?));
        }
        let repr = self.func.repr(vreg);
        if let Some(c) = self.func.vregs[vreg].imm {
            return Ok(DeoptLoc::Const(c));
        }
        if repr == VRepr::Tagged {
            if let Some(c) = (self.const_of)(vreg) {
                return Ok(DeoptLoc::Const(c));
            }
        }
        let loc = self
            .asg
            .loc_at(vreg, self.pos)
            .ok_or_else(|| format!("{vreg} is not live at {}", self.exit))?;
        Ok(match repr {
            VRepr::Double => DeoptLoc::UnboxedDouble(loc),
            VRepr::RawInt => DeoptLoc::UnboxedI32(loc),
            // A raw int64 that leaves i32 is reboxed by allocating a bignum
            // in the deopt runtime (see `deopt::runtime`).
            VRepr::RawInt64 => DeoptLoc::UnboxedI64(loc),
            // A two-limb value names two registers, and no builder path puts
            // one in a frame state
            VRepr::RawInt128 => {
                return Err(format!(
                    "{} holds a two-limb Int128 at {}, which a deopt cannot rebuild",
                    vreg, self.exit
                ));
            }
            VRepr::Tagged => match loc {
                Loc::Reg(r) => DeoptLoc::Reg(r),
                Loc::Slot(s) => DeoptLoc::Slot(s),
            },
            // A raw flag has no Scheme value to rebuild the frame with (a
            // boolean would have to become #t or #f first).
            VRepr::RawBits => {
                return Err(format!(
                    "{} holds raw bits at {}, which a deopt cannot rebuild",
                    vreg, self.exit
                ));
            }
        })
    }

    fn materialize(&mut self, vreg: VReg) -> Result<u32, String> {
        if let Some(&id) = self.memo.get(&vreg) {
            return Ok(id);
        }
        let rid = self.func.vregs[vreg]
            .remat
            .ok_or_else(|| format!("{vreg} is not a remat marker"))?;
        if !self.visiting.insert(vreg) {
            return Err(format!(
                "{vreg} has a cyclic remat descriptor at {}",
                self.exit
            ));
        }
        let id = self.table.len() as u32;
        let kind = self.func.remats[rid].kind;
        // Reserve the id before expanding the fields so that a nested entry
        // appended by the recursion does not shift it.
        self.table.push(RematDesc {
            kind,
            fields: Vec::new(),
        });
        let fields = self.func.remats[rid].fields.clone();
        let mut locs = Vec::with_capacity(fields.len());
        for field in fields {
            locs.push(self.loc(field)?);
        }
        self.table[id as usize].fields = locs;
        self.visiting.remove(&vreg);
        self.memo.insert(vreg, id);
        Ok(id)
    }
}
