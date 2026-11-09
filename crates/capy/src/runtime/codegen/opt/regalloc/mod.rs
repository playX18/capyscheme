//! Register allocation for the Tier 2 LIR.
//!
//! - [`linear_scan`]: Wimmer–Franz linear scan, the default.
//! - [`greedy`]: whole-function greedy allocation based on LLVM/WebKit Greedy algorithm.
//!
//!
//! [`allocate`] rewrites the function (copies, split edges, livemasks) and
//! returns an [`Allocation`]. The register pool comes in as a [`RegFile`].
//! The emitter maps its abstract [`PhysReg`] ids to machine registers.

mod assign;
mod flowcheck;
mod greedy;
mod intervals;
mod linear_scan;
mod liveness;
mod loops;
pub(crate) mod pos;
mod resolve;
mod slots;
mod split;
mod stackmap;
pub(crate) mod stats;
mod weights;

use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter, Write as _};

use cranelift_entity::{PrimaryMap, SecondaryMap};

use crate::runtime::codegen::jit::config::{RegAllocKind, opt};
use crate::runtime::codegen::opt::ir::inst::{Constant, RematKind};
use crate::runtime::codegen::opt::lir::{
    Clobbers, Constraint, DeoptExit, LBlock, LirFunc, LirInst, PhysRole, RegClass, Role, SafePoint,
    VReg,
};
use crate::utils::flags;

/// An abstract physical register
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysReg(pub u16);

impl Display for PhysReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "p{}", self.0)
    }
}

/// The registers of one target.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegFile {
    /// Allocatable general-purpose registers, in preference order.
    pub gprs: Vec<PhysReg>,
    /// Allocatable floating-point registers.
    pub fprs: Vec<PhysReg>,
    /// The return value register ([`PhysRole::Rv`]).
    pub rv: PhysReg,
    /// The helper argument registers ([`PhysRole::OpArg`]).
    pub op_args: Vec<PhysReg>,
    /// Registers that an out-of-line slow path destroys
    /// ([`Clobbers::Slow`](crate::runtime::codegen::opt::lir::Clobbers::Slow)).
    pub slow_clobbers: Vec<PhysReg>,
    /// Pool registers that a Rust helper call preserves (the target C ABI's
    /// callee-saved set). Empty means every register is call-clobbered.
    pub callee_saved: Vec<PhysReg>,
}

impl RegFile {
    fn pool(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RegClass::Gpr => &self.gprs,
            RegClass::Fpr => &self.fprs,
        }
    }

    /// Whether a Rust helper call preserves `reg`.
    fn call_preserved(&self, reg: PhysReg) -> bool {
        self.callee_saved.contains(&reg)
    }

    /// The pool registers a Rust helper call destroys: the caller-saved set.
    fn call_clobbered(&self) -> Vec<PhysReg> {
        self.gprs
            .iter()
            .chain(&self.fprs)
            .copied()
            .filter(|&r| !self.call_preserved(r))
            .collect()
    }

    fn validate(&self, scratch: Scratch) -> Result<(), String> {
        for (i, r) in self.gprs.iter().enumerate() {
            if self.gprs[..i].contains(r) || self.fprs.contains(r) {
                return Err(format!("{r} appears twice in the register pool"));
            }
        }
        for (i, r) in self.fprs.iter().enumerate() {
            if self.fprs[..i].contains(r) {
                return Err(format!("{r} appears twice in the register pool"));
            }
        }
        for r in [scratch.gpr, scratch.fpr] {
            if self.gprs.contains(&r) || self.fprs.contains(&r) {
                return Err(format!("the scratch register {r} is allocatable"));
            }
        }
        for r in &self.callee_saved {
            if !self.gprs.contains(r) && !self.fprs.contains(r) {
                return Err(format!("the callee-saved register {r} is not allocatable"));
            }
        }
        if scratch.gpr == scratch.fpr {
            return Err("the GPR and FPR scratch registers are the same".to_string());
        }
        Ok(())
    }
}

/// Scratch registers outside the pool. Cycles of moves are broken through
/// them, one per register class.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Scratch {
    pub gpr: PhysReg,
    pub fpr: PhysReg,
}

/// Where a value lives.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Loc {
    Reg(PhysReg),
    /// A spill slot, numbered from 0. The emitter maps slots to frame words.
    Slot(u32),
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Loc::Reg(r) => write!(f, "{r}"),
            Loc::Slot(s) => write!(f, "[s{s}]"),
        }
    }
}

/// A move of one value. Slot-to-slot moves are possible, and the emitter
/// performs them through its own scratch.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Move {
    pub class: RegClass,
    pub src: Loc,
    pub dst: Loc,
    /// A reload of a constant: the emitter rebuilds the constant
    /// `Allocation::consts[i]` in `dst` instead of reading `src`. The slot
    /// still holds the value, so the move keeps its meaning for the checkers.
    pub konst: Option<u32>,
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(i) = self.konst {
            return write!(f, "{} <- #const{i}", self.dst);
        }
        write!(f, "{} <- {}", self.dst, self.src)
    }
}

/// Where a deopt value lives at its exit. An unboxed value is boxed while the
/// runtime rebuilds the frame.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeoptLoc {
    Reg(PhysReg),
    Slot(u32),
    Const(Constant),
    UnboxedDouble(Loc),
    UnboxedI32(Loc),
    /// A raw int64. The deopt runtime reboxes it: a fixnum when it fits
    /// `i32`, otherwise a bignum allocated during frame reconstruction.
    UnboxedI64(Loc),
    /// A sunk object. The id indexes the exit's remat table; the deopt
    /// runtime rebuilds the object from that descriptor before writing any
    /// frame.
    Materialize(u32),
}

/// One entry of an exit's remat table: the object to rebuild and where its
/// fields live.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RematDesc {
    pub kind: RematKind,
    pub fields: Vec<DeoptLoc>,
}

impl Display for DeoptLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeoptLoc::Reg(r) => write!(f, "{r}"),
            DeoptLoc::Slot(s) => write!(f, "[s{s}]"),
            DeoptLoc::Const(c) => write!(f, "const {c}"),
            DeoptLoc::UnboxedDouble(l) => write!(f, "f64 {l}"),
            DeoptLoc::UnboxedI32(l) => write!(f, "i32 {l}"),
            DeoptLoc::UnboxedI64(l) => write!(f, "i64 {l}"),
            DeoptLoc::Materialize(id) => write!(f, "materialize[{id}]"),
        }
    }
}

/// A run of positions where a vreg has one location.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Piece {
    /// Sorted, disjoint half-open position ranges.
    pub ranges: Vec<(u32, u32)>,
    pub loc: Loc,
}

/// The result of allocation.
pub struct Allocation {
    /// Blocks in linear order.
    pub order: Vec<LBlock>,
    /// Global index of the first instruction of each block.
    pub base: SecondaryMap<LBlock, u32>,
    /// Spill slots used. Each slot is one word. This includes the slots
    /// reserved for the yieldpoint register saves.
    pub spill_slots: u32,
    /// Spill slots assigned by linear scan, before `reserve_poll_saves`
    /// adds the poll save slots. The install guard budgets this count so
    /// a unit is not rejected for the poll saves every unit pays for.
    pub spill_slots_before_polls: u32,
    /// Moves before each instruction, by global index. Each block's terminator
    /// is its last instruction.
    pub moves: Vec<Vec<Move>>,
    /// The constants that `Move::konst` rebuilds.
    pub consts: Vec<Constant>,
    /// The pieces of each vreg.
    pub pieces: SecondaryMap<VReg, Vec<Piece>>,
    /// The deopt locations of each exit, in the order of its frame values.
    pub deopt: PrimaryMap<DeoptExit, Vec<DeoptLoc>>,
    /// The remat descriptors of each exit, indexed by `DeoptLoc::Materialize`.
    pub remat: PrimaryMap<DeoptExit, Vec<RematDesc>>,
    /// The registers each yieldpoint saves across its yield, and their slots.
    pub polls: SecondaryMap<SafePoint, Vec<stackmap::PollSave>>,
    /// The representative of the affinity group of each coalesced vreg.
    pub group: SecondaryMap<VReg, Option<VReg>>,
}

impl Allocation {
    /// The vreg that stands for `v` when values are compared: coalesced vregs
    /// are one value.
    pub(crate) fn canon(&self, v: VReg) -> VReg {
        self.group[v].unwrap_or(v)
    }

    /// The global index of instruction `k` of `block`. `k` equals the number
    /// of instructions for the terminator.
    pub fn index(&self, block: LBlock, k: u32) -> u32 {
        self.base[block] + k
    }

    /// The moves to run before instruction `k` of `block`.
    pub fn moves_before(&self, block: LBlock, k: u32) -> &[Move] {
        &self.moves[self.index(block, k) as usize]
    }

    /// The location of `vreg` at `pos`, if it is live there.
    pub fn loc_at(&self, vreg: VReg, pos: u32) -> Option<Loc> {
        self.pieces[vreg]
            .iter()
            .find(|p| p.ranges.iter().any(|&(lo, hi)| lo <= pos && pos < hi))
            .map(|p| p.loc)
    }
}

/// Allocates registers for `func`.
pub fn allocate(
    func: &mut LirFunc,
    regs: &RegFile,
    scratch: Scratch,
) -> Result<Allocation, String> {
    regs.validate(scratch)?;
    intervals::legalize(func);
    intervals::split_critical_edges(func);
    let num = liveness::Numbering::new(func)?;
    let live = liveness::compute(func, &num)?;
    let built = intervals::build(func, &num, &live, regs)?;
    let weights = weights::Weights::compute(func, &num);
    let cfg = opt();

    // Compare mode: every allocator named after the active one runs the same
    // post-pass and the same verifier, but only the active one is kept. They
    // run first because the post-pass writes the safepoint masks of `func`.
    for &other in cfg.regalloc_compare() {
        if let Err(err) = compare(other, func, &num, &live, &built, regs, &weights, scratch) {
            crate::heap::logging::ensure_max_level(log::LevelFilter::Warn);
            log::warn!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "regalloc {} failed: {err}",
                other.name()
            );
            stats::record_failure(other);
        }
    }

    let started = std::time::Instant::now();
    let asg = run_one(cfg.regalloc(), func, &num, &built, regs, &weights)?;
    let alloc = complete(func, &num, &live, asg, regs, scratch)?;
    if stats::enabled() {
        let micros = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX);
        stats::record(cfg.regalloc(), &alloc, &weights, micros);
    }
    if cfg.verify {
        if let Err(err) = check(func, &num, &alloc, regs) {
            // Without this a rejected allocation leaves no trace: the unit is
            // simply never installed, and the reason is lost.
            if cfg.dump.iter().any(|stage| stage == "regalloc") {
                crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
                log::error!("allocation rejected: {err}\n{}", display(func, &alloc));
            }
            return Err(err);
        }
    }
    Ok(alloc)
}

/// Runs one allocator. Every allocator takes the same inputs and returns the
/// same placement, so the choice is a single dispatch.
fn run_one(
    kind: RegAllocKind,
    func: &LirFunc,
    num: &liveness::Numbering,
    built: &intervals::Built,
    regs: &RegFile,
    weights: &weights::Weights,
) -> Result<assign::Assignment, String> {
    match kind {
        RegAllocKind::LinearScan => linear_scan::run(func, num, built, regs, weights),
        RegAllocKind::Greedy => greedy::run(func, num, built, regs, weights),
    }
}

/// Runs a non-active allocator for its measurements: the same post-pass and
/// verifier as the active one, with the result dropped.
fn compare(
    kind: RegAllocKind,
    func: &mut LirFunc,
    num: &liveness::Numbering,
    live: &liveness::Liveness,
    built: &intervals::Built,
    regs: &RegFile,
    weights: &weights::Weights,
    scratch: Scratch,
) -> Result<(), String> {
    let started = std::time::Instant::now();
    let asg = run_one(kind, func, num, built, regs, weights)?;
    let alloc = complete(func, num, live, asg, regs, scratch)?;
    if let Err(err) = check(func, num, &alloc, regs) {
        if opt().dump.iter().any(|stage| stage == "regalloc") {
            crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
            log::error!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "allocation rejected ({}): {err}\n{}",
                kind.name(),
                display(func, &alloc)
            );
        }
        return Err(err);
    }
    let micros = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX);
    if stats::enabled() {
        stats::record(kind, &alloc, weights, micros);
    } else {
        log::debug!(
            "regalloc {}: {} spill slots, {} moves, {micros} us",
            kind.name(),
            alloc.spill_slots,
            alloc.moves.iter().map(Vec::len).sum::<usize>(),
        );
    }
    Ok(())
}

fn complete(
    func: &mut LirFunc,
    num: &liveness::Numbering,
    live: &liveness::Liveness,
    mut asg: assign::Assignment,
    regs: &RegFile,
    scratch: Scratch,
) -> Result<Allocation, String> {
    let (moves, consts) = resolve::resolve(func, num, live, &asg, scratch)?;
    let spill_slots_before_polls = asg.spill_slots;
    let polls = stackmap::reserve_poll_saves(func, num, &mut asg);
    let (deopt, remat) = stackmap::build(func, num, &asg, regs, &polls)?;

    let mut pieces: SecondaryMap<VReg, Vec<Piece>> = SecondaryMap::new();
    for v in func.vregs.keys() {
        let mut list = Vec::new();
        for &id in &asg.vreg_children[v] {
            let c = &asg.children[id];
            if let Some(loc) = asg.loc_of_child(c) {
                list.push(Piece {
                    ranges: c.ranges.clone(),
                    loc,
                });
            }
        }
        list.sort_by_key(|p| p.ranges[0].0);
        pieces[v] = list;
    }
    Ok(Allocation {
        order: num.order.clone(),
        base: num.base.clone(),
        spill_slots: asg.spill_slots,
        spill_slots_before_polls,
        moves,
        consts,
        pieces,
        deopt,
        remat,
        polls,
        group: asg.groups.clone(),
    })
}

pub fn check(
    func: &LirFunc,
    num: &liveness::Numbering,
    alloc: &Allocation,
    regs: &RegFile,
) -> Result<(), String> {
    // Pieces that share a location must not overlap.
    let mut by_loc: BTreeMap<(u8, u32), Vec<(u32, u32, VReg)>> = BTreeMap::new();
    for v in func.vregs.keys() {
        for piece in &alloc.pieces[v] {
            let key = match piece.loc {
                Loc::Reg(r) => (0, r.0 as u32),
                Loc::Slot(s) => {
                    if s >= alloc.spill_slots {
                        return Err(format!("{v} uses slot {s} beyond {}", alloc.spill_slots));
                    }
                    (1, s)
                }
            };
            for &(lo, hi) in &piece.ranges {
                by_loc
                    .entry(key)
                    .or_default()
                    .push((lo, hi, alloc.canon(v)));
            }
        }
    }
    for (key, mut list) in by_loc {
        list.sort();
        // Members of one coalesced group may overlap: they are one value.
        let mut cur = list[0];
        for &(lo, hi, v) in &list[1..] {
            if v == cur.2 && lo <= cur.1 {
                cur.1 = cur.1.max(hi);
            } else if lo < cur.1 {
                let loc = match key {
                    (0, r) => format!("p{r}"),
                    (_, s) => format!("[s{s}]"),
                };
                return Err(format!("{} and {} share {loc} at {lo}", cur.2, v));
            } else {
                cur = (lo, hi, v);
            }
        }
    }
    check_coalescing(func, num, alloc)?;

    let in_pool = |r: PhysReg| regs.gprs.contains(&r) || regs.fprs.contains(&r);
    for &b in &alloc.order {
        let data = &func.blocks[b];
        let base = alloc.base[b];
        for (k, inst) in data.insts.iter().enumerate() {
            let g = base + k as u32;
            let ops = intervals::inst_ops(func, inst);
            check_ops(alloc, regs, g, &ops)?;
            check_clobbers(func, alloc, regs, inst, g, &ops, &in_pool)?;
        }
        if let Some(term) = &data.term {
            let g = alloc.index(b, data.insts.len() as u32);
            let ops = intervals::term_ops(func, term);
            check_ops(alloc, regs, g, &ops)?;
        }
    }

    // Livemasks cover the tagged values live across each safepoint.
    for &b in &alloc.order {
        let data = &func.blocks[b];
        for (k, inst) in data.insts.iter().enumerate() {
            let Some(sp) = inst.safepoint() else {
                continue;
            };
            let g = alloc.index(b, k as u32);
            let Some(mask) = &func.safepoints[sp].livemask else {
                return Err(format!("{sp} has no livemask"));
            };
            let has = |s: u32| (mask.words[s as usize / 64] >> (s % 64)) & 1 == 1;
            for v in func.vregs.keys() {
                if func.class(v) != RegClass::Gpr || !func.is_gc_root(v) {
                    continue;
                }
                for piece in &alloc.pieces[v] {
                    let covers = piece
                        .ranges
                        .iter()
                        .any(|&(lo, hi)| lo <= pos::late(g) && pos::late(g) < hi);
                    if let (true, Loc::Slot(s)) = (covers, piece.loc) {
                        if !has(s) {
                            return Err(format!(
                                "{v} is live across {sp} but missing from its livemask"
                            ));
                        }
                    }
                }
            }

            let regmask = func.safepoints[sp]
                .regmask
                .as_ref()
                .ok_or_else(|| format!("{sp} has no register mask"))?;
            let poll = matches!(inst, LirInst::Poll { .. });
            let frame_call = matches!(
                inst,
                LirInst::Call {
                    target: crate::runtime::codegen::opt::lir::CallTarget::Known(_)
                        | crate::runtime::codegen::opt::lir::CallTarget::Generic,
                    ..
                }
            );
            let pos = if poll { pos::late(g) } else { pos::early(g) };
            for v in func.vregs.keys() {
                if func.class(v) != RegClass::Gpr || !func.is_gc_root(v) {
                    continue;
                }
                let Some(Loc::Reg(r)) = alloc.loc_at(v, pos) else {
                    continue;
                };
                let expected = if poll {
                    true
                } else if frame_call {
                    false
                } else {
                    regs.call_preserved(r)
                };
                if regmask.get(r.0) != expected {
                    return Err(format!(
                        "the register mask of {sp} {} {r}, which holds {v}",
                        if expected { "omits" } else { "names" }
                    ));
                }
            }
            for (w, &word) in regmask.words.iter().enumerate() {
                for bit in 0..64 {
                    if (word >> bit) & 1 == 1 {
                        let r = PhysReg((w * 64 + bit) as u16);
                        if !regs.gprs.contains(&r) {
                            return Err(format!(
                                "the register mask of {sp} names {r}, which is not a pool GPR"
                            ));
                        }
                    }
                }
            }
        }
    }

    for v in func.vregs.keys() {
        let pieces = &alloc.pieces[v];
        let is_slot = |l: Loc| matches!(l, Loc::Slot(_));
        for a in pieces {
            for &(_, p) in &a.ranges {
                let Some(b) = pieces.iter().find(|b| b.ranges.iter().any(|r| r.0 == p)) else {
                    continue;
                };
                if std::ptr::eq(a, b) || a.loc == b.loc || pos::inst_of(p) >= num.count {
                    continue;
                }
                let block = num.block_at[pos::inst_of(p) as usize];
                if p == num.from(block) {
                    continue;
                }

                if !num.can_split_at(p) {
                    return Err(format!(
                        "{v} changes location from {} to {} at {p}, which is not a gap",
                        a.loc, b.loc
                    ));
                }

                let g = pos::inst_of(p) as usize;
                if !is_slot(a.loc)
                    && is_slot(b.loc)
                    && !reaches(
                        alloc,
                        if p % pos::POINTS == 0 { g } else { g + 1 },
                        a.loc,
                        b.loc,
                    )
                {
                    continue;
                }

                let at = if p % pos::POINTS == 0 { g } else { g + 1 };

                if !reaches(alloc, at, a.loc, b.loc) {
                    return Err(format!(
                        "no move connects {v} from {} to {} at {p} (expected before {at})",
                        a.loc, b.loc
                    ));
                }
            }
        }
    }
    flowcheck::check(func, num, alloc, regs)
}

/// Whether the parallel move at instruction `at` carries a value from `from`
/// to `to`.
fn reaches(alloc: &Allocation, at: usize, from: Loc, to: Loc) -> bool {
    let Some(moves) = alloc.moves.get(at) else {
        return from == to;
    };
    let mut frontier = vec![from];
    let mut seen: Vec<Loc> = Vec::new();
    while let Some(loc) = frontier.pop() {
        if loc == to {
            return true;
        }
        if seen.contains(&loc) {
            continue;
        }
        seen.push(loc);
        for m in moves {
            if m.src == loc {
                frontier.push(m.dst);
            }
        }
    }
    from == to
}

fn check_ops(
    alloc: &Allocation,
    regs: &RegFile,
    g: u32,
    ops: &[intervals::OpInfo],
) -> Result<(), String> {
    for op in ops {
        let pos = if op.role == Role::Use {
            pos::early(g)
        } else {
            pos::late(g)
        };
        let loc = alloc
            .loc_at(op.vreg, pos)
            .ok_or_else(|| format!("{} has no location at {pos}", op.vreg))?;
        if op.slot && !matches!(loc, Loc::Slot(_)) {
            return Err(format!("frame value {} is not in a slot at {pos}", op.vreg));
        }

        if op.call_safe && !matches!(loc, Loc::Slot(_)) {
            match loc {
                Loc::Reg(r) if regs.call_preserved(r) => {}
                _ => {
                    return Err(format!(
                        "call-safe frame value {} is in {loc} at {pos}",
                        op.vreg
                    ));
                }
            }
        }
        match op.constraint {
            Constraint::Any => {}
            Constraint::Fixed(role) => {
                let want = match role {
                    PhysRole::Rv => regs.rv,
                    PhysRole::OpArg(n) => *regs
                        .op_args
                        .get(n as usize)
                        .ok_or_else(|| format!("no helper register {n}"))?,
                };
                if loc != Loc::Reg(want) {
                    return Err(format!(
                        "{} must be in {want} at {pos}, is in {loc}",
                        op.vreg
                    ));
                }
            }
            Constraint::Tied(_) => {}
        }
    }
    // A two-address op's destination shares the register of its left operand.
    for op in ops.iter().filter(|o| o.role == Role::Def) {
        if let Constraint::Tied(n) = op.constraint {
            let src = ops.get(n).ok_or("a tied operand is missing")?;
            let a = alloc.loc_at(src.vreg, pos::early(g));
            let b = alloc.loc_at(op.vreg, pos::late(g));
            if a != b {
                return Err(format!("the tied operands of {} differ", op.vreg));
            }
        }
    }
    Ok(())
}

fn check_clobbers(
    func: &LirFunc,
    alloc: &Allocation,
    regs: &RegFile,
    inst: &LirInst,
    g: u32,
    ops: &[intervals::OpInfo],
    in_pool: &dyn Fn(PhysReg) -> bool,
) -> Result<(), String> {
    let rv_def = ops
        .iter()
        .find(|o| o.role == Role::Def && o.constraint == Constraint::Fixed(PhysRole::Rv))
        .map(|o| o.vreg);
    match inst.clobbers() {
        Clobbers::All => {
            for v in func.vregs.keys() {
                for piece in &alloc.pieces[v] {
                    let covers = piece
                        .ranges
                        .iter()
                        .any(|&(lo, hi)| lo <= pos::late(g) && pos::late(g) < hi);
                    if !covers {
                        continue;
                    }
                    if let Loc::Reg(r) = piece.loc {
                        if in_pool(r) && !(Some(v) == rv_def && r == regs.rv) {
                            return Err(format!("{v} is in {r} across the clobber at {g}"));
                        }
                    }
                }
            }
        }
        Clobbers::RustCall => {
            for v in func.vregs.keys() {
                for piece in &alloc.pieces[v] {
                    let covers = piece
                        .ranges
                        .iter()
                        .any(|&(lo, hi)| lo <= pos::late(g) && pos::late(g) < hi);
                    if !covers {
                        continue;
                    }
                    let Loc::Reg(r) = piece.loc else {
                        continue;
                    };
                    if !in_pool(r) {
                        continue;
                    }
                    //let root = func.is_gc_root(v);
                    let clobbered = !regs.call_preserved(r);
                    if clobbered && !(Some(v) == rv_def && r == regs.rv) {
                        return Err(format!("{v} is in {r} across the clobber at {g}"));
                    }
                }
            }
        }
        Clobbers::Slow => {
            for v in func.vregs.keys() {
                for piece in &alloc.pieces[v] {
                    let covers = piece
                        .ranges
                        .iter()
                        .any(|&(lo, hi)| lo < pos::post(g) && pos::early(g) < hi);
                    if !covers {
                        continue;
                    }
                    if let Loc::Reg(r) = piece.loc {
                        if regs.slow_clobbers.contains(&r) {
                            return Err(format!("{v} is in slow-clobbered {r} at {g}"));
                        }
                    }
                }
            }
        }
        Clobbers::None => {}
    }
    Ok(())
}

pub fn dump_if_enabled(func: &LirFunc, alloc: &Allocation, name: &str) {
    if !opt().dump.iter().any(|stage| stage == "regalloc") {
        return;
    }
    let text = display(func, alloc);
    let Some(dir) = flags::jit_dump_dir() else {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::info!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "{name} after regalloc\n{text}"
        );
        return;
    };
    let file = format!(
        "opt-regalloc-{:04}-{}.regalloc",
        super::next_dump_id(),
        super::file_safe(name)
    );
    let written =
        std::fs::create_dir_all(dir).and_then(|()| std::fs::write(dir.join(&file), &text));
    if let Err(err) = written {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::warn!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "cannot write regalloc dump to {}: {err}",
            dir.display()
        );
    }
}

pub fn display(func: &LirFunc, alloc: &Allocation) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "regalloc spill_slots={}", alloc.spill_slots);
    for &b in &alloc.order {
        let data = &func.blocks[b];
        let _ = writeln!(out, "{b}:");
        for (k, inst) in data.insts.iter().enumerate() {
            let g = alloc.index(b, k as u32);
            for m in &alloc.moves[g as usize] {
                let _ = writeln!(out, "    move {m} ({})", m.class);
            }
            let _ = writeln!(out, "  [{g}] {inst}");
        }
        if let Some(term) = &data.term {
            let g = alloc.index(b, data.insts.len() as u32);
            for m in &alloc.moves[g as usize] {
                let _ = writeln!(out, "    move {m} ({})", m.class);
            }
            let _ = writeln!(out, "  [{g}] {term}");
        }
    }
    let _ = writeln!(out, "pieces:");
    for v in func.vregs.keys() {
        for p in &alloc.pieces[v] {
            let ranges: Vec<String> = p.ranges.iter().map(|(a, b)| format!("[{a},{b})")).collect();
            let _ = writeln!(
                out,
                "  {v} ({}): {} at {}",
                func.repr(v),
                p.loc,
                ranges.join(" ")
            );
        }
    }
    for (sp, data) in func.safepoints.iter() {
        if let Some(mask) = &data.livemask {
            let _ = writeln!(out, "{sp} livemask={:x?}", mask.words);
        }
        if let Some(mask) = &data.regmask {
            let regs: Vec<String> = (0..mask.words.len() * 64)
                .filter(|&i| mask.get(i as u16))
                .map(|i| format!("p{i}"))
                .collect();
            let _ = writeln!(out, "{sp} regmask=[{}]", regs.join(", "));
        }
    }
    for (exit, locs) in alloc.deopt.iter() {
        let items: Vec<String> = locs.iter().map(|l| l.to_string()).collect();
        let _ = writeln!(out, "{exit} = [{}]", items.join(", "));
    }
    for (exit, descs) in alloc.remat.iter() {
        for (id, desc) in descs.iter().enumerate() {
            let items: Vec<String> = desc.fields.iter().map(|l| l.to_string()).collect();
            let _ = writeln!(
                out,
                "{exit}.remat[{id}] = {} [{}]",
                desc.kind,
                items.join(", ")
            );
        }
    }
    out
}

fn check_coalescing(
    func: &LirFunc,
    num: &liveness::Numbering,
    alloc: &Allocation,
) -> Result<(), String> {
    let mut members: BTreeMap<VReg, Vec<VReg>> = BTreeMap::new();
    for v in func.vregs.keys() {
        if let Some(rep) = alloc.group[v] {
            members.entry(rep).or_default().push(v);
        }
    }
    if members.is_empty() {
        return Ok(());
    }
    let live_at = |v: VReg, p: u32| {
        alloc.pieces[v]
            .iter()
            .any(|piece| assign::covers(&piece.ranges, p))
    };
    for &b in &num.order {
        for (k, inst) in func.blocks[b].insts.iter().enumerate() {
            let q = pos::late(num.base[b] + k as u32);
            for op in inst.operands(func) {
                if op.role != Role::Def {
                    continue;
                }
                let Some(rep) = alloc.group[op.vreg] else {
                    continue;
                };
                if let LirInst::Move { src, .. } = inst {
                    if alloc.group[*src] == Some(rep) {
                        continue;
                    }
                }
                for &w in &members[&rep] {
                    if w != op.vreg && live_at(w, q) {
                        return Err(format!(
                            "{}: a non-copy def of {} at {q} while its group member {w} is live",
                            b, op.vreg
                        ));
                    }
                }
            }
        }
    }
    Ok(())
}
