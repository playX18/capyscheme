use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    hash::Hash,
};

use super::x64::abi::X64Abi;
use crate::cps_ssa::{
    Block, BlockId, Inst, InstKind, Module, Proc, ProcId, TailTarget, Terminator, ValueId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueLoc<Reg> {
    Reg(Reg),
    Spill(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CopyLoc<Reg> {
    Reg(Reg),
    Spill(u32),
    IncomingStack(u32),
    OutgoingStack(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CopyOp<Reg> {
    pub from: CopyLoc<Reg>,
    pub to: CopyLoc<Reg>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryMove<Reg> {
    Copy(CopyOp<Reg>),
    PackRest {
        from_arg_index: usize,
        to: CopyLoc<Reg>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ParallelCopy<Reg> {
    pub ops: Vec<CopyOp<Reg>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EdgeKind {
    Jump,
    Then,
    Else,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EdgeKey {
    pub from: BlockId,
    pub to: BlockId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TailSitePlan<Reg> {
    pub argc: usize,
    pub copies: ParallelCopy<Reg>,
    pub outgoing_stack_args: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcRegalloc<Reg> {
    pub value_locs: BTreeMap<ValueId, ValueLoc<Reg>>,
    pub spill_slots: u32,
    pub edge_copies: BTreeMap<EdgeKey, ParallelCopy<Reg>>,
    pub entry_moves: Vec<EntryMove<Reg>>,
    pub tail_sites: BTreeMap<BlockId, TailSitePlan<Reg>>,
    pub max_outgoing_stack_args: u32,
}

impl<Reg> ProcRegalloc<Reg> {
    pub fn pretty_with<'a, F>(&'a self, reg_fmt: F) -> PrettyProcRegalloc<'a, Reg, F>
    where
        F: Fn(Reg) -> String + Copy,
    {
        PrettyProcRegalloc {
            regalloc: self,
            reg_fmt,
        }
    }

    pub fn pretty_string_with<F>(&self, reg_fmt: F) -> String
    where
        Reg: Copy,
        F: Fn(Reg) -> String + Copy,
    {
        self.pretty_with(reg_fmt).to_string()
    }
}

pub struct PrettyProcRegalloc<'a, Reg, F>
where
    F: Fn(Reg) -> String + Copy,
{
    regalloc: &'a ProcRegalloc<Reg>,
    reg_fmt: F,
}

impl<Reg, F> fmt::Display for PrettyProcRegalloc<'_, Reg, F>
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "regalloc spill-slots={} max-outgoing-stack-args={}",
            self.regalloc.spill_slots, self.regalloc.max_outgoing_stack_args
        )?;

        writeln!(f, "values:")?;
        for (&value, &loc) in &self.regalloc.value_locs {
            writeln!(
                f,
                "  {} -> {}",
                fmt_value(value),
                fmt_value_loc(loc, self.reg_fmt)
            )?;
        }

        writeln!(f, "entry-moves:")?;
        if self.regalloc.entry_moves.is_empty() {
            writeln!(f, "  []")?;
        } else {
            for entry_move in &self.regalloc.entry_moves {
                writeln!(f, "  {}", fmt_entry_move(entry_move, self.reg_fmt))?;
            }
        }

        writeln!(f, "edge-copies:")?;
        if self.regalloc.edge_copies.is_empty() {
            writeln!(f, "  []")?;
        } else {
            for (edge, copies) in &self.regalloc.edge_copies {
                writeln!(
                    f,
                    "  {} -> {} ({:?}):",
                    fmt_block(edge.from),
                    fmt_block(edge.to),
                    edge.kind
                )?;
                fmt_parallel_copy_indented(f, copies, self.reg_fmt, "    ")?;
            }
        }

        writeln!(f, "tail-sites:")?;
        if self.regalloc.tail_sites.is_empty() {
            writeln!(f, "  []")?;
        } else {
            for (&block, plan) in &self.regalloc.tail_sites {
                writeln!(
                    f,
                    "  {} argc={} outgoing-stack-args={}:",
                    fmt_block(block),
                    plan.argc,
                    plan.outgoing_stack_args
                )?;
                fmt_parallel_copy_indented(f, &plan.copies, self.reg_fmt, "    ")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstConstraints<Reg> {
    pub fixed_uses: Vec<Reg>,
    pub fixed_defs: Vec<Reg>,
    pub clobbers: Vec<Reg>,
    pub call_like: bool,
}

impl<Reg> Default for InstConstraints<Reg> {
    fn default() -> Self {
        Self {
            fixed_uses: Vec::new(),
            fixed_defs: Vec::new(),
            clobbers: Vec::new(),
            call_like: false,
        }
    }
}

pub trait RegallocAbi {
    type Reg: Copy + Eq + Hash + Ord;

    fn allocatable_regs(&self) -> &[Self::Reg];
    fn scratch_reg(&self) -> Self::Reg;
    fn argc_reg(&self) -> Self::Reg;
    fn closure_reg(&self) -> Self::Reg;
    fn context_reg(&self) -> Self::Reg;
    fn argument_regs(&self) -> &[Self::Reg];
    fn inst_constraints(&self, inst: &Inst<'_>) -> InstConstraints<Self::Reg>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegallocError {
    MissingBlock(BlockId),
    MissingDefinition(ValueId),
    MissingHome(ValueId),
}

impl fmt::Display for RegallocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingBlock(block) => write!(f, "missing block {:?}", block),
            Self::MissingDefinition(value) => write!(f, "missing definition for {:?}", value),
            Self::MissingHome(value) => write!(f, "missing home for {:?}", value),
        }
    }
}

impl std::error::Error for RegallocError {}

pub fn allocate_proc<A: RegallocAbi>(
    proc: &Proc<'_>,
    abi: &A,
) -> Result<ProcRegalloc<A::Reg>, RegallocError> {
    let blocks = proc
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let block_order = canonical_block_order(proc)?;
    let layout = number_proc(&blocks, &block_order, abi)?;
    let mut value_locs = allocate_value_locs(&layout.intervals, &layout.reserved_regs, abi);
    // The closure register is a fixed incoming value; treat it as pre-colored.
    value_locs.insert(proc.self_param, ValueLoc::Reg(abi.closure_reg()));
    let entry_moves = build_entry_moves(proc, &value_locs, abi)?;
    let edge_copies = build_edge_copies(&blocks, &value_locs)?;
    let (tail_sites, max_outgoing_stack_args) = build_tail_sites(&blocks, &value_locs, abi)?;

    Ok(ProcRegalloc {
        spill_slots: value_locs
            .values()
            .filter_map(|loc| match loc {
                ValueLoc::Spill(slot) => Some(slot + 1),
                ValueLoc::Reg(_) => None,
            })
            .max()
            .unwrap_or(0),
        value_locs,
        edge_copies,
        entry_moves,
        tail_sites,
        max_outgoing_stack_args,
    })
}

pub fn allocate_module<A: RegallocAbi>(
    module: &Module<'_>,
    abi: &A,
) -> Result<BTreeMap<ProcId, ProcRegalloc<A::Reg>>, RegallocError> {
    module
        .procs
        .iter()
        .map(|proc| Ok((proc.id, allocate_proc(proc, abi)?)))
        .collect()
}

pub fn allocate_proc_x64(proc: &Proc<'_>) -> Result<ProcRegalloc<asmkit::x86::Gpq>, RegallocError> {
    allocate_proc(proc, &X64Abi)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Interval {
    value: ValueId,
    start: u32,
    end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ActiveReg<Reg> {
    value: ValueId,
    reg: Reg,
    end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ActiveSpill {
    slot: u32,
    end: u32,
}

#[derive(Debug, Clone)]
struct Layout<Reg> {
    intervals: Vec<Interval>,
    reserved_regs: BTreeMap<Reg, Vec<u32>>,
}

#[derive(Debug, Clone, Copy)]
struct BlockPositions {
    term_use: u32,
}

fn canonical_block_order(proc: &Proc<'_>) -> Result<Vec<BlockId>, RegallocError> {
    let blocks = proc
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let mut visited = BTreeSet::new();
    let mut order = rpo_component(proc.entry, &blocks, &mut visited)?;

    for block_id in blocks.keys().copied().collect::<Vec<_>>() {
        if visited.contains(&block_id) {
            continue;
        }
        order.extend(rpo_component(block_id, &blocks, &mut visited)?);
    }

    Ok(order)
}

fn rpo_component(
    root: BlockId,
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    visited: &mut BTreeSet<BlockId>,
) -> Result<Vec<BlockId>, RegallocError> {
    let mut postorder = Vec::new();
    dfs_postorder(root, blocks, visited, &mut postorder)?;
    postorder.reverse();
    Ok(postorder)
}

fn dfs_postorder(
    block_id: BlockId,
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    visited: &mut BTreeSet<BlockId>,
    postorder: &mut Vec<BlockId>,
) -> Result<(), RegallocError> {
    if !visited.insert(block_id) {
        return Ok(());
    }

    let block = blocks
        .get(&block_id)
        .copied()
        .ok_or(RegallocError::MissingBlock(block_id))?;
    for (_, succ) in successor_edges(&block.term) {
        dfs_postorder(succ, blocks, visited, postorder)?;
    }
    postorder.push(block_id);
    Ok(())
}

fn successor_edges(term: &Terminator<'_>) -> Vec<(EdgeKind, BlockId)> {
    match term {
        Terminator::Jump { block, .. } => vec![(EdgeKind::Jump, *block)],
        Terminator::Branch {
            then_block,
            else_block,
            ..
        } => vec![(EdgeKind::Then, *then_block), (EdgeKind::Else, *else_block)],
        Terminator::TailContinue { .. } | Terminator::TailApp { .. } => Vec::new(),
    }
}

fn number_proc<A: RegallocAbi>(
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    block_order: &[BlockId],
    abi: &A,
) -> Result<Layout<A::Reg>, RegallocError> {
    let mut def_positions = BTreeMap::new();
    let mut last_use = BTreeMap::new();
    let mut reserved_regs = BTreeMap::<A::Reg, Vec<u32>>::new();
    let mut block_positions = BTreeMap::new();
    let mut next_pos = 0u32;

    for &block_id in block_order {
        let block = blocks
            .get(&block_id)
            .copied()
            .ok_or(RegallocError::MissingBlock(block_id))?;

        for param in block.params.iter() {
            def_positions.insert(param.value, next_pos);
            next_pos += 2;
        }

        for inst in block.insts.iter() {
            let use_pos = next_pos;
            for value in inst_uses(inst) {
                record_use(&mut last_use, &def_positions, value, use_pos)?;
            }

            let constraints = abi.inst_constraints(inst);
            reserve_regs(&mut reserved_regs, &constraints.fixed_uses, use_pos);
            reserve_regs(&mut reserved_regs, &constraints.clobbers, use_pos);
            reserve_regs(&mut reserved_regs, &constraints.fixed_defs, use_pos + 1);

            if let Some(result) = inst.result {
                def_positions.insert(result, use_pos + 1);
            }
            next_pos += 2;
        }

        let term_use = next_pos;
        for value in term_uses(&block.term) {
            record_use(&mut last_use, &def_positions, value, term_use)?;
        }
        block_positions.insert(block_id, BlockPositions { term_use });
        next_pos += 2;
    }

    for block in blocks.values().copied() {
        match &block.term {
            Terminator::Jump { args, .. } => {
                for value in args.iter().copied() {
                    let pos = block_positions[&block.id].term_use;
                    record_use(&mut last_use, &def_positions, value, pos)?;
                }
            }
            Terminator::Branch {
                then_args,
                else_args,
                ..
            } => {
                for value in then_args.iter().chain(else_args.iter()).copied() {
                    let pos = block_positions[&block.id].term_use;
                    record_use(&mut last_use, &def_positions, value, pos)?;
                }
            }
            Terminator::TailContinue { .. } | Terminator::TailApp { .. } => {}
        }
    }

    let mut intervals = def_positions
        .into_iter()
        .map(|(value, start)| Interval {
            value,
            start,
            end: last_use.get(&value).copied().unwrap_or(start),
        })
        .collect::<Vec<_>>();
    intervals.sort_by_key(|interval| (interval.start, interval.end, interval.value));

    Ok(Layout {
        intervals,
        reserved_regs,
    })
}

fn record_use(
    last_use: &mut BTreeMap<ValueId, u32>,
    def_positions: &BTreeMap<ValueId, u32>,
    value: ValueId,
    pos: u32,
) -> Result<(), RegallocError> {
    if !def_positions.contains_key(&value) {
        return Err(RegallocError::MissingDefinition(value));
    }

    match last_use.get_mut(&value) {
        Some(last) => {
            *last = (*last).max(pos);
        }
        None => {
            last_use.insert(value, pos);
        }
    }
    Ok(())
}

fn reserve_regs<Reg: Copy + Ord>(
    reserved_regs: &mut BTreeMap<Reg, Vec<u32>>,
    regs: &[Reg],
    pos: u32,
) {
    for &reg in regs {
        reserved_regs.entry(reg).or_default().push(pos);
    }
}

fn allocate_value_locs<A: RegallocAbi>(
    intervals: &[Interval],
    reserved_regs: &BTreeMap<A::Reg, Vec<u32>>,
    abi: &A,
) -> BTreeMap<ValueId, ValueLoc<A::Reg>> {
    let allocatable = abi
        .allocatable_regs()
        .iter()
        .copied()
        .filter(|reg| {
            *reg != abi.scratch_reg() && *reg != abi.closure_reg() && *reg != abi.context_reg()
        })
        .collect::<Vec<_>>();

    let mut homes = BTreeMap::new();
    let mut active_regs = Vec::<ActiveReg<A::Reg>>::new();
    let mut active_spills = Vec::<(ValueId, ActiveSpill)>::new();
    let mut free_spill_slots = BTreeSet::new();
    let mut next_spill_slot = 0u32;

    for interval in intervals {
        expire_active_regs(&mut active_regs, interval.start);
        expire_active_spills(&mut active_spills, interval.start, &mut free_spill_slots);

        if let Some(reg) = allocatable.iter().copied().find(|reg| {
            !active_regs.iter().any(|active| active.reg == *reg)
                && !reg_reserved_in_range(reserved_regs.get(reg), interval.start, interval.end)
        }) {
            homes.insert(interval.value, ValueLoc::Reg(reg));
            active_regs.push(ActiveReg {
                value: interval.value,
                reg,
                end: interval.end,
            });
            continue;
        }

        let victim = active_regs
            .iter()
            .enumerate()
            .filter(|(_, active)| {
                !reg_reserved_in_range(reserved_regs.get(&active.reg), interval.start, interval.end)
            })
            .max_by_key(|(_, active)| active.end);

        match victim {
            Some((victim_index, victim)) if victim.end > interval.end => {
                let victim = *victim;
                let slot = alloc_spill_slot(&mut free_spill_slots, &mut next_spill_slot);
                homes.insert(victim.value, ValueLoc::Spill(slot));
                active_spills.push((
                    victim.value,
                    ActiveSpill {
                        slot,
                        end: victim.end,
                    },
                ));

                homes.insert(interval.value, ValueLoc::Reg(victim.reg));
                active_regs[victim_index] = ActiveReg {
                    value: interval.value,
                    reg: victim.reg,
                    end: interval.end,
                };
            }
            _ => {
                let slot = alloc_spill_slot(&mut free_spill_slots, &mut next_spill_slot);
                homes.insert(interval.value, ValueLoc::Spill(slot));
                active_spills.push((
                    interval.value,
                    ActiveSpill {
                        slot,
                        end: interval.end,
                    },
                ));
            }
        }
    }

    homes
}

fn expire_active_regs<Reg>(active_regs: &mut Vec<ActiveReg<Reg>>, start: u32) {
    active_regs.retain(|active| active.end >= start);
}

fn expire_active_spills(
    active_spills: &mut Vec<(ValueId, ActiveSpill)>,
    start: u32,
    free_spill_slots: &mut BTreeSet<u32>,
) {
    active_spills.retain(|(_, active)| {
        if active.end < start {
            free_spill_slots.insert(active.slot);
            false
        } else {
            true
        }
    });
}

fn reg_reserved_in_range(positions: Option<&Vec<u32>>, start: u32, end: u32) -> bool {
    let Some(positions) = positions else {
        return false;
    };
    let index = positions.partition_point(|pos| *pos < start);
    positions.get(index).is_some_and(|pos| *pos <= end)
}

fn alloc_spill_slot(free_spill_slots: &mut BTreeSet<u32>, next_spill_slot: &mut u32) -> u32 {
    match free_spill_slots.pop_first() {
        Some(slot) => slot,
        None => {
            let slot = *next_spill_slot;
            *next_spill_slot += 1;
            slot
        }
    }
}

fn build_entry_moves<A: RegallocAbi>(
    proc: &Proc<'_>,
    homes: &BTreeMap<ValueId, ValueLoc<A::Reg>>,
    abi: &A,
) -> Result<Vec<EntryMove<A::Reg>>, RegallocError> {
    let mut moves = Vec::new();
    // self_param is pre-colored to the closure register; no entry move needed.

    let mut arg_index = 0usize;
    if let Some(retk) = proc.retk_param {
        push_entry_copy(
            &mut moves,
            incoming_arg_loc(abi, arg_index),
            value_copy_loc(homes, retk)?,
        );
        arg_index += 1;
    }

    for arg in proc.arg_params.iter().copied() {
        push_entry_copy(
            &mut moves,
            incoming_arg_loc(abi, arg_index),
            value_copy_loc(homes, arg)?,
        );
        arg_index += 1;
    }

    if let Some(rest) = proc.rest_param {
        moves.push(EntryMove::PackRest {
            from_arg_index: arg_index,
            to: value_copy_loc(homes, rest)?,
        });
    }

    Ok(moves)
}

fn incoming_arg_loc<A: RegallocAbi>(abi: &A, arg_index: usize) -> CopyLoc<A::Reg> {
    let argument_regs = abi.argument_regs();
    if arg_index < argument_regs.len() {
        CopyLoc::Reg(argument_regs[arg_index])
    } else {
        CopyLoc::IncomingStack((arg_index - argument_regs.len()) as u32)
    }
}

fn outgoing_arg_loc<A: RegallocAbi>(abi: &A, arg_index: usize) -> CopyLoc<A::Reg> {
    let argument_regs = abi.argument_regs();
    if arg_index < argument_regs.len() {
        CopyLoc::Reg(argument_regs[arg_index])
    } else {
        CopyLoc::OutgoingStack((arg_index - argument_regs.len()) as u32)
    }
}

fn build_edge_copies<Reg: Copy + Ord>(
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    homes: &BTreeMap<ValueId, ValueLoc<Reg>>,
) -> Result<BTreeMap<EdgeKey, ParallelCopy<Reg>>, RegallocError> {
    let mut edge_copies = BTreeMap::new();

    for block in blocks.values().copied() {
        match &block.term {
            Terminator::Jump {
                block: target,
                args,
            } => {
                let target_block = blocks
                    .get(target)
                    .copied()
                    .ok_or(RegallocError::MissingBlock(*target))?;
                let copies = edge_parallel_copy(target_block, args.as_slice(), homes)?;
                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *target,
                        kind: EdgeKind::Jump,
                    },
                    copies,
                );
            }
            Terminator::Branch {
                then_block,
                then_args,
                else_block,
                else_args,
                ..
            } => {
                let then_target = blocks
                    .get(then_block)
                    .copied()
                    .ok_or(RegallocError::MissingBlock(*then_block))?;
                let else_target = blocks
                    .get(else_block)
                    .copied()
                    .ok_or(RegallocError::MissingBlock(*else_block))?;
                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *then_block,
                        kind: EdgeKind::Then,
                    },
                    edge_parallel_copy(then_target, then_args.as_slice(), homes)?,
                );
                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *else_block,
                        kind: EdgeKind::Else,
                    },
                    edge_parallel_copy(else_target, else_args.as_slice(), homes)?,
                );
            }
            Terminator::TailContinue { .. } | Terminator::TailApp { .. } => {}
        }
    }

    Ok(edge_copies)
}

fn edge_parallel_copy<Reg: Copy + Ord>(
    target: &Block<'_>,
    args: &[ValueId],
    homes: &BTreeMap<ValueId, ValueLoc<Reg>>,
) -> Result<ParallelCopy<Reg>, RegallocError> {
    let mut ops = Vec::new();
    for (arg, param) in args.iter().copied().zip(target.params.iter()) {
        push_copy(
            &mut ops,
            value_copy_loc(homes, arg)?,
            value_copy_loc(homes, param.value)?,
        );
    }
    Ok(ParallelCopy { ops })
}

fn build_tail_sites<A: RegallocAbi>(
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    homes: &BTreeMap<ValueId, ValueLoc<A::Reg>>,
    abi: &A,
) -> Result<(BTreeMap<BlockId, TailSitePlan<A::Reg>>, u32), RegallocError> {
    let mut tail_sites = BTreeMap::new();
    let mut max_outgoing_stack_args = 0u32;

    for block in blocks.values().copied() {
        match &block.term {
            Terminator::TailContinue { callee, args, .. } => {
                let mut copies = Vec::new();
                push_copy(
                    &mut copies,
                    value_copy_loc(homes, tail_target_closure(callee))?,
                    CopyLoc::Reg(abi.closure_reg()),
                );
                for (arg_index, arg) in args.iter().copied().enumerate() {
                    push_copy(
                        &mut copies,
                        value_copy_loc(homes, arg)?,
                        outgoing_arg_loc(abi, arg_index),
                    );
                }
                let overflow = args.len().saturating_sub(abi.argument_regs().len()) as u32;
                max_outgoing_stack_args = max_outgoing_stack_args.max(overflow);
                tail_sites.insert(
                    block.id,
                    TailSitePlan {
                        argc: args.len(),
                        copies: ParallelCopy { ops: copies },
                        outgoing_stack_args: overflow,
                    },
                );
            }
            Terminator::TailApp {
                callee, retk, args, ..
            } => {
                let mut copies = Vec::new();
                push_copy(
                    &mut copies,
                    value_copy_loc(homes, tail_target_closure(callee))?,
                    CopyLoc::Reg(abi.closure_reg()),
                );
                push_copy(
                    &mut copies,
                    value_copy_loc(homes, *retk)?,
                    outgoing_arg_loc(abi, 0),
                );
                for (arg_index, arg) in args.iter().copied().enumerate() {
                    push_copy(
                        &mut copies,
                        value_copy_loc(homes, arg)?,
                        outgoing_arg_loc(abi, arg_index + 1),
                    );
                }
                let outgoing_argc = args.len() + 1;
                let overflow = outgoing_argc.saturating_sub(abi.argument_regs().len()) as u32;
                max_outgoing_stack_args = max_outgoing_stack_args.max(overflow);
                tail_sites.insert(
                    block.id,
                    TailSitePlan {
                        argc: outgoing_argc,
                        copies: ParallelCopy { ops: copies },
                        outgoing_stack_args: overflow,
                    },
                );
            }
            Terminator::Jump { .. } | Terminator::Branch { .. } => {}
        }
    }

    Ok((tail_sites, max_outgoing_stack_args))
}

fn push_copy<Reg: Copy + Eq>(copies: &mut Vec<CopyOp<Reg>>, from: CopyLoc<Reg>, to: CopyLoc<Reg>) {
    if from == to {
        return;
    }
    copies.push(CopyOp { from, to });
}

fn push_entry_copy<Reg: Copy + Eq>(
    moves: &mut Vec<EntryMove<Reg>>,
    from: CopyLoc<Reg>,
    to: CopyLoc<Reg>,
) {
    if from == to {
        return;
    }
    moves.push(EntryMove::Copy(CopyOp { from, to }));
}

fn value_copy_loc<Reg: Copy>(
    homes: &BTreeMap<ValueId, ValueLoc<Reg>>,
    value: ValueId,
) -> Result<CopyLoc<Reg>, RegallocError> {
    match homes.get(&value).copied() {
        Some(ValueLoc::Reg(reg)) => Ok(CopyLoc::Reg(reg)),
        Some(ValueLoc::Spill(slot)) => Ok(CopyLoc::Spill(slot)),
        None => Err(RegallocError::MissingHome(value)),
    }
}

fn tail_target_closure(target: &TailTarget) -> ValueId {
    match target {
        TailTarget::Direct { closure, .. } | TailTarget::Indirect { closure } => *closure,
    }
}

fn fmt_parallel_copy_indented<Reg, F>(
    f: &mut fmt::Formatter<'_>,
    copies: &ParallelCopy<Reg>,
    reg_fmt: F,
    indent: &str,
) -> fmt::Result
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    if copies.ops.is_empty() {
        writeln!(f, "{indent}[]")
    } else {
        for copy in &copies.ops {
            writeln!(f, "{indent}{}", fmt_copy(copy, reg_fmt))?;
        }
        Ok(())
    }
}

fn fmt_copy<Reg, F>(copy: &CopyOp<Reg>, reg_fmt: F) -> String
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    format!(
        "{} -> {}",
        fmt_copy_loc(copy.from, reg_fmt),
        fmt_copy_loc(copy.to, reg_fmt)
    )
}

fn fmt_entry_move<Reg, F>(entry_move: &EntryMove<Reg>, reg_fmt: F) -> String
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    match entry_move {
        EntryMove::Copy(copy) => fmt_copy(copy, reg_fmt),
        EntryMove::PackRest { from_arg_index, to } => format!(
            "pack-rest incoming-args[{from_arg_index}..] -> {}",
            fmt_copy_loc(*to, reg_fmt)
        ),
    }
}

fn fmt_value_loc<Reg, F>(loc: ValueLoc<Reg>, reg_fmt: F) -> String
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    match loc {
        ValueLoc::Reg(reg) => format!("reg {}", reg_fmt(reg)),
        ValueLoc::Spill(slot) => format!("spill{slot}"),
    }
}

fn fmt_copy_loc<Reg, F>(loc: CopyLoc<Reg>, reg_fmt: F) -> String
where
    Reg: Copy,
    F: Fn(Reg) -> String + Copy,
{
    match loc {
        CopyLoc::Reg(reg) => format!("reg {}", reg_fmt(reg)),
        CopyLoc::Spill(slot) => format!("spill{slot}"),
        CopyLoc::IncomingStack(slot) => format!("incoming-stack{slot}"),
        CopyLoc::OutgoingStack(slot) => format!("outgoing-stack{slot}"),
    }
}

fn fmt_value(value: ValueId) -> String {
    format!("v{}", value.0)
}

fn fmt_block(block: BlockId) -> String {
    format!("b{}", block.0)
}

fn inst_uses(inst: &Inst<'_>) -> Vec<ValueId> {
    match &inst.kind {
        InstKind::Const(_) | InstKind::MakeClosure { .. } => Vec::new(),
        InstKind::PackRest { items } | InstKind::PrimCall { args: items, .. } => {
            items.iter().copied().collect()
        }
        InstKind::RestRef { rest, .. }
        | InstKind::RestLength { rest, .. }
        | InstKind::RestToList { rest, .. }
        | InstKind::RestIsEmpty { rest, .. }
        | InstKind::RestIsNotEmpty { rest, .. }
        | InstKind::ClosureRef { closure: rest, .. } => vec![*rest],
        InstKind::InitClosure { closure, captures } => std::iter::once(*closure)
            .chain(captures.iter().copied())
            .collect(),
    }
}

fn term_uses(term: &Terminator<'_>) -> Vec<ValueId> {
    match term {
        Terminator::Jump { args, .. } => args.iter().copied().collect(),
        Terminator::Branch {
            cond,
            then_args,
            else_args,
            ..
        } => std::iter::once(*cond)
            .chain(then_args.iter().copied())
            .chain(else_args.iter().copied())
            .collect(),
        Terminator::TailContinue { callee, args, .. } => {
            std::iter::once(tail_target_closure(callee))
                .chain(args.iter().copied())
                .collect()
        }
        Terminator::TailApp {
            callee, retk, args, ..
        } => std::iter::once(tail_target_closure(callee))
            .chain(std::iter::once(*retk))
            .chain(args.iter().copied())
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::packed::Primitive,
        cps_ssa::{BlockParam, ConstRef, ImmediateConst, Inst, InstKind, ProcKind},
        rsgc::alloc::Array,
        runtime::{Context, Scheme},
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum TinyReg {
        R0,
        R1,
        Argc,
        Closure,
        Context,
        Scratch,
        Arg0,
        Arg1,
    }

    impl TinyReg {
        fn as_str(self) -> &'static str {
            match self {
                Self::R0 => "r0",
                Self::R1 => "r1",
                Self::Argc => "argc",
                Self::Closure => "closure",
                Self::Context => "context",
                Self::Scratch => "scratch",
                Self::Arg0 => "arg0",
                Self::Arg1 => "arg1",
            }
        }
    }

    struct TinyAbi {
        allocatable: &'static [TinyReg],
        argument_regs: &'static [TinyReg],
    }

    impl RegallocAbi for TinyAbi {
        type Reg = TinyReg;

        fn allocatable_regs(&self) -> &[Self::Reg] {
            self.allocatable
        }

        fn scratch_reg(&self) -> Self::Reg {
            TinyReg::Scratch
        }

        fn argc_reg(&self) -> Self::Reg {
            TinyReg::Argc
        }

        fn closure_reg(&self) -> Self::Reg {
            TinyReg::Closure
        }

        fn context_reg(&self) -> Self::Reg {
            TinyReg::Context
        }

        fn argument_regs(&self) -> &[Self::Reg] {
            self.argument_regs
        }

        fn inst_constraints(&self, _inst: &Inst<'_>) -> InstConstraints<Self::Reg> {
            InstConstraints::default()
        }
    }

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn proc<'gc>(
        ctx: Context<'gc>,
        kind: ProcKind,
        entry: BlockId,
        self_param: ValueId,
        retk_param: Option<ValueId>,
        arg_params: &[ValueId],
        rest_param: Option<ValueId>,
        blocks: &[Block<'gc>],
    ) -> Proc<'gc> {
        Proc {
            id: ProcId(0),
            kind,
            binding_name: ConstRef::Inline(ImmediateConst::Null),
            display_name: None,
            source: None,
            meta: None,
            cold: false,
            noinline: false,
            entry,
            self_param,
            retk_param,
            arg_params: Array::from_slice(*ctx, arg_params),
            rest_param,
            captures: Array::from_slice(*ctx, &[]),
            blocks: Array::from_slice(*ctx, blocks),
        }
    }

    fn block<'gc>(
        ctx: Context<'gc>,
        id: BlockId,
        params: &[ValueId],
        insts: &[Inst<'gc>],
        term: Terminator<'gc>,
    ) -> Block<'gc> {
        let params = params
            .iter()
            .copied()
            .map(|value| BlockParam { value, name: None })
            .collect::<Vec<_>>();
        Block {
            id,
            name: None,
            params: Array::from_slice(*ctx, &params),
            insts: Array::from_slice(*ctx, insts),
            term,
            cold: false,
        }
    }

    fn inst_const<'gc>(result: ValueId) -> Inst<'gc> {
        Inst {
            result: Some(result),
            source: None,
            kind: InstKind::Const(ConstRef::Inline(ImmediateConst::Null)),
        }
    }

    fn inst_pack_rest<'gc>(result: ValueId, items: &[ValueId], ctx: Context<'gc>) -> Inst<'gc> {
        Inst {
            result: Some(result),
            source: None,
            kind: InstKind::PackRest {
                items: Array::from_slice(*ctx, items),
            },
        }
    }

    fn has_entry_copy<Reg: Copy + Eq>(
        moves: &[EntryMove<Reg>],
        from: CopyLoc<Reg>,
        to: CopyLoc<Reg>,
    ) -> bool {
        moves.iter().any(|entry_move| {
            matches!(
                entry_move,
                EntryMove::Copy(CopyOp {
                    from: move_from,
                    to: move_to,
                }) if *move_from == from && *move_to == to
            )
        })
    }

    #[test]
    fn pretty_prints_regalloc_complex_example() {
        with_ctx(|ctx| {
            let abi = TinyAbi {
                allocatable: &[TinyReg::R0, TinyReg::R1, TinyReg::Arg0, TinyReg::Arg1],
                argument_regs: &[TinyReg::Arg0, TinyReg::Arg1],
            };
            let proc = proc(
                ctx,
                ProcKind::Function,
                BlockId(0),
                ValueId(0),
                Some(ValueId(1)),
                &[ValueId(2), ValueId(3)],
                Some(ValueId(4)),
                &[
                    block(
                        ctx,
                        BlockId(1),
                        &[ValueId(5), ValueId(6)],
                        &[],
                        Terminator::TailContinue {
                            callee: TailTarget::Indirect {
                                closure: ValueId(5),
                            },
                            args: Array::from_slice(*ctx, &[ValueId(6)]),
                            source: None,
                        },
                    ),
                    block(
                        ctx,
                        BlockId(2),
                        &[ValueId(7), ValueId(8)],
                        &[],
                        Terminator::TailApp {
                            callee: TailTarget::Indirect {
                                closure: ValueId(7),
                            },
                            retk: ValueId(1),
                            args: Array::from_slice(*ctx, &[ValueId(8)]),
                            source: None,
                        },
                    ),
                    block(
                        ctx,
                        BlockId(0),
                        &[ValueId(0), ValueId(1), ValueId(2), ValueId(3), ValueId(4)],
                        &[inst_const(ValueId(9)), inst_const(ValueId(10))],
                        Terminator::Branch {
                            cond: ValueId(2),
                            then_block: BlockId(1),
                            then_args: Array::from_slice(*ctx, &[ValueId(3), ValueId(9)]),
                            else_block: BlockId(2),
                            else_args: Array::from_slice(*ctx, &[ValueId(9), ValueId(10)]),
                            hints: [
                                crate::cps::term::BranchHint::Normal,
                                crate::cps::term::BranchHint::Normal,
                            ],
                        },
                    ),
                ],
            );

            let alloc = allocate_proc(&proc, &abi).unwrap();
            let rendered = alloc.pretty_string_with(|reg| reg.as_str().to_owned());

            println!("{}", rendered);

            assert!(rendered.contains("regalloc spill-slots="));
            assert!(rendered.contains("values:"));
            assert!(rendered.contains("v0 ->"));
            assert!(rendered.contains("entry-moves:"));
            assert!(rendered.contains("pack-rest incoming-args[3..]"));
            assert!(rendered.contains("edge-copies:"));
            assert!(rendered.contains("b0 -> b1 (Then):"));
            assert!(rendered.contains("b0 -> b2 (Else):"));
            assert!(rendered.contains("tail-sites:"));
            assert!(rendered.contains("b1 argc=1 outgoing-stack-args=0:"));
            assert!(rendered.contains("b2 argc=2 outgoing-stack-args=0:"));
            assert!(rendered.contains("reg closure"));
            assert!(rendered.contains("reg arg0"));
        });
    }

    #[test]
    fn x64_entry_copies_map_function_and_continuation_inputs() {
        with_ctx(|ctx| {
            let function = proc(
                ctx,
                ProcKind::Function,
                BlockId(0),
                ValueId(0),
                Some(ValueId(1)),
                &[ValueId(2)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1), ValueId(2)],
                    &[],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(2)]),
                        source: None,
                    },
                )],
            );

            let continuation = proc(
                ctx,
                ProcKind::Continuation,
                BlockId(0),
                ValueId(0),
                None,
                &[ValueId(1)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1)],
                    &[],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(1)]),
                        source: None,
                    },
                )],
            );

            let function_alloc = allocate_proc_x64(&function).unwrap();
            let continuation_alloc = allocate_proc_x64(&continuation).unwrap();

            assert!(
                value_copy_loc(&function_alloc.value_locs, ValueId(0)).unwrap()
                    == CopyLoc::Reg(crate::jit::x64::abi::CLOSURE_POINTER_REG)
            );
            let retk_home = value_copy_loc(&function_alloc.value_locs, ValueId(1)).unwrap();
            assert!(
                retk_home == CopyLoc::Reg(crate::jit::x64::abi::ARGUMENT_REGS[0])
                    || has_entry_copy(
                        &function_alloc.entry_moves,
                        CopyLoc::Reg(crate::jit::x64::abi::ARGUMENT_REGS[0]),
                        retk_home,
                    )
            );
            assert!(has_entry_copy(
                &function_alloc.entry_moves,
                CopyLoc::Reg(crate::jit::x64::abi::ARGUMENT_REGS[1]),
                value_copy_loc(&function_alloc.value_locs, ValueId(2)).unwrap(),
            ));

            let cont_arg_home = value_copy_loc(&continuation_alloc.value_locs, ValueId(1)).unwrap();
            assert!(
                cont_arg_home == CopyLoc::Reg(crate::jit::x64::abi::ARGUMENT_REGS[0])
                    || has_entry_copy(
                        &continuation_alloc.entry_moves,
                        CopyLoc::Reg(crate::jit::x64::abi::ARGUMENT_REGS[0]),
                        cont_arg_home,
                    )
            );
        });
    }

    #[test]
    fn variadic_entry_uses_pack_rest_from_remaining_incoming_args() {
        with_ctx(|ctx| {
            let abi = TinyAbi {
                allocatable: &[TinyReg::R0, TinyReg::R1, TinyReg::Arg0, TinyReg::Arg1],
                argument_regs: &[TinyReg::Arg0, TinyReg::Arg1],
            };
            let proc = proc(
                ctx,
                ProcKind::Function,
                BlockId(0),
                ValueId(0),
                Some(ValueId(1)),
                &[ValueId(2), ValueId(3)],
                Some(ValueId(4)),
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1), ValueId(2), ValueId(3), ValueId(4)],
                    &[],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(2)]),
                        source: None,
                    },
                )],
            );

            let alloc = allocate_proc(&proc, &abi).unwrap();
            let rest_home = value_copy_loc(&alloc.value_locs, ValueId(4)).unwrap();

            assert!(alloc.entry_moves.iter().any(|entry_move| {
                matches!(
                    entry_move,
                    EntryMove::PackRest {
                        from_arg_index: 3,
                        to,
                    } if *to == rest_home
                )
            }));
        });
    }

    #[test]
    fn edge_parallel_copy_uses_value_and_param_homes() {
        with_ctx(|ctx| {
            let target = block(
                ctx,
                BlockId(1),
                &[ValueId(10), ValueId(11)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(10),
                    },
                    args: Array::from_slice(*ctx, &[ValueId(11)]),
                    source: None,
                },
            );
            let homes = BTreeMap::from([
                (ValueId(1), ValueLoc::Reg(TinyReg::R0)),
                (ValueId(2), ValueLoc::Reg(TinyReg::R1)),
                (ValueId(10), ValueLoc::Reg(TinyReg::R1)),
                (ValueId(11), ValueLoc::Reg(TinyReg::R0)),
            ]);

            let copies = edge_parallel_copy(&target, &[ValueId(1), ValueId(2)], &homes).unwrap();
            assert_eq!(
                copies.ops,
                vec![
                    CopyOp {
                        from: CopyLoc::Reg(TinyReg::R0),
                        to: CopyLoc::Reg(TinyReg::R1),
                    },
                    CopyOp {
                        from: CopyLoc::Reg(TinyReg::R1),
                        to: CopyLoc::Reg(TinyReg::R0),
                    },
                ]
            );
        });
    }

    #[test]
    fn spill_slots_are_reused() {
        with_ctx(|ctx| {
            let abi = TinyAbi {
                allocatable: &[TinyReg::R0, TinyReg::R1],
                argument_regs: &[TinyReg::Arg0, TinyReg::Arg1],
            };
            let proc = proc(
                ctx,
                ProcKind::Continuation,
                BlockId(0),
                ValueId(0),
                None,
                &[ValueId(1), ValueId(2)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1), ValueId(2)],
                    &[
                        inst_pack_rest(ValueId(3), &[ValueId(2)], ctx),
                        inst_const(ValueId(4)),
                        inst_pack_rest(ValueId(5), &[ValueId(4)], ctx),
                    ],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(1)]),
                        source: None,
                    },
                )],
            );

            let alloc = allocate_proc(&proc, &abi).unwrap();
            assert_eq!(alloc.spill_slots, 1);
        });
    }

    #[test]
    fn tail_app_plan_counts_retk_and_overflow_args() {
        with_ctx(|ctx| {
            let abi = TinyAbi {
                allocatable: &[TinyReg::R0, TinyReg::R1, TinyReg::Arg0, TinyReg::Arg1],
                argument_regs: &[TinyReg::Arg0, TinyReg::Arg1],
            };
            let proc = proc(
                ctx,
                ProcKind::Function,
                BlockId(0),
                ValueId(0),
                Some(ValueId(1)),
                &[ValueId(2), ValueId(3), ValueId(4)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1), ValueId(2), ValueId(3), ValueId(4)],
                    &[],
                    Terminator::TailApp {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        retk: ValueId(1),
                        args: Array::from_slice(*ctx, &[ValueId(2), ValueId(3), ValueId(4)]),
                        source: None,
                    },
                )],
            );

            let alloc = allocate_proc(&proc, &abi).unwrap();
            let tail = alloc.tail_sites.get(&BlockId(0)).unwrap();

            assert_eq!(tail.argc, 4);
            assert_eq!(tail.outgoing_stack_args, 2);
            assert!(
                tail.copies
                    .ops
                    .iter()
                    .any(|copy| copy.to == CopyLoc::OutgoingStack(0))
            );
            assert!(
                tail.copies
                    .ops
                    .iter()
                    .any(|copy| copy.to == CopyLoc::OutgoingStack(1))
            );
        });
    }

    #[test]
    fn canonical_block_order_ignores_proc_storage_order() {
        with_ctx(|ctx| {
            let b0 = block(
                ctx,
                BlockId(0),
                &[ValueId(0)],
                &[],
                Terminator::Jump {
                    block: BlockId(1),
                    args: Array::from_slice(*ctx, &[ValueId(0)]),
                },
            );
            let b1 = block(
                ctx,
                BlockId(1),
                &[ValueId(1)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(1),
                    },
                    args: Array::from_slice(*ctx, &[] as &[ValueId]),
                    source: None,
                },
            );
            let b2 = block(
                ctx,
                BlockId(2),
                &[ValueId(2)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(2),
                    },
                    args: Array::from_slice(*ctx, &[] as &[ValueId]),
                    source: None,
                },
            );
            let proc = proc(
                ctx,
                ProcKind::Continuation,
                BlockId(0),
                ValueId(0),
                None,
                &[],
                None,
                &[b2, b1, b0],
            );

            assert_eq!(
                canonical_block_order(&proc).unwrap(),
                vec![BlockId(0), BlockId(1), BlockId(2)]
            );
        });
    }

    #[test]
    fn x64_allocation_never_uses_reserved_regs() {
        with_ctx(|ctx| {
            let proc = proc(
                ctx,
                ProcKind::Continuation,
                BlockId(0),
                ValueId(0),
                None,
                &[ValueId(1), ValueId(2)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1), ValueId(2)],
                    &[
                        inst_const(ValueId(3)),
                        inst_pack_rest(ValueId(4), &[ValueId(3)], ctx),
                    ],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(1), ValueId(2)]),
                        source: None,
                    },
                )],
            );
            let alloc = allocate_proc_x64(&proc).unwrap();

            for (&value, loc) in &alloc.value_locs {
                let ValueLoc::Reg(reg) = loc else {
                    continue;
                };
                assert!(*reg != crate::jit::x64::abi::SCRATCH_REGISTER);
                if value != ValueId(0) {
                    assert!(*reg != crate::jit::x64::abi::CLOSURE_POINTER_REG);
                }
                assert!(*reg != crate::jit::x64::abi::CONTEXT_POINTER_REG);
            }
        });
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn x64_primcall_constraints_are_stubbed() {
        with_ctx(|ctx| {
            let proc = proc(
                ctx,
                ProcKind::Continuation,
                BlockId(0),
                ValueId(0),
                None,
                &[ValueId(1)],
                None,
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0), ValueId(1)],
                    &[Inst {
                        result: Some(ValueId(2)),
                        source: None,
                        kind: InstKind::PrimCall {
                            prim: Primitive::Cons,
                            args: Array::from_slice(*ctx, &[ValueId(1), ValueId(1)]),
                        },
                    }],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(1)]),
                        source: None,
                    },
                )],
            );
            let _ = allocate_proc_x64(&proc);
        });
    }
}
