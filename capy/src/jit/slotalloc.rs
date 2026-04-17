use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use crate::cps_ssa::{Block, BlockId, Inst, InstKind, Module, Proc, ProcId, Terminator, ValueId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SlotId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CopyOp {
    pub from: SlotId,
    pub to: SlotId,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ParallelCopy {
    pub ops: Vec<CopyOp>,
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProcSlotalloc {
    pub value_slots: BTreeMap<ValueId, SlotId>,
    pub slot_count: u32,
    pub edge_copies: BTreeMap<EdgeKey, ParallelCopy>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SlotallocError {
    MissingBlock(BlockId),
    MissingDefinition(ValueId),
    MissingSlot(ValueId),
}

impl fmt::Display for SlotallocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingBlock(block) => write!(f, "missing block {:?}", block),
            Self::MissingDefinition(value) => write!(f, "missing definition for {:?}", value),
            Self::MissingSlot(value) => write!(f, "missing slot for {:?}", value),
        }
    }
}

impl std::error::Error for SlotallocError {}

pub fn allocate_proc(proc: &Proc<'_>) -> Result<ProcSlotalloc, SlotallocError> {
    let blocks = proc
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let block_order = canonical_block_order(proc)?;
    let intervals = number_proc(&blocks, &block_order)?;
    let (value_slots, slot_count) = allocate_value_slots(&intervals);
    let edge_copies = build_edge_copies(&blocks, &value_slots)?;

    Ok(ProcSlotalloc {
        value_slots,
        slot_count,
        edge_copies,
    })
}

pub fn allocate_module(
    module: &Module<'_>,
) -> Result<BTreeMap<ProcId, ProcSlotalloc>, SlotallocError> {
    module
        .procs
        .iter()
        .map(|proc| Ok((proc.id, allocate_proc(proc)?)))
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Interval {
    value: ValueId,
    start: u32,
    end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ActiveSlot {
    slot: SlotId,
    end: u32,
}

fn canonical_block_order(proc: &Proc<'_>) -> Result<Vec<BlockId>, SlotallocError> {
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
) -> Result<Vec<BlockId>, SlotallocError> {
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
) -> Result<(), SlotallocError> {
    if !visited.insert(block_id) {
        return Ok(());
    }

    let block = blocks
        .get(&block_id)
        .copied()
        .ok_or(SlotallocError::MissingBlock(block_id))?;
    for succ in successor_blocks(&block.term) {
        dfs_postorder(succ, blocks, visited, postorder)?;
    }
    postorder.push(block_id);
    Ok(())
}

fn successor_blocks(term: &Terminator<'_>) -> Vec<BlockId> {
    match term {
        Terminator::Jump { block, .. } => vec![*block],
        Terminator::Branch {
            then_block,
            else_block,
            ..
        } => vec![*then_block, *else_block],
        Terminator::TailContinue { .. } | Terminator::TailApp { .. } => Vec::new(),
    }
}

fn number_proc(
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    block_order: &[BlockId],
) -> Result<Vec<Interval>, SlotallocError> {
    let mut def_positions = BTreeMap::new();
    let mut last_use = BTreeMap::new();
    let mut next_pos = 0u32;

    for &block_id in block_order {
        let block = blocks
            .get(&block_id)
            .copied()
            .ok_or(SlotallocError::MissingBlock(block_id))?;

        for param in block.params.iter() {
            def_positions.insert(param.value, next_pos);
            next_pos += 2;
        }

        for inst in block.insts.iter() {
            let use_pos = next_pos;
            for value in inst_uses(inst) {
                record_use(&mut last_use, &def_positions, value, use_pos)?;
            }
            if let Some(result) = inst.result {
                def_positions.insert(result, use_pos + 1);
            }
            next_pos += 2;
        }

        let term_use = next_pos;
        for value in term_uses(&block.term) {
            record_use(&mut last_use, &def_positions, value, term_use)?;
        }
        next_pos += 2;
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
    Ok(intervals)
}

fn record_use(
    last_use: &mut BTreeMap<ValueId, u32>,
    def_positions: &BTreeMap<ValueId, u32>,
    value: ValueId,
    pos: u32,
) -> Result<(), SlotallocError> {
    if !def_positions.contains_key(&value) {
        return Err(SlotallocError::MissingDefinition(value));
    }

    match last_use.get_mut(&value) {
        Some(last) => *last = (*last).max(pos),
        None => {
            last_use.insert(value, pos);
        }
    }
    Ok(())
}

fn allocate_value_slots(intervals: &[Interval]) -> (BTreeMap<ValueId, SlotId>, u32) {
    let mut value_slots = BTreeMap::new();
    let mut active = Vec::<(ValueId, ActiveSlot)>::new();
    let mut free_slots = BTreeSet::new();
    let mut next_slot = 0u32;

    for interval in intervals {
        expire_active(&mut active, interval.start, &mut free_slots);
        let slot = alloc_slot(&mut free_slots, &mut next_slot);
        value_slots.insert(interval.value, slot);
        active.push((
            interval.value,
            ActiveSlot {
                slot,
                end: interval.end,
            },
        ));
    }

    (value_slots, next_slot)
}

fn expire_active(
    active: &mut Vec<(ValueId, ActiveSlot)>,
    start: u32,
    free_slots: &mut BTreeSet<SlotId>,
) {
    active.retain(|(_, slot)| {
        if slot.end < start {
            free_slots.insert(slot.slot);
            false
        } else {
            true
        }
    });
}

fn alloc_slot(free_slots: &mut BTreeSet<SlotId>, next_slot: &mut u32) -> SlotId {
    match free_slots.pop_first() {
        Some(slot) => slot,
        None => {
            let slot = SlotId(*next_slot);
            *next_slot += 1;
            slot
        }
    }
}

fn build_edge_copies(
    blocks: &BTreeMap<BlockId, &Block<'_>>,
    value_slots: &BTreeMap<ValueId, SlotId>,
) -> Result<BTreeMap<EdgeKey, ParallelCopy>, SlotallocError> {
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
                    .ok_or(SlotallocError::MissingBlock(*target))?;
                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *target,
                        kind: EdgeKind::Jump,
                    },
                    edge_parallel_copy(target_block, args.as_slice(), value_slots)?,
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
                    .ok_or(SlotallocError::MissingBlock(*then_block))?;
                let else_target = blocks
                    .get(else_block)
                    .copied()
                    .ok_or(SlotallocError::MissingBlock(*else_block))?;

                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *then_block,
                        kind: EdgeKind::Then,
                    },
                    edge_parallel_copy(then_target, then_args.as_slice(), value_slots)?,
                );
                edge_copies.insert(
                    EdgeKey {
                        from: block.id,
                        to: *else_block,
                        kind: EdgeKind::Else,
                    },
                    edge_parallel_copy(else_target, else_args.as_slice(), value_slots)?,
                );
            }
            Terminator::TailContinue { .. } | Terminator::TailApp { .. } => {}
        }
    }

    Ok(edge_copies)
}

fn edge_parallel_copy(
    target: &Block<'_>,
    args: &[ValueId],
    value_slots: &BTreeMap<ValueId, SlotId>,
) -> Result<ParallelCopy, SlotallocError> {
    let mut ops = Vec::new();
    for (arg, param) in args.iter().copied().zip(target.params.iter()) {
        push_copy(
            &mut ops,
            slot_for(value_slots, arg)?,
            slot_for(value_slots, param.value)?,
        );
    }
    Ok(ParallelCopy { ops })
}

fn push_copy(copies: &mut Vec<CopyOp>, from: SlotId, to: SlotId) {
    if from == to {
        return;
    }
    copies.push(CopyOp { from, to });
}

fn slot_for(
    value_slots: &BTreeMap<ValueId, SlotId>,
    value: ValueId,
) -> Result<SlotId, SlotallocError> {
    value_slots
        .get(&value)
        .copied()
        .ok_or(SlotallocError::MissingSlot(value))
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

fn tail_target_closure(target: &crate::cps_ssa::TailTarget) -> ValueId {
    match target {
        crate::cps_ssa::TailTarget::Direct { closure, .. }
        | crate::cps_ssa::TailTarget::Indirect { closure } => *closure,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::term::BranchHint,
        cps_ssa::{BlockParam, ConstRef, ImmediateConst, InstKind, TailTarget},
        rsgc::alloc::Array,
        runtime::{Context, Scheme},
    };

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
        entry: BlockId,
        self_param: ValueId,
        arg_params: &[ValueId],
        blocks: &[Block<'gc>],
    ) -> Proc<'gc> {
        Proc {
            id: ProcId(0),
            kind: crate::cps_ssa::ProcKind::Continuation,
            binding_name: ConstRef::Inline(ImmediateConst::Null),
            display_name: None,
            source: None,
            meta: None,
            cold: false,
            noinline: false,
            entry,
            self_param,
            retk_param: None,
            arg_params: Array::from_slice(*ctx, arg_params),
            rest_param: None,
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

    #[test]
    fn straight_line_slots_are_reused_after_value_dies() {
        with_ctx(|ctx| {
            let proc = proc(
                ctx,
                BlockId(0),
                ValueId(0),
                &[],
                &[block(
                    ctx,
                    BlockId(0),
                    &[ValueId(0)],
                    &[inst_const(ValueId(1)), inst_const(ValueId(2))],
                    Terminator::TailContinue {
                        callee: TailTarget::Indirect {
                            closure: ValueId(0),
                        },
                        args: Array::from_slice(*ctx, &[ValueId(2)]),
                        source: None,
                    },
                )],
            );

            let alloc = allocate_proc(&proc).unwrap();
            assert_eq!(
                alloc.value_slots[&ValueId(1)],
                alloc.value_slots[&ValueId(2)]
            );
            assert_eq!(alloc.slot_count, 2);
        });
    }

    #[test]
    fn forward_jump_reuses_slot_without_edge_copy() {
        with_ctx(|ctx| {
            let b0 = block(
                ctx,
                BlockId(0),
                &[ValueId(0), ValueId(1)],
                &[],
                Terminator::Jump {
                    block: BlockId(1),
                    args: Array::from_slice(*ctx, &[ValueId(1)]),
                },
            );
            let b1 = block(
                ctx,
                BlockId(1),
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
            let proc = proc(ctx, BlockId(0), ValueId(0), &[ValueId(1)], &[b0, b1]);

            let alloc = allocate_proc(&proc).unwrap();
            assert_eq!(
                alloc.value_slots[&ValueId(1)],
                alloc.value_slots[&ValueId(2)]
            );
            assert!(
                alloc.edge_copies[&EdgeKey {
                    from: BlockId(0),
                    to: BlockId(1),
                    kind: EdgeKind::Jump,
                }]
                    .ops
                    .is_empty()
            );
        });
    }

    #[test]
    fn branch_join_only_emits_copy_for_mismatched_path() {
        with_ctx(|ctx| {
            let join = block(
                ctx,
                BlockId(1),
                &[ValueId(3)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(3),
                    },
                    args: Array::from_slice(*ctx, &[] as &[ValueId]),
                    source: None,
                },
            );
            let entry = block(
                ctx,
                BlockId(0),
                &[ValueId(0), ValueId(1), ValueId(2)],
                &[],
                Terminator::Branch {
                    cond: ValueId(1),
                    then_block: BlockId(1),
                    then_args: Array::from_slice(*ctx, &[ValueId(1)]),
                    else_block: BlockId(1),
                    else_args: Array::from_slice(*ctx, &[ValueId(2)]),
                    hints: [BranchHint::Normal, BranchHint::Normal],
                },
            );
            let proc = proc(
                ctx,
                BlockId(0),
                ValueId(0),
                &[ValueId(1), ValueId(2)],
                &[entry, join],
            );

            let alloc = allocate_proc(&proc).unwrap();
            let join_slot = alloc.value_slots[&ValueId(3)];

            assert_eq!(alloc.value_slots[&ValueId(1)], join_slot);
            assert_eq!(
                alloc.edge_copies[&EdgeKey {
                    from: BlockId(0),
                    to: BlockId(1),
                    kind: EdgeKind::Then,
                }]
                    .ops,
                Vec::<CopyOp>::new()
            );
            assert_eq!(
                alloc.edge_copies[&EdgeKey {
                    from: BlockId(0),
                    to: BlockId(1),
                    kind: EdgeKind::Else,
                }]
                    .ops,
                vec![CopyOp {
                    from: alloc.value_slots[&ValueId(2)],
                    to: join_slot,
                }]
            );
        });
    }

    #[test]
    fn loop_backedge_uses_copy_into_header_param_slot() {
        with_ctx(|ctx| {
            let entry = block(
                ctx,
                BlockId(0),
                &[ValueId(0), ValueId(1)],
                &[],
                Terminator::Jump {
                    block: BlockId(1),
                    args: Array::from_slice(*ctx, &[ValueId(1)]),
                },
            );
            let loop_block = block(
                ctx,
                BlockId(1),
                &[ValueId(2)],
                &[inst_const(ValueId(3))],
                Terminator::Branch {
                    cond: ValueId(2),
                    then_block: BlockId(1),
                    then_args: Array::from_slice(*ctx, &[ValueId(3)]),
                    else_block: BlockId(2),
                    else_args: Array::from_slice(*ctx, &[ValueId(2)]),
                    hints: [BranchHint::Normal, BranchHint::Normal],
                },
            );
            let exit = block(
                ctx,
                BlockId(2),
                &[ValueId(4)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(4),
                    },
                    args: Array::from_slice(*ctx, &[] as &[ValueId]),
                    source: None,
                },
            );
            let proc = proc(
                ctx,
                BlockId(0),
                ValueId(0),
                &[ValueId(1)],
                &[entry, loop_block, exit],
            );

            let alloc = allocate_proc(&proc).unwrap();
            assert_ne!(
                alloc.value_slots[&ValueId(2)],
                alloc.value_slots[&ValueId(3)]
            );
            assert_eq!(
                alloc.edge_copies[&EdgeKey {
                    from: BlockId(1),
                    to: BlockId(1),
                    kind: EdgeKind::Then,
                }]
                    .ops,
                vec![CopyOp {
                    from: alloc.value_slots[&ValueId(3)],
                    to: alloc.value_slots[&ValueId(2)],
                }]
            );
        });
    }

    #[test]
    fn allocation_is_independent_of_proc_block_storage_order() {
        with_ctx(|ctx| {
            let b0 = block(
                ctx,
                BlockId(0),
                &[ValueId(0), ValueId(1)],
                &[],
                Terminator::Jump {
                    block: BlockId(1),
                    args: Array::from_slice(*ctx, &[ValueId(1)]),
                },
            );
            let b1 = block(
                ctx,
                BlockId(1),
                &[ValueId(2)],
                &[inst_const(ValueId(3))],
                Terminator::Jump {
                    block: BlockId(2),
                    args: Array::from_slice(*ctx, &[ValueId(3)]),
                },
            );
            let b2 = block(
                ctx,
                BlockId(2),
                &[ValueId(4)],
                &[],
                Terminator::TailContinue {
                    callee: TailTarget::Indirect {
                        closure: ValueId(4),
                    },
                    args: Array::from_slice(*ctx, &[] as &[ValueId]),
                    source: None,
                },
            );

            let ordered = proc(
                ctx,
                BlockId(0),
                ValueId(0),
                &[ValueId(1)],
                &[b0.clone(), b1.clone(), b2.clone()],
            );
            let shuffled = proc(ctx, BlockId(0), ValueId(0), &[ValueId(1)], &[b2, b0, b1]);

            assert_eq!(
                allocate_proc(&ordered).unwrap(),
                allocate_proc(&shuffled).unwrap()
            );
        });
    }
}
