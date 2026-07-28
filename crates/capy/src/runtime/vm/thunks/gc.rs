use crate::heap::{
    ObjectSlot,
    mmtk::{AllocationSemantics, MutatorContext, util::ObjectReference},
};
use crate::runtime::{Context, value::Value};

/// Block for a pending GC / interrupt at a compiled Scheme entry yieldpoint.
///
/// Compiled code must store ABI roots into [`State::gc_save`](crate::runtime::State::gc_save)
/// before calling this thunk, then reload those slots afterward and clear the
/// save area. GC relocates the `gc_save` slots in place (they are not pinned).
pub fn yieldpoint_block<'gc>(ctx: Context<'gc>) {
    crate::heap::sync::thread::Thread::yieldpoint();
    crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
}

pub fn pre_write_barrier_at_slot<'gc>(
    ctx: Context<'gc>,
    src: ObjectReference,
    slot: ObjectSlot,
    target: ObjectReference,
) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        log::debug!(
            "pre write barrier: src={:?}, slot={:?}, target={:?}",
            src,
            slot,
            target
        );
        ctx.mc
            .thread_unchecked()
            .mutator_unchecked()
            .barrier()
            .object_reference_write_pre(src, slot, Some(target))
    }
}

pub fn post_write_barrier_at_slot<'gc>(
    ctx: Context<'gc>,
    src: ObjectReference,
    slot: ObjectSlot,
    target: ObjectReference,
) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        log::debug!(
            "post write barrier: src={:?}, slot={:?}, target={:?}",
            src,
            slot,
            target
        );
        ctx.mc
            .thread_unchecked()
            .mutator_unchecked()
            .barrier()
            .object_reference_write_slow(src, slot, Some(target))
    }
}

pub fn post_write_barrier_slow<'gc>(
    ctx: Context<'gc>,
    src: ObjectReference,
    offset: i32,
    target: ObjectReference,
) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        log::debug!(
            "post write barrier: src={:?}, offset={}, target={:?}",
            src,
            offset,
            target
        );
        ctx.mc
            .thread_unchecked()
            .mutator_unchecked()
            .barrier()
            .object_reference_write_slow(
                src,
                ObjectSlot::from_address(src.to_raw_address().offset(offset as _)),
                Some(target),
            )
    }
}

pub fn alloc_with_header_word<'gc>(
    ctx: Context<'gc>,
    header_word: usize,
    size: usize,
) -> Value<'gc> {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let val = ctx.mc.raw_allocate_with_header_word(
            size,
            8,
            header_word as u64,
            AllocationSemantics::Default,
        );

        Value::from_raw(val.to_address().as_usize() as u64)
    }
}
