use crate::heap::Space;
use crate::heap::{
    ObjectSlot,
    mmtk::{MutatorContext, util::ObjectReference},
};
use crate::runtime::{MutationContext, value::Value};

/// Block for a pending GC / interrupt at a compiled Scheme entry yieldpoint.

pub fn yieldpoint_block<'gc>(ctx: MutationContext<'gc>) {
    crate::heap::sync::thread::Thread::yieldpoint();
    unsafe { crate::runtime::sched::scheduler::kill_at_yieldpoint(ctx) };
    crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
    crate::runtime::sched::scheduler::preempt_at_yieldpoint();
    unsafe { crate::runtime::sched::scheduler::kill_at_yieldpoint(ctx) };
}

pub fn pre_write_barrier_at_slot<'gc>(
    ctx: MutationContext<'gc>,
    src: ObjectReference,
    slot: ObjectSlot,
    target: ObjectReference,
) {
    unsafe {
        log::debug!(
            "pre write barrier: src={:?}, slot={:?}, target={:?}",
            src,
            slot,
            target
        );

        ctx.thread_unchecked()
            .mutator_unchecked()
            .barrier()
            .object_reference_write_pre(src, slot, Some(target))
    }
}

pub fn post_write_barrier_at_slot<'gc>(
    ctx: MutationContext<'gc>,
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

        ctx.thread_unchecked()
            .mutator_unchecked()
            .barrier()
            .object_reference_write_slow(src, slot, Some(target))
    }
}

pub fn post_write_barrier_slow<'gc>(
    ctx: MutationContext<'gc>,
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

        ctx.thread_unchecked()
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
    ctx: MutationContext<'gc>,
    header_word: usize,
    size: usize,
) -> Value<'gc> {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let val = ctx.allocate_raw(size, 8, header_word as u64, Space::Default);

        Value::from_raw(val.to_address().as_usize() as u64)
    }
}
