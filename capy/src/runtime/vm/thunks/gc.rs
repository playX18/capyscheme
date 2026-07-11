use crate::rsgc::{
    ObjectSlot,
    mmtk::{AllocationSemantics, MutatorContext, util::ObjectReference},
};
use crate::runtime::{Context, value::Value};

pub fn yieldpoint_block<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
) {
    // Entry yieldpoints run before argument loading. Save the ABI roots so
    // GC does not rely on machine registers being visible to conservative
    // stack scanning while this thunk blocks.
    ctx.state()
        .gc_save
        .save_entry(rator, argc, [arg0, arg1, arg2, arg3]);
    crate::rsgc::sync::thread::Thread::yieldpoint();
    crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
    ctx.state().gc_save.clear();
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
