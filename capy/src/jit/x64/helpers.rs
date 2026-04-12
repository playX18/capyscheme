use asmkit::{
    core::operand::Label,
    x86::{self, Gpq},
};
use mmtk::{AllocationSemantics, util::alloc::AllocatorSelector};

use crate::{
    GarbageCollector,
    jit::{self, x64::lower::CompilerX64},
    sync::thread::Thread,
};

pub fn allocate(
    comp: &mut CompilerX64<'_, '_>,
    thread: Gpq,
    obj: Gpq,
    var_size_in_bytes: Option<Gpq>,
    con_size_in_bytes: usize,
    t1: Gpq,
    slow_case: Label,
) {
    let max_non_los_bytes = comp.ctx.mc.thread().max_non_los_alloc_bytes();

    if !jit::options::get().enable_allocation_fastpath {
        comp.asm.jmp(slow_case);
        return;
    }

    // MMTk size check. If the alloc size is larger than the allowed max size for non los,
    // we jump to slow path and allodate with LOS in slowpath.

    let allocator = AllocationSemantics::Default;
    let selector =
        mmtk::memory_manager::get_allocator_mapping(&GarbageCollector::get().mmtk, allocator);

    if matches!(
        selector,
        AllocatorSelector::FreeList(_)
            | AllocatorSelector::Malloc(_)
            | AllocatorSelector::LargeObject(_)
    ) {
        // We can't at the moment inline them, so we just jump to the slow path which will call the allocator.
        comp.asm.jmp(slow_case);
        return;
    }

    if let Some(var_size_in_bytes) = var_size_in_bytes {
        // slow path if var_size_in_bytes > max_non_los_bytes
        comp.asm
            .cmp(var_size_in_bytes, x86::imm(max_non_los_bytes as i64));
        comp.asm.jnc(slow_case);
    } else {
        if con_size_in_bytes > max_non_los_bytes {
            comp.asm.jmp(slow_case);
            return;
        }
    }

    let tlab_cursor_offset = Thread::LAB_OFFSET_CURSOR as i32;
    let tlab_limit_offset = Thread::LAB_OFFSET_LIMIT as i32;

    let cursor = x86::ptr64(thread, tlab_cursor_offset);
    let limit = x86::ptr64(thread, tlab_limit_offset);

    comp.asm.mov(obj, cursor);

    let end = t1;

    if let Some(var_size_in_bytes) = var_size_in_bytes {
        comp.asm
            .lea(end, x86::ptr64_index(obj, var_size_in_bytes, 1, 0));
    } else {
        comp.asm.lea(end, x86::ptr64(obj, con_size_in_bytes as i32));
    }

    // slow path if end < obj
    comp.asm.cmp(end, obj);
    comp.asm.jc(slow_case);
    // slow path if end > limit
    comp.asm.cmp(end, limit);
    comp.asm.ja(slow_case);
    comp.asm.mov(cursor, end);
}
