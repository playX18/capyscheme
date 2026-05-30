use mmtk::util::{Address, ObjectReference};
use mmtk::vm::RootsWorkFactory;

use crate::rsgc::sync::thread::Thread;

/// Check if `addr` is a valid MMTk heap object reference (requires VO bit).
pub fn is_mmtk_heap_object(addr: Address) -> Option<ObjectReference> {
    if addr.is_zero() || !addr.is_aligned_to(mmtk::util::ObjectReference::ALIGNMENT) {
        return None;
    }
    mmtk::memory_manager::is_mmtk_object(addr)
}

pub(crate) fn scan_conservative_native_stack(
    thread: &Thread,
    factory: &mut impl RootsWorkFactory<crate::rsgc::ObjectSlot>,
) {
    // Compiled entry yieldpoints save their ABI roots in State::gc_save before
    // blocking. This native stack scan is only the conservative fallback for
    // Rust/native frames and compiler spills between the recorded SP and stack
    // high bound.
    let scan_sp = thread.gc_scan_sp();
    if scan_sp.is_zero() {
        return;
    }

    let (stack_low, stack_high) = thread.stack_bounds();
    if stack_low.is_zero() || stack_high <= stack_low {
        return;
    }

    let scan_start = scan_sp.max(stack_low);
    if scan_start >= stack_high {
        return;
    }

    let heap_start = mmtk::memory_manager::starting_heap_address();
    let heap_end = mmtk::memory_manager::last_heap_address();

    let mut pinning_roots = Vec::new();

    let mut word_addr = scan_start;
    while word_addr < stack_high {
        let word = unsafe { word_addr.load::<usize>() };
        if word != 0 && word % mmtk::util::ObjectReference::ALIGNMENT == 0 {
            let addr = unsafe { Address::from_usize(word) };
            if addr >= heap_start && addr < heap_end {
                if let Some(obj) = is_mmtk_heap_object(addr) {
                    pinning_roots.push(obj);
                } else if let Some(obj) =
                    mmtk::memory_manager::find_object_from_internal_pointer(addr, 64)
                {
                    pinning_roots.push(obj);
                }
            }
        }
        word_addr += mmtk::util::ObjectReference::ALIGNMENT;
    }

    if !pinning_roots.is_empty() {
        pinning_roots.sort_unstable();
        pinning_roots.dedup();
        factory.create_process_pinning_roots_work(pinning_roots);
    }
}
