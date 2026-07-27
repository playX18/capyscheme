use mmtk::util::{Address, ObjectReference};

/// Check if `addr` is a valid MMTk heap object reference (requires VO bit).
pub fn is_mmtk_heap_object(addr: Address) -> Option<ObjectReference> {
    if addr.is_zero() || !addr.is_aligned_to(mmtk::util::ObjectReference::ALIGNMENT) {
        return None;
    }
    mmtk::memory_manager::is_mmtk_object(addr)
}
