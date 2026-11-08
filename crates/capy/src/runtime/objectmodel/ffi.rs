//! Heap objects for the foreign-function interface: wrapped pointers and
//! prepared libffi call interfaces.

use super::*;
use crate::heap::mmtk::util::Address;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};


/// A wrapped C pointer. This allows us to protect
/// from accidental UBs, but we won't take away
/// the ability to do unsafe things.
///
/// A pointer into GC-managed memory records that object in `owner`, so the
/// memory lives exactly as long as the pointer. `owner` is `#f` otherwise.
#[repr(C)]
pub struct Pointer<'gc> {
    pub(crate) value: Address,
    pub(crate) owner: Value<'gc>,
}

// SAFETY: `Send` is required so pointers can be queued for finalization. The finalizer
// queue only stores the `ObjectReference` and the collector, which owns `owner` through
// `Trace`, is the only code that touches it from another thread.
unsafe impl Send for Pointer<'_> {}

pub fn pointer_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::POINTER).expect("builtin class id is nonzero"),
    )
}

// SAFETY: GC trace for `Pointer`, all reachable heap fields are visited
unsafe impl<'gc> Trace for Pointer<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        visitor.trace(&mut self.owner);
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Pointer<'gc> {
    pub fn new(value: *mut std::ffi::c_void) -> Self {
        Self::with_owner(value, Value::new(false))
    }

    /// A pointer that keeps `owner` alive for as long as it is reachable.
    pub fn with_owner(value: *mut std::ffi::c_void, owner: Value<'gc>) -> Self {
        Self {
            value: Address::from_ptr(value as *mut u8),
            owner,
        }
    }

    pub fn value(&self) -> *mut std::ffi::c_void {
        self.value.to_mut_ptr()
    }
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Pointer`
unsafe impl<'gc> ClassTagged for Pointer<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::POINTER];
    const TYPE_NAME: &'static str = "pointer";
}

pub struct Cif<'gc> {
    pub(crate) cif: libffi::middle::Cif,
    pub(crate) nargs: u32,
    pub(crate) variadic: bool,
    /// Whether the foreign call can block. Blocking calls first yield to native
    /// before using the slower calling convention.
    ///
    /// Anything that can block the mutator thread should be marked as blocking. Example:
    /// - `epoll`, `poll`, `read` etc can block
    pub(crate) blocking: bool,
    pub(crate) args: Value<'gc>,
    pub(crate) return_type: Value<'gc>,
}

pub(crate) fn cif_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::CIF).expect("builtin class id is nonzero"))
}

// SAFETY: `gc` for `Cif` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Cif<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CIF];
    const TYPE_NAME: &'static str = "cif";
}

// SAFETY: `gc` for `Cif` upholds all trait invariants
unsafe impl<'gc> Trace for Cif<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        visitor.trace(&mut self.args);
        visitor.trace(&mut self.return_type);
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}
