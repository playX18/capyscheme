//! Heap objects for the I/O layer.

use super::*;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Visitor, WeakProcessor};
use std::sync::Arc;


#[repr(C)]
pub struct Poller {
    pub(crate) inner: Arc<polling::Poller>,
}

pub(crate) fn poller_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::POLLER).expect("builtin class id is nonzero"))
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Poller`
unsafe impl ClassTagged for Poller {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::POLLER];
    const TYPE_NAME: &'static str = "poller";
}

// SAFETY: GC trace for `Poller`, all reachable heap fields are visited
unsafe impl Trace for Poller {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}
