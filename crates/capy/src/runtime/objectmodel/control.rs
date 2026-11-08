//! Heap objects for continuation marks.

use super::*;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};

#[repr(C)]
#[derive(Trace)]
pub struct ContinuationMarks<'gc> {
    pub cmarks: Value<'gc>,
}

pub(crate) fn continuation_marks_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONTINUATION_MARKS).expect("builtin class id is nonzero"),
    )
}

// SAFETY: `gc` for `ContinuationMarks` upholds all trait invariants
unsafe impl<'gc> ClassTagged for ContinuationMarks<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONTINUATION_MARKS];

    const TYPE_NAME: &'static str = "continuation-marks";
}


