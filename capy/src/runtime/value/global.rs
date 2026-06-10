//! Value storage used by global roots and module-level state.

use crate::rsgc::{
    Mutation, ObjectSlot, Trace,
    mmtk::{util::Address, vm::SlotVisitor},
};

use super::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// A value encoded for storage outside a particular GC lifetime.
pub struct GlobalValue(u64);

impl GlobalValue {
    /// Captures the raw bits of a value.
    pub const fn new<'gc>(value: Value<'gc>) -> Self {
        Self(value.bits())
    }

    /// Reconstitutes the value for the active mutation context.
    pub fn get<'gc>(self, _ctx: Mutation<'gc>) -> Value<'gc> {
        Value::from_raw_i64(self.0 as i64)
    }
}

// SAFETY: Zero bits is a valid GlobalValue (wraps VALUE_EMPTY).
unsafe impl bytemuck::Zeroable for GlobalValue {}

// SAFETY: GlobalValue is #[repr(transparent)] over u64: any bit pattern is a
// valid value, and it has no padding or interior references.
unsafe impl bytemuck::Pod for GlobalValue {}

// SAFETY: GlobalValue stores the same NaN-boxed bits as Value. The `is_cell()`
// check ensures we only expose GC pointers to the visitor.
unsafe impl Trace for GlobalValue {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        let value = Value::from_raw_i64(self.0 as i64);
        if value.is_cell() {
            visitor.visit_slot(ObjectSlot::from_address(Address::from_mut_ptr(&mut self.0)));
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}
