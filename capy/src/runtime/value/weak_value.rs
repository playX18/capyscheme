//! Weak references encoded in value slots.

use std::marker::PhantomData;

use crate::rsgc::{Mutation, Trace};

use super::{EncodedValueDescriptor, Value};

#[derive(Copy, Clone)]
/// A value slot that is processed as a weak reference by the GC.
pub struct WeakValue<'gc> {
    desc: EncodedValueDescriptor,
    pd: PhantomData<&'gc ()>,
}

impl<'gc> From<Value<'gc>> for WeakValue<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Self {
            desc: value.desc,
            pd: PhantomData,
        }
    }
}

impl<'gc> WeakValue<'gc> {
    /// Wraps a value for weak processing.
    pub fn from_value(value: Value<'gc>) -> Self {
        Self {
            desc: value.desc,
            pd: PhantomData,
        }
    }

    /// Returns the current value representation.
    pub fn as_value(self) -> Value<'gc> {
        Value {
            desc: self.desc,
            pd: PhantomData,
        }
    }

    pub fn is_broken(&self) -> bool {
        self.as_value().raw_i64() == Value::bwp().raw_i64()
    }

    pub fn get(&self, mc: Mutation<'gc>) -> Value<'gc> {
        if !self.as_value().is_cell() {
            return self.as_value();
        }

        // SAFETY: The value descriptor contains a valid GC object pointer
        unsafe {
            mc.raw_weak_reference_load(self.as_value().desc.ptr());
        }
        self.as_value()
    }
}

// SAFETY: WeakValue wraps the same NaN-boxed representation as Value. During
// tracing we only register for weak processing; actual pointer updates happen
// in `process_weak_refs` where we check liveness and replace dead refs with BWP.
unsafe impl<'gc> Trace for WeakValue<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        visitor.register_for_weak_processing();
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let value = self.as_value();

            if value.is_cell() {
                let ptr = weak_processor.is_live_object(value.desc.ptr());
                if ptr.is_null() {
                    self.desc = Value::bwp().desc;
                } else {
                    self.desc.ptr = ptr.to_address().to_mut_ptr();
                }
            } else {
                self.desc = Value::bwp().desc;
            }
        }
    }
}

impl<'gc> Eq for WeakValue<'gc> {}

impl<'gc> PartialEq for WeakValue<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.as_value() == other.as_value()
    }
}

impl<'gc> PartialEq<Value<'gc>> for WeakValue<'gc> {
    fn eq(&self, other: &Value<'gc>) -> bool {
        self.as_value() == *other
    }
}
