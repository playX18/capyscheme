use std::fmt;

use crate::rsgc::object::{AllocationHooks, GCObject};
use crate::rsgc::{Gc, Visitor, WeakProcessor};
use crate::runtime::Context;
use crate::runtime::value::Value;
use super::descriptor::ClassDescriptor;
use super::slot::{SlotAccessError, SlotAccessorDescriptor, SlotInitError};

#[derive(Clone, Copy)]
pub struct PrimitiveLayoutHooks {
    trace: extern "C" fn(GCObject, &mut Visitor),
    weak_proc: extern "C" fn(GCObject, &mut WeakProcessor),
    instance_size: usize,
    compute_size: Option<extern "C" fn(GCObject) -> usize>,
    alignment: usize,
    compute_alignment: Option<extern "C" fn(GCObject) -> usize>,
    type_name: &'static str,
}

impl PrimitiveLayoutHooks {
    pub const fn from_allocation_hooks(hooks: AllocationHooks) -> Self {
        Self {
            trace: hooks.trace,
            weak_proc: hooks.weak_proc,
            instance_size: hooks.instance_size,
            compute_size: hooks.compute_size,
            alignment: hooks.alignment,
            compute_alignment: hooks.compute_alignment,
            type_name: hooks.type_name,
        }
    }

    pub fn trace(self) -> extern "C" fn(GCObject, &mut Visitor) {
        self.trace
    }

    pub fn weak_proc(self) -> extern "C" fn(GCObject, &mut WeakProcessor) {
        self.weak_proc
    }

    pub fn instance_size(self) -> usize {
        self.instance_size
    }

    pub fn compute_size(self) -> Option<extern "C" fn(GCObject) -> usize> {
        self.compute_size
    }

    pub fn alignment(self) -> usize {
        self.alignment
    }

    pub fn compute_alignment(self) -> Option<extern "C" fn(GCObject) -> usize> {
        self.compute_alignment
    }

    pub fn type_name(self) -> &'static str {
        self.type_name
    }
}

impl core::fmt::Debug for PrimitiveLayoutHooks {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrimitiveLayoutHooks")
            .field("instance_size", &self.instance_size)
            .field("compute_size", &self.compute_size.is_some())
            .field("alignment", &self.alignment)
            .field("compute_alignment", &self.compute_alignment.is_some())
            .field("type_name", &self.type_name)
            .finish()
    }
}

pub type PrimitiveAllocateHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>>;
pub type PrimitivePrintHook = for<'gc, 'a, 'b> fn(
    &'b mut fmt::Formatter<'a>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
) -> fmt::Result;
pub type PrimitiveCompareHook =
    for<'gc> fn(Gc<'gc, ClassDescriptor<'gc>>, Value<'gc>, Value<'gc>) -> Option<bool>;
pub type PrimitiveHashHook = for<'gc> fn(Gc<'gc, ClassDescriptor<'gc>>, Value<'gc>) -> u64;
pub type PrimitiveSlotRefHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
) -> Result<Value<'gc>, SlotAccessError>;
pub type PrimitiveSlotSetHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
    Value<'gc>,
) -> Result<(), SlotAccessError>;
pub type PrimitiveSlotBoundHook = for<'gc> fn(
    Context<'gc>,
    Gc<'gc, ClassDescriptor<'gc>>,
    Value<'gc>,
    SlotAccessorDescriptor<'gc>,
) -> Result<bool, SlotAccessError>;

#[derive(Clone, Copy, Default)]
pub struct PrimitiveOperationHooks {
    allocate: Option<PrimitiveAllocateHook>,
    print: Option<PrimitivePrintHook>,
    compare: Option<PrimitiveCompareHook>,
    hash: Option<PrimitiveHashHook>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
}

impl PrimitiveOperationHooks {
    pub const fn new(
        allocate: Option<PrimitiveAllocateHook>,
        print: Option<PrimitivePrintHook>,
        compare: Option<PrimitiveCompareHook>,
        hash: Option<PrimitiveHashHook>,
        slot_ref: Option<PrimitiveSlotRefHook>,
        slot_set: Option<PrimitiveSlotSetHook>,
        slot_bound: Option<PrimitiveSlotBoundHook>,
    ) -> Self {
        Self {
            allocate,
            print,
            compare,
            hash,
            slot_ref,
            slot_set,
            slot_bound,
        }
    }

    pub fn allocate(self) -> Option<PrimitiveAllocateHook> {
        self.allocate
    }

    pub fn print(self) -> Option<PrimitivePrintHook> {
        self.print
    }

    pub fn compare(self) -> Option<PrimitiveCompareHook> {
        self.compare
    }

    pub fn hash(self) -> Option<PrimitiveHashHook> {
        self.hash
    }

    pub fn slot_ref(self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }
}

impl core::fmt::Debug for PrimitiveOperationHooks {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrimitiveOperationHooks")
            .field("allocate", &self.allocate.is_some())
            .field("print", &self.print.is_some())
            .field("compare", &self.compare.is_some())
            .field("hash", &self.hash.is_some())
            .field("slot_ref", &self.slot_ref.is_some())
            .field("slot_set", &self.slot_set.is_some())
            .field("slot_bound", &self.slot_bound.is_some())
            .finish()
    }
}
