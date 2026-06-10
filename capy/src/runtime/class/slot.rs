use super::builtin::builtin_id;
use super::hooks::{PrimitiveSlotBoundHook, PrimitiveSlotRefHook, PrimitiveSlotSetHook};
use crate::rsgc::alloc::{Array, ArrayRef};
use crate::rsgc::object::{ClassId, builtin_class_ids, class_header_word};
use crate::rsgc::{Gc, Trace, Visitor, WeakProcessor};
use crate::runtime::value::conversions::ClassTagged;
use crate::runtime::{Context, value::Value};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct SlotFlags(u8);

impl SlotFlags {
    const IMMUTABLE: u8 = 1 << 0;
    const INITIALIZABLE: u8 = 1 << 1;
    const SETTABLE: u8 = 1 << 2;
    const CLASS_ALLOCATED: u8 = 1 << 3;

    pub const fn mutable() -> Self {
        Self(Self::INITIALIZABLE | Self::SETTABLE)
    }

    pub const fn immutable() -> Self {
        Self(Self::IMMUTABLE | Self::INITIALIZABLE)
    }

    pub const fn immutable_flag(self) -> bool {
        self.0 & Self::IMMUTABLE != 0
    }

    pub const fn initializable(self) -> bool {
        self.0 & Self::INITIALIZABLE != 0
    }

    pub const fn settable(self) -> bool {
        self.0 & Self::SETTABLE != 0
    }

    pub const fn class_allocated(self) -> bool {
        self.0 & Self::CLASS_ALLOCATED != 0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotSpec<'a, 'gc> {
    name: &'a str,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'a, 'gc> SlotSpec<'a, 'gc> {
    pub const fn mutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::mutable(),
        }
    }

    pub const fn immutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::immutable(),
        }
    }

    pub const fn with_init_value(mut self, value: Value<'gc>) -> Self {
        self.init_value = value;
        self
    }

    pub const fn with_init_keyword(mut self, keyword: Value<'gc>) -> Self {
        self.init_keyword = keyword;
        self
    }

    pub const fn with_init_thunk(mut self, thunk: Value<'gc>) -> Self {
        self.init_thunk = thunk;
        self
    }

    pub const fn with_native_slot_hooks(
        mut self,
        slot_ref: Option<PrimitiveSlotRefHook>,
        slot_set: Option<PrimitiveSlotSetHook>,
        slot_bound: Option<PrimitiveSlotBoundHook>,
    ) -> Self {
        self.slot_ref = slot_ref;
        self.slot_set = slot_set;
        self.slot_bound = slot_bound;
        self
    }

    pub const fn with_scheme_slot_procedures(
        mut self,
        slot_ref: Value<'gc>,
        slot_set: Value<'gc>,
        slot_bound: Value<'gc>,
    ) -> Self {
        self.scheme_slot_ref = slot_ref;
        self.scheme_slot_set = slot_set;
        self.scheme_slot_bound = slot_bound;
        self
    }

    pub const fn with_accessor_names(
        mut self,
        getter: Value<'gc>,
        setter: Value<'gc>,
        accessor: Value<'gc>,
    ) -> Self {
        self.getter = getter;
        self.setter = setter;
        self.accessor = accessor;
        self
    }

    pub const fn without_initargs(mut self) -> Self {
        self.flags.0 &= !SlotFlags::INITIALIZABLE;
        self
    }

    pub const fn without_setter(mut self) -> Self {
        self.flags.0 &= !SlotFlags::SETTABLE;
        self
    }

    pub const fn with_class_allocation(mut self) -> Self {
        self.flags.0 |= SlotFlags::CLASS_ALLOCATED;
        self
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotDescriptor<'gc> {
    pub(crate) name: ArrayRef<'gc, u8>,
    pub(crate) owner: ClassId,
    pub(crate) index: usize,
    pub(crate) init_value: Value<'gc>,
    pub(crate) init_keyword: Value<'gc>,
    pub(crate) init_thunk: Value<'gc>,
    pub(crate) slot_ref: Option<PrimitiveSlotRefHook>,
    pub(crate) slot_set: Option<PrimitiveSlotSetHook>,
    pub(crate) slot_bound: Option<PrimitiveSlotBoundHook>,
    pub(crate) scheme_slot_ref: Value<'gc>,
    pub(crate) scheme_slot_set: Value<'gc>,
    pub(crate) scheme_slot_bound: Value<'gc>,
    pub(crate) getter: Value<'gc>,
    pub(crate) setter: Value<'gc>,
    pub(crate) accessor: Value<'gc>,
    pub(crate) flags: SlotFlags,
}

unsafe impl Trace for SlotDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

impl<'gc> SlotDescriptor<'gc> {
    pub(crate) fn from_spec(
        ctx: Context<'gc>,
        owner: ClassId,
        index: usize,
        spec: SlotSpec<'_, 'gc>,
    ) -> Self {
        Self {
            name: Array::from_slice(*ctx, spec.name.as_bytes()),
            owner,
            index,
            init_value: spec.init_value,
            init_keyword: spec.init_keyword,
            init_thunk: spec.init_thunk,
            slot_ref: spec.slot_ref,
            slot_set: spec.slot_set,
            slot_bound: spec.slot_bound,
            scheme_slot_ref: spec.scheme_slot_ref,
            scheme_slot_set: spec.scheme_slot_set,
            scheme_slot_bound: spec.scheme_slot_bound,
            getter: spec.getter,
            setter: spec.setter,
            accessor: spec.accessor,
            flags: spec.flags,
        }
    }

    pub fn name(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn immutable(&self) -> bool {
        self.flags.immutable_flag()
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotAccessorDescriptor<'gc> {
    pub(crate) name: ArrayRef<'gc, u8>,
    pub(crate) owner: ClassId,
    pub(crate) slot_index: usize,
    pub(crate) init_value: Value<'gc>,
    pub(crate) init_keyword: Value<'gc>,
    pub(crate) init_thunk: Value<'gc>,
    pub(crate) slot_ref: Option<PrimitiveSlotRefHook>,
    pub(crate) slot_set: Option<PrimitiveSlotSetHook>,
    pub(crate) slot_bound: Option<PrimitiveSlotBoundHook>,
    pub(crate) scheme_slot_ref: Value<'gc>,
    pub(crate) scheme_slot_set: Value<'gc>,
    pub(crate) scheme_slot_bound: Value<'gc>,
    pub(crate) getter: Value<'gc>,
    pub(crate) setter: Value<'gc>,
    pub(crate) accessor: Value<'gc>,
    pub(crate) flags: SlotFlags,
}

unsafe impl Trace for SlotAccessorDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotAccessError {
    Immutable,
    OutOfBounds,
    SchemeHookFailed,
    Unbound,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotInitError<'gc> {
    UnknownKeyword(Value<'gc>),
    NotInitializable(Value<'gc>),
    InitThunkFailed(Value<'gc>),
    MissingInvokerSlot(Value<'gc>),
    InvokerNotProcedure(Value<'gc>),
}

impl<'gc> SlotAccessorDescriptor<'gc> {
    pub(crate) fn from_slot(ctx: Context<'gc>, slot: SlotDescriptor<'gc>) -> Self {
        Self {
            name: Array::from_slice(*ctx, slot.name.as_slice()),
            owner: slot.owner,
            slot_index: slot.index,
            init_value: slot.init_value,
            init_keyword: slot.init_keyword,
            init_thunk: slot.init_thunk,
            slot_ref: slot.slot_ref,
            slot_set: slot.slot_set,
            slot_bound: slot.slot_bound,
            scheme_slot_ref: slot.scheme_slot_ref,
            scheme_slot_set: slot.scheme_slot_set,
            scheme_slot_bound: slot.scheme_slot_bound,
            getter: slot.getter,
            setter: slot.setter,
            accessor: slot.accessor,
            flags: slot.flags,
        }
    }

    pub fn name(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotDefinitionDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'gc> SlotDefinitionDescriptor<'gc> {
    pub(crate) fn from_slot(ctx: Context<'gc>, slot: SlotDescriptor<'gc>) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                name: Array::from_slice(*ctx, slot.name.as_slice()),
                owner: slot.owner,
                index: slot.index,
                init_value: slot.init_value,
                init_keyword: slot.init_keyword,
                init_thunk: slot.init_thunk,
                scheme_slot_ref: slot.scheme_slot_ref,
                scheme_slot_set: slot.scheme_slot_set,
                scheme_slot_bound: slot.scheme_slot_bound,
                getter: slot.getter,
                setter: slot.setter,
                accessor: slot.accessor,
                flags: slot.flags,
            },
            class_header_word(builtin_id(builtin_class_ids::SLOT_DEFINITION)),
        )
    }

    pub fn name(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

unsafe impl<'gc> ClassTagged for SlotDefinitionDescriptor<'gc> {
    const TYPE_NAME: &'static str = "slot-definition";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SLOT_DEFINITION];
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotAccessorDefinition<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    slot_index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'gc> SlotAccessorDefinition<'gc> {
    pub(crate) fn from_accessor(
        ctx: Context<'gc>,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                name: Array::from_slice(*ctx, accessor.name.as_slice()),
                owner: accessor.owner,
                slot_index: accessor.slot_index,
                init_value: accessor.init_value,
                init_keyword: accessor.init_keyword,
                init_thunk: accessor.init_thunk,
                scheme_slot_ref: accessor.scheme_slot_ref,
                scheme_slot_set: accessor.scheme_slot_set,
                scheme_slot_bound: accessor.scheme_slot_bound,
                getter: accessor.getter,
                setter: accessor.setter,
                accessor: accessor.accessor,
                flags: accessor.flags,
            },
            class_header_word(builtin_id(builtin_class_ids::SLOT_ACCESSOR)),
        )
    }

    pub fn name(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub(crate) fn as_accessor_descriptor(&self) -> SlotAccessorDescriptor<'gc> {
        SlotAccessorDescriptor {
            name: self.name,
            owner: self.owner,
            slot_index: self.slot_index,
            init_value: self.init_value,
            init_keyword: self.init_keyword,
            init_thunk: self.init_thunk,
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: self.scheme_slot_ref,
            scheme_slot_set: self.scheme_slot_set,
            scheme_slot_bound: self.scheme_slot_bound,
            getter: self.getter,
            setter: self.setter,
            accessor: self.accessor,
            flags: self.flags,
        }
    }
}

unsafe impl<'gc> ClassTagged for SlotAccessorDefinition<'gc> {
    const TYPE_NAME: &'static str = "slot-accessor";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SLOT_ACCESSOR];
}
