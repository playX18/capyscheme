use super::flags::{ClassCategory, ClassFlags};
use super::hooks::{PrimitiveLayoutHooks, PrimitiveOperationHooks};
use super::instance::SchemeInstance;
use super::slot::{SlotAccessError, SlotAccessorDescriptor, SlotDescriptor, SlotSpec};
use crate::prelude::*;
use crate::rsgc::alloc::{Array, ArrayRef};
use crate::rsgc::cell::Lock;
use crate::rsgc::object::{AllocationHooksOf, ClassId, builtin_class_ids};
use crate::rsgc::{Gc, Trace, Visitor, Weak, WeakProcessor};
use crate::runtime::value::conversions::ClassTagged;
use crate::runtime::{Context, value::Value};

pub struct ClassDescriptor<'gc> {
    id: ClassId,
    name: ArrayRef<'gc, u8>,
    category: ClassCategory,
    flags: Lock<ClassFlags>,
    primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
    primitive_operation_hooks: PrimitiveOperationHooks,
    direct_supers: ArrayRef<'gc, ClassId>,
    cpl: ArrayRef<'gc, ClassId>,
    direct_slots: ArrayRef<'gc, SlotDescriptor<'gc>>,
    slots: ArrayRef<'gc, SlotDescriptor<'gc>>,
    accessors: ArrayRef<'gc, SlotAccessorDescriptor<'gc>>,
    class_slots: ArrayRef<'gc, Lock<Value<'gc>>>,
    direct_subclasses: Lock<ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>>>,
    direct_methods: Lock<ArrayRef<'gc, Value<'gc>>>,
    initargs: ArrayRef<'gc, Value<'gc>>,
}

pub(crate) struct ClassDescriptorInit<'a, 'gc> {
    pub(crate) id: ClassId,
    pub(crate) name: &'a str,
    pub(crate) category: ClassCategory,
    pub(crate) primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
    pub(crate) primitive_operation_hooks: Option<PrimitiveOperationHooks>,
    pub(crate) flags: Option<ClassFlags>,
    pub(crate) direct_supers: &'a [ClassId],
    pub(crate) cpl: &'a [ClassId],
    pub(crate) inherited_slots: &'a [SlotDescriptor<'gc>],
    pub(crate) direct_slot_specs: &'a [SlotSpec<'a, 'gc>],
    pub(crate) direct_subclasses: Option<ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>>>,
    pub(crate) direct_methods: Option<ArrayRef<'gc, Value<'gc>>>,
}

impl core::fmt::Debug for ClassDescriptor<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ClassDescriptor")
            .field("id", &self.id)
            .field("name", &self.name())
            .field("category", &self.category)
            .field("flags", &self.flags())
            .field("primitive_layout_hooks", &self.primitive_layout_hooks)
            .field("primitive_operation_hooks", &self.primitive_operation_hooks)
            .field("direct_supers", &self.direct_supers())
            .field("cpl", &self.cpl())
            .field("direct_slots_len", &self.direct_slots.len())
            .field("slots_len", &self.slots.len())
            .field("accessors_len", &self.accessors.len())
            .field("class_slots_len", &self.class_slots.len())
            .field("direct_subclasses_len", &self.direct_subclasses.get().len())
            .field("direct_methods_len", &self.direct_methods.get().len())
            .field("initargs_len", &self.initargs.len())
            .finish()
    }
}

unsafe impl Trace for ClassDescriptor<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.flags.trace(visitor);
            self.direct_supers.trace(visitor);
            self.cpl.trace(visitor);
            self.direct_slots.trace(visitor);
            self.slots.trace(visitor);
            self.accessors.trace(visitor);
            self.class_slots.trace(visitor);
            self.direct_subclasses.trace(visitor);
            self.direct_methods.trace(visitor);
            self.initargs.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.flags.process_weak_refs(weak_processor);
            self.direct_supers.process_weak_refs(weak_processor);
            self.cpl.process_weak_refs(weak_processor);
            self.direct_slots.process_weak_refs(weak_processor);
            self.slots.process_weak_refs(weak_processor);
            self.accessors.process_weak_refs(weak_processor);
            self.class_slots.process_weak_refs(weak_processor);
            self.direct_subclasses.process_weak_refs(weak_processor);
            self.direct_methods.process_weak_refs(weak_processor);
            self.initargs.process_weak_refs(weak_processor);
        }
    }
}

unsafe impl<'gc> ClassTagged for ClassDescriptor<'gc> {
    const TYPE_NAME: &'static str = "class";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CLASS];
}

impl<'gc> ClassDescriptor<'gc> {
    pub fn new(ctx: Context<'gc>, id: ClassId, name: &str, category: ClassCategory) -> Self {
        Self::with_hierarchy(ctx, id, name, category, &[], &[id])
    }

    pub fn with_hierarchy(
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        cpl: &[ClassId],
    ) -> Self {
        Self::with_primitive_layout_hooks(ctx, id, name, category, None, direct_supers, cpl)
    }

    pub fn with_primitive_layout_hooks(
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        primitive_layout_hooks: Option<PrimitiveLayoutHooks>,
        direct_supers: &[ClassId],
        cpl: &[ClassId],
    ) -> Self {
        Self::from_init(
            ctx,
            ClassDescriptorInit {
                id,
                name,
                category,
                primitive_layout_hooks,
                primitive_operation_hooks: None,
                flags: None,
                direct_supers,
                cpl,
                inherited_slots: &[],
                direct_slot_specs: &[],
                direct_subclasses: None,
                direct_methods: None,
            },
        )
    }

    pub(crate) fn from_init(ctx: Context<'gc>, init: ClassDescriptorInit<'_, 'gc>) -> Self {
        let inherited_len = init.inherited_slots.len();
        let primitive_layout_hooks = init
            .primitive_layout_hooks
            .or_else(|| default_primitive_layout_hooks(init.category));
        let primitive_operation_hooks = init
            .primitive_operation_hooks
            .unwrap_or_else(|| default_primitive_operation_hooks(init.category));
        let direct_slots = Array::with(*ctx, init.direct_slot_specs.len(), |ctx, index| {
            SlotDescriptor::from_spec(
                ctx.into(),
                init.id,
                inherited_len + index,
                init.direct_slot_specs[index],
            )
        });
        let slots = Array::with(
            *ctx,
            inherited_len + init.direct_slot_specs.len(),
            |ctx, index| {
                if index < inherited_len {
                    init.inherited_slots[index]
                } else {
                    SlotDescriptor::from_spec(
                        ctx.into(),
                        init.id,
                        index,
                        init.direct_slot_specs[index - inherited_len],
                    )
                }
            },
        );
        let accessors = Array::with(*ctx, slots.len(), |ctx, index| {
            SlotAccessorDescriptor::from_slot(ctx.into(), slots[index])
        });
        let class_slots = Array::with(*ctx, slots.len(), |_, index| {
            let slot = slots[index];
            if slot.class_allocated() {
                Lock::new(slot.init_value())
            } else {
                Lock::new(Value::empty())
            }
        });
        let initargs = initarg_metadata(ctx, slots.as_slice());

        Self {
            id: init.id,
            name: Array::from_slice(*ctx, init.name.as_bytes()),
            category: init.category,
            flags: Lock::new(init.flags.unwrap_or_else(|| init.category.default_flags())),
            primitive_layout_hooks,
            primitive_operation_hooks,
            direct_supers: Array::from_slice(*ctx, init.direct_supers),
            cpl: Array::from_slice(*ctx, init.cpl),
            direct_slots,
            slots,
            accessors,
            class_slots,
            direct_subclasses: Lock::new(
                init.direct_subclasses
                    .unwrap_or_else(|| empty_weak_subclasses(ctx)),
            ),
            direct_methods: Lock::new(init.direct_methods.unwrap_or_else(|| empty_metadata(ctx))),
            initargs,
        }
    }

    pub fn id(&self) -> ClassId {
        self.id
    }

    pub fn name(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn category(&self) -> ClassCategory {
        self.category
    }

    pub fn flags(&self) -> ClassFlags {
        self.flags.get()
    }

    pub fn seal(&self) {
        let flags = self.flags().with_sealed().without_malleable();
        unsafe { self.flags.as_cell().set(flags) };
    }

    pub fn unseal(&self) -> bool {
        let flags = if self.category == ClassCategory::Scheme {
            self.flags().without_sealed().with_malleable()
        } else {
            return false;
        };
        unsafe { self.flags.as_cell().set(flags) };
        true
    }

    pub fn primitive_layout_hooks(&self) -> Option<PrimitiveLayoutHooks> {
        self.primitive_layout_hooks
    }

    pub fn primitive_operation_hooks(&self) -> PrimitiveOperationHooks {
        self.primitive_operation_hooks
    }

    pub fn allocate_instance(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Option<Result<Value<'gc>, super::slot::SlotInitError<'gc>>> {
        self.primitive_operation_hooks
            .allocate()
            .map(|allocate| allocate(ctx, class, initargs))
    }

    pub fn slot_value_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
        fallback: Option<Value<'gc>>,
    ) -> Option<Result<Value<'gc>, SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(fallback.ok_or(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_ref().is_empty() {
            return Some(call_scheme_slot_ref(
                ctx,
                receiver,
                accessor.scheme_slot_ref(),
            ));
        }
        accessor
            .slot_ref()
            .or_else(|| self.primitive_operation_hooks.slot_ref())
            .map(|slot_ref| slot_ref(ctx, class, receiver, accessor))
    }

    pub fn set_slot_value_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
        value: Value<'gc>,
    ) -> Option<Result<(), SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(Err(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_set().is_empty() {
            return Some(call_scheme_slot_set(
                ctx,
                receiver,
                value,
                accessor.scheme_slot_set(),
            ));
        }
        accessor
            .slot_set()
            .or_else(|| self.primitive_operation_hooks.slot_set())
            .map(|slot_set| slot_set(ctx, class, receiver, accessor, value))
    }

    pub fn slot_bound_by_name(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        receiver: Value<'gc>,
        name: &str,
    ) -> Option<Result<bool, SlotAccessError>> {
        let accessor = match self.accessor_named(name) {
            Some(accessor) => accessor,
            None => return Some(Err(SlotAccessError::Unknown)),
        };
        if !accessor.scheme_slot_bound().is_empty() {
            return Some(call_scheme_slot_bound(
                ctx,
                receiver,
                accessor.scheme_slot_bound(),
            ));
        }
        accessor
            .slot_bound()
            .or_else(|| self.primitive_operation_hooks.slot_bound())
            .map(|slot_bound| slot_bound(ctx, class, receiver, accessor))
    }

    pub fn direct_supers(&self) -> &[ClassId] {
        self.direct_supers.as_slice()
    }

    pub fn cpl(&self) -> &[ClassId] {
        self.cpl.as_slice()
    }

    pub fn direct_slots(&self) -> &[SlotDescriptor<'gc>] {
        self.direct_slots.as_slice()
    }

    pub fn slots(&self) -> &[SlotDescriptor<'gc>] {
        self.slots.as_slice()
    }

    pub fn accessors(&self) -> &[SlotAccessorDescriptor<'gc>] {
        self.accessors.as_slice()
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn accessor_named(&self, name: &str) -> Option<SlotAccessorDescriptor<'gc>> {
        self.accessors()
            .iter()
            .copied()
            .find(|accessor| accessor.name() == name)
    }

    pub fn slot_for_init_keyword(&self, keyword: Value<'gc>) -> Option<SlotDescriptor<'gc>> {
        if keyword.is_empty() {
            return None;
        }
        self.slots()
            .iter()
            .copied()
            .find(|slot| !slot.class_allocated() && slot.init_keyword() == keyword)
    }

    pub fn direct_subclasses(&self) -> ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>> {
        self.direct_subclasses.get()
    }

    pub fn direct_methods(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.direct_methods.get()
    }

    pub fn initargs(&self) -> &[Value<'gc>] {
        self.initargs.as_slice()
    }

    pub fn class_slot(&self, index: usize) -> Option<Value<'gc>> {
        self.class_slots.as_slice().get(index).map(Lock::get)
    }

    pub fn class_slot_is_bound(&self, index: usize) -> bool {
        self.class_slot(index)
            .is_some_and(|value| !value.is_empty())
    }

    pub fn read_class_slot(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        if !accessor.class_allocated() {
            return Err(SlotAccessError::OutOfBounds);
        }
        match self.class_slot(accessor.slot_index()) {
            Some(value) if !value.is_empty() => Ok(value),
            Some(_) => Err(SlotAccessError::Unbound),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    pub fn set_class_slot(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if !accessor.class_allocated() || accessor.slot_index() >= class.class_slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        if accessor.flags().immutable_flag() || !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        Gc::write(*ctx, class.class_slots)[accessor.slot_index()]
            .unlock()
            .set(value);
        Ok(())
    }

    pub fn class_slot_bound(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<bool, SlotAccessError> {
        if !accessor.class_allocated() || accessor.slot_index() >= self.class_slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Ok(self.class_slot_is_bound(accessor.slot_index()))
    }

    pub fn is_a(&self, id: ClassId) -> bool {
        self.cpl.as_slice().contains(&id)
    }

    pub(crate) fn add_direct_subclass(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let current = class.direct_subclasses.get();
        if current.iter().any(|weak| {
            weak.upgrade(*ctx)
                .is_some_and(|existing| existing.id() == subclass.id())
        }) {
            return;
        }
        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                Gc::downgrade(subclass)
            }
        });
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(appended);
    }

    pub(crate) fn remove_direct_subclass(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass_id: ClassId,
    ) {
        let current = class.direct_subclasses.get();
        let filtered = current
            .iter()
            .copied()
            .filter(|weak| {
                weak.upgrade(*ctx)
                    .is_none_or(|existing| existing.id() != subclass_id)
            })
            .collect::<Vec<_>>();
        if filtered.len() == current.len() {
            return;
        }
        let filtered = Array::from_slice(*ctx, &filtered);
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(filtered);
    }

    pub(crate) fn add_direct_method(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        method: Value<'gc>,
    ) {
        let current = class.direct_methods.get();
        if current.contains(&method) {
            return;
        }
        let appended = Array::with(*ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(*ctx, class), ClassDescriptor, direct_methods)
            .unlock()
            .set(appended);
    }

    pub(crate) fn invalidate_direct_method_caches(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        for method in class.direct_methods().iter().copied() {
            if let Some(method) = method.try_as::<super::generic::MethodDescriptor>() {
                super::generic::GenericDescriptor::clear_dispatcher_cache(ctx, method.generic());
            }
        }
    }
}

fn empty_metadata<'gc>(ctx: Context<'gc>) -> ArrayRef<'gc, Value<'gc>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length metadata arrays are never initialized")
    })
}

fn initarg_metadata<'gc>(
    ctx: Context<'gc>,
    slots: &[SlotDescriptor<'gc>],
) -> ArrayRef<'gc, Value<'gc>> {
    let initargs = slots
        .iter()
        .filter_map(|slot| {
            let keyword = slot.init_keyword();
            (!slot.class_allocated() && !keyword.is_empty() && slot.flags().initializable())
                .then_some(keyword)
        })
        .collect::<Vec<_>>();
    Array::from_slice(*ctx, &initargs)
}

pub(crate) fn class_id_list_to_class_objects<'gc>(
    ctx: Context<'gc>,
    ids: &[ClassId],
) -> Value<'gc> {
    let classes = ids
        .iter()
        .filter_map(|id| super::table::class_table(ctx).lookup(*id).map(Value::from))
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &classes)
}

pub(crate) fn class_id_to_class_object<'gc>(ctx: Context<'gc>, id: ClassId) -> Value<'gc> {
    super::table::class_table(ctx)
        .lookup(id)
        .map(Value::from)
        .unwrap_or_else(|| Value::new(false))
}

pub(crate) fn slot_definition_list<'gc>(
    ctx: Context<'gc>,
    slots: &[SlotDescriptor<'gc>],
) -> Value<'gc> {
    let definitions = slots
        .iter()
        .map(|slot| super::slot::SlotDefinitionDescriptor::from_slot(ctx, *slot).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

pub(crate) fn slot_accessor_list<'gc>(
    ctx: Context<'gc>,
    accessors: &[SlotAccessorDescriptor<'gc>],
) -> Value<'gc> {
    let definitions = accessors
        .iter()
        .map(|accessor| super::slot::SlotAccessorDefinition::from_accessor(ctx, *accessor).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

fn empty_weak_subclasses<'gc>(ctx: Context<'gc>) -> ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>> {
    Array::with(*ctx, 0, |_, _| {
        unreachable!("zero-length weak subclass arrays are never initialized")
    })
}

fn default_primitive_layout_hooks(category: ClassCategory) -> Option<PrimitiveLayoutHooks> {
    match category {
        ClassCategory::Scheme => Some(PrimitiveLayoutHooks::from_allocation_hooks(
            AllocationHooksOf::<'static, SchemeInstance<'static>>::HOOKS,
        )),
        ClassCategory::Abstract
        | ClassCategory::Builtin
        | ClassCategory::Immediate
        | ClassCategory::Internal => None,
    }
}

fn default_primitive_operation_hooks(category: ClassCategory) -> PrimitiveOperationHooks {
    match category {
        ClassCategory::Scheme => PrimitiveOperationHooks::new(
            Some(super::instance::scheme_instance_allocate_hook),
            Some(super::instance::scheme_instance_print_hook),
            Some(super::instance::scheme_instance_compare_hook),
            Some(super::instance::scheme_instance_hash_hook),
            Some(super::instance::scheme_instance_slot_ref_hook),
            Some(super::instance::scheme_instance_slot_set_hook),
            Some(super::instance::scheme_instance_slot_bound_hook),
        ),
        ClassCategory::Abstract
        | ClassCategory::Builtin
        | ClassCategory::Immediate
        | ClassCategory::Internal => PrimitiveOperationHooks::default(),
    }
}

pub(crate) fn call_scheme_slot_ref<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<Value<'gc>, SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver]) {
        crate::runtime::vm::VMResult::Ok(value) => Ok(value),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}

pub(crate) fn call_scheme_slot_set<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    value: Value<'gc>,
    proc: Value<'gc>,
) -> Result<(), SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver, value]) {
        crate::runtime::vm::VMResult::Ok(_) => Ok(()),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}

pub(crate) fn call_scheme_slot_bound<'gc>(
    ctx: Context<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<bool, SlotAccessError> {
    match crate::runtime::vm::call_scheme(ctx, proc, [receiver]) {
        crate::runtime::vm::VMResult::Ok(value) => Ok(value != Value::new(false)),
        crate::runtime::vm::VMResult::Err(_) => Err(SlotAccessError::SchemeHookFailed),
    }
}
