use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

use smallvec::SmallVec;

use super::descriptor::ClassDescriptor;
use super::flags::{ClassCategory, ClassFlags};
use super::generic::generic_assertion_violation;
use super::slot::{SlotAccessError, SlotAccessorDescriptor, SlotInitError};
use super::storage::SlotStorage;
use super::table::class_table;
use crate::heap::object::class_header_word;
use crate::heap::{Gc, Trace};
use crate::runtime::{
    MutationContext,
    interp::stack::Slot,
    value::{Closure, PROCEDURES, ReturnCode, Value},
    vm::Call,
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SchemeInstance<'gc> {
    class: Gc<'gc, ClassDescriptor<'gc>>,
    storage: Gc<'gc, SlotStorage<'gc>>,
}

impl<'gc> SchemeInstance<'gc> {
    pub fn allocate(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Gc<'gc, Self> {
        debug_assert_eq!(class.category(), ClassCategory::Scheme);
        let header_word = class_header_word(class.id());
        ctx.allocate_with(
            Self {
                class,
                storage: SlotStorage::with(
                    ctx,
                    class.layout_stamp(),
                    class.slot_count(),
                    |_, index| class.slots()[index].init_value(),
                ),
            },
            header_word,
        )
    }

    pub fn allocate_with_initargs(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, Self>, SlotInitError<'gc>> {
        let mut resolved_initargs: SmallVec<[(usize, Value<'gc>); 8]> = SmallVec::new();
        for &(keyword, value) in initargs {
            let slot = class
                .slot_for_init_keyword(keyword)
                .ok_or(SlotInitError::UnknownKeyword(keyword))?;
            if !slot.flags().initializable() {
                return Err(SlotInitError::NotInitializable(keyword));
            }
            resolved_initargs.push((slot.index(), value));
        }

        let instance = Self::allocate(ctx, class);
        let mut explicit_slots: SmallVec<[usize; 8]> = SmallVec::new();
        for &(slot_index, value) in &resolved_initargs {
            assert!(
                slot_index < instance.storage.len(),
                "scheme instance slot out of bounds"
            );
            // SAFETY: the storage is fresh (allocated above), so these
            // init stores need no write barrier, matching the template
            // allocators' raw init stores.
            unsafe {
                instance.storage.values()[slot_index].as_cell().set(value);
            }
            explicit_slots.push(slot_index);
        }

        // Most classes have no init thunks: scan first and return without
        // rooting a frame. Nothing above can collect, so the fresh
        // instance cannot have moved.
        let slot_count = class.slots().len();
        let mut needs_thunks = false;
        for index in 0..slot_count {
            let slot = class.slots()[index];
            if !slot.init_thunk().is_empty() && !explicit_slots.contains(&slot.index()) {
                needs_thunks = true;
                break;
            }
        }
        if !needs_thunks {
            return Ok(instance);
        }

        // An init thunk may collect: the instance and the class are rooted
        // across each call and reloaded. The count is read once: `class`
        // below is stale after the first call.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let instance_slot = frame.new_local_ref(instance.into()).as_ptr();
        let class_slot = frame.new_local_ref(class.into()).as_ptr();
        // SAFETY: the slots live until `frame` drops, and hold the instance
        // and the class, updated by the collector.
        let reload = |slot: *mut Value<'static>| unsafe {
            std::mem::transmute::<Value<'static>, Value<'gc>>(*slot).as_cell_raw()
        };
        for index in 0..slot_count {
            // SAFETY: see `reload`.
            let class: Gc<'gc, ClassDescriptor<'gc>> =
                unsafe { Gc::from_gc_object(reload(class_slot)) };
            let slot = class.slots()[index];
            if slot.init_thunk().is_empty() || explicit_slots.contains(&slot.index()) {
                continue;
            }
            let slot_index = slot.index();
            match crate::runtime::sni::call_function(ctx, slot.init_thunk(), []) {
                crate::runtime::vm::ExecutionResult::Ok(value) => {
                    // SAFETY: see `reload`.
                    let instance = unsafe { Gc::from_gc_object(reload(instance_slot)) };
                    Self::set_slot(ctx, instance, slot_index, value);
                }
                crate::runtime::vm::ExecutionResult::Err(error) => {
                    return Err(SlotInitError::InitThunkFailed(error));
                }
            }
        }

        // SAFETY: see `reload`.
        Ok(unsafe { Gc::from_gc_object(reload(instance_slot)) })
    }

    pub fn class(&self) -> Gc<'gc, ClassDescriptor<'gc>> {
        self.class
    }

    /// The current slot storage. Migration replaces it with a Release store,
    /// so every reader loads it once and works on that snapshot.
    #[inline(always)]
    pub fn storage(&self) -> Gc<'gc, SlotStorage<'gc>> {
        // SAFETY: `storage` is a single non-null `Gc` word, written only at
        // construction and by `migrate_to_class`'s atomic store.
        unsafe {
            let word = &*(&self.storage as *const _ as *const AtomicUsize);
            Gc::from_ptr(word.load(Ordering::Acquire) as *const _)
        }
    }

    /// Slot `index` if it is in bounds of the current storage.
    #[inline(always)]
    pub fn slot_checked(&self, index: usize) -> Option<Value<'gc>> {
        self.storage().get(index)
    }

    /// Slot `index` without bounds or binding checks.
    ///
    /// # Safety
    /// `index < self.slot_count()`.
    #[inline(always)]
    pub unsafe fn slot_unchecked(&self, index: usize) -> Value<'gc> {
        let storage = self.storage();
        debug_assert!(index < storage.len());
        unsafe { storage.values().get_unchecked(index).get() }
    }

    fn migrate_to_class(
        ctx: MutationContext<'gc>,
        instance: Gc<'gc, Self>,
        old_class: Gc<'gc, ClassDescriptor<'gc>>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let migrated = SlotStorage::with(
            ctx,
            new_class.layout_stamp(),
            new_class.slot_count(),
            |_, index| {
                let slot = new_class.slots()[index];
                old_class
                    .accessor_named(slot.name())
                    .and_then(|old_accessor| instance.slot(old_accessor.slot_index()))
                    .unwrap_or_else(|| slot.init_value())
            },
        );

        Gc::write(ctx, instance);
        // Publish the class, then the storage (both Release); the storage
        // store is the publish point, since cache hits read only it. The
        // header id follows for `change-class`, which changes the identity
        // id. A slot write racing this migration of the same instance may
        // be lost.
        // SAFETY: `class` and `storage` are single `Gc` words; this is the
        // only writer, and readers load them atomically.
        unsafe {
            let instance_ptr = instance.as_gc_object().to_address().to_mut_ptr::<Self>();
            let class = &*(&raw const (*instance_ptr).class as *const AtomicUsize);
            class.store(new_class.as_ptr() as usize, Ordering::Release);
            let storage = &*(&raw const (*instance_ptr).storage as *const AtomicUsize);
            storage.store(migrated.as_ptr() as usize, Ordering::Release);
        }
        std::sync::atomic::fence(Ordering::Release);
        instance
            .as_gc_object()
            .header()
            .set_class_id(new_class.id());
    }

    pub fn change_class(
        ctx: MutationContext<'gc>,
        instance: Gc<'gc, Self>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        debug_assert_eq!(new_class.category(), ClassCategory::Scheme);
        let old_class = instance.class;
        Self::migrate_to_class(ctx, instance, old_class, new_class);
    }

    pub fn touch_in_table(
        ctx: MutationContext<'gc>,
        table: &super::table::ClassTable<'gc>,
        instance: Gc<'gc, Self>,
    ) -> bool {
        let old_class = instance.class;
        let Some(current_class) = table.lookup(old_class.id()) else {
            return false;
        };
        if current_class.layout_stamp() == instance.storage().stamp() {
            return false;
        }
        Self::migrate_to_class(ctx, instance, old_class, current_class);
        true
    }

    pub fn touch(ctx: MutationContext<'gc>, instance: Gc<'gc, Self>) -> bool {
        Self::touch_in_table(ctx, class_table(ctx), instance)
    }

    pub fn slot_count(&self) -> usize {
        self.storage().len()
    }

    pub fn slot(&self, index: usize) -> Option<Value<'gc>> {
        self.slot_checked(index)
    }

    pub fn slot_is_bound(&self, index: usize) -> bool {
        self.slot(index).is_some_and(|value| !value.is_empty())
    }

    pub fn read_slot(&self, index: usize) -> Result<Value<'gc>, SlotAccessError> {
        match self.slot(index) {
            Some(value) if !value.is_empty() => Ok(value),
            Some(_) => Err(SlotAccessError::Unbound),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    pub fn set_slot(
        ctx: MutationContext<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        index: usize,
        value: Value<'gc>,
    ) {
        // One snapshot for the check and the store: a migration between two
        // loads of `storage` must not turn the check into a stale one.
        let storage = instance.storage();
        assert!(index < storage.len(), "scheme instance slot out of bounds");
        Gc::write(ctx, storage)[index].unlock().set(value);
    }

    /// [`Self::set_slot`] that reports an out-of-bounds index instead of
    /// panicking: cache hits that raced a migration fall back to the slow path.
    #[inline]
    pub fn set_slot_checked(
        ctx: MutationContext<'gc>,
        instance: Gc<'gc, Self>,
        index: usize,
        value: Value<'gc>,
    ) -> bool {
        let storage = instance.storage();
        if index >= storage.len() {
            return false;
        }
        Gc::write(ctx, storage)[index].unlock().set(value);
        true
    }

    pub fn slot_by_accessor(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        self.read_slot(accessor.slot_index())
    }

    pub fn slot_by_name(
        &self,
        name: &str,
        fallback: Option<Value<'gc>>,
    ) -> Result<Value<'gc>, SlotAccessError> {
        match self.class.accessor_named(name) {
            Some(accessor) => self.slot_by_accessor(accessor),
            None => fallback.ok_or(SlotAccessError::Unknown),
        }
    }

    pub fn slot_bound_by_accessor(
        &self,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Result<bool, SlotAccessError> {
        if accessor.slot_index() >= self.storage().len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Ok(self.slot_is_bound(accessor.slot_index()))
    }

    pub fn slot_bound_by_name(&self, name: &str) -> Result<bool, SlotAccessError> {
        match self.class.accessor_named(name) {
            Some(accessor) => self.slot_bound_by_accessor(accessor),
            None => Err(SlotAccessError::Unknown),
        }
    }

    pub fn set_slot_by_accessor(
        ctx: MutationContext<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        if accessor.slot_index() >= instance.storage().len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Self::set_slot(ctx, instance, accessor.slot_index(), value);
        Ok(())
    }
}

impl<'gc> From<Gc<'gc, SchemeInstance<'gc>>> for Value<'gc> {
    fn from(instance: Gc<'gc, SchemeInstance<'gc>>) -> Self {
        Value::from_raw(instance.as_gc_object().to_address().as_usize() as u64)
    }
}

pub fn try_scheme_instance<'gc>(
    ctx: MutationContext<'gc>,
    value: Value<'gc>,
) -> Option<Gc<'gc, SchemeInstance<'gc>>> {
    let class = value.class(ctx)?;
    if class.category() != ClassCategory::Scheme {
        return None;
    }
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    Some(unsafe { Gc::from_gc_object(value.as_cell_raw()) })
}

pub fn print_primitive_value(
    fmt: &mut fmt::Formatter<'_>,
    value: Value<'_>,
) -> Option<fmt::Result> {
    let class = scheme_instance_class_for_primitive_value(value)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gc_object(value.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .print()
        .map(|print| print(fmt, class, value))
}

pub fn compare_primitive_values(lhs: Value<'_>, rhs: Value<'_>) -> Option<bool> {
    let class = scheme_instance_class_for_primitive_value(lhs)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gc_object(lhs.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .compare()
        .and_then(|compare| compare(class, lhs, rhs))
}

pub fn hash_primitive_value(value: Value<'_>) -> Option<u64> {
    let class = scheme_instance_class_for_primitive_value(value)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gc_object(value.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .hash()
        .map(|hash| hash(class, value))
}

fn scheme_instance_class_for_primitive_value<'gc>(
    value: Value<'gc>,
) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
    if !value.is_cell() {
        return None;
    }
    let class_id = value.as_cell_raw().class_id();
    let table = super::table::CLASS_TABLE.get()?;
    // SAFETY: The table is guaranteed to be initialized before this point
    let class = unsafe { table.fetch_unchecked() }.lookup(class_id)?;
    (class.category() == ClassCategory::Scheme).then_some(class)
}

pub fn scheme_instance_allocate_hook<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    SchemeInstance::allocate_with_initargs(ctx, class, initargs).map(Value::from)
}

fn invocable_scheme_instance_allocate_hook<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    let instance = SchemeInstance::allocate_with_initargs(ctx, class, initargs)?;
    // Init thunks may have collected: take the class from the instance.
    let class = instance.class();
    let Some(invoker) = class.accessor_named("procedure") else {
        return Err(SlotInitError::MissingInvokerSlot(class.into()));
    };
    let target = match instance.slot_by_accessor(invoker) {
        Ok(target) if target.is::<Closure>() => target,
        Ok(target) => return Err(SlotInitError::InvokerNotProcedure(target)),
        Err(_) => return Err(SlotInitError::InvokerNotProcedure(invoker.init_keyword())),
    };
    Ok(invocable_instance_procedure(ctx, class, instance, target).into())
}

extern "C" fn invocable_instance_closure_proc<'gc>(
    ctx: MutationContext<'gc>,
    sfp: Slot<'gc>,
) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        if !call.rator.is::<Closure>() {
            return generic_assertion_violation(
                ctx,
                "invocable-instance",
                "invocable wrapper is not a closure",
                call.rator,
            );
        }
        let closure = call.rator.downcast::<Closure>();
        if closure.nfree <= 2 {
            return generic_assertion_violation(
                ctx,
                "invocable-instance",
                "invocable wrapper is missing its target",
                call.rator,
            );
        }
        let instance = closure[1].get();
        let target = closure[2].get();
        if !target.is::<Closure>() {
            return generic_assertion_violation(
                ctx,
                "invocable-instance",
                "invocable target is not a procedure",
                target,
            );
        }
        let mut forwarded = Vec::with_capacity(call.args.len() + 1);
        forwarded.push(instance);
        forwarded.extend_from_slice(call.args);
        ctx.stage_tail_call(target, &forwarded)
    })
}

fn invocable_instance_procedure<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    instance: Gc<'gc, SchemeInstance<'gc>>,
    target: Value<'gc>,
) -> Gc<'gc, Closure<'gc>> {
    let properties = super::generic::generic_property_entry(
        ctx,
        ctx.intern("name"),
        ctx.intern(class.name()),
        super::generic::generic_property_entry(
            ctx,
            ctx.intern("class"),
            class.into(),
            super::generic::generic_property_entry(
                ctx,
                ctx.intern("instance"),
                instance.into(),
                Value::null(),
            ),
        ),
    );
    PROCEDURES.fetch(ctx).make_closure(
        ctx,
        invocable_instance_closure_proc,
        [instance.into(), target],
        properties,
    )
}

pub fn scheme_instance_print_hook<'gc>(
    fmt: &mut fmt::Formatter<'_>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    value: Value<'gc>,
) -> fmt::Result {
    write!(fmt, "#<instance {} {:p}>", class.name(), value)
}

pub fn scheme_instance_compare_hook<'gc>(
    _class: Gc<'gc, ClassDescriptor<'gc>>,
    lhs: Value<'gc>,
    rhs: Value<'gc>,
) -> Option<bool> {
    Some(lhs == rhs)
}

pub fn scheme_instance_hash_hook<'gc>(
    _class: Gc<'gc, ClassDescriptor<'gc>>,
    value: Value<'gc>,
) -> u64 {
    value.as_cell_raw().hashcode()
}

pub fn scheme_instance_slot_ref_hook<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
) -> Result<Value<'gc>, SlotAccessError> {
    if accessor.class_allocated() {
        return class.read_class_slot(accessor);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    instance.slot_by_accessor(accessor)
}

pub fn scheme_instance_slot_set_hook<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
    value: Value<'gc>,
) -> Result<(), SlotAccessError> {
    if accessor.class_allocated() {
        return ClassDescriptor::set_class_slot(ctx, class, accessor, value);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    SchemeInstance::set_slot_by_accessor(ctx, instance, accessor, value)
}

pub fn scheme_instance_slot_bound_hook<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
) -> Result<bool, SlotAccessError> {
    if accessor.class_allocated() {
        return class.class_slot_bound(accessor);
    }
    let Some(instance) = try_scheme_instance(ctx, receiver) else {
        return Err(SlotAccessError::OutOfBounds);
    };
    instance.slot_bound_by_accessor(accessor)
}

pub(crate) fn invocable_scheme_class_options() -> super::table::DynamicClassOptions {
    use super::hooks::PrimitiveOperationHooks;
    super::table::DynamicClassOptions {
        primitive_operation_hooks: Some(PrimitiveOperationHooks::new(
            Some(invocable_scheme_instance_allocate_hook),
            Some(scheme_instance_print_hook),
            Some(scheme_instance_compare_hook),
            Some(scheme_instance_hash_hook),
            Some(scheme_instance_slot_ref_hook),
            Some(scheme_instance_slot_set_hook),
            Some(scheme_instance_slot_bound_hook),
        )),
        flags: Some(ClassFlags::scheme_applicable()),
    }
}
