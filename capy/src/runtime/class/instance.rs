use std::fmt;

use super::descriptor::ClassDescriptor;
use super::flags::{ClassCategory, ClassFlags};
use super::generic::generic_assertion_violation;
use super::slot::{SlotAccessError, SlotAccessorDescriptor, SlotInitError};
use super::table::class_table;
use crate::rsgc::alloc::{Array, ArrayRef};
use crate::rsgc::cell::Lock;
use crate::rsgc::object::class_header_word;
use crate::rsgc::{Gc, Trace};
use crate::runtime::{
    Context,
    value::{Closure, NativeReturn, PROCEDURES, Value},
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SchemeInstance<'gc> {
    class: Gc<'gc, ClassDescriptor<'gc>>,
    slots: ArrayRef<'gc, Lock<Value<'gc>>>,
}

impl<'gc> SchemeInstance<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot_count: usize,
    ) -> Gc<'gc, Self> {
        debug_assert_eq!(class.category(), ClassCategory::Scheme);
        let header_word = class_header_word(class.id());
        Gc::new_with_header_word(
            *ctx,
            Self {
                class,
                slots: Array::with(*ctx, slot_count, |_, _| Lock::new(Value::empty())),
            },
            header_word,
        )
    }

    pub fn allocate(ctx: Context<'gc>, class: Gc<'gc, ClassDescriptor<'gc>>) -> Gc<'gc, Self> {
        debug_assert_eq!(class.category(), ClassCategory::Scheme);
        let header_word = class_header_word(class.id());
        Gc::new_with_header_word(
            *ctx,
            Self {
                class,
                slots: Array::with(*ctx, class.slot_count(), |_, index| {
                    Lock::new(class.slots()[index].init_value())
                }),
            },
            header_word,
        )
    }

    pub fn allocate_with_initargs(
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, Self>, SlotInitError<'gc>> {
        let mut resolved_initargs = Vec::with_capacity(initargs.len());
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
        let mut explicit_slots = Vec::with_capacity(resolved_initargs.len());
        for &(slot_index, value) in &resolved_initargs {
            Self::set_slot(ctx, instance, slot_index, value);
            explicit_slots.push(slot_index);
        }

        for slot in class.slots() {
            if slot.init_thunk().is_empty() || explicit_slots.contains(&slot.index()) {
                continue;
            }
            match crate::runtime::vm::call_scheme(ctx, slot.init_thunk(), []) {
                crate::runtime::vm::VMResult::Ok(value) => {
                    Self::set_slot(ctx, instance, slot.index(), value);
                }
                crate::runtime::vm::VMResult::Err(error) => {
                    return Err(SlotInitError::InitThunkFailed(error));
                }
            }
        }

        Ok(instance)
    }

    pub fn class(&self) -> Gc<'gc, ClassDescriptor<'gc>> {
        self.class
    }

    fn migrate_to_class(
        ctx: Context<'gc>,
        instance: Gc<'gc, Self>,
        old_class: Gc<'gc, ClassDescriptor<'gc>>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let migrated_slots = Array::with(*ctx, new_class.slot_count(), |_, index| {
            let slot = new_class.slots()[index];
            let value = old_class
                .accessor_named(slot.name())
                .and_then(|old_accessor| instance.slot(old_accessor.slot_index()))
                .unwrap_or_else(|| slot.init_value());
            Lock::new(value)
        });

        Gc::write(*ctx, instance);
        instance.as_gcobj().header().set_class_id(new_class.id());
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let instance_mut = instance.as_gcobj().to_address().as_mut_ref::<Self>();
            instance_mut.class = new_class;
            instance_mut.slots = migrated_slots;
        }
    }

    pub fn change_class(
        ctx: Context<'gc>,
        instance: Gc<'gc, Self>,
        new_class: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        debug_assert_eq!(new_class.category(), ClassCategory::Scheme);
        let old_class = instance.class;
        Self::migrate_to_class(ctx, instance, old_class, new_class);
    }

    pub fn touch_in_table(
        ctx: Context<'gc>,
        table: &super::table::ClassTable<'gc>,
        instance: Gc<'gc, Self>,
    ) -> bool {
        let old_class = instance.class;
        let Some(current_class) = table.lookup(old_class.id()) else {
            return false;
        };
        if current_class.as_gcobj() == old_class.as_gcobj() {
            return false;
        }
        Self::migrate_to_class(ctx, instance, old_class, current_class);
        true
    }

    pub fn touch(ctx: Context<'gc>, instance: Gc<'gc, Self>) -> bool {
        Self::touch_in_table(ctx, class_table(ctx), instance)
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn slot(&self, index: usize) -> Option<Value<'gc>> {
        self.slots.as_slice().get(index).map(Lock::get)
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
        ctx: Context<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        index: usize,
        value: Value<'gc>,
    ) {
        assert!(
            index < instance.slots.len(),
            "scheme instance slot out of bounds"
        );
        Gc::write(*ctx, instance.slots)[index].unlock().set(value);
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
        if accessor.slot_index() >= self.slots.len() {
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
        ctx: Context<'gc>,
        instance: Gc<'gc, SchemeInstance<'gc>>,
        accessor: SlotAccessorDescriptor<'gc>,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        if !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        if accessor.slot_index() >= instance.slots.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Self::set_slot(ctx, instance, accessor.slot_index(), value);
        Ok(())
    }
}

impl<'gc> From<Gc<'gc, SchemeInstance<'gc>>> for Value<'gc> {
    fn from(instance: Gc<'gc, SchemeInstance<'gc>>) -> Self {
        Value::from_raw(instance.as_gcobj().to_address().as_usize() as u64)
    }
}

pub fn try_scheme_instance<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
) -> Option<Gc<'gc, SchemeInstance<'gc>>> {
    let class = value.class(ctx)?;
    if class.category() != ClassCategory::Scheme {
        return None;
    }
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    Some(unsafe { Gc::from_gcobj(value.as_cell_raw()) })
}

pub fn print_primitive_value(
    fmt: &mut fmt::Formatter<'_>,
    value: Value<'_>,
) -> Option<fmt::Result> {
    let class = scheme_instance_class_for_primitive_value(value)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(value.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .print()
        .map(|print| print(fmt, class, value))
}

pub fn compare_primitive_values(lhs: Value<'_>, rhs: Value<'_>) -> Option<bool> {
    let class = scheme_instance_class_for_primitive_value(lhs)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(lhs.as_cell_raw()) };
    debug_assert_eq!(instance.class().id(), class.id());
    class
        .primitive_operation_hooks()
        .compare()
        .and_then(|compare| compare(class, lhs, rhs))
}

pub fn hash_primitive_value(value: Value<'_>) -> Option<u64> {
    let class = scheme_instance_class_for_primitive_value(value)?;
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    let instance: Gc<'_, SchemeInstance<'_>> = unsafe { Gc::from_gcobj(value.as_cell_raw()) };
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
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    SchemeInstance::allocate_with_initargs(ctx, class, initargs).map(Value::from)
}

fn invocable_scheme_instance_allocate_hook<'gc>(
    ctx: Context<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[(Value<'gc>, Value<'gc>)],
) -> Result<Value<'gc>, SlotInitError<'gc>> {
    let instance = SchemeInstance::allocate_with_initargs(ctx, class, initargs)?;
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

extern "C-unwind" fn invocable_instance_closure_proc<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
) -> NativeReturn<'gc> {
    if !rator.is::<Closure>() {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable wrapper is not a closure",
            rator,
        );
    }
    let closure = rator.downcast::<Closure>();
    if closure.nfree <= 2 {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable wrapper is missing its target",
            rator,
        );
    }
    let instance = closure[1].get();
    let target = closure[2].get();
    if !target.is::<Closure>() {
        return generic_assertion_violation(
            ctx,
            retk,
            "invocable-instance",
            "invocable target is not a procedure",
            target,
        );
    }
    // SAFETY: Pointer is valid for the given element count
    let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };
    let mut forwarded = Vec::with_capacity(num_rands + 1);
    forwarded.push(instance);
    forwarded.extend_from_slice(args);
    ctx.return_call(target, forwarded, Some(retk))
}

fn invocable_instance_procedure<'gc>(
    ctx: Context<'gc>,
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
    PROCEDURES.fetch(*ctx).make_closure(
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
    ctx: Context<'gc>,
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
    ctx: Context<'gc>,
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
    ctx: Context<'gc>,
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
