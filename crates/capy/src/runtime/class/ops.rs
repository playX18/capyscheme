use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::class::ClassDescriptor;
use crate::runtime::class::{
    GenericDescriptor, OP_BOUND, OP_REF, OP_SET, generic_descriptor_from_value,
};
use crate::runtime::interp::ic::protocol_is_plain;
use crate::runtime::value::Value;
use capy_derive::scheme;
use smallvec::SmallVec;

fn class_slot_argument<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    name: &str,
    fallback: Value<'gc>,
) -> Value<'gc> {
    ClassDescriptor::slot_definition_for(ctx, class, name, fallback).unwrap_or(fallback)
}

/// Call a slot protocol generic through its cached effective method: the
/// first method's body runs directly with its next-method argument, with no
/// trip through the generic procedure. `args` are the dispatch arguments.
fn call_protocol<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::Call<'_, 'gc>,
    binding: Value<'gc>,
    args: &[Value<'gc>],
) -> Value<'gc> {
    if let Some(generic) = generic_descriptor_from_value(ctx, binding)
        && let Some(result) = GenericDescriptor::tail_effective(ctx, call, generic, args)
    {
        return result;
    }
    call.tail(binding, args)
}

/// `make-instance`'s body, shared with the native `make` method body.
pub(crate) fn instantiate<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::Call<'_, 'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[Value<'gc>],
) -> Value<'gc> {
    use crate::runtime::class::{ClassCategory, SlotInitError};
    use crate::runtime::value::Keyword;
    if class.category() != ClassCategory::Scheme {
        let who = ctx.intern("make-instance");
        let message = ctx.str("not a Scheme class");
        return call.raise_assertion_violation(who, message, class.into());
    }
    if !initargs.len().is_multiple_of(2) {
        let who = ctx.intern("make-instance");
        let message = ctx.str("initargs must be keyword/value pairs");
        return call.raise_assertion_violation(who, message, Value::new(initargs.len() as i32));
    }

    let mut parsed_initargs: SmallVec<[(Value, Value); 8]> = SmallVec::new();
    for pair in initargs.as_chunks::<2>().0 {
        let keyword_value = pair[0];
        if !keyword_value.is::<Keyword>() {
            let who = ctx.intern("make-instance");
            let message = ctx.str("initarg name is not a keyword");
            return call.raise_assertion_violation(who, message, keyword_value);
        }
        let keyword = keyword_value.downcast::<Keyword>().to_symbol().into();
        parsed_initargs.push((keyword, pair[1]));
    }

    let Some(allocation) = class.allocate_instance(ctx, class, &parsed_initargs) else {
        let who = ctx.intern("make-instance");
        let message = ctx.str("class is not allocatable");
        return call.raise_assertion_violation(who, message, class.into());
    };

    match allocation {
        Ok(instance) => instance,
        Err(SlotInitError::UnknownKeyword(keyword)) => {
            let who = ctx.intern("make-instance");
            let message = ctx.str("unknown init keyword");
            call.raise_assertion_violation(who, message, keyword)
        }
        Err(SlotInitError::NotInitializable(keyword)) => {
            let who = ctx.intern("make-instance");
            let message = ctx.str("slot is not initializable");
            call.raise_assertion_violation(who, message, keyword)
        }
        Err(SlotInitError::InitThunkFailed(error)) => {
            let who = ctx.intern("make-instance");
            let message = ctx.str("slot init thunk failed");
            call.raise_assertion_violation(who, message, error)
        }
        Err(SlotInitError::MissingInvokerSlot(value)) => {
            let who = ctx.intern("make-instance");
            let message = ctx.str("invocable class has no procedure slot");
            call.raise_assertion_violation(who, message, value)
        }
        Err(SlotInitError::InvokerNotProcedure(value)) => {
            let who = ctx.intern("make-instance");
            let message = ctx.str("invocable procedure slot is not a procedure");
            call.raise_assertion_violation(who, message, value)
        }
    }
}

#[scheme(path=capy)]
pub mod class_ops {
    use crate::heap::alloc::Array;
    use crate::prelude::*;

    use crate::runtime::class::{
        ClassCategory, ClassDescriptor, ClassListError, GenericDescriptor, GenericDispatchError,
        MethodDescriptor, MethodFlags, NextMethodDescriptor, SchemeClassSpecError, SchemeInstance,
        SlotAccessError, SlotAccessorDefinition, SlotDefinitionDescriptor, builtin_class_specs,
        call_scheme_slot_bound, call_scheme_slot_ref, call_scheme_slot_set,
        class_id_list_to_class_objects, class_id_to_class_object, class_table,
        generic_descriptor_from_value, generic_procedure, parse_class_list, slot_accessor_list,
        slot_definition_list, try_scheme_instance,
    };
    use crate::runtime::value::{Keyword, Symbol};
    use crate::runtime::watchpoint::grace;

    #[scheme(name = "class?")]
    pub fn class_p(value: Value<'gc>) -> bool {
        value.is::<ClassDescriptor>()
    }

    #[scheme(name = "%builtin-class")]
    pub fn builtin_class(name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let name_string = name.to_string();
        let class_id = if let Some(spec) = builtin_class_specs()
            .iter()
            .copied()
            .find(|spec| spec.name() == name_string)
        {
            spec.id()
        } else {
            let who = ctx.intern("%builtin-class");
            let message = ctx.str("unknown built-in class");
            return call.raise_assertion_violation(who, message, name.into());
        };

        let Some(class) = class_table(ctx).lookup(class_id) else {
            let who = ctx.intern("%builtin-class");
            let message = ctx.str("built-in class is not initialized");
            return call.raise_assertion_violation(who, message, name.into());
        };
        class.into()
    }

    #[scheme(name = "class-name")]
    pub fn class_name(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let name = ctx.str(class.name());
        name
    }

    #[scheme(name = "class-direct-supers")]
    pub fn class_direct_supers(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let supers = class_id_list_to_class_objects(ctx, class.direct_supers());
        supers
    }

    #[scheme(name = "class-precedence-list")]
    pub fn class_precedence_list(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let cpl = class_id_list_to_class_objects(ctx, class.cpl());
        cpl
    }

    #[scheme(name = "class-direct-slots")]
    pub fn class_direct_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(ctx, class.direct_slots());
        slots
    }

    #[scheme(name = "class-slots")]
    pub fn class_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(ctx, class.slots());
        slots
    }

    #[scheme(name = "class-accessors")]
    pub fn class_accessors(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let accessors = slot_accessor_list(ctx, class.accessors());
        accessors
    }

    #[scheme(name = "class-initargs")]
    pub fn class_initargs(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let initargs = Value::list_from_slice(ctx, class.initargs());
        initargs
    }

    #[scheme(name = "class-direct-methods")]
    pub fn class_direct_methods(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let methods = Value::list_from_slice(ctx, class.direct_methods().as_slice());
        methods
    }

    #[scheme(name = "class-direct-subclasses")]
    pub fn class_direct_subclasses(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let subclasses = class
            .direct_subclasses()
            .iter()
            .filter_map(|weak| weak.upgrade(ctx).map(Value::from))
            .collect::<Vec<_>>();
        let subclasses = Value::list_from_slice(ctx, &subclasses);
        subclasses
    }

    #[scheme(name = "class-applicable?")]
    pub fn class_applicable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        class.flags().applicable()
    }

    #[scheme(name = "class-malleable?")]
    pub fn class_malleable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        class.flags().malleable()
    }

    #[scheme(name = "class-sealed?")]
    pub fn class_sealed_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        class.flags().sealed()
    }

    #[scheme(name = "class-seal!")]
    pub fn class_seal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        class.seal();
        class.into()
    }

    #[scheme(name = "class-unseal!")]
    pub fn class_unseal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        if class.unseal() {
            class.into()
        } else {
            let who = ctx.intern("class-unseal!");
            let message = ctx.str("class cannot be unsealed");
            call.raise_assertion_violation(who, message, class.into())
        }
    }

    #[scheme(name = "generic?")]
    pub fn generic_p(value: Value<'gc>) -> bool {
        let result = generic_descriptor_from_value(ctx, value).is_some();
        result
    }

    #[scheme(name = "generic-name")]
    pub fn generic_name(value: Value<'gc>) -> Value<'gc> {
        let name = generic_descriptor_from_value(ctx, value)
            .map(|generic| ctx.str(generic.name()))
            .unwrap_or_else(|| Value::new(false));
        name
    }

    #[scheme(name = "generic-methods")]
    pub fn generic_methods(value: Value<'gc>) -> Value<'gc> {
        let methods = generic_descriptor_from_value(ctx, value)
            .map(|generic| {
                let methods = generic
                    .methods()
                    .iter()
                    .copied()
                    .map(Value::from)
                    .collect::<Vec<_>>();
                Value::list_from_slice(ctx, &methods)
            })
            .unwrap_or_else(|| Value::new(false));
        methods
    }

    #[scheme(name = "generic-fallback")]
    pub fn generic_fallback(value: Value<'gc>) -> Value<'gc> {
        let fallback = generic_descriptor_from_value(ctx, value)
            .map(|generic| generic.fallback())
            .unwrap_or_else(|| Value::new(false));
        fallback
    }

    #[scheme(name = "generic-required-dispatch-arg-count")]
    pub fn generic_required_dispatch_arg_count(value: Value<'gc>) -> Value<'gc> {
        let count = generic_descriptor_from_value(ctx, value)
            .map(|generic| generic.max_required_dispatch_args())
            .unwrap_or(0);
        Value::new(count as i32)
    }

    #[scheme(name = "generic-sealed?")]
    pub fn generic_sealed_p(value: Value<'gc>) -> bool {
        let sealed = generic_descriptor_from_value(ctx, value)
            .map(|generic| generic.flags().sealed())
            .unwrap_or(false);
        sealed
    }

    #[scheme(name = "generic-seal!")]
    pub fn generic_seal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-seal!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(ctx, generic, true);
        Value::void()
    }

    #[scheme(name = "generic-unseal!")]
    pub fn generic_unseal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-unseal!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(ctx, generic, false);
        Value::void()
    }

    #[scheme(name = "method?")]
    pub fn method_p(value: Value<'gc>) -> bool {
        value.is::<MethodDescriptor>()
    }

    #[scheme(name = "method-generic")]
    pub fn method_generic(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        method.generic().into()
    }

    #[scheme(name = "method-specializers")]
    pub fn method_specializers(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        let specializers = class_id_list_to_class_objects(ctx, method.specializers());
        specializers
    }

    #[scheme(name = "method-required-arg-count")]
    pub fn method_required_arg_count(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        Value::new(method.required_arg_count() as i32)
    }

    #[scheme(name = "%make-method-body")]
    pub fn make_method_body_scheme(
        initialize: Value<'gc>,
        default_body: Value<'gc>,
        slow: Value<'gc>,
    ) -> Value<'gc> {
        crate::runtime::class::generic::make_method_body(ctx, initialize, default_body, slow)
    }

    /// Mark every method of a slot protocol generic as the stock one (called
    /// by `base.scm` right after defining them). `Plain` slot caches need to
    /// tell the stock methods from user replacements.
    #[scheme(name = "%mark-boot-slot-protocol!")]
    pub fn mark_boot_slot_protocol(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%mark-boot-slot-protocol!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        for method in generic.methods().iter().copied() {
            MethodDescriptor::set_flags(ctx, method, method.flags().with_boot_slot_protocol());
        }
        Value::undefined()
    }

    /// Count (or stop counting) calls that reach the Rust generic entry, and
    /// read the count: shows whether the machine stub served a call (a test
    /// aid).
    #[scheme(name = "%generic-stats!")]
    pub fn generic_stats(on: bool) -> Value<'gc> {
        crate::runtime::class::generic::DISPATCH_STATS
            .store(on, std::sync::atomic::Ordering::Relaxed);
        crate::runtime::class::generic::RUST_ENTRIES.store(0, std::sync::atomic::Ordering::Relaxed);
        Value::undefined()
    }

    #[scheme(name = "%generic-rust-entries")]
    pub fn generic_rust_entries() -> Value<'gc> {
        Value::new(
            crate::runtime::class::generic::RUST_ENTRIES.load(std::sync::atomic::Ordering::Relaxed)
                as i32,
        )
    }

    /// Slot IC dependents cleared by watchpoint fires so far (a test aid).
    #[scheme(name = "%ic-dependents-cleared")]
    pub fn ic_dependents_cleared() -> Value<'gc> {
        Value::new(
            crate::runtime::watchpoint::DEPENDENTS_CLEARED
                .load(std::sync::atomic::Ordering::Relaxed) as i32,
        )
    }

    #[scheme(name = "method-body")]
    pub fn method_body(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        method.body()
    }

    #[scheme(name = "method-locked?")]
    pub fn method_locked_p(method: Gc<'gc, MethodDescriptor<'gc>>) -> bool {
        method.flags().locked()
    }

    #[scheme(name = "slot-definition?")]
    pub fn slot_definition_p(value: Value<'gc>) -> bool {
        value.is::<SlotDefinitionDescriptor>()
    }

    #[scheme(name = "slot-definition-name")]
    pub fn slot_definition_name(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(ctx, slot.name()).into();
        name
    }

    #[scheme(name = "slot-definition-owner")]
    pub fn slot_definition_owner(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(ctx, slot.owner());
        owner
    }

    #[scheme(name = "slot-definition-index")]
    pub fn slot_definition_index(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        Value::new(slot.index() as i32)
    }

    #[scheme(name = "%slot-definition-allocation")]
    pub fn slot_definition_allocation(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let allocation = if slot.class_allocated() {
            ctx.keyword("class")
        } else {
            ctx.keyword("instance")
        };
        allocation.into()
    }

    #[scheme(name = "slot-definition-init-keyword")]
    pub fn slot_definition_init_keyword(
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        slot.init_keyword()
    }

    #[scheme(name = "slot-definition-init-value")]
    pub fn slot_definition_init_value(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.init_value()
    }

    #[scheme(name = "slot-definition-init-thunk")]
    pub fn slot_definition_init_thunk(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.init_thunk()
    }

    #[scheme(name = "slot-definition-slot-ref")]
    pub fn slot_definition_slot_ref(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.scheme_slot_ref()
    }

    #[scheme(name = "slot-definition-slot-set!")]
    pub fn slot_definition_slot_set(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.scheme_slot_set()
    }

    #[scheme(name = "slot-definition-slot-bound?")]
    pub fn slot_definition_slot_bound(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.scheme_slot_bound()
    }

    #[scheme(name = "slot-definition-getter")]
    pub fn slot_definition_getter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.getter()
    }

    #[scheme(name = "slot-definition-setter")]
    pub fn slot_definition_setter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.setter()
    }

    #[scheme(name = "slot-definition-accessor")]
    pub fn slot_definition_accessor(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        slot.accessor()
    }

    #[scheme(name = "slot-definition-initializable?")]
    pub fn slot_definition_initializable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        slot.flags().initializable()
    }

    #[scheme(name = "slot-definition-settable?")]
    pub fn slot_definition_settable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        slot.flags().settable()
    }

    #[scheme(name = "slot-definition-immutable?")]
    pub fn slot_definition_immutable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        slot.flags().immutable_flag()
    }

    #[scheme(name = "slot-accessor?")]
    pub fn slot_accessor_p(value: Value<'gc>) -> bool {
        value.is::<SlotAccessorDefinition>()
    }

    #[scheme(name = "slot-accessor-name")]
    pub fn slot_accessor_name(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(ctx, accessor.name()).into();
        name
    }

    #[scheme(name = "slot-accessor-owner")]
    pub fn slot_accessor_owner(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(ctx, accessor.owner());
        owner
    }

    #[scheme(name = "slot-accessor-index")]
    pub fn slot_accessor_index(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        Value::new(accessor.slot_index() as i32)
    }

    #[scheme(name = "slot-accessor-init-keyword")]
    pub fn slot_accessor_init_keyword(
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        accessor.init_keyword()
    }

    #[scheme(name = "slot-accessor-init-value")]
    pub fn slot_accessor_init_value(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.init_value()
    }

    #[scheme(name = "slot-accessor-init-thunk")]
    pub fn slot_accessor_init_thunk(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.init_thunk()
    }

    #[scheme(name = "slot-accessor-slot-ref")]
    pub fn slot_accessor_slot_ref(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.scheme_slot_ref()
    }

    #[scheme(name = "slot-accessor-slot-set!")]
    pub fn slot_accessor_slot_set(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.scheme_slot_set()
    }

    #[scheme(name = "slot-accessor-slot-bound?")]
    pub fn slot_accessor_slot_bound(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.scheme_slot_bound()
    }

    #[scheme(name = "slot-accessor-getter")]
    pub fn slot_accessor_getter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.getter()
    }

    #[scheme(name = "slot-accessor-setter")]
    pub fn slot_accessor_setter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.setter()
    }

    #[scheme(name = "slot-accessor-accessor")]
    pub fn slot_accessor_accessor(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        accessor.accessor()
    }

    #[scheme(name = "slot-accessor-initializable?")]
    pub fn slot_accessor_initializable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        accessor.flags().initializable()
    }

    #[scheme(name = "slot-accessor-settable?")]
    pub fn slot_accessor_settable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        accessor.flags().settable()
    }

    #[scheme(name = "slot-accessor-immutable?")]
    pub fn slot_accessor_immutable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        accessor.flags().immutable_flag()
    }

    #[scheme(name = "class-of")]
    pub fn class_of(value: Value<'gc>) -> Value<'gc> {
        match value.class(ctx) {
            Some(class) => class.into(),
            None => Value::new(false),
        }
    }

    #[scheme(name = "current-class-of")]
    pub fn current_class_of(value: Value<'gc>) -> Value<'gc> {
        if let Some(instance) = try_scheme_instance(ctx, value) {
            return instance.class().into();
        }
        match value.class(ctx) {
            Some(class) => class.into(),
            None => Value::new(false),
        }
    }

    #[scheme(name = "%class-redefined?")]
    pub fn class_redefined_p(value: Value<'gc>) -> bool {
        let table = class_table(ctx);
        let class = if let Some(instance) = try_scheme_instance(ctx, value) {
            instance.class()
        } else if value.is::<ClassDescriptor>() {
            value.downcast::<ClassDescriptor>()
        } else {
            return false;
        };
        let Some(current_class) = table.lookup(class.id()) else {
            return false;
        };
        current_class.as_gc_object() != class.as_gc_object()
    }

    #[scheme(name = "touch-instance!")]
    pub fn touch_instance(value: Value<'gc>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, value) else {
            let who = ctx.intern("touch-instance!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, value);
        };
        SchemeInstance::touch(ctx, instance);
        value
    }

    #[scheme(name = "change-class")]
    pub fn change_class(value: Value<'gc>, new_class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, value) else {
            let who = ctx.intern("change-class");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, value);
        };
        if new_class.category() != ClassCategory::Scheme {
            let who = ctx.intern("change-class");
            let message = ctx.str("new class is not a Scheme class");
            return call.raise_assertion_violation(who, message, new_class.into());
        }
        SchemeInstance::touch(ctx, instance);
        SchemeInstance::change_class(ctx, instance, new_class);
        value
    }

    #[scheme(name = "make-class")]
    pub fn make_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(ctx).register_scheme_class_from_slot_list(
            ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => class.into(),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("slot names must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("slot name is not a symbol");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("slot spec options must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("slot option name is not a keyword");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("slot option is missing a value");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("unknown slot option");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("direct supers must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("direct super is not a class");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme classes do not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("could not register class");
                call.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "make-invocable-class")]
    pub fn make_invocable_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(ctx).register_invocable_scheme_class_from_slot_list(
            ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => class.into(),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("slot names must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("slot name is not a symbol");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("slot spec options must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("slot option name is not a keyword");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("slot option is missing a value");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("unknown slot option");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("direct supers must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("direct super is not a class");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("invocable class must define a procedure slot");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("could not register class");
                call.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "redefine-class!")]
    pub fn redefine_class(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(ctx).redefine_scheme_class_from_slot_list(
            ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => grace(ctx, class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("slot names must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("slot name is not a symbol");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("slot spec options must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("slot option name is not a keyword");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("slot option is missing a value");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("unknown slot option");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("direct supers must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("direct super is not a class");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme class redefinition does not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("could not redefine class");
                call.raise_assertion_violation(who, message, class.into())
            }
        }
    }

    #[scheme(name = "redefine-invocable-class!")]
    pub fn redefine_invocable_class(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(ctx).redefine_invocable_scheme_class_from_slot_list(
            ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => grace(ctx, class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("slot names must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("slot name is not a symbol");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("slot spec options must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("slot option name is not a keyword");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("slot option is missing a value");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("unknown slot option");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("direct supers must be a proper list");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("direct super is not a class");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("invocable class must define a procedure slot");
                call.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("could not redefine class");
                call.raise_assertion_violation(who, message, class.into())
            }
        }
    }

    #[scheme(name = "make-generic")]
    pub fn make_generic(
        name: Gc<'gc, Symbol<'gc>>,
        max_required_dispatch_args: Option<i32>,
    ) -> Value<'gc> {
        let max_required_dispatch_args = max_required_dispatch_args.unwrap_or(1);
        if max_required_dispatch_args < 0 {
            let who = ctx.intern("make-generic");
            let message = ctx.str("required dispatch argument count must be non-negative");
            return call.raise_assertion_violation(
                who,
                message,
                Value::new(max_required_dispatch_args),
            );
        }

        let name_value = name.into();
        let name_string = name.to_string();
        let generic =
            GenericDescriptor::new(ctx, &name_string, max_required_dispatch_args as usize);
        let proc = generic_procedure(ctx, name_value, generic);
        grace(ctx, proc.into())
    }

    #[scheme(name = "add-method!")]
    pub fn add_method(
        generic: Value<'gc>,
        specializers: Value<'gc>,
        required_arg_count: i32,
        body: Value<'gc>,
        locked: Option<bool>,
        no_next: Option<bool>,
        slot_kind: Option<Value<'gc>>,
        slot_name: Option<Value<'gc>>,
    ) -> Value<'gc> {
        if required_arg_count < 0 {
            let who = ctx.intern("add-method!");
            let message = ctx.str("required argument count must be non-negative");
            return call.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        let specializers = match parse_class_list(specializers) {
            Ok(specializers) => specializers,
            Err(ClassListError::NotList(value)) => {
                let who = ctx.intern("add-method!");
                let message = ctx.str("specializers must be a proper list");
                return call.raise_assertion_violation(who, message, value);
            }
            Err(ClassListError::NotClass(value)) => {
                let who = ctx.intern("add-method!");
                let message = ctx.str("specializer is not a class");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("add-method!");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };

        if specializers.len() > required_arg_count as usize {
            let who = ctx.intern("add-method!");
            let message = ctx.str("method cannot specialize more arguments than it requires");
            return call.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        if generic.flags().sealed() {
            let who = ctx.intern("add-method!");
            let message = ctx.str("generic is sealed");
            return call.raise_assertion_violation(who, message, generic.into());
        }

        if let Some(method) = generic.find_method(&specializers, required_arg_count as usize)
            && method.flags().locked()
        {
            let who = ctx.intern("add-method!");
            let message = ctx.str("method is locked");
            return call.raise_assertion_violation(who, message, method.into());
        }

        let mut flags = if locked.unwrap_or(false) {
            MethodFlags::locked_flag()
        } else {
            MethodFlags::empty()
        };
        if no_next.unwrap_or(false) {
            flags = flags.with_no_next();
        }
        // `slot_kind` is `reader` or `writer` for a body that is exactly a
        // slot read or write of the symbol `slot_name`, else `#f`.
        let slot_name = slot_name.filter(|name| name.is::<Symbol>());
        let slot_kind = slot_kind
            .filter(|_| slot_name.is_some())
            .and_then(|kind| kind.try_as::<Symbol>())
            .map(|kind| kind.as_str().to_string());
        match slot_kind.as_deref() {
            Some("reader") => flags = flags.with_slot_reader(),
            Some("writer") => flags = flags.with_slot_writer(),
            _ => {}
        }
        let method = GenericDescriptor::add_method_with_flags(
            ctx,
            generic,
            &specializers,
            required_arg_count as usize,
            body,
            flags,
        );
        if let (Some(_), Some(name)) = (slot_kind, slot_name) {
            MethodDescriptor::set_slot_name(ctx, method, name);
            GenericDescriptor::clear_dispatcher_cache(ctx, generic);
        }
        grace(ctx, method.into())
    }

    #[scheme(name = "set-generic-fallback!")]
    pub fn set_generic_fallback(generic: Value<'gc>, fallback: Value<'gc>) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("set-generic-fallback!");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        GenericDescriptor::set_fallback(ctx, generic, fallback);
        grace(ctx, Value::undefined())
    }

    #[scheme(name = "generic-dispatch")]
    pub fn generic_dispatch(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("generic-dispatch");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::dispatch_body_cached(ctx, generic, args) {
            Ok(body) => body,
            Err(GenericDispatchError::Arity) => {
                let who = ctx.intern("generic-dispatch");
                let message = ctx.str("not enough dispatch arguments");
                call.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = ctx.intern("generic-dispatch");
                let message = ctx.str("no applicable method");
                call.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-invoke")]
    pub fn generic_invoke(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("generic-invoke");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::invocation(ctx, generic, args) {
            Ok(invocation) => {
                let args = invocation.fused_args(ctx);
                call.tail(invocation.body(), args.as_slice())
            }
            Err(GenericDispatchError::Arity) => {
                let who = ctx.intern("generic-invoke");
                let message = ctx.str("not enough dispatch arguments");
                call.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = ctx.intern("generic-invoke");
                let message = ctx.str("no applicable method");
                call.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-next-methods")]
    pub fn generic_next_methods(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("generic-next-methods");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::next_method_chain(ctx, generic, args) {
            Ok(next_method) => next_method.into(),
            Err(GenericDispatchError::Arity) => {
                let who = ctx.intern("generic-next-methods");
                let message = ctx.str("not enough dispatch arguments");
                call.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = ctx.intern("generic-next-methods");
                let message = ctx.str("no applicable method");
                call.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "%make-next-method")]
    pub fn make_next_method(
        generic: Value<'gc>,
        methods: Value<'gc>,
        args: Value<'gc>,
    ) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("%make-next-method");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };

        let mut method_values = Vec::new();
        let mut cursor = methods;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = ctx.intern("%make-next-method");
                let message = ctx.str("methods must be a proper list");
                return call.raise_assertion_violation(who, message, cursor);
            }
            let value = cursor.car();
            let Some(method) = value.try_as::<MethodDescriptor>() else {
                let who = ctx.intern("%make-next-method");
                let message = ctx.str("method list contains a non-method");
                return call.raise_assertion_violation(who, message, value);
            };
            method_values.push(method);
            cursor = cursor.cdr();
        }

        let mut arg_values = Vec::new();
        let mut cursor = args;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = ctx.intern("%make-next-method");
                let message = ctx.str("args must be a proper list");
                return call.raise_assertion_violation(who, message, cursor);
            }
            arg_values.push(cursor.car());
            cursor = cursor.cdr();
        }

        if method_values.is_empty() {
            let who = ctx.intern("%make-next-method");
            let message = ctx.str("method list is empty");
            return call.raise_assertion_violation(who, message, methods);
        }

        let next_method = NextMethodDescriptor::new(
            ctx,
            generic,
            Array::from_slice(ctx, &method_values),
            Array::from_slice(ctx, &arg_values),
            0,
        );
        next_method.into()
    }

    #[scheme(name = "next-method-methods")]
    pub fn next_method_methods(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let methods = next_method
            .methods()
            .iter()
            .map(|method| Value::from(*method))
            .collect::<Vec<_>>();
        let methods = Value::list_from_slice(ctx, &methods);
        methods
    }

    #[scheme(name = "next-method?")]
    pub fn next_method_p(value: Value<'gc>) -> bool {
        value.is::<NextMethodDescriptor>()
    }

    #[scheme(name = "next-method-generic")]
    pub fn next_method_generic(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        next_method.generic().into()
    }

    #[scheme(name = "next-method-args")]
    pub fn next_method_args(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let args = Value::list_from_slice(ctx, next_method.args().as_slice());
        args
    }

    #[scheme(name = "next-method-index")]
    pub fn next_method_index(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        Value::new(next_method.index() as i32)
    }

    #[scheme(name = "next-method-body")]
    pub fn next_method_body(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        next_method.body()
    }

    #[scheme(name = "next-method-has-next?")]
    pub fn next_method_has_next_p(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> bool {
        next_method.has_next()
    }

    #[scheme(name = "next-method-invoke")]
    pub fn next_method_invoke(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        if next_method.has_next() {
            let method = next_method.methods()[next_method.index() + 1];
            let body = method.body();
            if body.is::<Closure>() {
                // A body that never reads its next argument gets `#f`: the
                // chain step allocates nothing then.
                let lead = if method.flags().no_next() {
                    Value::new(false)
                } else {
                    NextMethodDescriptor::new(
                        ctx,
                        next_method.generic(),
                        next_method.methods(),
                        next_method.args(),
                        next_method.index() + 1,
                    )
                    .into()
                };
                return call.enter_with(body, lead, next_method.args().as_slice());
            }
        }
        match NextMethodDescriptor::next_invocation(ctx, next_method) {
            Some((body, args)) => call.tail(body, args.as_slice()),
            None => {
                let who = ctx.intern("next-method-invoke");
                let message = ctx.str("no next method");
                call.raise_assertion_violation(who, message, next_method.into())
            }
        }
    }

    #[scheme(name = "next-method-next")]
    pub fn next_method_next(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        match next_method.next(ctx) {
            Some(next_method) => next_method.into(),
            None => Value::new(false),
        }
    }

    #[scheme(name = "scheme-instance?")]
    pub fn scheme_instance_p(value: Value<'gc>) -> bool {
        let is_instance = try_scheme_instance(ctx, value).is_some();
        is_instance
    }

    #[scheme(name = "make-instance")]
    pub fn make_instance(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &'gc [Value<'gc>],
    ) -> Value<'gc> {
        super::instantiate(ctx, call, class, initargs)
    }

    #[scheme(name = "slot-ref")]
    pub fn slot_ref(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-ref");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        if !protocol_is_plain(ctx, class, OP_REF)
            && let Some(slot_ref_using_class) =
                ctx.try_hook(crate::runtime::hooks::Hook::SlotRefUsingClass)
        {
            let slot = class_slot_argument(ctx, class, &slot_name, name.into());
            return if let Some(fallback) = fallback {
                call_protocol(
                    ctx,
                    call,
                    slot_ref_using_class,
                    &[class.into(), receiver, slot, fallback],
                )
            } else {
                call_protocol(
                    ctx,
                    call,
                    slot_ref_using_class,
                    &[class.into(), receiver, slot],
                )
            };
        }

        // A Scheme hook may collect: the values used below are rooted
        // across the call and reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let class_slot = frame.new_local_ref(class.into()).as_ptr();
        let receiver_slot = frame.new_local_ref(receiver).as_ptr();
        let name_slot = frame.new_local_ref(name.into()).as_ptr();

        let Some(result) = class.slot_value_by_name(ctx, class, receiver, &slot_name, fallback)
        else {
            let who = ctx.intern("slot-ref");
            let message = ctx.str("class has no slot reader");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slots live until `frame` drops, and hold a class, a
        // receiver and a symbol, updated by the collector.
        let reload = |slot: *mut Value<'static>| unsafe {
            std::mem::transmute::<Value<'static>, Value<'gc>>(*slot)
        };
        let class: Gc<'gc, ClassDescriptor<'gc>> =
            unsafe { Gc::from_gc_object(reload(class_slot).as_cell_raw()) };
        let receiver: Value<'gc> = reload(receiver_slot);
        let name: Gc<'gc, Symbol<'gc>> =
            unsafe { Gc::from_gc_object(reload(name_slot).as_cell_raw()) };

        match result {
            Ok(value) => value,
            Err(SlotAccessError::Unbound) => {
                if let Some(slot_unbound) = ctx.try_hook(crate::runtime::hooks::Hook::SlotUnbound) {
                    return call.tail(slot_unbound, &[class.into(), receiver, name.into()]);
                }
                let who = ctx.intern("slot-ref");
                let message = ctx.str("unbound slot");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-ref");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-ref");
                let message = ctx.str("slot reader failed");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                if let Some(slot_missing) = ctx.try_hook(crate::runtime::hooks::Hook::SlotMissing) {
                    return call.tail(slot_missing, &[class.into(), receiver, name.into()]);
                }
                let who = ctx.intern("slot-ref");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "%class-slot-ref")]
    pub fn class_slot_ref(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = ctx.intern("class-slot-ref");
            let message = ctx.str("class has no slot");
            return call.raise_assertion_violation(who, message, slot.into());
        };
        match class.read_class_slot(accessor) {
            Ok(value) => value,
            Err(SlotAccessError::Unbound) => {
                let who = ctx.intern("class-slot-ref");
                let message = ctx.str("unbound class slot");
                call.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = ctx.intern("class-slot-ref");
                let message = ctx.str("slot is not class allocated");
                call.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "%class-slot-set!")]
    pub fn class_slot_set(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = ctx.intern("class-slot-set!");
            let message = ctx.str("class has no slot");
            return call.raise_assertion_violation(who, message, slot.into());
        };
        match ClassDescriptor::set_class_slot(ctx, class, accessor, value) {
            Ok(()) => value,
            Err(SlotAccessError::Immutable) => {
                let who = ctx.intern("class-slot-set!");
                let message = ctx.str("immutable class slot");
                call.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = ctx.intern("class-slot-set!");
                let message = ctx.str("slot is not class allocated");
                call.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "%class-slot-bound?")]
    pub fn class_slot_bound_p(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = ctx.intern("class-slot-bound?");
            let message = ctx.str("class has no slot");
            return call.raise_assertion_violation(who, message, slot.into());
        };
        match class.class_slot_bound(accessor) {
            Ok(bound) => Value::new(bound),
            Err(_) => {
                let who = ctx.intern("class-slot-bound?");
                let message = ctx.str("slot is not class allocated");
                call.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "slot-set!")]
    pub fn slot_set(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-set!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance_obj);
        let slot_name = name.to_string();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);

        if !protocol_is_plain(ctx, class, OP_SET)
            && let Some(slot_set_using_class) =
                ctx.try_hook(crate::runtime::hooks::Hook::SlotSetUsingClass)
        {
            let slot = class_slot_argument(ctx, class, &slot_name, name.into());
            return call_protocol(
                ctx,
                call,
                slot_set_using_class,
                &[class.into(), receiver, slot, value],
            );
        }

        // A Scheme hook may collect: `name` is rooted across the call and
        // reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let name_slot = frame.new_local_ref(name.into()).as_ptr();

        let Some(result) = class.set_slot_value_by_name(ctx, class, receiver, &slot_name, value)
        else {
            let who = ctx.intern("slot-set!");
            let message = ctx.str("class has no slot writer");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slot lives until `frame` drops.
        let name: Gc<'gc, Symbol<'gc>> = unsafe {
            Gc::from_gc_object(
                std::mem::transmute::<Value<'static>, Value<'gc>>(*name_slot).as_cell_raw(),
            )
        };

        match result {
            Ok(()) => Value::undefined(),
            Err(SlotAccessError::Immutable) => {
                let who = ctx.intern("slot-set!");
                let message = ctx.str("immutable slot");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-set!");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                let who = ctx.intern("slot-set!");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-set!");
                let message = ctx.str("slot writer failed");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unbound) => unreachable!("slot writes cannot fail as unbound"),
        }
    }

    #[scheme(name = "instance-slot-ref")]
    pub fn instance_slot_ref(
        instance: Value<'gc>,
        index: i32,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        if index < 0 {
            let who = ctx.intern("instance-slot-ref");
            let message = ctx.str("slot index must be non-negative");
            return call.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("instance-slot-ref");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);

        match instance.read_slot(index as usize) {
            Ok(value) => value,
            Err(SlotAccessError::Unbound) => match fallback {
                Some(value) => value,
                None => {
                    let who = ctx.intern("instance-slot-ref");
                    let message = ctx.str("unbound slot");
                    call.raise_assertion_violation(who, message, Value::new(index))
                }
            },
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("instance-slot-ref");
                let message = ctx.str("slot index out of bounds");
                call.raise_assertion_violation(who, message, Value::new(index))
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("raw slot reads cannot fail as immutable")
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                unreachable!("raw slot reads do not dispatch Scheme hooks")
            }
            Err(SlotAccessError::Unknown) => unreachable!("raw slot reads do not resolve names"),
        }
    }

    #[scheme(name = "instance-slot-set!")]
    pub fn instance_slot_set(instance: Value<'gc>, index: i32, value: Value<'gc>) -> Value<'gc> {
        if index < 0 {
            let who = ctx.intern("instance-slot-set!");
            let message = ctx.str("slot index must be non-negative");
            return call.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("instance-slot-set!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        if index as usize >= instance.slot_count() {
            let who = ctx.intern("instance-slot-set!");
            let message = ctx.str("slot index out of bounds");
            return call.raise_assertion_violation(who, message, Value::new(index));
        }

        SchemeInstance::set_slot(ctx, instance, index as usize, value);
        Value::undefined()
    }

    #[scheme(name = "slot-initialize-using-accessor!")]
    pub fn slot_initialize_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        initargs: Value<'gc>,
    ) -> Value<'gc> {
        let Some(mut instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-initialize-using-accessor!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        let accessor_definition = accessor;
        let mut accessor = accessor_definition.as_accessor_descriptor();
        let mut accessor_name: Value<'gc> = Symbol::from_str(ctx, accessor.name()).into();
        if accessor.slot_index() >= instance.slot_count() {
            let who = ctx.intern("slot-initialize-using-accessor!");
            let message = ctx.str("stale slot accessor");
            return call.raise_assertion_violation(who, message, accessor_name);
        }

        let mut explicit_value = None;
        let mut cursor = initargs;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = ctx.intern("slot-initialize-using-accessor!");
                let message = ctx.str("initargs must be keyword/value pairs");
                return call.raise_assertion_violation(who, message, cursor);
            }
            let keyword_value = cursor.car();
            if !keyword_value.is::<Keyword>() {
                let who = ctx.intern("slot-initialize-using-accessor!");
                let message = ctx.str("initarg name is not a keyword");
                return call.raise_assertion_violation(who, message, keyword_value);
            }
            let rest = cursor.cdr();
            if !rest.is_pair() {
                let who = ctx.intern("slot-initialize-using-accessor!");
                let message = ctx.str("initargs must be keyword/value pairs");
                return call.raise_assertion_violation(who, message, cursor);
            }
            let keyword: Value<'gc> = keyword_value.downcast::<Keyword>().to_symbol().into();
            if keyword == accessor.init_keyword() {
                explicit_value = Some(rest.car());
            }
            cursor = rest.cdr();
        }

        let value = if let Some(value) = explicit_value {
            if !accessor.flags().initializable() {
                let who = ctx.intern("slot-initialize-using-accessor!");
                let message = ctx.str("slot is not initializable");
                return call.raise_assertion_violation(who, message, accessor.init_keyword());
            }
            value
        } else if !accessor.init_thunk().is_empty() {
            // The thunk may collect: everything used afterwards is rooted
            // across the call and reloaded.
            let mut frame = crate::runtime::sni::LocalFrame::push();
            let instance_slot = frame.new_local_ref(instance.into()).as_ptr();
            let definition_slot = frame.new_local_ref(accessor_definition.into()).as_ptr();
            let name_slot = frame.new_local_ref(accessor_name).as_ptr();
            let result = crate::runtime::sni::call_function(ctx, accessor.init_thunk(), []);
            // SAFETY: the slots live until `frame` drops.
            unsafe {
                let reload = |slot: *mut Value<'static>| {
                    std::mem::transmute::<Value<'static>, Value<'gc>>(*slot)
                };
                instance = try_scheme_instance(ctx, reload(instance_slot))
                    .expect("a Scheme instance stays one across a collection");
                accessor = reload(definition_slot)
                    .downcast::<SlotAccessorDefinition>()
                    .as_accessor_descriptor();
                accessor_name = reload(name_slot);
            }
            match result {
                crate::runtime::vm::ExecutionResult::Ok(value) => value,
                crate::runtime::vm::ExecutionResult::Err(error) => {
                    let who = ctx.intern("slot-initialize-using-accessor!");
                    let message = ctx.str("slot init thunk failed");
                    return call.raise_assertion_violation(who, message, error);
                }
            }
        } else {
            accessor.init_value()
        };

        if !value.is_empty() && accessor.class_allocated() {
            match ClassDescriptor::set_class_slot(ctx, instance.class(), accessor, value) {
                Ok(()) => {}
                Err(SlotAccessError::Immutable) => {
                    let who = ctx.intern("slot-initialize-using-accessor!");
                    let message = ctx.str("immutable class slot");
                    return call.raise_assertion_violation(who, message, accessor_name);
                }
                Err(_) => {
                    let who = ctx.intern("slot-initialize-using-accessor!");
                    let message = ctx.str("stale slot accessor");
                    return call.raise_assertion_violation(who, message, accessor_name);
                }
            }
        } else if !value.is_empty() {
            SchemeInstance::set_slot(ctx, instance, accessor.slot_index(), value);
        }
        Value::undefined()
    }

    #[scheme(name = "slot-ref-using-accessor")]
    pub fn slot_ref_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-ref-using-accessor");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(ctx, accessor.name()).into();

        // A Scheme hook may collect: `accessor_name` is rooted across the
        // call and reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let name_slot = frame.new_local_ref(accessor_name).as_ptr();

        let Some(result) = ({
            if !accessor.scheme_slot_ref().is_empty() {
                Some(call_scheme_slot_ref(
                    ctx,
                    receiver,
                    accessor.scheme_slot_ref(),
                ))
            } else {
                accessor
                    .slot_ref()
                    .or_else(|| class.primitive_operation_hooks().slot_ref())
                    .map(|slot_ref| slot_ref(ctx, class, receiver, accessor))
            }
        }) else {
            let who = ctx.intern("slot-ref-using-accessor");
            let message = ctx.str("class has no slot reader");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slot lives until `frame` drops.
        let accessor_name =
            unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*name_slot) };

        match result {
            Ok(value) => value,
            Err(SlotAccessError::Unbound) => {
                let who = ctx.intern("slot-ref-using-accessor");
                let message = ctx.str("unbound slot");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-ref-using-accessor");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-ref-using-accessor");
                let message = ctx.str("slot reader failed");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = ctx.intern("slot-ref-using-accessor");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, accessor_name)
            }
        }
    }

    #[scheme(name = "slot-set-using-accessor!")]
    pub fn slot_set_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-set-using-accessor!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance_obj);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);
        let accessor_name = Symbol::from_str(ctx, accessor.name()).into();

        // A Scheme hook may collect: `accessor_name` is rooted across the
        // call and reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let name_slot = frame.new_local_ref(accessor_name).as_ptr();

        let Some(result) = ({
            if !accessor.scheme_slot_set().is_empty() {
                Some(call_scheme_slot_set(
                    ctx,
                    receiver,
                    value,
                    accessor.scheme_slot_set(),
                ))
            } else {
                accessor
                    .slot_set()
                    .or_else(|| class.primitive_operation_hooks().slot_set())
                    .map(|slot_set| slot_set(ctx, class, receiver, accessor, value))
            }
        }) else {
            let who = ctx.intern("slot-set-using-accessor!");
            let message = ctx.str("class has no slot writer");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slot lives until `frame` drops.
        let accessor_name =
            unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*name_slot) };

        match result {
            Ok(()) => Value::undefined(),
            Err(SlotAccessError::Immutable) => {
                let who = ctx.intern("slot-set-using-accessor!");
                let message = ctx.str("immutable slot");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-set-using-accessor!");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = ctx.intern("slot-set-using-accessor!");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-set-using-accessor!");
                let message = ctx.str("slot writer failed");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unbound) => unreachable!("slot writes cannot fail as unbound"),
        }
    }

    #[scheme(name = "slot-bound?")]
    pub fn slot_bound_p(instance: Value<'gc>, name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-bound?");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        if !protocol_is_plain(ctx, class, OP_BOUND)
            && let Some(slot_bound_using_class) =
                ctx.try_hook(crate::runtime::hooks::Hook::SlotBoundUsingClass)
        {
            let slot = class_slot_argument(ctx, class, &slot_name, name.into());
            return call_protocol(
                ctx,
                call,
                slot_bound_using_class,
                &[class.into(), receiver, slot],
            );
        }

        // A Scheme hook may collect: `name` is rooted across the call and
        // reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let name_slot = frame.new_local_ref(name.into()).as_ptr();

        let Some(result) = class.slot_bound_by_name(ctx, class, receiver, &slot_name) else {
            let who = ctx.intern("slot-bound?");
            let message = ctx.str("class has no slot bound checker");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slot lives until `frame` drops.
        let name: Gc<'gc, Symbol<'gc>> = unsafe {
            Gc::from_gc_object(
                std::mem::transmute::<Value<'static>, Value<'gc>>(*name_slot).as_cell_raw(),
            )
        };

        match result {
            Ok(bound) => Value::new(bound),
            Err(SlotAccessError::Unknown) => {
                let who = ctx.intern("slot-bound?");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-bound?");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-bound?");
                let message = ctx.str("slot bound checker failed");
                call.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unbound) => {
                unreachable!("slot-bound? reports unbound slots as false")
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("slot-bound? does not mutate slots")
            }
        }
    }

    #[scheme(name = "slot-bound-using-accessor?")]
    pub fn slot_bound_using_accessor_p(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-bound-using-accessor?");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(ctx, accessor.name()).into();

        // A Scheme hook may collect: `accessor_name` is rooted across the
        // call and reloaded.
        let mut frame = crate::runtime::sni::LocalFrame::push();
        let name_slot = frame.new_local_ref(accessor_name).as_ptr();

        let Some(result) = ({
            if !accessor.scheme_slot_bound().is_empty() {
                Some(call_scheme_slot_bound(
                    ctx,
                    receiver,
                    accessor.scheme_slot_bound(),
                ))
            } else {
                accessor
                    .slot_bound()
                    .or_else(|| class.primitive_operation_hooks().slot_bound())
                    .map(|slot_bound| slot_bound(ctx, class, receiver, accessor))
            }
        }) else {
            let who = ctx.intern("slot-bound-using-accessor?");
            let message = ctx.str("class has no slot bound checker");
            return call.raise_assertion_violation(who, message, class.into());
        };

        // SAFETY: the slot lives until `frame` drops.
        let accessor_name =
            unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*name_slot) };

        match result {
            Ok(bound) => Value::new(bound),
            Err(SlotAccessError::Unknown) => {
                let who = ctx.intern("slot-bound-using-accessor?");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = ctx.intern("slot-bound-using-accessor?");
                let message = ctx.str("stale slot accessor");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = ctx.intern("slot-bound-using-accessor?");
                let message = ctx.str("slot bound checker failed");
                call.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unbound) => {
                unreachable!("slot-bound-using-accessor? reports unbound slots as false")
            }
            Err(SlotAccessError::Immutable) => {
                unreachable!("slot-bound-using-accessor? does not mutate slots")
            }
        }
    }
}

pub(crate) fn init_class_ops<'gc>(ctx: MutationContext<'gc>) {
    class_ops::register(ctx);
}
