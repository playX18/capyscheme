use capy_derive::scheme;
use crate::runtime::Context;

#[scheme(path=capy)]
pub mod class_ops {
    use crate::prelude::*;
    use crate::rsgc::alloc::Array;
    
    use crate::runtime::class::{
        ClassCategory, ClassDescriptor, ClassListError, GenericDescriptor, GenericDispatchError,
        MethodDescriptor, MethodFlags, NextMethodDescriptor, SchemeClassSpecError, SchemeInstance,
        SlotAccessError, SlotAccessorDefinition, SlotDefinitionDescriptor, SlotInitError,
        builtin_class_specs, call_scheme_slot_ref, call_scheme_slot_set, call_scheme_slot_bound,
        class_id_list_to_class_objects, class_id_to_class_object,
        class_root_binding, class_table, generic_descriptor_from_value,
        generic_procedure, parse_class_list, slot_accessor_list, slot_definition_list,
        try_scheme_instance,
    };
    use crate::runtime::value::{Keyword, Symbol};

    #[scheme(name = "class?")]
    pub fn class_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<ClassDescriptor>())
    }

    #[scheme(name = "%builtin-class")]
    pub fn builtin_class(name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let name_string = name.to_string();
        let Some(spec) = builtin_class_specs()
            .iter()
            .copied()
            .find(|spec| spec.name() == name_string)
        else {
            let who = nctx.ctx.intern("%builtin-class");
            let message = nctx.ctx.str("unknown built-in class");
            return nctx.raise_assertion_violation(who, message, name.into());
        };

        let Some(class) = class_table(nctx.ctx).lookup(spec.id()) else {
            let who = nctx.ctx.intern("%builtin-class");
            let message = nctx.ctx.str("built-in class is not initialized");
            return nctx.raise_assertion_violation(who, message, name.into());
        };
        nctx.return_(class.into())
    }

    #[scheme(name = "class-name")]
    pub fn class_name(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let name = nctx.ctx.str(class.name());
        nctx.return_(name)
    }

    #[scheme(name = "class-direct-supers")]
    pub fn class_direct_supers(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let supers = class_id_list_to_class_objects(nctx.ctx, class.direct_supers());
        nctx.return_(supers)
    }

    #[scheme(name = "class-precedence-list")]
    pub fn class_precedence_list(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let cpl = class_id_list_to_class_objects(nctx.ctx, class.cpl());
        nctx.return_(cpl)
    }

    #[scheme(name = "class-direct-slots")]
    pub fn class_direct_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(nctx.ctx, class.direct_slots());
        nctx.return_(slots)
    }

    #[scheme(name = "class-slots")]
    pub fn class_slots(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let slots = slot_definition_list(nctx.ctx, class.slots());
        nctx.return_(slots)
    }

    #[scheme(name = "class-accessors")]
    pub fn class_accessors(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let accessors = slot_accessor_list(nctx.ctx, class.accessors());
        nctx.return_(accessors)
    }

    #[scheme(name = "class-initargs")]
    pub fn class_initargs(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let initargs = Value::list_from_slice(nctx.ctx, class.initargs());
        nctx.return_(initargs)
    }

    #[scheme(name = "class-direct-methods")]
    pub fn class_direct_methods(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let methods = Value::list_from_slice(nctx.ctx, class.direct_methods().as_slice());
        nctx.return_(methods)
    }

    #[scheme(name = "class-direct-subclasses")]
    pub fn class_direct_subclasses(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let subclasses = class
            .direct_subclasses()
            .iter()
            .filter_map(|weak| weak.upgrade(*nctx.ctx).map(Value::from))
            .collect::<Vec<_>>();
        let subclasses = Value::list_from_slice(nctx.ctx, &subclasses);
        nctx.return_(subclasses)
    }

    #[scheme(name = "class-applicable?")]
    pub fn class_applicable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().applicable())
    }

    #[scheme(name = "class-malleable?")]
    pub fn class_malleable_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().malleable())
    }

    #[scheme(name = "class-sealed?")]
    pub fn class_sealed_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        nctx.return_(class.flags().sealed())
    }

    #[scheme(name = "class-seal!")]
    pub fn class_seal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        class.seal();
        nctx.return_(class.into())
    }

    #[scheme(name = "class-unseal!")]
    pub fn class_unseal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        if class.unseal() {
            nctx.return_(class.into())
        } else {
            let who = nctx.ctx.intern("class-unseal!");
            let message = nctx.ctx.str("class cannot be unsealed");
            nctx.raise_assertion_violation(who, message, class.into())
        }
    }

    #[scheme(name = "generic?")]
    pub fn generic_p(value: Value<'gc>) -> bool {
        let result = generic_descriptor_from_value(nctx.ctx, value).is_some();
        nctx.return_(result)
    }

    #[scheme(name = "generic-name")]
    pub fn generic_name(value: Value<'gc>) -> Value<'gc> {
        let name = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| nctx.ctx.str(generic.name()))
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(name)
    }

    #[scheme(name = "generic-methods")]
    pub fn generic_methods(value: Value<'gc>) -> Value<'gc> {
        let methods = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| {
                let methods = generic
                    .methods()
                    .iter()
                    .copied()
                    .map(Value::from)
                    .collect::<Vec<_>>();
                Value::list_from_slice(nctx.ctx, &methods)
            })
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(methods)
    }

    #[scheme(name = "generic-fallback")]
    pub fn generic_fallback(value: Value<'gc>) -> Value<'gc> {
        let fallback = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.fallback())
            .unwrap_or_else(|| Value::new(false));
        nctx.return_(fallback)
    }

    #[scheme(name = "generic-required-dispatch-arg-count")]
    pub fn generic_required_dispatch_arg_count(value: Value<'gc>) -> Value<'gc> {
        let count = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.max_required_dispatch_args())
            .unwrap_or(0);
        nctx.return_(Value::new(count as i32))
    }

    #[scheme(name = "generic-sealed?")]
    pub fn generic_sealed_p(value: Value<'gc>) -> bool {
        let sealed = generic_descriptor_from_value(nctx.ctx, value)
            .map(|generic| generic.flags().sealed())
            .unwrap_or(false);
        nctx.return_(sealed)
    }

    #[scheme(name = "generic-seal!")]
    pub fn generic_seal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(nctx.ctx, value) else {
            let who = nctx.ctx.intern("generic-seal!");
            let message = nctx.ctx.str("not a generic procedure");
            return nctx.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(nctx.ctx, generic, true);
        nctx.return_(Value::void())
    }

    #[scheme(name = "generic-unseal!")]
    pub fn generic_unseal(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(nctx.ctx, value) else {
            let who = nctx.ctx.intern("generic-unseal!");
            let message = nctx.ctx.str("not a generic procedure");
            return nctx.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(nctx.ctx, generic, false);
        nctx.return_(Value::void())
    }

    #[scheme(name = "method?")]
    pub fn method_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<MethodDescriptor>())
    }

    #[scheme(name = "method-generic")]
    pub fn method_generic(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(method.generic().into())
    }

    #[scheme(name = "method-specializers")]
    pub fn method_specializers(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        let specializers = class_id_list_to_class_objects(nctx.ctx, method.specializers());
        nctx.return_(specializers)
    }

    #[scheme(name = "method-required-arg-count")]
    pub fn method_required_arg_count(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(method.required_arg_count() as i32))
    }

    #[scheme(name = "method-body")]
    pub fn method_body(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(method.body())
    }

    #[scheme(name = "method-locked?")]
    pub fn method_locked_p(method: Gc<'gc, MethodDescriptor<'gc>>) -> bool {
        nctx.return_(method.flags().locked())
    }

    #[scheme(name = "slot-definition?")]
    pub fn slot_definition_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<SlotDefinitionDescriptor>())
    }

    #[scheme(name = "slot-definition-name")]
    pub fn slot_definition_name(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(nctx.ctx, slot.name()).into();
        nctx.return_(name)
    }

    #[scheme(name = "slot-definition-owner")]
    pub fn slot_definition_owner(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(nctx.ctx, slot.owner());
        nctx.return_(owner)
    }

    #[scheme(name = "slot-definition-index")]
    pub fn slot_definition_index(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(slot.index() as i32))
    }

    #[scheme(name = "%slot-definition-allocation")]
    pub fn slot_definition_allocation(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        let allocation = if slot.class_allocated() {
            nctx.ctx.keyword("class")
        } else {
            nctx.ctx.keyword("instance")
        };
        nctx.return_(allocation.into())
    }

    #[scheme(name = "slot-definition-init-keyword")]
    pub fn slot_definition_init_keyword(
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        nctx.return_(slot.init_keyword())
    }

    #[scheme(name = "slot-definition-init-value")]
    pub fn slot_definition_init_value(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.init_value())
    }

    #[scheme(name = "slot-definition-init-thunk")]
    pub fn slot_definition_init_thunk(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.init_thunk())
    }

    #[scheme(name = "slot-definition-slot-ref")]
    pub fn slot_definition_slot_ref(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_ref())
    }

    #[scheme(name = "slot-definition-slot-set!")]
    pub fn slot_definition_slot_set(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_set())
    }

    #[scheme(name = "slot-definition-slot-bound?")]
    pub fn slot_definition_slot_bound(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.scheme_slot_bound())
    }

    #[scheme(name = "slot-definition-getter")]
    pub fn slot_definition_getter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.getter())
    }

    #[scheme(name = "slot-definition-setter")]
    pub fn slot_definition_setter(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.setter())
    }

    #[scheme(name = "slot-definition-accessor")]
    pub fn slot_definition_accessor(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(slot.accessor())
    }

    #[scheme(name = "slot-definition-initializable?")]
    pub fn slot_definition_initializable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().initializable())
    }

    #[scheme(name = "slot-definition-settable?")]
    pub fn slot_definition_settable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().settable())
    }

    #[scheme(name = "slot-definition-immutable?")]
    pub fn slot_definition_immutable_p(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        nctx.return_(slot.flags().immutable_flag())
    }

    #[scheme(name = "slot-accessor?")]
    pub fn slot_accessor_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<SlotAccessorDefinition>())
    }

    #[scheme(name = "slot-accessor-name")]
    pub fn slot_accessor_name(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let name = Symbol::from_str(nctx.ctx, accessor.name()).into();
        nctx.return_(name)
    }

    #[scheme(name = "slot-accessor-owner")]
    pub fn slot_accessor_owner(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        let owner = class_id_to_class_object(nctx.ctx, accessor.owner());
        nctx.return_(owner)
    }

    #[scheme(name = "slot-accessor-index")]
    pub fn slot_accessor_index(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(accessor.slot_index() as i32))
    }

    #[scheme(name = "slot-accessor-init-keyword")]
    pub fn slot_accessor_init_keyword(
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        nctx.return_(accessor.init_keyword())
    }

    #[scheme(name = "slot-accessor-init-value")]
    pub fn slot_accessor_init_value(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.init_value())
    }

    #[scheme(name = "slot-accessor-init-thunk")]
    pub fn slot_accessor_init_thunk(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.init_thunk())
    }

    #[scheme(name = "slot-accessor-slot-ref")]
    pub fn slot_accessor_slot_ref(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_ref())
    }

    #[scheme(name = "slot-accessor-slot-set!")]
    pub fn slot_accessor_slot_set(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_set())
    }

    #[scheme(name = "slot-accessor-slot-bound?")]
    pub fn slot_accessor_slot_bound(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.scheme_slot_bound())
    }

    #[scheme(name = "slot-accessor-getter")]
    pub fn slot_accessor_getter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.getter())
    }

    #[scheme(name = "slot-accessor-setter")]
    pub fn slot_accessor_setter(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.setter())
    }

    #[scheme(name = "slot-accessor-accessor")]
    pub fn slot_accessor_accessor(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> Value<'gc> {
        nctx.return_(accessor.accessor())
    }

    #[scheme(name = "slot-accessor-initializable?")]
    pub fn slot_accessor_initializable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().initializable())
    }

    #[scheme(name = "slot-accessor-settable?")]
    pub fn slot_accessor_settable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().settable())
    }

    #[scheme(name = "slot-accessor-immutable?")]
    pub fn slot_accessor_immutable_p(accessor: Gc<'gc, SlotAccessorDefinition<'gc>>) -> bool {
        nctx.return_(accessor.flags().immutable_flag())
    }

    #[scheme(name = "class-of")]
    pub fn class_of(value: Value<'gc>) -> Value<'gc> {
        match value.class(nctx.ctx) {
            Some(class) => nctx.return_(class.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "current-class-of")]
    pub fn current_class_of(value: Value<'gc>) -> Value<'gc> {
        if let Some(instance) = try_scheme_instance(nctx.ctx, value) {
            return nctx.return_(instance.class().into());
        }
        match value.class(nctx.ctx) {
            Some(class) => nctx.return_(class.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "%class-redefined?")]
    pub fn class_redefined_p(value: Value<'gc>) -> bool {
        let table = class_table(nctx.ctx);
        let class = if let Some(instance) = try_scheme_instance(nctx.ctx, value) {
            instance.class()
        } else if value.is::<ClassDescriptor>() {
            value.downcast::<ClassDescriptor>()
        } else {
            return nctx.return_(false);
        };
        let Some(current_class) = table.lookup(class.id()) else {
            return nctx.return_(false);
        };
        nctx.return_(current_class.as_gcobj() != class.as_gcobj())
    }

    #[scheme(name = "touch-instance!")]
    pub fn touch_instance(value: Value<'gc>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, value) else {
            let who = nctx.ctx.intern("touch-instance!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, value);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        nctx.return_(value)
    }

    #[scheme(name = "change-class")]
    pub fn change_class(value: Value<'gc>, new_class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, value) else {
            let who = nctx.ctx.intern("change-class");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, value);
        };
        if new_class.category() != ClassCategory::Scheme {
            let who = nctx.ctx.intern("change-class");
            let message = nctx.ctx.str("new class is not a Scheme class");
            return nctx.raise_assertion_violation(who, message, new_class.into());
        }
        SchemeInstance::touch(nctx.ctx, instance);
        SchemeInstance::change_class(nctx.ctx, instance, new_class);
        nctx.return_(value)
    }

    #[scheme(name = "make-class")]
    pub fn make_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).register_scheme_class_from_slot_list(
            nctx.ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme classes do not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("make-class");
                let message = nctx.ctx.str("could not register class");
                nctx.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "make-invocable-class")]
    pub fn make_invocable_class(
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match class_table(nctx.ctx).register_invocable_scheme_class_from_slot_list(
            nctx.ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("invocable class must define a procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("make-invocable-class");
                let message = nctx.ctx.str("could not register class");
                nctx.raise_assertion_violation(who, message, name.into())
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
        match class_table(nctx.ctx).redefine_scheme_class_from_slot_list(
            nctx.ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(_)) => {
                unreachable!("ordinary Scheme class redefinition does not require an invoker slot")
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("redefine-class!");
                let message = nctx.ctx.str("could not redefine class");
                nctx.raise_assertion_violation(who, message, class.into())
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
        match class_table(nctx.ctx).redefine_invocable_scheme_class_from_slot_list(
            nctx.ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
        ) {
            Ok(class) => nctx.return_(class.into()),
            Err(SchemeClassSpecError::SlotsNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot names must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotNameNotSymbol(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot name is not a symbol");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotSpecNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot spec options must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionNameNotKeyword(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot option name is not a keyword");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SlotOptionMissingValue(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("slot option is missing a value");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::UnknownSlotOption(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("unknown slot option");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SupersNotList(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("direct supers must be a proper list");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::SuperNotClass(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("direct super is not a class");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::MissingProcedureSlot(value)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("invocable class must define a procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = nctx.ctx.intern("redefine-invocable-class!");
                let message = nctx.ctx.str("could not redefine class");
                nctx.raise_assertion_violation(who, message, class.into())
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
            let who = nctx.ctx.intern("make-generic");
            let message = nctx
                .ctx
                .str("required dispatch argument count must be non-negative");
            return nctx.raise_assertion_violation(
                who,
                message,
                Value::new(max_required_dispatch_args),
            );
        }

        let name_value = name.into();
        let name_string = name.to_string();
        let generic =
            GenericDescriptor::new(nctx.ctx, &name_string, max_required_dispatch_args as usize);
        let proc = generic_procedure(nctx.ctx, name_value, generic);
        nctx.return_(proc.into())
    }

    #[scheme(name = "add-method!")]
    pub fn add_method(
        generic: Value<'gc>,
        specializers: Value<'gc>,
        required_arg_count: i32,
        body: Value<'gc>,
        locked: Option<bool>,
    ) -> Value<'gc> {
        if required_arg_count < 0 {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("required argument count must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        let specializers = match parse_class_list(specializers) {
            Ok(specializers) => specializers,
            Err(ClassListError::NotList(value)) => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("specializers must be a proper list");
                return nctx.raise_assertion_violation(who, message, value);
            }
            Err(ClassListError::NotClass(value)) => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("specializer is not a class");
                return nctx.raise_assertion_violation(who, message, value);
            }
        };
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("add-method!");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };

        if specializers.len() > required_arg_count as usize {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx
                .ctx
                .str("method cannot specialize more arguments than it requires");
            return nctx.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        if generic.flags().sealed() {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("generic is sealed");
            return nctx.raise_assertion_violation(who, message, generic.into());
        }

        if let Some(method) = generic.find_method(&specializers, required_arg_count as usize)
            && method.flags().locked()
        {
            let who = nctx.ctx.intern("add-method!");
            let message = nctx.ctx.str("method is locked");
            return nctx.raise_assertion_violation(who, message, method.into());
        }

        let flags = if locked.unwrap_or(false) {
            MethodFlags::locked_flag()
        } else {
            MethodFlags::empty()
        };
        let method = GenericDescriptor::add_method_with_flags(
            nctx.ctx,
            generic,
            &specializers,
            required_arg_count as usize,
            body,
            flags,
        );
        nctx.return_(method.into())
    }

    #[scheme(name = "set-generic-fallback!")]
    pub fn set_generic_fallback(generic: Value<'gc>, fallback: Value<'gc>) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("set-generic-fallback!");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        GenericDescriptor::set_fallback(nctx.ctx, generic, fallback);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "generic-dispatch")]
    pub fn generic_dispatch(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::dispatch_body_cached(nctx.ctx, generic, args) {
            Ok(body) => nctx.return_(body),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-dispatch");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-invoke")]
    pub fn generic_invoke(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::invocation(nctx.ctx, generic, args) {
            Ok(invocation) => nctx.return_call(invocation.body(), invocation.args().as_slice()),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-invoke");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "generic-next-methods")]
    pub fn generic_next_methods(generic: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };
        match GenericDescriptor::next_method_chain(nctx.ctx, generic, args) {
            Ok(next_method) => nctx.return_(next_method.into()),
            Err(GenericDispatchError::Arity) => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("not enough dispatch arguments");
                nctx.raise_assertion_violation(who, message, Value::new(args.len() as i32))
            }
            Err(GenericDispatchError::NoApplicableMethod) => {
                let who = nctx.ctx.intern("generic-next-methods");
                let message = nctx.ctx.str("no applicable method");
                nctx.raise_assertion_violation(who, message, generic.into())
            }
        }
    }

    #[scheme(name = "%make-next-method")]
    pub fn make_next_method(
        generic: Value<'gc>,
        methods: Value<'gc>,
        args: Value<'gc>,
    ) -> Value<'gc> {
        let generic = match generic_descriptor_from_value(nctx.ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("not a generic procedure");
                return nctx.raise_assertion_violation(who, message, generic);
            }
        };

        let mut method_values = Vec::new();
        let mut cursor = methods;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("methods must be a proper list");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let value = cursor.car();
            let Some(method) = value.try_as::<MethodDescriptor>() else {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("method list contains a non-method");
                return nctx.raise_assertion_violation(who, message, value);
            };
            method_values.push(method);
            cursor = cursor.cdr();
        }

        let mut arg_values = Vec::new();
        let mut cursor = args;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("%make-next-method");
                let message = nctx.ctx.str("args must be a proper list");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            arg_values.push(cursor.car());
            cursor = cursor.cdr();
        }

        if method_values.is_empty() {
            let who = nctx.ctx.intern("%make-next-method");
            let message = nctx.ctx.str("method list is empty");
            return nctx.raise_assertion_violation(who, message, methods);
        }

        let next_method = NextMethodDescriptor::new(
            nctx.ctx,
            generic,
            Array::from_slice(*nctx.ctx, &method_values),
            Array::from_slice(*nctx.ctx, &arg_values),
            0,
        );
        nctx.return_(next_method.into())
    }

    #[scheme(name = "next-method-methods")]
    pub fn next_method_methods(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let methods = next_method
            .methods()
            .iter()
            .map(|method| Value::from(*method))
            .collect::<Vec<_>>();
        let methods = Value::list_from_slice(nctx.ctx, &methods);
        nctx.return_(methods)
    }

    #[scheme(name = "next-method?")]
    pub fn next_method_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<NextMethodDescriptor>())
    }

    #[scheme(name = "next-method-generic")]
    pub fn next_method_generic(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(next_method.generic().into())
    }

    #[scheme(name = "next-method-args")]
    pub fn next_method_args(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        let args = Value::list_from_slice(nctx.ctx, next_method.args().as_slice());
        nctx.return_(args)
    }

    #[scheme(name = "next-method-index")]
    pub fn next_method_index(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(Value::new(next_method.index() as i32))
    }

    #[scheme(name = "next-method-body")]
    pub fn next_method_body(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        nctx.return_(next_method.body())
    }

    #[scheme(name = "next-method-has-next?")]
    pub fn next_method_has_next_p(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> bool {
        nctx.return_(next_method.has_next())
    }

    #[scheme(name = "next-method-invoke")]
    pub fn next_method_invoke(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        match NextMethodDescriptor::next_invocation(nctx.ctx, next_method) {
            Some((body, args)) => nctx.return_call(body, args.as_slice()),
            None => {
                let who = nctx.ctx.intern("next-method-invoke");
                let message = nctx.ctx.str("no next method");
                nctx.raise_assertion_violation(who, message, next_method.into())
            }
        }
    }

    #[scheme(name = "next-method-next")]
    pub fn next_method_next(next_method: Gc<'gc, NextMethodDescriptor<'gc>>) -> Value<'gc> {
        match next_method.next(nctx.ctx) {
            Some(next_method) => nctx.return_(next_method.into()),
            None => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "scheme-instance?")]
    pub fn scheme_instance_p(value: Value<'gc>) -> bool {
        let is_instance = try_scheme_instance(nctx.ctx, value).is_some();
        nctx.return_(is_instance)
    }

    #[scheme(name = "make-instance")]
    pub fn make_instance(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &'gc [Value<'gc>],
    ) -> Value<'gc> {
        if class.category() != ClassCategory::Scheme {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("not a Scheme class");
            return nctx.raise_assertion_violation(who, message, class.into());
        }
        if !initargs.len().is_multiple_of(2) {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("initargs must be keyword/value pairs");
            return nctx.raise_assertion_violation(who, message, Value::new(initargs.len() as i32));
        }

        let mut parsed_initargs = Vec::new();
        for pair in initargs.chunks_exact(2) {
            let keyword_value = pair[0];
            if !keyword_value.is::<Keyword>() {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("initarg name is not a keyword");
                return nctx.raise_assertion_violation(who, message, keyword_value);
            }
            let keyword = keyword_value.downcast::<Keyword>().to_symbol().into();
            parsed_initargs.push((keyword, pair[1]));
        }

        let Some(allocation) = class.allocate_instance(nctx.ctx, class, &parsed_initargs) else {
            let who = nctx.ctx.intern("make-instance");
            let message = nctx.ctx.str("class is not allocatable");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match allocation {
            Ok(instance) => nctx.return_(instance),
            Err(SlotInitError::UnknownKeyword(keyword)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("unknown init keyword");
                nctx.raise_assertion_violation(who, message, keyword)
            }
            Err(SlotInitError::NotInitializable(keyword)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("slot is not initializable");
                nctx.raise_assertion_violation(who, message, keyword)
            }
            Err(SlotInitError::InitThunkFailed(error)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("slot init thunk failed");
                nctx.raise_assertion_violation(who, message, error)
            }
            Err(SlotInitError::MissingInvokerSlot(value)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("invocable class has no procedure slot");
                nctx.raise_assertion_violation(who, message, value)
            }
            Err(SlotInitError::InvokerNotProcedure(value)) => {
                let who = nctx.ctx.intern("make-instance");
                let message = nctx.ctx.str("invocable procedure slot is not a procedure");
                nctx.raise_assertion_violation(who, message, value)
            }
        }
    }

    #[scheme(name = "slot-ref")]
    pub fn slot_ref(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-ref");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        let Some(result) =
            class.slot_value_by_name(nctx.ctx, class, receiver, &slot_name, fallback)
        else {
            let who = nctx.ctx.intern("slot-ref");
            let message = nctx.ctx.str("class has no slot reader");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                if let Some(slot_unbound) = class_root_binding(nctx.ctx, "slot-unbound") {
                    return nctx.return_call(slot_unbound, &[class.into(), receiver, name.into()]);
                }
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("unbound slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("slot reader failed");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                if let Some(slot_missing) = class_root_binding(nctx.ctx, "slot-missing") {
                    return nctx.return_call(slot_missing, &[class.into(), receiver, name.into()]);
                }
                let who = nctx.ctx.intern("slot-ref");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "%class-slot-ref")]
    pub fn class_slot_ref(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = nctx.ctx.intern("class-slot-ref");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match class.read_class_slot(accessor) {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                let who = nctx.ctx.intern("class-slot-ref");
                let message = nctx.ctx.str("unbound class slot");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-ref");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
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
            let who = nctx.ctx.intern("class-slot-set!");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match ClassDescriptor::set_class_slot(nctx.ctx, class, accessor, value) {
            Ok(()) => nctx.return_(value),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("class-slot-set!");
                let message = nctx.ctx.str("immutable class slot");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-set!");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "%class-slot-bound?")]
    pub fn class_slot_bound_p(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>,
    ) -> Value<'gc> {
        let Some(accessor) = class.accessor_named(slot.name()) else {
            let who = nctx.ctx.intern("class-slot-bound?");
            let message = nctx.ctx.str("class has no slot");
            return nctx.raise_assertion_violation(who, message, slot.into());
        };
        match class.class_slot_bound(accessor) {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(_) => {
                let who = nctx.ctx.intern("class-slot-bound?");
                let message = nctx.ctx.str("slot is not class allocated");
                nctx.raise_assertion_violation(who, message, slot.into())
            }
        }
    }

    #[scheme(name = "slot-set!")]
    pub fn slot_set(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-set!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance_obj);
        let slot_name = name.to_string();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);

        let Some(result) =
            class.set_slot_value_by_name(nctx.ctx, class, receiver, &slot_name, value)
        else {
            let who = nctx.ctx.intern("slot-set!");
            let message = nctx.ctx.str("class has no slot writer");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(()) => nctx.return_(Value::undefined()),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("immutable slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-set!");
                let message = nctx.ctx.str("slot writer failed");
                nctx.raise_assertion_violation(who, message, name.into())
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
            let who = nctx.ctx.intern("instance-slot-ref");
            let message = nctx.ctx.str("slot index must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("instance-slot-ref");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);

        match instance.read_slot(index as usize) {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => match fallback {
                Some(value) => nctx.return_(value),
                None => {
                    let who = nctx.ctx.intern("instance-slot-ref");
                    let message = nctx.ctx.str("unbound slot");
                    nctx.raise_assertion_violation(who, message, Value::new(index))
                }
            },
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("instance-slot-ref");
                let message = nctx.ctx.str("slot index out of bounds");
                nctx.raise_assertion_violation(who, message, Value::new(index))
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
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("slot index must be non-negative");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        if index as usize >= instance.slot_count() {
            let who = nctx.ctx.intern("instance-slot-set!");
            let message = nctx.ctx.str("slot index out of bounds");
            return nctx.raise_assertion_violation(who, message, Value::new(index));
        }

        SchemeInstance::set_slot(nctx.ctx, instance, index as usize, value);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "slot-initialize-using-accessor!")]
    pub fn slot_initialize_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        initargs: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-initialize-using-accessor!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();
        if accessor.slot_index() >= instance.slot_count() {
            let who = nctx.ctx.intern("slot-initialize-using-accessor!");
            let message = nctx.ctx.str("stale slot accessor");
            return nctx.raise_assertion_violation(who, message, accessor_name);
        }

        let mut explicit_value = None;
        let mut cursor = initargs;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initargs must be keyword/value pairs");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let keyword_value = cursor.car();
            if !keyword_value.is::<Keyword>() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initarg name is not a keyword");
                return nctx.raise_assertion_violation(who, message, keyword_value);
            }
            let rest = cursor.cdr();
            if !rest.is_pair() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("initargs must be keyword/value pairs");
                return nctx.raise_assertion_violation(who, message, cursor);
            }
            let keyword: Value<'gc> = keyword_value.downcast::<Keyword>().to_symbol().into();
            if keyword == accessor.init_keyword() {
                explicit_value = Some(rest.car());
            }
            cursor = rest.cdr();
        }

        let value = if let Some(value) = explicit_value {
            if !accessor.flags().initializable() {
                let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                let message = nctx.ctx.str("slot is not initializable");
                return nctx.raise_assertion_violation(who, message, accessor.init_keyword());
            }
            value
        } else if !accessor.init_thunk().is_empty() {
            match crate::runtime::vm::call_scheme(nctx.ctx, accessor.init_thunk(), []) {
                crate::runtime::vm::VMResult::Ok(value) => value,
                crate::runtime::vm::VMResult::Err(error) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("slot init thunk failed");
                    return nctx.raise_assertion_violation(who, message, error);
                }
            }
        } else {
            accessor.init_value()
        };

        if !value.is_empty() && accessor.class_allocated() {
            match ClassDescriptor::set_class_slot(nctx.ctx, instance.class(), accessor, value) {
                Ok(()) => {}
                Err(SlotAccessError::Immutable) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("immutable class slot");
                    return nctx.raise_assertion_violation(who, message, accessor_name);
                }
                Err(_) => {
                    let who = nctx.ctx.intern("slot-initialize-using-accessor!");
                    let message = nctx.ctx.str("stale slot accessor");
                    return nctx.raise_assertion_violation(who, message, accessor_name);
                }
            }
        } else if !value.is_empty() {
            SchemeInstance::set_slot(nctx.ctx, instance, accessor.slot_index(), value);
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "slot-ref-using-accessor")]
    pub fn slot_ref_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-ref-using-accessor");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_ref().is_empty() {
                Some(call_scheme_slot_ref(
                    nctx.ctx,
                    receiver,
                    accessor.scheme_slot_ref(),
                ))
            } else {
                accessor
                    .slot_ref()
                    .or_else(|| class.primitive_operation_hooks().slot_ref())
                    .map(|slot_ref| slot_ref(nctx.ctx, class, receiver, accessor))
            }
        }) else {
            let who = nctx.ctx.intern("slot-ref-using-accessor");
            let message = nctx.ctx.str("class has no slot reader");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(value) => nctx.return_(value),
            Err(SlotAccessError::Unbound) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("unbound slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Immutable) => unreachable!("slot reads cannot fail as immutable"),
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("slot reader failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-ref-using-accessor");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
        }
    }

    #[scheme(name = "slot-set-using-accessor!")]
    pub fn slot_set_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-set-using-accessor!");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance_obj);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_set().is_empty() {
                Some(call_scheme_slot_set(
                    nctx.ctx,
                    receiver,
                    value,
                    accessor.scheme_slot_set(),
                ))
            } else {
                accessor
                    .slot_set()
                    .or_else(|| class.primitive_operation_hooks().slot_set())
                    .map(|slot_set| slot_set(nctx.ctx, class, receiver, accessor, value))
            }
        }) else {
            let who = nctx.ctx.intern("slot-set-using-accessor!");
            let message = nctx.ctx.str("class has no slot writer");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(()) => nctx.return_(Value::undefined()),
            Err(SlotAccessError::Immutable) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("immutable slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-set-using-accessor!");
                let message = nctx.ctx.str("slot writer failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::Unbound) => unreachable!("slot writes cannot fail as unbound"),
        }
    }

    #[scheme(name = "slot-bound?")]
    pub fn slot_bound_p(instance: Value<'gc>, name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-bound?");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let slot_name = name.to_string();
        let class = instance.class();
        let receiver = Value::from(instance);

        let Some(result) = class.slot_bound_by_name(nctx.ctx, class, receiver, &slot_name) else {
            let who = nctx.ctx.intern("slot-bound?");
            let message = nctx.ctx.str("class has no slot bound checker");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, name.into())
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-bound?");
                let message = nctx.ctx.str("slot bound checker failed");
                nctx.raise_assertion_violation(who, message, name.into())
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
        let Some(instance) = try_scheme_instance(nctx.ctx, instance) else {
            let who = nctx.ctx.intern("slot-bound-using-accessor?");
            let message = nctx.ctx.str("not a Scheme instance");
            return nctx.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(nctx.ctx, instance);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance.class();
        let receiver = Value::from(instance);
        let accessor_name = Symbol::from_str(nctx.ctx, accessor.name()).into();

        let Some(result) = ({
            if !accessor.scheme_slot_bound().is_empty() {
                Some(call_scheme_slot_bound(
                    nctx.ctx,
                    receiver,
                    accessor.scheme_slot_bound(),
                ))
            } else {
                accessor
                    .slot_bound()
                    .or_else(|| class.primitive_operation_hooks().slot_bound())
                    .map(|slot_bound| slot_bound(nctx.ctx, class, receiver, accessor))
            }
        }) else {
            let who = nctx.ctx.intern("slot-bound-using-accessor?");
            let message = nctx.ctx.str("class has no slot bound checker");
            return nctx.raise_assertion_violation(who, message, class.into());
        };

        match result {
            Ok(bound) => nctx.return_(Value::new(bound)),
            Err(SlotAccessError::Unknown) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("unknown slot");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::OutOfBounds) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("stale slot accessor");
                nctx.raise_assertion_violation(who, message, accessor_name)
            }
            Err(SlotAccessError::SchemeHookFailed) => {
                let who = nctx.ctx.intern("slot-bound-using-accessor?");
                let message = nctx.ctx.str("slot bound checker failed");
                nctx.raise_assertion_violation(who, message, accessor_name)
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

pub(crate) fn init_class_ops<'gc>(ctx: Context<'gc>) {
    class_ops::register(ctx);
}
