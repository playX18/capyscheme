use super::ic::protocol_is_plain;
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::Value;
use crate::runtime::objectmodel::class::flags::LayoutFamily;
use crate::runtime::objectmodel::class::{
    ClassDescriptor, SlotAccessError, SlotAccessorDescriptor,
};
use crate::runtime::objectmodel::class::{
    GenericDescriptor, OP_BOUND, OP_REF, OP_SET, OP_UNBIND,
    generic::{Reason, invocable_instance_of, resolved_class_id},
    generic_descriptor_from_value,
};
use crate::runtime::objectmodel::{Keyword, Symbol};
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

/// Slot protocol call.
fn call_protocol<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::NativeCall<'_, 'gc>,
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

/// Current descriptor.
fn current_descriptor<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
) -> Gc<'gc, ClassDescriptor<'gc>> {
    crate::runtime::objectmodel::class_table(ctx)
        .lookup(class.id())
        .unwrap_or(class)
}

pub(crate) fn instantiate<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::NativeCall<'_, 'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    initargs: &[Value<'gc>],
) -> Value<'gc> {
    use crate::runtime::objectmodel::Keyword;
    use crate::runtime::objectmodel::class::{ClassCategory, SlotInitError};
    // Resolve forward reference.
    let class = if class.unfinalized() {
        let current = current_descriptor(ctx, class);
        if current.unfinalized() {
            let who = ctx.intern("make-instance");
            let message = ctx.str("class is not finalized: a superclass is a forward reference");
            return call.raise_assertion_violation(who, message, class.into());
        }
        current
    } else {
        class
    };
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

    // Default initargs for the keys not supplied. Their thunks may collect,
    // so the class is the one they return.
    let completed;
    let (class, initargs) = if class.default_initargs().is_empty() {
        (class, initargs)
    } else {
        match super::instance::initargs_with_default_initargs(ctx, class, initargs) {
            Ok((reloaded, full)) => {
                completed = full;
                (reloaded, completed.as_slice())
            }
            Err(error) => {
                let who = ctx.intern("make-instance");
                let message = ctx.str("default initarg thunk failed");
                return call.raise_assertion_violation(who, message, error);
            }
        }
    };

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

/// The metaclass id named by an optional `metaclass` argument. `Err` carries
/// the offending value when it is not a class.
fn requested_metaclass<'gc>(
    metaclass: Option<Value<'gc>>,
) -> Result<Option<crate::heap::object::ClassId>, Value<'gc>> {
    match metaclass {
        None => Ok(None),
        // The expansion of `define-class` passes `#f` for the default.
        Some(value) if value == Value::new(false) => Ok(None),
        Some(value) if value.is::<ClassDescriptor>() => {
            Ok(Some(value.downcast::<ClassDescriptor>().id()))
        }
        Some(value) => Err(value),
    }
}

/// The default initargs named by a list of `(keyword thunk)` pairs, `None`
/// for no list. `Err` carries the first element that is not such a pair.
fn parse_default_initargs<'gc>(
    list: Option<Value<'gc>>,
) -> Result<Vec<(Value<'gc>, Value<'gc>)>, Value<'gc>> {
    let Some(list) = list else {
        return Ok(Vec::new());
    };
    let mut defaults = Vec::new();
    let mut cursor = list;
    while cursor.is_pair() {
        let entry = cursor.car();
        let well_formed = entry.is_pair()
            && entry.cdr().is_pair()
            && entry.cdr().cdr().is_null()
            && entry.car().is::<crate::runtime::objectmodel::Keyword>()
            && entry
                .cdr()
                .car()
                .is::<crate::runtime::objectmodel::proc::Closure>();
        if !well_formed {
            return Err(entry);
        }
        defaults.push((entry.car(), entry.cdr().car()));
        cursor = cursor.cdr();
    }
    if cursor.is_null() {
        Ok(defaults)
    } else {
        Err(list)
    }
}

/// The list of `(keyword thunk)` pairs of a flat default initarg array.
fn default_initarg_list<'gc>(ctx: MutationContext<'gc>, flat: &[Value<'gc>]) -> Value<'gc> {
    flat.chunks_exact(2)
        .rev()
        .fold(Value::null(), |rest, pair| {
            let thunk = Value::cons(ctx, pair[1], Value::null());
            Value::cons(ctx, Value::cons(ctx, pair[0], thunk), rest)
        })
}

/// `slot-ref` on a class object: its slots are the extension slots of its
/// metaclass.
fn class_extension_slot_ref<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::NativeCall<'_, 'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    name: Gc<'gc, Symbol<'gc>>,
    fallback: Option<Value<'gc>>,
) -> Value<'gc> {
    match ClassDescriptor::extension_slot_ref(ctx, class, &name.to_string()) {
        Ok(value) => value,
        Err(SlotAccessError::Unknown) => match fallback {
            Some(value) => value,
            None => {
                let who = ctx.intern("slot-ref");
                let message = ctx.str("unknown slot");
                call.raise_assertion_violation(who, message, name.into())
            }
        },
        Err(SlotAccessError::Unbound) => {
            let who = ctx.intern("slot-ref");
            let message = ctx.str("unbound slot");
            call.raise_assertion_violation(who, message, name.into())
        }
        Err(_) => {
            let who = ctx.intern("slot-ref");
            let message = ctx.str("stale slot accessor");
            call.raise_assertion_violation(who, message, name.into())
        }
    }
}

/// Store the unbound marker in `accessor`'s slot (`slot-unbind!`). `None`
/// when the class has no writer for the slot. A slot with a Scheme writer is
/// refused as immutable: its writer would receive the marker.
fn unbind_slot_value<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    receiver: Value<'gc>,
    accessor: SlotAccessorDescriptor<'gc>,
) -> Option<Result<(), SlotAccessError>> {
    if !accessor.scheme_slot_set().is_empty() {
        return Some(Err(SlotAccessError::Immutable));
    }
    accessor
        .slot_set()
        .or_else(|| class.primitive_operation_hooks().slot_set())
        .map(|slot_set| slot_set(ctx, class, receiver, accessor, Value::empty()))
}

/// Raise the assertion violation for a failed unbind or raw slot access.
fn raise_slot_error<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::NativeCall<'_, 'gc>,
    who: &str,
    error: SlotAccessError,
    irritant: Value<'gc>,
) -> Value<'gc> {
    let message = match error {
        SlotAccessError::Immutable => "slot cannot be unbound",
        SlotAccessError::OutOfBounds => "stale slot accessor",
        SlotAccessError::Unknown => "unknown slot",
        SlotAccessError::SchemeHookFailed => "slot writer failed",
        SlotAccessError::Unbound => "unbound slot",
    };
    let who = ctx.intern(who);
    let message = ctx.str(message);
    call.raise_assertion_violation(who, message, irritant)
}

#[scheme(path=capy)]
pub mod class_ops {
    use crate::heap::alloc::Array;
    use crate::prelude::*;

    use super::{class_extension_slot_ref, requested_metaclass};
    use crate::heap::object::ClassId;
    use crate::runtime::objectmodel::class::{
        ClassCategory, ClassDescriptor, ClassListError, ClassTableError, EqlSpecializer,
        GenericDescriptor, GenericDispatchError, MethodCombination, MethodDescriptor, MethodFlags,
        NextMethodDescriptor, SchemeClassSpecError, SchemeInstance, SlotAccessError,
        SlotAccessorDefinition, SlotDefinitionDescriptor, builtin_class_specs,
        call_scheme_slot_bound, call_scheme_slot_ref, call_scheme_slot_set,
        class_id_list_to_class_objects, class_id_to_class_object, class_table,
        generic_descriptor_from_value, generic_procedure, parse_class_list, parse_specializer_list,
        slot_accessor_list, slot_definition_list, try_scheme_instance,
    };
    use crate::runtime::objectmodel::{Keyword, Symbol};
    use crate::runtime::watchpoint::grace;

    use super::super::combination::standard_combination;

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

    #[scheme(name = "class-direct-default-initargs")]
    pub fn class_direct_default_initargs(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        default_initarg_list(ctx, class.direct_default_initargs())
    }

    #[scheme(name = "class-default-initargs")]
    pub fn class_default_initargs(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        default_initarg_list(ctx, class.default_initargs())
    }

    #[scheme(name = "class-prototype")]
    pub fn class_prototype(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        // A prototype is a `SchemeInstance`, so the class must lay out its
        // instances that way: a Scheme class that is not a metaobject.
        if class.category() != ClassCategory::Scheme || class.layout_family() != LayoutFamily::None
        {
            let who = ctx.intern("class-prototype");
            let message = ctx.str("class does not have Scheme instances");
            return call.raise_assertion_violation(who, message, class.into());
        }
        ClassDescriptor::prototype(ctx, class)
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

    #[scheme(name = "class-finalized?")]
    pub fn class_finalized_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        !current_descriptor(ctx, class).unfinalized()
    }

    #[scheme(name = "%make-forward-referenced-class")]
    pub fn make_forward_referenced_class(name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        match class_table(ctx).register_forward_reference(ctx, &name.to_string()) {
            Ok(class) => class.into(),
            Err(_) => {
                let who = ctx.intern("%make-forward-referenced-class");
                let message = ctx.str("could not register forward reference");
                call.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "%resolve-forward-reference!")]
    pub fn resolve_forward_reference(
        placeholder: Gc<'gc, ClassDescriptor<'gc>>,
        target: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Value<'gc> {
        match class_table(ctx).resolve_forward_reference(ctx, placeholder.id(), target.id()) {
            Ok(()) => target.into(),
            Err(error) => {
                let who = ctx.intern("%resolve-forward-reference!");
                let (message, irritant) = match error {
                    ClassTableError::NotForwardReference(_) => {
                        ("class is not a forward reference", placeholder.into())
                    }
                    ClassTableError::Cycle(_) => (
                        "forward reference would make a class its own superclass",
                        target.into(),
                    ),
                    ClassTableError::InconsistentPrecedence(remaining) => (
                        "inconsistent class precedence",
                        class_id_list_to_class_objects(ctx, &remaining),
                    ),
                    _ => ("could not resolve forward reference", placeholder.into()),
                };
                call.raise_assertion_violation(who, ctx.str(message), irritant)
            }
        }
    }

    #[scheme(name = "class-sealed?")]
    pub fn class_sealed_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        class.flags().sealed()
    }

    /// Seal class.
    #[scheme(name = "class-seal!")]
    pub fn class_seal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        class.seal(false);
        grace(ctx, class.into())
    }

    /// Seal allowing subclasses.
    #[scheme(name = "%class-seal-allowing-subclasses!")]
    pub fn class_seal_allowing_subclasses(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        class.seal(true);
        grace(ctx, class.into())
    }

    #[scheme(name = "class-final?")]
    pub fn class_final_p(class: Gc<'gc, ClassDescriptor<'gc>>) -> bool {
        class.flags().no_subclasses()
    }

    #[scheme(name = "class-unseal!")]
    pub fn class_unseal(class: Gc<'gc, ClassDescriptor<'gc>>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        if ClassDescriptor::unseal(class) {
            grace(ctx, class.into())
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
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-seal!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(ctx, generic, true);
        grace(ctx, Value::void())
    }

    /// Empty every cache of a generic and dispatch afresh (CMUCL
    /// `flush-emf-cache`), see `GenericDescriptor::flush_cache`.
    #[scheme(name = "generic-flush-cache!")]
    pub fn generic_flush_cache(value: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-flush-cache!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::flush_cache(ctx, generic);
        grace(ctx, Value::undefined())
    }

    /// Precompute the dispatch entry for the classes `classes` (a list with
    /// one class per dispatch argument). `#t` when the entry is now cached.
    #[scheme(name = "%generic-precompute-key!")]
    pub fn generic_precompute_key(value: Value<'gc>, classes: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-precompute!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        let ids: Vec<ClassId> = match parse_class_list(classes) {
            Ok(ids) => ids,
            Err(_) => {
                let who = ctx.intern("generic-precompute!");
                let message = ctx.str("classes must be a list of classes");
                return call.raise_assertion_violation(who, message, classes);
            }
        };
        let cached = GenericDescriptor::precompute_key(ctx, generic, &ids);
        grace(ctx, Value::new(cached))
    }

    #[scheme(name = "generic-unseal!")]
    pub fn generic_unseal(value: Value<'gc>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let Some(generic) = generic_descriptor_from_value(ctx, value) else {
            let who = ctx.intern("generic-unseal!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, value);
        };
        GenericDescriptor::set_sealed(ctx, generic, false);
        grace(ctx, Value::void())
    }

    #[scheme(name = "method?")]
    pub fn method_p(value: Value<'gc>) -> bool {
        value.is::<MethodDescriptor>()
    }

    #[scheme(name = "method-generic")]
    pub fn method_generic(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        method.generic().into()
    }

    /// The specializers as written: a class for a class position, the
    /// interned `eql-specializer` for an eql one.
    #[scheme(name = "method-specializers")]
    pub fn method_specializers(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        let objects = method
            .specializers()
            .iter()
            .enumerate()
            .map(|(position, id)| match method.eql_at(position) {
                Some(object) => Value::from(EqlSpecializer::intern(ctx, object)),
                None => class_id_to_class_object(ctx, *id),
            })
            .collect::<Vec<_>>();
        Value::list_from_slice(ctx, &objects)
    }

    /// The specializer `(eql object)`, interned: the same object for `eqv?`
    /// arguments.
    #[scheme(name = "intern-eql-specializer")]
    pub fn intern_eql_specializer(object: Value<'gc>) -> Value<'gc> {
        Value::from(EqlSpecializer::intern(ctx, object))
    }

    #[scheme(name = "eql-specializer?")]
    pub fn eql_specializer_p(value: Value<'gc>) -> bool {
        value.is::<EqlSpecializer>()
    }

    #[scheme(name = "eql-specializer-object")]
    pub fn eql_specializer_object(specializer: Gc<'gc, EqlSpecializer<'gc>>) -> Value<'gc> {
        specializer.object()
    }

    /// The standard method combination, the one every generic starts with.
    #[scheme(name = "%standard-method-combination")]
    pub fn standard_method_combination() -> Value<'gc> {
        Value::from(standard_combination(ctx))
    }

    /// A combination named `name` whose effective methods `builder` makes:
    /// `(builder gf methods options) -> emf`.
    #[scheme(name = "%make-method-combination")]
    pub fn make_method_combination(name: Gc<'gc, Symbol<'gc>>, builder: Value<'gc>) -> Value<'gc> {
        Value::from(MethodCombination::new(
            ctx,
            name,
            Value::null(),
            builder,
            false,
        ))
    }

    #[scheme(name = "method-combination?")]
    pub fn method_combination_p(value: Value<'gc>) -> bool {
        value.is::<MethodCombination>()
    }

    #[scheme(name = "method-combination-name")]
    pub fn method_combination_name(combination: Gc<'gc, MethodCombination<'gc>>) -> Value<'gc> {
        Value::from(combination.name())
    }

    #[scheme(name = "method-combination-options")]
    pub fn method_combination_options(combination: Gc<'gc, MethodCombination<'gc>>) -> Value<'gc> {
        combination.options()
    }

    /// The builder procedure of a combination, `#f` for the standard one.
    #[scheme(name = "%method-combination-builder")]
    pub fn method_combination_builder(combination: Gc<'gc, MethodCombination<'gc>>) -> Value<'gc> {
        combination.builder()
    }

    /// The methods applicable to the arguments `args` (a list), every role,
    /// most specific first with `eql` matches ranked above classes. `()`
    /// when none applies.
    #[scheme(name = "generic-applicable-methods")]
    pub fn generic_applicable_methods(generic: Value<'gc>, args: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            return Value::new(false);
        };
        let args = crate::runtime::objectmodel::class::generic::list_items(args);
        match generic.all_applicable_methods(ctx, &args) {
            Ok(methods) => {
                let values: Vec<Value<'gc>> = methods.iter().copied().map(Value::from).collect();
                Value::list_from_slice(ctx, &values)
            }
            Err(_) => Value::list_from_slice(ctx, &[]),
        }
    }

    /// `(%generic-cache-emf! gf epoch emf args)`: publish `emf`, the effective
    /// method Scheme built for the applicable methods of the argument list
    /// `args`, computed under `epoch` (refused once a change has happened).
    #[scheme(name = "%generic-cache-emf!")]
    pub fn generic_cache_emf(
        generic: Value<'gc>,
        epoch: i32,
        emf: Value<'gc>,
        args: Value<'gc>,
    ) -> Value<'gc> {
        if let Some(generic) = generic_descriptor_from_value(ctx, generic) {
            let args = crate::runtime::objectmodel::class::generic::list_items(args);
            GenericDescriptor::cache_emf(ctx, generic, epoch as u32, emf, &args);
        }
        Value::undefined()
    }

    #[scheme(name = "generic-method-combination")]
    pub fn generic_method_combination(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("generic-method-combination");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        Value::from(generic.combination())
    }

    /// Give `generic` the combination `combination` with `options` (a Scheme
    /// list). Roles are derived again and the generic is invalidated.
    #[scheme(name = "%set-generic-combination!")]
    pub fn set_generic_combination(
        generic: Value<'gc>,
        combination: Gc<'gc, MethodCombination<'gc>>,
        options: Value<'gc>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let Some(generic_descriptor) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("set-generic-method-combination!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        if generic_descriptor.flags().sealed() {
            let who = ctx.intern("set-generic-method-combination!");
            let message = ctx.str("generic is sealed");
            return call.raise_assertion_violation(who, message, generic_descriptor.into());
        }
        let named = MethodCombination::new(
            ctx,
            combination.name(),
            options,
            combination.builder(),
            combination.is_standard(),
        );
        GenericDescriptor::set_combination(ctx, generic_descriptor, named);
        grace(ctx, Value::undefined())
    }

    /// Store the argument precedence order of `generic`: a list of argument
    /// positions (small exact integers), or `()` to clear it.
    #[scheme(name = "%set-generic-precedence!")]
    pub fn set_generic_precedence(generic: Value<'gc>, order: Value<'gc>) -> Value<'gc> {
        let Some(generic_descriptor) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%set-generic-precedence!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        if generic_descriptor.flags().sealed() {
            let who = ctx.intern("%set-generic-precedence!");
            let message = ctx.str("generic is sealed");
            return call.raise_assertion_violation(who, message, generic_descriptor.into());
        }
        let mut positions = Vec::new();
        let mut rest = order;
        while rest.is_pair() {
            let item = rest.car();
            if !item.is_int32() || !(0..256).contains(&item.as_int32()) {
                let who = ctx.intern("%set-generic-precedence!");
                let message = ctx.str("argument precedence order must list argument positions");
                return call.raise_assertion_violation(who, message, item);
            }
            positions.push(item.as_int32() as u8);
            rest = rest.cdr();
        }
        let stored = if positions.is_empty() {
            None
        } else {
            Some(Array::from_slice(ctx, &positions))
        };
        GenericDescriptor::set_precedence(ctx, generic_descriptor, stored);
        grace(ctx, Value::undefined())
    }

    /// The argument precedence order of a generic, `()` when none was set.
    #[scheme(name = "generic-argument-precedence-order")]
    pub fn generic_argument_precedence_order(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic_descriptor) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("generic-argument-precedence-order");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        let positions: Vec<Value<'gc>> = generic_descriptor
            .precedence()
            .map(|order| order.iter().map(|p| Value::new(*p as i32)).collect())
            .unwrap_or_default();
        Value::list_from_slice(ctx, &positions)
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
        crate::runtime::objectmodel::class::generic::make_method_body(
            ctx,
            initialize,
            default_body,
            slow,
        )
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
        GenericDescriptor::mark_protocol_role(
            ctx,
            generic,
            crate::runtime::objectmodel::class::generic::ProtocolRole::SlotProtocol,
        );
        for method in generic.methods().iter().copied() {
            MethodDescriptor::set_flags(ctx, method, method.flags().with_boot_slot_protocol());
        }
        Value::undefined()
    }

    /// Mark every method `generic` has now as a stock one: the boot library's own, as opposed to a
    /// user override (called by `base.scm` right after defining a protocol generic).
    #[scheme(name = "%mark-stock-methods!")]
    pub fn mark_stock_methods(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%mark-stock-methods!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        for method in generic.methods().iter().copied() {
            MethodDescriptor::set_flags(ctx, method, method.flags().with_stock());
        }
        Value::undefined()
    }

    /// Whether every method of `generic` that can apply to an argument whose class is the class of
    /// `argument` is a stock one. Reads the methods as they are now, so a method added later is
    /// seen by the next call.
    #[scheme(name = "%stock-methods-only?")]
    pub fn stock_methods_only(generic: Value<'gc>, argument: Value<'gc>) -> bool {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            return false;
        };
        let Some(class) =
            resolved_class_id(ctx, argument).and_then(|id| class_table(ctx).lookup(id))
        else {
            return false;
        };
        generic
            .methods()
            .iter()
            .copied()
            .filter(|method| method.may_apply_to_classes(&[class]))
            .all(|method| method.flags().stock())
    }

    /// Install `cpl` (a list of classes) as the precedence list of `class`. It must be a
    /// reordering of the computed list. Returns the redefined class.
    #[scheme(name = "%install-class-precedence!")]
    pub fn install_class_precedence(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        cpl: Value<'gc>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let ids = match parse_class_list(cpl) {
            Ok(ids) => ids,
            Err(_) => {
                let who = ctx.intern("finalize-inheritance");
                let message = ctx.str("the class precedence list must be a list of classes");
                return call.raise_assertion_violation(who, message, cpl);
            }
        };
        match class_table(ctx).install_precedence_list(ctx, class.id(), ids) {
            Ok(redefined) => grace(ctx, redefined.into()),
            Err(ClassTableError::BadPrecedenceOverride(_)) => {
                let who = ctx.intern("finalize-inheritance");
                let message = ctx.str(
                    "the class precedence list must hold the classes of the computed one, once each, starting with the class",
                );
                call.raise_assertion_violation(who, message, cpl)
            }
            Err(ClassTableError::InconsistentPrecedence(remaining)) => {
                let who = ctx.intern("finalize-inheritance");
                let message = ctx.str("inconsistent class precedence");
                let irritant = class_id_list_to_class_objects(ctx, &remaining);
                call.raise_assertion_violation(who, message, irritant)
            }
            Err(_) => {
                let who = ctx.intern("finalize-inheritance");
                let message = ctx.str("could not install the class precedence list");
                call.raise_assertion_violation(who, message, class.into())
            }
        }
    }

    /// Mark `generic` as a boot instance lifecycle generic (called by `base.scm` once its stock
    /// methods exist): a method added to it afterwards turns off the native `make` fast path.
    #[scheme(name = "%mark-lifecycle-generic!")]
    pub fn mark_lifecycle_generic(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%mark-lifecycle-generic!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        GenericDescriptor::mark_protocol_role(
            ctx,
            generic,
            crate::runtime::objectmodel::class::generic::ProtocolRole::Lifecycle,
        );
        Value::undefined()
    }

    /// Mark `generic` as the boot `initialize` generic (called by `base.scm` right after defining
    /// it), so that slot caches treat it, and no generic that merely has the name, as the protocol.
    #[scheme(name = "%mark-boot-initialize!")]
    pub fn mark_boot_initialize(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%mark-boot-initialize!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        GenericDescriptor::mark_protocol_role(
            ctx,
            generic,
            crate::runtime::objectmodel::class::generic::ProtocolRole::Initialize,
        );
        Value::undefined()
    }

    /// Count (or stop counting) calls that reach the Rust generic entry, and
    /// read the count: shows whether the machine stub served a call (a test
    /// aid).
    #[scheme(name = "%generic-stats!")]
    pub fn generic_stats(on: bool) -> Value<'gc> {
        crate::runtime::objectmodel::class::generic::DISPATCH_STATS
            .store(on, std::sync::atomic::Ordering::Relaxed);
        crate::runtime::objectmodel::class::generic::RUST_ENTRIES
            .store(0, std::sync::atomic::Ordering::Relaxed);
        Value::undefined()
    }

    #[scheme(name = "%generic-rust-entries")]
    pub fn generic_rust_entries() -> Value<'gc> {
        Value::new(
            crate::runtime::objectmodel::class::generic::RUST_ENTRIES
                .load(std::sync::atomic::Ordering::Relaxed) as i32,
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

    /// The shape of a generic's slot cache, as `(kind one-index?)` with kind
    /// `empty`, `linear` or `table`.
    #[scheme(name = "%generic-slot-cache-shape")]
    pub fn generic_slot_cache_shape(generic: Value<'gc>) -> Value<'gc> {
        use crate::runtime::objectmodel::class::generic::SlotCacheShape;
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%generic-slot-cache-shape");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        let (shape, one_index) = generic.slot_cache_state();
        let kind = match shape {
            SlotCacheShape::Empty => "empty",
            SlotCacheShape::Linear => "linear",
            SlotCacheShape::Table => "table",
        };
        let kind: Value<'gc> = Symbol::from_str(ctx, kind).into();
        Value::cons(
            ctx,
            kind,
            Value::cons(ctx, Value::new(one_index), Value::null()),
        )
    }

    /// Change watchpoint dependents of a generic (a test aid): how many Tier 2
    /// units depend on its method set.
    #[scheme(name = "%generic-change-watch-count")]
    pub fn generic_change_watch_count(generic: Value<'gc>) -> Value<'gc> {
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("%generic-change-watch-count");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        Value::new(generic.change_wp().dependent_count() as i32)
    }

    /// The AMOP dependents of a generic or class, as a list.
    #[scheme(name = "%dependents")]
    pub fn dependents(metaobject: Value<'gc>) -> Value<'gc> {
        if let Some(generic) = generic_descriptor_from_value(ctx, metaobject) {
            return generic.dependents();
        }
        if let Some(class) = metaobject.try_as::<ClassDescriptor>() {
            return class.dependents();
        }
        let who = ctx.intern("%dependents");
        let message = ctx.str("not a generic function or class");
        call.raise_assertion_violation(who, message, metaobject)
    }

    /// Replace the AMOP dependents of a generic or class with a list.
    #[scheme(name = "%set-dependents!")]
    pub fn set_dependents(metaobject: Value<'gc>, dependents: Value<'gc>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        if let Some(generic) = generic_descriptor_from_value(ctx, metaobject) {
            GenericDescriptor::set_dependents(ctx, generic, dependents);
        } else if let Some(class) = metaobject.try_as::<ClassDescriptor>() {
            ClassDescriptor::set_dependents(ctx, class, dependents);
        } else {
            let who = ctx.intern("%set-dependents!");
            let message = ctx.str("not a generic function or class");
            return call.raise_assertion_violation(who, message, metaobject);
        }
        grace(ctx, Value::undefined())
    }

    /// Add `dependent` to the AMOP dependents of a generic or class, under the metaobject lock so
    /// that two threads adding at once keep both. `#f` when it was already there.
    #[scheme(name = "%add-dependent!")]
    pub fn add_dependent(metaobject: Value<'gc>, dependent: Value<'gc>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let generic = generic_descriptor_from_value(ctx, metaobject);
        let class = metaobject.try_as::<ClassDescriptor>();
        let current = match (generic, class) {
            (Some(generic), _) => generic.dependents(),
            (None, Some(class)) => class.dependents(),
            (None, None) => {
                let who = ctx.intern("add-dependent");
                let message = ctx.str("not a generic function or class");
                return call.raise_assertion_violation(who, message, metaobject);
            }
        };
        let mut cursor = current;
        while cursor.is_pair() {
            if cursor.car().bits() == dependent.bits() {
                return Value::new(false);
            }
            cursor = cursor.cdr();
        }
        let extended = Value::cons(ctx, dependent, current);
        match (generic, class) {
            (Some(generic), _) => GenericDescriptor::set_dependents(ctx, generic, extended),
            (None, Some(class)) => ClassDescriptor::set_dependents(ctx, class, extended),
            (None, None) => unreachable!("checked above"),
        }
        grace(ctx, Value::undefined())
    }

    /// Remove `dependent` from the AMOP dependents of a generic or class, under the metaobject lock.
    #[scheme(name = "%remove-dependent!")]
    pub fn remove_dependent(metaobject: Value<'gc>, dependent: Value<'gc>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let generic = generic_descriptor_from_value(ctx, metaobject);
        let class = metaobject.try_as::<ClassDescriptor>();
        let current = match (generic, class) {
            (Some(generic), _) => generic.dependents(),
            (None, Some(class)) => class.dependents(),
            (None, None) => {
                let who = ctx.intern("remove-dependent");
                let message = ctx.str("not a generic function or class");
                return call.raise_assertion_violation(who, message, metaobject);
            }
        };
        let mut kept = Vec::new();
        let mut cursor = current;
        while cursor.is_pair() {
            if cursor.car().bits() != dependent.bits() {
                kept.push(cursor.car());
            }
            cursor = cursor.cdr();
        }
        let list = Value::list_from_slice(ctx, &kept);
        match (generic, class) {
            (Some(generic), _) => GenericDescriptor::set_dependents(ctx, generic, list),
            (None, Some(class)) => ClassDescriptor::set_dependents(ctx, class, list),
            (None, None) => unreachable!("checked above"),
        }
        grace(ctx, Value::undefined())
    }

    #[scheme(name = "method-body")]
    pub fn method_body(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        method.body()
    }

    #[scheme(name = "method-locked?")]
    pub fn method_locked_p(method: Gc<'gc, MethodDescriptor<'gc>>) -> bool {
        method.flags().locked()
    }

    /// The qualifier keywords the method was added with (`()` for a primary).
    #[scheme(name = "method-qualifiers")]
    pub fn method_qualifiers(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        method.qualifiers()
    }

    /// The method's role in the standard combination: `primary`, `before`,
    /// `after`, `around` or `other`.
    #[scheme(name = "%method-role")]
    pub fn method_role(method: Gc<'gc, MethodDescriptor<'gc>>) -> Value<'gc> {
        ctx.intern(method.role().name()).into()
    }

    /// Whether the method's body never reads its next-method argument, so a
    /// caller may pass `#f` instead of building the chain.
    #[scheme(name = "%method-no-next?")]
    pub fn method_no_next(method: Gc<'gc, MethodDescriptor<'gc>>) -> bool {
        method.flags().no_next()
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

    /// Whether the slot definition has an init value (`#:init-value`), as opposed to none.
    #[scheme(name = "%slot-definition-has-init-value?")]
    pub fn slot_definition_has_init_value(slot: Gc<'gc, SlotDefinitionDescriptor<'gc>>) -> bool {
        !slot.init_value().is_empty()
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
        match resolved_class_id(ctx, value).and_then(|id| class_table(ctx).lookup(id)) {
            Some(class) => class.into(),
            None => Value::new(false),
        }
    }

    #[scheme(name = "current-class-of")]
    pub fn current_class_of(value: Value<'gc>) -> Value<'gc> {
        if let Some(instance) = try_scheme_instance(ctx, value) {
            return instance.class().into();
        }
        match resolved_class_id(ctx, value).and_then(|id| class_table(ctx).lookup(id)) {
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

    /// Move `value` to `new_class`, keeping the slots they share. `change-class`, the generic of
    /// `base.scm`, wraps it with `update-instance-for-different-class`.
    #[scheme(name = "%change-class")]
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
        // A sealed class is not malleable: no instance may move into it or out.
        let current = value.class_id().and_then(|id| class_table(ctx).lookup(id));
        if !new_class.flags().malleable() || current.is_some_and(|c| !c.flags().malleable()) {
            let who = ctx.intern("change-class");
            let message = ctx.str("change-class cannot involve a sealed class");
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
        metaclass: Option<Value<'gc>>,
        default_initargs: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let default_initargs = match parse_default_initargs(default_initargs) {
            Ok(defaults) => defaults,
            Err(value) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("default initargs must be (keyword thunk) pairs");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        let metaclass_id = match requested_metaclass(metaclass) {
            Ok(id) => id,
            Err(value) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("metaclass is not a class");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        match class_table(ctx).register_scheme_class_from_slot_list(
            ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
            metaclass_id,
            &default_initargs,
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
            Err(SchemeClassSpecError::ClassTable(ClassTableError::NotMetaclass(_))) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("metaclass is not a class metaobject class");
                call.raise_assertion_violation(who, message, metaclass.unwrap_or(name.into()))
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::InconsistentPrecedence(
                remaining,
            ))) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("inconsistent class precedence");
                let irritant = class_id_list_to_class_objects(ctx, &remaining);
                call.raise_assertion_violation(who, message, irritant)
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::SealedSuperclass(id))) => {
                let who = ctx.intern("make-class");
                let message = ctx.str("superclass is sealed and takes no subclasses");
                call.raise_assertion_violation(who, message, class_id_to_class_object(ctx, id))
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
        default_initargs: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let default_initargs = match parse_default_initargs(default_initargs) {
            Ok(defaults) => defaults,
            Err(value) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("default initargs must be (keyword thunk) pairs");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        match class_table(ctx).register_invocable_scheme_class_from_slot_list(
            ctx,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
            &default_initargs,
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
            Err(SchemeClassSpecError::ClassTable(ClassTableError::InconsistentPrecedence(
                remaining,
            ))) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("inconsistent class precedence");
                let irritant = class_id_list_to_class_objects(ctx, &remaining);
                call.raise_assertion_violation(who, message, irritant)
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::SealedSuperclass(id))) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("superclass is sealed and takes no subclasses");
                call.raise_assertion_violation(who, message, class_id_to_class_object(ctx, id))
            }
            Err(SchemeClassSpecError::ClassTable(_)) => {
                let who = ctx.intern("make-invocable-class");
                let message = ctx.str("could not register class");
                call.raise_assertion_violation(who, message, name.into())
            }
        }
    }

    #[scheme(name = "%redefine-class!")]
    pub fn redefine_class(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Option<Value<'gc>>,
        metaclass: Option<Value<'gc>>,
        default_initargs: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let default_initargs = match parse_default_initargs(default_initargs) {
            Ok(defaults) => defaults,
            Err(value) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("default initargs must be (keyword thunk) pairs");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        let metaclass_id = match requested_metaclass(metaclass) {
            Ok(id) => id,
            Err(value) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("metaclass is not a class");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        match class_table(ctx).redefine_scheme_class_from_slot_list(
            ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
            metaclass_id,
            &default_initargs,
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
            Err(SchemeClassSpecError::ClassTable(ClassTableError::NotMetaclass(_))) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("metaclass is not a class metaobject class");
                call.raise_assertion_violation(who, message, metaclass.unwrap_or(class.into()))
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::MetaclassChanged(_))) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("metaclass of a class cannot change on redefinition");
                call.raise_assertion_violation(who, message, class.into())
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::InconsistentPrecedence(
                remaining,
            ))) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("inconsistent class precedence");
                let irritant = class_id_list_to_class_objects(ctx, &remaining);
                call.raise_assertion_violation(who, message, irritant)
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::SealedSuperclass(id))) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("superclass is sealed and takes no subclasses");
                call.raise_assertion_violation(who, message, class_id_to_class_object(ctx, id))
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::NotMalleable(_))) => {
                let who = ctx.intern("redefine-class!");
                let message = ctx.str("class is sealed and cannot be redefined");
                call.raise_assertion_violation(who, message, class.into())
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
        default_initargs: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let default_initargs = match parse_default_initargs(default_initargs) {
            Ok(defaults) => defaults,
            Err(value) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("default initargs must be (keyword thunk) pairs");
                return call.raise_assertion_violation(who, message, value);
            }
        };
        match class_table(ctx).redefine_invocable_scheme_class_from_slot_list(
            ctx,
            class,
            name,
            slot_names,
            direct_supers.unwrap_or(Value::null()),
            &default_initargs,
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
            Err(SchemeClassSpecError::ClassTable(ClassTableError::InconsistentPrecedence(
                remaining,
            ))) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("inconsistent class precedence");
                let irritant = class_id_list_to_class_objects(ctx, &remaining);
                call.raise_assertion_violation(who, message, irritant)
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::SealedSuperclass(id))) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("superclass is sealed and takes no subclasses");
                call.raise_assertion_violation(who, message, class_id_to_class_object(ctx, id))
            }
            Err(SchemeClassSpecError::ClassTable(ClassTableError::NotMalleable(_))) => {
                let who = ctx.intern("redefine-invocable-class!");
                let message = ctx.str("class is sealed and cannot be redefined");
                call.raise_assertion_violation(who, message, class.into())
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

    #[scheme(name = "%make-generic-with-class")]
    pub fn make_generic_with_class(
        name: Gc<'gc, Symbol<'gc>>,
        max_required_dispatch_args: i32,
        class: Value<'gc>,
    ) -> Value<'gc> {
        let who = ctx.intern("%make-generic-with-class");
        if max_required_dispatch_args < 0 {
            let message = ctx.str("required dispatch argument count must be non-negative");
            return call.raise_assertion_violation(
                who,
                message,
                Value::new(max_required_dispatch_args),
            );
        }
        let Some(class_descriptor) = class
            .is::<ClassDescriptor>()
            .then(|| class.downcast::<ClassDescriptor>())
            .filter(|class| class.layout_family() == LayoutFamily::Generic)
        else {
            let message = ctx.str("generic function class must be a subclass of <generic>");
            return call.raise_assertion_violation(who, message, class);
        };
        let generic = GenericDescriptor::new_in_class(
            ctx,
            &name.to_string(),
            max_required_dispatch_args as usize,
            class_descriptor.id(),
        );
        let proc = generic_procedure(ctx, name.into(), generic);
        grace(ctx, proc.into())
    }

    /// AMOP `set-funcallable-instance-function!`. A generic takes a
    /// discriminating function (`#f` restores native dispatch). An invocable
    /// instance takes the procedure it forwards to.
    #[scheme(name = "set-funcallable-instance-function!")]
    pub fn set_funcallable_instance_function(
        funcallable: Value<'gc>,
        function: Value<'gc>,
    ) -> Value<'gc> {
        let who = ctx.intern("set-funcallable-instance-function!");
        if let Some(generic) = generic_descriptor_from_value(ctx, funcallable) {
            if generic.flags().sealed() {
                let message = ctx.str("generic is sealed");
                return call.raise_assertion_violation(who, message, funcallable);
            }
            let removing = function == Value::new(false);
            if !removing && !function.is::<Closure>() {
                let message = ctx.str("discriminating function is not a procedure");
                return call.raise_assertion_violation(who, message, function);
            }
            GenericDescriptor::set_custom_dfun(ctx, generic, (!removing).then_some(function));
            return grace(ctx, Value::undefined());
        }
        if invocable_instance_of(ctx, funcallable).is_some() && function.is::<Closure>() {
            // The wrapper forwards to free slot 2 (see `invocable_instance_closure_proc`).
            let closure = funcallable.downcast::<Closure>();
            Gc::write(ctx, closure)[2].unlock().set(function);
            return Value::undefined();
        }
        let message = ctx.str("not a funcallable instance");
        call.raise_assertion_violation(who, message, funcallable)
    }

    #[scheme(name = "%enable-mop-protocol!")]
    pub fn enable_mop_protocol() -> Value<'gc> {
        crate::runtime::objectmodel::class::generic::MOP_ENABLED
            .store(true, std::sync::atomic::Ordering::Release);
        Value::undefined()
    }

    #[scheme(name = "%mop-protocol-enabled?")]
    pub fn mop_protocol_enabled() -> bool {
        crate::runtime::objectmodel::class::generic::MOP_ENABLED
            .load(std::sync::atomic::Ordering::Acquire)
    }

    #[scheme(name = "%add-method!")]
    pub fn add_method(
        generic: Value<'gc>,
        specializers: Value<'gc>,
        required_arg_count: i32,
        body: Value<'gc>,
        locked: Option<bool>,
        no_next: Option<bool>,
        slot_kind: Option<Value<'gc>>,
        slot_name: Option<Value<'gc>>,
        qualifiers: Option<Value<'gc>>,
        next_cursor: Option<bool>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        if required_arg_count < 0 {
            let who = ctx.intern("add-method!");
            let message = ctx.str("required argument count must be non-negative");
            return call.raise_assertion_violation(who, message, Value::new(required_arg_count));
        }

        let parsed = match parse_specializer_list(specializers) {
            Ok(parsed) => parsed,
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
        let specializers: Vec<ClassId> = parsed.iter().map(|spec| spec.class).collect();
        let eql: Vec<Value<'gc>> = parsed.iter().map(|spec| spec.eql).collect();
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

        let qualifiers = qualifiers.unwrap_or(Value::null());
        if !is_keyword_list(qualifiers) {
            let who = ctx.intern("add-method!");
            let message = ctx.str("qualifiers must be a list of keywords");
            return call.raise_assertion_violation(who, message, qualifiers);
        }

        if let Some(method) =
            generic.find_method(&specializers, &eql, required_arg_count as usize, qualifiers)
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
        // A body whose zero-argument next-method uses its own captured
        // parameters: dispatch passes it a chain cursor (see `next_cursor`).
        if next_cursor.unwrap_or(false) && !no_next.unwrap_or(false) {
            flags = flags.with_next_cursor();
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
            &eql,
            required_arg_count as usize,
            body,
            flags,
            qualifiers,
        );
        if let (Some(_), Some(name)) = (slot_kind, slot_name) {
            MethodDescriptor::set_slot_name(ctx, method, name);
            GenericDescriptor::invalidate(ctx, generic, Reason::AddMethod(method));
        }
        grace(ctx, method.into())
    }

    /// Remove a method from a generic. `#f` when the method is not one of its
    /// methods. Sealed generics and locked methods refuse.
    #[scheme(name = "%remove-method!")]
    pub fn remove_method(
        generic: Value<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let Some(generic) = generic_descriptor_from_value(ctx, generic) else {
            let who = ctx.intern("remove-method!");
            let message = ctx.str("not a generic procedure");
            return call.raise_assertion_violation(who, message, generic);
        };
        if generic.flags().sealed() {
            let who = ctx.intern("remove-method!");
            let message = ctx.str("generic is sealed");
            return call.raise_assertion_violation(who, message, generic.into());
        }
        if method.flags().locked() {
            let who = ctx.intern("remove-method!");
            let message = ctx.str("method is locked");
            return call.raise_assertion_violation(who, message, method.into());
        }
        let removed = GenericDescriptor::remove_method(ctx, generic, method);
        grace(ctx, Value::new(removed))
    }

    #[scheme(name = "set-generic-fallback!")]
    pub fn set_generic_fallback(generic: Value<'gc>, fallback: Value<'gc>) -> Value<'gc> {
        let _mop = crate::runtime::objectmodel::class::lock::mop_lock(ctx);
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("set-generic-fallback!");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        if generic.flags().sealed() {
            let who = ctx.intern("set-generic-fallback!");
            let message = ctx.str("generic is sealed");
            return call.raise_assertion_violation(who, message, generic.into());
        }
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
        if !generic.flags().simple() {
            let who = ctx.intern("generic-dispatch");
            let message = ctx.str("generic has a method combination, use generic-invoke");
            return call.raise_assertion_violation(who, message, generic.into());
        }
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
        let generic_value = generic;
        let generic = match generic_descriptor_from_value(ctx, generic) {
            Some(generic) => generic,
            None => {
                let who = ctx.intern("generic-invoke");
                let message = ctx.str("not a generic procedure");
                return call.raise_assertion_violation(who, message, generic);
            }
        };
        if !generic.flags().simple() {
            // Combined methods: the effective method, as a generic call makes.
            let epoch = generic.epoch();
            return match GenericDescriptor::applicable_methods_at(ctx, generic, args, epoch) {
                Ok(methods) => {
                    let (hook, full) =
                        crate::runtime::objectmodel::class::generic::effective_method_call(
                            ctx,
                            generic_value,
                            epoch,
                            &methods,
                            args,
                        );
                    call.tail(hook, &full)
                }
                Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                    call.tail(generic.fallback(), args)
                }
                Err(GenericDispatchError::Arity) => {
                    let who = ctx.intern("generic-invoke");
                    let message = ctx.str("not enough dispatch arguments");
                    call.raise_assertion_violation(who, message, Value::new(args.len() as i32))
                }
                Err(GenericDispatchError::NoApplicableMethod) => {
                    match ctx.try_hook(crate::runtime::hooks::Hook::NoApplicableMethod) {
                        Some(hook) => {
                            let mut full: Vec<Value<'gc>> = Vec::with_capacity(args.len() + 1);
                            full.push(generic.into());
                            full.extend_from_slice(args);
                            call.tail(hook, &full)
                        }
                        None => {
                            let who = ctx.intern("generic-invoke");
                            let message = ctx.str("no applicable method");
                            call.raise_assertion_violation(who, message, generic.into())
                        }
                    }
                }
            };
        }
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
                match ctx.try_hook(crate::runtime::hooks::Hook::NoApplicableMethod) {
                    Some(hook) => {
                        let mut full: Vec<Value<'gc>> = Vec::with_capacity(args.len() + 1);
                        full.push(generic.into());
                        full.extend_from_slice(args);
                        call.tail(hook, &full)
                    }
                    None => {
                        let who = ctx.intern("generic-invoke");
                        let message = ctx.str("no applicable method");
                        call.raise_assertion_violation(who, message, generic.into())
                    }
                }
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
        if !generic.flags().simple() {
            let who = ctx.intern("generic-next-methods");
            let message = ctx.str("generic has a method combination, no single next-method chain");
            return call.raise_assertion_violation(who, message, generic.into());
        }
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

    /// `(%make-next-method generic methods args [tail])`: a next-method
    /// object over the method list. `tail` (a procedure, or `#f`) runs when
    /// the list is exhausted, see `next-method-invoke`.
    #[scheme(name = "%make-next-method")]
    pub fn make_next_method(
        generic: Value<'gc>,
        methods: Value<'gc>,
        args: Value<'gc>,
        tail: Option<Value<'gc>>,
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

        let tail = match tail {
            Some(tail) if tail != Value::new(false) => tail,
            _ => Value::empty(),
        };
        let next_method = NextMethodDescriptor::with_tail(
            ctx,
            generic,
            Array::from_slice(ctx, &method_values),
            Array::from_slice(ctx, &arg_values),
            0,
            tail,
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

    /// Run the next method of `next` with its own args, or the chain's tail,
    /// or raise `no-next-method`. `#f` (no next-method object: the body was
    /// entered without one) raises the same violation.
    #[scheme(name = "next-method-invoke")]
    pub fn next_method_invoke(next: Value<'gc>) -> Value<'gc> {
        next_method_call(ctx, call, next, None)
    }

    /// `next-method-invoke` with the arguments replaced: `(next-method-invoke-with
    /// next arg ...)`. CLOS wants the same applicable methods; this is not checked.
    #[scheme(name = "next-method-invoke-with")]
    pub fn next_method_invoke_with(next: Value<'gc>, args: &'gc [Value<'gc>]) -> Value<'gc> {
        next_method_call(ctx, call, next, Some(args))
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

    /// `make-instance` with the initargs as one list: `make` calls it so it
    /// needs no `apply`.
    #[scheme(name = "%make-instance-list")]
    pub fn make_instance_list(
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: Value<'gc>,
    ) -> Value<'gc> {
        let items = crate::runtime::objectmodel::class::generic::list_items(initargs);
        super::instantiate(ctx, call, class, &items)
    }

    #[scheme(name = "slot-ref")]
    pub fn slot_ref(
        instance: Value<'gc>,
        name: Gc<'gc, Symbol<'gc>>,
        fallback: Option<Value<'gc>>,
    ) -> Value<'gc> {
        if instance.is::<ClassDescriptor>() {
            let class = instance.downcast::<ClassDescriptor>();
            return class_extension_slot_ref(ctx, call, class, name, fallback);
        }
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
        if instance.is::<ClassDescriptor>() {
            let class = instance.downcast::<ClassDescriptor>();
            return match ClassDescriptor::extension_slot_set(ctx, class, &name.to_string(), value) {
                Ok(()) => Value::undefined(),
                Err(SlotAccessError::Immutable) => {
                    let who = ctx.intern("slot-set!");
                    let message = ctx.str("immutable slot");
                    call.raise_assertion_violation(who, message, name.into())
                }
                Err(SlotAccessError::Unknown) => {
                    let who = ctx.intern("slot-set!");
                    let message = ctx.str("unknown slot");
                    call.raise_assertion_violation(who, message, name.into())
                }
                Err(_) => {
                    let who = ctx.intern("slot-set!");
                    let message = ctx.str("stale slot accessor");
                    call.raise_assertion_violation(who, message, name.into())
                }
            };
        }
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

    /// `slot-unbind!`: make slot `name` of `instance` unbound, so its reader
    /// reports it unbound until a write binds it again. Goes through the
    /// slot-unbind protocol generic when the class's protocol is not plain.
    #[scheme(name = "slot-unbind!")]
    pub fn slot_unbind(instance: Value<'gc>, name: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-unbind!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance_obj);
        let slot_name = name.to_string();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);

        if !protocol_is_plain(ctx, class, OP_UNBIND)
            && let Some(slot_unbind_using_class) =
                ctx.try_hook(crate::runtime::hooks::Hook::SlotUnbindUsingClass)
        {
            let slot = class_slot_argument(ctx, class, &slot_name, name.into());
            return call_protocol(
                ctx,
                call,
                slot_unbind_using_class,
                &[class.into(), receiver, slot],
            );
        }

        let Some(accessor) = class.accessor_named(&slot_name) else {
            let who = ctx.intern("slot-unbind!");
            let message = ctx.str("class has no slot writer");
            return call.raise_assertion_violation(who, message, class.into());
        };
        match unbind_slot_value(ctx, class, receiver, accessor) {
            None => {
                let who = ctx.intern("slot-unbind!");
                let message = ctx.str("class has no slot writer");
                call.raise_assertion_violation(who, message, class.into())
            }
            Some(Ok(())) => Value::undefined(),
            Some(Err(error)) => raise_slot_error(ctx, call, "slot-unbind!", error, name.into()),
        }
    }

    /// `slot-unbind-using-accessor!`: the stock unbind of one slot, with no
    /// protocol dispatch (the standard `slot-unbind-using-class!` method).
    #[scheme(name = "slot-unbind-using-accessor!")]
    pub fn slot_unbind_using_accessor(
        instance: Value<'gc>,
        accessor: Gc<'gc, SlotAccessorDefinition<'gc>>,
    ) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("slot-unbind-using-accessor!");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance_obj);
        let accessor = accessor.as_accessor_descriptor();
        let class = instance_obj.class();
        let receiver = Value::from(instance_obj);
        let accessor_name: Value<'gc> = Symbol::from_str(ctx, accessor.name()).into();
        match unbind_slot_value(ctx, class, receiver, accessor) {
            None => {
                let who = ctx.intern("slot-unbind-using-accessor!");
                let message = ctx.str("class has no slot writer");
                call.raise_assertion_violation(who, message, class.into())
            }
            Some(Ok(())) => Value::undefined(),
            Some(Err(error)) => raise_slot_error(
                ctx,
                call,
                "slot-unbind-using-accessor!",
                error,
                accessor_name,
            ),
        }
    }

    /// `standard-instance-access`: slot `index` of a Scheme instance, read
    /// from its storage without the slot protocol or the slot caches. The
    /// instance is migrated first, and an unbound slot raises rather than
    /// letting the unbound marker escape.
    #[scheme(name = "standard-instance-access")]
    pub fn standard_instance_access(instance: Value<'gc>, index: i32) -> Value<'gc> {
        let Some(instance_obj) = try_scheme_instance(ctx, instance) else {
            let who = ctx.intern("standard-instance-access");
            let message = ctx.str("not a Scheme instance");
            return call.raise_assertion_violation(who, message, instance);
        };
        SchemeInstance::touch(ctx, instance_obj);
        if index < 0 || index as usize >= instance_obj.slot_count() {
            let who = ctx.intern("standard-instance-access");
            let message = ctx.str("slot index out of bounds");
            return call.raise_assertion_violation(who, message, Value::new(index));
        }
        match instance_obj.slot_checked(index as usize) {
            Some(value) if !value.is_empty() => value,
            _ => {
                let who = ctx.intern("standard-instance-access");
                let message = ctx.str("unbound slot");
                call.raise_assertion_violation(who, message, Value::new(index))
            }
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
                crate::runtime::objectmodel::ExecutionResult::Ok(value) => value,
                crate::runtime::objectmodel::ExecutionResult::Err(error) => {
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
        if instance.is::<ClassDescriptor>() {
            let class = instance.downcast::<ClassDescriptor>();
            return match ClassDescriptor::extension_slot_bound(ctx, class, &name.to_string()) {
                Ok(bound) => Value::new(bound),
                Err(SlotAccessError::Unknown) => {
                    let who = ctx.intern("slot-bound?");
                    let message = ctx.str("unknown slot");
                    call.raise_assertion_violation(who, message, name.into())
                }
                Err(_) => {
                    let who = ctx.intern("slot-bound?");
                    let message = ctx.str("stale slot accessor");
                    call.raise_assertion_violation(who, message, name.into())
                }
            };
        }
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

/// Whether `list` is a proper list whose elements are all keywords.
fn is_keyword_list<'gc>(list: Value<'gc>) -> bool {
    let mut cursor = list;
    while cursor.is_pair() {
        if !cursor.car().is::<Keyword>() {
            return false;
        }
        cursor = cursor.cdr();
    }
    cursor.is_null()
}

pub(crate) fn init_class_ops<'gc>(ctx: MutationContext<'gc>) {
    class_ops::register(ctx);
}

/// The body of `next-method-invoke` and `next-method-invoke-with`: enter the
/// next method of the chain with `args` (the chain's own args when `None`).
fn next_method_call<'gc>(
    ctx: MutationContext<'gc>,
    call: &crate::runtime::vm::NativeCall<'_, 'gc>,
    next: Value<'gc>,
    args: Option<&[Value<'gc>]>,
) -> Value<'gc> {
    use crate::runtime::objectmodel::Closure;
    use crate::runtime::objectmodel::class::NextMethodDescriptor;
    let Some(next_method) = next.try_as::<NextMethodDescriptor>() else {
        let who = ctx.intern("next-method-invoke");
        let message = ctx.str("no next method");
        return call.raise_assertion_violation(who, message, next);
    };
    // A cursor carries no arguments. The bodies it is passed to call only
    // `next-method-invoke-with`, so a zero-argument call on one is an error.
    if args.is_none() && next_method.is_cursor() {
        let who = ctx.intern("next-method-invoke");
        let message = ctx.str("next method has no arguments of its own");
        return call.raise_assertion_violation(who, message, next);
    }
    let stored = next_method.args();
    let args: &[Value<'gc>] = match args {
        Some(args) => args,
        None => stored.as_slice(),
    };
    let methods = next_method.methods();
    let index = next_method.index() + 1;
    if index < methods.len() {
        let body = methods[index].body();
        // A body that never reads its next-method argument gets `#f`: the
        // chain step allocates nothing then. A cursor body gets no argument
        // array either.
        let lead = NextMethodDescriptor::lead(
            ctx,
            next_method.generic(),
            methods,
            index,
            next_method.tail().unwrap_or(Value::empty()),
            args,
        );
        if body.is::<Closure>() {
            return call.enter_with(body, lead, args);
        }
        let mut full = vec![lead];
        full.extend_from_slice(args);
        return call.tail(body, &full);
    }
    if let Some(tail) = next_method.tail() {
        return call.tail(tail, args);
    }
    let who = ctx.intern("next-method-invoke");
    let message = ctx.str("no next method");
    call.raise_assertion_violation(who, message, next)
}
