//! Inline cache for instance slots and generic calls.

use crate::heap::Gc;
use crate::heap::object::{ClassId, builtin_class_ids};
use crate::runtime::class::hooks::{
    PrimitiveSlotBoundHook, PrimitiveSlotRefHook, PrimitiveSlotSetHook,
};
use crate::runtime::class::instance::{
    scheme_instance_slot_bound_hook, scheme_instance_slot_ref_hook, scheme_instance_slot_set_hook,
};
use crate::runtime::class::{
    ClassCategory, SLOT_PROTOCOL_GENERICS, SchemeInstance, class_root_binding, class_table,
    generic_descriptor_from_value, try_scheme_instance,
};
use crate::runtime::class::{OP_REF, OP_SET, PROTOCOL_CUSTOM, PROTOCOL_PLAIN};
use crate::runtime::value::Symbol;
use crate::runtime::watchpoint::WatchpointSet;
use crate::runtime::{MutationContext, value::Value};

/// What `get_slot`/`put_slot` may cache for `obj` and `name`.
pub struct SlotHit<'gc> {
    pub class_id: u32,
    /// The class's slot-protocol set the decision was made under. A fill
    /// registers on it before publishing and rechecks it after, so a
    /// protocol change during the lookup clears the entry (see
    /// [`crate::runtime::watchpoint`]).
    pub wp: Gc<'gc, WatchpointSet<'gc>>,
    /// Layout stamp of the descriptor the index was resolved against.
    pub stamp: u32,
    pub index: u32,
}

/// Decide whether the access `obj.name` can be served from a cache. `None` means "take the slow
/// path through the real procedure".
///
/// The instance is touched (migrated when its class was redefined) before
/// anything else, so a hit always describes the current descriptor.
pub fn slot_lookup<'gc>(
    ctx: MutationContext<'gc>,
    obj: Value<'gc>,
    name: Value<'gc>,
    for_write: bool,
) -> Option<SlotHit<'gc>> {
    let name = name.try_as::<Symbol>()?;
    let instance = try_scheme_instance(ctx, obj)?;
    SchemeInstance::touch(ctx, instance);
    let class = instance.class();
    debug_assert_eq!(class.category(), ClassCategory::Scheme);
    // Read the class's set before deciding anything: writers mutate the
    // protocol first and fire last, so a decision made under a set that is
    // still valid at registration saw no change, and a fresh set already
    // sees it.
    let wp = class.slot_protocol_wp();
    if wp.is_invalid() {
        return None;
    }
    // The instance must point at the current descriptor for its class id:
    // anything else is an obsolete layout the touch should have migrated.
    let current = class_table(ctx).lookup(class.id())?;
    if !Gc::ptr_eq(current, class) {
        return None;
    }
    if !slot_protocol_plain(ctx, class, wp, if for_write { OP_SET } else { OP_REF }) {
        return None;
    }
    // The boot method must actually apply to this class: its `<class>`
    // specializer only matches classes whose metaclass inherits `<class>`.
    let class_id = ClassId::new(builtin_class_ids::CLASS)?;
    let meta = Value::from(class).class(ctx)?;
    if !meta.is_a(class_id) {
        return None;
    }
    let accessor = class.accessor_named(&name.as_str())?;
    // Plain storage only
    if !accessor.scheme_slot_ref().is_empty()
        || !accessor.scheme_slot_set().is_empty()
        || !accessor.scheme_slot_bound().is_empty()
    {
        return None;
    }
    if accessor.slot_ref().is_some()
        || accessor.slot_set().is_some()
        || accessor.slot_bound().is_some()
    {
        return None;
    }
    if accessor.class_allocated() {
        return None;
    }
    let hooks = class.primitive_operation_hooks();

    let default_ref: PrimitiveSlotRefHook = scheme_instance_slot_ref_hook;
    let default_set: PrimitiveSlotSetHook = scheme_instance_slot_set_hook;
    let default_bound: PrimitiveSlotBoundHook = scheme_instance_slot_bound_hook;
    if !hooks
        .slot_ref()
        .is_some_and(|hook| std::ptr::fn_addr_eq(hook, default_ref))
        || !hooks
            .slot_set()
            .is_some_and(|hook| std::ptr::fn_addr_eq(hook, default_set))
        || !hooks
            .slot_bound()
            .is_some_and(|hook| std::ptr::fn_addr_eq(hook, default_bound))
    {
        return None;
    }
    if for_write && !accessor.flags().settable() {
        return None;
    }
    let index = accessor.slot_index();
    if index >= instance.slot_count() {
        return None;
    }
    Some(SlotHit {
        class_id: class.id().bits(),
        wp,
        stamp: class.layout_stamp(),
        index: index as u32,
    })
}

/// Whether the stock method alone answers protocol operation `op` for
/// `class` right now (false when unsure).
pub(crate) fn protocol_is_plain<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, crate::runtime::class::ClassDescriptor<'gc>>,
    op: usize,
) -> bool {
    let wp = class.slot_protocol_wp();
    !wp.is_invalid() && slot_protocol_plain(ctx, class, wp, op)
}

/// Whether the stock method alone answers the slot protocol operation for
/// instances of `class`: among the methods of the protocol generic that may
/// apply to `(metaclass, class, <slot-definition> . _)`, exactly one, the
/// stock one. Decided per class, so a user method on one class leaves every
/// other class cached. The answer is cached on the descriptor and dropped
/// whenever its `wp` fires
fn slot_protocol_plain<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, crate::runtime::class::ClassDescriptor<'gc>>,
    wp: Gc<'gc, WatchpointSet<'gc>>,
    op: usize,
) -> bool {
    match class.protocol_state(op) {
        PROTOCOL_PLAIN => return true,
        PROTOCOL_CUSTOM => return false,
        _ => {}
    }
    let state = if compute_plain(ctx, class, op) {
        PROTOCOL_PLAIN
    } else {
        PROTOCOL_CUSTOM
    };
    let state = class.publish_protocol_state(op, state);
    // A fire between the read of `wp` and the publish may have reset
    // states before this one landed: take it back.
    if wp.is_invalid() {
        class.reset_protocol_state(op);
        return false;
    }
    state == PROTOCOL_PLAIN
}

fn compute_plain<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, crate::runtime::class::ClassDescriptor<'gc>>,
    op: usize,
) -> bool {
    let (name, dispatch_args) = SLOT_PROTOCOL_GENERICS[op];
    let Some(bound) = class_root_binding(ctx, name) else {
        return true;
    };
    let Some(generic) = generic_descriptor_from_value(ctx, bound) else {
        return false;
    };
    if !generic.fallback().is_empty() || generic.max_required_dispatch_args() != dispatch_args {
        return false;
    }
    let Some(meta) = Value::from(class).class(ctx) else {
        return false;
    };

    let Some(slot_definition) =
        ClassId::new(builtin_class_ids::SLOT_DEFINITION).and_then(|id| class_table(ctx).lookup(id))
    else {
        return false;
    };
    let key = [meta, class, slot_definition];
    let methods = generic.methods();
    let mut applicable = methods
        .iter()
        .copied()
        .filter(|method| method.may_apply_to_classes(&key));
    match (applicable.next(), applicable.next()) {
        (Some(method), None) => method.flags().boot_slot_protocol(),
        _ => false,
    }
}
