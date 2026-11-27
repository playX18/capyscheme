use super::builtin::builtin_id;
use super::flags::{ClassCategory, ClassFlags, LayoutFamily};
use super::hooks::{PrimitiveLayoutHooks, PrimitiveOperationHooks};
use super::instance::SchemeInstance;
use super::slot::{SlotAccessError, SlotAccessorDescriptor, SlotDescriptor, SlotSpec};
use super::storage::SlotStorage;
use super::table::{
    class_table, primitive_layout_hooks_for_class_id, registered_is_forward_reference,
    registered_layout_family,
};
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::Lock;
use crate::heap::object::{AllocationHooksOf, ClassId, builtin_class_ids};
use crate::heap::{Gc, Trace, Visitor, Weak, WeakProcessor};
use crate::prelude::*;
use crate::runtime::objectmodel::Keyword;
use crate::runtime::objectmodel::conversions::ClassTagged;
use crate::runtime::watchpoint::WatchpointSet;
use crate::runtime::{MutationContext, objectmodel::Value};

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
    /// The assumption slot inline caches over this class rely on: its slot
    /// protocol is the plain boot one. Fired (and replaced by a fresh set)
    /// when that may have changed; see [`Self::renew_slot_protocol_wp`].
    slot_protocol_wp: Lock<Gc<'gc, WatchpointSet<'gc>>>,
    /// Identity of this descriptor's slot layout, unique among all
    /// descriptors ever created (0 for classes that are never slot-cached:
    /// builtin and primitive-layout ones). A `SlotStorage` carries the stamp
    /// of the descriptor it was built for.
    layout_stamp: u32,
    /// The native layout family of this class's instances, see
    /// [`LayoutFamily`]. Inherited from the first non-`None` family in the
    /// class precedence list, so a user subclass of a metaobject class shares
    /// its family.
    layout_family: LayoutFamily,
    /// Whether `make-instance` allocates instances of this class through the
    /// stock `SchemeInstance` allocation hook: a Scheme class that supplied
    /// no hook of its own. Optimized code may then build the class header and
    /// slot storage inline.
    plain_scheme_allocation: bool,
    /// Fired when a redefinition replaces this descriptor in the class table.
    /// Optimized allocation units register here so their assumption that this
    /// descriptor and its layout are current invalidates them. The descriptor
    /// is dead after the fire, so the set is never renewed. `None` unless
    /// inline instance allocation is enabled when the descriptor is made, so
    /// the feature costs nothing while off.
    redefinition_wp: Lock<Gc<'gc, WatchpointSet<'gc>>>,
    /// Fired when a direct subclass of this class is linked or unlinked.
    subclass_wp: Lock<Gc<'gc, WatchpointSet<'gc>>>,
    /// Counts the changes to `direct_subclasses`. A unit records it with
    /// `subclass_wp` and rechecks it at install, which closes the race
    /// between reading the subtree and registering on the set.
    subclass_stamp: std::sync::atomic::AtomicU32,
    /// Whether the stock method alone answers each slot protocol operation
    /// (ref, set) for instances of this class: `PROTOCOL_*`, computed on the
    /// first slot IC miss and reset whenever the set above fires.
    protocol_state: [std::sync::atomic::AtomicU8; 5],
    /// Slot definition objects handed to the slot protocol generics, as an
    /// alist from slot name to definition. Slots never change on a
    /// descriptor (redefinition makes a new one), so entries never go
    /// stale; a racing writer may drop an entry, which is only recomputed.
    slot_definitions: Lock<Value<'gc>>,
    /// The class id of this descriptor's metaclass, the class of this class
    /// object. The header of the descriptor carries the same id.
    metaclass_id: ClassId,
    /// Values of the metaclass's own slots (its extension slots), indexed as
    /// the metaclass's slot list. Built when the descriptor is made, and
    /// rebuilt on demand once the metaclass's layout stamp moves on, see
    /// [`Self::extension_storage`]. `None` when the metaclass has no slots.
    ext: Lock<Option<Gc<'gc, SlotStorage<'gc>>>>,
    /// The metaclass descriptor `ext` was built for. When the metaclass is
    /// redefined, the values of `ext` are carried over by slot name through
    /// this descriptor's layout.
    ext_layout: Lock<Option<Gc<'gc, ClassDescriptor<'gc>>>>,
    /// The default initargs this class names itself, flat: the keyword, then
    /// its thunk, for each. See [`Self::direct_default_initargs`].
    direct_default_initargs: ArrayRef<'gc, Value<'gc>>,
    /// The default initargs `make-instance` supplies, flat like
    /// `direct_default_initargs`: the class's own, then each superclass's in
    /// precedence order, a key once. Empty for most classes, so that the
    /// instantiation fast paths can test `is_empty`.
    default_initargs: ArrayRef<'gc, Value<'gc>>,
    /// The class prototype, see [`Self::prototype`]. `Value::empty()` until
    /// it is first requested.
    prototype: Lock<Value<'gc>>,
    /// The AMOP dependents (`add-dependent`): a Scheme list, held strongly and
    /// deduplicated by `eq?`. Notified by redefinition, see `pkgs/core/src/boot/base.scm`.
    dependents: Lock<Value<'gc>>,
}

pub(crate) const PROTOCOL_UNKNOWN: u8 = 0;
pub(crate) const PROTOCOL_PLAIN: u8 = 1;
pub(crate) const PROTOCOL_CUSTOM: u8 = 2;

static NEXT_LAYOUT_STAMP: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1);

fn next_layout_stamp() -> u32 {
    let stamp = NEXT_LAYOUT_STAMP.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    assert!(stamp != 0, "layout stamps exhausted");
    stamp
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
    /// The precedence list of each superclass, see
    /// [`super::slot::compute_effective_slots`].
    pub(crate) superclass_cpls: &'a [(ClassId, Vec<ClassId>)],
    pub(crate) direct_slot_specs: &'a [SlotSpec<'a, 'gc>],
    pub(crate) direct_subclasses: Option<ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>>>,
    pub(crate) direct_methods: Option<ArrayRef<'gc, Value<'gc>>>,
    /// The metaclass of the new class. `None` means `<class>`, the ordinary
    /// metaclass; the table passes the resolved descriptor otherwise.
    pub(crate) metaclass: Option<Gc<'gc, ClassDescriptor<'gc>>>,
    /// The default initargs the class names itself, as keyword and thunk pairs.
    pub(crate) direct_default_initargs: &'a [(Value<'gc>, Value<'gc>)],
    /// The default initargs `make-instance` supplies, see
    /// [`ClassDescriptor::default_initargs`].
    pub(crate) default_initargs: &'a [(Value<'gc>, Value<'gc>)],
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

// SAFETY: GC trace for `ClassDescriptor`, all reachable heap fields are visited
unsafe impl Trace for ClassDescriptor<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
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
            self.slot_protocol_wp.trace(visitor);
            self.redefinition_wp.trace(visitor);
            self.subclass_wp.trace(visitor);
            self.slot_definitions.trace(visitor);
            self.ext.trace(visitor);
            self.ext_layout.trace(visitor);
            self.direct_default_initargs.trace(visitor);
            self.default_initargs.trace(visitor);
            self.prototype.trace(visitor);
            self.dependents.trace(visitor);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
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
            self.slot_protocol_wp.process_weak_refs(weak_processor);
            self.redefinition_wp.process_weak_refs(weak_processor);
            self.subclass_wp.process_weak_refs(weak_processor);
            self.slot_definitions.process_weak_refs(weak_processor);
            self.ext.process_weak_refs(weak_processor);
            self.ext_layout.process_weak_refs(weak_processor);
            self.direct_default_initargs
                .process_weak_refs(weak_processor);
            self.default_initargs.process_weak_refs(weak_processor);
            self.prototype.process_weak_refs(weak_processor);
            self.dependents.process_weak_refs(weak_processor);
        }
    }
}

// SAFETY: `gc` for `ClassDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for ClassDescriptor<'gc> {
    const TYPE_NAME: &'static str = "class";
    const CLASS_IDS: &'static [u32] = &[
        builtin_class_ids::CLASS,
        builtin_class_ids::FORWARD_REFERENCED_CLASS,
    ];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::Class);
}

/// The family for a class with identity `id` and class precedence list `cpl`:
/// the builtin family of `id` itself, else the first non-`None` family found
/// walking `cpl`. Registered supers are read from the class table; `id` is
/// never looked up there, since on redefinition the table still holds the
/// old descriptor.
fn layout_family_from_hierarchy(id: ClassId, cpl: &[ClassId]) -> LayoutFamily {
    if let Some(family) = LayoutFamily::of_builtin_id(id.bits()) {
        return family;
    }
    cpl.iter()
        .find_map(|&class_id| {
            if let Some(family) = LayoutFamily::of_builtin_id(class_id.bits()) {
                return Some(family);
            }
            if class_id == id {
                return None;
            }
            match registered_layout_family(class_id) {
                LayoutFamily::None => None,
                family => Some(family),
            }
        })
        .unwrap_or(LayoutFamily::None)
}

/// The family of the registered or builtin class `id`, or `None`.
fn family_of_class(id: ClassId) -> LayoutFamily {
    LayoutFamily::of_builtin_id(id.bits()).unwrap_or_else(|| registered_layout_family(id))
}

/// The nearest class after `id` in `cpl` whose instances have a metaobject
/// layout. A Scheme class with such a base (a metaclass, say) shares its
/// layout and hooks.
fn metaobject_base(id: ClassId, cpl: &[ClassId]) -> Option<ClassId> {
    cpl.iter()
        .copied()
        .filter(|&class_id| class_id != id)
        .find(|&class_id| family_of_class(class_id) != LayoutFamily::None)
}

/// A fresh extension storage for instances of `metaclass`. A slot takes its
/// value from `previous` (the old values and the metaclass they were laid out
/// by, when the metaclass was redefined) by name, else its initial value.
fn extension_storage_for<'gc>(
    ctx: MutationContext<'gc>,
    metaclass: Gc<'gc, ClassDescriptor<'gc>>,
    previous: Option<(Gc<'gc, ClassDescriptor<'gc>>, Gc<'gc, SlotStorage<'gc>>)>,
) -> Gc<'gc, SlotStorage<'gc>> {
    SlotStorage::with(
        ctx,
        metaclass.layout_stamp(),
        metaclass.slot_count(),
        |_, index| {
            let slot = metaclass.slots()[index];
            previous
                .and_then(|(old_metaclass, old_values)| {
                    let accessor = old_metaclass.accessor_named(slot.name())?;
                    old_values.get(accessor.slot_index())
                })
                .filter(|value| !value.is_empty())
                .unwrap_or_else(|| slot.init_value())
        },
    )
}

impl<'gc> ClassDescriptor<'gc> {
    pub fn new(
        ctx: MutationContext<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
    ) -> Self {
        Self::with_hierarchy(ctx, id, name, category, &[], &[id])
    }

    pub fn with_hierarchy(
        ctx: MutationContext<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        cpl: &[ClassId],
    ) -> Self {
        Self::with_primitive_layout_hooks(ctx, id, name, category, None, direct_supers, cpl)
    }

    pub fn with_primitive_layout_hooks(
        ctx: MutationContext<'gc>,
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
                superclass_cpls: &[],
                direct_slot_specs: &[],
                direct_subclasses: None,
                direct_methods: None,
                metaclass: None,
                direct_default_initargs: &[],
                default_initargs: &[],
            },
        )
    }

    pub(crate) fn from_init(ctx: MutationContext<'gc>, init: ClassDescriptorInit<'_, 'gc>) -> Self {
        // A Scheme class under a metaobject class (e.g. a metaclass under
        // `<class>`) has instances laid out as that metaobject, not as a
        // `SchemeInstance`. Its layout and operation hooks come from the base.
        let metaobject_base = if init.category == ClassCategory::Scheme {
            metaobject_base(init.id, init.cpl)
        } else {
            None
        };
        // The default hooks (and so the stock `SchemeInstance` layout) are
        // exactly the descriptors that supplied none of their own.
        // Default initargs make `make-instance` with no initargs more than an
        // allocation, so such classes are not plain either.
        // A forward-reference placeholder, or a class with one among its
        // superclasses, is unfinalized: it has no instances yet. Recomputed on
        // every descriptor, so a resolved redefinition clears it.
        let unfinalized = init
            .metaclass
            .is_some_and(|meta| meta.id().bits() == builtin_class_ids::FORWARD_REFERENCED_CLASS)
            || init
                .cpl
                .iter()
                .any(|&class_id| class_id != init.id && registered_is_forward_reference(class_id));
        let flags = init
            .flags
            .unwrap_or_else(|| init.category.default_flags())
            .with_unfinalized(unfinalized);
        let plain_scheme_allocation = init.category == ClassCategory::Scheme
            && init.primitive_operation_hooks.is_none()
            && init.primitive_layout_hooks.is_none()
            && metaobject_base.is_none()
            && !unfinalized
            && init.default_initargs.is_empty();
        let primitive_layout_hooks =
            init.primitive_layout_hooks
                .or_else(|| match metaobject_base {
                    Some(base) => Some(
                        primitive_layout_hooks_for_class_id(base)
                            .expect("metaobject base classes have layout hooks"),
                    ),
                    None => default_primitive_layout_hooks(init.category),
                });
        // Instances of a metaobject subclass are not allocated by
        // `make-instance` yet (Phase 2), so they get no allocation hook.
        let primitive_operation_hooks = init.primitive_operation_hooks.unwrap_or_else(|| {
            if metaobject_base.is_some() {
                PrimitiveOperationHooks::default()
            } else {
                default_primitive_operation_hooks(init.category)
            }
        });
        let metaclass_id = init
            .metaclass
            .map_or_else(|| builtin_id(builtin_class_ids::CLASS), |meta| meta.id());
        let ext_layout = init.metaclass.filter(|meta| meta.slot_count() > 0);
        let ext = ext_layout.map(|meta| extension_storage_for(ctx, meta, None));
        let (direct_defs, effective_defs) = super::slot::compute_effective_slots(
            ctx,
            init.id,
            init.inherited_slots,
            init.direct_slot_specs,
            init.superclass_cpls,
        );
        let direct_slots = Array::new(ctx, direct_defs.len(), |_, index| direct_defs[index]);
        let slots = Array::new(ctx, effective_defs.len(), |_, index| effective_defs[index]);
        let accessors = Array::new(ctx, slots.len(), |ctx, index| {
            SlotAccessorDescriptor::from_slot(ctx, slots[index])
        });
        let class_slots = Array::new(ctx, slots.len(), |_, index| {
            let slot = slots[index];
            if slot.class_allocated() {
                Lock::new(slot.init_value())
            } else {
                Lock::new(Value::empty())
            }
        });
        let initargs = initarg_metadata(ctx, slots.as_slice());
        let layout_family = layout_family_from_hierarchy(init.id, init.cpl);

        Self {
            id: init.id,
            name: Array::from_slice(ctx, init.name.as_bytes()),
            category: init.category,
            flags: Lock::new(flags),
            primitive_layout_hooks,
            primitive_operation_hooks,
            direct_supers: Array::from_slice(ctx, init.direct_supers),
            cpl: Array::from_slice(ctx, init.cpl),
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
            slot_protocol_wp: Lock::new(WatchpointSet::new(ctx)),
            protocol_state: Default::default(),
            slot_definitions: Lock::new(Value::null()),
            layout_stamp: if init.category == ClassCategory::Scheme {
                next_layout_stamp()
            } else {
                0
            },
            layout_family,
            plain_scheme_allocation,
            redefinition_wp: Lock::new(WatchpointSet::new(ctx)),
            subclass_wp: Lock::new(WatchpointSet::new(ctx)),
            subclass_stamp: std::sync::atomic::AtomicU32::new(0),
            metaclass_id,
            ext: Lock::new(ext),
            ext_layout: Lock::new(ext_layout),
            direct_default_initargs: default_initarg_array(ctx, init.direct_default_initargs),
            default_initargs: default_initarg_array(ctx, init.default_initargs),
            prototype: Lock::new(Value::empty()),
            dependents: Lock::new(Value::null()),
        }
    }

    /// The default initargs this class names itself: flat, the keyword then
    /// its thunk for each. The thunks are called, lazily, by `make-instance`
    /// for each keyword the caller does not supply.
    pub fn direct_default_initargs(&self) -> &[Value<'gc>] {
        self.direct_default_initargs.as_slice()
    }

    /// The default initargs `make-instance` supplies, flat like
    /// [`Self::direct_default_initargs`]: the class's own first, then those
    /// of each superclass in precedence order, a key once. Empty when no
    /// class in the precedence list names any, which is what the fast
    /// instantiation paths test.
    #[inline(always)]
    pub fn default_initargs(&self) -> &[Value<'gc>] {
        self.default_initargs.as_slice()
    }

    /// The AMOP dependents, see the field.
    pub fn dependents(&self) -> Value<'gc> {
        self.dependents.get()
    }

    /// Replace the dependents list, see the field.
    pub fn set_dependents(ctx: MutationContext<'gc>, class: Gc<'gc, Self>, dependents: Value<'gc>) {
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, dependents)
            .unlock()
            .set(dependents);
    }

    /// The class prototype: an instance of this class with every slot unbound.
    /// Allocated on the first request, and that same object returned from
    /// then on. `initialize` does not run on it.
    pub(crate) fn prototype(ctx: MutationContext<'gc>, class: Gc<'gc, Self>) -> Value<'gc> {
        let existing = class.prototype.get();
        if !existing.is_empty() {
            return existing;
        }
        let instance: Value<'gc> = SchemeInstance::allocate_unbound(ctx, class).into();
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, prototype)
            .unlock()
            .set(instance);
        instance
    }

    /// The layout stamp, see the field.
    #[inline(always)]
    pub fn layout_stamp(&self) -> u32 {
        self.layout_stamp
    }

    /// The native layout family of this class's instances, see the field.
    #[inline(always)]
    pub fn layout_family(&self) -> LayoutFamily {
        self.layout_family
    }

    /// Whether instances use the stock `SchemeInstance` allocation hook and
    /// layout, so `make-instance` with no initargs is exactly "allocate the
    /// class header and a slot storage of the descriptor's slots".
    #[inline(always)]
    pub fn has_plain_scheme_allocation(&self) -> bool {
        self.plain_scheme_allocation
    }

    /// The watchpoint set that fires when a redefinition replaces this
    /// descriptor, see the field. `None` when inline instance allocation was
    /// off when the descriptor was made.
    pub fn redefinition_wp(&self) -> Gc<'gc, WatchpointSet<'gc>> {
        self.redefinition_wp.get()
    }

    /// The current slot-protocol set. A cache fill reads it before its
    /// lookup and registers on it with [`WatchpointSet::add`].
    pub fn slot_protocol_wp(&self) -> Gc<'gc, WatchpointSet<'gc>> {
        self.slot_protocol_wp.get()
    }

    /// The slot protocol of `class` may have changed: install a fresh set
    /// and fire the old one, clearing every cache that depended on it.
    /// Callers mutate the protocol first and renew after, so a filler that
    /// reads the fresh set already sees the change. Returns the number of
    /// dependents cleared. The caller owes the grace period.
    pub fn renew_slot_protocol_wp(ctx: MutationContext<'gc>, class: Gc<'gc, Self>) -> usize {
        let old = class.slot_protocol_wp.get();
        let fresh = WatchpointSet::new(ctx);
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, slot_protocol_wp)
            .unlock()
            .set(fresh);
        let cleared = WatchpointSet::fire(old, ctx);
        // After the fire: a stale computation that publishes later rechecks
        // the (now invalid) set it read and resets its own state.
        for state in &class.protocol_state {
            state.store(PROTOCOL_UNKNOWN, std::sync::atomic::Ordering::Release);
        }
        cleared
    }

    /// The slot definition object for slot `name`, made once per descriptor.
    pub(crate) fn slot_definition_for(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        name: &str,
        key: Value<'gc>,
    ) -> Option<Value<'gc>> {
        let mut entries = class.slot_definitions.get();
        while entries.is_pair() {
            let entry = entries.car();
            if entry.car().bits() == key.bits() {
                return Some(entry.cdr());
            }
            entries = entries.cdr();
        }
        let accessor = class.accessor_named(name)?;
        let slot = *class.slots().get(accessor.slot_index())?;
        let definition: Value<'gc> =
            super::slot::SlotDefinitionDescriptor::from_slot(ctx, slot).into();
        let entry = Value::cons(ctx, key, definition);
        let list = Value::cons(ctx, entry, class.slot_definitions.get());
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, slot_definitions)
            .unlock()
            .set(list);
        Some(definition)
    }

    /// The computed protocol state of operation `op`, `PROTOCOL_*`.
    #[inline]
    pub(crate) fn protocol_state(&self, op: usize) -> u8 {
        self.protocol_state[op].load(std::sync::atomic::Ordering::Acquire)
    }

    /// Publish a computed state if none is yet, returning the state now
    /// held.
    pub(crate) fn publish_protocol_state(&self, op: usize, state: u8) -> u8 {
        use std::sync::atomic::Ordering::{AcqRel, Acquire};
        match self.protocol_state[op].compare_exchange(PROTOCOL_UNKNOWN, state, AcqRel, Acquire) {
            Ok(_) => state,
            Err(current) => current,
        }
    }

    /// Drop a published state (a computation found its set invalid).
    pub(crate) fn reset_protocol_state(&self, op: usize) {
        self.protocol_state[op].store(PROTOCOL_UNKNOWN, std::sync::atomic::Ordering::Release);
    }

    pub fn id(&self) -> ClassId {
        self.id
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn category(&self) -> ClassCategory {
        self.category
    }

    pub fn flags(&self) -> ClassFlags {
        self.flags.get()
    }

    /// Seal the class: no redefinition and no change-class. Unless
    /// `allow_subclasses`, the class is also final (no subclass may be
    /// defined). Builtin classes stay subclassable: their flags already
    /// mark them sealed, and their metaclass protocol needs subclasses.
    pub fn seal(&self, allow_subclasses: bool) {
        let mut flags = self.flags().with_sealed().without_malleable();
        if !allow_subclasses && self.category == ClassCategory::Scheme {
            flags = flags.with_no_subclasses();
        }
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.flags.as_cell().set(flags) };
    }

    /// Unseal a Scheme class, making it malleable and subclassable again.
    /// `false` for a builtin class.
    pub fn unseal(class: Gc<'gc, Self>) -> bool {
        if class.category != ClassCategory::Scheme {
            return false;
        }
        let flags = class
            .flags()
            .without_sealed()
            .without_no_subclasses()
            .with_malleable();
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { class.flags.as_cell().set(flags) };
        true
    }

    /// The subclass watchpoint: see [`Self::renew_subclass_wp`].
    pub fn subclass_wp(&self) -> Gc<'gc, WatchpointSet<'gc>> {
        self.subclass_wp.get()
    }

    /// The number of changes to the direct subclasses of this class.
    pub fn subclass_stamp(&self) -> u32 {
        self.subclass_stamp
            .load(std::sync::atomic::Ordering::Acquire)
    }

    /// The direct subclasses of `class` changed: bump the stamp, install a
    /// fresh subclass set and fire the old one. Returns the number of
    /// dependents cleared. The caller owes the grace period.
    pub fn renew_subclass_wp(ctx: MutationContext<'gc>, class: Gc<'gc, Self>) -> usize {
        class
            .subclass_stamp
            .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        let old = class.subclass_wp.get();
        let fresh = WatchpointSet::new(ctx);
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, subclass_wp)
            .unlock()
            .set(fresh);
        WatchpointSet::fire(old, ctx)
    }

    pub fn primitive_layout_hooks(&self) -> Option<PrimitiveLayoutHooks> {
        self.primitive_layout_hooks
    }

    pub fn primitive_operation_hooks(&self) -> PrimitiveOperationHooks {
        self.primitive_operation_hooks
    }

    pub fn allocate_instance(
        &self,
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Option<Result<Value<'gc>, super::slot::SlotInitError<'gc>>> {
        self.primitive_operation_hooks
            .allocate()
            .map(|allocate| allocate(ctx, class, initargs))
    }

    /// Whether this class is unfinalized: a forward-reference placeholder or
    /// a class under one. Such a class has no instances (see `ClassFlags`).
    pub fn unfinalized(&self) -> bool {
        self.flags().unfinalized()
    }

    /// The id of this class's metaclass, the class `class-of` returns.
    pub fn metaclass_id(&self) -> ClassId {
        self.metaclass_id
    }

    /// Give `class` the extension storage of `from`, the descriptor it
    /// replaces. Its stamp is checked again on the next access.
    pub(crate) fn adopt_extension_storage(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        from: Gc<'gc, Self>,
    ) {
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, ext)
            .unlock()
            .set(from.ext.get());
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, ext_layout)
            .unlock()
            .set(from.ext_layout.get());
    }

    /// The metaclass as it is in the class table now: a redefined metaclass
    /// has a new descriptor under the same id.
    pub(crate) fn current_metaclass(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
    ) -> Option<Gc<'gc, Self>> {
        class_table(ctx).lookup(class.metaclass_id)
    }

    /// The extension storage of `class` for its metaclass `metaclass`. A
    /// storage made for an older layout stamp of the metaclass (it was
    /// redefined) is replaced by one whose slots keep their values by name
    /// where the old layout had the slot. Returns `None` when the metaclass
    /// has no slots.
    pub(crate) fn extension_storage(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        metaclass: Gc<'gc, Self>,
    ) -> Option<Gc<'gc, SlotStorage<'gc>>> {
        if metaclass.slot_count() == 0 {
            return None;
        }
        if let Some(storage) = class.ext.get()
            && storage.stamp() == metaclass.layout_stamp()
            && storage.len() == metaclass.slot_count()
        {
            return Some(storage);
        }
        let previous = class.ext_layout.get().zip(class.ext.get());
        let storage = extension_storage_for(ctx, metaclass, previous);
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, ext)
            .unlock()
            .set(Some(storage));
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, ext_layout)
            .unlock()
            .set(Some(metaclass));
        Some(storage)
    }

    /// Read extension slot `name` of `class` (a slot of its metaclass).
    /// Class-allocated slots of the metaclass live on the metaclass itself.
    pub(crate) fn extension_slot_ref(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        name: &str,
    ) -> Result<Value<'gc>, SlotAccessError> {
        let metaclass = Self::current_metaclass(ctx, class).ok_or(SlotAccessError::Unknown)?;
        let accessor = metaclass
            .accessor_named(name)
            .ok_or(SlotAccessError::Unknown)?;
        if accessor.class_allocated() {
            return metaclass.read_class_slot(accessor);
        }
        let storage =
            Self::extension_storage(ctx, class, metaclass).ok_or(SlotAccessError::OutOfBounds)?;
        match storage.get(accessor.slot_index()) {
            Some(value) if !value.is_empty() => Ok(value),
            Some(_) => Err(SlotAccessError::Unbound),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    /// Write extension slot `name` of `class`, see [`Self::extension_slot_ref`].
    pub(crate) fn extension_slot_set(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        name: &str,
        value: Value<'gc>,
    ) -> Result<(), SlotAccessError> {
        let metaclass = Self::current_metaclass(ctx, class).ok_or(SlotAccessError::Unknown)?;
        let accessor = metaclass
            .accessor_named(name)
            .ok_or(SlotAccessError::Unknown)?;
        if accessor.class_allocated() {
            return Self::set_class_slot(ctx, metaclass, accessor, value);
        }
        if !accessor.flags().settable() {
            return Err(SlotAccessError::Immutable);
        }
        let storage =
            Self::extension_storage(ctx, class, metaclass).ok_or(SlotAccessError::OutOfBounds)?;
        if accessor.slot_index() >= storage.len() {
            return Err(SlotAccessError::OutOfBounds);
        }
        Gc::write(ctx, storage)[accessor.slot_index()]
            .unlock()
            .set(value);
        Ok(())
    }

    /// Whether extension slot `name` of `class` is bound.
    pub(crate) fn extension_slot_bound(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, Self>,
        name: &str,
    ) -> Result<bool, SlotAccessError> {
        let metaclass = Self::current_metaclass(ctx, class).ok_or(SlotAccessError::Unknown)?;
        let accessor = metaclass
            .accessor_named(name)
            .ok_or(SlotAccessError::Unknown)?;
        if accessor.class_allocated() {
            return metaclass.class_slot_bound(accessor);
        }
        let storage =
            Self::extension_storage(ctx, class, metaclass).ok_or(SlotAccessError::OutOfBounds)?;
        match storage.get(accessor.slot_index()) {
            Some(value) => Ok(!value.is_empty()),
            None => Err(SlotAccessError::OutOfBounds),
        }
    }

    pub fn slot_value_by_name(
        &self,
        ctx: MutationContext<'gc>,
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
        ctx: MutationContext<'gc>,
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
        ctx: MutationContext<'gc>,
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
            .find(|slot| !slot.class_allocated() && slot.accepts_init_keyword(keyword))
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
        ctx: MutationContext<'gc>,
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
        Gc::write(ctx, class.class_slots)[accessor.slot_index()]
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
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
        let current = class.direct_subclasses.get();
        // A redefinition links a new descriptor under the same id: the stale
        // weak entry is replaced, not kept, so the walk sees the live class.
        let stale = current.iter().position(|weak| {
            weak.upgrade(ctx)
                .is_some_and(|existing| existing.id() == subclass.id())
        });
        if let Some(index) = stale {
            let live = current[index]
                .upgrade(ctx)
                .is_some_and(|existing| existing.as_ptr() == subclass.as_ptr());
            if live {
                return;
            }
        }
        let appended = Array::new(
            ctx,
            current.len() + usize::from(stale.is_none()),
            |_, index| match stale {
                Some(stale) if index == stale => Gc::downgrade(subclass),
                _ if index < current.len() => current[index],
                _ => Gc::downgrade(subclass),
            },
        );
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(appended);
        Self::renew_subclass_wp(ctx, class);
    }

    pub(crate) fn remove_direct_subclass(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        subclass_id: ClassId,
    ) {
        let current = class.direct_subclasses.get();
        let filtered = current
            .iter()
            .copied()
            .filter(|weak| {
                weak.upgrade(ctx)
                    .is_none_or(|existing| existing.id() != subclass_id)
            })
            .collect::<Vec<_>>();
        if filtered.len() == current.len() {
            return;
        }
        let filtered = Array::from_slice(ctx, &filtered);
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, direct_subclasses)
            .unlock()
            .set(filtered);
        Self::renew_subclass_wp(ctx, class);
    }

    pub(crate) fn add_direct_method(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        method: Value<'gc>,
    ) {
        let current = class.direct_methods.get();
        if current.contains(&method) {
            return;
        }
        let appended = Array::new(ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, direct_methods)
            .unlock()
            .set(appended);
    }

    /// Forget `method` as one that names `class` (the counterpart of
    /// [`Self::add_direct_method`]).
    pub(crate) fn remove_direct_method(
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        method: Value<'gc>,
    ) {
        let current = class.direct_methods.get();
        if !current.contains(&method) {
            return;
        }
        let kept: Vec<Value<'gc>> = current.iter().copied().filter(|m| *m != method).collect();
        barrier::field!(Gc::write(ctx, class), ClassDescriptor, direct_methods)
            .unlock()
            .set(Array::from_slice(ctx, &kept));
    }
}

/// Flat keyword and thunk array of default initargs, the shared empty array
/// when there are none.
fn default_initarg_array<'gc>(
    ctx: MutationContext<'gc>,
    pairs: &[(Value<'gc>, Value<'gc>)],
) -> ArrayRef<'gc, Value<'gc>> {
    if pairs.is_empty() {
        return empty_metadata(ctx);
    }
    let flat = pairs
        .iter()
        .flat_map(|&(key, thunk)| [key, thunk])
        .collect::<Vec<_>>();
    Array::from_slice(ctx, &flat)
}

/// The (keyword, thunk) pairs of a flat default initarg array.
pub(crate) fn default_initarg_pairs<'gc>(flat: &[Value<'gc>]) -> Vec<(Value<'gc>, Value<'gc>)> {
    flat.as_chunks::<2>().0.iter()
        .map(|pair| (pair[0], pair[1]))
        .collect()
}

/// The value an initarg is compared by: the symbol of a keyword, so that two
/// keyword objects naming the same keyword match. Other values are their own
/// key.
pub(crate) fn initarg_key<'gc>(key: Value<'gc>) -> Value<'gc> {
    if key.is::<Keyword>() {
        key.downcast::<Keyword>().to_symbol().into()
    } else {
        key
    }
}

fn empty_metadata<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, Value<'gc>> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length metadata arrays are never initialized")
    })
}

fn initarg_metadata<'gc>(
    ctx: MutationContext<'gc>,
    slots: &[SlotDescriptor<'gc>],
) -> ArrayRef<'gc, Value<'gc>> {
    let initargs = slots
        .iter()
        .filter(|slot| !slot.class_allocated() && slot.flags().initializable())
        .flat_map(|slot| slot.init_keywords())
        .collect::<Vec<_>>();
    Array::from_slice(ctx, &initargs)
}

pub(crate) fn class_id_list_to_class_objects<'gc>(
    ctx: MutationContext<'gc>,
    ids: &[ClassId],
) -> Value<'gc> {
    let classes = ids
        .iter()
        .filter_map(|id| super::table::class_table(ctx).lookup(*id).map(Value::from))
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &classes)
}

pub(crate) fn class_id_to_class_object<'gc>(ctx: MutationContext<'gc>, id: ClassId) -> Value<'gc> {
    super::table::class_table(ctx)
        .lookup(id)
        .map(Value::from)
        .unwrap_or_else(|| Value::new(false))
}

pub(crate) fn slot_definition_list<'gc>(
    ctx: MutationContext<'gc>,
    slots: &[SlotDescriptor<'gc>],
) -> Value<'gc> {
    let definitions = slots
        .iter()
        .map(|slot| super::slot::SlotDefinitionDescriptor::from_slot(ctx, *slot).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

pub(crate) fn slot_accessor_list<'gc>(
    ctx: MutationContext<'gc>,
    accessors: &[SlotAccessorDescriptor<'gc>],
) -> Value<'gc> {
    let definitions = accessors
        .iter()
        .map(|accessor| super::slot::SlotAccessorDefinition::from_accessor(ctx, *accessor).into())
        .collect::<Vec<_>>();
    Value::list_from_slice(ctx, &definitions)
}

fn empty_weak_subclasses<'gc>(
    ctx: MutationContext<'gc>,
) -> ArrayRef<'gc, Weak<'gc, ClassDescriptor<'gc>>> {
    Array::new(ctx, 0, |_, _| {
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
    ctx: MutationContext<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<Value<'gc>, SlotAccessError> {
    match crate::runtime::sni::call_function(ctx, proc, [receiver]) {
        crate::runtime::objectmodel::ExecutionResult::Ok(value) => Ok(value),
        crate::runtime::objectmodel::ExecutionResult::Err(_) => {
            Err(SlotAccessError::SchemeHookFailed)
        }
    }
}

pub(crate) fn call_scheme_slot_set<'gc>(
    ctx: MutationContext<'gc>,
    receiver: Value<'gc>,
    value: Value<'gc>,
    proc: Value<'gc>,
) -> Result<(), SlotAccessError> {
    match crate::runtime::sni::call_function(ctx, proc, [receiver, value]) {
        crate::runtime::objectmodel::ExecutionResult::Ok(_) => Ok(()),
        crate::runtime::objectmodel::ExecutionResult::Err(_) => {
            Err(SlotAccessError::SchemeHookFailed)
        }
    }
}

pub(crate) fn call_scheme_slot_bound<'gc>(
    ctx: MutationContext<'gc>,
    receiver: Value<'gc>,
    proc: Value<'gc>,
) -> Result<bool, SlotAccessError> {
    match crate::runtime::sni::call_function(ctx, proc, [receiver]) {
        crate::runtime::objectmodel::ExecutionResult::Ok(value) => Ok(value != Value::new(false)),
        crate::runtime::objectmodel::ExecutionResult::Err(_) => {
            Err(SlotAccessError::SchemeHookFailed)
        }
    }
}
