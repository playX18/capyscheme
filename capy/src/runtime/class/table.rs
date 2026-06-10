use std::sync::OnceLock;

use super::builtin::{builtin_class_specs, builtin_id, builtin_primitive_layout_hooks};
use super::descriptor::{ClassDescriptor, ClassDescriptorInit};
use super::flags::{ClassCategory, ClassFlags};
use super::hooks::PrimitiveLayoutHooks;
use super::parse::{ParsedSlotSpec, parse_scheme_class_shape};
use super::slot::{SlotDescriptor, SlotSpec};
use crate::Rootable;
use crate::rsgc::alloc::{Array, ArrayRef};
use crate::rsgc::cell::Lock;
use crate::rsgc::object::{
    AllocationHooks, ClassId, MAX_CLASS_ID, allocate_class_id, builtin_class_ids,
    class_header_word, drain_pending_type_classes, pending_hooks_for_class_id,
};
use crate::rsgc::sync::monitor::Monitor;
use crate::rsgc::{Gc, Global as GcGlobal, Trace, Visitor, WeakProcessor};
use crate::runtime::Context;
use crate::runtime::value::Value;

#[derive(Debug, PartialEq, Eq)]
pub enum ClassTableError {
    DuplicateId(ClassId),
    Exhausted,
    IdOutOfRange,
    MissingId(ClassId),
    NotMalleable(ClassId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SchemeClassSpecError<'gc> {
    SlotsNotList(Value<'gc>),
    SlotNameNotSymbol(Value<'gc>),
    SlotSpecNotList(Value<'gc>),
    SlotOptionNameNotKeyword(Value<'gc>),
    SlotOptionMissingValue(Value<'gc>),
    UnknownSlotOption(Value<'gc>),
    SupersNotList(Value<'gc>),
    SuperNotClass(Value<'gc>),
    MissingProcedureSlot(Value<'gc>),
    ClassTable(ClassTableError),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ClassListError<'gc> {
    NotList(Value<'gc>),
    NotClass(Value<'gc>),
}

#[derive(Clone, Copy, Default)]
enum ClassSlot<'gc> {
    #[default]
    Empty,
    Class(Gc<'gc, ClassDescriptor<'gc>>),
}

// SAFETY: GC trace for `ClassSlot` — all reachable heap fields are visited
unsafe impl Trace for ClassSlot<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        if let Self::Class(class) = self {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe { class.trace(visitor) };
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        if let Self::Class(class) = self {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe { class.process_weak_refs(weak_processor) };
        }
    }
}

#[derive(Clone, Copy)]
struct ClassPage<'gc> {
    slots: ArrayRef<'gc, Lock<ClassSlot<'gc>>>,
}

impl<'gc> ClassPage<'gc> {
    fn new(ctx: Context<'gc>) -> Self {
        Self {
            slots: Array::with(*ctx, super::builtin::CLASS_TABLE_PAGE_SIZE, |_, _| {
                Lock::new(ClassSlot::Empty)
            }),
        }
    }
}

// SAFETY: GC trace for `ClassPage` — all reachable heap fields are visited
unsafe impl Trace for ClassPage<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.slots.trace(visitor) };
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.slots.process_weak_refs(weak_processor) };
    }
}

pub(crate) struct ClassTableInner<'gc> {
    pages: ArrayRef<'gc, Lock<Option<ClassPage<'gc>>>>,
    pub(crate) max_id: u32,
}

#[derive(Default)]
struct ClassRedefinitionState {
    epoch: u64,
}

// SAFETY: GC trace for `ClassTableInner` — all reachable heap fields are visited
unsafe impl Trace for ClassTableInner<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.pages.trace(visitor) };
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.pages.process_weak_refs(weak_processor) };
    }
}

pub struct ClassTable<'gc> {
    pub(crate) inner: Monitor<ClassTableInner<'gc>>,
    redefinition: Monitor<ClassRedefinitionState>,
}

pub(crate) struct DynamicClassOptions {
    pub(crate) primitive_operation_hooks: Option<super::hooks::PrimitiveOperationHooks>,
    pub(crate) flags: Option<ClassFlags>,
}

struct DynamicClassRedefinition<'a> {
    id: ClassId,
    name: &'a str,
    category: ClassCategory,
}

// SAFETY: GC trace for `ClassTable` — all reachable heap fields are visited
unsafe impl Trace for ClassTable<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.inner.get_mut().trace(visitor) };
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { self.inner.get_mut().process_weak_refs(weak_processor) };
    }
}

// SAFETY: `ClassTable` is `Send` because all mutable state is synchronized
unsafe impl Send for ClassTable<'_> {}
// SAFETY: `ClassTable` is `Sync` because all mutable access is serialized
unsafe impl Sync for ClassTable<'_> {}

type RootedClassTable = Rootable!(ClassTable<'_>);

pub(crate) static CLASS_TABLE: OnceLock<GcGlobal<RootedClassTable>> = OnceLock::new();

pub(crate) fn class_table_initialized() -> bool {
    CLASS_TABLE.get().is_some()
}

pub(crate) fn is_type_class_registered(id: ClassId) -> bool {
    let Some(table) = CLASS_TABLE.get() else {
        return false;
    };
    // SAFETY: The table is guaranteed to be initialized before this point
    unsafe { table.fetch_unchecked().lookup(id).is_some() }
}

pub fn init_builtin_classes<'gc>(ctx: Context<'gc>) -> &'gc ClassTable<'gc> {
    CLASS_TABLE
        .get_or_init(|| {
            let table: GcGlobal<RootedClassTable> = GcGlobal::new(ClassTable::new(ctx));
            let rooted_table = table.fetch(*ctx);
            for spec in builtin_class_specs() {
                rooted_table
                    .register_builtin(ctx, spec.descriptor(ctx))
                    .expect("built-in class IDs must be unique");
            }
            register_pending_type_classes(ctx, rooted_table);
            table
        })
        .fetch(*ctx)
}

pub fn class_table<'gc>(ctx: Context<'gc>) -> &'gc ClassTable<'gc> {
    init_builtin_classes(ctx)
}

pub(crate) fn register_internal_type_class_if_ready<'gc>(
    ctx: Context<'gc>,
    id: ClassId,
    hooks: AllocationHooks,
) {
    let Some(table) = CLASS_TABLE.get() else {
        return;
    };
    // SAFETY: The table is guaranteed to be initialized before this point
    let table = unsafe { table.fetch_unchecked() };
    register_internal_type_class(ctx, table, id, hooks);
}

pub(crate) fn register_pending_type_classes<'gc>(ctx: Context<'gc>, table: &ClassTable<'gc>) {
    use crate::rsgc::object::{TypeClassRegistrationGuard, record_pending_type_class};
    loop {
        let pending = drain_pending_type_classes();
        if pending.is_empty() {
            break;
        }
        for (id, hooks) in pending {
            if table.lookup(id).is_some() {
                continue;
            }
            if let Some(_guard) = TypeClassRegistrationGuard::enter() {
                register_internal_type_class(ctx, table, id, hooks);
            } else {
                record_pending_type_class(id, hooks);
            }
        }
    }
}

pub(crate) fn register_internal_type_class<'gc>(
    ctx: Context<'gc>,
    table: &ClassTable<'gc>,
    id: ClassId,
    hooks: AllocationHooks,
) {
    if table.lookup(id).is_some() {
        return;
    }
    let layout_hooks = PrimitiveLayoutHooks::from_allocation_hooks(hooks);
    table
        .register_internal_type_class(ctx, id, hooks.type_name, layout_hooks)
        .expect("type class IDs must be unique while registering into class table");
}

pub fn primitive_layout_hooks_for_class_id(id: ClassId) -> Option<PrimitiveLayoutHooks> {
    if let Some(table) = CLASS_TABLE.get() {
        // SAFETY: The table is guaranteed to be initialized before this point
        return unsafe { table.fetch_unchecked() }
            .lookup(id)
            .and_then(|descriptor| descriptor.primitive_layout_hooks());
    }
    if let Some(hooks) = builtin_primitive_layout_hooks(id) {
        return Some(hooks);
    }
    pending_hooks_for_class_id(id).map(PrimitiveLayoutHooks::from_allocation_hooks)
}

fn normalized_dynamic_direct_supers(
    category: ClassCategory,
    direct_supers: &[ClassId],
) -> Vec<ClassId> {
    let mut normalized = direct_supers.to_vec();
    if category == ClassCategory::Scheme {
        let object = builtin_id(builtin_class_ids::OBJECT);
        if !normalized.contains(&object) {
            normalized.push(object);
        }
    }
    normalized
}

fn push_unique_class_id(ids: &mut Vec<ClassId>, id: ClassId) {
    if !ids.contains(&id) {
        ids.push(id);
    }
}

fn collect_dynamic_hierarchy<'gc>(
    inner: &ClassTableInner<'gc>,
    id: ClassId,
    direct_supers: &[ClassId],
) -> (Vec<SlotDescriptor<'gc>>, Vec<ClassId>) {
    let mut inherited_slots = Vec::new();
    let mut cpl = vec![id];
    for super_id in direct_supers {
        if let Some(superclass) = inner.lookup(*super_id) {
            inherited_slots.extend_from_slice(superclass.slots());
            for class_id in superclass.cpl() {
                push_unique_class_id(&mut cpl, *class_id);
            }
        } else {
            push_unique_class_id(&mut cpl, *super_id);
        }
    }
    (inherited_slots, cpl)
}

impl<'gc> ClassTable<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self::with_max_id(ctx, MAX_CLASS_ID)
    }

    pub(crate) fn with_max_id(ctx: Context<'gc>, max_id: u32) -> Self {
        let page_count = (max_id >> super::builtin::CLASS_TABLE_PAGE_BITS) as usize + 1;
        let pages = Array::with(*ctx, page_count, |_, _| Lock::new(None));
        Self {
            inner: Monitor::new(ClassTableInner { pages, max_id }),
            redefinition: Monitor::new(ClassRedefinitionState::default()),
        }
    }

    #[cfg(test)]
    pub(crate) fn redefinition_epoch(&self) -> u64 {
        self.redefinition.lock().epoch
    }

    pub fn register_builtin(
        &self,
        ctx: Context<'gc>,
        descriptor: ClassDescriptor<'gc>,
    ) -> Result<(), ClassTableError> {
        let descriptor = Gc::new_with_header_word(
            *ctx,
            descriptor,
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        {
            let mut inner = self.inner.lock();
            inner.register_builtin(ctx, descriptor)?;
        }
        self.link_direct_subclasses(ctx, descriptor);
        Ok(())
    }

    pub(crate) fn register_internal_type_class(
        &self,
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        hooks: PrimitiveLayoutHooks,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        if let Some(existing) = self.lookup(id) {
            return Ok(existing);
        }
        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id,
                    name,
                    category: ClassCategory::Internal,
                    primitive_layout_hooks: Some(hooks),
                    primitive_operation_hooks: Some(
                        super::hooks::PrimitiveOperationHooks::default(),
                    ),
                    flags: None,
                    direct_supers: &[],
                    cpl: &[id],
                    inherited_slots: &[],
                    direct_slot_specs: &[],
                    direct_subclasses: None,
                    direct_methods: None,
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        if let Some(existing) = inner.lookup(id) {
            return Ok(existing);
        }
        inner.register(ctx, descriptor)?;
        drop(inner);
        self.link_direct_subclasses(ctx, descriptor);
        Ok(descriptor)
    }

    pub fn register_dynamic(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots(ctx, name, category, &[], &[])
    }

    pub fn register_dynamic_with_slots(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots_and_hooks(
            ctx,
            name,
            category,
            DynamicClassOptions {
                primitive_operation_hooks: None,
                flags: None,
            },
            direct_supers,
            direct_slots,
        )
    }

    fn register_dynamic_with_slots_and_hooks(
        &self,
        ctx: Context<'gc>,
        name: &str,
        category: ClassCategory,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let direct_supers = normalized_dynamic_direct_supers(category, direct_supers);
        let (id, inherited_slots, cpl) = {
            let mut inner = self.inner.lock();
            let id = inner.next_free_dynamic_id()?;
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, id, &direct_supers);
            (id, inherited_slots, cpl)
        };
        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id,
                    name,
                    category,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: options.primitive_operation_hooks,
                    flags: options.flags,
                    direct_supers: &direct_supers,
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: None,
                    direct_methods: None,
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        inner.register_dynamic(ctx, descriptor)?;
        drop(inner);
        self.link_direct_subclasses(ctx, descriptor);
        Ok(descriptor)
    }

    pub fn redefine_dynamic_with_slots(
        &self,
        ctx: Context<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition { id, name, category },
            DynamicClassOptions {
                primitive_operation_hooks: None,
                flags: None,
            },
            direct_supers,
            direct_slots,
        )
    }

    fn redefine_dynamic_with_slots_and_hooks(
        &self,
        ctx: Context<'gc>,
        spec: DynamicClassRedefinition<'_>,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let mut redefinition = self.redefinition.lock();
        let direct_supers = normalized_dynamic_direct_supers(spec.category, direct_supers);
        let (old, inherited_slots, cpl, direct_subclasses, direct_methods, removed_direct_supers) = {
            let inner = self.inner.lock();
            let old = inner
                .lookup(spec.id)
                .ok_or(ClassTableError::MissingId(spec.id))?;
            if !old.flags().malleable() {
                return Err(ClassTableError::NotMalleable(spec.id));
            }
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, spec.id, &direct_supers);
            (
                old,
                inherited_slots,
                cpl,
                old.direct_subclasses(),
                old.direct_methods(),
                old.direct_supers()
                    .iter()
                    .filter(|old_super_id| !direct_supers.contains(old_super_id))
                    .filter_map(|old_super_id| inner.lookup(*old_super_id))
                    .collect::<Vec<_>>(),
            )
        };
        let descriptor = Gc::new_with_header_word(
            *ctx,
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id: spec.id,
                    name: spec.name,
                    category: spec.category,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: options.primitive_operation_hooks,
                    flags: options.flags,
                    direct_supers: &direct_supers,
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: Some(direct_subclasses),
                    direct_methods: Some(direct_methods),
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let mut inner = self.inner.lock();
        let descriptor = inner.redefine_dynamic(ctx, descriptor)?;
        drop(inner);
        for old_super in removed_direct_supers {
            ClassDescriptor::remove_direct_subclass(ctx, old_super, spec.id);
        }
        ClassDescriptor::invalidate_direct_method_caches(ctx, old);
        self.link_direct_subclasses(ctx, descriptor);
        ClassDescriptor::invalidate_direct_method_caches(ctx, descriptor);
        redefinition.epoch += 1;
        Ok(descriptor)
    }

    pub fn register_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();
        self.register_dynamic_with_slots(
            ctx,
            &name,
            ClassCategory::Scheme,
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn register_invocable_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        if !parsed_slots.iter().any(|slot| slot.name == "procedure") {
            return Err(SchemeClassSpecError::MissingProcedureSlot(
                ctx.intern(&name),
            ));
        }
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();
        self.register_dynamic_with_slots_and_hooks(
            ctx,
            &name,
            ClassCategory::Scheme,
            super::instance::invocable_scheme_class_options(),
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn redefine_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();
        self.redefine_dynamic_with_slots(
            ctx,
            class.id(),
            &name,
            ClassCategory::Scheme,
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn redefine_invocable_scheme_class_from_slot_list(
        &self,
        ctx: Context<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::value::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        if !parsed_slots.iter().any(|slot| slot.name == "procedure") {
            return Err(SchemeClassSpecError::MissingProcedureSlot(
                ctx.intern(&name),
            ));
        }
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();
        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition {
                id: class.id(),
                name: &name,
                category: ClassCategory::Scheme,
            },
            super::instance::invocable_scheme_class_options(),
            &direct_supers,
            &direct_slots,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn lookup(&self, id: ClassId) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        self.inner.lock().lookup(id)
    }

    pub fn allocated_page_count(&self) -> usize {
        self.inner.lock().allocated_page_count()
    }

    pub fn is_subclass(&self, class_id: ClassId, superclass_id: ClassId) -> bool {
        self.inner.lock().is_subclass(class_id, superclass_id)
    }

    fn link_direct_subclasses(&self, ctx: Context<'gc>, descriptor: Gc<'gc, ClassDescriptor<'gc>>) {
        let (direct_supers, existing_subclasses) = {
            let inner = self.inner.lock();
            let direct_supers = descriptor
                .direct_supers()
                .iter()
                .filter_map(|super_id| inner.lookup(*super_id))
                .collect::<Vec<_>>();
            let mut existing_subclasses = Vec::new();
            inner.for_each_class(|existing| {
                if existing.id() != descriptor.id()
                    && existing.direct_supers().contains(&descriptor.id())
                {
                    existing_subclasses.push(existing);
                }
            });
            (direct_supers, existing_subclasses)
        };
        for superclass in direct_supers {
            ClassDescriptor::add_direct_subclass(ctx, superclass, descriptor);
        }
        for subclass in existing_subclasses {
            ClassDescriptor::add_direct_subclass(ctx, descriptor, subclass);
        }
    }
}

impl<'gc> ClassTableInner<'gc> {
    fn register_builtin(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn register(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        let (page, slot_index) = self.empty_slot(ctx, descriptor.id())?;
        Gc::write(*ctx, page.slots)[slot_index]
            .unlock()
            .set(ClassSlot::Class(descriptor));
        Ok(())
    }

    fn for_each_class(&self, mut f: impl FnMut(Gc<'gc, ClassDescriptor<'gc>>)) {
        for page in self.pages.iter().filter_map(|page| page.get()) {
            for slot in page.slots.iter() {
                if let ClassSlot::Class(class) = slot.get() {
                    f(class);
                }
            }
        }
    }

    fn register_dynamic(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn redefine_dynamic(
        &mut self,
        ctx: Context<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let old = self
            .lookup(descriptor.id())
            .ok_or(ClassTableError::MissingId(descriptor.id()))?;
        if !old.flags().malleable() {
            return Err(ClassTableError::NotMalleable(descriptor.id()));
        }
        let (page, slot_index) = self.existing_slot(descriptor.id())?;
        Gc::write(*ctx, page.slots)[slot_index]
            .unlock()
            .set(ClassSlot::Class(descriptor));
        Ok(descriptor)
    }

    fn lookup(&self, id: ClassId) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        if id.bits() > self.max_id {
            return None;
        }
        let (page_index, slot_index) = indexes(id);
        let page = self.pages.get(page_index)?.get()?;
        match page.slots[slot_index].get() {
            ClassSlot::Empty => None,
            ClassSlot::Class(descriptor) => Some(descriptor),
        }
    }

    fn allocated_page_count(&self) -> usize {
        self.pages
            .iter()
            .filter(|page| page.get().is_some())
            .count()
    }

    fn is_subclass(&self, class_id: ClassId, superclass_id: ClassId) -> bool {
        self.lookup(class_id)
            .is_some_and(|class| class.is_a(superclass_id))
    }

    fn empty_slot(
        &mut self,
        ctx: Context<'gc>,
        id: ClassId,
    ) -> Result<(ClassPage<'gc>, usize), ClassTableError> {
        if id.bits() > self.max_id {
            return Err(ClassTableError::IdOutOfRange);
        }
        let (page_index, slot_index) = indexes(id);
        let page = match self.pages[page_index].get() {
            Some(page) => page,
            None => {
                let page = ClassPage::new(ctx);
                Gc::write(*ctx, self.pages)[page_index]
                    .unlock()
                    .set(Some(page));
                page
            }
        };
        match page.slots[slot_index].get() {
            ClassSlot::Empty => Ok((page, slot_index)),
            ClassSlot::Class(_) => Err(ClassTableError::DuplicateId(id)),
        }
    }

    fn existing_slot(&self, id: ClassId) -> Result<(ClassPage<'gc>, usize), ClassTableError> {
        if id.bits() > self.max_id {
            return Err(ClassTableError::IdOutOfRange);
        }
        let (page_index, slot_index) = indexes(id);
        let Some(page) = self.pages[page_index].get() else {
            return Err(ClassTableError::MissingId(id));
        };
        match page.slots[slot_index].get() {
            ClassSlot::Empty => Err(ClassTableError::MissingId(id)),
            ClassSlot::Class(_) => Ok((page, slot_index)),
        }
    }

    fn next_free_dynamic_id(&mut self) -> Result<ClassId, ClassTableError> {
        let id = allocate_class_id();
        if id.bits() > self.max_id {
            return Err(ClassTableError::Exhausted);
        }
        if self.lookup(id).is_some() {
            return Err(ClassTableError::DuplicateId(id));
        }
        Ok(id)
    }
}

fn indexes(id: ClassId) -> (usize, usize) {
    let raw = id.bits();
    (
        (raw >> super::builtin::CLASS_TABLE_PAGE_BITS) as usize,
        (raw & super::builtin::CLASS_TABLE_SLOT_MASK) as usize,
    )
}
