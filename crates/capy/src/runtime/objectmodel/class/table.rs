use std::cell::Cell;
use std::collections::HashMap;
use std::sync::OnceLock;

use super::builtin::{builtin_class_specs, builtin_id, builtin_primitive_layout_hooks};
use super::descriptor::{ClassDescriptor, ClassDescriptorInit};
use super::flags::{ClassCategory, ClassFlags, LayoutFamily};
use super::generic::{GenericDescriptor, MethodDescriptor, Reason};
use super::hooks::PrimitiveLayoutHooks;
use super::parse::{ParsedSlotSpec, parse_scheme_class_shape};
use super::slot::{SlotDescriptor, SlotSpec};
use crate::Rootable;
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::Lock;
use crate::heap::object::{
    AllocationHooks, ClassId, MAX_CLASS_ID, allocate_class_id, builtin_class_ids,
    class_header_word, drain_pending_type_classes, pending_hooks_for_class_id,
};
use crate::heap::sync::monitor::ReentrantMonitor;
use crate::heap::{Gc, Global as GcGlobal, Trace, Visitor, WeakProcessor};
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::Value;

#[derive(Debug, PartialEq, Eq)]
pub enum ClassTableError {
    DuplicateId(ClassId),
    Exhausted,
    IdOutOfRange,
    MissingId(ClassId),
    NotMalleable(ClassId),
    /// The requested metaclass is registered but has no `Class` layout family.
    NotMetaclass(ClassId),
    /// A redefinition named a metaclass other than the class's current one.
    MetaclassChanged(ClassId),
    /// A redefinition named a superclass that is the class itself or one of
    /// its subclasses.
    Cycle(ClassId),
    /// A forward-reference resolution named a class that is not a placeholder.
    NotForwardReference(ClassId),
    /// A class names as a direct superclass a sealed class that takes no
    /// subclasses
    SealedSuperclass(ClassId),
    /// No class precedence list exists: the local precedence orders of these
    /// classes (the ones not yet placed) conflict (CLHS 4.3.5).
    InconsistentPrecedence(Vec<ClassId>),
    /// A precedence list given for a class is not a reordering of its computed one: it must hold
    /// the same classes, once each, starting with the class itself.
    BadPrecedenceOverride(ClassId),
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

// SAFETY: GC trace for `ClassSlot`, all reachable heap fields are visited
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
    fn new(ctx: MutationContext<'gc>) -> Self {
        Self {
            slots: Array::new(ctx, super::builtin::CLASS_TABLE_PAGE_SIZE, |_, _| {
                Lock::new(ClassSlot::Empty)
            }),
        }
    }
}

// SAFETY: GC trace for `ClassPage`, all reachable heap fields are visited
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
    /// Class precedence lists chosen by a `compute-class-precedence-list` method, by class id.
    /// Each is a reordering of the list the CLOS algorithm computes; `collect_dynamic_hierarchy`
    /// uses it while that stays true and falls back to the computed list when it does not.
    pub(crate) cpl_overrides: std::cell::RefCell<HashMap<ClassId, Vec<ClassId>>>,
}

#[derive(Default)]
struct ClassRedefinitionState {
    epoch: Cell<u64>,
}

// SAFETY: GC trace for `ClassTableInner`, all reachable heap fields are visited
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
    pub(crate) inner: ReentrantMonitor<ClassTableInner<'gc>>,
    redefinition: ReentrantMonitor<ClassRedefinitionState>,
}

pub(crate) struct DynamicClassOptions {
    pub(crate) primitive_operation_hooks: Option<super::hooks::PrimitiveOperationHooks>,
    pub(crate) flags: Option<ClassFlags>,
    /// The id of the metaclass to register under. `None` is `<class>`.
    pub(crate) metaclass: Option<ClassId>,
}

impl DynamicClassOptions {
    pub(crate) const fn plain() -> Self {
        Self {
            primitive_operation_hooks: None,
            flags: None,
            metaclass: None,
        }
    }
}

struct DynamicClassRedefinition<'a> {
    id: ClassId,
    name: &'a str,
    category: ClassCategory,
}

// SAFETY: GC trace for `ClassTable`, all reachable heap fields are visited
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

/// The layout family of the class registered under `id`, or
/// [`LayoutFamily::None`] when there is none (or the table is not up yet).
///
/// Needs no mutation context: it reads the global table, the same way
/// [`primitive_layout_hooks_for_class_id`] does, and returns a copied value so
/// no GC pointer escapes.
/// Whether the class registered under `id` is a forward-reference placeholder.
/// Like [`registered_layout_family`], reads the global table.
pub(crate) fn registered_is_forward_reference(id: ClassId) -> bool {
    let Some(table) = CLASS_TABLE.get() else {
        return false;
    };
    // SAFETY: The table is guaranteed to be initialized before this point
    unsafe { table.fetch_unchecked() }
        .lookup(id)
        .is_some_and(|descriptor| {
            descriptor.metaclass_id().bits() == builtin_class_ids::FORWARD_REFERENCED_CLASS
        })
}

pub(crate) fn registered_layout_family(id: ClassId) -> LayoutFamily {
    let Some(table) = CLASS_TABLE.get() else {
        return LayoutFamily::None;
    };
    // SAFETY: The table is guaranteed to be initialized before this point
    unsafe { table.fetch_unchecked() }
        .lookup(id)
        .map_or(LayoutFamily::None, |descriptor| descriptor.layout_family())
}

pub fn init_builtin_classes<'gc>(ctx: MutationContext<'gc>) -> &'gc ClassTable<'gc> {
    CLASS_TABLE
        .get_or_init(|| {
            let table: GcGlobal<RootedClassTable> = GcGlobal::new(ClassTable::new(ctx));
            let rooted_table = table.fetch(ctx);
            for spec in builtin_class_specs() {
                rooted_table
                    .register_builtin(ctx, spec.descriptor(ctx))
                    .expect("built-in class IDs must be unique");
            }
            register_pending_type_classes(ctx, rooted_table);
            table
        })
        .fetch(ctx)
}

pub fn class_table<'gc>(ctx: MutationContext<'gc>) -> &'gc ClassTable<'gc> {
    init_builtin_classes(ctx)
}

pub(crate) fn register_internal_type_class_if_ready<'gc>(
    ctx: MutationContext<'gc>,
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

pub(crate) fn register_pending_type_classes<'gc>(
    ctx: MutationContext<'gc>,
    table: &ClassTable<'gc>,
) {
    use crate::heap::object::{TypeClassRegistrationGuard, record_pending_type_class};
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
    ctx: MutationContext<'gc>,
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
    if let Some(hooks) = builtin_primitive_layout_hooks(id) {
        return Some(hooks);
    }
    if let Some(table) = CLASS_TABLE.get() {
        // SAFETY: The table is guaranteed to be initialized before this point
        return unsafe { table.fetch_unchecked() }
            .lookup(id)
            .and_then(|descriptor| descriptor.primitive_layout_hooks());
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

/// The metaclass descriptor for `metaclass` (`<class>` when `None`), which must
/// have the `Class` layout family.
/// Refuse each of `supers` that takes no subclasses (see
/// `ClassFlags::no_subclasses`). A redefinition checks only the supers it
/// adds, so a class that already had a super keeps it when that super is
/// sealed later.
fn check_subclassable<'gc>(
    inner: &ClassTableInner<'gc>,
    supers: &[ClassId],
    existing: &[ClassId],
) -> Result<(), ClassTableError> {
    for id in supers {
        if existing.contains(id) {
            continue;
        }
        let descriptor = inner.lookup(*id).ok_or(ClassTableError::MissingId(*id))?;
        if descriptor.flags().no_subclasses() {
            return Err(ClassTableError::SealedSuperclass(*id));
        }
    }
    Ok(())
}

fn resolve_metaclass<'gc>(
    inner: &ClassTableInner<'gc>,
    metaclass: Option<ClassId>,
) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
    let id = metaclass.unwrap_or_else(|| builtin_id(builtin_class_ids::CLASS));
    let descriptor = inner.lookup(id).ok_or(ClassTableError::MissingId(id))?;
    if descriptor.layout_family() != LayoutFamily::Class {
        return Err(ClassTableError::NotMetaclass(id));
    }
    Ok(descriptor)
}

fn push_unique_class_id(ids: &mut Vec<ClassId>, id: ClassId) {
    if !ids.contains(&id) {
        ids.push(id);
    }
}

/// The precedence list of each superclass in `cpl` (which starts with the class
/// itself), for the slot merging of [`super::slot::compute_effective_slots`].
fn superclass_cpls(inner: &ClassTableInner<'_>, cpl: &[ClassId]) -> Vec<(ClassId, Vec<ClassId>)> {
    cpl.iter()
        .skip(1)
        .filter_map(|&id| inner.lookup(id).map(|class| (id, class.cpl().to_vec())))
        .collect()
}

/// The inherited slots and the class precedence list of class `id` with the
/// given direct supers. Uses the CLOS algorithm unless `CAPY_CLASS_CPL=legacy`.
/// The default initargs of a class with id `id`, own `direct` ones and class
/// precedence list `cpl`: its own first, then each superclass's direct ones
/// in precedence order, a key once (the first one named wins).
fn merged_default_initargs<'gc>(
    inner: &ClassTableInner<'gc>,
    id: ClassId,
    direct: &[(Value<'gc>, Value<'gc>)],
    cpl: &[ClassId],
) -> Vec<(Value<'gc>, Value<'gc>)> {
    use super::descriptor::{default_initarg_pairs, initarg_key};
    let mut merged: Vec<(Value<'gc>, Value<'gc>)> = Vec::new();
    let mut take = |pairs: &[(Value<'gc>, Value<'gc>)]| {
        for &(key, thunk) in pairs {
            if !merged
                .iter()
                .any(|(seen, _)| initarg_key(*seen) == initarg_key(key))
            {
                merged.push((key, thunk));
            }
        }
    };
    take(direct);
    for &super_id in cpl.iter().filter(|&&super_id| super_id != id) {
        if let Some(class) = inner.lookup(super_id) {
            take(&default_initarg_pairs(class.direct_default_initargs()));
        }
    }
    merged
}

/// Whether `chosen` holds exactly the classes of `computed`, once each, starting with the class.
fn is_reordering(chosen: &[ClassId], computed: &[ClassId]) -> bool {
    if chosen.len() != computed.len() || chosen.first() != computed.first() {
        return false;
    }
    let mut left: Vec<ClassId> = chosen.to_vec();
    let mut right: Vec<ClassId> = computed.to_vec();
    left.sort_by_key(|id| id.bits());
    right.sort_by_key(|id| id.bits());
    left == right
}

fn collect_dynamic_hierarchy<'gc>(
    inner: &ClassTableInner<'gc>,
    id: ClassId,
    direct_supers: &[ClassId],
) -> Result<(Vec<SlotDescriptor<'gc>>, Vec<ClassId>), ClassTableError> {
    if crate::utils::flags::class_cpl() == "legacy" {
        return Ok(legacy_hierarchy(inner, id, direct_supers));
    }
    let mut cpl =
        clos_cpl(inner, id, direct_supers).map_err(ClassTableError::InconsistentPrecedence)?;
    if let Some(chosen) = inner.cpl_overrides.borrow().get(&id)
        && is_reordering(chosen, &cpl)
    {
        cpl = chosen.clone();
    }
    let inherited_slots = inherited_slots_for_cpl(inner, &cpl);
    Ok((inherited_slots, cpl))
}

/// The pre-CLOS hierarchy: a depth-first, first-occurrence merge of the supers'
/// precedence lists, and every super's slots appended in order.
fn legacy_hierarchy<'gc>(
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

/// The direct supers of `class` as the precedence algorithm sees them. The
/// pending class (`pending.0`, not yet registered, or registered under the old
/// definition on redefinition) uses the supers it is being given. A registered
/// class uses its own direct supers. A builtin class has its single super
/// recorded there too, so builtin chains are read as local precedence orders
/// unchanged.
fn local_supers(
    inner: &ClassTableInner<'_>,
    class: ClassId,
    pending: (ClassId, &[ClassId]),
) -> Vec<ClassId> {
    if class == pending.0 {
        return pending.1.to_vec();
    }
    match inner.lookup(class) {
        Some(c) if !c.direct_supers().is_empty() => c.direct_supers().to_vec(),
        Some(c) => c.cpl().get(1).copied().into_iter().collect(),
        None => Vec::new(),
    }
}

/// The class precedence list of `class` with the given direct supers, by the
/// CLHS 4.3.5 topological sort. `Err` carries the classes that could not be
/// placed.
///
/// The output starts with `class`. Among the classes with no unplaced
/// predecessor, the one picked is the direct superclass of the rightmost placed
/// class that has such a superclass, which keeps a class next to its subclass
/// where the local orders allow it. The fallback when no placed class has a
/// direct super among the candidates is the earliest discovered candidate;
/// that case is not reached by any valid local ordering I know of, so it only
/// keeps the sort total.
pub(crate) fn clos_cpl(
    inner: &ClassTableInner<'_>,
    class: ClassId,
    direct_supers: &[ClassId],
) -> Result<Vec<ClassId>, Vec<ClassId>> {
    let mut unique_supers = Vec::with_capacity(direct_supers.len());
    for &super_id in direct_supers {
        push_unique_class_id(&mut unique_supers, super_id);
    }
    let pending = (class, unique_supers.as_slice());

    // S_C: the class and all its superclasses, in discovery order.
    let mut order: Vec<ClassId> = Vec::new();
    let mut index: HashMap<ClassId, usize> = HashMap::new();
    let mut stack = vec![class];
    while let Some(c) = stack.pop() {
        if index.contains_key(&c) {
            continue;
        }
        index.insert(c, order.len());
        order.push(c);
        for s in local_supers(inner, c, pending).into_iter().rev() {
            stack.push(s);
        }
    }
    let n = order.len();

    // R: local precedence edges c -> d1 -> d2 -> ... -> dn for each class c.
    let mut preds = vec![0u32; n];
    let mut succs: Vec<Vec<usize>> = vec![Vec::new(); n];
    for &c in &order {
        let mut prev = index[&c];
        for d in local_supers(inner, c, pending) {
            let di = index[&d];
            if !succs[prev].contains(&di) {
                succs[prev].push(di);
                preds[di] += 1;
            }
            prev = di;
        }
    }

    // Topological sort with the CLOS tie-break.
    let mut out: Vec<usize> = Vec::with_capacity(n);
    let mut placed = vec![false; n];
    let mut candidates: Vec<usize> = (0..n).filter(|&i| preds[i] == 0).collect();
    while !candidates.is_empty() {
        let pick = if candidates.len() == 1 {
            0
        } else {
            out.iter()
                .rev()
                .find_map(|&placed_index| {
                    local_supers(inner, order[placed_index], pending)
                        .into_iter()
                        .find_map(|d| candidates.iter().position(|&c| c == index[&d]))
                })
                .unwrap_or(0)
        };
        let node = candidates.remove(pick);
        placed[node] = true;
        out.push(node);
        for &s in &succs[node] {
            preds[s] -= 1;
            if preds[s] == 0 {
                candidates.push(s);
            }
        }
    }

    if out.len() != n {
        return Err((0..n).filter(|&i| !placed[i]).map(|i| order[i]).collect());
    }
    Ok(out.into_iter().map(|i| order[i]).collect())
}

/// Inherited slots for a class with precedence list `cpl` (the class first).
///
/// Slots are taken from the superclasses, most general first (reverse CPL), and
/// a slot is kept once per definition, keyed by `(owner, name)`: a diamond
/// inherits the root's slot through both branches, and it must occupy one
/// position. Each kept slot's index is then its position in the result, since
/// instance storage is indexed by it.
///
/// Single inheritance is unchanged: the reverse CPL of a chain is its
/// ancestors root-first, each contributing its own direct slots, which is the
/// order and the indices the old append-the-supers' slots produced. Slots with
/// the same name but different owners (a subclass shadowing a superclass slot)
/// are both kept, as before.
fn inherited_slots_for_cpl<'gc>(
    inner: &ClassTableInner<'gc>,
    cpl: &[ClassId],
) -> Vec<SlotDescriptor<'gc>> {
    let mut slots: Vec<SlotDescriptor<'gc>> = Vec::new();
    for class_id in cpl[1..].iter().rev() {
        let Some(class) = inner.lookup(*class_id) else {
            continue;
        };
        for slot in class.slots() {
            let duplicate = slots
                .iter()
                .any(|kept| kept.owner() == slot.owner() && kept.name() == slot.name());
            if !duplicate {
                slots.push(*slot);
            }
        }
    }
    for (index, slot) in slots.iter_mut().enumerate() {
        slot.index = index;
    }
    slots
}

impl<'gc> ClassTable<'gc> {
    pub fn new(ctx: MutationContext<'gc>) -> Self {
        Self::with_max_id(ctx, MAX_CLASS_ID)
    }

    pub(crate) fn with_max_id(ctx: MutationContext<'gc>, max_id: u32) -> Self {
        let page_count = (max_id >> super::builtin::CLASS_TABLE_PAGE_BITS) as usize + 1;
        let pages = Array::new(ctx, page_count, |_, _| Lock::new(None));
        Self {
            inner: ReentrantMonitor::new(ClassTableInner {
                pages,
                max_id,
                cpl_overrides: Default::default(),
            }),
            redefinition: ReentrantMonitor::new(ClassRedefinitionState::default()),
        }
    }

    #[cfg(test)]
    pub(crate) fn redefinition_epoch(&self) -> u64 {
        self.redefinition.lock().epoch.get()
    }

    pub fn register_builtin(
        &self,
        ctx: MutationContext<'gc>,
        descriptor: ClassDescriptor<'gc>,
    ) -> Result<(), ClassTableError> {
        let descriptor = ctx.allocate_with(
            descriptor,
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        {
            let inner = self.inner.lock();
            inner.register_builtin(ctx, descriptor)?;
        }
        self.link_direct_subclasses(ctx, descriptor);
        Ok(())
    }

    pub(crate) fn register_internal_type_class(
        &self,
        ctx: MutationContext<'gc>,
        id: ClassId,
        name: &str,
        hooks: PrimitiveLayoutHooks,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        if let Some(existing) = self.lookup(id) {
            return Ok(existing);
        }
        let descriptor = ctx.allocate_with(
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
                    superclass_cpls: &[],
                    direct_slot_specs: &[],
                    direct_subclasses: None,
                    direct_methods: None,
                    metaclass: None,
                    direct_default_initargs: &[],
                    default_initargs: &[],
                },
            ),
            class_header_word(builtin_id(builtin_class_ids::CLASS)),
        );
        let inner = self.inner.lock();
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
        ctx: MutationContext<'gc>,
        name: &str,
        category: ClassCategory,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots(ctx, name, category, &[], &[])
    }

    pub fn register_dynamic_with_slots(
        &self,
        ctx: MutationContext<'gc>,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.register_dynamic_with_slots_and_hooks(
            ctx,
            name,
            category,
            DynamicClassOptions::plain(),
            direct_supers,
            direct_slots,
            &[],
        )
    }

    fn register_dynamic_with_slots_and_hooks(
        &self,
        ctx: MutationContext<'gc>,
        name: &str,
        category: ClassCategory,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
        direct_default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let direct_supers = normalized_dynamic_direct_supers(category, direct_supers);
        let (id, inherited_slots, superclass_cpls, cpl, metaclass, default_initargs) = {
            let inner = self.inner.lock();
            let id = inner.next_free_dynamic_id()?;
            check_subclassable(&inner, &direct_supers, &[])?;
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, id, &direct_supers)?;
            let superclass_cpls = superclass_cpls(&inner, &cpl);
            let metaclass = resolve_metaclass(&inner, options.metaclass)?;
            let default_initargs =
                merged_default_initargs(&inner, id, direct_default_initargs, &cpl);
            (
                id,
                inherited_slots,
                superclass_cpls,
                cpl,
                metaclass,
                default_initargs,
            )
        };
        let descriptor = ctx.allocate_with(
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
                    superclass_cpls: &superclass_cpls,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: None,
                    direct_methods: None,
                    metaclass: Some(metaclass),
                    direct_default_initargs,
                    default_initargs: &default_initargs,
                },
            ),
            class_header_word(metaclass.id()),
        );
        let inner = self.inner.lock();
        inner.register_dynamic(ctx, descriptor)?;
        drop(inner);
        self.link_direct_subclasses(ctx, descriptor);
        Ok(descriptor)
    }

    /// Register a forward-reference placeholder named `name`: a class with no
    /// superclasses and no slots, whose metaclass is
    /// `<forward-referenced-class>`. Its subclasses are unfinalized until
    /// [`Self::resolve_forward_reference`] replaces it.
    pub fn register_forward_reference(
        &self,
        ctx: MutationContext<'gc>,
        name: &str,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let (id, inherited_slots, superclass_cpls, cpl, metaclass, default_initargs) = {
            let inner = self.inner.lock();
            let id = inner.next_free_dynamic_id()?;
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, id, &[])?;
            let superclass_cpls = superclass_cpls(&inner, &cpl);
            let forward_id = ClassId::new(builtin_class_ids::FORWARD_REFERENCED_CLASS)
                .expect("builtin class id is nonzero");
            let metaclass = inner
                .lookup(forward_id)
                .ok_or(ClassTableError::MissingId(forward_id))?;
            let default_initargs = merged_default_initargs(&inner, id, &[], &cpl);
            (
                id,
                inherited_slots,
                superclass_cpls,
                cpl,
                metaclass,
                default_initargs,
            )
        };
        let metaclass_id = metaclass.id();
        let descriptor = ctx.allocate_with(
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id,
                    name,
                    category: ClassCategory::Scheme,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: None,
                    flags: None,
                    direct_supers: &[],
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    superclass_cpls: &superclass_cpls,
                    direct_slot_specs: &[],
                    direct_subclasses: None,
                    direct_methods: None,
                    metaclass: Some(metaclass),
                    direct_default_initargs: &[],
                    default_initargs: &default_initargs,
                },
            ),
            class_header_word(metaclass_id),
        );
        let inner = self.inner.lock();
        inner.register_dynamic(ctx, descriptor)?;
        drop(inner);
        Ok(descriptor)
    }

    /// Replace the forward-reference placeholder `placeholder` by the class
    /// `target` in every direct subclass of the placeholder. Each subclass is
    /// redefined with its own declaration, so its precedence list, layout and
    /// (through the usual fan-out) its descendants' follow `target`.
    pub fn resolve_forward_reference(
        &self,
        ctx: MutationContext<'gc>,
        placeholder: ClassId,
        target: ClassId,
    ) -> Result<(), ClassTableError> {
        let placeholder_class = self
            .lookup(placeholder)
            .ok_or(ClassTableError::MissingId(placeholder))?;
        if placeholder_class.metaclass_id().bits() != builtin_class_ids::FORWARD_REFERENCED_CLASS {
            return Err(ClassTableError::NotForwardReference(placeholder));
        }
        self.lookup(target)
            .ok_or(ClassTableError::MissingId(target))?;
        let subclasses: Vec<ClassId> = placeholder_class
            .direct_subclasses()
            .iter()
            .filter_map(|weak| weak.upgrade(ctx))
            .map(|class| class.id())
            .collect();
        for id in self.superclass_first(&subclasses) {
            let Some(sub) = self.lookup(id) else {
                continue;
            };
            let supers: Vec<ClassId> = sub
                .direct_supers()
                .iter()
                .map(|super_id| {
                    if *super_id == placeholder {
                        target
                    } else {
                        *super_id
                    }
                })
                .collect();
            self.redefine_with_own_declaration(ctx, id, sub, &supers)?;
        }
        Ok(())
    }

    /// Redefine class `sub` (id `id`) with its own slots, defaults, metaclass and hooks and the
    /// direct supers `supers`, so that its precedence list, layout and its descendants' follow.
    fn redefine_with_own_declaration(
        &self,
        ctx: MutationContext<'gc>,
        id: ClassId,
        sub: Gc<'gc, ClassDescriptor<'gc>>,
        supers: &[ClassId],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let names: Vec<String> = sub
            .direct_slots()
            .iter()
            .map(|slot| slot.name().to_string())
            .collect();
        let specs: Vec<SlotSpec<'_, 'gc>> = sub
            .direct_slots()
            .iter()
            .zip(names.iter())
            .map(|(slot, name)| slot.to_spec(name))
            .collect();
        let defaults = super::descriptor::default_initarg_pairs(sub.direct_default_initargs());
        let name = sub.name().to_string();
        let options = DynamicClassOptions {
            primitive_operation_hooks: Some(sub.primitive_operation_hooks()),
            flags: Some(sub.flags()),
            metaclass: Some(sub.metaclass_id()),
        };
        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition {
                id,
                name: &name,
                category: sub.category(),
            },
            options,
            supers,
            &specs,
            &defaults,
        )
    }

    /// Install `cpl` as the precedence list of the class `id`, which must be a reordering of the
    /// list computed for it (the same classes, once each, the class first). The class is
    /// redefined with its own declaration, so layout and descendants follow. A list equal to the
    /// computed one removes any earlier choice.
    pub fn install_precedence_list(
        &self,
        ctx: MutationContext<'gc>,
        id: ClassId,
        cpl: Vec<ClassId>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let _redefinition = self.redefinition.lock();
        let current = self.lookup(id).ok_or(ClassTableError::MissingId(id))?;
        let supers: Vec<ClassId> = current.direct_supers().to_vec();
        {
            let inner = self.inner.lock();
            let previous = inner.cpl_overrides.borrow_mut().remove(&id);
            let (_, computed) = collect_dynamic_hierarchy(&inner, id, &supers)?;
            if cpl == computed {
                // No choice to record.
            } else if is_reordering(&cpl, &computed) {
                inner.cpl_overrides.borrow_mut().insert(id, cpl);
            } else {
                if let Some(previous) = previous {
                    inner.cpl_overrides.borrow_mut().insert(id, previous);
                }
                return Err(ClassTableError::BadPrecedenceOverride(id));
            }
        }
        self.redefine_with_own_declaration(ctx, id, current, &supers)
    }

    /// Forget the precedence list chosen for class `id`, if any.
    pub(crate) fn clear_precedence_override(&self, id: ClassId) {
        self.inner.lock().cpl_overrides.borrow_mut().remove(&id);
    }

    pub fn redefine_dynamic_with_slots(
        &self,
        ctx: MutationContext<'gc>,
        id: ClassId,
        name: &str,
        category: ClassCategory,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        self.redefine_dynamic_with_slots_and_hooks(
            ctx,
            DynamicClassRedefinition { id, name, category },
            DynamicClassOptions::plain(),
            direct_supers,
            direct_slots,
            &[],
        )
    }

    fn redefine_dynamic_with_slots_and_hooks(
        &self,
        ctx: MutationContext<'gc>,
        spec: DynamicClassRedefinition<'_>,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
        direct_default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let redefinition = self.redefinition.lock();
        let direct_supers = normalized_dynamic_direct_supers(spec.category, direct_supers);
        // A new super cannot be the class itself or one of its subclasses.
        let descendants = self.transitive_subclass_ids(ctx, spec.id);
        if let Some(bad) = direct_supers
            .iter()
            .find(|super_id| **super_id == spec.id || descendants.contains(super_id))
        {
            return Err(ClassTableError::Cycle(*bad));
        }
        let (old, descriptor) = self.redefine_one(
            ctx,
            &redefinition,
            spec,
            options,
            &direct_supers,
            direct_slots,
            direct_default_initargs,
        )?;
        // Subclasses whose precedence list or inherited layout moved are
        // redefined in place, so their instances migrate on touch.
        let mut changed = vec![(old, descriptor)];
        changed.extend(self.refresh_subclasses(ctx, &redefinition, &descendants)?);
        Self::invalidate_class_change(ctx, &changed);
        Ok(descriptor)
    }

    /// The ids of every class below `id` in the subclass graph, breadth first,
    /// without `id` itself. Walks the weak `direct_subclasses` lists.
    fn transitive_subclass_ids(&self, ctx: MutationContext<'gc>, id: ClassId) -> Vec<ClassId> {
        let mut found: Vec<ClassId> = Vec::new();
        let mut queue = vec![id];
        while let Some(current) = queue.pop() {
            let Some(class) = self.lookup(current) else {
                continue;
            };
            for weak in class.direct_subclasses().iter() {
                let Some(sub) = weak.upgrade(ctx) else {
                    continue;
                };
                let sub_id = sub.id();
                if sub_id != id && !found.contains(&sub_id) {
                    found.push(sub_id);
                    queue.push(sub_id);
                }
            }
        }
        found
    }

    /// Redefine each class of `ids` whose precedence list or inherited slots
    /// changed, in an order where every superclass in `ids` comes first.
    /// Returns (old, new) for each redefined class. Classes that are not
    /// malleable are skipped (their layout stays as it was).
    fn refresh_subclasses(
        &self,
        ctx: MutationContext<'gc>,
        redefinition: &ClassRedefinitionState,
        ids: &[ClassId],
    ) -> Result<Vec<(Gc<'gc, ClassDescriptor<'gc>>, Gc<'gc, ClassDescriptor<'gc>>)>, ClassTableError>
    {
        let order = self.superclass_first(ids);
        let mut changed = Vec::new();
        for id in order {
            let Some(current) = self.lookup(id) else {
                continue;
            };
            if !current.flags().malleable() {
                log::info!(
                    target: crate::heap::logging::IC_LOG_TARGET,
                    "class {} not redefined under a changed superclass: not malleable",
                    current.name()
                );
                continue;
            }
            let (inherited, cpl) = {
                let inner = self.inner.lock();
                collect_dynamic_hierarchy(&inner, id, current.direct_supers())?
            };
            let inherited_key = |slots: &[SlotDescriptor<'gc>]| {
                slots
                    .iter()
                    .map(|slot| (slot.owner(), slot.name().to_string()))
                    .collect::<Vec<_>>()
            };
            let stored_inherited: Vec<SlotDescriptor<'gc>> = current
                .slots()
                .iter()
                .filter(|slot| slot.owner() != id)
                .copied()
                .collect();
            if cpl == current.cpl() && inherited_key(&inherited) == inherited_key(&stored_inherited)
            {
                continue;
            }
            let names: Vec<String> = current
                .direct_slots()
                .iter()
                .map(|slot| slot.name().to_string())
                .collect();
            let specs: Vec<SlotSpec<'_, 'gc>> = current
                .direct_slots()
                .iter()
                .zip(names.iter())
                .map(|(slot, name)| slot.to_spec(name))
                .collect();
            let defaults =
                super::descriptor::default_initarg_pairs(current.direct_default_initargs());
            let name = current.name().to_string();
            let sub_options = DynamicClassOptions {
                primitive_operation_hooks: Some(current.primitive_operation_hooks()),
                flags: Some(current.flags()),
                metaclass: Some(current.metaclass_id()),
            };
            let supers = current.direct_supers().to_vec();
            let (old, new) = self.redefine_one(
                ctx,
                redefinition,
                DynamicClassRedefinition {
                    id,
                    name: &name,
                    category: current.category(),
                },
                sub_options,
                &supers,
                &specs,
                &defaults,
            )?;
            changed.push((old, new));
        }
        Ok(changed)
    }

    /// `ids` ordered so that a class comes after its superclasses within `ids`.
    fn superclass_first(&self, ids: &[ClassId]) -> Vec<ClassId> {
        let mut remaining: Vec<ClassId> = ids.to_vec();
        let mut order = Vec::with_capacity(ids.len());
        while !remaining.is_empty() {
            let next = remaining.iter().position(|id| {
                self.lookup(*id).is_none_or(|class| {
                    !class
                        .direct_supers()
                        .iter()
                        .any(|super_id| *super_id != *id && remaining.contains(super_id))
                })
            });
            // A cycle cannot reach here (refused up front); keep the order total anyway.
            let index = next.unwrap_or(0);
            order.push(remaining.remove(index));
        }
        order
    }

    /// Redefinition's side effects on dispatch: every generic with a method
    /// on a class in the old or new precedence list of a changed class is
    /// invalidated once, with `ClassChanged` naming that class.
    fn invalidate_class_change(
        ctx: MutationContext<'gc>,
        changed: &[(Gc<'gc, ClassDescriptor<'gc>>, Gc<'gc, ClassDescriptor<'gc>>)],
    ) {
        let table = class_table(ctx);
        let mut affected: Vec<(
            Gc<'gc, GenericDescriptor<'gc>>,
            Gc<'gc, ClassDescriptor<'gc>>,
        )> = Vec::new();
        for (old, new) in changed {
            let ids = old.cpl().iter().chain(new.cpl().iter());
            for id in ids {
                let Some(class) = table.lookup(*id) else {
                    continue;
                };
                for member in class.direct_methods().iter().copied() {
                    let Some(method) = member.try_as::<MethodDescriptor>() else {
                        continue;
                    };
                    let generic = method.generic();
                    if !affected.iter().any(|(seen, _)| Gc::ptr_eq(*seen, generic)) {
                        affected.push((generic, *new));
                    }
                }
            }
        }
        for (generic, class) in affected {
            GenericDescriptor::invalidate(ctx, generic, Reason::ClassChanged(class));
        }
    }

    /// Redefine one class in place: build its descriptor from the given
    /// declaration, install it, and fire the watchpoints over the old one.
    /// Returns the old and the new descriptor. The caller owns the fan-out.
    #[allow(clippy::too_many_arguments)]
    fn redefine_one(
        &self,
        ctx: MutationContext<'gc>,
        redefinition: &ClassRedefinitionState,
        spec: DynamicClassRedefinition<'_>,
        options: DynamicClassOptions,
        direct_supers: &[ClassId],
        direct_slots: &[SlotSpec<'_, 'gc>],
        direct_default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<(Gc<'gc, ClassDescriptor<'gc>>, Gc<'gc, ClassDescriptor<'gc>>), ClassTableError>
    {
        let (
            old,
            inherited_slots,
            superclass_cpls,
            cpl,
            direct_subclasses,
            direct_methods,
            removed_direct_supers,
            metaclass,
            default_initargs,
        ) = {
            let inner = self.inner.lock();
            let old = inner
                .lookup(spec.id)
                .ok_or(ClassTableError::MissingId(spec.id))?;
            if !old.flags().malleable() {
                return Err(ClassTableError::NotMalleable(spec.id));
            }
            check_subclassable(&inner, direct_supers, old.direct_supers())?;
            // The metaclass of a class is fixed: a redefinition keeps it unless
            // it names the same one.
            if let Some(requested) = options.metaclass
                && requested != old.metaclass_id()
            {
                return Err(ClassTableError::MetaclassChanged(spec.id));
            }
            let metaclass = resolve_metaclass(&inner, Some(old.metaclass_id()))?;
            let (inherited_slots, cpl) = collect_dynamic_hierarchy(&inner, spec.id, direct_supers)?;
            let superclass_cpls = superclass_cpls(&inner, &cpl);
            let default_initargs =
                merged_default_initargs(&inner, spec.id, direct_default_initargs, &cpl);
            (
                old,
                inherited_slots,
                superclass_cpls,
                cpl,
                old.direct_subclasses(),
                old.direct_methods(),
                old.direct_supers()
                    .iter()
                    .filter(|old_super_id| !direct_supers.contains(old_super_id))
                    .filter_map(|old_super_id| inner.lookup(*old_super_id))
                    .collect::<Vec<_>>(),
                metaclass,
                default_initargs,
            )
        };
        let descriptor = ctx.allocate_with(
            ClassDescriptor::from_init(
                ctx,
                ClassDescriptorInit {
                    id: spec.id,
                    name: spec.name,
                    category: spec.category,
                    primitive_layout_hooks: None,
                    primitive_operation_hooks: options.primitive_operation_hooks,
                    flags: options.flags,
                    direct_supers,
                    cpl: &cpl,
                    inherited_slots: &inherited_slots,
                    superclass_cpls: &superclass_cpls,
                    direct_slot_specs: direct_slots,
                    direct_subclasses: Some(direct_subclasses),
                    direct_methods: Some(direct_methods),
                    metaclass: Some(metaclass),
                    direct_default_initargs,
                    default_initargs: &default_initargs,
                },
            ),
            class_header_word(metaclass.id()),
        );
        // Extension values survive a redefinition. A storage made for another
        // stamp of the metaclass is replaced on its next access.
        ClassDescriptor::adopt_extension_storage(ctx, descriptor, old);
        // The dependents are about the class, not this descriptor.
        ClassDescriptor::set_dependents(ctx, descriptor, old.dependents());
        let inner = self.inner.lock();
        let descriptor = inner.redefine_dynamic(ctx, descriptor)?;
        drop(inner);
        for old_super in removed_direct_supers {
            ClassDescriptor::remove_direct_subclass(ctx, old_super, spec.id);
        }
        self.link_direct_subclasses(ctx, descriptor);
        redefinition.epoch.set(redefinition.epoch.get() + 1);
        // The descriptor keeps its class id, so slot caches keyed on the old
        // layout would still hit an obsolete instance (which carries the old
        // stamp): clear only this class's caches so it misses, touches and
        // migrates. Other classes keep theirs. (The caller owes the grace
        // period.)
        super::generic::fire_class_slot_protocol(ctx, old);
        // Optimized units that allocated instances of the old descriptor
        // inline assumed it stays the table's current layout; invalidate
        // them. The old descriptor is dead, so the set is not renewed.
        let set = old.redefinition_wp();
        crate::runtime::watchpoint::WatchpointSet::fire(set, ctx);

        Ok((old, descriptor))
    }

    /// Register a Scheme class. `metaclass` is the id of its metaclass, or
    /// `None` for `<class>`.
    pub fn register_scheme_class_from_slot_list(
        &self,
        ctx: MutationContext<'gc>,
        name: Gc<'gc, crate::runtime::objectmodel::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
        metaclass: Option<ClassId>,
        default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
        let direct_slots = parsed_slots
            .iter()
            .map(ParsedSlotSpec::as_slot_spec)
            .collect::<Vec<_>>();
        self.register_dynamic_with_slots_and_hooks(
            ctx,
            &name,
            ClassCategory::Scheme,
            DynamicClassOptions {
                metaclass,
                ..DynamicClassOptions::plain()
            },
            &direct_supers,
            &direct_slots,
            default_initargs,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn register_invocable_scheme_class_from_slot_list(
        &self,
        ctx: MutationContext<'gc>,
        name: Gc<'gc, crate::runtime::objectmodel::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
        default_initargs: &[(Value<'gc>, Value<'gc>)],
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
            default_initargs,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    /// Redefine a Scheme class. `metaclass`, when given, must be the class's
    /// current metaclass; `None` keeps it.
    pub fn redefine_scheme_class_from_slot_list(
        &self,
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::objectmodel::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
        metaclass: Option<ClassId>,
        default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        self.clear_precedence_override(class.id());
        let (name, direct_supers, parsed_slots) =
            parse_scheme_class_shape(ctx, name, slot_names, direct_supers)?;
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
            DynamicClassOptions {
                metaclass,
                ..DynamicClassOptions::plain()
            },
            &direct_supers,
            &direct_slots,
            default_initargs,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn redefine_invocable_scheme_class_from_slot_list(
        &self,
        ctx: MutationContext<'gc>,
        class: Gc<'gc, ClassDescriptor<'gc>>,
        name: Gc<'gc, crate::runtime::objectmodel::Symbol<'gc>>,
        slot_names: Value<'gc>,
        direct_supers: Value<'gc>,
        default_initargs: &[(Value<'gc>, Value<'gc>)],
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, SchemeClassSpecError<'gc>> {
        self.clear_precedence_override(class.id());
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
            default_initargs,
        )
        .map_err(SchemeClassSpecError::ClassTable)
    }

    pub fn lookup(&self, id: ClassId) -> Option<Gc<'gc, ClassDescriptor<'gc>>> {
        self.inner.lock().lookup(id)
    }

    /// Reset the computed protocol state `op` of every class `applies` holds
    /// for (the next use recomputes it).
    pub(crate) fn reset_protocol_state_matching(
        &self,
        op: usize,
        mut applies: impl FnMut(Gc<'gc, ClassDescriptor<'gc>>) -> bool,
    ) {
        let inner = self.inner.lock();
        inner.for_each_class(|class| {
            if applies(class) {
                class.reset_protocol_state(op);
            }
        });
    }

    /// Fire and renew the slot-protocol watchpoint of every class for which
    /// `applies(class, metaclass)` holds. Returns how many dependents were
    /// cleared.
    pub(crate) fn fire_matching_slot_protocol(
        &self,
        ctx: MutationContext<'gc>,
        mut applies: impl FnMut(Gc<'gc, ClassDescriptor<'gc>>, Gc<'gc, ClassDescriptor<'gc>>) -> bool,
    ) -> usize {
        let inner = self.inner.lock();
        let mut cleared = 0;
        inner.for_each_class(|class| {
            let Some(meta) = Value::from(class).class(ctx) else {
                return;
            };
            if applies(class, meta) {
                cleared += ClassDescriptor::renew_slot_protocol_wp(ctx, class);
            }
        });
        cleared
    }

    /// Fire and renew the slot-protocol watchpoint of every class. Returns
    /// how many dependents were cleared.
    pub(crate) fn fire_all_slot_protocol(&self, ctx: MutationContext<'gc>) -> usize {
        let inner = self.inner.lock();
        let mut cleared = 0;
        inner.for_each_class(|class| {
            cleared += ClassDescriptor::renew_slot_protocol_wp(ctx, class);
        });
        cleared
    }

    pub fn allocated_page_count(&self) -> usize {
        self.inner.lock().allocated_page_count()
    }

    pub fn is_subclass(&self, class_id: ClassId, superclass_id: ClassId) -> bool {
        self.inner.lock().is_subclass(class_id, superclass_id)
    }

    fn link_direct_subclasses(
        &self,
        ctx: MutationContext<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) {
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
        &self,
        ctx: MutationContext<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn register(
        &self,
        ctx: MutationContext<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        let (page, slot_index) = self.empty_slot(ctx, descriptor.id())?;
        Gc::write(ctx, page.slots)[slot_index]
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
        &self,
        ctx: MutationContext<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<(), ClassTableError> {
        self.register(ctx, descriptor)
    }

    fn redefine_dynamic(
        &self,
        ctx: MutationContext<'gc>,
        descriptor: Gc<'gc, ClassDescriptor<'gc>>,
    ) -> Result<Gc<'gc, ClassDescriptor<'gc>>, ClassTableError> {
        let old = self
            .lookup(descriptor.id())
            .ok_or(ClassTableError::MissingId(descriptor.id()))?;
        if !old.flags().malleable() {
            return Err(ClassTableError::NotMalleable(descriptor.id()));
        }
        let (page, slot_index) = self.existing_slot(descriptor.id())?;
        Gc::write(ctx, page.slots)[slot_index]
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
        &self,
        ctx: MutationContext<'gc>,
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
                Gc::write(ctx, self.pages)[page_index]
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

    fn next_free_dynamic_id(&self) -> Result<ClassId, ClassTableError> {
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
