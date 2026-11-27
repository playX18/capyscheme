use super::builtin::builtin_id;
use super::flags::LayoutFamily;
use super::hooks::{PrimitiveSlotBoundHook, PrimitiveSlotRefHook, PrimitiveSlotSetHook};
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, Visitor, WeakProcessor};
use crate::runtime::objectmodel::conversions::ClassTagged;
use crate::runtime::{MutationContext, objectmodel::Value};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct SlotFlags(u8);

impl SlotFlags {
    const IMMUTABLE: u8 = 1 << 0;
    const INITIALIZABLE: u8 = 1 << 1;
    const SETTABLE: u8 = 1 << 2;
    const CLASS_ALLOCATED: u8 = 1 << 3;

    pub const fn mutable() -> Self {
        Self(Self::INITIALIZABLE | Self::SETTABLE)
    }

    pub const fn immutable() -> Self {
        Self(Self::IMMUTABLE | Self::INITIALIZABLE)
    }

    pub const fn immutable_flag(self) -> bool {
        self.0 & Self::IMMUTABLE != 0
    }

    pub const fn initializable(self) -> bool {
        self.0 & Self::INITIALIZABLE != 0
    }

    pub const fn settable(self) -> bool {
        self.0 & Self::SETTABLE != 0
    }

    pub const fn class_allocated(self) -> bool {
        self.0 & Self::CLASS_ALLOCATED != 0
    }
}

/// Which of the two slot-definition families a [`SlotDescriptor`] belongs to
/// (AMOP 5.5): a direct slot is what one class's definition declares, an
/// effective slot is what the class uses, one per slot name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotKind {
    Direct,
    Effective,
}

#[derive(Clone, Copy, Debug)]
pub struct SlotSpec<'a, 'gc> {
    name: &'a str,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    slot_ref: Option<PrimitiveSlotRefHook>,
    slot_set: Option<PrimitiveSlotSetHook>,
    slot_bound: Option<PrimitiveSlotBoundHook>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'a, 'gc> SlotSpec<'a, 'gc> {
    pub const fn mutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::mutable(),
        }
    }

    pub const fn immutable(name: &'a str) -> Self {
        Self {
            name,
            init_value: Value::empty(),
            init_keyword: Value::empty(),
            init_thunk: Value::empty(),
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: Value::empty(),
            scheme_slot_set: Value::empty(),
            scheme_slot_bound: Value::empty(),
            getter: Value::from_raw_i64(Value::VALUE_FALSE),
            setter: Value::from_raw_i64(Value::VALUE_FALSE),
            accessor: Value::from_raw_i64(Value::VALUE_FALSE),
            flags: SlotFlags::immutable(),
        }
    }

    pub const fn with_init_value(mut self, value: Value<'gc>) -> Self {
        self.init_value = value;
        self
    }

    pub const fn with_init_keyword(mut self, keyword: Value<'gc>) -> Self {
        self.init_keyword = keyword;
        self
    }

    pub const fn with_init_thunk(mut self, thunk: Value<'gc>) -> Self {
        self.init_thunk = thunk;
        self
    }

    pub const fn with_native_slot_hooks(
        mut self,
        slot_ref: Option<PrimitiveSlotRefHook>,
        slot_set: Option<PrimitiveSlotSetHook>,
        slot_bound: Option<PrimitiveSlotBoundHook>,
    ) -> Self {
        self.slot_ref = slot_ref;
        self.slot_set = slot_set;
        self.slot_bound = slot_bound;
        self
    }

    pub const fn with_scheme_slot_procedures(
        mut self,
        slot_ref: Value<'gc>,
        slot_set: Value<'gc>,
        slot_bound: Value<'gc>,
    ) -> Self {
        self.scheme_slot_ref = slot_ref;
        self.scheme_slot_set = slot_set;
        self.scheme_slot_bound = slot_bound;
        self
    }

    pub const fn with_accessor_names(
        mut self,
        getter: Value<'gc>,
        setter: Value<'gc>,
        accessor: Value<'gc>,
    ) -> Self {
        self.getter = getter;
        self.setter = setter;
        self.accessor = accessor;
        self
    }

    pub const fn without_initargs(mut self) -> Self {
        self.flags.0 &= !SlotFlags::INITIALIZABLE;
        self
    }

    pub const fn without_setter(mut self) -> Self {
        self.flags.0 &= !SlotFlags::SETTABLE;
        self
    }

    pub const fn with_class_allocation(mut self) -> Self {
        self.flags.0 |= SlotFlags::CLASS_ALLOCATED;
        self
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SlotDescriptor<'gc> {
    pub(crate) name: ArrayRef<'gc, u8>,
    pub(crate) owner: ClassId,
    pub(crate) index: usize,
    pub(crate) kind: SlotKind,
    pub(crate) init_value: Value<'gc>,
    pub(crate) init_keyword: Value<'gc>,
    /// Further init keywords accepted for an effective slot, a Scheme list
    /// (AMOP: the union of the direct slots' initargs). Empty for a direct slot.
    pub(crate) extra_init_keywords: Value<'gc>,
    pub(crate) init_thunk: Value<'gc>,
    pub(crate) slot_ref: Option<PrimitiveSlotRefHook>,
    pub(crate) slot_set: Option<PrimitiveSlotSetHook>,
    pub(crate) slot_bound: Option<PrimitiveSlotBoundHook>,
    pub(crate) scheme_slot_ref: Value<'gc>,
    pub(crate) scheme_slot_set: Value<'gc>,
    pub(crate) scheme_slot_bound: Value<'gc>,
    pub(crate) getter: Value<'gc>,
    pub(crate) setter: Value<'gc>,
    pub(crate) accessor: Value<'gc>,
    pub(crate) flags: SlotFlags,
}

// SAFETY: GC trace for `SlotDescriptor`, all reachable heap fields are visited
unsafe impl Trace for SlotDescriptor<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.extra_init_keywords.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.extra_init_keywords.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

impl<'gc> SlotDescriptor<'gc> {
    /// A direct slot declared by the class with owner `owner`, at position
    /// `index` of its effective layout.
    pub(crate) fn from_spec(
        ctx: MutationContext<'gc>,
        owner: ClassId,
        index: usize,
        spec: SlotSpec<'_, 'gc>,
    ) -> Self {
        Self {
            name: Array::from_slice(ctx, spec.name.as_bytes()),
            owner,
            index,
            kind: SlotKind::Direct,
            init_value: spec.init_value,
            init_keyword: spec.init_keyword,
            extra_init_keywords: Value::null(),
            init_thunk: spec.init_thunk,
            slot_ref: spec.slot_ref,
            slot_set: spec.slot_set,
            slot_bound: spec.slot_bound,
            scheme_slot_ref: spec.scheme_slot_ref,
            scheme_slot_set: spec.scheme_slot_set,
            scheme_slot_bound: spec.scheme_slot_bound,
            getter: spec.getter,
            setter: spec.setter,
            accessor: spec.accessor,
            flags: spec.flags,
        }
    }

    /// The direct slot as a spec, so its class can be redefined with the
    /// same declaration. `name` is the slot's name (it must outlive the spec).
    pub(crate) fn to_spec<'a>(&self, name: &'a str) -> SlotSpec<'a, 'gc> {
        SlotSpec {
            name,
            init_value: self.init_value,
            init_keyword: self.init_keyword,
            init_thunk: self.init_thunk,
            slot_ref: self.slot_ref,
            slot_set: self.slot_set,
            slot_bound: self.slot_bound,
            scheme_slot_ref: self.scheme_slot_ref,
            scheme_slot_set: self.scheme_slot_set,
            scheme_slot_bound: self.scheme_slot_bound,
            getter: self.getter,
            setter: self.setter,
            accessor: self.accessor,
            flags: self.flags,
        }
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn kind(&self) -> SlotKind {
        self.kind
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    /// Whether `keyword` is one of this slot's init keywords.
    pub fn accepts_init_keyword(&self, keyword: Value<'gc>) -> bool {
        if self.init_keyword == keyword {
            return true;
        }
        let mut rest = self.extra_init_keywords;
        while rest.is_pair() {
            if rest.car() == keyword {
                return true;
            }
            rest = rest.cdr();
        }
        false
    }

    /// Every init keyword of this slot, the primary one first.
    pub fn init_keywords(&self) -> Vec<Value<'gc>> {
        let mut keywords = Vec::new();
        if !self.init_keyword.is_empty() {
            keywords.push(self.init_keyword);
        }
        let mut rest = self.extra_init_keywords;
        while rest.is_pair() {
            keywords.push(rest.car());
            rest = rest.cdr();
        }
        keywords
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn immutable(&self) -> bool {
        self.flags.immutable_flag()
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

/// AMOP `compute-slots` for a standard class.
///
/// `inherited` are the candidate slots of the superclasses, most general first
/// (the reverse class precedence list), and `own` are the class's direct slot
/// declarations, which are more specific than any inherited one.
///
/// Definitions are grouped by name. Each group becomes ONE effective slot:
/// the most specific definition gives the owner, the allocation and the
/// options, the most specific initform wins, and the init keywords are
/// united. The merged slot takes the position of the group's first
/// definition, so a redeclaring subclass keeps its superclass's layout.
///
/// Returns `(direct, effective)`: the class's direct slots, with their
/// positions in the effective layout, and the effective layout.
pub(crate) fn compute_effective_slots<'gc>(
    ctx: MutationContext<'gc>,
    owner: ClassId,
    inherited: &[SlotDescriptor<'gc>],
    own: &[SlotSpec<'_, 'gc>],
    _superclass_cpls: &[(ClassId, Vec<ClassId>)],
) -> (Vec<SlotDescriptor<'gc>>, Vec<SlotDescriptor<'gc>>) {
    let own_defs: Vec<SlotDescriptor<'gc>> = own
        .iter()
        .map(|spec| SlotDescriptor::from_spec(ctx, owner, 0, *spec))
        .collect();
    let defs: Vec<SlotDescriptor<'gc>> = inherited.iter().chain(&own_defs).copied().collect();

    // Groups of definition indices by name, in order of first appearance.
    let mut group_of_name: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut group_of_def = Vec::with_capacity(defs.len());
    for (i, def) in defs.iter().enumerate() {
        let next = groups.len();
        let g = *group_of_name.entry(def.name().to_owned()).or_insert(next);
        if g == next {
            groups.push(Vec::new());
        }
        groups[g].push(i);
        group_of_def.push(g);
    }

    // Per AMOP, every definition sharing a name merges into one effective
    // slot, whether the owners lie on one superclass chain (a redeclaration)
    // or on different branches.
    let merged: Vec<bool> = groups.iter().map(|members| members.len() > 1).collect();

    // Position of each definition in the effective layout: a merged group
    // takes one position, at its first definition.
    let mut group_position: Vec<Option<usize>> = vec![None; groups.len()];
    let mut position_of_def = Vec::with_capacity(defs.len());
    let mut count = 0;
    for &g in &group_of_def {
        let position = if merged[g] {
            *group_position[g].get_or_insert_with(|| {
                count += 1;
                count - 1
            })
        } else {
            count += 1;
            count - 1
        };
        position_of_def.push(position);
    }

    let mut effective: Vec<Option<SlotDescriptor<'gc>>> = vec![None; count];
    for (g, members) in groups.iter().enumerate() {
        if merged[g] {
            let group: Vec<SlotDescriptor<'gc>> = members.iter().map(|&i| defs[i]).collect();
            let position = group_position[g].expect("a merged group has a position");
            effective[position] = Some(merge_slot_group(ctx, &group, position));
        }
    }
    for (i, def) in defs.iter().enumerate() {
        if !merged[group_of_def[i]] {
            let mut slot = *def;
            slot.index = position_of_def[i];
            slot.kind = SlotKind::Effective;
            effective[slot.index] = Some(slot);
        }
    }
    let effective: Vec<SlotDescriptor<'gc>> = effective
        .into_iter()
        .map(|slot| slot.expect("every effective position is filled"))
        .collect();

    let direct = own_defs
        .into_iter()
        .enumerate()
        .map(|(k, mut slot)| {
            slot.index = position_of_def[inherited.len() + k];
            slot
        })
        .collect();
    (direct, effective)
}

/// The effective slot at position `index` for one group of definitions (see
/// [`compute_effective_slots`]).
fn merge_slot_group<'gc>(
    ctx: MutationContext<'gc>,
    group: &[SlotDescriptor<'gc>],
    index: usize,
) -> SlotDescriptor<'gc> {
    let most_specific_first = || group.iter().rev();
    let winner = *group.last().expect("a slot group is never empty");
    let mut merged = winner;
    merged.index = index;
    merged.kind = SlotKind::Effective;

    merged.init_value = most_specific_first()
        .map(|slot| slot.init_value)
        .find(|value| !value.is_empty())
        .unwrap_or_else(Value::empty);
    merged.init_thunk = most_specific_first()
        .map(|slot| slot.init_thunk)
        .find(|value| !value.is_empty())
        .unwrap_or_else(Value::empty);
    merged.scheme_slot_ref = most_specific_first()
        .map(|slot| slot.scheme_slot_ref)
        .find(|value| !value.is_empty())
        .unwrap_or_else(Value::empty);
    merged.scheme_slot_set = most_specific_first()
        .map(|slot| slot.scheme_slot_set)
        .find(|value| !value.is_empty())
        .unwrap_or_else(Value::empty);
    merged.scheme_slot_bound = most_specific_first()
        .map(|slot| slot.scheme_slot_bound)
        .find(|value| !value.is_empty())
        .unwrap_or_else(Value::empty);
    merged.slot_ref = most_specific_first().find_map(|slot| slot.slot_ref);
    merged.slot_set = most_specific_first().find_map(|slot| slot.slot_set);
    merged.slot_bound = most_specific_first().find_map(|slot| slot.slot_bound);

    let unset = Value::from_raw_i64(Value::VALUE_FALSE);
    merged.getter = most_specific_first()
        .map(|slot| slot.getter)
        .find(|value| *value != unset)
        .unwrap_or(unset);
    merged.setter = most_specific_first()
        .map(|slot| slot.setter)
        .find(|value| *value != unset)
        .unwrap_or(unset);
    merged.accessor = most_specific_first()
        .map(|slot| slot.accessor)
        .find(|value| *value != unset)
        .unwrap_or(unset);

    // Init keywords: the most specific one is primary, the others are kept
    // as further accepted keywords.
    let mut keywords: Vec<Value<'gc>> = Vec::new();
    for slot in most_specific_first() {
        for keyword in slot.init_keywords() {
            if !keywords.contains(&keyword) {
                keywords.push(keyword);
            }
        }
    }
    merged.init_keyword = keywords.first().copied().unwrap_or_else(Value::empty);
    merged.extra_init_keywords = if keywords.len() > 1 {
        Value::list_from_slice(ctx, &keywords[1..])
    } else {
        Value::null()
    };
    merged
}

#[derive(Clone, Copy, Debug)]
pub struct SlotAccessorDescriptor<'gc> {
    pub(crate) name: ArrayRef<'gc, u8>,
    pub(crate) owner: ClassId,
    pub(crate) slot_index: usize,
    pub(crate) init_value: Value<'gc>,
    pub(crate) init_keyword: Value<'gc>,
    pub(crate) init_thunk: Value<'gc>,
    pub(crate) slot_ref: Option<PrimitiveSlotRefHook>,
    pub(crate) slot_set: Option<PrimitiveSlotSetHook>,
    pub(crate) slot_bound: Option<PrimitiveSlotBoundHook>,
    pub(crate) scheme_slot_ref: Value<'gc>,
    pub(crate) scheme_slot_set: Value<'gc>,
    pub(crate) scheme_slot_bound: Value<'gc>,
    pub(crate) getter: Value<'gc>,
    pub(crate) setter: Value<'gc>,
    pub(crate) accessor: Value<'gc>,
    pub(crate) flags: SlotFlags,
}

// SAFETY: GC trace for `SlotAccessorDescriptor`, all reachable heap fields are visited
unsafe impl Trace for SlotAccessorDescriptor<'_> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.name.trace(visitor);
            self.init_value.trace(visitor);
            self.init_keyword.trace(visitor);
            self.init_thunk.trace(visitor);
            self.scheme_slot_ref.trace(visitor);
            self.scheme_slot_set.trace(visitor);
            self.scheme_slot_bound.trace(visitor);
            self.getter.trace(visitor);
            self.setter.trace(visitor);
            self.accessor.trace(visitor);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.name.process_weak_refs(weak_processor);
            self.init_value.process_weak_refs(weak_processor);
            self.init_keyword.process_weak_refs(weak_processor);
            self.init_thunk.process_weak_refs(weak_processor);
            self.scheme_slot_ref.process_weak_refs(weak_processor);
            self.scheme_slot_set.process_weak_refs(weak_processor);
            self.scheme_slot_bound.process_weak_refs(weak_processor);
            self.getter.process_weak_refs(weak_processor);
            self.setter.process_weak_refs(weak_processor);
            self.accessor.process_weak_refs(weak_processor);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotAccessError {
    Immutable,
    OutOfBounds,
    SchemeHookFailed,
    Unbound,
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotInitError<'gc> {
    UnknownKeyword(Value<'gc>),
    NotInitializable(Value<'gc>),
    InitThunkFailed(Value<'gc>),
    MissingInvokerSlot(Value<'gc>),
    InvokerNotProcedure(Value<'gc>),
}

impl<'gc> SlotAccessorDescriptor<'gc> {
    pub(crate) fn from_slot(ctx: MutationContext<'gc>, slot: SlotDescriptor<'gc>) -> Self {
        Self {
            name: Array::from_slice(ctx, slot.name.as_slice()),
            owner: slot.owner,
            slot_index: slot.index,
            init_value: slot.init_value,
            init_keyword: slot.init_keyword,
            init_thunk: slot.init_thunk,
            slot_ref: slot.slot_ref,
            slot_set: slot.slot_set,
            slot_bound: slot.slot_bound,
            scheme_slot_ref: slot.scheme_slot_ref,
            scheme_slot_set: slot.scheme_slot_set,
            scheme_slot_bound: slot.scheme_slot_bound,
            getter: slot.getter,
            setter: slot.setter,
            accessor: slot.accessor,
            flags: slot.flags,
        }
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn slot_ref(&self) -> Option<PrimitiveSlotRefHook> {
        self.slot_ref
    }

    pub fn slot_set(&self) -> Option<PrimitiveSlotSetHook> {
        self.slot_set
    }

    pub fn slot_bound(&self) -> Option<PrimitiveSlotBoundHook> {
        self.slot_bound
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotDefinitionDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'gc> SlotDefinitionDescriptor<'gc> {
    /// The Scheme object for `slot`. A direct slot gets the
    /// `<standard-direct-slot-definition>` header and an effective slot the
    /// `<standard-effective-slot-definition>` one; both are slot definitions.
    pub(crate) fn from_slot(ctx: MutationContext<'gc>, slot: SlotDescriptor<'gc>) -> Gc<'gc, Self> {
        let class_id = match slot.kind {
            SlotKind::Direct => builtin_class_ids::DIRECT_SLOT_DEFINITION,
            SlotKind::Effective => builtin_class_ids::EFFECTIVE_SLOT_DEFINITION,
        };
        ctx.allocate_with(
            Self {
                name: Array::from_slice(ctx, slot.name.as_slice()),
                owner: slot.owner,
                index: slot.index,
                init_value: slot.init_value,
                init_keyword: slot.init_keyword,
                init_thunk: slot.init_thunk,
                scheme_slot_ref: slot.scheme_slot_ref,
                scheme_slot_set: slot.scheme_slot_set,
                scheme_slot_bound: slot.scheme_slot_bound,
                getter: slot.getter,
                setter: slot.setter,
                accessor: slot.accessor,
                flags: slot.flags,
            },
            class_header_word(builtin_id(class_id)),
        )
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub fn class_allocated(&self) -> bool {
        self.flags.class_allocated()
    }
}

// SAFETY: `gc` for `SlotDefinitionDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for SlotDefinitionDescriptor<'gc> {
    const TYPE_NAME: &'static str = "slot-definition";
    const CLASS_IDS: &'static [u32] = &[
        builtin_class_ids::SLOT_DEFINITION,
        builtin_class_ids::DIRECT_SLOT_DEFINITION,
        builtin_class_ids::EFFECTIVE_SLOT_DEFINITION,
    ];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::SlotDefinition);
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotAccessorDefinition<'gc> {
    name: ArrayRef<'gc, u8>,
    owner: ClassId,
    slot_index: usize,
    init_value: Value<'gc>,
    init_keyword: Value<'gc>,
    init_thunk: Value<'gc>,
    scheme_slot_ref: Value<'gc>,
    scheme_slot_set: Value<'gc>,
    scheme_slot_bound: Value<'gc>,
    getter: Value<'gc>,
    setter: Value<'gc>,
    accessor: Value<'gc>,
    flags: SlotFlags,
}

impl<'gc> SlotAccessorDefinition<'gc> {
    pub(crate) fn from_accessor(
        ctx: MutationContext<'gc>,
        accessor: SlotAccessorDescriptor<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name: Array::from_slice(ctx, accessor.name.as_slice()),
                owner: accessor.owner,
                slot_index: accessor.slot_index,
                init_value: accessor.init_value,
                init_keyword: accessor.init_keyword,
                init_thunk: accessor.init_thunk,
                scheme_slot_ref: accessor.scheme_slot_ref,
                scheme_slot_set: accessor.scheme_slot_set,
                scheme_slot_bound: accessor.scheme_slot_bound,
                getter: accessor.getter,
                setter: accessor.setter,
                accessor: accessor.accessor,
                flags: accessor.flags,
            },
            class_header_word(builtin_id(builtin_class_ids::SLOT_ACCESSOR)),
        )
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn slot_index(&self) -> usize {
        self.slot_index
    }

    pub fn init_value(&self) -> Value<'gc> {
        self.init_value
    }

    pub fn init_keyword(&self) -> Value<'gc> {
        self.init_keyword
    }

    pub fn init_thunk(&self) -> Value<'gc> {
        self.init_thunk
    }

    pub fn scheme_slot_ref(&self) -> Value<'gc> {
        self.scheme_slot_ref
    }

    pub fn scheme_slot_set(&self) -> Value<'gc> {
        self.scheme_slot_set
    }

    pub fn scheme_slot_bound(&self) -> Value<'gc> {
        self.scheme_slot_bound
    }

    pub fn getter(&self) -> Value<'gc> {
        self.getter
    }

    pub fn setter(&self) -> Value<'gc> {
        self.setter
    }

    pub fn accessor(&self) -> Value<'gc> {
        self.accessor
    }

    pub fn flags(&self) -> SlotFlags {
        self.flags
    }

    pub(crate) fn as_accessor_descriptor(&self) -> SlotAccessorDescriptor<'gc> {
        SlotAccessorDescriptor {
            name: self.name,
            owner: self.owner,
            slot_index: self.slot_index,
            init_value: self.init_value,
            init_keyword: self.init_keyword,
            init_thunk: self.init_thunk,
            slot_ref: None,
            slot_set: None,
            slot_bound: None,
            scheme_slot_ref: self.scheme_slot_ref,
            scheme_slot_set: self.scheme_slot_set,
            scheme_slot_bound: self.scheme_slot_bound,
            getter: self.getter,
            setter: self.setter,
            accessor: self.accessor,
            flags: self.flags,
        }
    }
}

// SAFETY: `gc` for `SlotAccessorDefinition` upholds all trait invariants
unsafe impl<'gc> ClassTagged for SlotAccessorDefinition<'gc> {
    const TYPE_NAME: &'static str = "slot-accessor";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SLOT_ACCESSOR];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::SlotDefinition);
}
