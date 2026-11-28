use std::mem::size_of;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};

use smallvec::SmallVec;

use super::combination::{MethodCombination, standard_combination};
use super::descriptor::ClassDescriptor;
use super::flags::LayoutFamily;
use super::ic;
use super::instance::SchemeInstance;
use super::specializer::EqlSpecializer;
use super::table::class_table;
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::FILL_RETRIES;
use crate::heap::cell::Lock;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace};
use crate::prelude::*;
use crate::runtime::objectmodel::conversions::ClassTagged;
use crate::runtime::watchpoint::{Dependent, WatchpointSet};
use crate::runtime::{
    MutationContext,
    objectmodel::{Closure, Keyword, PROCEDURES, Value},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenericDispatchError {
    Arity,
    NoApplicableMethod,
}

pub const SLOT_PROTOCOL_GENERICS: &[(crate::runtime::hooks::Hook, usize)] = &[
    (crate::runtime::hooks::Hook::SlotRefUsingClass, 3),
    (crate::runtime::hooks::Hook::SlotSetUsingClass, 4),
    (crate::runtime::hooks::Hook::SlotBoundUsingClass, 3),
    (crate::runtime::hooks::Hook::SlotUnbindUsingClass, 3),
];

/// Indices into [`SLOT_PROTOCOL_GENERICS`], and of the per-class state of
/// `initialize` after them.
pub(crate) const OP_REF: usize = 0;
pub(crate) const OP_SET: usize = 1;
pub(crate) const OP_BOUND: usize = 2;
pub(crate) const OP_UNBIND: usize = 3;
pub(crate) const OP_INIT: usize = 4;

/// The role of a boot generic that the runtime treats specially, see
/// [`GenericDescriptor::mark_protocol_role`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProtocolRole {
    Initialize,
    SlotProtocol,
    Lifecycle,
}

/// Set when a method that is not the boot library's is added to a lifecycle generic
/// (`allocate-instance`, `initialize-instance`, `shared-initialize`). From then on `make` takes the
/// Scheme path, which runs the whole lifecycle. It is never cleared.
pub(crate) static LIFECYCLE_CUSTOM: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Record a change that may affect slot caches: fire the slot-protocol
pub fn fire_slot_protocol<'gc>(ctx: MutationContext<'gc>) {
    let cleared = class_table(ctx).fire_all_slot_protocol(ctx);

    log::info!(target: crate::heap::logging::IC_LOG_TARGET, "slot protocol fired: {cleared} dependents cleared");
}

/// A method was added to (or replaced on) a slot protocol generic: fire the
/// slot-protocol set of every class it may apply to, and no other.
pub fn fire_slot_protocol_for_method<'gc>(
    ctx: MutationContext<'gc>,
    method: Gc<'gc, MethodDescriptor<'gc>>,
) {
    let Some(slot_definition) =
        ClassId::new(builtin_class_ids::SLOT_DEFINITION).and_then(|id| class_table(ctx).lookup(id))
    else {
        fire_slot_protocol(ctx);
        return;
    };
    let cleared = class_table(ctx).fire_matching_slot_protocol(ctx, |class, meta| {
        method.may_apply_to_classes(&[meta, class, slot_definition])
    });

    log::info!(target: crate::heap::logging::IC_LOG_TARGET, "slot protocol method added: {cleared} dependents cleared");
}

/// Fire only `class`'s slot-protocol watchpoint (a redefinition: caches
pub fn fire_class_slot_protocol<'gc>(
    ctx: MutationContext<'gc>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
) {
    let cleared = ClassDescriptor::renew_slot_protocol_wp(ctx, class);

    log::info!(
        target: crate::heap::logging::IC_LOG_TARGET, "slot protocol of {} fired: {cleared} dependents cleared",
        class.name()
    );
}

pub struct GenericInvocation<'gc> {
    body: Value<'gc>,
    next: Option<Value<'gc>>,
    args: ArrayRef<'gc, Value<'gc>>,
}

impl<'gc> GenericInvocation<'gc> {
    pub fn body(&self) -> Value<'gc> {
        self.body
    }

    /// The next-method argument, first in the call: `None` for fallback
    /// calls, whose body takes the plain call args.
    pub fn next_arg(&self) -> Option<Value<'gc>> {
        self.next
    }

    /// The call args, unfused: the caller chains [`Self::next_arg`] ahead
    pub fn call_args(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.args
    }

    /// The fused `[next, args...]` call args for callers that need a
    pub fn fused_args(&self, ctx: MutationContext<'gc>) -> ArrayRef<'gc, Value<'gc>> {
        match self.next {
            Some(next) => Array::new(ctx, self.args.len() + 1, |_, index| {
                if index == 0 {
                    next
                } else {
                    self.args[index - 1]
                }
            }),
            None => self.args,
        }
    }
}

/// Discriminating-function state.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(require_static)]
#[repr(u8)]
#[allow(dead_code)]
pub(crate) enum DfunState {
    /// Initial, next call classifies.
    Initial = 0,
    /// No methods.
    NoMethods = 1,
    /// One top-only `NO_NEXT` method.
    DefaultOnly = 2,
    /// Slot accessor methods.
    Accessor = 3,
    /// General dispatch.
    Caching = 4,
    /// Custom dfun installed.
    Custom = 5,
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct GenericDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    methods: Lock<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    /// Dispatch table.
    dispatch_table: Lock<ArrayRef<'gc, DispatchBucket<'gc>>>,
    /// Dispatch width.
    dispatch_width: Lock<u32>,
    /// Dispatch arity cap.
    dispatch_arity: Lock<u32>,
    /// Slot-access cache.
    slot_cache: Lock<ArrayRef<'gc, SlotAccessEntry>>,
    /// `NO_NEXT` method cache.
    method_cache: Lock<ArrayRef<'gc, MethodEntry<'gc>>>,
    /// Chain cursor cache.
    cursor_cache: Lock<ArrayRef<'gc, CursorEntry<'gc>>>,
    max_required_dispatch_args: usize,
    fallback: Lock<Value<'gc>>,
    flags: Lock<GenericFlags>,
    /// Cache epoch.
    epoch: Lock<u32>,
    /// Method combination.
    combination: Lock<Gc<'gc, MethodCombination<'gc>>>,
    /// Argument precedence order.
    precedence: Lock<Option<ArrayRef<'gc, u8>>>,
    /// Fired by invalidate.
    change_wp: Lock<Gc<'gc, WatchpointSet<'gc>>>,
    /// AMOP dependents.
    dependents: Lock<Value<'gc>>,
    /// Dfun state.
    dfun_state: Lock<DfunState>,
    /// Default-only entry.
    default_entry: Lock<Value<'gc>>,
    /// Custom dfun.
    custom_dfun: Lock<Value<'gc>>,
}

/// Slot accessor resolved for one class.
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct SlotAccessEntry {
    class_id: u32,
    stamp: u32,
    index: u32,
    writer: bool,
}

/// Max linear slot-cache entries.
pub(crate) const SLOT_CACHE_LINEAR_MAX: usize = 8;
/// Min slot-cache table size.
const SLOT_CACHE_TABLE_MIN: usize = 32;
/// Max slot-cache table size.
const SLOT_CACHE_TABLE_MAX: usize = 256;

/// Empty slot entry marker.
const EMPTY_SLOT_ENTRY: SlotAccessEntry = SlotAccessEntry {
    class_id: 0,
    stamp: 0,
    index: 0,
    writer: false,
};

/// The shape of a slot cache: how it is laid out and whether every entry
/// reads the same slot index with the same accessor kind (ONE-INDEX).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SlotCacheShape {
    Empty,
    Linear,
    Table,
}

/// Position of `class_id`'s entry in a slot cache, if it has one. A plain
/// array is scanned; a table is probed from the class id's home slot.
#[inline]
fn slot_entry_position(cache: &[SlotAccessEntry], class_id: u32) -> Option<usize> {
    if cache.len() <= SLOT_CACHE_LINEAR_MAX {
        return cache.iter().position(|entry| entry.class_id == class_id);
    }
    let mut at = dispatch_hash(class_id as u64, &[], cache.len());
    loop {
        match cache[at].class_id {
            0 => return None,
            id if id == class_id => return Some(at),
            _ => at = (at + 1) & (cache.len() - 1),
        }
    }
}

/// The table capacity for `entries` entries at load of at most one half.
fn slot_table_capacity(entries: usize) -> Option<usize> {
    let mut capacity = SLOT_CACHE_TABLE_MIN;
    while entries * 2 > capacity {
        capacity *= 2;
    }
    (capacity <= SLOT_CACHE_TABLE_MAX).then_some(capacity)
}

/// Insert `entry` into an open-addressed table of entries.
fn slot_table_place(table: &mut [SlotAccessEntry], entry: SlotAccessEntry) {
    let mut at = dispatch_hash(entry.class_id as u64, &[], table.len());
    while table[at].class_id != 0 {
        at = (at + 1) & (table.len() - 1);
    }
    table[at] = entry;
}

/// The shape of a slot cache, and whether every live entry reads one slot
pub(crate) fn slot_cache_shape(cache: &[SlotAccessEntry]) -> (SlotCacheShape, bool) {
    let shape = match cache.len() {
        0 => SlotCacheShape::Empty,
        len if len > SLOT_CACHE_LINEAR_MAX => SlotCacheShape::Table,
        _ => SlotCacheShape::Linear,
    };
    let mut live = cache.iter().filter(|entry| entry.class_id != 0);
    let one_index = match live.next() {
        None => false,
        Some(first) => live.all(|entry| entry.index == first.index && entry.writer == first.writer),
    };
    (shape, one_index)
}

/// The slot cache with `entry` added to `current`, or `None` when the cache
fn slot_cache_with<'gc>(
    ctx: MutationContext<'gc>,
    current: ArrayRef<'gc, SlotAccessEntry>,
    entry: SlotAccessEntry,
) -> Option<ArrayRef<'gc, SlotAccessEntry>> {
    let live = current
        .iter()
        .copied()
        .filter(|existing| existing.class_id != 0)
        .count();
    if current.len() <= SLOT_CACHE_LINEAR_MAX && live < SLOT_CACHE_LINEAR_MAX {
        return Some(Array::new(ctx, live + 1, |_, index| {
            if index < live { current[index] } else { entry }
        }));
    }
    let capacity = slot_table_capacity(live + 1)?;
    let mut table = vec![EMPTY_SLOT_ENTRY; capacity];
    for existing in current
        .iter()
        .copied()
        .filter(|existing| existing.class_id != 0)
    {
        slot_table_place(&mut table, existing);
    }
    slot_table_place(&mut table, entry);
    // The copies are plain data, so the allocation below may collect freely.
    Some(Array::new(ctx, capacity, |_, index| table[index]))
}

/// A dispatch resolved to a chain cursor body () for arguments that are all
/// cells of these classes: the body and the cursor for its chain.
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct CursorEntry<'gc> {
    argc: u32,
    class0: u32,
    class1: u32,
    pad: u32,
    body: Value<'gc>,
    cursor: Value<'gc>,
}

/// A dispatch resolved to a `NO_NEXT` method body for arguments that are all
/// cells of these classes (`class1` is 0 with one argument).
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct MethodEntry<'gc> {
    argc: u32,
    class0: u32,
    class1: u32,
    pad: u32,
    body: Value<'gc>,
}

/// Layout of the method cache, read by the machine `generic_call` entry.
pub(crate) mod method_cache_layout {
    use super::{GenericDescriptor, MethodEntry};
    use core::mem::{offset_of, size_of};

    pub const FIELD: usize = offset_of!(GenericDescriptor<'static>, method_cache);
    pub const ENTRY_SIZE: usize = size_of::<MethodEntry<'static>>();
    pub const ARGC: usize = offset_of!(MethodEntry<'static>, argc);
    pub const CLASS0: usize = offset_of!(MethodEntry<'static>, class0);
    pub const CLASS1: usize = offset_of!(MethodEntry<'static>, class1);
    pub const BODY: usize = offset_of!(MethodEntry<'static>, body);
    pub const ARRAY_DATA: usize = 8;
}

/// Layout of the slot-access cache, read by the machine `generic_call` entry
/// (`codegen::stubs::emit_generic_call`).
pub(crate) mod slot_cache_layout {
    use super::{GenericDescriptor, SlotAccessEntry};
    use crate::heap::alloc::ArrayRef;
    use crate::heap::cell::Lock;
    use core::mem::{offset_of, size_of};

    pub const FIELD: usize = offset_of!(GenericDescriptor<'static>, slot_cache);
    pub const ENTRY_SIZE: usize = size_of::<SlotAccessEntry>();
    pub const CLASS_ID: usize = offset_of!(SlotAccessEntry, class_id);
    pub const INDEX: usize = offset_of!(SlotAccessEntry, index);
    pub const WRITER: usize = offset_of!(SlotAccessEntry, writer);
    pub const STAMP: usize = offset_of!(SlotAccessEntry, stamp);
    /// Offset of the elements of the cache `Array`.
    pub const ARRAY_DATA: usize = 8;

    const _: () = assert!(size_of::<Lock<ArrayRef<'static, SlotAccessEntry>>>() == 8);
}

/// Layout of the descriptor fields Tier 2 reads for generic devirtualization
pub(crate) mod cache_layout {
    use super::GenericDescriptor;
    use core::mem::offset_of;

    pub const EPOCH: usize = offset_of!(GenericDescriptor<'static>, epoch);
    pub const SLOT_CACHE: usize = offset_of!(GenericDescriptor<'static>, slot_cache);
    pub const METHOD_CACHE: usize = offset_of!(GenericDescriptor<'static>, method_cache);
}

/// A [`CursorEntry`] copied out for the compiler, as `(class0, class1, body, cursor)`.
pub(crate) type CursorSnapshot<'gc> = (u32, u32, Value<'gc>, Value<'gc>);

/// A [`MethodEntry`] copied out for the compiler, as `(class0, class1, body)`.
pub(crate) type MethodSnapshot<'gc> = (u32, u32, Value<'gc>);

/// Layout of a cached method descriptor, read by the lazy generic site
/// (`codegen::lazy::generic`): the body of the first cached method.
pub(crate) mod method_layout {
    use super::MethodDescriptor;
    use core::mem::offset_of;

    pub const BODY: usize = offset_of!(MethodDescriptor<'static>, body);
    pub const FLAGS: usize = offset_of!(MethodDescriptor<'static>, flags);
    /// The `NO_NEXT` flag bit in the byte at [`FLAGS`] (see `MethodFlags`).
    pub const NO_NEXT: i64 = 1 << 1;
}

/// Most distinct argument-class keys a generic's dispatch table holds. A
pub(crate) const MAX_DISPATCH_ENTRIES: usize = 4096;

/// Smallest table capacity (a power of two).
const MIN_DISPATCH_CAPACITY: usize = 8;

/// Calls that reached `generic_closure_proc` (the Rust entry) while
/// [`DISPATCH_STATS`] was on: those the `generic_call` stub did not serve.
pub(crate) static RUST_ENTRIES: AtomicU64 = AtomicU64::new(0);
pub(crate) static DISPATCH_STATS: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Set once the boot image is complete (`%mop-fixup!`,). Until then no
pub(crate) static MOP_ENABLED: AtomicBool = AtomicBool::new(false);

/// Keys that did not fit a full table
pub(crate) static DISPATCH_OVERFLOWS: AtomicU64 = AtomicU64::new(0);

/// One slot of a generic's dispatch table: the applicable methods for
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct DispatchBucket<'gc> {
    key: u64,
    /// `BUCKET_DIRECT` when `body` can be entered with `#f` as the
    /// next-method argument.
    flags: u32,
    /// Class ids past the first two, for generics dispatching on more.
    rest: Option<ArrayRef<'gc, ClassId>>,
    methods: Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    /// The first method's body when it is a `NO_NEXT` closure, else null.
    body: Value<'gc>,
    /// The effective method of a combined generic for these classes, else
    emf: Value<'gc>,
    /// Eql buckets only (a generic with an eql method): the methods that
    eql: Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    /// Eql buckets only: the resolved answers for the argument sets whose
    /// eql candidates match (keyed by the match mask), copy-on-write.
    memo: Option<ArrayRef<'gc, EqlMemoEntry<'gc>>>,
    /// `BUCKET_EQL_DIRECT` only: one entry per eql candidate, which the stub
    eql_direct: Option<ArrayRef<'gc, EqlDirectEntry<'gc>>>,
}

const BUCKET_DIRECT: u32 = 1;

/// An eql bucket the machine stub resolves itself: when the argument at its
const BUCKET_EQL_DIRECT: u32 = 2;
const BUCKET_EQL_DEFAULT: u32 = 4;
/// With `BUCKET_EQL_DIRECT`: the compared argument is the second one.
const BUCKET_EQL_POS1: u32 = 8;
/// With `BUCKET_EQL_DIRECT`: more than one entry (the stub loops over them).
const BUCKET_EQL_MULTI: u32 = 16;

/// Most candidates an eql-direct bucket holds.
const MAX_EQL_DIRECT: usize = 4;

#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub(crate) struct EqlDirectEntry<'gc> {
    word: u64,
    body: Value<'gc>,
}

const _: () = assert!(size_of::<EqlDirectEntry<'static>>() == 16);

/// Most eql candidates one bucket resolves with a 64-bit match mask. A
/// generic with more falls back to the uncached applicable-method path.
const MAX_EQL_CANDIDATES: usize = 64;

/// Most resolved match masks one eql bucket keeps.
const MAX_EQL_MEMO: usize = 16;

/// The resolution of one eql match mask under an eql bucket: the applicable
#[derive(Clone, Copy, Trace)]
pub(crate) struct EqlMemoEntry<'gc> {
    mask: u64,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    emf: Value<'gc>,
}

const _: () = assert!(size_of::<Option<ArrayRef<'static, u8>>>() == size_of::<usize>());

impl<'gc> DispatchBucket<'gc> {
    fn empty() -> Self {
        Self {
            key: 0,
            flags: 0,
            rest: None,
            methods: None,
            body: Value::null(),
            emf: Value::null(),
            eql: None,
            memo: None,
            eql_direct: None,
        }
    }
}

/// The key of a dispatch: class ids are 24 bits, so two of them and the
/// argument count (16 bits) pack into one word the stub compares at once.
#[inline]
fn dispatch_key(id0: u32, id1: u32, arity: u32) -> u64 {
    id0 as u64 | (id1 as u64) << 24 | (arity as u64) << 48
}

/// Most arguments a dispatch key tells apart.
const MAX_ARITY_KEY: u32 = 0xFFFF;

/// Layout of the dispatch table, for the machine `generic_call` entry.
pub(crate) mod dispatch_table_layout {
    use super::{DispatchBucket, GenericDescriptor};
    use core::mem::{offset_of, size_of};

    pub const FIELD: usize = offset_of!(GenericDescriptor<'static>, dispatch_table);
    pub const BUCKET_SIZE: usize = size_of::<DispatchBucket<'static>>();
    pub const KEY: usize = offset_of!(DispatchBucket<'static>, key);
    pub const FLAGS: usize = offset_of!(DispatchBucket<'static>, flags);
    /// The `flags` bit set when `body` can be entered directly.
    pub const DIRECT: i64 = super::BUCKET_DIRECT as i64;
    pub const METHODS: usize = offset_of!(DispatchBucket<'static>, methods);
    pub const BODY: usize = offset_of!(DispatchBucket<'static>, body);
    pub const EQL_DIRECT: usize = offset_of!(DispatchBucket<'static>, eql_direct);
    /// The `flags` bits of an eql bucket the stub resolves.
    pub const EQL_DIRECT_FLAG: i64 = super::BUCKET_EQL_DIRECT as i64;
    pub const EQL_DEFAULT_FLAG: i64 = super::BUCKET_EQL_DEFAULT as i64;
    pub const EQL_POS1_FLAG: i64 = super::BUCKET_EQL_POS1 as i64;
    pub const EQL_MULTI_FLAG: i64 = super::BUCKET_EQL_MULTI as i64;
    pub const ENTRY_SIZE: usize = size_of::<super::EqlDirectEntry<'static>>();
    pub const ENTRY_WORD: usize = offset_of!(super::EqlDirectEntry<'static>, word);
    pub const ENTRY_BODY: usize = offset_of!(super::EqlDirectEntry<'static>, body);
    pub const WIDTH: usize = offset_of!(GenericDescriptor<'static>, dispatch_width);
    pub const ARITY_CAP: usize = offset_of!(GenericDescriptor<'static>, dispatch_arity);
    /// Offset of the elements of the table `Array`.
    pub const ARRAY_DATA: usize = 8;
}

/// Multipliers of [`dispatch_hash`]; the `generic_call` stub multiplies the
/// key by the first for keys without `rest`.
pub(crate) const DISPATCH_MUL: [u64; 2] = [0x9E37_79B9_7F4A_7C15, 0xC2B2_AE3D_27D4_EB4F];

/// The bucket index of a key in a table of `len` (a power of two) buckets:
/// bits 32.. of a 64-bit multiplicative mix.
#[inline]
fn dispatch_hash(key: u64, rest: &[ClassId], len: usize) -> usize {
    let mut h = key.wrapping_mul(DISPATCH_MUL[0]);
    for id in rest {
        h = (h.rotate_left(13) ^ id.bits() as u64).wrapping_mul(DISPATCH_MUL[1]);
    }
    ((h >> 32) as usize) & (len - 1)
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct GenericFlags(u8);

impl GenericFlags {
    const SEALED: u8 = 1 << 0;
    /// Some method specializes an argument on a funcallable class (a
    const RESOLVED_KEYS: u8 = 1 << 1;
    /// Standard method combination, every method primary, no eql method:
    const SIMPLE: u8 = 1 << 2;
    /// Some method specializes an argument on an `eql` specializer. Such a
    const HAS_EQL: u8 = 1 << 3;
    /// A user discriminating function is installed. Such a generic
    const CUSTOM_DFUN: u8 = 1 << 4;
    /// The boot generic of the slot protocol (`slot-ref-using-class` and its three siblings).
    /// A user generic with the same name does not have it.
    const SLOT_PROTOCOL: u8 = 1 << 5;
    /// The boot `initialize` generic.
    const INITIALIZE: u8 = 1 << 6;
    /// One of the boot instance lifecycle generics (`allocate-instance`, `initialize-instance`,
    /// `shared-initialize`): a method added to one disables the native `make` fast path.
    const LIFECYCLE: u8 = 1 << 7;

    pub const fn empty() -> Self {
        Self(0)
    }

    /// Whether this is one of the boot slot protocol generics.
    pub const fn slot_protocol(self) -> bool {
        self.0 & Self::SLOT_PROTOCOL != 0
    }

    pub const fn with_slot_protocol(self) -> Self {
        Self(self.0 | Self::SLOT_PROTOCOL)
    }

    /// Whether this is the boot `initialize` generic.
    pub const fn initialize(self) -> bool {
        self.0 & Self::INITIALIZE != 0
    }

    pub const fn with_initialize(self) -> Self {
        Self(self.0 | Self::INITIALIZE)
    }

    /// Whether this is one of the boot instance lifecycle generics.
    pub const fn lifecycle(self) -> bool {
        self.0 & Self::LIFECYCLE != 0
    }

    pub const fn with_lifecycle(self) -> Self {
        Self(self.0 | Self::LIFECYCLE)
    }

    pub const fn sealed(self) -> bool {
        self.0 & Self::SEALED != 0
    }

    pub const fn with_sealed(self) -> Self {
        Self(self.0 | Self::SEALED)
    }

    pub const fn without_sealed(self) -> Self {
        Self(self.0 & !Self::SEALED)
    }

    pub const fn resolved_keys(self) -> bool {
        self.0 & Self::RESOLVED_KEYS != 0
    }

    pub const fn with_resolved_keys(self) -> Self {
        Self(self.0 | Self::RESOLVED_KEYS)
    }

    pub const fn has_eql(self) -> bool {
        self.0 & Self::HAS_EQL != 0
    }

    pub const fn with_has_eql(self) -> Self {
        Self(self.0 | Self::HAS_EQL)
    }

    pub const fn without_resolved_keys(self) -> Self {
        Self(self.0 & !Self::RESOLVED_KEYS)
    }

    pub const fn without_has_eql(self) -> Self {
        Self(self.0 & !Self::HAS_EQL)
    }

    pub const fn simple(self) -> bool {
        self.0 & Self::SIMPLE != 0
    }

    pub const fn with_simple(self) -> Self {
        Self(self.0 | Self::SIMPLE)
    }

    pub const fn without_simple(self) -> Self {
        Self(self.0 & !Self::SIMPLE)
    }

    pub const fn custom_dfun(self) -> bool {
        self.0 & Self::CUSTOM_DFUN != 0
    }

    pub const fn with_custom_dfun(self) -> Self {
        Self(self.0 | Self::CUSTOM_DFUN)
    }

    pub const fn without_custom_dfun(self) -> Self {
        Self(self.0 & !Self::CUSTOM_DFUN)
    }
}

/// The class id a generic's dispatch keys on when [`GenericFlags::resolved_keys`]
pub(crate) fn resolved_class_id<'gc>(
    ctx: MutationContext<'gc>,
    value: Value<'gc>,
) -> Option<ClassId> {
    let id = value.class_id()?;
    if id.bits() != builtin_class_ids::CLOSURE {
        return Some(id);
    }
    if let Some(generic) = generic_descriptor_from_value(ctx, value) {
        let descriptor: Value<'gc> = generic.into();
        return descriptor.class_id();
    }
    if let Some(instance) = invocable_instance_of(ctx, value) {
        return instance.class_id();
    }
    Some(id)
}

/// The instance an invocable closure wraps, from its `instance` property.
pub(crate) fn invocable_instance_of<'gc>(
    ctx: MutationContext<'gc>,
    value: Value<'gc>,
) -> Option<Value<'gc>> {
    if !value.is::<Closure>() {
        return None;
    }
    let properties = value.downcast::<Closure>().meta.get();
    if !properties.is_pair() {
        return None;
    }
    let instance = properties.assq(ctx.intern("instance"))?.cdr();
    instance.is_cell().then_some(instance)
}

/// The key function a dispatch uses, chosen once per call from the generic's
pub(crate) type KeyFn<'gc> = fn(MutationContext<'gc>, Value<'gc>) -> Option<ClassId>;

fn plain_key<'gc>(_ctx: MutationContext<'gc>, value: Value<'gc>) -> Option<ClassId> {
    value.class_id()
}

fn resolved_key<'gc>(ctx: MutationContext<'gc>, value: Value<'gc>) -> Option<ClassId> {
    resolved_class_id(ctx, value)
}

/// Whether a method specialized on class `id` needs its generic to key
fn specializer_needs_resolved_keys<'gc>(
    table: &super::table::ClassTable<'gc>,
    id: ClassId,
) -> bool {
    let Some(class) = table.lookup(id) else {
        return false;
    };
    class.layout_family() == LayoutFamily::Generic
        || class
            .cpl()
            .iter()
            .any(|superclass| superclass.bits() == builtin_class_ids::FUNCALLABLE_OBJECT)
        || class.cpl().iter().any(|superclass| {
            table
                .lookup(*superclass)
                .is_some_and(|class| class.flags().applicable())
        })
}

pub(crate) fn generic_property_entry<'gc>(
    ctx: MutationContext<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    rest: Value<'gc>,
) -> Value<'gc> {
    Value::cons(ctx, Value::cons(ctx, key, value), rest)
}

pub fn generic_descriptor_from_value<'gc>(
    ctx: MutationContext<'gc>,
    value: Value<'gc>,
) -> Option<Gc<'gc, GenericDescriptor<'gc>>> {
    if value.is::<GenericDescriptor>() {
        return Some(value.downcast::<GenericDescriptor>());
    }
    if !value.is::<Closure>() {
        return None;
    }
    let closure = value.downcast::<Closure>();
    if closure.nfree > 1 {
        let descriptor = closure[1].get();
        if descriptor.is::<GenericDescriptor>() {
            return Some(descriptor.downcast::<GenericDescriptor>());
        }
    }
    let properties = closure.meta.get();
    if properties.is_pair() {
        let descriptor = properties.assq(ctx.intern("generic"))?.cdr();
        if descriptor.is::<GenericDescriptor>() {
            return Some(descriptor.downcast::<GenericDescriptor>());
        }
    }
    None
}

pub fn generic_assertion_violation<'gc>(
    ctx: MutationContext<'gc>,
    who: &'static str,
    message: &'static str,
    irritant: Value<'gc>,
) -> ReturnCode {
    let assertion_violation = ctx.hook(crate::runtime::hooks::Hook::AssertionViolation);
    ctx.stage_tail_call(
        assertion_violation,
        &[ctx.intern(who), ctx.str(message), irritant],
    )
}

extern "C" fn generic_closure_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        // `generic_procedure` builds this closure with the descriptor in its
        let direct = {
            // SAFETY: the stub only enters this proc through a closure
            // (`generic_procedure`), whose rator slot holds that closure.
            let closure = unsafe { call.rator.downcast_unchecked::<Closure>() };
            (closure.nfree > 1).then(|| closure[1].get())
        };
        let generic = match direct
            .filter(|value| value.is::<GenericDescriptor>())
            .map(|value| value.downcast::<GenericDescriptor>())
            .or_else(|| generic_descriptor_from_value(ctx, call.rator))
        {
            Some(generic) => generic,
            None => {
                return generic_assertion_violation(
                    ctx,
                    "generic-invoke",
                    "generic procedure is missing its descriptor",
                    call.rator,
                );
            }
        };
        if DISPATCH_STATS.load(Ordering::Relaxed) {
            RUST_ENTRIES.fetch_add(1, Ordering::Relaxed);
        }
        // A user discriminating function answers every call. It is
        if let Some(function) = generic.custom_dfun() {
            return ctx.stage_tail_call(function, call.args);
        }
        if !generic.flags().simple() {
            // Qualified methods combine. The effective method for these
            let epoch = generic.epoch();
            let resolved = if generic.flags().has_eql() {
                GenericDescriptor::eql_resolve(ctx, generic, call.args, epoch)
            } else {
                if let Some(emf) = GenericDescriptor::cached_emf(generic, call.args) {
                    return call.enter_in_place(emf, Some(Value::new(false)));
                }
                GenericDescriptor::applicable_methods_at(ctx, generic, call.args, epoch)
                    .map(|methods| (methods, Value::null()))
            };
            return match resolved {
                Ok((methods, emf)) => {
                    if !emf.is_null() {
                        return call.enter_in_place(emf, Some(Value::new(false)));
                    }
                    // Standard combination of primary methods (an eql generic
                    if generic.combination().is_standard()
                        && methods
                            .iter()
                            .all(|method| method.role() == MethodRole::Primary)
                    {
                        return enter_method_chain(ctx, call, generic, methods);
                    }
                    let (hook, full) =
                        effective_method_call(ctx, call.rator, epoch, &methods, call.args);
                    ctx.stage_tail_call(hook, &full)
                }
                Err(error) => dispatch_failure(ctx, call, generic, error),
            };
        }
        // Read before any lookup: fills below publish only if no cache clear
        // ran since, so a result computed against old methods is never stored.
        let epoch = generic.epoch();
        if let Some(body) = generic.default_body_at(epoch, call.args.len()) {
            return call.enter_in_place(body, Some(Value::new(false)));
        }
        if let Some(value) = GenericDescriptor::slot_access_fast(ctx, generic, call.args) {
            return call.return_one(value);
        }
        if let Some((body, cursor)) = GenericDescriptor::cursor_for_args(generic, call.args) {
            return call.enter_in_place(body, Some(cursor));
        }
        let found = match GenericDescriptor::cached_for_args(generic, call.args) {
            Some(methods) => Ok(methods),
            None => GenericDescriptor::applicable_methods_at(ctx, generic, call.args, epoch),
        };
        match found {
            Ok(methods) => {
                GenericDescriptor::classify_dfun(
                    ctx,
                    generic,
                    epoch,
                    methods.as_slice(),
                    call.args.len(),
                );
                let first = methods[0];
                GenericDescriptor::note_slot_access(ctx, generic, first, call.args, epoch);
                GenericDescriptor::note_method_entry(ctx, generic, first, call.args, epoch);
                GenericDescriptor::note_cursor_entry(ctx, generic, methods, call.args, epoch);
                enter_method_chain(ctx, call, generic, methods)
            }
            Err(error) => dispatch_failure(ctx, call, generic, error),
        }
    })
}

/// Enter the first of `methods` (most specific first, all primary, standard
#[inline]
fn enter_method_chain<'gc>(
    ctx: MutationContext<'gc>,
    call: &NativeCall<'_, 'gc>,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
) -> ReturnCode {
    let first = methods[0];
    let body = first.body();
    if !body.is::<Closure>() {
        return ctx.stage_tail_call(body, &[]);
    }
    let lead = NextMethodDescriptor::lead(ctx, generic, methods, 0, Value::empty(), call.args);
    call.enter_in_place(body, Some(lead))
}

/// The call that builds and applies the effective method of a combined
pub(crate) fn effective_method_call<'gc>(
    ctx: MutationContext<'gc>,
    gf: Value<'gc>,
    epoch: u32,
    methods: &[Gc<'gc, MethodDescriptor<'gc>>],
    args: &[Value<'gc>],
) -> (Value<'gc>, SmallVec<[Value<'gc>; 8]>) {
    let values: Vec<Value<'gc>> = methods.iter().map(|method| Value::from(*method)).collect();
    let mut full: SmallVec<[Value<'gc>; 8]> = SmallVec::new();
    full.push(gf);
    full.push(Value::new(epoch as i32));
    full.push(Value::list_from_slice(ctx, &values));
    full.extend_from_slice(args);
    (ctx.hook(crate::runtime::hooks::Hook::GenericEmf), full)
}

/// The elements of a proper Scheme list, as a vector.
pub(crate) fn list_items<'gc>(mut list: Value<'gc>) -> Vec<Value<'gc>> {
    let mut items = Vec::new();
    while list.is_pair() {
        items.push(list.car());
        list = list.cdr();
    }
    items
}

/// The outcome of a dispatch that found no method to run: the fallback
/// when the generic has one, else the assertion violation.
fn dispatch_failure<'gc>(
    ctx: MutationContext<'gc>,
    call: &NativeCall<'_, 'gc>,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    error: GenericDispatchError,
) -> ReturnCode {
    match error {
        GenericDispatchError::NoApplicableMethod if !generic.fallback().is_empty() => {
            let fallback = generic.fallback();
            if fallback.is::<Closure>() {
                call.enter_in_place(fallback, None)
            } else {
                ctx.stage_tail_call(fallback, call.args)
            }
        }
        GenericDispatchError::Arity => generic_assertion_violation(
            ctx,
            "generic-invoke",
            "not enough dispatch arguments",
            Value::new(call.args.len() as i32),
        ),
        GenericDispatchError::NoApplicableMethod => {
            // `(no-applicable-method gf . args)`: its stock method raises the error below, and a
            // method of a user metaclass may return a value instead.
            match ctx.try_hook(crate::runtime::hooks::Hook::NoApplicableMethod) {
                Some(hook) => {
                    let mut full: SmallVec<[Value<'gc>; 8]> = SmallVec::new();
                    full.push(call.rator);
                    full.extend_from_slice(call.args);
                    ctx.stage_tail_call(hook, &full)
                }
                None => generic_assertion_violation(
                    ctx,
                    "generic-invoke",
                    "no applicable method",
                    call.rator,
                ),
            }
        }
    }
}

/// The native body of the stock `make` method. Called as a method body, so
extern "C" fn make_method_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    NativeCall::invoke(ctx, sfp, |call| {
        // SAFETY: only entered through the closure `make_method_body` builds.
        let closure = unsafe { call.rator.downcast_unchecked::<Closure>() };
        let (initialize, default_body, slow) =
            (closure[1].get(), closure[2].get(), closure[3].get());
        if call.args.len() >= 2 && call.args[1].is::<super::ClassDescriptor>() {
            let class = call.args[1].downcast::<super::ClassDescriptor>();
            // A class with default initargs takes the Scheme path, which
            // supplies them before `initialize` sees the initargs.
            if class.default_initargs().is_empty()
                && generic_descriptor_from_value(ctx, initialize)
                    .is_some_and(|generic| initialize_is_plain(ctx, generic, class, default_body))
            {
                return call.run(|call| super::ops::instantiate(ctx, call, class, &call.args[2..]));
            }
        }
        ctx.stage_tail_call(slow, call.args)
    })
}

/// Whether the stock `initialize` method alone applies to instances of
fn initialize_is_plain<'gc>(
    ctx: MutationContext<'gc>,
    initialize: Gc<'gc, GenericDescriptor<'gc>>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    default_body: Value<'gc>,
) -> bool {
    use super::descriptor::{PROTOCOL_CUSTOM, PROTOCOL_PLAIN};
    if LIFECYCLE_CUSTOM.load(std::sync::atomic::Ordering::Acquire) {
        return false;
    }
    match class.protocol_state(OP_INIT) {
        PROTOCOL_PLAIN => return true,
        PROTOCOL_CUSTOM => return false,
        _ => {}
    }
    let _ = ctx;
    let epoch = initialize.epoch();
    let methods = initialize.methods();
    let mut applicable = methods
        .iter()
        .copied()
        .filter(|method| method.may_apply_to_classes(&[class]));
    let plain = matches!(
        (applicable.next(), applicable.next()),
        (Some(method), None) if method.body().bits() == default_body.bits()
    );
    class.publish_protocol_state(
        OP_INIT,
        if plain {
            PROTOCOL_PLAIN
        } else {
            PROTOCOL_CUSTOM
        },
    );
    if initialize.epoch() != epoch {
        class.reset_protocol_state(OP_INIT);
        return false;
    }
    plain
}

/// A `make` method body for `(make (class <class>) . initargs)`, see
/// [`make_method_proc`].
pub(crate) fn make_method_body<'gc>(
    ctx: MutationContext<'gc>,
    initialize: Value<'gc>,
    default_body: Value<'gc>,
    slow: Value<'gc>,
) -> Value<'gc> {
    PROCEDURES
        .fetch(ctx)
        .make_closure(
            ctx,
            make_method_proc,
            [initialize, default_body, slow],
            Value::null(),
        )
        .into()
}

pub fn generic_procedure<'gc>(
    ctx: MutationContext<'gc>,
    name: Value<'gc>,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
) -> Gc<'gc, Closure<'gc>> {
    let properties = generic_property_entry(
        ctx,
        ctx.intern("name"),
        name,
        generic_property_entry(ctx, ctx.intern("generic"), generic.into(), Value::null()),
    );
    let closure =
        PROCEDURES
            .fetch(ctx)
            .make_closure(ctx, generic_closure_proc, [generic.into()], properties);
    // Generics enter through the machine `generic_call` stub, which serves a
    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    if let Some(addr) = crate::runtime::codegen::template_image::generic_call_if_loaded() {
        // SAFETY: the closure is fresh and unshared, `code` is only read at
        // call time.
        unsafe {
            closure
                .as_gc_object()
                .to_address()
                .as_mut_ref::<Closure>()
                .code = mmtk::util::Address::from_ptr(addr);
        }
    }
    closure
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct MethodDescriptor<'gc> {
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    /// The class part of each specializer: for an `eql` position, the class
    /// of the eql object.
    specializers: ArrayRef<'gc, ClassId>,
    /// Per specializer position, the eql object or `Value::empty()`. `None`
    /// when no position is eql (the common case).
    eql: Option<ArrayRef<'gc, Value<'gc>>>,
    required_arg_count: usize,
    body: Lock<Value<'gc>>,
    flags: Lock<MethodFlags>,
    /// The slot a `SLOT_READER`/`SLOT_WRITER` body accesses (a symbol), else empty.
    slot_name: Lock<Value<'gc>>,
    /// The qualifier keywords as written, a Scheme list (`()` for a primary).
    qualifiers: Value<'gc>,
    /// The method's part in the standard combination, derived from
    /// `qualifiers` when the method is added.
    role: Lock<MethodRole>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Trace)]
pub struct MethodFlags(u8);

impl MethodFlags {
    const LOCKED: u8 = 1 << 0;
    /// The body never reads its next-method argument, so dispatch passes
    /// `#f` there instead of allocating a `NextMethodDescriptor`.
    const NO_NEXT: u8 = 1 << 1;
    /// The body is exactly `(slot-ref obj 'name)`: dispatch may read the slot
    /// directly (see [`GenericDescriptor::slot_access_fast`]).
    const SLOT_READER: u8 = 1 << 2;
    /// The body is exactly `(slot-set! obj 'name v) v`.
    const SLOT_WRITER: u8 = 1 << 3;
    /// One of the stock methods of a slot protocol generic, marked after
    const BOOT_SLOT_PROTOCOL: u8 = 1 << 4;
    /// The body reads its next-method argument only through explicit-argument
    const NEXT_CURSOR: u8 = 1 << 5;
    /// A method the boot library defines for a protocol generic whose overrides the runtime
    /// checks for (see `%stock-methods-only?`).
    const STOCK: u8 = 1 << 6;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn locked_flag() -> Self {
        Self(Self::LOCKED)
    }

    pub const fn locked(self) -> bool {
        self.0 & Self::LOCKED != 0
    }

    pub const fn with_locked(self) -> Self {
        Self(self.0 | Self::LOCKED)
    }

    pub const fn with_no_next(self) -> Self {
        Self(self.0 | Self::NO_NEXT)
    }

    pub const fn no_next(self) -> bool {
        self.0 & Self::NO_NEXT != 0
    }

    pub const fn with_next_cursor(self) -> Self {
        Self(self.0 | Self::NEXT_CURSOR)
    }

    pub const fn next_cursor(self) -> bool {
        self.0 & Self::NEXT_CURSOR != 0
    }

    pub const fn with_slot_reader(self) -> Self {
        Self(self.0 | Self::SLOT_READER)
    }

    pub const fn with_slot_writer(self) -> Self {
        Self(self.0 | Self::SLOT_WRITER)
    }

    pub const fn with_boot_slot_protocol(self) -> Self {
        Self(self.0 | Self::BOOT_SLOT_PROTOCOL)
    }

    pub const fn with_stock(self) -> Self {
        Self(self.0 | Self::STOCK)
    }

    pub const fn stock(self) -> bool {
        self.0 & Self::STOCK != 0
    }

    pub const fn boot_slot_protocol(self) -> bool {
        self.0 & Self::BOOT_SLOT_PROTOCOL != 0
    }

    pub const fn slot_reader(self) -> bool {
        self.0 & Self::SLOT_READER != 0
    }

    pub const fn slot_writer(self) -> bool {
        self.0 & Self::SLOT_WRITER != 0
    }
}

/// The part a method plays in the standard method combination, derived from
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(require_static)]
#[repr(u8)]
pub enum MethodRole {
    Primary = 0,
    Before = 1,
    After = 2,
    Around = 3,
    Other = 4,
}

impl MethodRole {
    /// The role of a method with this qualifier list (a Scheme list of
    /// keywords; `()` is primary).
    pub(crate) fn of_qualifiers<'gc>(qualifiers: Value<'gc>) -> Self {
        if qualifiers.is_null() {
            return MethodRole::Primary;
        }
        if !qualifiers.is_pair() || !qualifiers.cdr().is_null() {
            return MethodRole::Other;
        }
        match qualifier_symbol(qualifiers.car()) {
            Some(symbol) => match symbol.as_str().as_ref() {
                "before" => MethodRole::Before,
                "after" => MethodRole::After,
                "around" => MethodRole::Around,
                _ => MethodRole::Other,
            },
            None => MethodRole::Other,
        }
    }

    /// The role under a combination: the standard one's roles when
    pub(crate) fn for_combination<'gc>(standard: bool, qualifiers: Value<'gc>) -> Self {
        let role = Self::of_qualifiers(qualifiers);
        if standard || role == MethodRole::Around {
            role
        } else {
            MethodRole::Other
        }
    }

    /// The name `%method-role` reports.
    pub fn name(self) -> &'static str {
        match self {
            MethodRole::Primary => "primary",
            MethodRole::Before => "before",
            MethodRole::After => "after",
            MethodRole::Around => "around",
            MethodRole::Other => "other",
        }
    }
}

/// The symbol a qualifier keyword names, `None` for any other value.
fn qualifier_symbol<'gc>(
    value: Value<'gc>,
) -> Option<Gc<'gc, crate::runtime::objectmodel::Symbol<'gc>>> {
    value.try_as::<Keyword>().map(|keyword| keyword.symbol)
}

/// Whether two qualifier lists name the same keywords, in the same order
/// (keywords compare by their interned symbol).
pub(crate) fn same_qualifiers<'gc>(left: Value<'gc>, right: Value<'gc>) -> bool {
    let (mut left, mut right) = (left, right);
    loop {
        match (left.is_pair(), right.is_pair()) {
            (false, false) => return left.is_null() && right.is_null(),
            (true, true) => {
                match (qualifier_symbol(left.car()), qualifier_symbol(right.car())) {
                    (Some(a), Some(b)) if Gc::ptr_eq(a, b) => {}
                    _ => return false,
                }
                left = left.cdr();
                right = right.cdr();
            }
            _ => return false,
        }
    }
}

/// Why the method set of a generic changed: the single reason
/// [`GenericDescriptor::invalidate`] takes (AMOP's four triggers).
pub(crate) enum Reason<'gc> {
    AddMethod(Gc<'gc, MethodDescriptor<'gc>>),
    RemoveMethod(Gc<'gc, MethodDescriptor<'gc>>),
    Reinitialize,
    ClassChanged(Gc<'gc, ClassDescriptor<'gc>>),
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct NextMethodDescriptor<'gc> {
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    args: ArrayRef<'gc, Value<'gc>>,
    index: usize,
    /// The procedure an exhausted chain tail-calls with the args, instead of
    tail: Value<'gc>,
    /// A cursor: `args` is empty and only the chain position is meaningful.
    cursor: bool,
}

impl<'gc> GenericDescriptor<'gc> {
    pub fn new(
        ctx: MutationContext<'gc>,
        name: &str,
        max_required_dispatch_args: usize,
    ) -> Gc<'gc, Self> {
        Self::new_with_header(
            ctx,
            name,
            max_required_dispatch_args,
            super::builtin::builtin_id(builtin_class_ids::GENERIC),
        )
    }

    /// A generic whose class is `class`, a generic-function class defined
    pub fn new_in_class(
        ctx: MutationContext<'gc>,
        name: &str,
        max_required_dispatch_args: usize,
        class: ClassId,
    ) -> Gc<'gc, Self> {
        Self::new_with_header(ctx, name, max_required_dispatch_args, class)
    }

    fn new_with_header(
        ctx: MutationContext<'gc>,
        name: &str,
        max_required_dispatch_args: usize,
        class: ClassId,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name: Array::from_slice(ctx, name.as_bytes()),
                methods: Lock::new(empty_method_metadata(ctx)),
                dispatch_table: Lock::new(empty_dispatch_table(ctx)),
                dispatch_width: Lock::new(1),
                dispatch_arity: Lock::new(0),
                slot_cache: Lock::new(empty_slot_cache(ctx)),
                method_cache: Lock::new(empty_method_cache(ctx)),
                cursor_cache: Lock::new(empty_cursor_cache(ctx)),
                max_required_dispatch_args,
                fallback: Lock::new(Value::empty()),
                flags: Lock::new(GenericFlags::empty().with_simple()),
                epoch: Lock::new(0),
                combination: Lock::new(standard_combination(ctx)),
                precedence: Lock::new(None),
                change_wp: Lock::new(WatchpointSet::new(ctx)),
                dependents: Lock::new(Value::null()),
                dfun_state: Lock::new(DfunState::Initial),
                default_entry: Lock::new(Value::null()),
                custom_dfun: Lock::new(Value::empty()),
            },
            class_header_word(class),
        )
    }

    /// The installed discriminating function, if any.
    pub(crate) fn custom_dfun(&self) -> Option<Value<'gc>> {
        let function = self.custom_dfun.get();
        (!function.is_empty()).then_some(function)
    }

    /// Install `function` as the discriminating function of `generic`, or
    pub fn set_custom_dfun(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        function: Option<Value<'gc>>,
    ) {
        Self::begin_change(ctx, generic);
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, custom_dfun)
            .unlock()
            .set(function.unwrap_or(Value::empty()));
        Self::invalidate(ctx, generic, Reason::Reinitialize);
    }

    pub fn name(&self) -> &str {
        // SAFETY: The byte content is known to be valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.name.as_slice()) }
    }

    pub fn methods(&self) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods.get()
    }

    pub fn max_required_dispatch_args(&self) -> usize {
        self.max_required_dispatch_args
    }

    pub fn fallback(&self) -> Value<'gc> {
        self.fallback.get()
    }

    /// The method combination this generic uses.
    pub fn combination(&self) -> Gc<'gc, MethodCombination<'gc>> {
        self.combination.get()
    }

    /// The argument precedence order, if one was set.
    pub fn precedence(&self) -> Option<ArrayRef<'gc, u8>> {
        self.precedence.get()
    }

    /// The change watchpoint, see the field. Tier 2 registers its code with
    /// [`WatchpointSet::add`] and rechecks the epoch after it.
    pub fn change_wp(&self) -> Gc<'gc, WatchpointSet<'gc>> {
        self.change_wp.get()
    }

    /// The AMOP dependents, see the field.
    pub fn dependents(&self) -> Value<'gc> {
        self.dependents.get()
    }

    /// Replace the dependents list, see the field.
    pub fn set_dependents(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        dependents: Value<'gc>,
    ) {
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, dependents)
            .unlock()
            .set(dependents);
    }

    /// Fire the change watchpoint and install a fresh set: the method set
    /// may hold again. Returns the dependents cleared. The caller owes grace.
    fn renew_change_wp(ctx: MutationContext<'gc>, generic: Gc<'gc, Self>) -> usize {
        let old = generic.change_wp.get();
        let fresh = WatchpointSet::new(ctx);
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, change_wp)
            .unlock()
            .set(fresh);
        WatchpointSet::fire(old, ctx)
    }

    /// Store an argument precedence order (positions, most significant first).
    /// Dispatch does not use it yet, so this only invalidates the caches.
    pub fn set_precedence(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        precedence: Option<ArrayRef<'gc, u8>>,
    ) {
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, precedence)
            .unlock()
            .set(precedence);
        Self::invalidate(ctx, generic, Reason::Reinitialize);
    }

    /// Use `combination` for this generic. Every method's role is derived
    /// again under it, so the change is one invalidation.
    pub fn set_combination(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        combination: Gc<'gc, MethodCombination<'gc>>,
    ) {
        Self::begin_change(ctx, generic);
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, combination)
            .unlock()
            .set(combination);
        let standard = combination.is_standard();
        for method in generic.methods().iter().copied() {
            let role = MethodRole::for_combination(standard, method.qualifiers());
            MethodDescriptor::set_role(ctx, method, role);
        }
        Self::invalidate(ctx, generic, Reason::Reinitialize);
    }

    pub fn flags(&self) -> GenericFlags {
        self.flags.get()
    }

    /// Whether the slot-access cache holds at least one resolved plain slot.
    /// Tier 2 emits its slot fast path only when it does.
    pub fn has_slot_cache(&self) -> bool {
        !self.slot_cache.get().is_empty()
    }

    /// Whether the `NO_NEXT` method cache holds at least one entry. Tier 2
    /// emits its method fast path only when it does.
    pub fn has_method_cache(&self) -> bool {
        !self.method_cache.get().is_empty()
    }

    /// The cache epoch (Acquire). A filler reads it before computing a
    /// result and publishes only if it is unchanged afterwards.
    pub fn epoch(&self) -> u32 {
        self.epoch_atomic().load(Ordering::Acquire)
    }

    /// Whether a filler that read `epoch` may still publish: no cache clear
    #[inline]
    fn epoch_is_current(&self, epoch: u32) -> bool {
        epoch & 1 == 0 && self.epoch() == epoch
    }

    #[inline]
    fn epoch_atomic(&self) -> &AtomicU32 {
        // SAFETY: `Lock<u32>` is `repr(transparent)` over `Cell<u32>`, which has
        unsafe { &*(self.epoch.as_ptr() as *const AtomicU32) }
    }

    /// Seal or unseal `generic`. Sealing only sets the flag. Unsealing is a
    pub fn set_sealed(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        sealed: bool,
    ) {
        if sealed {
            let flags = generic.flags().with_sealed();
            barrier::field!(Gc::write(ctx, generic), GenericDescriptor, flags)
                .unlock()
                .set(flags);
            return;
        }
        if !generic.flags().sealed() {
            return;
        }
        Self::begin_change(ctx, generic);
        let flags = generic.flags().without_sealed();
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, flags)
            .unlock()
            .set(flags);
        Self::invalidate(ctx, generic, Reason::Reinitialize);
    }

    /// Give `generic` its protocol role: `initialize` when `initialize` is true, else one of the
    /// slot protocol generics. Called once for each boot generic; the flags, not the name, tell
    /// the runtime which generics get the special slot-cache treatment.
    pub fn mark_protocol_role(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        role: ProtocolRole,
    ) {
        let flags = match role {
            ProtocolRole::Initialize => generic.flags().with_initialize(),
            ProtocolRole::SlotProtocol => generic.flags().with_slot_protocol(),
            ProtocolRole::Lifecycle => generic.flags().with_lifecycle(),
        };
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, flags)
            .unlock()
            .set(flags);
    }

    /// Empty every cache of `generic` and run the `Reinitialize` invalidation,
    pub fn flush_cache(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        Self::begin_change(ctx, generic);
        Self::invalidate(ctx, generic, Reason::Reinitialize);
    }

    /// Fill the dispatch table for arguments of the classes `ids` (one per
    pub(crate) fn precompute_key(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        ids: &[ClassId],
    ) -> bool {
        if ids.is_empty()
            || ids.len() != generic.dispatch_width_value() as usize
            || !Self::emf_cacheable(generic)
        {
            return false;
        }
        let epoch = generic.epoch();
        let primary_only = generic.flags().simple();
        let Some(methods) = generic.applicable_for_class_ids(ctx, ids, primary_only) else {
            return false;
        };
        let arity = generic.arity_key(ids.len());
        Self::insert_dispatcher_cache(ctx, generic, ids, arity, methods, epoch);
        true
    }

    /// The applicable methods for arguments of the classes `ids`, ordered as
    pub(crate) fn applicable_for_class_ids(
        &self,
        ctx: MutationContext<'gc>,
        ids: &[ClassId],
        primary_only: bool,
    ) -> Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>> {
        if ids.len() < self.max_required_dispatch_args {
            return None;
        }
        let mut methods = Vec::new();
        for method in self.methods().iter().copied() {
            if primary_only && method.role() != MethodRole::Primary {
                continue;
            }
            let Some(score) = method.class_specificity_score(ctx, ids) else {
                continue;
            };
            methods.push((method, score));
        }
        if methods.is_empty() {
            return None;
        }
        methods.sort_by(|(_, left), (_, right)| left.cmp(right));
        Some(Array::from_slice(
            ctx,
            methods
                .iter()
                .map(|(method, _)| *method)
                .collect::<Vec<_>>(),
        ))
    }

    pub fn set_fallback(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        fallback: Value<'gc>,
    ) {
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, fallback)
            .unlock()
            .set(fallback);
        Self::invalidate(ctx, generic, Reason::Reinitialize);
        // Slot caches require protocol generics to have no fallback; the
        // invalidation above fired their protocol.
        if generic.flags().initialize() {
            class_table(ctx).reset_protocol_state_matching(OP_INIT, |_| true);
        }
    }

    pub fn add_method(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        Self::add_method_with_flags(
            ctx,
            generic,
            specializers,
            &[],
            required_arg_count,
            body,
            MethodFlags::empty(),
            Value::null(),
        )
    }

    /// Add (or replace) a method. `specializers` holds the class part of each
    #[allow(clippy::too_many_arguments)]
    pub fn add_method_with_flags(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        eql: &[Value<'gc>],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
        qualifiers: Value<'gc>,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        Self::add_method_in_table(
            ctx,
            class_table(ctx),
            generic,
            specializers,
            eql,
            required_arg_count,
            body,
            flags,
            qualifiers,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_method_in_table(
        ctx: MutationContext<'gc>,
        table: &super::table::ClassTable<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        eql: &[Value<'gc>],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
        qualifiers: Value<'gc>,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        assert!(
            specializers.len() <= required_arg_count,
            "method cannot specialize more arguments than it requires"
        );
        assert!(
            eql.is_empty() || eql.len() == specializers.len(),
            "eql positions must match the specializers"
        );
        assert!(
            !generic.flags().sealed(),
            "sealed generic cannot accept methods"
        );
        Self::begin_change(ctx, generic);
        if let Some(method) = generic.find_method(specializers, eql, required_arg_count, qualifiers)
        {
            MethodDescriptor::set_body(ctx, method, body);
            MethodDescriptor::set_flags(ctx, method, flags);
            Self::invalidate(ctx, generic, Reason::AddMethod(method));
            return method;
        }
        let method = MethodDescriptor::new(
            ctx,
            generic,
            specializers,
            eql,
            required_arg_count,
            body,
            flags,
            qualifiers,
        );
        Self::append_method(ctx, generic, method);
        Self::invalidate(ctx, generic, Reason::AddMethod(method));
        for (position, specializer) in specializers.iter().enumerate() {
            match eql.get(position).filter(|object| !object.is_empty()) {
                Some(object) => {
                    let specializer = EqlSpecializer::intern(ctx, *object);
                    EqlSpecializer::add_direct_method(ctx, specializer, method.into());
                }
                None => {
                    if let Some(class) = table.lookup(*specializer) {
                        ClassDescriptor::add_direct_method(ctx, class, method.into());
                    }
                }
            }
        }
        method
    }

    /// The method with these specializers (classes, and eql objects compared
    pub fn find_method(
        &self,
        specializers: &[ClassId],
        eql: &[Value<'gc>],
        required_arg_count: usize,
        qualifiers: Value<'gc>,
    ) -> Option<Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods().iter().copied().find(|method| {
            method.required_arg_count() == required_arg_count
                && method.specializers() == specializers
                && same_qualifiers(method.qualifiers(), qualifiers)
                && (0..specializers.len()).all(|position| {
                    match (
                        method.eql_at(position),
                        eql.get(position).filter(|object| !object.is_empty()),
                    ) {
                        (None, None) => true,
                        (Some(left), Some(right)) => left.eqv(*right),
                        _ => false,
                    }
                })
        })
    }

    pub fn select_method(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Gc<'gc, MethodDescriptor<'gc>>, GenericDispatchError> {
        self.slow_applicable_methods(ctx, args)
            .map(|methods| methods[0])
    }

    pub fn dispatch_body(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, GenericDispatchError> {
        match self.select_method(ctx, args) {
            Ok(method) => Ok(method.body()),
            Err(GenericDispatchError::NoApplicableMethod) if !self.fallback().is_empty() => {
                Ok(self.fallback())
            }
            Err(error) => Err(error),
        }
    }

    pub fn invocation(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<GenericInvocation<'gc>, GenericDispatchError> {
        match Self::next_method_chain(ctx, generic, args) {
            Ok(next_method) => Ok(GenericInvocation {
                body: next_method.body(),
                next: Some(next_method.into()),
                args: next_method.args(),
            }),
            Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                Ok(GenericInvocation {
                    body: generic.fallback(),
                    next: None,
                    args: Array::from_slice(ctx, args),
                })
            }
            Err(error) => Err(error),
        }
    }

    pub fn dispatch_body_cached(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, GenericDispatchError> {
        match Self::applicable_methods(ctx, generic, args) {
            Ok(methods) => Ok(methods[0].body()),
            Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                Ok(generic.fallback())
            }
            Err(error) => Err(error),
        }
    }

    /// Dispatch `generic` on `args` and tail-call the first applicable
    pub(crate) fn tail_effective(
        ctx: MutationContext<'gc>,
        call: &crate::runtime::vm::NativeCall<'_, 'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        // A combined generic's first method is not its effective method.
        if !generic.flags().simple() {
            return None;
        }
        let methods = Self::applicable_methods(ctx, generic, args).ok()?;
        let first = methods[0];
        let body = first.body();
        if !body.is::<Closure>() {
            return None;
        }
        let lead = NextMethodDescriptor::lead(ctx, generic, methods, 0, Value::empty(), args);
        let mut full: SmallVec<[Value<'gc>; 6]> = SmallVec::new();
        full.push(lead);
        full.extend_from_slice(args);
        Some(call.tail(body, &full))
    }

    pub fn next_method_chain(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<Gc<'gc, NextMethodDescriptor<'gc>>, GenericDispatchError> {
        let methods = Self::applicable_methods(ctx, generic, args)?;
        Ok(NextMethodDescriptor::new(
            ctx,
            generic,
            methods,
            Array::from_slice(ctx, args),
            0,
        ))
    }

    pub(crate) fn applicable_methods(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        let epoch = generic.epoch();
        Self::applicable_methods_at(ctx, generic, args, epoch)
    }

    /// [`Self::applicable_methods`] for a caller that read `epoch` before
    /// doing any lookup of its own.
    pub(crate) fn applicable_methods_at(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        if args.len() < generic.max_required_dispatch_args {
            return Err(GenericDispatchError::Arity);
        }
        // A generic keyed by resolved class ids keeps its dispatch table
        let primary_only = generic.flags().simple();
        if !GenericDescriptor::emf_cacheable(generic) {
            return generic.applicable_with_roles(ctx, args, primary_only);
        }
        if generic.flags().has_eql() {
            return Self::eql_applicable_methods_at(ctx, generic, args, epoch);
        }
        let width = (generic.dispatch_width_value() as usize).min(args.len());
        let arity = generic.arity_key(args.len());
        // Stack key: the hit path allocates nothing. Only a miss builds the
        let mut class_ids: SmallVec<[ClassId; 4]> = SmallVec::new();
        for arg in &args[..width] {
            let Some(class_id) = Self::dispatch_class_id(*arg) else {
                return Err(GenericDispatchError::NoApplicableMethod);
            };
            class_ids.push(class_id);
        }
        if let Some(methods) = Self::cached_for_args(generic, args) {
            return Ok(methods);
        }
        let methods = generic.applicable_with_roles(ctx, args, primary_only)?;
        Self::insert_dispatcher_cache(ctx, generic, &class_ids, arity, methods, epoch);
        Ok(methods)
    }

    /// Whether `generic`'s dispatch is keyed by plain class ids: the table
    fn emf_cacheable(generic: Gc<'gc, GenericDescriptor<'gc>>) -> bool {
        !generic.flags().resolved_keys()
    }

    /// The applicable methods of an eql generic for `args`, every role, most
    fn eql_applicable_methods_at(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        Self::eql_resolve(ctx, generic, args, epoch).map(|(methods, _)| methods)
    }

    /// [`Self::eql_applicable_methods_at`] with the cached effective method
    pub(crate) fn eql_resolve(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) -> Result<(ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, Value<'gc>), GenericDispatchError>
    {
        let Some((key, rest)) = Self::lookup_key(generic, args) else {
            return Err(GenericDispatchError::NoApplicableMethod);
        };
        let table = generic.dispatch_table.get();
        let bucket = match Self::probe(table, key, &rest).copied() {
            Some(bucket) if bucket.eql.is_some() => bucket,
            // Never happens for a generic with eql methods (the table is
            // wiped by every change that sets the flag); answer uncached.
            Some(_) => {
                return generic
                    .applicable_with_roles(ctx, args, false)
                    .map(|methods| (methods, Value::null()));
            }
            None => {
                let Some(fresh) = Self::eql_bucket(ctx, generic, args, key, &rest) else {
                    return generic
                        .applicable_with_roles(ctx, args, false)
                        .map(|methods| (methods, Value::null()));
                };
                Self::publish_bucket(ctx, generic, epoch, key, &rest, |existing| {
                    existing.is_none().then_some(fresh)
                });
                fresh
            }
        };
        let candidates: &[Gc<'gc, MethodDescriptor<'gc>>] = match &bucket.eql {
            Some(eql) => eql.as_slice(),
            None => &[],
        };
        let mask = Self::eql_match_mask(candidates, args);
        let Some(plain) = bucket.methods else {
            return Err(GenericDispatchError::NoApplicableMethod);
        };
        if mask == 0 {
            if plain.is_empty() {
                return Err(GenericDispatchError::NoApplicableMethod);
            }
            return Ok((plain, bucket.emf));
        }
        if let Some(entry) = bucket
            .memo
            .and_then(|memo| memo.iter().find(|entry| entry.mask == mask).copied())
        {
            return Ok((entry.methods, entry.emf));
        }
        let mut set: Vec<Gc<'gc, MethodDescriptor<'gc>>> = plain.iter().copied().collect();
        set.extend(
            candidates
                .iter()
                .enumerate()
                .filter(|(index, _)| mask & (1 << index) != 0)
                .map(|(_, method)| *method),
        );
        let methods = Array::from_slice(ctx, generic.rank_methods(ctx, args, &set, true));
        Self::publish_bucket(ctx, generic, epoch, key, &rest, |existing| {
            let mut bucket = *existing?;
            let mut entries: Vec<EqlMemoEntry<'gc>> = bucket
                .memo
                .map_or_else(Vec::new, |memo| memo.as_slice().to_vec());
            if entries.len() >= MAX_EQL_MEMO || entries.iter().any(|entry| entry.mask == mask) {
                return None;
            }
            entries.push(EqlMemoEntry {
                mask,
                methods,
                emf: Value::null(),
            });
            bucket.memo = Some(Array::from_slice(ctx, &entries));
            Some(bucket)
        });
        Ok((methods, Value::null()))
    }

    /// A fresh eql bucket for the classes of `args` (see
    fn eql_bucket(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
        key: u64,
        rest: &[ClassId],
    ) -> Option<DispatchBucket<'gc>> {
        let mut candidates = Vec::new();
        let mut plain = Vec::new();
        for method in generic.methods().iter().copied() {
            // Class parts only: an eql position is not checked here.
            if method.score(ctx, args, plain_key, false).is_none() {
                continue;
            }
            if method.eql.is_some() {
                candidates.push(method);
            } else {
                plain.push(method);
            }
        }
        if candidates.len() > MAX_EQL_CANDIDATES {
            return None;
        }
        let plain = generic.rank_methods(ctx, args, &plain, false);
        let (flags, body, eql_direct) =
            match Self::eql_direct_entries(ctx, generic, args, &candidates, &plain) {
                Some((position, entries, default)) => {
                    let mut flags = BUCKET_EQL_DIRECT;
                    if position == 1 {
                        flags |= BUCKET_EQL_POS1;
                    }
                    if entries.len() > 1 {
                        flags |= BUCKET_EQL_MULTI;
                    }
                    if default.is_some() {
                        flags |= BUCKET_EQL_DEFAULT;
                    }
                    (
                        flags,
                        default.unwrap_or_else(Value::null),
                        Some(Array::from_slice(ctx, &entries)),
                    )
                }
                None => (0, Value::null(), None),
            };
        Some(DispatchBucket {
            key,
            flags,
            rest: (!rest.is_empty()).then(|| Array::from_slice(ctx, rest)),
            methods: Some(Array::from_slice(ctx, &plain)),
            body,
            emf: Value::null(),
            eql: Some(Array::from_slice(ctx, &candidates)),
            memo: None,
            eql_direct,
        })
    }

    /// The stub-resolved form of a fresh eql bucket, or `None` when the stub
    fn eql_direct_entries(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
        candidates: &[Gc<'gc, MethodDescriptor<'gc>>],
        plain: &[Gc<'gc, MethodDescriptor<'gc>>],
    ) -> Option<(u32, Vec<EqlDirectEntry<'gc>>, Option<Value<'gc>>)> {
        // SIMPLE is off for an eql generic (its eql methods are not simple),
        let flags = generic.flags();
        if flags.resolved_keys()
            || generic.custom_dfun().is_some()
            || !generic.combination().is_standard()
            || !generic
                .methods()
                .iter()
                .all(|method| method.role() == MethodRole::Primary)
        {
            return None;
        }
        if candidates.is_empty() || candidates.len() > MAX_EQL_DIRECT {
            return None;
        }
        let mut position = None;
        let mut entries: Vec<EqlDirectEntry<'gc>> = Vec::with_capacity(candidates.len());
        for method in candidates {
            let mut eql_positions =
                (0..method.specializers.len()).filter(|&index| method.eql_at(index).is_some());
            let (Some(index), None) = (eql_positions.next(), eql_positions.next()) else {
                return None;
            };
            if index > 1 || position.is_some_and(|known| known != index as u32) {
                return None;
            }
            position = Some(index as u32);
            let object = method.eql_at(index)?;
            let immediate =
                object.is_int32() || object.is_char() || object.is_bool() || object.is_null();
            if !immediate || !method.flags().no_next() || !method.body().is::<Closure>() {
                return None;
            }
            let word = object.raw_i64() as u64;
            if entries.iter().any(|entry| entry.word == word) {
                return None;
            }
            let mut set = vec![*method];
            set.extend_from_slice(plain);
            let ranked = generic.rank_methods(ctx, args, &set, false);
            if !ranked
                .first()
                .is_some_and(|first| Gc::ptr_eq(*first, *method))
            {
                return None;
            }
            entries.push(EqlDirectEntry {
                word,
                body: method.body(),
            });
        }
        let default = plain
            .first()
            .filter(|first| first.flags().no_next() && first.body().is::<Closure>())
            .map(|first| first.body());
        Some((position?, entries, default))
    }

    /// The bit `i` set when candidate `i` of an eql bucket matches `args`
    /// (every eql position `eqv?` its object).
    fn eql_match_mask(candidates: &[Gc<'gc, MethodDescriptor<'gc>>], args: &[Value<'gc>]) -> u64 {
        candidates
            .iter()
            .enumerate()
            .filter(|(_, method)| method.eql_matches(args))
            .fold(0, |mask, (index, _)| mask | (1 << index))
    }

    /// The highest argument count any method distinguishes (see
    /// `dispatch_arity`).
    pub(crate) fn dispatch_width_value(&self) -> u32 {
        // SAFETY: a word-sized `Lock<u32>` written only by `append_method`.
        unsafe { (*(self.dispatch_width.as_ptr() as *const AtomicU32)).load(Ordering::Acquire) }
    }

    /// The arity part of a dispatch key for a call with `argc` arguments.
    fn arity_key(&self, argc: usize) -> u32 {
        // SAFETY: as in `dispatch_width_value`.
        let cap = unsafe {
            (*(self.dispatch_arity.as_ptr() as *const AtomicU32)).load(Ordering::Acquire)
        };
        (argc as u32).min(cap)
    }

    /// The (width, arity) a method needs from the dispatch key: the highest
    /// specialized position plus one, and the argument count it requires.
    fn method_shape(method: &MethodDescriptor<'_>) -> (u32, u32) {
        let top = ClassId::new(builtin_class_ids::TOP);
        let specialized = method
            .specializers
            .iter()
            .rposition(|specializer| Some(*specializer) != top)
            .map_or(0, |position| position + 1) as u32;
        let needed = (method.required_arg_count as u32)
            .max(method.specializers.len() as u32)
            .min(MAX_ARITY_KEY);
        (specialized, needed)
    }

    /// Recompute the key shape after a method was added: the width reaches
    fn update_dispatch_shape(&self, method: &MethodDescriptor<'gc>) {
        let (specialized, needed) = Self::method_shape(method);
        // SAFETY: as in `dispatch_width_value`; `fetch_max` is atomic, so
        // concurrent adds need no lock.
        unsafe {
            (*(self.dispatch_width.as_ptr() as *const AtomicU32))
                .fetch_max(specialized.max(1), Ordering::AcqRel);
            (*(self.dispatch_arity.as_ptr() as *const AtomicU32))
                .fetch_max(needed, Ordering::AcqRel);
        }
    }

    /// Recompute the key shape from the whole method set, after a removal
    fn recompute_dispatch_shape(&self) {
        let (mut width, mut arity) = (1u32, 0u32);
        for method in self.methods().iter() {
            let (specialized, needed) = Self::method_shape(method);
            width = width.max(specialized);
            arity = arity.max(needed);
        }
        // SAFETY: as in `dispatch_width_value`.
        unsafe {
            (*(self.dispatch_width.as_ptr() as *const AtomicU32)).store(width, Ordering::Release);
            (*(self.dispatch_arity.as_ptr() as *const AtomicU32)).store(arity, Ordering::Release);
        }
    }

    /// The cached `NO_NEXT` method body for arguments that are all heap cells
    pub(crate) fn cached_method_body(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        if !(1..=2).contains(&args.len()) {
            return None;
        }
        let cache = generic.method_cache.get();
        if cache.is_empty() {
            return None;
        }
        let class_of = |value: Value<'gc>| -> Option<u32> {
            value
                .is_cell()
                .then(|| value.as_cell_raw().class_id().bits())
        };
        let class0 = class_of(args[0])?;
        let class1 = match args.get(1) {
            Some(value) => class_of(*value)?,
            None => 0,
        };
        let argc = args.len() as u32;
        cache
            .iter()
            .find(|entry| entry.argc == argc && entry.class0 == class0 && entry.class1 == class1)
            .map(|entry| entry.body)
    }

    /// The chain cursor body and its cursor for calls with these arguments,
    pub(crate) fn cursor_for_args(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<(Value<'gc>, Value<'gc>)> {
        if !(1..=2).contains(&args.len()) {
            return None;
        }
        let cache = generic.cursor_cache.get();
        if cache.is_empty() {
            return None;
        }
        let class_of = |value: Value<'gc>| -> Option<u32> {
            value
                .is_cell()
                .then(|| value.as_cell_raw().class_id().bits())
        };
        let class0 = class_of(args[0])?;
        let class1 = match args.get(1) {
            Some(value) => class_of(*value)?,
            None => 0,
        };
        let argc = args.len() as u32;
        cache
            .iter()
            .find(|entry| entry.argc == argc && entry.class0 == class0 && entry.class1 == class1)
            .map(|entry| (entry.body, entry.cursor))
    }

    /// Every cursor cache entry for calls of `argc` arguments, as
    pub(crate) fn cursor_entries(&self, argc: usize) -> Option<(u32, Vec<CursorSnapshot<'gc>>)> {
        let epoch = self.epoch();
        if epoch & 1 != 0 {
            return None;
        }
        let entries: Vec<CursorSnapshot<'gc>> = self
            .cursor_cache
            .get()
            .iter()
            .filter(|entry| entry.argc as usize == argc)
            .map(|entry| (entry.class0, entry.class1, entry.body, entry.cursor))
            .collect();
        (self.epoch() == epoch).then_some((epoch, entries))
    }

    /// Every `NO_NEXT` method cache entry for calls of `argc` arguments, as
    pub(crate) fn method_cache_entries(
        &self,
        argc: usize,
    ) -> Option<(u32, Vec<MethodSnapshot<'gc>>)> {
        let epoch = self.epoch();
        if epoch & 1 != 0 {
            return None;
        }
        let entries: Vec<MethodSnapshot<'gc>> = self
            .method_cache
            .get()
            .iter()
            .filter(|entry| entry.argc as usize == argc)
            .map(|entry| (entry.class0, entry.class1, entry.body))
            .collect();
        (self.epoch() == epoch).then_some((epoch, entries))
    }

    /// The sole effective method of a combined generic for calls of `argc`
    pub(crate) fn sole_emf(&self, argc: usize) -> Option<(u32, usize, [u32; 2], Value<'gc>)> {
        let flags = self.flags();
        if flags.simple() || flags.has_eql() || flags.resolved_keys() || !(1..=2).contains(&argc) {
            return None;
        }
        let epoch = self.epoch();
        if epoch & 1 != 0 {
            return None;
        }
        let arity = self.arity_key(argc) as u64;
        let mut found = None;
        for bucket in self.dispatch_table.get().as_slice() {
            if bucket.emf.is_null() || bucket.key >> 48 != arity {
                continue;
            }
            if found.is_some() || bucket.rest.is_some() || bucket.eql.is_some() {
                return None;
            }
            found = Some(*bucket);
        }
        let bucket = found?;
        let key_len = (self.dispatch_width_value() as usize).min(argc);
        if key_len > 2 {
            return None;
        }
        let ids = [
            (bucket.key & 0xFF_FFFF) as u32,
            ((bucket.key >> 24) & 0xFF_FFFF) as u32,
        ];
        (self.epoch() == epoch).then_some((epoch, key_len, ids, bucket.emf))
    }

    /// Serve a plain slot accessor call without dispatching: the first
    #[inline]
    pub(crate) fn slot_access_fast(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        let cache = generic.slot_cache.get();
        if cache.is_empty() {
            return None;
        }
        let obj = *args.first()?;
        if !obj.is_cell() {
            return None;
        }
        let class_id = obj.as_cell_raw().class_id().bits();
        let entry = cache[slot_entry_position(&cache, class_id)?];
        if args.len() != 1 + entry.writer as usize {
            return None;
        }
        // SAFETY: a Scheme class id on the header means a `SchemeInstance`
        let instance: Gc<SchemeInstance> = unsafe { Gc::from_gc_object(obj.as_cell_raw()) };
        if instance.storage().stamp() != entry.stamp {
            return None;
        }
        if entry.writer {
            return SchemeInstance::set_slot_checked(ctx, instance, entry.index as usize, args[1])
                .then_some(args[1]);
        }
        let value = instance.slot_checked(entry.index as usize)?;
        (!value.is_empty()).then_some(value)
    }

    /// After a dispatch, remember a plain slot accessor under the argument's
    /// class so the next call skips dispatch (see [`Self::slot_access_fast`]).
    #[inline]
    pub(crate) fn note_slot_access(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) {
        let flags = method.flags();
        let writer = flags.slot_writer();
        if method.role() != MethodRole::Primary || !generic.flags().simple() {
            return;
        }
        if !(flags.slot_reader() || writer) || args.len() != 1 + writer as usize {
            return;
        }
        if generic.flags().resolved_keys() || generic.flags().has_eql() {
            return;
        }
        // Only a generic whose every method is a slot accessor caches them
        // (see `classify_dfun`); anything else dispatches each call.
        if generic.dfun_state() != DfunState::Accessor {
            return;
        }
        let Some(hit) = ic::slot_lookup(ctx, args[0], method.slot_name(), writer) else {
            return;
        };
        // Depend on the class's slot protocol before publishing. `add`
        // refuses if the set has already been invalidated.
        if !WatchpointSet::add(hit.wp, ctx, Dependent::generic_slot_cache(generic)) {
            return;
        }
        for _ in 0..=FILL_RETRIES {
            let current = generic.slot_cache.load(Ordering::Acquire);
            if !generic.epoch_is_current(epoch) {
                return;
            }
            if slot_entry_position(&current, hit.class_id).is_some() {
                return;
            }
            let entry = SlotAccessEntry {
                class_id: hit.class_id,
                stamp: hit.stamp,
                index: hit.index,
                writer,
            };
            let Some(appended) = slot_cache_with(ctx, current, entry) else {
                // More classes than a table holds: this generic dispatches
                // each call until the next change.
                if generic.epoch_is_current(epoch) {
                    barrier::field!(Gc::write(ctx, generic), GenericDescriptor, dfun_state)
                        .unlock()
                        .set(DfunState::Caching);
                }
                return;
            };
            let owner = Gc::write(ctx, generic);
            if generic
                .slot_cache
                .compare_exchange(current, appended, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let _ = owner;
                // Fired between registering and publishing: the sweep may
                // have missed this entry.
                if hit.wp.is_invalid() {
                    Self::clear_slot_cache(ctx, generic);
                }
                return;
            }
        }
    }

    /// The methods applicable to `args`, every role, most specific first
    pub(crate) fn all_applicable_methods(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        self.applicable_with_roles(ctx, args, false)
    }

    /// The primary methods applicable to `args`, most specific first. The
    /// simple generics' dispatch: every method of such a generic is primary.
    fn slow_applicable_methods(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        self.applicable_with_roles(ctx, args, true)
    }

    /// The members of `methods` that apply to `args`, most specific first
    /// (`check_eql` as in [`MethodDescriptor::score`]).
    fn rank_methods(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
        methods: &[Gc<'gc, MethodDescriptor<'gc>>],
        check_eql: bool,
    ) -> Vec<Gc<'gc, MethodDescriptor<'gc>>> {
        let key_of: KeyFn<'gc> = if self.flags().resolved_keys() {
            resolved_key
        } else {
            plain_key
        };
        let mut scored: Vec<_> = methods
            .iter()
            .filter_map(|method| {
                method
                    .score(ctx, args, key_of, check_eql)
                    .map(|score| (*method, score))
            })
            .collect();
        scored.sort_by(|(_, left), (_, right)| left.cmp(right));
        scored.into_iter().map(|(method, _)| method).collect()
    }

    fn applicable_with_roles(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
        primary_only: bool,
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        if args.len() < self.max_required_dispatch_args {
            return Err(GenericDispatchError::Arity);
        }
        let key_of: KeyFn<'gc> = if self.flags().resolved_keys() {
            resolved_key
        } else {
            plain_key
        };
        let mut methods = Vec::new();
        for method in self.methods().iter().copied() {
            if primary_only && method.role() != MethodRole::Primary {
                continue;
            }
            let Some(score) = method.specificity_score(ctx, args, key_of) else {
                continue;
            };
            methods.push((method, score));
        }
        if methods.is_empty() {
            return Err(GenericDispatchError::NoApplicableMethod);
        }
        methods.sort_by(|(_, left), (_, right)| left.cmp(right));
        let methods = Array::from_slice(
            ctx,
            methods
                .iter()
                .map(|(method, _)| *method)
                .collect::<Vec<_>>(),
        );
        Ok(methods)
    }

    /// The class id dispatch keys on: slot and call caches use the same
    pub(crate) fn dispatch_class_id(value: Value<'_>) -> Option<ClassId> {
        value.class_id()
    }

    /// The table key and the class ids past the first two of `args`.
    fn lookup_key(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<(u64, SmallVec<[ClassId; 4]>)> {
        let width = (generic.dispatch_width_value() as usize).min(args.len());
        let id0 = Self::dispatch_class_id(*args.first()?)?.bits();
        let id1 = if width > 1 {
            Self::dispatch_class_id(args[1])?.bits()
        } else {
            0
        };
        let key = dispatch_key(id0, id1, generic.arity_key(args.len()));
        let mut rest: SmallVec<[ClassId; 4]> = SmallVec::new();
        for arg in args.iter().take(width).skip(2) {
            rest.push(Self::dispatch_class_id(*arg)?);
        }
        Some((key, rest))
    }

    pub(crate) fn cached_for_args(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>> {
        let table = generic.dispatch_table.get();
        if table.is_empty() {
            return None;
        }
        let (key, rest) = Self::lookup_key(generic, args)?;
        // An eql bucket's `methods` are the class-level part only: its
        // answer depends on the eql matches (see `eql_applicable_methods_at`).
        Self::probe(table, key, &rest)
            .filter(|bucket| bucket.eql.is_none())
            .and_then(|bucket| bucket.methods)
    }

    /// The cached effective method of a combined generic for `args`'
    pub(crate) fn cached_emf(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        if generic.flags().simple() || !Self::emf_cacheable(generic) {
            return None;
        }
        let table = generic.dispatch_table.get();
        if table.is_empty() {
            return None;
        }
        let (key, rest) = Self::lookup_key(generic, args)?;
        let bucket = Self::probe(table, key, &rest)?;
        let emf = match bucket.eql {
            None => bucket.emf,
            Some(candidates) => {
                let mask = Self::eql_match_mask(candidates.as_slice(), args);
                if mask == 0 {
                    bucket.emf
                } else {
                    let entry = bucket
                        .memo
                        .and_then(|memo| memo.iter().find(|entry| entry.mask == mask).copied());
                    entry.map_or(Value::null(), |entry| entry.emf)
                }
            }
        };
        (!emf.is_null()).then_some(emf)
    }

    /// Publish `emf`, the effective method Scheme built for the methods the
    pub(crate) fn cache_emf(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        epoch: u32,
        emf: Value<'gc>,
        args: &[Value<'gc>],
    ) {
        if generic.flags().simple() || !emf.is::<Closure>() || !Self::emf_cacheable(generic) {
            return;
        }
        let Some((key, rest)) = Self::lookup_key(generic, args) else {
            return;
        };
        // The stub enters a combined generic's effective method directly
        let direct = !generic.flags().has_eql();
        Self::publish_bucket(ctx, generic, epoch, key, &rest, |existing| {
            let mut bucket = *existing?;
            let Some(candidates) = bucket.eql else {
                if !bucket.emf.is_null() {
                    return None;
                }
                bucket.emf = emf;
                if direct {
                    bucket.flags |= BUCKET_DIRECT;
                    bucket.body = emf;
                }
                return Some(bucket);
            };
            let mask = Self::eql_match_mask(candidates.as_slice(), args);
            if mask == 0 {
                if !bucket.emf.is_null() {
                    return None;
                }
                bucket.emf = emf;
                return Some(bucket);
            }
            let mut entries: Vec<EqlMemoEntry<'gc>> = bucket
                .memo
                .map_or_else(Vec::new, |memo| memo.as_slice().to_vec());
            let entry = entries.iter_mut().find(|entry| entry.mask == mask)?;
            if !entry.emf.is_null() {
                return None;
            }
            entry.emf = emf;
            bucket.memo = Some(Array::from_slice(ctx, &entries));
            Some(bucket)
        });
    }

    fn probe<'a>(
        table: ArrayRef<'gc, DispatchBucket<'gc>>,
        key: u64,
        rest: &[ClassId],
    ) -> Option<&'a DispatchBucket<'gc>>
    where
        'gc: 'a,
    {
        let len = table.len();
        if len == 0 {
            return None;
        }
        let mask = len - 1;
        let mut index = dispatch_hash(key, rest, len);
        // The load factor is at most one half, so an empty bucket ends the
        // probe.
        loop {
            // SAFETY: tables are immutable and outlive the call (the caller
            // holds the generic).
            let bucket: &'a DispatchBucket<'gc> =
                unsafe { &*(&table.as_slice()[index] as *const DispatchBucket<'gc>) };
            let methods = bucket.methods?;
            if bucket.key == key
                && bucket
                    .rest
                    .map_or(rest.is_empty(), |stored| stored.as_slice() == rest)
            {
                let _ = methods;
                return Some(bucket);
            }
            index = (index + 1) & mask;
        }
    }

    fn insert_dispatcher_cache(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        ids: &[ClassId],
        arity: u32,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        epoch: u32,
    ) {
        let key = dispatch_key(
            ids.first().map_or(0, |id| id.bits()),
            ids.get(1).map_or(0, |id| id.bits()),
            arity,
        );
        let rest = ids.get(2..).unwrap_or(&[]);
        // A combined generic's first method is not its effective method, so
        // its bucket is never entered directly.
        let simple = generic.flags().simple();
        Self::publish_bucket(ctx, generic, epoch, key, rest, |existing| {
            if existing.is_some() {
                return None;
            }
            let first = methods[0];
            let direct = simple && first.flags().no_next() && first.body().is::<Closure>();
            Some(DispatchBucket {
                key,
                flags: if direct { BUCKET_DIRECT } else { 0 },
                rest: (!rest.is_empty()).then(|| Array::from_slice(ctx, rest)),
                methods: Some(methods),
                body: if direct { first.body() } else { Value::null() },
                emf: Value::null(),
                eql: None,
                memo: None,
                eql_direct: None,
            })
        });
    }

    /// Copy-on-write publish of one bucket. `make` sees the bucket already
    fn publish_bucket(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        epoch: u32,
        key: u64,
        rest: &[ClassId],
        make: impl Fn(Option<&DispatchBucket<'gc>>) -> Option<DispatchBucket<'gc>>,
    ) {
        for _ in 0..=FILL_RETRIES {
            // Load before validating the epoch (see `begin_change`).
            let current = generic.dispatch_table.load(Ordering::Acquire);
            if !generic.epoch_is_current(epoch) {
                return;
            }
            let existing = Self::probe(current, key, rest).copied();
            let Some(fresh) = make(existing.as_ref()) else {
                return;
            };
            let occupied: Vec<DispatchBucket<'gc>> = current
                .as_slice()
                .iter()
                .copied()
                .filter(|bucket| {
                    bucket.methods.is_some()
                        && !(bucket.key == key
                            && bucket
                                .rest
                                .map_or(rest.is_empty(), |stored| stored.as_slice() == rest))
                })
                .collect();
            if existing.is_none() && occupied.len() >= MAX_DISPATCH_ENTRIES {
                DISPATCH_OVERFLOWS.fetch_add(1, Ordering::Relaxed);
                return;
            }
            let mut capacity = current.len().max(MIN_DISPATCH_CAPACITY);
            while (occupied.len() + 1) * 2 > capacity {
                capacity *= 2;
            }
            let mut buckets = vec![DispatchBucket::empty(); capacity];
            for bucket in occupied.into_iter().chain([fresh]) {
                let stored: &[ClassId] = match &bucket.rest {
                    Some(rest) => rest.as_slice(),
                    None => &[],
                };
                let mut index = dispatch_hash(bucket.key, stored, capacity);
                while buckets[index].methods.is_some() {
                    index = (index + 1) & (capacity - 1);
                }
                buckets[index] = bucket;
            }
            let table = Array::from_slice(ctx, &buckets);
            let owner = Gc::write(ctx, generic);
            if generic
                .dispatch_table
                .compare_exchange(current, table, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let _ = owner;
                return;
            }
        }
    }

    /// The discriminating-function state since the last change.
    pub(crate) fn dfun_state(&self) -> DfunState {
        self.dfun_state.get()
    }

    /// The `DefaultOnly` body for a call of `argc` arguments made under
    #[inline]
    pub(crate) fn default_body_at(&self, epoch: u32, argc: usize) -> Option<Value<'gc>> {
        let entry = self.default_entry.get();
        if !entry.is_pair() {
            return None;
        }
        if entry.car().bits() != Value::new(epoch as i32).bits() {
            return None;
        }
        let rest = entry.cdr();
        if rest.car().bits() != Value::new(argc as i32).bits() {
            return None;
        }
        Some(rest.cdr())
    }

    /// Classify a generic on the first call since the last change.
    fn classify_dfun(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        epoch: u32,
        methods: &[Gc<'gc, MethodDescriptor<'gc>>],
        argc: usize,
    ) {
        if generic.dfun_state.get() != DfunState::Initial {
            return;
        }
        let top = ClassId::new(builtin_class_ids::TOP);
        let default = match methods {
            [method]
                if generic.methods().len() == 1
                    && method.role() == MethodRole::Primary
                    && method.flags().no_next()
                    && !method.flags().slot_reader()
                    && !method.flags().slot_writer()
                    && method.body().is::<Closure>()
                    && method.required_arg_count() == argc
                    && method.specializers().iter().all(|id| Some(*id) == top)
                    && (0..method.specializers().len()).all(|p| method.eql_at(p).is_none()) =>
            {
                Some(method.body())
            }
            _ => None,
        };
        if !generic.epoch_is_current(epoch) {
            return;
        }
        let state = match default {
            Some(body) => {
                let entry = Value::cons(
                    ctx,
                    Value::new(epoch as i32),
                    Value::cons(ctx, Value::new(argc as i32), body),
                );
                barrier::field!(Gc::write(ctx, generic), GenericDescriptor, default_entry)
                    .unlock()
                    .set(entry);
                DfunState::DefaultOnly
            }
            None if Self::accessors_only(generic) => DfunState::Accessor,
            None => DfunState::Caching,
        };
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, dfun_state)
            .unlock()
            .set(state);
    }

    /// Whether every method of `generic` is a primary slot reader or writer
    fn accessors_only(generic: Gc<'gc, GenericDescriptor<'gc>>) -> bool {
        let methods = generic.methods();
        !methods.is_empty()
            && methods.iter().all(|method| {
                method.role() == MethodRole::Primary
                    && (method.flags().slot_reader() || method.flags().slot_writer())
                    && (0..method.specializers().len()).all(|p| method.eql_at(p).is_none())
            })
    }

    /// The shape of the slot cache (see [`slot_cache_shape`]).
    pub(crate) fn slot_cache_state(&self) -> (SlotCacheShape, bool) {
        slot_cache_shape(&self.slot_cache.get())
    }

    /// The reader entries of the slot cache as `(class_id, stamp, index)`,
    pub(crate) fn slot_reader_entries(&self, epoch: u32) -> Option<Vec<(u32, u32, u32)>> {
        let cache = self.slot_cache.get();
        let entries: Vec<(u32, u32, u32)> = cache
            .iter()
            .filter(|entry| entry.class_id != 0 && !entry.writer)
            .map(|entry| (entry.class_id, entry.stamp, entry.index))
            .collect();
        (self.epoch() == epoch && epoch & 1 == 0).then_some(entries)
    }

    pub(crate) fn clear_dispatcher_cache(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
    ) {
        // Bump first: a filler that read the old epoch is refused from here
        let step = if generic.epoch() & 1 == 1 { 1 } else { 2 };
        generic.epoch_atomic().fetch_add(step, Ordering::AcqRel);
        Self::wipe_caches(ctx, generic);
    }

    /// Open a method change: the epoch turns odd, which refuses every filler,
    fn begin_change(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        generic.epoch_atomic().fetch_add(1, Ordering::AcqRel);
        Self::wipe_caches(ctx, generic);
    }

    fn wipe_caches(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, dfun_state)
            .unlock()
            .set(DfunState::Initial);
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, default_entry)
            .unlock()
            .set(Value::null());
        generic
            .dispatch_table
            .atomic_swap(empty_dispatch_table(ctx), Ordering::Release);
        generic
            .slot_cache
            .atomic_swap(empty_slot_cache(ctx), Ordering::Release);
        Self::clear_method_cache(ctx, generic);
    }

    /// Empty the slot-access cache: a watchpoint the cache depended on fired.
    pub(crate) fn clear_slot_cache(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
    ) {
        if generic.epoch() & 1 == 0 {
            generic.epoch_atomic().fetch_add(2, Ordering::AcqRel);
        }
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        generic
            .slot_cache
            .atomic_swap(empty_slot_cache(ctx), Ordering::Release);
        Self::renew_change_wp(ctx, generic);
    }

    fn clear_method_cache(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        generic
            .method_cache
            .atomic_swap(empty_method_cache(ctx), Ordering::Release);
        generic
            .cursor_cache
            .atomic_swap(empty_cursor_cache(ctx), Ordering::Release);
    }

    /// After a dispatch to a `NO_NEXT` closure method, remember it for
    pub(crate) fn note_cursor_entry(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) {
        let first = methods[0];
        if !first.flags().next_cursor() || !(1..=2).contains(&args.len()) {
            return;
        }
        let flags = generic.flags();
        if first.role() != MethodRole::Primary
            || !flags.simple()
            || flags.resolved_keys()
            || flags.has_eql()
            || flags.custom_dfun()
        {
            return;
        }
        let body = first.body();
        if !body.is::<Closure>() || !args.iter().all(|a| a.is_cell()) {
            return;
        }
        let class_of = |v: Value<'gc>| v.as_cell_raw().class_id().bits();
        let (class0, class1) = (class_of(args[0]), args.get(1).map_or(0, |a| class_of(*a)));
        for _ in 0..=FILL_RETRIES {
            let current = generic.cursor_cache.load(Ordering::Acquire);
            if !generic.epoch_is_current(epoch) {
                return;
            }
            if current.len() >= 8
                || current.iter().any(|e| {
                    e.argc as usize == args.len() && e.class0 == class0 && e.class1 == class1
                })
            {
                return;
            }
            let cursor = NextMethodDescriptor::cursor(ctx, generic, methods, 0, Value::empty());
            let appended = Array::new(ctx, current.len() + 1, |_, index| {
                if index < current.len() {
                    current[index]
                } else {
                    CursorEntry {
                        argc: args.len() as u32,
                        class0,
                        class1,
                        pad: 0,
                        body,
                        cursor: cursor.into(),
                    }
                }
            });
            if generic
                .cursor_cache
                .compare_exchange(current, appended, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                return;
            }
        }
    }

    #[inline]
    pub(crate) fn note_method_entry(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        args: &[Value<'gc>],
        epoch: u32,
    ) {
        if !method.flags().no_next() || !(1..=2).contains(&args.len()) {
            return;
        }
        if method.role() != MethodRole::Primary
            || !generic.flags().simple()
            || generic.flags().resolved_keys()
            || generic.flags().has_eql()
        {
            return;
        }
        let body = method.body();
        if !body.is::<Closure>() || !args.iter().all(|a| a.is_cell()) {
            return;
        }
        let class_of = |v: Value<'gc>| v.as_cell_raw().class_id().bits();
        let (class0, class1) = (class_of(args[0]), args.get(1).map_or(0, |a| class_of(*a)));
        for _ in 0..=FILL_RETRIES {
            let current = generic.method_cache.load(Ordering::Acquire);
            if !generic.epoch_is_current(epoch) {
                return;
            }
            if current.len() >= 8
                || current.iter().any(|e| {
                    e.argc as usize == args.len() && e.class0 == class0 && e.class1 == class1
                })
            {
                return;
            }
            let appended = Array::new(ctx, current.len() + 1, |_, index| {
                if index < current.len() {
                    current[index]
                } else {
                    MethodEntry {
                        argc: args.len() as u32,
                        class0,
                        class1,
                        pad: 0,
                        body,
                    }
                }
            });
            let owner = Gc::write(ctx, generic);
            if generic
                .method_cache
                .compare_exchange(current, appended, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let _ = owner;
                return;
            }
        }
    }

    /// The method set changed (added or replaced): slot caches over a
    fn note_method_change(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) {
        if generic.flags().slot_protocol() {
            fire_slot_protocol_for_method(ctx, method);
        } else if generic.flags().initialize() {
            class_table(ctx).reset_protocol_state_matching(OP_INIT, |class| {
                method.may_apply_to_classes(&[class])
            });
        } else if generic.flags().lifecycle() {
            LIFECYCLE_CUSTOM.store(true, std::sync::atomic::Ordering::Release);
            class_table(ctx).reset_protocol_state_matching(OP_INIT, |_| true);
        }
    }

    fn append_method(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) {
        let current = generic.methods.get();
        let appended = Array::new(ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, methods)
            .unlock()
            .set(appended);
        generic.update_dispatch_shape(&method);
    }

    /// Recompute the sticky-by-definition flags from the current method set:
    fn recompute_flags(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let table = class_table(ctx);
        let methods = generic.methods();
        let resolved = methods.iter().any(|method| {
            method
                .specializers()
                .iter()
                .any(|id| specializer_needs_resolved_keys(table, *id))
        });
        let has_eql = methods.iter().any(|method| method.eql.is_some());
        let current = generic.flags();
        let flags = if resolved {
            current.with_resolved_keys()
        } else {
            current.without_resolved_keys()
        };
        let flags = if has_eql {
            flags.with_has_eql()
        } else {
            flags.without_has_eql()
        };
        let custom = generic.custom_dfun().is_some();
        let flags = if custom {
            flags.with_custom_dfun()
        } else {
            flags.without_custom_dfun()
        };
        let simple = !custom
            && generic.combination().is_standard()
            && methods
                .iter()
                .all(|method| method.role() == MethodRole::Primary && method.eql.is_none());
        let flags = if simple {
            flags.with_simple()
        } else {
            flags.without_simple()
        };
        if flags != current {
            barrier::field!(Gc::write(ctx, generic), GenericDescriptor, flags)
                .unlock()
                .set(flags);
        }
    }

    /// I1: the one primitive every change that can alter dispatch results
    pub(crate) fn invalidate(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        reason: Reason<'gc>,
    ) {
        Self::recompute_flags(ctx, generic);
        Self::clear_dispatcher_cache(ctx, generic);
        let cleared = Self::renew_change_wp(ctx, generic);
        log::info!(
            target: crate::heap::logging::IC_LOG_TARGET,
            "generic {} invalidated: {cleared} dependents cleared",
            generic.name()
        );
        match reason {
            Reason::AddMethod(method) | Reason::RemoveMethod(method) => {
                Self::note_method_change(ctx, generic, method)
            }
            Reason::ClassChanged(class) => {
                log::info!(
                    target: crate::heap::logging::IC_LOG_TARGET,
                    "class {} changed under generic {}",
                    class.name(),
                    generic.name()
                );
                if generic.flags().slot_protocol() {
                    fire_slot_protocol(ctx);
                }
            }
            Reason::Reinitialize => {
                if generic.flags().slot_protocol() {
                    fire_slot_protocol(ctx);
                }
            }
        }
    }

    /// Remove `method` from `generic`: the method set shrinks, the dispatch
    pub fn remove_method(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) -> bool {
        let member = generic
            .methods()
            .iter()
            .any(|member| Gc::ptr_eq(*member, method));
        if !member {
            return false;
        }
        Self::begin_change(ctx, generic);
        let kept: Vec<_> = generic
            .methods()
            .iter()
            .copied()
            .filter(|member| !Gc::ptr_eq(*member, method))
            .collect();
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, methods)
            .unlock()
            .set(Array::from_slice(ctx, &kept));
        generic.recompute_dispatch_shape();
        Self::invalidate(ctx, generic, Reason::RemoveMethod(method));
        for (position, specializer) in method.specializers().iter().enumerate() {
            match method.eql_at(position) {
                Some(object) => {
                    let specializer = EqlSpecializer::intern(ctx, object);
                    EqlSpecializer::remove_direct_method(ctx, specializer, method.into());
                }
                None => {
                    if let Some(class) = class_table(ctx).lookup(*specializer) {
                        ClassDescriptor::remove_direct_method(ctx, class, method.into());
                    }
                }
            }
        }
        true
    }
}

impl<'gc> MethodDescriptor<'gc> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        eql: &[Value<'gc>],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
        qualifiers: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let eql = eql
            .iter()
            .any(|object| !object.is_empty())
            .then(|| Array::from_slice(ctx, eql));
        let role = MethodRole::for_combination(generic.combination().is_standard(), qualifiers);
        ctx.allocate_with(
            Self {
                generic,
                specializers: Array::from_slice(ctx, specializers),
                eql,
                required_arg_count,
                body: Lock::new(body),
                flags: Lock::new(flags),
                slot_name: Lock::new(Value::empty()),
                qualifiers,
                role: Lock::new(role),
            },
            class_header_word(super::builtin::builtin_id(builtin_class_ids::METHOD)),
        )
    }

    pub fn generic(&self) -> Gc<'gc, GenericDescriptor<'gc>> {
        self.generic
    }

    pub fn specializers(&self) -> &[ClassId] {
        self.specializers.as_slice()
    }

    /// The eql object of specializer `position`, or `None` for a class
    /// specializer (or a position past the list).
    pub fn eql_at(&self, position: usize) -> Option<Value<'gc>> {
        self.eql
            .and_then(|eql| eql.as_slice().get(position).copied())
            .filter(|object| !object.is_empty())
    }

    pub fn required_arg_count(&self) -> usize {
        self.required_arg_count
    }

    pub fn body(&self) -> Value<'gc> {
        self.body.load(Ordering::Acquire)
    }

    pub fn flags(&self) -> MethodFlags {
        self.flags.get()
    }

    /// The qualifier keywords, a Scheme list (`()` for a primary method).
    pub fn qualifiers(&self) -> Value<'gc> {
        self.qualifiers
    }

    /// The part this method plays in the standard method combination.
    pub fn role(&self) -> MethodRole {
        self.role.get()
    }

    pub fn set_body(
        ctx: MutationContext<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        body: Value<'gc>,
    ) {
        // Release, not a plain store: `begin_change` has just swapped in an
        // empty dispatch table, and a dispatch-table bucket keeps a copy of the
        // old body. On a weakly ordered CPU a plain store can be seen before
        // that swap, so a reader would get the new body for one class and the
        // old cached body for the next.
        let owner = Gc::write(ctx, method);
        let _ = owner;
        method.body.atomic_swap(body, Ordering::Release);
        GenericDescriptor::clear_method_cache(ctx, method.generic);
    }

    pub fn slot_name(&self) -> Value<'gc> {
        self.slot_name.get()
    }

    pub fn set_slot_name(
        ctx: MutationContext<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        name: Value<'gc>,
    ) {
        barrier::field!(Gc::write(ctx, method), MethodDescriptor, slot_name)
            .unlock()
            .set(name);
    }

    /// Store the method's role under its generic's combination (see
    /// [`GenericDescriptor::set_combination`]).
    fn set_role(
        ctx: MutationContext<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        role: MethodRole,
    ) {
        barrier::field!(Gc::write(ctx, method), MethodDescriptor, role)
            .unlock()
            .set(role);
    }

    pub fn set_flags(
        ctx: MutationContext<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        flags: MethodFlags,
    ) {
        barrier::field!(Gc::write(ctx, method), MethodDescriptor, flags)
            .unlock()
            .set(flags);
    }

    /// Whether this method may apply to a call whose first arguments are
    pub(crate) fn may_apply_to_classes(&self, classes: &[Gc<'gc, ClassDescriptor<'gc>>]) -> bool {
        self.specializers
            .iter()
            .zip(classes)
            .all(|(specializer, class)| class.cpl().contains(specializer))
    }

    /// [`Self::specificity_score`] for arguments known only by their class
    fn class_specificity_score(
        &self,
        ctx: MutationContext<'gc>,
        ids: &[ClassId],
    ) -> Option<Vec<usize>> {
        if ids.len() < self.required_arg_count || ids.len() < self.specializers.len() {
            return None;
        }
        let mut score = Vec::with_capacity(ids.len() + 1);
        for (index, id) in ids.iter().enumerate() {
            if let Some(specializer) = self.specializers.get(index) {
                if self.eql_at(index).is_some() {
                    return None;
                }
                let class = class_table(ctx).lookup(*id)?;
                let distance = class
                    .cpl()
                    .iter()
                    .position(|class_id| class_id == specializer)?;
                score.push(1 + distance);
            } else {
                score.push(usize::MAX / 2);
            }
        }
        score.push(usize::MAX - self.required_arg_count);
        Some(score)
    }

    /// Whether every eql position of this method holds the `eqv?` object
    /// (the eql test of [`Self::score`], without the class parts).
    pub(crate) fn eql_matches(&self, args: &[Value<'gc>]) -> bool {
        (0..self.specializers.len()).all(|position| match self.eql_at(position) {
            Some(object) => args.get(position).is_some_and(|arg| arg.eqv(object)),
            None => true,
        })
    }

    fn specificity_score(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
        key_of: KeyFn<'gc>,
    ) -> Option<Vec<usize>> {
        self.score(ctx, args, key_of, true)
    }

    /// The sort key of this method for `args` (smaller is more specific), or
    fn score(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
        key_of: KeyFn<'gc>,
        check_eql: bool,
    ) -> Option<Vec<usize>> {
        if args.len() < self.required_arg_count || args.len() < self.specializers.len() {
            return None;
        }
        let mut score = Vec::with_capacity(args.len() + 1);
        for (index, arg) in args.iter().enumerate() {
            if let Some(specializer) = self.specializers.get(index) {
                // An eql match ranks above every class at its position.
                if let Some(object) = self.eql_at(index) {
                    if check_eql && !arg.eqv(object) {
                        return None;
                    }
                    score.push(0);
                    continue;
                }
                let class = class_table(ctx).lookup(key_of(ctx, *arg)?)?;
                let distance = class
                    .cpl()
                    .iter()
                    .position(|class_id| class_id == specializer)?;
                score.push(1 + distance);
            } else {
                score.push(usize::MAX / 2);
            }
        }
        score.push(usize::MAX - self.required_arg_count);
        Some(score)
    }
}

impl<'gc> NextMethodDescriptor<'gc> {
    pub(crate) fn new(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        args: ArrayRef<'gc, Value<'gc>>,
        index: usize,
    ) -> Gc<'gc, Self> {
        Self::with_tail(ctx, generic, methods, args, index, Value::empty())
    }

    /// A chain whose exhaustion tail-calls `tail` with the args (empty
    /// `tail`: the chain ends in `no-next-method`).
    pub(crate) fn with_tail(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        args: ArrayRef<'gc, Value<'gc>>,
        index: usize,
        tail: Value<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                generic,
                methods,
                args,
                index,
                tail,
                cursor: false,
            },
            class_header_word(super::builtin::builtin_id(builtin_class_ids::NEXT_METHOD)),
        )
    }

    /// The next-method argument for `methods[index]` of a chain, when that
    pub(crate) fn cursor(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        index: usize,
        tail: Value<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                generic,
                methods,
                args: Array::from_slice(ctx, []),
                index,
                tail,
                cursor: true,
            },
            class_header_word(super::builtin::builtin_id(builtin_class_ids::NEXT_METHOD)),
        )
    }

    /// Whether this is a cursor (see [`Self::cursor`]).
    pub fn is_cursor(&self) -> bool {
        self.cursor
    }

    /// The next-method argument `methods[index]` gets when the chain is
    pub(crate) fn lead(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
        index: usize,
        tail: Value<'gc>,
        args: &[Value<'gc>],
    ) -> Value<'gc> {
        let flags = methods[index].flags();
        if flags.no_next() {
            Value::new(false)
        } else if flags.next_cursor() {
            Self::cursor(ctx, generic, methods, index, tail).into()
        } else {
            Self::with_tail(
                ctx,
                generic,
                methods,
                Array::from_slice(ctx, args),
                index,
                tail,
            )
            .into()
        }
    }

    /// The tail procedure of the chain, or `None` when it has none.
    pub fn tail(&self) -> Option<Value<'gc>> {
        (!self.tail.is_empty()).then_some(self.tail)
    }

    pub fn generic(&self) -> Gc<'gc, GenericDescriptor<'gc>> {
        self.generic
    }

    pub fn args(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.args
    }

    pub fn methods(&self) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn method(&self) -> Gc<'gc, MethodDescriptor<'gc>> {
        self.methods[self.index]
    }

    pub fn body(&self) -> Value<'gc> {
        self.method().body()
    }

    /// Whether a next method exists, or the chain has a tail to run.
    pub fn has_next(&self) -> bool {
        self.index + 1 < self.methods.len() || !self.tail.is_empty()
    }

    /// The next method of the chain, `None` past its last method (a tail
    /// is not a method).
    pub fn next(&self, ctx: MutationContext<'gc>) -> Option<Gc<'gc, Self>> {
        (self.index + 1 < self.methods.len()).then(|| {
            Self::with_tail(
                ctx,
                self.generic,
                self.methods,
                self.args,
                self.index + 1,
                self.tail,
            )
        })
    }

    pub fn invocation_args(
        ctx: MutationContext<'gc>,
        next_method: Gc<'gc, Self>,
    ) -> ArrayRef<'gc, Value<'gc>> {
        Array::new(ctx, next_method.args.len() + 1, |_, index| {
            if index == 0 {
                next_method.into()
            } else {
                next_method.args[index - 1]
            }
        })
    }

    pub fn next_invocation(
        ctx: MutationContext<'gc>,
        next_method: Gc<'gc, Self>,
    ) -> Option<(Value<'gc>, ArrayRef<'gc, Value<'gc>>)> {
        next_method
            .next(ctx)
            .map(|next_method| (next_method.body(), Self::invocation_args(ctx, next_method)))
    }
}

// SAFETY: `gc` for `GenericDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for GenericDescriptor<'gc> {
    const TYPE_NAME: &'static str = "generic";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::GENERIC];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::Generic);
}

// SAFETY: `gc` for `MethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for MethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::METHOD];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::Method);
}

// SAFETY: `gc` for `NextMethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for NextMethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "next-method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::NEXT_METHOD];
}

fn empty_method_metadata<'gc>(
    ctx: MutationContext<'gc>,
) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length method arrays are never initialized")
    })
}

fn empty_slot_cache<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, SlotAccessEntry> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length slot cache arrays are never initialized")
    })
}

fn empty_method_cache<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, MethodEntry<'gc>> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length method cache arrays are never initialized")
    })
}

fn empty_cursor_cache<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, CursorEntry<'gc>> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length cursor cache arrays are never initialized")
    })
}

fn empty_dispatch_table<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, DispatchBucket<'gc>> {
    Array::new(ctx, 0, |_, _| {
        unreachable!("zero-length dispatch tables are never initialized")
    })
}
