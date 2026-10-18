use std::mem::size_of;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

use smallvec::SmallVec;

use super::descriptor::ClassDescriptor;
use super::instance::SchemeInstance;
use super::table::class_table;
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::FILL_RETRIES;
use crate::heap::cell::Lock;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace};
use crate::prelude::*;
use crate::runtime::interp::ic;
use crate::runtime::value::conversions::ClassTagged;
use crate::runtime::watchpoint::{Dependent, WatchpointSet};
use crate::runtime::{
    MutationContext,
    value::{Closure, PROCEDURES, Value},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenericDispatchError {
    Arity,
    NoApplicableMethod,
}

/// Generics whose only method must be the boot `<class>` method for slot
/// inline caches to apply, with the required dispatch argument count the
/// boot `define-generic` used.
pub const SLOT_PROTOCOL_GENERICS: &[(&str, usize)] = &[
    ("slot-ref-using-class", 3),
    ("slot-set-using-class!", 4),
    ("slot-bound-using-class?", 3),
];

/// Indices into [`SLOT_PROTOCOL_GENERICS`], and of the per-class state of
/// `initialize` after them.
pub(crate) const OP_REF: usize = 0;
pub(crate) const OP_SET: usize = 1;
pub(crate) const OP_BOUND: usize = 2;
pub(crate) const OP_INIT: usize = 3;

/// Whether `name` is a slot protocol generic.
pub fn is_slot_protocol_generic(name: &str) -> bool {
    SLOT_PROTOCOL_GENERICS
        .iter()
        .any(|(protocol, _)| *protocol == name)
}

/// Record a change that may affect slot caches: fire the slot-protocol
/// watchpoint of every class, clearing each cache that depended on one.
/// Writers mutate first and fire last, so a filler that reads a fresh set
/// already sees the change (see [`crate::runtime::watchpoint`]). The caller owes the
/// grace period once it holds no unrooted `Gc` ([`crate::runtime::watchpoint::grace`]).
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
/// over its old layout must miss so obsolete instances migrate). The caller
/// owes the grace period, see [`fire_slot_protocol`].
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
    /// of these into the call frame. (Fusing a `[next, args...]` heap
    /// array here just to explode it into the frame again wastes an
    /// allocation per call, `invocation_args` keeps serving the cold
    /// `next-method-invoke` path.)
    pub fn call_args(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.args
    }

    /// The fused `[next, args...]` call args for callers that need a
    /// slice (`generic-invoke`). Fallback calls return the plain args
    /// with no fuse.
    pub fn fused_args(&self, ctx: MutationContext<'gc>) -> ArrayRef<'gc, Value<'gc>> {
        match self.next {
            Some(next) => Array::with(ctx, self.args.len() + 1, |_, index| {
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

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct GenericDescriptor<'gc> {
    name: ArrayRef<'gc, u8>,
    methods: Lock<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    /// Open-addressed table of applicable-method lists by the classes of the
    /// first `dispatch_width` arguments; immutable, replaced copy-on-write.
    dispatch_table: Lock<ArrayRef<'gc, DispatchBucket<'gc>>>,
    /// One more than the highest argument position any method specializes.
    dispatch_width: Lock<u32>,
    /// The highest argument count any method distinguishes: calls with at
    /// least this many arguments share a key.
    dispatch_arity: Lock<u32>,
    /// Classes whose accessor method was resolved to a plain slot, keyed by
    /// the first argument's class: hits skip dispatch entirely.
    slot_cache: Lock<ArrayRef<'gc, SlotAccessEntry>>,
    /// Bodies of `NO_NEXT` methods by the classes of all (one or two)
    /// arguments, read by the machine `generic_call` entry: a hit enters the
    /// body without leaving generated code.
    method_cache: Lock<ArrayRef<'gc, MethodEntry<'gc>>>,
    max_required_dispatch_args: usize,
    fallback: Lock<Value<'gc>>,
    flags: Lock<GenericFlags>,
    /// Incremented with every dispatcher cache clear: any dispatch computed
    /// earlier may be stale. Call caches validate their epoch against this.
    epoch: Lock<u32>,
}

/// A slot accessor call resolved for one class: the same facts a slot inline
/// cache keeps (`class_id`, class `descriptor`, `index`).
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct SlotAccessEntry {
    class_id: u32,
    stamp: u32,
    index: u32,
    writer: bool,
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

/// Most distinct argument-class keys a generic's dispatch table holds. A
/// generic past this still dispatches correctly, it just recomputes the
/// applicable methods for keys the table has no room for.
pub(crate) const MAX_DISPATCH_ENTRIES: usize = 4096;

/// Smallest table capacity (a power of two).
const MIN_DISPATCH_CAPACITY: usize = 8;

/// Calls that reached `generic_closure_proc` (the Rust entry) while
/// [`DISPATCH_STATS`] was on: those the `generic_call` stub did not serve.
pub(crate) static RUST_ENTRIES: AtomicU64 = AtomicU64::new(0);
pub(crate) static DISPATCH_STATS: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Keys that did not fit a full table
pub(crate) static DISPATCH_OVERFLOWS: AtomicU64 = AtomicU64::new(0);

/// One slot of a generic's dispatch table: the applicable methods for
/// arguments whose first `dispatch_width` classes and argument count make up
/// `key` (see [`dispatch_key`]), plus `rest` for the classes past the first
/// two. Empty when `methods` is `None`. Buckets are immutable once
/// published; the table is replaced, never edited.
#[derive(Clone, Copy, Trace)]
#[repr(C)]
pub struct DispatchBucket<'gc> {
    key: u64,
    /// `BUCKET_DIRECT` when `body` can be entered with `#f` as the
    /// next-method argument.
    flags: u32,
    pad: u32,
    /// Class ids past the first two, for generics dispatching on more.
    rest: Option<ArrayRef<'gc, ClassId>>,
    methods: Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>>,
    /// The first method's body when it is a `NO_NEXT` closure, else null.
    body: Value<'gc>,
}

const BUCKET_DIRECT: u32 = 1;

const _: () = assert!(size_of::<Option<ArrayRef<'static, u8>>>() == size_of::<usize>());

impl<'gc> DispatchBucket<'gc> {
    fn empty() -> Self {
        Self {
            key: 0,
            flags: 0,
            pad: 0,
            rest: None,
            methods: None,
            body: Value::null(),
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

    pub const fn empty() -> Self {
        Self(0)
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
    let assertion_violation = ctx
        .globals()
        .root_module()
        .get_str(ctx, "assertion-violation")
        .unwrap_or_else(|| {
            panic!("pre boot error at {who}: {message} with {irritant}");
        });
    ctx.stage_tail_call(
        assertion_violation,
        &[ctx.intern(who), ctx.str(message), irritant],
    )
}

extern "C" fn generic_closure_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        // `generic_procedure` builds this closure with the descriptor in its
        // second free slot (the first holds the native), so the hot path
        // reads it directly and only a foreign closure takes the full lookup.
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
        // Read before any lookup: fills below publish only if no cache clear
        // ran since, so a result computed against old methods is never stored.
        let epoch = generic.epoch();
        if let Some(value) = GenericDescriptor::slot_access_fast(ctx, generic, call.args) {
            return call.return_one(value);
        }
        let found = match GenericDescriptor::cached_for_args(generic, call.args) {
            Some(methods) => Ok(methods),
            None => GenericDescriptor::applicable_methods_at(ctx, generic, call.args, epoch),
        };
        match found {
            Ok(methods) => {
                let first = methods[0];
                GenericDescriptor::note_slot_access(ctx, generic, first, call.args, epoch);
                GenericDescriptor::note_method_entry(ctx, generic, first, call.args, epoch);
                // The method body takes the next-method argument first. A
                // body that never reads it gets `#f`, so the hit path
                // allocates nothing.
                let body = first.body();
                if !body.is::<Closure>() {
                    return ctx.stage_tail_call(body, &[]);
                }
                let lead = if first.flags().no_next() {
                    Value::new(false)
                } else {
                    NextMethodDescriptor::new(
                        ctx,
                        generic,
                        methods,
                        Array::from_slice(ctx, call.args),
                        0,
                    )
                    .into()
                };
                call.enter_in_place(body, Some(lead))
            }
            Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                let fallback = generic.fallback();
                if fallback.is::<Closure>() {
                    call.enter_in_place(fallback, None)
                } else {
                    ctx.stage_tail_call(fallback, call.args)
                }
            }
            Err(GenericDispatchError::Arity) => generic_assertion_violation(
                ctx,
                "generic-invoke",
                "not enough dispatch arguments",
                Value::new(call.args.len() as i32),
            ),
            Err(GenericDispatchError::NoApplicableMethod) => generic_assertion_violation(
                ctx,
                "generic-invoke",
                "no applicable method",
                call.rator,
            ),
        }
    })
}

/// The native body of the stock `make` method. Called as a method body, so
/// the frame is `[next, class, initarg ...]`. While `initialize` still has
/// only its default method (`default_body`), `make` is just `make-instance`:
/// the init args are read straight from the frame, with no rest list, no
/// `apply` and no `initialize` call. Anything else runs the Scheme method
/// (`slow`, the third free variable) with the same arguments.
extern "C" fn make_method_proc<'gc>(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        // SAFETY: only entered through the closure `make_method_body` builds.
        let closure = unsafe { call.rator.downcast_unchecked::<Closure>() };
        let (initialize, default_body, slow) =
            (closure[1].get(), closure[2].get(), closure[3].get());
        if call.args.len() >= 2 && call.args[1].is::<super::ClassDescriptor>() {
            let class = call.args[1].downcast::<super::ClassDescriptor>();
            if generic_descriptor_from_value(ctx, initialize)
                .is_some_and(|generic| initialize_is_plain(ctx, generic, class, default_body))
            {
                return call.run(|call| super::ops::instantiate(ctx, call, class, &call.args[2..]));
            }
        }
        ctx.stage_tail_call(slow, call.args)
    })
}

/// Whether the stock `initialize` method alone applies to instances of
/// `class`, so `make` need not call it. Decided per class and cached on the
/// descriptor; adding or replacing an `initialize` method resets the answer
/// for the classes it can apply to (after bumping the generic's epoch, which
/// a computation that raced with the change sees and takes back).
fn initialize_is_plain<'gc>(
    ctx: MutationContext<'gc>,
    initialize: Gc<'gc, GenericDescriptor<'gc>>,
    class: Gc<'gc, ClassDescriptor<'gc>>,
    default_body: Value<'gc>,
) -> bool {
    use super::descriptor::{PROTOCOL_CUSTOM, PROTOCOL_PLAIN};
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
    // cached slot reader without leaving generated code and otherwise falls
    // into `subr_call`.
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
    specializers: ArrayRef<'gc, ClassId>,
    required_arg_count: usize,
    body: Lock<Value<'gc>>,
    flags: Lock<MethodFlags>,
    /// The slot a `SLOT_READER`/`SLOT_WRITER` body accesses (a symbol), else empty.
    slot_name: Lock<Value<'gc>>,
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
    /// boot by `%mark-boot-slot-protocol!`. Replacing the method (the same
    /// specializers) installs fresh flags, which drops the mark.
    const BOOT_SLOT_PROTOCOL: u8 = 1 << 4;

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

    pub const fn with_slot_reader(self) -> Self {
        Self(self.0 | Self::SLOT_READER)
    }

    pub const fn with_slot_writer(self) -> Self {
        Self(self.0 | Self::SLOT_WRITER)
    }

    pub const fn with_boot_slot_protocol(self) -> Self {
        Self(self.0 | Self::BOOT_SLOT_PROTOCOL)
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

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct NextMethodDescriptor<'gc> {
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    args: ArrayRef<'gc, Value<'gc>>,
    index: usize,
}

impl<'gc> GenericDescriptor<'gc> {
    pub fn new(
        ctx: MutationContext<'gc>,
        name: &str,
        max_required_dispatch_args: usize,
    ) -> Gc<'gc, Self> {
        // A fresh protocol generic may replace the boot one in the root
        // module, invalidate slot caches filled against the old object.
        // (The caller owes the grace period.)
        if is_slot_protocol_generic(name) {
            fire_slot_protocol(ctx);
        }
        Gc::new_with_header_word(
            ctx,
            Self {
                name: Array::from_slice(ctx, name.as_bytes()),
                methods: Lock::new(empty_method_metadata(ctx)),
                dispatch_table: Lock::new(empty_dispatch_table(ctx)),
                dispatch_width: Lock::new(1),
                dispatch_arity: Lock::new(0),
                slot_cache: Lock::new(empty_slot_cache(ctx)),
                method_cache: Lock::new(empty_method_cache(ctx)),
                max_required_dispatch_args,
                fallback: Lock::new(Value::empty()),
                flags: Lock::new(GenericFlags::empty()),
                epoch: Lock::new(0),
            },
            class_header_word(super::builtin::builtin_id(builtin_class_ids::GENERIC)),
        )
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

    pub fn flags(&self) -> GenericFlags {
        self.flags.get()
    }

    /// The cache epoch (Acquire). A filler reads it before computing a
    /// result and publishes only if it is unchanged afterwards.
    pub fn epoch(&self) -> u32 {
        self.epoch_atomic().load(Ordering::Acquire)
    }

    /// Whether a filler that read `epoch` may still publish: no cache clear
    /// ran since, and none was mid-change when it read (an odd epoch marks a
    /// method change between [`Self::begin_change`] and its closing clear).
    #[inline]
    fn epoch_is_current(&self, epoch: u32) -> bool {
        epoch & 1 == 0 && self.epoch() == epoch
    }

    #[inline]
    fn epoch_atomic(&self) -> &AtomicU32 {
        // SAFETY: `Lock<u32>` is `repr(transparent)` over `Cell<u32>`, which has
        // the layout of `u32`; the field is only ever written through this view
        // (and the initializer), so mixing atomic and plain access cannot occur.
        unsafe { &*(self.epoch.as_ptr() as *const AtomicU32) }
    }

    pub fn set_sealed(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        sealed: bool,
    ) {
        let flags = if sealed {
            generic.flags().with_sealed()
        } else {
            generic.flags().without_sealed()
        };
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, flags)
            .unlock()
            .set(flags);
    }

    pub fn set_fallback(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        fallback: Value<'gc>,
    ) {
        barrier::field!(Gc::write(ctx, generic), GenericDescriptor, fallback)
            .unlock()
            .set(fallback);
        // Slot caches require protocol generics to have no fallback.
        if is_slot_protocol_generic(generic.name()) {
            fire_slot_protocol(ctx);
        } else if generic.name() == "initialize" {
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
            required_arg_count,
            body,
            MethodFlags::empty(),
        )
    }

    pub fn add_method_with_flags(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        Self::add_method_in_table(
            ctx,
            class_table(ctx),
            generic,
            specializers,
            required_arg_count,
            body,
            flags,
        )
    }

    pub fn add_method_in_table(
        ctx: MutationContext<'gc>,
        table: &super::table::ClassTable<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, MethodDescriptor<'gc>> {
        assert!(
            specializers.len() <= required_arg_count,
            "method cannot specialize more arguments than it requires"
        );
        assert!(
            !generic.flags().sealed(),
            "sealed generic cannot accept methods"
        );
        Self::begin_change(ctx, generic);
        if let Some(method) = generic.find_method(specializers, required_arg_count) {
            MethodDescriptor::set_body(ctx, method, body);
            MethodDescriptor::set_flags(ctx, method, flags);
            Self::clear_dispatcher_cache(ctx, generic);
            Self::note_method_change(ctx, generic, method);
            return method;
        }
        let method =
            MethodDescriptor::new(ctx, generic, specializers, required_arg_count, body, flags);
        Self::append_method(ctx, generic, method);
        Self::clear_dispatcher_cache(ctx, generic);
        Self::note_method_change(ctx, generic, method);
        for specializer in specializers {
            if let Some(class) = table.lookup(*specializer) {
                ClassDescriptor::add_direct_method(ctx, class, method.into());
            }
        }
        method
    }

    pub fn find_method(
        &self,
        specializers: &[ClassId],
        required_arg_count: usize,
    ) -> Option<Gc<'gc, MethodDescriptor<'gc>>> {
        self.methods().iter().copied().find(|method| {
            method.required_arg_count() == required_arg_count
                && method.specializers() == specializers
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
    /// method's body directly, with its next-method argument: the machinery
    /// of `generic_closure_proc` without re-entering the generic procedure.
    /// `None` when there is no applicable method, or the body is not a
    /// closure: the caller then calls the generic itself.
    pub(crate) fn tail_effective(
        ctx: MutationContext<'gc>,
        call: &crate::runtime::vm::Call<'_, 'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        let methods = Self::applicable_methods(ctx, generic, args).ok()?;
        let first = methods[0];
        let body = first.body();
        if !body.is::<Closure>() {
            return None;
        }
        let lead = if first.flags().no_next() {
            Value::new(false)
        } else {
            NextMethodDescriptor::new(ctx, generic, methods, Array::from_slice(ctx, args), 0).into()
        };
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
        let width = (generic.dispatch_width_value() as usize).min(args.len());
        let arity = generic.arity_key(args.len());
        // Stack key: the hit path allocates nothing. Only a miss builds the
        // heap key the table bucket keeps. Arguments past `width` cannot
        // change which methods apply, so they are not part of it.
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
        let methods = generic.slow_applicable_methods(ctx, args)?;
        Self::insert_dispatcher_cache(ctx, generic, &class_ids, arity, methods, epoch);
        Ok(methods)
    }

    /// The highest argument count any method distinguishes (see
    /// `dispatch_arity`).
    fn dispatch_width_value(&self) -> u32 {
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

    /// Recompute the key shape after a method was added: the width reaches
    /// the highest specialized position, the arity the highest argument
    /// count a method needs. Called before the cache clear, so a table is
    /// never read under a shape newer than its own for longer than that
    /// window.
    fn update_dispatch_shape(&self, method: &MethodDescriptor<'gc>) {
        let top = ClassId::new(builtin_class_ids::TOP);
        let specialized = method
            .specializers
            .iter()
            .rposition(|specializer| Some(*specializer) != top)
            .map_or(0, |position| position + 1) as u32;
        let needed = (method.required_arg_count as u32)
            .max(method.specializers.len() as u32)
            .min(MAX_ARITY_KEY);
        // SAFETY: as in `dispatch_width_value`; `fetch_max` is atomic, so
        // concurrent adds need no lock.
        unsafe {
            (*(self.dispatch_width.as_ptr() as *const AtomicU32))
                .fetch_max(specialized.max(1), Ordering::AcqRel);
            (*(self.dispatch_arity.as_ptr() as *const AtomicU32))
                .fetch_max(needed, Ordering::AcqRel);
        }
    }

    /// Serve a plain slot accessor call without dispatching: the first
    /// argument's class has a cached resolution to a slot reader or writer.
    /// `None` takes the normal path (nothing cached, unbound slot, changed
    /// protocol or class).
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
        for entry in cache.iter() {
            if entry.class_id != class_id {
                continue;
            }
            if args.len() != 1 + entry.writer as usize {
                return None;
            }
            // SAFETY: a Scheme class id on the header means a `SchemeInstance`
            // layout. Storage with the entry's stamp was built for the
            // descriptor `index` was resolved against.
            let instance: Gc<SchemeInstance> = unsafe { Gc::from_gc_object(obj.as_cell_raw()) };
            if instance.storage().stamp() != entry.stamp {
                return None;
            }
            if entry.writer {
                return SchemeInstance::set_slot_checked(
                    ctx,
                    instance,
                    entry.index as usize,
                    args[1],
                )
                .then_some(args[1]);
            }
            let value = instance.slot_checked(entry.index as usize)?;
            return (!value.is_empty()).then_some(value);
        }
        None
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
        if !(flags.slot_reader() || writer) || args.len() != 1 + writer as usize {
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
            if current.len() >= 8 || current.iter().any(|e| e.class_id == hit.class_id) {
                return;
            }
            let appended = Array::with(ctx, current.len() + 1, |_, index| {
                if index < current.len() {
                    current[index]
                } else {
                    SlotAccessEntry {
                        class_id: hit.class_id,
                        stamp: hit.stamp,
                        index: hit.index,
                        writer,
                    }
                }
            });
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

    fn slow_applicable_methods(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>, GenericDispatchError> {
        if args.len() < self.max_required_dispatch_args {
            return Err(GenericDispatchError::Arity);
        }
        let mut methods = Vec::new();
        for method in self.methods().iter().copied() {
            let Some(score) = method.specificity_score(ctx, args) else {
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
    /// function so a cached key always agrees with a fresh dispatch.
    ///
    /// This is the value's own class id, read directly without a table lookup.
    /// Cache entries are only inserted after slow dispatch resolved the same ids, and the
    /// class table only grows (duplicate registration is an error), so an
    /// inserted key always resolves the way it did at insert time.
    pub(crate) fn dispatch_class_id(value: Value<'_>) -> Option<ClassId> {
        value.class_id()
    }

    pub(crate) fn cached_for_args(
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        args: &[Value<'gc>],
    ) -> Option<ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>> {
        let table = generic.dispatch_table.get();
        if table.is_empty() {
            return None;
        }
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
        Self::probe(table, key, &rest).and_then(|bucket| bucket.methods)
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
        for _ in 0..=FILL_RETRIES {
            // Load before validating the epoch (see `begin_change`).
            let current = generic.dispatch_table.load(Ordering::Acquire);
            if !generic.epoch_is_current(epoch) {
                return;
            }
            if Self::probe(current, key, rest).is_some() {
                return;
            }
            let occupied: Vec<DispatchBucket<'gc>> = current
                .as_slice()
                .iter()
                .copied()
                .filter(|bucket| bucket.methods.is_some())
                .collect();
            if occupied.len() >= MAX_DISPATCH_ENTRIES {
                DISPATCH_OVERFLOWS.fetch_add(1, Ordering::Relaxed);
                return;
            }
            let first = methods[0];
            let direct = first.flags().no_next() && first.body().is::<Closure>();
            let fresh = DispatchBucket {
                key,
                flags: if direct { BUCKET_DIRECT } else { 0 },
                pad: 0,
                rest: (!rest.is_empty()).then(|| Array::from_slice(ctx, rest)),
                methods: Some(methods),
                body: if direct { first.body() } else { Value::null() },
            };
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

    pub(crate) fn clear_dispatcher_cache(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
    ) {
        // Bump first: a filler that read the old epoch is refused from here
        // on, and one that already passed its check loses the CAS below
        // because the array it expected has been swapped out (fillers load
        // that array before checking the epoch). After `begin_change` the
        // epoch is odd and this closes the change (+1); otherwise it steps
        // over an even epoch (+2), keeping it even.
        let step = if generic.epoch() & 1 == 1 { 1 } else { 2 };
        generic.epoch_atomic().fetch_add(step, Ordering::AcqRel);
        Self::wipe_caches(ctx, generic);
    }

    /// Open a method change: the epoch turns odd, which refuses every filler,
    /// and the caches are emptied *before* the method set or a body is
    /// touched. A filler that read an even epoch then either published
    /// before this wipe (and is wiped) or loses its CAS.
    fn begin_change(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        generic.epoch_atomic().fetch_add(1, Ordering::AcqRel);
        Self::wipe_caches(ctx, generic);
    }

    fn wipe_caches(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        generic
            .dispatch_table
            .atomic_swap(empty_dispatch_table(ctx), Ordering::Release);
        generic
            .slot_cache
            .atomic_swap(empty_slot_cache(ctx), Ordering::Release);
        Self::clear_method_cache(ctx, generic);
    }

    /// Empty the slot-access cache: a watchpoint the cache depended on fired.
    /// The dispatcher and method caches do not depend on the slot protocol
    /// and stay.
    pub(crate) fn clear_slot_cache(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
    ) {
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        generic
            .slot_cache
            .atomic_swap(empty_slot_cache(ctx), Ordering::Release);
    }

    fn clear_method_cache(ctx: MutationContext<'gc>, generic: Gc<'gc, GenericDescriptor<'gc>>) {
        let owner = Gc::write(ctx, generic);
        let _ = owner;
        generic
            .method_cache
            .atomic_swap(empty_method_cache(ctx), Ordering::Release);
    }

    /// After a dispatch to a `NO_NEXT` closure method, remember it for
    /// arguments that are all cells (see [`MethodEntry`]).
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
            let appended = Array::with(ctx, current.len() + 1, |_, index| {
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
    /// protocol generic may no longer apply, but only for the classes the
    /// method can apply to (its specializers on the three dispatch
    /// arguments: metaclass, class, slot definition).
    fn note_method_change(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) {
        if is_slot_protocol_generic(generic.name()) {
            fire_slot_protocol_for_method(ctx, method);
        } else if generic.name() == "initialize" {
            class_table(ctx).reset_protocol_state_matching(OP_INIT, |class| {
                method.may_apply_to_classes(&[class])
            });
        }
    }

    fn append_method(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
    ) {
        let current = generic.methods.get();
        let appended = Array::with(ctx, current.len() + 1, |_, index| {
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
}

impl<'gc> MethodDescriptor<'gc> {
    fn new(
        ctx: MutationContext<'gc>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        specializers: &[ClassId],
        required_arg_count: usize,
        body: Value<'gc>,
        flags: MethodFlags,
    ) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            ctx,
            Self {
                generic,
                specializers: Array::from_slice(ctx, specializers),
                required_arg_count,
                body: Lock::new(body),
                flags: Lock::new(flags),
                slot_name: Lock::new(Value::empty()),
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

    pub fn required_arg_count(&self) -> usize {
        self.required_arg_count
    }

    pub fn body(&self) -> Value<'gc> {
        self.body.get()
    }

    pub fn flags(&self) -> MethodFlags {
        self.flags.get()
    }

    pub fn set_body(
        ctx: MutationContext<'gc>,
        method: Gc<'gc, MethodDescriptor<'gc>>,
        body: Value<'gc>,
    ) {
        barrier::field!(Gc::write(ctx, method), MethodDescriptor, body)
            .unlock()
            .set(body);
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
    /// instances of `classes` (further arguments unknown): every
    /// specializer within `classes` must be in its class's precedence list.
    /// Conservative for positions beyond `classes`: they may match.
    pub(crate) fn may_apply_to_classes(&self, classes: &[Gc<'gc, ClassDescriptor<'gc>>]) -> bool {
        self.specializers
            .iter()
            .zip(classes)
            .all(|(specializer, class)| class.cpl().contains(specializer))
    }

    fn specificity_score(
        &self,
        ctx: MutationContext<'gc>,
        args: &[Value<'gc>],
    ) -> Option<Vec<usize>> {
        if args.len() < self.required_arg_count || args.len() < self.specializers.len() {
            return None;
        }
        let mut score = Vec::with_capacity(args.len() + 1);
        for (index, arg) in args.iter().enumerate() {
            if let Some(specializer) = self.specializers.get(index) {
                let class = arg.class(ctx)?;
                let distance = class
                    .cpl()
                    .iter()
                    .position(|class_id| class_id == specializer)?;
                score.push(distance);
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
        Gc::new_with_header_word(
            ctx,
            Self {
                generic,
                methods,
                args,
                index,
            },
            class_header_word(super::builtin::builtin_id(builtin_class_ids::NEXT_METHOD)),
        )
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

    pub fn has_next(&self) -> bool {
        self.index + 1 < self.methods.len()
    }

    pub fn next(&self, ctx: MutationContext<'gc>) -> Option<Gc<'gc, Self>> {
        self.has_next()
            .then(|| Self::new(ctx, self.generic, self.methods, self.args, self.index + 1))
    }

    pub fn invocation_args(
        ctx: MutationContext<'gc>,
        next_method: Gc<'gc, Self>,
    ) -> ArrayRef<'gc, Value<'gc>> {
        Array::with(ctx, next_method.args.len() + 1, |_, index| {
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
}

// SAFETY: `gc` for `MethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for MethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::METHOD];
}

// SAFETY: `gc` for `NextMethodDescriptor` upholds all trait invariants
unsafe impl<'gc> ClassTagged for NextMethodDescriptor<'gc> {
    const TYPE_NAME: &'static str = "next-method";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::NEXT_METHOD];
}

fn empty_method_metadata<'gc>(
    ctx: MutationContext<'gc>,
) -> ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>> {
    Array::with(ctx, 0, |_, _| {
        unreachable!("zero-length method arrays are never initialized")
    })
}

fn empty_slot_cache<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, SlotAccessEntry> {
    Array::with(ctx, 0, |_, _| {
        unreachable!("zero-length slot cache arrays are never initialized")
    })
}

fn empty_method_cache<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, MethodEntry<'gc>> {
    Array::with(ctx, 0, |_, _| {
        unreachable!("zero-length method cache arrays are never initialized")
    })
}

fn empty_dispatch_table<'gc>(ctx: MutationContext<'gc>) -> ArrayRef<'gc, DispatchBucket<'gc>> {
    Array::with(ctx, 0, |_, _| {
        unreachable!("zero-length dispatch tables are never initialized")
    })
}
