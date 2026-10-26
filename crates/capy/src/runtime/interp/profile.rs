//! Profiles and caches stored in code block metadata.

use std::sync::atomic::Ordering::{AcqRel, Acquire, Relaxed, Release};
use std::sync::atomic::{AtomicU32, AtomicU64, fence};

use crate::runtime::codegen::addr::AtomicAddress;

use std::marker::PhantomData;

use crate::heap::alloc::Array;
use crate::heap::barrier::Write;
use crate::heap::mmtk::util::Address;
use crate::heap::object::{ClassId, GcObject, builtin_class_ids};
use crate::heap::{Gc, Trace, Visitor};
use crate::runtime::class::{ClassDescriptor, GenericDescriptor, MethodDescriptor};
use crate::runtime::value::Value;
use crate::runtime::value::{CodeBlock, CodeBuffer, CodeImage};

/// A typed GC reference stored in code buffer metadata. Zero bits mean
/// "none". The layout is one bare word so compiled code can load it by offset.
///
/// Stores take a [`Write`] proof for the owning [`CodeBuffer`], obtained from
/// [`Gc::write`], so a store without the barrier does not compile.
#[repr(transparent)]
pub struct GcCell<T: ?Sized = ()>(AtomicU64, PhantomData<fn() -> *const T>);

impl<T: ?Sized> Default for GcCell<T> {
    fn default() -> Self {
        Self::from_bits(0)
    }
}

impl<T: ?Sized> GcCell<T> {
    #[inline(always)]
    pub fn from_bits(bits: u64) -> Self {
        Self(AtomicU64::new(bits), PhantomData)
    }
    #[inline(always)]
    pub fn bits(&self) -> u64 {
        self.0.load(Relaxed)
    }
    #[inline(always)]
    pub fn is_none(&self) -> bool {
        self.bits() == 0
    }
    /// The stored reference as an untyped value. The collector may move the
    /// target at the next safepoint, like any other `Value`.
    #[inline(always)]
    pub fn get_value<'gc>(&self) -> Option<Value<'gc>> {
        let bits = self.bits();
        (bits != 0).then(|| Value::from_raw(bits))
    }
    /// Stores `value`. `_barrier` proves the write barrier ran on the
    /// buffer that holds this cell.
    #[inline(always)]
    pub fn set_value(&self, _barrier: &Write<CodeBuffer<'_>>, value: Option<Value<'_>>) {
        self.0.store(value.map_or(0, |v| v.bits()), Relaxed)
    }
}

/// Re-lifetimes the `'static` marker type a [`GcCell`] is declared with:
/// metadata structs cannot carry `'gc`, so the cell is declared over
/// `T<'static>` and hands out `T<'gc>`.
///
/// # Safety
/// `To` must be `Self` with its GC lifetimes replaced by `'gc`.
pub unsafe trait Rebind<'gc> {
    type To: 'gc;
}

macro_rules! rebind {
    ($($ty:ident),*) => {$(
        // SAFETY: only the lifetime differs.
        unsafe impl<'gc> Rebind<'gc> for $ty<'static> { type To = $ty<'gc>; }
    )*};
}
rebind!(
    CodeBlock,
    CodeImage,
    ClassDescriptor,
    GenericDescriptor,
    MethodDescriptor
);
// SAFETY: only the lifetimes differ.
unsafe impl<'gc, T: Rebind<'gc>> Rebind<'gc> for Array<Gc<'static, T>>
where
    Gc<'static, T>: 'static,
{
    type To = Array<Gc<'gc, T::To>>;
}

impl<T> GcCell<T> {
    /// The stored object. Zero bits are none, anything else is the address
    /// of a live object of type `T` (the only ways in are `set` and `set_value`).
    #[inline(always)]
    pub fn get<'gc>(&self) -> Option<Gc<'gc, <T as Rebind<'gc>>::To>>
    where
        T: Rebind<'gc>,
    {
        let bits = self.bits();
        // SAFETY: see above, a cell word is an object address, which is also
        // the `Value` encoding of a cell.
        (bits != 0).then(|| unsafe { Gc::from_gc_object(Value::from_raw(bits).as_cell_raw()) })
    }
    #[inline(always)]
    pub fn set<'gc>(
        &self,
        _barrier: &Write<CodeBuffer<'_>>,
        value: Option<Gc<'gc, <T as Rebind<'gc>>::To>>,
    ) where
        T: Rebind<'gc>,
    {
        let bits = value.map_or(0, |gc| gc.as_gc_object().to_address().as_usize() as u64);
        self.0.store(bits, Relaxed)
    }
}

impl<T> GcCell<T> {
    /// [`Self::get`] with Acquire
    #[inline(always)]
    pub fn get_acquire<'gc>(&self) -> Option<Gc<'gc, <T as Rebind<'gc>>::To>>
    where
        T: Rebind<'gc>,
    {
        let bits = self.0.load(Acquire);
        // SAFETY: as in `get`.
        (bits != 0).then(|| unsafe { Gc::from_gc_object(Value::from_raw(bits).as_cell_raw()) })
    }

    /// Replace `current` by `new` if the cell still holds `current`
    pub fn compare_exchange<'gc>(
        &self,
        _barrier: &Write<CodeBuffer<'_>>,
        current: Option<Gc<'gc, <T as Rebind<'gc>>::To>>,
        new: Option<Gc<'gc, <T as Rebind<'gc>>::To>>,
    ) -> bool
    where
        T: Rebind<'gc>,
    {
        let bits = |gc: Option<Gc<'gc, <T as Rebind<'gc>>::To>>| {
            gc.map_or(0, |gc| gc.as_gc_object().to_address().as_usize() as u64)
        };
        self.0
            .compare_exchange(bits(current), bits(new), AcqRel, Acquire)
            .is_ok()
    }

    /// Store none (Release). Dropping a reference needs no barrier.
    #[inline]
    pub fn clear_release(&self) {
        self.0.store(0, Release);
    }
}

impl<T: ?Sized> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        Self::from_bits(self.bits())
    }
}
impl<T: ?Sized> std::fmt::Debug for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcCell").field(&self.bits()).finish()
    }
}
impl<T: ?Sized> PartialEq for GcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.bits() == other.bits()
    }
}
impl<T: ?Sized> Eq for GcCell<T> {}
impl<T: ?Sized> std::hash::Hash for GcCell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bits().hash(state);
    }
}

const _: () = assert!(
    size_of::<Value<'static>>() == size_of::<AtomicU64>()
        && align_of::<Value<'static>>() <= align_of::<AtomicU64>()
);

macro_rules! no_gc_refs {
    ($($name:ident),*) => {$(
        // SAFETY: holds no GC references.
        unsafe impl Trace for $name {
            unsafe fn trace(&mut self, _visitor: &mut Visitor) {}
        }
    )*};
}
no_gc_refs!(ArithProfile, ValueProfile, ArrayProfile);

// SAFETY: the only GC reference is the stored word, traced in place.
unsafe impl<T: ?Sized> Trace for GcCell<T> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        // The visitor may record the slot's address and update it after this
        // returns, so the slot must be the field itself, never a copy.
        // SAFETY: `Value` has the layout of a `u64`, and `&mut self` is
        // exclusive: the world is stopped while the collector traces.
        if self.bits() == 0 {
            return;
        }
        // SAFETY: as above, the cast slot is the field itself.
        unsafe { (&mut *(self.0.get_mut() as *mut u64).cast::<Value<'static>>()).trace(visitor) }
    }
}

#[derive(Default)]
pub struct ArithProfile(pub AtomicU32);
#[derive(Default)]
pub struct ValueProfile(pub AtomicU32);
#[derive(Default)]
pub struct ArrayProfile(pub AtomicU32);

// Snapshot traits for metadata cells. Generated `Instruction`s own their
// metadata structs by value and derive `Clone`/`Debug`/equality, so the
// atomic cells implement those traits over relaxed loads: a clone is an
// independent cell holding the loaded snapshot, and equality compares
// snapshots. Races only affect tests and dumps, never the runtime paths,
// which observe the cells in place.
macro_rules! snapshot_cell {
    ($name:ident) => {
        impl Clone for $name {
            fn clone(&self) -> Self {
                Self(AtomicU32::new(self.0.load(Relaxed)))
            }
        }
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.0.load(Relaxed))
                    .finish()
            }
        }
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.0.load(Relaxed) == other.0.load(Relaxed)
            }
        }
        impl Eq for $name {}
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.load(Relaxed).hash(state);
            }
        }
    };
}

snapshot_cell!(ArithProfile);
snapshot_cell!(ValueProfile);
snapshot_cell!(ArrayProfile);
snapshot_cell!(CacheVersion);

/// Set `bits` in `cell`, a store only when something new was seen.
#[inline(always)]
pub fn observe(cell: &AtomicU32, bits: u32) {
    let old = cell.load(Relaxed);
    if old & bits != bits {
        cell.store(old | bits, Relaxed);
    }
}

// The profile formats follow WebKit JSC: arithmetic and array sites OR small
// type sets into a monotonic word (JSC's `ArithProfile`, updated with one
// `or16`). Value sites do the same with immediate kinds, plus the class id
// of the first heap object seen and a flag for further classes (a
// monomorphic inline cache in profile form). They store a class id, not a
// `Value`: a reference would need a write barrier for the code buffer,
// whose object address generated code does not have. Updates are racy but
// benign: a lost update only delays what the next observation re-records.

/// `number_kind` bits: what an arithmetic operand was seen as. Every
/// number type stays distinct, so a site records pairs like
/// lhs-flonum/rhs-fixnum or lhs-bignum/rhs-fixnum exactly.
pub const NUMBER_FIXNUM: u32 = 1;
pub const NUMBER_FLONUM: u32 = 1 << 1;
pub const NUMBER_BIGNUM: u32 = 1 << 2;
pub const NUMBER_RATNUM: u32 = 1 << 3;
pub const NUMBER_COMPLEX: u32 = 1 << 4;
pub const NUMBER_NON_NUMBER: u32 = 1 << 5;
/// Mask for one side's `number_kind` bits (bits 0-5 of a shifted word).
pub const NUMBER_KIND_MASK: u32 = 0x3f;

/// `ArithProfile` layout: `number_kind(lhs)` in bits 0-5, `rhs` in 6-11,
/// the result in 12-17, then the overflow and slow flags.
pub const ARITH_RHS_SHIFT: u32 = 6;
pub const ARITH_RESULT_SHIFT: u32 = 12;
/// Fixnum overflow seen (a slow path ran on two fixnums).
pub const ARITH_OVERFLOW: u32 = 1 << 18;
/// A slow path recorded here.
pub const ARITH_SLOW: u32 = 1 << 19;
/// What a fixnum-fixnum fast path ORs in.
pub const ARITH_FIXNUMS: u32 = NUMBER_FIXNUM | (NUMBER_FIXNUM << ARITH_RHS_SHIFT);
/// What a fixnum-flonum fast path ORs in.
pub const ARITH_FIX_FLON: u32 = NUMBER_FIXNUM | (NUMBER_FLONUM << ARITH_RHS_SHIFT);
/// What a flonum-fixnum fast path ORs in.
pub const ARITH_FLON_FIX: u32 = NUMBER_FLONUM | (NUMBER_FIXNUM << ARITH_RHS_SHIFT);
/// What a flonum-flonum fast path ORs in.
pub const ARITH_FLON_FLON: u32 = NUMBER_FLONUM | (NUMBER_FLONUM << ARITH_RHS_SHIFT);

/// Classify a value for arithmetic profiling.
#[inline(always)]
pub fn number_kind(value: Value<'_>) -> u32 {
    if value.is_int32() {
        return NUMBER_FIXNUM;
    }
    if value.is_flonum() {
        return NUMBER_FLONUM;
    }
    if value.is_cell() {
        match value.as_cell_raw().class_id().bits() {
            builtin_class_ids::BIGINT => return NUMBER_BIGNUM,
            builtin_class_ids::RATIONAL => return NUMBER_RATNUM,
            builtin_class_ids::COMPLEX => return NUMBER_COMPLEX,
            _ => {}
        }
    }
    NUMBER_NON_NUMBER
}

impl ArithProfile {
    /// Record the operand types of a binary operation.
    #[inline(always)]
    pub fn observe_operands(&self, lhs: Value<'_>, rhs: Value<'_>) {
        observe(
            &self.0,
            number_kind(lhs) | number_kind(rhs) << ARITH_RHS_SHIFT,
        )
    }
    /// Fast-path shortcut: both operands were fixnums.
    #[inline(always)]
    pub fn observe_fixnums(&self) {
        observe(&self.0, ARITH_FIXNUMS)
    }
    /// Record the operand of a unary operation (`neg`, `abs`, `sqrt`).
    /// Unary sites mirror the operand into both halves so
    /// [`ArithShape::from_profile`](crate::runtime::codegen::template::ArithShape::from_profile)
    /// classifies them like binary sites: all-fixnum observations read
    /// `FixnumOnly`, all-flonum read `FlonumOnly`.
    #[inline(always)]
    pub fn observe_unary(&self, x: Value<'_>) {
        self.observe_operands(x, x)
    }
    /// Record a slow path: the result kind (`None` for comparisons, whose
    /// boolean result carries no information) and whether the operands were
    /// fixnums that overflowed.
    #[inline(always)]
    pub fn observe_result(&self, result: Option<Value<'_>>, overflowed: bool) {
        let mut bits = ARITH_SLOW;
        if overflowed {
            bits |= ARITH_OVERFLOW;
        }
        if let Some(result) = result {
            bits |= number_kind(result) << ARITH_RESULT_SHIFT;
        }
        observe(&self.0, bits)
    }
}

/// `ValueProfile` layout. Every update only sets bits (or writes the class
/// field once, while it is zero):
///
/// ```text
/// bits 0-3   immediate kinds seen (VALUE_FIXNUM .. VALUE_OTHER_IMM)
/// bit  4     VALUE_CELL_MULTI: more than one heap class was seen
/// bits 5-7   reserved
/// bits 8-31  class id of the first heap object seen (0 = none yet)
/// ```

pub const VALUE_FIXNUM: u32 = 1;
pub const VALUE_FLONUM: u32 = 1 << 1;
pub const VALUE_BOOL: u32 = 1 << 2;
/// Null, chars, and the other immediates.
pub const VALUE_OTHER_IMM: u32 = 1 << 3;
pub const VALUE_IMM_MASK: u32 = 0xf;
pub const VALUE_CELL_MULTI: u32 = 1 << 4;
pub const VALUE_CLASS_SHIFT: u32 = 8;

#[inline(always)]
fn immediate_bit(value: Value<'_>) -> u32 {
    if value.is_int32() {
        VALUE_FIXNUM
    } else if value.is_flonum() {
        VALUE_FLONUM
    } else if value.is_bool() {
        VALUE_BOOL
    } else {
        VALUE_OTHER_IMM
    }
}

#[inline(always)]
pub fn update_value_profile(word: u32, value: Value<'_>) -> u32 {
    if !value.is_cell() {
        return word | immediate_bit(value);
    }
    let class = value.as_cell_raw().class_id().bits() << VALUE_CLASS_SHIFT;
    let seen = word >> VALUE_CLASS_SHIFT << VALUE_CLASS_SHIFT;
    if seen == 0 {
        word | class
    } else if seen == class {
        word
    } else {
        word | VALUE_CELL_MULTI
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ValueShape {
    /// Nothing observed yet.
    Never,
    Fixnum,
    Flonum,
    Bool,
    /// Heap objects of exactly one class, and nothing else.
    Class(ClassId),
    /// Fixnums and flonums only.
    Number,
    /// Anything else: several classes, or heap objects mixed with immediates.
    Mixed,
}

impl ValueShape {
    pub fn from_profile(word: u32) -> Self {
        let _ = word;
        ValueShape::Mixed
    }
}

impl ValueProfile {
    #[inline(always)]
    pub fn observe(&self, value: Value<'_>) {
        let word = self.0.load(Relaxed);
        let updated = update_value_profile(word, value);
        if updated != word {
            self.0.store(updated, Relaxed);
        }
    }
    pub fn shape(&self) -> ValueShape {
        ValueShape::from_profile(self.0.load(Relaxed))
    }
    /// Whether every observation (and at least one) was a heap object of `class`.
    pub fn only_class(&self, class: ClassId) -> bool {
        self.shape() == ValueShape::Class(class)
    }
    /// Whether more than one kind of value was seen.
    pub fn is_poly(&self) -> bool {
        matches!(self.shape(), ValueShape::Number | ValueShape::Mixed)
    }
}

/// `ArrayProfile` layout: what vector sites saw, OR-accumulated like
/// `ArithProfile`.
pub const ARRAY_VECTOR: u32 = 1 << 0;
pub const ARRAY_NON_VECTOR: u32 = 1 << 1;
pub const ARRAY_FIXNUM_INDEX: u32 = 1 << 2;
pub const ARRAY_NON_FIXNUM_INDEX: u32 = 1 << 3;
pub const ARRAY_IN_BOUNDS: u32 = 1 << 4;
pub const ARRAY_OUT_OF_BOUNDS: u32 = 1 << 5;
/// What a fast path (vector, fixnum index, in bounds) ORs in.
pub const ARRAY_FAST: u32 = ARRAY_VECTOR | ARRAY_FIXNUM_INDEX | ARRAY_IN_BOUNDS;

impl ArrayProfile {
    #[inline(always)]
    pub fn observe(&self, bits: u32) {
        observe(&self.0, bits)
    }
}

/// Version counter that guards a multi-field cache entry (a seqlock).
#[derive(Default)]
pub struct CacheVersion(AtomicU32);

impl CacheVersion {
    /// Version at the start of a read.
    #[inline(always)]
    pub fn begin_read(&self) -> u32 {
        self.0.load(Acquire)
    }

    /// Whether the fields loaded since `begin_read` returned `start` all
    /// belong to one completed fill.
    #[inline(always)]
    pub fn validate(&self, start: u32) -> bool {
        fence(Acquire);
        start & 1 == 0 && self.0.load(Relaxed) == start
    }

    /// Claims the entry for a fill. Returns `None` while another thread
    /// fills it.
    #[inline]
    pub fn begin_write(&self) -> Option<u32> {
        let start = self.0.load(Relaxed);
        if start & 1 != 0 {
            return None;
        }
        self.0
            .compare_exchange(start, start.wrapping_add(1), Relaxed, Relaxed)
            .ok()?;
        // Orders the odd version before the field stores that follow.
        fence(Release);
        Some(start)
    }

    /// Publishes the fill claimed by the `begin_write` that returned `start`.
    #[inline]
    pub fn end_write(&self, start: u32) {
        self.0.store(start.wrapping_add(2), Release);
    }
}

/// What a slot IC entry does on a hit.
pub const SLOT_KIND_ACCESS: u32 = 0;

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SlotIcEntry<'gc> {
    pub class_id: u32,
    pub stamp: u32,
    pub index: u32,
    pub kind: u32,
    /// What a non-`ACCESS` entry calls, else the empty value.
    pub effective: Value<'gc>,
}

rebind!(SlotIcEntry);

/// Entries behind a site's inline entry.
pub const SLOT_POLY_ENTRIES: usize = 3;

/// The instance slot cache of a `get_slot` or `put_slot`.
///
/// `mono` is the inline entry the compiled code checks first (it keeps the
/// first class the site saw), `poly` an immutable array of up to
/// [`SLOT_POLY_ENTRIES`] more.
#[derive(Default, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SlotSite {
    pub mono: GcCell<SlotIcEntry<'static>>,
    pub poly: GcCell<Array<Gc<'static, SlotIcEntry<'static>>>>,
}

// SAFETY: traces both cells.
unsafe impl Trace for SlotSite {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.mono.trace(visitor);
            self.poly.trace(visitor);
        }
    }
}

type PolyRef<'gc> = Gc<'gc, Array<Gc<'gc, SlotIcEntry<'gc>>>>;

impl SlotSite {
    /// The inline entry.
    #[inline(always)]
    pub fn mono<'gc>(&self) -> Option<Gc<'gc, SlotIcEntry<'gc>>> {
        self.mono.get_acquire()
    }

    #[inline(always)]
    pub fn poly<'gc>(&self) -> Option<PolyRef<'gc>> {
        self.poly.get_acquire()
    }

    /// The entry for `class_id`, from the inline entry or the extra ones.
    #[inline(always)]
    pub fn find<'gc>(&self, class_id: u32) -> Option<Gc<'gc, SlotIcEntry<'gc>>> {
        if let Some(entry) = self.mono()
            && entry.class_id == class_id
        {
            return Some(entry);
        }
        self.poly()?
            .iter()
            .copied()
            .find(|entry| entry.class_id == class_id)
    }

    /// Publish `entry`: in the inline slot when it is empty or already this
    /// class, else among the extra entries (oldest out when full). Returns
    /// whether it was published; losing a race is fine, the next miss
    /// fills again. `barrier` proves the write barrier ran on the code
    /// buffer holding this site.
    pub fn fill<'gc>(
        &self,
        ctx: crate::runtime::MutationContext<'gc>,
        barrier: &Write<CodeBuffer<'_>>,
        entry: Gc<'gc, SlotIcEntry<'gc>>,
    ) -> bool {
        for _ in 0..2 {
            let mono = self.mono();
            if mono.is_none_or(|m| m.class_id == entry.class_id) {
                if self.mono.compare_exchange(barrier, mono, Some(entry)) {
                    return true;
                }
                continue;
            }
            let current = self.poly();
            let kept: Vec<Gc<'gc, SlotIcEntry<'gc>>> = current
                .iter()
                .flat_map(|poly| poly.iter().copied())
                .filter(|e| e.class_id != entry.class_id)
                .collect();
            let skip = (kept.len() + 1).saturating_sub(SLOT_POLY_ENTRIES);
            let rebuilt = Array::new(ctx, kept.len() - skip + 1, |_, index| {
                if index + skip < kept.len() {
                    kept[index + skip]
                } else {
                    entry
                }
            });
            if self.poly.compare_exchange(barrier, current, Some(rebuilt)) {
                return true;
            }
        }
        false
    }

    /// Empty the site: a watchpoint fired for the assumption its entries
    /// were filled under. A reader that loaded an entry before this finishes
    /// one access with it, which the grace period after the fire accounts
    /// for.
    pub fn clear(&self) {
        self.mono.clear_release();
        self.poly.clear_release();
    }
}

/// Call site profile and generic-function dispatch cache of a `call` or
/// `tail-call`.
#[derive(Default)]
pub struct CallLinkInfo {
    /// `CodeBlock` of the first closure called here. Never replaced: a
    /// different callee sets `polymorphic` instead.
    pub callee: GcCell<CodeBlock<'static>>,
    /// Call-site shape: [`LINK_MONO`] until a second code block is called
    /// here, [`LINK_POLY`] while the site's stub serves a few callees,
    /// [`LINK_MEGA`] once more distinct callees arrived than the stub
    /// holds.
    pub polymorphic: AtomicU32,
    /// Base address of the site's lazily generated call IC stub (0 = none).
    pub stub: AtomicAddress,
    /// The stub's cases: the `CodeBlock`s it enters directly.
    pub cases: [GcCell<CodeBlock<'static>>; CALL_IC_CASES],
    /// Byte distance from the start of the owning `CodeBuffer` to this
    /// link (0 = not bound).
    pub buffer_offset: AtomicU32,
    pub stub_owner: GcCell<CodeImage<'static>>,
    /// Guards every `generic_*` field below.
    pub generic_version: CacheVersion,
    /// The cached `GenericDescriptor`, 0 = empty.
    pub generic: GcCell<GenericDescriptor<'static>>,
    /// `GenericDescriptor::epoch` at fill time.
    pub generic_epoch: AtomicU32,
    /// Low two bits: number of class ids in the key, 0..=2. High bits:
    /// the argument count the cached chain was computed for. A hit needs at
    /// least that many arguments: the fill only caches generics whose
    /// methods all require at most that many, and method applicability only
    /// grows with the argument count.
    pub generic_dispatch: AtomicU32,
    pub generic_class_ids: [AtomicU32; 2],
    /// The applicable method chain
    pub generic_methods: GcCell<Array<Gc<'static, MethodDescriptor<'static>>>>,
}

/// [`CallLinkInfo::polymorphic`] while at most one code block was called
/// here (or none yet).
pub const LINK_MONO: u32 = 0;
/// [`CallLinkInfo::polymorphic`] once a second code block was called here:
/// the site's stub serves the few callees seen so far.
pub const LINK_POLY: u32 = 1;
/// [`CallLinkInfo::polymorphic`] once more distinct callees arrived than
/// the stub holds: the site stays on the generic inline path and the miss
/// path extends nothing.
pub const LINK_MEGA: u32 = 2;
/// Callee cases per call IC stub, one more distinct callee turns the site
/// megamorphic.
pub const CALL_IC_CASES: usize = 4;

/// A consistent copy of a filled generic dispatch cache.
#[derive(Clone, Copy)]
pub struct GenericCacheEntry<'gc> {
    pub generic: Gc<'gc, GenericDescriptor<'gc>>,
    pub epoch: u32,
    pub key_len: u32,
    pub min_argc: u32,
    pub class_ids: [u32; 2],
    pub methods: Gc<'gc, Array<Gc<'gc, MethodDescriptor<'gc>>>>,
}

impl CallLinkInfo {
    /// The cached dispatch, or `None` when the cache is empty or a fill
    /// raced the read.
    #[inline(always)]
    pub fn read_generic<'gc>(&self) -> Option<GenericCacheEntry<'gc>> {
        let start = self.generic_version.begin_read();
        let packed = self.generic_dispatch.load(Relaxed);
        let (generic, methods) = (self.generic.get()?, self.generic_methods.get()?);
        let entry = GenericCacheEntry {
            generic,
            epoch: self.generic_epoch.load(Relaxed),
            key_len: packed & 0b11,
            min_argc: packed >> 2,
            class_ids: [
                self.generic_class_ids[0].load(Relaxed),
                self.generic_class_ids[1].load(Relaxed),
            ],
            methods,
        };
        self.generic_version.validate(start).then_some(entry)
    }

    /// Stores a dispatch unless another thread is filling the cache. Returns
    /// whether it stored.
    pub fn fill_generic<'gc>(
        &self,
        barrier: &Write<CodeBuffer<'_>>,
        generic: Gc<'gc, GenericDescriptor<'gc>>,
        epoch: u32,
        class_ids: &[u32],
        min_argc: u32,
        methods: Gc<'gc, Array<Gc<'gc, MethodDescriptor<'gc>>>>,
    ) -> bool {
        debug_assert!(class_ids.len() <= 2);
        let Some(start) = self.generic_version.begin_write() else {
            return false;
        };
        self.generic.set(barrier, Some(generic));
        self.generic_epoch.store(epoch, Relaxed);
        self.generic_dispatch
            .store(class_ids.len() as u32 | (min_argc << 2), Relaxed);
        for (slot, class_id) in self.generic_class_ids.iter().zip(class_ids) {
            slot.store(*class_id, Relaxed);
        }
        self.generic_methods.set(barrier, Some(methods));
        self.generic_version.end_write(start);
        true
    }
}

impl CallLinkInfo {
    /// Records that this link lives `offset` bytes into its code buffer.
    pub(crate) fn bind_buffer(&self, offset: usize) {
        self.buffer_offset.store(
            u32::try_from(offset).expect("code buffer is too large"),
            Relaxed,
        );
    }

    /// The `CodeBuffer` that holds this link, or `None` for a link that was
    /// never bound to one.
    #[inline(always)]
    pub fn buffer<'gc>(&self) -> Option<Gc<'gc, CodeBuffer<'gc>>> {
        let offset = self.buffer_offset.load(Relaxed) as usize;
        // SAFETY: a bound link sits `offset` bytes into a live, non-moving
        // `CodeBuffer`, which the link's own liveness implies.
        (offset != 0).then(|| unsafe {
            Gc::from_gc_object(GcObject::from_address(Address::from_usize(
                self as *const Self as usize - offset,
            )))
        })
    }

    /// Whether the site went megamorphic: more distinct callees than the
    /// stub holds, served by the generic inline path.
    #[inline(always)]
    pub fn is_megamorphic(&self) -> bool {
        self.polymorphic.load(Relaxed) == LINK_MEGA
    }
}

impl Clone for CallLinkInfo {
    fn clone(&self) -> Self {
        Self {
            callee: self.callee.clone(),
            polymorphic: AtomicU32::new(self.polymorphic.load(Relaxed)),
            stub: AtomicAddress::new(self.stub.load(Relaxed)),
            cases: self.cases.clone(),
            buffer_offset: AtomicU32::new(0),
            stub_owner: self.stub_owner.clone(),
            generic_version: self.generic_version.clone(),
            generic: self.generic.clone(),
            generic_epoch: AtomicU32::new(self.generic_epoch.load(Relaxed)),
            generic_dispatch: AtomicU32::new(self.generic_dispatch.load(Relaxed)),
            generic_class_ids: [
                AtomicU32::new(self.generic_class_ids[0].load(Relaxed)),
                AtomicU32::new(self.generic_class_ids[1].load(Relaxed)),
            ],
            generic_methods: self.generic_methods.clone(),
        }
    }
}
impl std::fmt::Debug for CallLinkInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallLinkInfo")
            .field("callee", &self.callee)
            .field("polymorphic", &self.polymorphic.load(Relaxed))
            .field("stub", &self.stub.load(Relaxed))
            .field("cases", &self.cases)
            .field("stub_owner", &self.stub_owner)
            .field("generic_version", &self.generic_version)
            .field("generic", &self.generic)
            .field("generic_epoch", &self.generic_epoch.load(Relaxed))
            .field("generic_dispatch", &self.generic_dispatch.load(Relaxed))
            .field(
                "generic_class_ids",
                &[
                    self.generic_class_ids[0].load(Relaxed),
                    self.generic_class_ids[1].load(Relaxed),
                ],
            )
            .field("generic_methods", &self.generic_methods)
            .finish()
    }
}
impl PartialEq for CallLinkInfo {
    fn eq(&self, other: &Self) -> bool {
        self.callee == other.callee
            && self.polymorphic.load(Relaxed) == other.polymorphic.load(Relaxed)
            && self.stub.load(Relaxed) == other.stub.load(Relaxed)
            && self.cases == other.cases
            && self.stub_owner == other.stub_owner
            && self.generic_version == other.generic_version
            && self.generic == other.generic
            && self.generic_epoch.load(Relaxed) == other.generic_epoch.load(Relaxed)
            && self.generic_dispatch.load(Relaxed) == other.generic_dispatch.load(Relaxed)
            && self.generic_class_ids[0].load(Relaxed) == other.generic_class_ids[0].load(Relaxed)
            && self.generic_class_ids[1].load(Relaxed) == other.generic_class_ids[1].load(Relaxed)
            && self.generic_methods == other.generic_methods
    }
}
impl Eq for CallLinkInfo {}
impl std::hash::Hash for CallLinkInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.callee.hash(state);
        self.polymorphic.load(Relaxed).hash(state);
        self.stub.load(Relaxed).hash(state);
        self.cases.hash(state);
        self.stub_owner.hash(state);
        self.generic_version.hash(state);
        self.generic.hash(state);
        self.generic_epoch.load(Relaxed).hash(state);
        self.generic_dispatch.load(Relaxed).hash(state);
        self.generic_class_ids[0].load(Relaxed).hash(state);
        self.generic_class_ids[1].load(Relaxed).hash(state);
        self.generic_methods.hash(state);
    }
}

// SAFETY: traces every GC field.
unsafe impl Trace for CallLinkInfo {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.callee.trace(visitor);
            for case in &mut self.cases {
                case.trace(visitor);
            }
            self.stub_owner.trace(visitor);
            self.generic.trace(visitor);
            self.generic_methods.trace(visitor);
        }
    }
}
