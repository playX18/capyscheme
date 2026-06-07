use crate::rsgc::{collection::Visitor, mm::MemoryManager, traits::Trace, weak::WeakProcessor};
use crate::runtime::Context;
use crate::utils::easy_bitfield::{AtomicBitfieldContainer, BitField, BitFieldTrait};
use core::fmt;
use mmtk::util::{
    Address, ObjectReference, constants::LOG_BYTES_IN_ADDRESS, conversions::raw_align_up,
};
use std::{
    cell::Cell,
    collections::HashMap,
    marker::PhantomData,
    num::NonZeroU32,
    sync::{Mutex, OnceLock},
};

/// Offset from allocation to the actual object
pub const OBJECT_REF_OFFSET: isize = 8;
pub const OBJECT_HEADER_OFFSET: isize = -OBJECT_REF_OFFSET;
pub const HASHCODE_OFFSET: isize = -(OBJECT_REF_OFFSET + size_of::<u64>() as isize);
pub const HASHCODE_BYTES: usize = size_of::<u64>();

const HASH_UNHASHED: u8 = 0;
const HASH_HASHED: u8 = 1;
const HASH_HASHED_AND_MOVED: u8 = 2;
pub const MAX_CLASS_ID: u32 = (1 << 24) - 1;
pub const MIN_GC_ONLY_CLASS_ID: u32 = 1 << 23;

pub mod builtin_class_ids {
    pub const PAIR: u32 = 1;
    pub const VARIABLE: u32 = 2;
    pub const CLOSURE_PROC: u32 = 3;
    pub const CLOSURE_K: u32 = 4;
    pub const MUTABLE_VECTOR: u32 = 5;
    pub const IMMUTABLE_VECTOR: u32 = 6;
    pub const TUPLE: u32 = 7;

    pub const TOP: u32 = 8;
    pub const BOTTOM: u32 = 9;
    pub const TYPE: u32 = 10;
    pub const CLASS: u32 = 11;
    pub const OBJECT: u32 = 12;
    pub const BOOL: u32 = 13;
    pub const CHAR: u32 = 14;
    pub const NULL: u32 = 15;
    pub const EOF: u32 = 16;
    pub const VOID: u32 = 17;
    pub const UNSPECIFIED: u32 = 18;
    pub const UNDEFINED: u32 = 19;
    pub const FIXNUM: u32 = 20;
    pub const FLONUM: u32 = 21;
    pub const BIGINT: u32 = 22;
    pub const RATIONAL: u32 = 23;
    pub const COMPLEX: u32 = 24;
    pub const NUMBER: u32 = 25;
    pub const SYMBOL: u32 = 26;
    pub const KEYWORD: u32 = 27;
    pub const STRING: u32 = 28;
    pub const IMMUTABLE_STRING: u32 = 29;
    pub const STRINGBUF_WIDE: u32 = 30;
    pub const STRINGBUF_NARROW: u32 = 31;
    pub const MUTABLE_BYTEVECTOR: u32 = 32;
    pub const IMMUTABLE_BYTEVECTOR: u32 = 33;
    pub const MAPPED_BYTEVECTOR: u32 = 34;
    pub const HASHTABLE: u32 = 35;
    pub const IMMUTABLE_HASHTABLE: u32 = 36;
    pub const WEAK_SET: u32 = 37;
    pub const WEAK_TABLE: u32 = 38;
    pub const WEAK_MAPPING: u32 = 39;
    pub const EPHEMERON: u32 = 40;
    pub const BOX: u32 = 41;
    pub const FLUID: u32 = 42;
    pub const DYNAMIC_STATE: u32 = 43;
    pub const NATIVE_PROCEDURE: u32 = 44;
    pub const NATIVE_CONTINUATION: u32 = 45;
    pub const CODE_BLOCK: u32 = 46;
    pub const RELOCATABLE_CODE_BLOCK: u32 = 47;
    pub const MODULE: u32 = 48;
    pub const ENVIRONMENT: u32 = 49;
    pub const SYNTAX: u32 = 50;
    pub const SYNTAX_TRANSFORMER: u32 = 51;
    pub const PORT: u32 = 52;
    pub const SOCKET: u32 = 53;
    pub const POLLER: u32 = 54;
    pub const POINTER: u32 = 55;
    pub const CIF: u32 = 56;
    pub const THREAD: u32 = 57;
    pub const MUTEX: u32 = 58;
    pub const CONDITION: u32 = 59;
    pub const ANNOTATION: u32 = 60;
    pub const CONTINUATION_MARKS: u32 = 61;
    pub const UNINTERNED_SYMBOL: u32 = 62;
    pub const GENERIC: u32 = 63;
    pub const METHOD: u32 = 64;
    pub const NEXT_METHOD: u32 = 65;
    pub const SLOT_DEFINITION: u32 = 66;
    pub const SLOT_ACCESSOR: u32 = 67;

    pub const MAX: u32 = SLOT_ACCESSOR;
}

#[derive(Clone, Copy)]
pub struct AllocationHooks {
    pub trace: extern "C" fn(GCObject, &mut Visitor),
    pub weak_proc: extern "C" fn(GCObject, &mut WeakProcessor),
    pub instance_size: usize,
    pub compute_size: Option<extern "C" fn(GCObject) -> usize>,
    pub alignment: usize,
    pub compute_alignment: Option<extern "C" fn(GCObject) -> usize>,
    pub type_name: &'static str,
}

extern "C" fn default_trace<T: Trace>(obj: GCObject, vis: &mut Visitor) {
    vis.current_object = obj.to_objref();

    unsafe {
        obj.to_address().as_mut_ref::<T>().trace(vis);
    }
}

extern "C" fn default_weak_proc<T: Trace>(obj: GCObject, weak_processor: &mut WeakProcessor) {
    unsafe {
        obj.to_address()
            .as_mut_ref::<T>()
            .process_weak_refs(weak_processor);
    }
}

impl AllocationHooks {
    fn registry_key(self) -> AllocationHookKey {
        AllocationHookKey {
            trace: self.trace as usize,
            weak_proc: self.weak_proc as usize,
            instance_size: self.instance_size,
            compute_size: self.compute_size.map(|compute_size| compute_size as usize),
            alignment: self.alignment,
            compute_alignment: self
                .compute_alignment
                .map(|compute_alignment| compute_alignment as usize),
            type_name: self.type_name,
        }
    }
}

/// Allocation hooks for fixed-size traced Rust types.
///
/// Types with trailing data, such as arrays, must provide explicit hook records
/// with dynamic sizing rather than using this helper.
pub struct AllocationHooksOf<'gc, T: Trace>(PhantomData<&'gc T>);

impl<'gc, T: 'gc + Trace> AllocationHooksOf<'gc, T> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: std::any::type_name::<T>(),
        trace: default_trace::<T>,
        weak_proc: default_weak_proc::<T>,
        instance_size: size_of::<T>(),
        alignment: align_of::<T>(),
        compute_size: None,
        compute_alignment: None,
    };

    pub(crate) fn class_header_word(ctx: Context<'gc>) -> u64 {
        gc_only_class_header_word_with_context(ctx, Self::HOOKS)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct AllocationHookKey {
    trace: usize,
    weak_proc: usize,
    instance_size: usize,
    compute_size: Option<usize>,
    alignment: usize,
    compute_alignment: Option<usize>,
    type_name: &'static str,
}

type ClassIdBits = BitField<u64, u32, 0, 24, false>;
type HashBits = BitField<u64, u8, 57, 2, false>;
type FinalizationState = BitField<u64, bool, { HashBits::NEXT_BIT }, 1, false>;

type LastBitfield = FinalizationState;

static GC_ONLY_CLASS_REGISTRY: OnceLock<Mutex<GcOnlyClassRegistry>> = OnceLock::new();

fn gc_only_class_registry() -> &'static Mutex<GcOnlyClassRegistry> {
    GC_ONLY_CLASS_REGISTRY.get_or_init(|| Mutex::new(GcOnlyClassRegistry::new()))
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ClassId(NonZeroU32);

impl ClassId {
    pub const MAX_BITS: u32 = MAX_CLASS_ID;

    pub const fn new(id: u32) -> Option<Self> {
        if id == 0 || id > Self::MAX_BITS {
            return None;
        }

        match NonZeroU32::new(id) {
            Some(id) => Some(Self(id)),
            None => None,
        }
    }

    pub const fn bits(self) -> u32 {
        self.0.get()
    }
}

unsafe impl Trace for ClassId {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

pub struct GcOnlyClassInfo {
    class_id: ClassId,
    hooks: AllocationHooks,
}

impl GcOnlyClassInfo {
    pub fn class_id(&self) -> ClassId {
        self.class_id
    }

    pub fn hooks(&self) -> AllocationHooks {
        self.hooks
    }
}

struct GcOnlyClassRegistry {
    next_class_id: u32,
    by_hooks: HashMap<AllocationHookKey, &'static GcOnlyClassInfo>,
    by_class_id: HashMap<u32, &'static GcOnlyClassInfo>,
}

impl GcOnlyClassRegistry {
    fn new() -> Self {
        Self {
            next_class_id: MAX_CLASS_ID,
            by_hooks: HashMap::new(),
            by_class_id: HashMap::new(),
        }
    }

    fn get_or_register(&mut self, hooks: AllocationHooks) -> &'static GcOnlyClassInfo {
        let key = hooks.registry_key();
        if let Some(info) = self.by_hooks.get(&key) {
            return info;
        }

        let class_id = loop {
            assert!(
                self.next_class_id >= MIN_GC_ONLY_CLASS_ID,
                "GC-only class ID registry exhausted"
            );
            let Some(class_id) = ClassId::new(self.next_class_id) else {
                panic!("GC-only class ID registry exhausted");
            };
            self.next_class_id = self
                .next_class_id
                .checked_sub(1)
                .expect("GC-only class ID registry exhausted");

            if self.by_class_id.contains_key(&class_id.bits()) {
                continue;
            }

            break class_id;
        };

        let info = Box::leak(Box::new(GcOnlyClassInfo { class_id, hooks }));
        self.by_hooks.insert(key, info);
        self.by_class_id.insert(class_id.bits(), info);
        info
    }

    fn get_by_class_id(&self, id: ClassId) -> Option<&'static GcOnlyClassInfo> {
        self.by_class_id.get(&id.bits()).copied()
    }
}

pub(crate) fn gc_only_class_info(hooks: AllocationHooks) -> &'static GcOnlyClassInfo {
    gc_only_class_registry()
        .lock()
        .unwrap()
        .get_or_register(hooks)
}

pub(crate) fn gc_only_class_infos() -> Vec<&'static GcOnlyClassInfo> {
    gc_only_class_registry()
        .lock()
        .unwrap()
        .by_class_id
        .values()
        .copied()
        .collect()
}

thread_local! {
    static MIRRORING_GC_ONLY_CLASS: Cell<bool> = const { Cell::new(false) };
}

struct GcOnlyClassMirrorGuard;

impl GcOnlyClassMirrorGuard {
    fn enter() -> Option<Self> {
        MIRRORING_GC_ONLY_CLASS.with(|mirroring| {
            if mirroring.get() {
                return None;
            }

            mirroring.set(true);
            Some(Self)
        })
    }
}

impl Drop for GcOnlyClassMirrorGuard {
    fn drop(&mut self) {
        MIRRORING_GC_ONLY_CLASS.with(|mirroring| mirroring.set(false));
    }
}

pub(crate) fn gc_only_class_header_word_with_context<'gc>(
    ctx: Context<'gc>,
    hooks: AllocationHooks,
) -> u64 {
    let info = gc_only_class_info(hooks);
    if let Some(_guard) = GcOnlyClassMirrorGuard::enter() {
        crate::runtime::class::register_gc_only_class_if_initialized(ctx, info);
    }
    class_header_word(info.class_id())
}

pub(crate) fn gc_only_hooks_for_class_id(id: ClassId) -> Option<AllocationHooks> {
    gc_only_class_registry()
        .lock()
        .unwrap()
        .get_by_class_id(id)
        .map(GcOnlyClassInfo::hooks)
}

pub fn class_header_word(class_id: ClassId) -> u64 {
    class_id.bits() as u64
}

pub struct HeapObjectHeader {
    word: AtomicBitfieldContainer<u64>,
}

impl HeapObjectHeader {
    pub fn from_class_id(class_id: ClassId) -> Self {
        Self::from_word(class_header_word(class_id))
    }

    pub fn from_word(word: u64) -> Self {
        Self {
            word: AtomicBitfieldContainer::new(word),
        }
    }

    pub fn hash_state(&self) -> u8 {
        self.word.read::<HashBits>()
    }

    pub(crate) fn set_hash_state(&self, state: u8) {
        self.word.update::<HashBits>(state);
    }

    pub fn class_id(&self) -> ClassId {
        ClassId::new(self.word.read::<ClassIdBits>()).expect("heap object class ID must be nonzero")
    }

    pub(crate) fn set_class_id(&self, class_id: ClassId) {
        self.word.update::<ClassIdBits>(class_id.bits());
    }

    pub fn finalization_state(&self) -> bool {
        self.word.read::<FinalizationState>()
    }

    #[allow(dead_code)]
    pub(crate) fn set_finalization_state(&self, state: bool) {
        self.word.update::<FinalizationState>(state)
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct GCObject(Address);

impl fmt::Pointer for GCObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.0.to_ptr::<()>(), f)
    }
}

impl From<Address> for GCObject {
    fn from(value: Address) -> Self {
        Self(value)
    }
}

impl From<GCObject> for Address {
    fn from(val: GCObject) -> Self {
        val.0
    }
}

impl From<ObjectReference> for GCObject {
    fn from(value: ObjectReference) -> Self {
        Self(value.to_raw_address())
    }
}

impl GCObject {
    pub const NULL: Self = Self(Address::ZERO);

    pub fn is_null(self) -> bool {
        self.0.is_zero()
    }

    pub fn from_address(addr: Address) -> Self {
        Self(addr)
    }

    pub fn to_address(self) -> Address {
        self.0
    }

    pub fn from_objref_nullable(value: Option<ObjectReference>) -> Self {
        value.map_or(Self::NULL, Self::from)
    }

    pub fn to_objref(self) -> Option<ObjectReference> {
        ObjectReference::from_raw_address(self.0)
    }

    pub(crate) fn header<'a>(self) -> &'a HeapObjectHeader {
        unsafe { self.0.offset(-OBJECT_REF_OFFSET).as_ref() }
    }

    pub fn class_id(self) -> ClassId {
        self.header().class_id()
    }

    pub fn header_address(self) -> Address {
        self.0.offset(-OBJECT_REF_OFFSET)
    }

    pub fn trace(self, visitor: &mut Visitor) {
        if let Some(hooks) =
            crate::runtime::class::primitive_layout_hooks_for_class_id(self.class_id())
        {
            (hooks.trace())(self, visitor);
            return;
        }

        panic!(
            "missing primitive GC hooks for class id {} while tracing",
            self.class_id().bits()
        );
    }

    pub fn process_weak_refs(self, weak_processor: &mut WeakProcessor) {
        if let Some(hooks) =
            crate::runtime::class::primitive_layout_hooks_for_class_id(self.class_id())
        {
            (hooks.weak_proc())(self, weak_processor);
            return;
        }

        panic!(
            "missing primitive GC hooks for class id {} while processing weak refs",
            self.class_id().bits()
        );
    }

    pub fn alignment(self) -> usize {
        if let Some(hooks) =
            crate::runtime::class::primitive_layout_hooks_for_class_id(self.class_id())
        {
            return (hooks.alignment() + hooks.compute_alignment().map_or(0, |f| f(self))).min(16);
        }

        panic!(
            "missing primitive GC hooks for class id {} while computing alignment",
            self.class_id().bits()
        );
    }

    pub fn current_size(self) -> usize {
        self.bytes_used()
    }

    fn get_size_when_copied(self) -> usize {
        self.bytes_required_when_copied()
    }

    fn get_align_when_copied(self) -> usize {
        self.alignment()
    }

    fn get_align_offset_when_copied(self) -> usize {
        self.get_offset_for_alignment()
    }

    pub fn instance_size(self) -> usize {
        if let Some(hooks) =
            crate::runtime::class::primitive_layout_hooks_for_class_id(self.class_id())
        {
            return hooks.instance_size()
                + hooks.compute_size().map_or(0, |f| f(self))
                + size_of::<HeapObjectHeader>();
        }

        panic!(
            "missing primitive GC hooks for class id {} while computing instance size",
            self.class_id().bits()
        );
    }

    fn bytes_used(self) -> usize {
        self.instance_size() + self.hashcode_overhead::<false>()
    }

    fn bytes_required_when_copied(self) -> usize {
        self.instance_size() + self.hashcode_overhead::<true>()
    }

    pub fn hashcode_overhead<const WHEN_COPIED: bool>(self) -> usize {
        let hash_state = self.header().hash_state();

        let has_hashcode = if WHEN_COPIED {
            hash_state != HASH_UNHASHED
        } else {
            hash_state == HASH_HASHED_AND_MOVED
        };

        if has_hashcode { size_of::<u64>() } else { 0 }
    }

    pub fn object_start(self) -> Address {
        self.0
            .offset(-(OBJECT_REF_OFFSET + self.hashcode_overhead::<false>() as isize))
    }

    pub fn get_offset_for_alignment(self) -> usize {
        size_of::<HeapObjectHeader>() + self.hashcode_overhead::<true>()
    }

    pub fn hashcode(&self) -> u64 {
        let hash_status = self.header().hash_state();

        let hashcode = self.0.as_usize() as u64 >> LOG_BYTES_IN_ADDRESS;
        match hash_status {
            _x @ HASH_UNHASHED => {
                self.header().set_hash_state(HASH_HASHED);
                hashcode
            }

            _x @ HASH_HASHED => hashcode,

            _x @ HASH_HASHED_AND_MOVED => {
                let word = self.0 + HASHCODE_OFFSET;
                unsafe { word.load::<u64>() }
            }

            _ => unreachable!(),
        }
    }
}

enum MoveTarget {
    ToAddress(Address),
    ToObject(GCObject),
}

#[derive(Default)]
pub struct ObjectModel;

impl ObjectModel {
    fn move_object(from_obj: GCObject, mut to: MoveTarget, num_bytes: usize) -> GCObject {
        let mut copy_bytes = num_bytes;
        let mut obj_ref_offset = OBJECT_REF_OFFSET;
        let header = from_obj.header();

        let hash_state = header.hash_state();

        if hash_state == HASH_HASHED {
            copy_bytes -= HASHCODE_BYTES;

            if let MoveTarget::ToAddress(ref mut addr) = to {
                *addr += HASHCODE_BYTES;
            }
        } else if hash_state == HASH_HASHED_AND_MOVED {
            obj_ref_offset += HASHCODE_BYTES as isize;
        }

        let (to_address, to_obj) = match to {
            MoveTarget::ToAddress(addr) => {
                let obj = GCObject(addr + obj_ref_offset);
                (addr, obj)
            }

            MoveTarget::ToObject(obj) => {
                let addr = obj.to_address() + (-obj_ref_offset);
                debug_assert!(obj.to_address() == addr + obj_ref_offset);
                (addr, obj)
            }
        };

        let from_address = from_obj.to_address() + (-obj_ref_offset);

        /*if let MoveTarget::ToAddress(ref mut addr) = to {
            *addr += HASHCODE_BYTES;
        }*/

        unsafe {
            std::ptr::copy(
                from_address.to_ptr::<u8>(),
                to_address.to_mut_ptr::<u8>(),
                copy_bytes,
            );
        }

        if hash_state == HASH_HASHED {
            unsafe {
                let hash_code = from_obj.to_address().as_usize() >> LOG_BYTES_IN_ADDRESS;

                (to_obj.to_address() + HASHCODE_OFFSET).store::<usize>(hash_code);
            }

            to_obj.header().set_hash_state(HASH_HASHED_AND_MOVED);
        }

        debug_assert_eq!(to_obj.header().class_id(), from_obj.header().class_id());

        to_obj
    }
}

impl mmtk::vm::ObjectModel<MemoryManager> for ObjectModel {
    const VM_WORST_CASE_COPY_EXPANSION: f64 = 1.3;
    const GLOBAL_LOG_BIT_SPEC: mmtk::vm::VMGlobalLogBitSpec =
        mmtk::vm::VMGlobalLogBitSpec::side_first();
    const LOCAL_MARK_BIT_SPEC: mmtk::vm::VMLocalMarkBitSpec =
        mmtk::vm::VMLocalMarkBitSpec::side_first();
    const LOCAL_FORWARDING_BITS_SPEC: mmtk::vm::VMLocalForwardingBitsSpec =
        mmtk::vm::VMLocalForwardingBitsSpec::in_header(LastBitfield::NEXT_BIT as _);
    const LOCAL_FORWARDING_POINTER_SPEC: mmtk::vm::VMLocalForwardingPointerSpec =
        mmtk::vm::VMLocalForwardingPointerSpec::in_header(0);
    const LOCAL_LOS_MARK_NURSERY_SPEC: mmtk::vm::VMLocalLOSMarkNurserySpec =
        mmtk::vm::VMLocalLOSMarkNurserySpec::in_header(
            LastBitfield::NEXT_BIT as isize + Self::LOCAL_FORWARDING_BITS_SPEC.num_bits() as isize,
        );
    /// Required for tracing continuation objects.
    const NEED_VO_BITS_DURING_TRACING: bool = true;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET;
    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = false;

    fn dump_object(_object: mmtk::util::ObjectReference) {
        todo!()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        &[]
    }

    fn ref_to_header(object: mmtk::util::ObjectReference) -> Address {
        GCObject::from(object).header_address()
    }

    fn ref_to_object_start(object: mmtk::util::ObjectReference) -> Address {
        GCObject::from(object).object_start()
    }

    fn copy(
        from: ObjectReference,
        semantics: mmtk::util::copy::CopySemantics,
        copy_context: &mut mmtk::util::copy::GCWorkerCopyContext<MemoryManager>,
    ) -> ObjectReference {
        let gc_from = GCObject::from(from);

        let bytes = gc_from.bytes_required_when_copied();
        let align = gc_from.alignment();
        let offset = gc_from.get_offset_for_alignment();

        let bytes = raw_align_up(bytes, align);
        let addr = copy_context.alloc_copy(from, bytes, align, offset, semantics);
        debug_assert!(!addr.is_zero());

        let to_obj = Self::move_object(gc_from, MoveTarget::ToAddress(addr), bytes);

        let to_obj = ObjectReference::try_from(to_obj).unwrap();

        copy_context.post_copy(to_obj, bytes, semantics);

        to_obj
    }

    fn copy_to(from: ObjectReference, to: ObjectReference, _region: Address) -> Address {
        let gc_from = GCObject::from(from);
        let copy = from != to;

        let bytes = if copy {
            let gc_to = GCObject::from(to);
            let bytes = gc_from.bytes_required_when_copied();
            Self::move_object(gc_from, MoveTarget::ToObject(gc_to), bytes);
            bytes
        } else {
            gc_from.bytes_used()
        };

        let start = Self::ref_to_object_start(to);

        start + bytes
    }

    fn get_reference_when_copied_to(from: ObjectReference, to: Address) -> ObjectReference {
        let gc_from = GCObject::from(from);
        let res_addr = to + OBJECT_REF_OFFSET + gc_from.hashcode_overhead::<true>();
        let res = GCObject(res_addr);

        res.try_into().unwrap()
    }

    fn get_current_size(object: ObjectReference) -> usize {
        GCObject::from(object).current_size()
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        GCObject::from(object).get_size_when_copied()
    }

    fn get_align_offset_when_copied(object: ObjectReference) -> usize {
        GCObject::from(object).get_align_offset_when_copied()
    }

    fn get_align_when_copied(object: ObjectReference) -> usize {
        GCObject::from(object).get_align_when_copied()
    }
}

impl TryFrom<GCObject> for ObjectReference {
    type Error = ();
    fn try_from(value: GCObject) -> Result<Self, Self::Error> {
        if value.0.is_zero() {
            Err(())
        } else {
            unsafe { Ok(ObjectReference::from_raw_address_unchecked(value.0)) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Dummy;

    unsafe impl Trace for Dummy {
        unsafe fn trace(&mut self, _visitor: &mut Visitor) {}

        unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
    }

    #[test]
    fn heap_header_layout_stays_single_word() {
        assert_eq!(size_of::<HeapObjectHeader>(), 8);
        assert_eq!(OBJECT_REF_OFFSET, 8);
    }

    #[test]
    fn heap_header_roundtrips_class_id() {
        let class_id = ClassId::new(builtin_class_ids::CLASS).unwrap();
        let header = HeapObjectHeader::from_class_id(class_id);

        assert_eq!(header.class_id(), class_id);
        assert_eq!(header.hash_state(), HASH_UNHASHED);
        assert!(!header.finalization_state());
    }

    #[test]
    fn class_header_word_contains_only_the_class_id() {
        let class_id = ClassId::new(0x90ab).unwrap();
        let header_word = class_header_word(class_id);
        let header = HeapObjectHeader::from_word(header_word);

        assert_eq!(header_word, u64::from(class_id.bits()));
        assert_eq!(header.class_id(), class_id);
    }

    #[test]
    fn class_id_is_nonzero_and_24_bit() {
        assert!(ClassId::new(0).is_none());
        assert_eq!(ClassId::new(1).unwrap().bits(), 1);
        assert_eq!(ClassId::new(MAX_CLASS_ID).unwrap().bits(), MAX_CLASS_ID);
        assert!(ClassId::new(MAX_CLASS_ID + 1).is_none());
        assert_eq!(ClassIdBits::NEXT_BIT, 24);
    }

    #[test]
    fn mmtk_side_bits_do_not_overlap_class_id_field() {
        assert!(ClassIdBits::NEXT_BIT <= HashBits::shift());
        assert!(ClassIdBits::NEXT_BIT <= FinalizationState::shift());
        const {
            assert!(ClassIdBits::NEXT_BIT <= LastBitfield::NEXT_BIT);
        }
    }

    #[test]
    fn object_copy_preserves_class_id() {
        let bytes = size_of::<HeapObjectHeader>() + size_of::<u64>();
        let mut from = [0_u64; 2];
        let mut to = [0_u64; 2];
        let class_id = ClassId::new(builtin_class_ids::CLASS).unwrap();

        unsafe {
            from.as_mut_ptr()
                .cast::<HeapObjectHeader>()
                .write(HeapObjectHeader::from_class_id(class_id));
            from.as_mut_ptr()
                .cast::<u8>()
                .add(size_of::<HeapObjectHeader>())
                .cast::<u64>()
                .write(0xfeed_face_cafe_beef);
        }

        let from_obj = GCObject::from_address(Address::from_ptr(unsafe {
            from.as_ptr().cast::<u8>().add(OBJECT_REF_OFFSET as usize)
        }));
        let to_start = Address::from_ptr(to.as_mut_ptr());
        let to_obj = ObjectModel::move_object(from_obj, MoveTarget::ToAddress(to_start), bytes);

        assert_eq!(to_obj.header().class_id(), class_id);
    }

    #[test]
    fn gc_only_class_info_publishes_class_only_header_and_hooks() {
        let hooks = AllocationHooksOf::<'static, Dummy>::HOOKS;
        let info = gc_only_class_info(hooks);
        let same = gc_only_class_info(hooks);
        let header = HeapObjectHeader::from_word(class_header_word(info.class_id()));
        let registered_hooks = gc_only_hooks_for_class_id(info.class_id()).unwrap();

        assert!(std::ptr::eq(info, same));
        assert_eq!(header.class_id(), info.class_id());
        assert_eq!(
            registered_hooks.type_name,
            AllocationHooksOf::<'static, Dummy>::HOOKS.type_name
        );
        assert_eq!(registered_hooks.instance_size, size_of::<Dummy>());
        assert_eq!(registered_hooks.alignment, align_of::<Dummy>());
        assert!(gc_only_hooks_for_class_id(info.class_id()).is_some());
        assert!(info.class_id().bits() >= MIN_GC_ONLY_CLASS_ID);
    }
}
