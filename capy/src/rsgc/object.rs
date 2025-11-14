use crate::rsgc::{collection::Visitor, mm::MemoryManager, traits::Trace, weak::WeakProcessor};
use crate::utils::easy_bitfield::{AtomicBitfieldContainer, BitField, BitFieldTrait};
use core::fmt;
use mmtk::util::{
    Address, ObjectReference, constants::LOG_BYTES_IN_ADDRESS, conversions::raw_align_up,
};
use std::marker::PhantomData;

/// Offset from allocation to the actual object
pub const OBJECT_REF_OFFSET: isize = 8;
pub const OBJECT_HEADER_OFFSET: isize = -OBJECT_REF_OFFSET;
pub const HASHCODE_OFFSET: isize = -(OBJECT_REF_OFFSET + size_of::<u64>() as isize);
pub const HASHCODE_BYTES: usize = size_of::<u64>();

const HASH_UNHASHED: u8 = 0;
const HASH_HASHED: u8 = 1;
const HASH_HASHED_AND_MOVED: u8 = 2;

#[derive()]
#[repr(C)]
pub struct VTable {
    pub trace: extern "C" fn(GCObject, &mut Visitor),
    pub weak_proc: extern "C" fn(GCObject, &mut WeakProcessor),
    pub instance_size: usize,
    pub compute_size: Option<extern "C" fn(GCObject) -> usize>,
    pub alignment: usize,
    pub compute_alignment: Option<extern "C" fn(GCObject) -> usize>,
    pub type_name: &'static str,
}

#[cfg(target_pointer_width = "64")]
const VTABLE_BITS: usize = 57;
#[cfg(target_pointer_width = "32")]
const VTABLE_BITS: usize = 32;

type VTableBits = BitField<u64, usize, 0, VTABLE_BITS, false>;
type HashBits = BitField<u64, u8, { VTableBits::NEXT_BIT }, 2, false>;
type FinalizationState = BitField<u64, bool, { HashBits::NEXT_BIT }, 1, false>;

type LastBitfield = FinalizationState;

pub struct HeapObjectHeader {
    word: AtomicBitfieldContainer<u64>,
}

impl HeapObjectHeader {
    pub fn new(vt: &'static VTable) -> Self {
        Self {
            word: AtomicBitfieldContainer::new(VTableBits::encode(vt as *const VTable as usize)),
        }
    }

    pub fn hash_state(&self) -> u8 {
        self.word.read::<HashBits>()
    }

    pub(crate) fn set_hash_state(&self, state: u8) {
        self.word.update::<HashBits>(state);
    }

    pub fn vtable(&self) -> &'static VTable {
        unsafe {
            let bits = self.word.read::<VTableBits>();
            std::ptr::with_exposed_provenance::<VTable>(bits)
                .as_ref()
                .unwrap_or_else(|| {
                    panic!(
                        "Invalid vtable pointer: {bits:#x} in object header {:p} for object {:p}",
                        self,
                        (self as *const Self as isize + OBJECT_REF_OFFSET) as *const ()
                    )
                })
        }
    }

    pub fn vtable_bits(&self) -> usize {
        self.word.read::<VTableBits>()
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

    pub fn header_address(self) -> Address {
        self.0.offset(-OBJECT_REF_OFFSET)
    }

    pub fn alignment(self) -> usize {
        let header = self.header();
        let vt = header.vtable();

        (vt.alignment + vt.compute_alignment.map_or(0, |f| f(self))).min(16)
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
        let header = self.header();
        let vt = header.vtable();

        vt.instance_size + vt.compute_size.map_or(0, |f| f(self)) + size_of::<HeapObjectHeader>()
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

        debug_assert!(to_obj.header().vtable_bits() != 0);
        debug_assert_eq!(
            to_obj.header().vtable_bits(),
            from_obj.header().vtable_bits()
        );

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

/// A marker type to get VTable for specific `T`.
///
/// ## Notes
///
/// Note that using `VTableOf::<T>::VT` is unsafe in case `T` is a datatype with trailing
/// data e.g array. It should instead provide its own vtable and constructor functions rather
/// than using [`Gc::new`](crate::ptr::Gc::new)
pub struct VTableOf<'gc, T: Trace>(PhantomData<&'gc T>);

impl<'gc, T: 'gc + Trace> VTableOf<'gc, T> {
    /// A VTable for specific type `T` constructed based on the `Trace` impl and statically known size of `T`.
    pub const VT: &'static VTable = &VTable {
        type_name: std::any::type_name::<T>(),
        trace: {
            extern "C" fn trace<T: Trace>(obj: GCObject, vis: &mut Visitor) {
                vis.current_object = obj.to_objref();

                unsafe {
                    obj.to_address().as_mut_ref::<T>().trace(vis);
                }
            }

            trace::<T>
        },

        weak_proc: {
            extern "C" fn weak<T: Trace>(obj: GCObject, vis: &mut WeakProcessor) {
                unsafe {
                    obj.to_address().as_mut_ref::<T>().process_weak_refs(vis);
                }
            }

            weak::<T>
        },
        instance_size: size_of::<T>(),
        alignment: align_of::<T>(),
        compute_size: None,
        compute_alignment: None,
    };
}
