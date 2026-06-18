#![allow(dead_code, unused_variables)]

//! Vector, bytevector, and tuple heap objects.

use crate::rsgc::{
    Gc, Mutation, WeakProcessor,
    barrier::{AsRefWrite, IndexWrite},
    cell::Lock,
    collection::Visitor,
    mmtk::AllocationSemantics,
    object::{
        AllocationHooks, ClassId, builtin_class_ids, class_header_word,
        class_header_word_with_private_variant_flag,
    },
};
use std::ops::{Deref, DerefMut, Index};
use std::{mem::offset_of, ops::IndexMut};

use crate::runtime::{Context, value::*};

#[doc(hidden)]
pub fn vector_value<'gc>(mc: Context<'gc>, value: impl IntoValue<'gc>) -> Value<'gc> {
    value.into_value(mc)
}

#[repr(C, align(8))]
/// A fixed-length Scheme vector of values.
pub struct Vector<'gc> {
    pub(crate) length: usize,
    pub(crate) data: [Lock<Value<'gc>>; 0],
}

const _: () = {
    assert!(offset_of!(Vector<'static>, length) == 0);
};

fn vector_header_word(immutable: bool) -> u64 {
    let class_id = ClassId::new(builtin_class_ids::VECTOR).unwrap();

    if immutable {
        class_header_word_with_private_variant_flag(class_id)
    } else {
        class_header_word(class_id)
    }
}

#[inline(never)]
extern "C" fn trace_vector(vec: GCObject, vis: &mut Visitor) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let vec = vec.to_address().as_mut_ref::<Vector<'static>>();
        for i in 0..vec.len() {
            vec[i].trace(vis);
        }
    }
}

extern "C" fn process_weak_vector(_: GCObject, _: &mut WeakProcessor) {}

#[inline(never)]
extern "C" fn compute_vector_size(vec: GCObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe { vec.to_address().as_ref::<Vector<'static>>().len() * size_of::<Value>() }
}

impl<'gc> Vector<'gc> {
    pub const OFFSET_OF_DATA: usize = offset_of!(Vector, data);

    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "Vector",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(compute_vector_size),
        trace: trace_vector,
        weak_proc: process_weak_vector,
    };

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_immutable(&self) -> bool {
        heap_header(self).private_variant_flag()
    }

    /// Allocates a vector with all slots initialized to `fill`.
    pub fn new<const IMMUTABLE: bool>(
        mc: Mutation<'gc>,
        length: usize,
        fill: Value<'gc>,
    ) -> Gc<'gc, Self> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let alloc = mc.raw_allocate_with_header_word(
                size_of::<Self>() + size_of::<Value>() * length,
                align_of::<Self>(),
                vector_header_word(IMMUTABLE),
                AllocationSemantics::Default,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.length = length;
            for i in 0..length {
                vec.data.as_mut_ptr().add(i).write(Lock::new(fill));
            }

            Gc::from_gcobj(alloc)
        }
    }

    /// Allocates a mutable vector initialized from a slice.
    pub fn from_slice(mc: Mutation<'gc>, slice: &[Value<'gc>]) -> Gc<'gc, Self> {
        let length = slice.len();
        let vector = Self::new::<false>(mc, length, Value::undefined());

        Self::copy_from(vector, slice, mc);
        vector
    }
    pub fn as_slice(&self) -> &[Value<'gc>] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }

    /// Return the vector contents as a mutable slice without alias checks.
    ///
    /// # Safety
    ///
    /// The caller must ensure exclusive access to the vector contents for the
    /// duration of the returned borrow.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn as_slice_mut_unchecked(&mut self) -> &mut [Value<'gc>] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr().cast(), self.len()) }
    }

    pub fn fill(this: Gc<'gc, Self>, fill: Value<'gc>, mc: Mutation<'gc>) {
        let vec = Gc::write(mc, this);

        for i in 0..vec.len() {
            vec[i].unlock().set(fill);
        }
    }

    pub fn copy_from(self: Gc<'gc, Self>, other: impl AsRef<[Value<'gc>]>, mc: Mutation<'gc>) {
        let vec = Gc::write(mc, self);
        let other_slice = other.as_ref();

        assert!(
            other_slice.len() <= vec.len(),
            "Cannot copy from vector of different length"
        );

        for (i, &value) in other_slice.iter().enumerate() {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe {
                (vec.data.as_ptr() as *mut Value).add(i).write(value);
            }
        }
    }

    pub fn to_list(self: Gc<'gc, Self>, mc: Context<'gc>) -> Value<'gc> {
        let mut list = Value::null();
        for i in (0..self.len()).rev() {
            list = Value::cons(mc, self[i].get(), list);
        }
        list
    }
}

impl<'gc> AsRef<[Value<'gc>]> for Vector<'gc> {
    fn as_ref(&self) -> &[Value<'gc>] {
        self.as_slice()
    }
}

// SAFETY: `gc` for `Vector` upholds all trait invariants
unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Vector<'gc> {}

impl<'gc> Deref for Vector<'gc> {
    type Target = [Lock<Value<'gc>>];

    fn deref(&self) -> &Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }
}

// SAFETY: `gc` for `Vector` upholds all trait invariants
unsafe impl<'gc> IndexWrite<usize> for Vector<'gc> {}
impl<'gc> Index<usize> for Vector<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        // SAFETY: Pointer is valid for the given element count
        unsafe { &std::slice::from_raw_parts(self.data.as_ptr(), self.len())[index] }
    }
}

impl<'gc> IndexMut<usize> for Vector<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        // SAFETY: Pointer is valid for the given element count
        unsafe { &mut std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len())[index] }
    }
}

#[macro_export]
macro_rules! vector {
    ($mc: expr, $($value: expr),*) => {{
        let slice = &[$({
            let value = $value;
            $crate::runtime::value::vector::vector_value($mc, value)
        }),*];
        let length = slice.len();
        let vector = $crate::runtime::value::Vector::new::<false>(*$mc, length, $crate::runtime::value::Value::unspecified());
        vector.copy_from(slice, *$mc);
        vector
    }};

    ($mc: expr, $value: expr; $count: expr) => {{
        let mc = $mc;
        let value = $value;
        let count = $count;
        let vector = $crate::runtime::value::Vector::new(
            &$mc,
            count,
            $crate::runtime::value::vector::vector_value($mc, value)
        );
        vector
    }};

    ($mc: expr) => {
        $crate::runtime::value::Vector::new(&$mc, 0, Value::null())
    };
}
#[repr(C, align(8))]
/// A byte-addressable vector backed by owned or mapped memory.
pub struct ByteVector {
    pub(crate) len: usize,
    pub(crate) contents: Address,
}

fn bytevector_header_word(immutable: bool) -> u64 {
    let class_id = if immutable {
        builtin_class_ids::IMMUTABLE_BYTEVECTOR
    } else {
        builtin_class_ids::MUTABLE_BYTEVECTOR
    };

    class_header_word(ClassId::new(class_id).unwrap())
}

fn mapped_bytevector_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::MAPPED_BYTEVECTOR).unwrap())
}

pub const BYTE_VECTOR_MAX_LENGTH: usize = usize::MAX;

extern "C" fn trace_byte_vector_mapping(vec: GCObject, vis: &mut Visitor) {
    // SAFETY: The pointer references a valid GC-managed object of the expected type
    unsafe {
        let bv = vec.to_address().as_mut_ref::<ByteVector>();
        let orig = bv.contents.sub(size_of::<ByteVector>());
        let mut orig_bv = Gc::from_ptr(orig.to_ptr::<ByteVector>());
        orig_bv.trace(vis);
        bv.contents = Address::from_ptr(orig_bv.as_ptr().add(1));
    }
}

extern "C" fn process_weak_byte_vector(_: GCObject, _: &mut WeakProcessor) {}

extern "C" fn trace_owned_byte_vector(vec: GCObject, _: &mut Visitor) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let bv = vec.to_address().as_mut_ref::<ByteVector>();
        bv.contents = vec.to_address().add(size_of::<ByteVector>());
    }
}

extern "C" fn compute_owned_byte_vector_size(vec: GCObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe { vec.to_address().as_ref::<ByteVector>().len * size_of::<u8>() }
}

extern "C" fn compute_mapped_byte_vector_size(_: GCObject) -> usize {
    0
}

impl ByteVector {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "ByteVector",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(compute_owned_byte_vector_size),
        trace: trace_owned_byte_vector,
        weak_proc: process_weak_byte_vector,
    };

    pub const MAPPING_HOOKS: AllocationHooks = AllocationHooks {
        type_name: "MappedByteVector",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(compute_mapped_byte_vector_size),
        trace: trace_byte_vector_mapping,
        weak_proc: process_weak_byte_vector,
    };

    pub fn contents(&self) -> Address {
        self.contents
    }

    pub fn is_mapping(&self) -> bool {
        payload_class_id(self).bits() == builtin_class_ids::MAPPED_BYTEVECTOR
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_immutable(&self) -> bool {
        payload_class_id(self).bits() == builtin_class_ids::IMMUTABLE_BYTEVECTOR
    }

    /// Allocates an owned bytevector.
    pub fn new<'gc, const IMMUTABLE: bool>(
        mc: Mutation<'gc>,
        length: usize,
        movable: bool,
    ) -> Gc<'gc, Self> {
        let semantics = if movable {
            AllocationSemantics::Default
        } else {
            AllocationSemantics::NonMoving
        };

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let alloc = mc.raw_allocate_with_header_word(
                size_of::<Self>() + size_of::<u8>() * length,
                align_of::<Self>(),
                bytevector_header_word(IMMUTABLE),
                semantics,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.len = length;
            let contents = (vec as *mut Self).add(1);
            vec.contents = Address::from_ptr(contents as *mut u8);

            Gc::from_gcobj(alloc)
        }
    }

    /// Allocates a memory-mapped bytevector. This can be used to represent
    /// memory from FFI calls or memory-mapped files.
    pub fn new_mapping<'gc>(mc: Mutation<'gc>, addr: Address, length: usize) -> Gc<'gc, Self> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let alloc = mc.raw_allocate_with_header_word(
                size_of::<Self>(),
                align_of::<Self>(),
                mapped_bytevector_header_word(),
                AllocationSemantics::Default,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.len = length;
            vec.contents = addr;

            Gc::from_gcobj(alloc)
        }
    }

    /// Allocates a mutable bytevector initialized from a byte slice.
    #[inline(never)]
    pub fn from_slice<'gc>(mc: Mutation<'gc>, slice: &[u8], movable: bool) -> Gc<'gc, Self> {
        let length = slice.len();
        let byte_vector = Self::new::<false>(mc, length, movable);
        byte_vector.copy_from(slice);

        byte_vector
    }

    pub fn as_slice<'gc>(self: Gc<'gc, Self>) -> &'gc [u8] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }

    /// Return the bytevector contents as a mutable slice without alias checks.
    ///
    /// # Safety
    ///
    /// The caller must ensure exclusive access to the bytevector contents for
    /// the duration of the returned borrow.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn as_slice_mut_unchecked<'gc>(self: Gc<'gc, Self>) -> &'gc mut [u8] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.contents.to_mut_ptr(), self.len) }
    }

    pub fn fill<'gc>(self: Gc<'gc, Self>, fill: u8) {
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = self.as_slice_mut_unchecked();
            for byte in slice.iter_mut() {
                *byte = fill;
            }
        }
    }

    #[inline(never)]
    pub fn copy_from<'gc>(self: Gc<'gc, Self>, other: impl AsRef<[u8]>) {
        let other_slice = other.as_ref();
        assert!(
            other_slice.len() <= self.len,
            "Cannot copy from byte vector of different length"
        );

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            self.as_slice_mut_unchecked()[..other_slice.len()].copy_from_slice(other_slice);
        }
    }
}

impl AsRef<[u8]> for ByteVector {
    fn as_ref(&self) -> &[u8] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }
}

impl Deref for ByteVector {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }
}

impl Index<usize> for ByteVector {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::Range<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::Range<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::RangeFrom<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeFrom<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::RangeInclusive<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeInclusive<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::RangeTo<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeTo<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::RangeToInclusive<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeToInclusive<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl Index<core::ops::RangeFull> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeFull) -> &Self::Output {
        &(**self)[index]
    }
}

// SAFETY: `gc` for `Vector` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Vector<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::VECTOR];
    const TYPE_NAME: &'static str = "vector";
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `ByteVector`
unsafe impl ClassTagged for ByteVector {
    const CLASS_IDS: &'static [u32] = &[
        crate::rsgc::object::builtin_class_ids::MUTABLE_BYTEVECTOR,
        crate::rsgc::object::builtin_class_ids::IMMUTABLE_BYTEVECTOR,
        crate::rsgc::object::builtin_class_ids::MAPPED_BYTEVECTOR,
    ];
    const TYPE_NAME: &'static str = "bytevector";
}

#[repr(C, align(8))]
/// A fixed-length internal tuple of values.
pub struct Tuple<'gc> {
    pub(crate) length: usize,
    pub(crate) data: [Lock<Value<'gc>>; 0],
}

const _: () = {
    assert!(offset_of!(Tuple<'static>, length) == 0);
};

fn tuple_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::TUPLE).unwrap())
}

extern "C" fn trace_tuple(tuple: GCObject, vis: &mut Visitor) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let tuple = tuple.to_address().as_mut_ref::<Tuple<'static>>();

        for i in 0..tuple.len() {
            tuple[i].trace(vis);
        }
    }
}

extern "C" fn process_weak_tuple(_: GCObject, _: &mut WeakProcessor) {}

extern "C" fn compute_tuple_size(tuple: GCObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe { tuple.to_address().as_ref::<Tuple<'static>>().len() * size_of::<Value>() }
}

// SAFETY: `gc` for `Tuple` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Tuple<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::TUPLE];
    const TYPE_NAME: &'static str = "tuple";
}

impl<'gc> Tuple<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "Tuple",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(compute_tuple_size),
        trace: trace_tuple,
        weak_proc: process_weak_tuple,
    };

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Allocates a tuple with all slots initialized to `init`.
    pub fn new(mc: Mutation<'gc>, length: usize, init: Value<'gc>) -> Gc<'gc, Self> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let alloc = mc.raw_allocate_with_header_word(
                size_of::<Self>() + size_of::<Value>() * length,
                align_of::<Self>(),
                tuple_header_word(),
                AllocationSemantics::Default,
            );

            let tuple = alloc.to_address().as_mut_ref::<Self>();
            tuple.length = length;

            for i in 0..length {
                tuple.data.as_mut_ptr().add(i).write(Lock::new(init));
            }

            Gc::from_gcobj(alloc)
        }
    }

    /// Allocates a tuple initialized from a slice.
    pub fn from_slice(mc: Mutation<'gc>, slice: &[Value<'gc>]) -> Gc<'gc, Self> {
        let length = slice.len();
        let tuple = Self::new(mc, length, Value::new(false));
        Self::copy_from(tuple, slice, mc);
        tuple
    }

    pub fn as_slice(&self) -> &[Value<'gc>] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }

    /// Return the tuple contents as a mutable slice without alias checks.
    ///
    /// # Safety
    ///
    /// The caller must ensure exclusive access to the tuple contents for the
    /// duration of the returned borrow.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn as_slice_mut_unchecked(&mut self) -> &mut [Value<'gc>] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr().cast(), self.len()) }
    }

    pub fn fill(this: Gc<'gc, Self>, fill: Value<'gc>, mc: Mutation<'gc>) {
        let tuple = Gc::write(mc, this);

        for i in 0..tuple.len() {
            tuple[i].unlock().set(fill);
        }
    }

    pub fn copy_from(this: Gc<'gc, Self>, other: impl AsRef<[Value<'gc>]>, mc: Mutation<'gc>) {
        let tuple = Gc::write(mc, this);
        let other_slice = other.as_ref();

        assert!(
            other_slice.len() <= tuple.len(),
            "Cannot copy from tuple of different length"
        );

        for (i, value) in other_slice.iter().enumerate() {
            tuple[i].unlock().set(*value);
        }
    }
}

impl<'gc> AsRef<[Value<'gc>]> for Tuple<'gc> {
    fn as_ref(&self) -> &[Value<'gc>] {
        self.as_slice()
    }
}

impl<'gc> Deref for Tuple<'gc> {
    type Target = [Lock<Value<'gc>>];

    fn deref(&self) -> &Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

impl<'gc> DerefMut for Tuple<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len()) }
    }
}

impl<'gc> IndexMut<usize> for Tuple<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { &mut *self.data.as_mut_ptr().add(index) }
    }
}

impl<'gc> Index<usize> for Tuple<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

// SAFETY: `gc` for `Tuple` upholds all trait invariants
unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Tuple<'gc> {}
// SAFETY: `gc` for `Tuple` upholds all trait invariants
unsafe impl<'gc> IndexWrite<usize> for Tuple<'gc> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;

    #[test]
    fn bytevector_mapping_uses_class_id() {
        Scheme::new_uninit().enter(|ctx| {
            let owned = ByteVector::new::<false>(*ctx, 0, false);
            let mapped = ByteVector::new_mapping(*ctx, Address::ZERO, 0);

            assert!(!owned.is_mapping());
            assert!(mapped.is_mapping());
            assert_eq!(
                payload_class_id(&*mapped).bits(),
                builtin_class_ids::MAPPED_BYTEVECTOR
            );
        });
    }
}
