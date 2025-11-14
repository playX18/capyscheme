#![allow(dead_code, unused_variables)]

use crate::rsgc::{
    Gc, Mutation, WeakProcessor,
    barrier::{AsRefWrite, IndexWrite},
    cell::Lock,
    collection::Visitor,
    mmtk::AllocationSemantics,
    object::VTable,
};
use easy_bitfield::{BitField, BitFieldTrait};
use std::ops::{Deref, Index};
use std::{mem::offset_of, ops::IndexMut};

use crate::runtime::{Context, value::*};

#[repr(C, align(8))]
pub struct Vector<'gc> {
    pub(crate) hdr: ScmHeader,
    pub(crate) length: usize,
    pub(crate) data: [Lock<Value<'gc>>; 0],
}

#[inline(never)]
extern "C" fn trace_vector(vec: GCObject, vis: &mut Visitor) {
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
    unsafe { vec.to_address().as_ref::<Vector<'static>>().len() * size_of::<Value>() }
}

impl<'gc> Vector<'gc> {
    pub const OFFSET_OF_DATA: usize = offset_of!(Vector, data);

    pub const VT: &'static VTable = &VTable {
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

    pub fn is_immutable(&self) -> bool {
        self.hdr.type_bits() == TypeCode16::IMMUTABLE_BYTEVECTOR.0
    }

    pub fn new<const IMMUTABLE: bool>(
        mc: Mutation<'gc>,
        length: usize,
        fill: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let mut hdr = ScmHeader::new();
        let tc = if IMMUTABLE {
            TypeCode16::IMMUTABLE_VECTOR
        } else {
            TypeCode16::MUTABLE_VECTOR
        };
        hdr.set_type_bits(tc.0 as _);

        unsafe {
            let alloc = mc.raw_allocate(
                size_of::<Self>() + size_of::<Value>() * length,
                align_of::<Self>(),
                &Self::VT,
                AllocationSemantics::Default,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.length = length;
            vec.hdr = hdr;
            for i in 0..length {
                vec.data.as_mut_ptr().add(i).write(Lock::new(fill));
            }

            let x = Gc::from_gcobj(alloc);

            x
        }
    }

    pub fn from_slice(mc: Mutation<'gc>, slice: &[Value<'gc>]) -> Gc<'gc, Self> {
        let length = slice.len();
        let vector = Self::new::<false>(mc, length, Value::undefined());

        Self::copy_from(vector, slice, mc);
        vector
    }
    pub fn as_slice(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }

    pub unsafe fn as_slice_mut_unchecked(&mut self) -> &mut [Value<'gc>] {
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

unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Vector<'gc> {}

impl<'gc> Deref for Vector<'gc> {
    type Target = [Lock<Value<'gc>>];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }
}

unsafe impl<'gc> IndexWrite<usize> for Vector<'gc> {}
impl<'gc> Index<usize> for Vector<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        unsafe { &std::slice::from_raw_parts(self.data.as_ptr(), self.len())[index] }
    }
}

impl<'gc> IndexMut<usize> for Vector<'gc> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        unsafe { &mut std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len())[index] }
    }
}

#[macro_export]
macro_rules! vector {
    ($mc: expr, $($value: expr),*) => {{
        use $crate::runtime::value::IntoValue;

        let slice = &[$({
            let value = $value;
            value.into_value($mc)
        }),*];
        let length = slice.len();
        let vector = $crate::runtime::value::Vector::new::<false>(*$mc, length, $crate::runtime::value::Value::unspecified());
        vector.copy_from(slice, *$mc);
        vector
    }};

    ($mc: expr, $value: expr; $count: expr) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let value = $value;
        let count = $count;
        let vector = $crate::runtime::value::Vector::new(&$mc, count, value.into_value($mc));
        vector
    }};

    ($mc: expr) => {
        $crate::runtime::value::Vector::new(&$mc, 0, Value::null())
    };
}
type BytevectorMappingField = BitField<u64, bool, { TypeBits::NEXT_BIT }, 1, false>;

#[repr(C, align(8))]
pub struct ByteVector {
    hdr: ScmHeader,
    pub(crate) len: usize,
    pub(crate) contents: Address,
}

pub const BYTE_VECTOR_MAX_LENGTH: usize = usize::MAX;

extern "C" fn trace_byte_vector(vec: GCObject, vis: &mut Visitor) {
    let _ = vec;
    let _ = vis;
}

extern "C" fn process_weak_byte_vector(_: GCObject, _: &mut WeakProcessor) {}

impl ByteVector {
    pub const VT: &'static VTable = &VTable {
        type_name: "ByteVector",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some({
            extern "C" fn sz(vec: GCObject) -> usize {
                unsafe {
                    let bv = vec.to_address().as_ref::<ByteVector>();
                    if bv.is_mapping() {
                        return 0;
                    }
                    vec.to_address().as_ref::<ByteVector>().len * size_of::<u8>()
                }
            }
            sz
        }),
        trace: trace_byte_vector,
        weak_proc: process_weak_byte_vector,
    };

    pub fn contents(&self) -> Address {
        self.contents
    }

    pub fn is_mapping(&self) -> bool {
        BytevectorMappingField::decode(self.hdr.word)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_immutable(&self) -> bool {
        self.hdr.type_bits() == TypeCode16::IMMUTABLE_BYTEVECTOR.0
    }

    pub fn new<'gc, const IMMUTABLE: bool>(
        mc: Mutation<'gc>,
        length: usize,
        movable: bool,
    ) -> Gc<'gc, Self> {
        assert!(
            length <= BYTE_VECTOR_MAX_LENGTH,
            "Byte vector length exceeds maximum allowed size"
        );
        let mut hdr = ScmHeader::new();
        let tc = if IMMUTABLE {
            TypeCode16::IMMUTABLE_BYTEVECTOR
        } else {
            TypeCode16::MUTABLE_BYTEVECTOR
        };

        let semantics = if movable {
            AllocationSemantics::Default
        } else {
            AllocationSemantics::NonMoving
        };

        hdr.set_type_bits(tc.0 as _);
        unsafe {
            let alloc = mc.raw_allocate(
                size_of::<Self>() + size_of::<u8>() * length,
                align_of::<Self>(),
                &Self::VT,
                semantics,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.hdr = hdr;
            vec.len = length;
            let contents = (vec as *mut Self).add(1);
            vec.contents = Address::from_ptr(contents as *mut u8);

            Gc::from_gcobj(alloc)
        }
    }

    /// Allocate a new memory-mapped bytevector. This can be used to represent
    /// memory from FFI calls or memory-mapped files.
    pub fn new_mapping<'gc>(mc: Mutation<'gc>, addr: Address, length: usize) -> Gc<'gc, Self> {
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::BYTEVECTOR.0 as _);
        hdr.word = BytevectorMappingField::update(true, hdr.word);

        unsafe {
            let alloc = mc.raw_allocate(
                size_of::<Self>(),
                align_of::<Self>(),
                &Self::VT,
                AllocationSemantics::Default,
            );

            let vec = alloc.to_address().as_mut_ref::<Self>();
            vec.hdr = hdr;
            vec.len = length;
            vec.contents = addr;

            Gc::from_gcobj(alloc)
        }
    }

    #[inline(never)]
    pub fn from_slice<'gc>(mc: Mutation<'gc>, slice: &[u8], movable: bool) -> Gc<'gc, Self> {
        let length = slice.len();
        let byte_vector = Self::new::<false>(mc, length, movable);
        byte_vector.copy_from(slice);

        byte_vector
    }

    pub fn as_slice<'gc>(self: Gc<'gc, Self>) -> &'gc [u8] {
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }

    pub unsafe fn as_slice_mut_unchecked(&self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.contents.to_mut_ptr() as *mut u8, self.len) }
    }

    pub fn fill(&self, fill: u8) {
        unsafe {
            let slice = self.as_slice_mut_unchecked();
            for byte in slice.iter_mut() {
                *byte = fill;
            }
        }
    }

    #[inline(never)]
    pub fn copy_from(&self, other: impl AsRef<[u8]>) {
        let other_slice = other.as_ref();
        assert!(
            other_slice.len() <= self.len,
            "Cannot copy from byte vector of different length"
        );

        unsafe {
            self.as_slice_mut_unchecked()[..other_slice.len()].copy_from_slice(other_slice);
        }
    }
}

impl<'gc> AsRef<[u8]> for ByteVector {
    fn as_ref(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }
}

impl<'gc> Deref for ByteVector {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.contents.to_ptr(), self.len) }
    }
}

impl<'gc> Index<usize> for ByteVector {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::Range<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::Range<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::RangeFrom<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeFrom<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::RangeInclusive<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeInclusive<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::RangeTo<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeTo<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::RangeToInclusive<usize>> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeToInclusive<usize>) -> &Self::Output {
        &(**self)[index]
    }
}

impl<'gc> Index<core::ops::RangeFull> for ByteVector {
    type Output = [u8];

    fn index(&self, index: core::ops::RangeFull) -> &Self::Output {
        &(**self)[index]
    }
}

unsafe impl<'gc> Tagged for Vector<'gc> {
    const ONLY_TC16: bool = false;
    const TC16: &'static [TypeCode16] =
        &[TypeCode16::IMMUTABLE_VECTOR, TypeCode16::MUTABLE_BYTEVECTOR];
    const TC8: TypeCode8 = TypeCode8::VECTOR;
    const TYPE_NAME: &'static str = "vector";
}

unsafe impl<'gc> Tagged for ByteVector {
    const ONLY_TC16: bool = false;
    const TC16: &'static [TypeCode16] = &[
        TypeCode16::IMMUTABLE_BYTEVECTOR,
        TypeCode16::MUTABLE_BYTEVECTOR,
    ];
    const TC8: TypeCode8 = TypeCode8::BYTEVECTOR;
    const TYPE_NAME: &'static str = "bytevector";
}

pub(crate) type TupleLengthBits = BitField<u64, u32, { TypeBits::NEXT_BIT }, 32, false>;

#[repr(C, align(8))]
pub struct Tuple<'gc> {
    pub(crate) hdr: ScmHeader,
    pub(crate) data: [Lock<Value<'gc>>; 0],
}

extern "C" fn trace_tuple(tuple: GCObject, vis: &mut Visitor) {
    unsafe {
        let tuple = tuple
            .to_address()
            .as_mut_ref::<Tuple<'static>>()
            .as_slice_mut_unchecked();

        tuple.trace(vis);
    }
}

extern "C" fn process_weak_tuple(_: GCObject, _: &mut WeakProcessor) {}

unsafe impl<'gc> Tagged for Tuple<'gc> {
    const TC8: TypeCode8 = TypeCode8::TUPLE;
    const TYPE_NAME: &'static str = "tuple";
}

impl<'gc> Tuple<'gc> {
    pub const VT: &'static VTable = &VTable {
        type_name: "Tuple",
        instance_size: size_of::<Self>(),
        alignment: align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some({
            extern "C" fn sz(tuple: GCObject) -> usize {
                unsafe { tuple.to_address().as_ref::<Tuple<'static>>().len() * size_of::<Value>() }
            }
            sz
        }),
        trace: trace_tuple,
        weak_proc: process_weak_tuple,
    };

    pub fn len(&self) -> usize {
        TupleLengthBits::decode(self.hdr.word) as usize
    }

    pub fn new(mc: Mutation<'gc>, length: usize, init: Value<'gc>) -> Gc<'gc, Self> {
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::TUPLE.0 as _);
        hdr.word = TupleLengthBits::update(length as u32, hdr.word);

        unsafe {
            let alloc = mc.raw_allocate(
                size_of::<Self>() + size_of::<Value>() * length,
                align_of::<Self>(),
                &Self::VT,
                AllocationSemantics::Default,
            );

            let tuple = alloc.to_address().as_mut_ref::<Self>();
            tuple.hdr = hdr;

            for i in 0..length {
                tuple.data.as_mut_ptr().add(i).write(Lock::new(init));
            }

            Gc::from_gcobj(alloc)
        }
    }

    pub fn from_slice(mc: Mutation<'gc>, slice: &[Value<'gc>]) -> Gc<'gc, Self> {
        let length = slice.len();
        let tuple = Self::new(mc, length, Value::new(false));
        Self::copy_from(tuple, slice, mc);
        tuple
    }

    pub fn as_slice(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len()) }
    }

    pub unsafe fn as_slice_mut_unchecked(&mut self) -> &mut [Value<'gc>] {
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
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

impl<'gc> Index<usize> for Tuple<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "Index out of bounds");
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Tuple<'gc> {}
unsafe impl<'gc> IndexWrite<usize> for Tuple<'gc> {}
