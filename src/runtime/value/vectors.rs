use std::{
    marker::PhantomData, mem::MaybeUninit, ops::Index, sync::atomic::{AtomicU16, Ordering}
};

use rsgc::{
    EnsureGCInfo, GCInfo, GCInfoIndex, GCInfoIndexForT, Gc, Trace, Visitor,
    barrier::{AsRefWrite, IndexWrite},
    context::Mutation,
    gc::GLOBAL_GC_INFO_TABLE,
    generic_static::Namespace,
    vmkit::prelude::{GCMetadata, TraceCallback},
};

use crate::runtime::{value::TypeCode16, Context};

use super::{Tagged, TypeCode8, Value, ValuesNamespace};

pub const VECTOR_MAX_LENGTH: usize = i32::MAX as usize;

#[repr(align(8))]
pub struct Vector<'gc> {
    /// Vector length, encoded as int32 value
    length: Value<'gc>,
    data: [Value<'gc>; 0],
}

impl<'gc> Vector<'gc> {
    /// Creates a new vector with the given length.
    ///
    /// # Panics
    ///
    /// Panics if the length is greater than `VECTOR_MAX_LENGTH`.
    pub fn new(mc: &Mutation<'gc>, length: usize, fill: Value) -> Gc<'gc, Self> {
        assert!(length <= VECTOR_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let vector = mc.allocate_with_layout::<Self>(layout);
            vector.set_user_header(TypeCode16::MUTABLE_VECTOR.into());
            vector.as_ptr().write(MaybeUninit::new(Vector {
                length: Value::new(length as i32),
                data: [],
            }));
            let vector_data = vector.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();

            for i in 0..length {
                vector_data.add(i).write(fill);
            }

            vector.assume_init()
        }
    }

    pub fn from_iter(
        mc: &Mutation<'gc>,
        iter: impl IntoIterator<Item = Value<'gc>, IntoIter: ExactSizeIterator<Item = Value<'gc>>>,
    ) -> Gc<'gc, Self> {
        let iter = iter.into_iter();
        let length = iter.len();
        assert!(length <= VECTOR_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let vector = mc.allocate_with_layout::<Self>(layout);

            vector.as_ptr().write(MaybeUninit::new(Vector {
                length: Value::new(length as i32),
                data: [],
            }));
            let vector_data = vector.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();
            for (i, value) in iter.enumerate() {
                vector_data.add(i).write(value);
            }
            vector.assume_init()
        }
    }

    pub fn fill(self: Gc<'gc, Self>, mc: Context<'gc>, fill: Value<'gc>) {
        let vec = Gc::write(&mc, self);

        for i in 0..vec.len() {
            vec[i].write(fill);
        }
    }

    pub fn copy_from(self: Gc<'gc, Self>, mc: Context<'gc>, other: impl AsRef<[Value<'gc>]>) {
        let vec = Gc::write(&mc, self);
        let other = other.as_ref();

        assert!(other.len() <= vec.len());

        for i in 0..other.len() {
            vec[i].write(other[i]);
        }
    }

    pub fn len(&self) -> usize {
        self.length.as_int32() as usize
    }
}
impl<'gc> AsRef<[Value<'gc>]> for Vector<'gc> {
    fn as_ref(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

unsafe impl<'gc> Trace for Vector<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        for value in unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len()) } {
            value.trace(visitor);
        }
    }
}

impl<'gc> std::ops::Deref for Vector<'gc> {
    type Target = [Value<'gc>];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.length.as_int32() as usize) }
    }
}

unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Vector<'gc> {}
unsafe impl<'gc> IndexWrite<usize> for Vector<'gc> {}

impl<'gc> Index<usize> for Vector<'gc> {
    type Output = Value<'gc>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len());
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Vector<'gc> {
    fn ensure_gc_info() -> rsgc::gc::GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<Vector<'gc>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_vector_info(&registered_index.index)
    }
}

fn register_vector_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: align_of::<Value<'static>>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let vec = unsafe { object.as_address().as_ref::<Vector<'static>>() };
                    vec.len() * size_of::<Value<'static>>()
                }),
                instance_size: size_of::<Vector<'static>>(),
                trace: TraceCallback::TraceObject(|object, tracer| {
                    let vec = unsafe { object.as_address().as_mut_ref::<Vector<'static>>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object.as_object_unchecked())) };
                    vec.trace(&mut visitor);
                }),
            },
        },
    )
}

unsafe impl<'gc> Tagged for Vector<'gc> {
    const TC8: TypeCode8 = TypeCode8::VECTOR;
    const TC16: &'static [TypeCode16] = &[TypeCode16::MUTABLE_VECTOR, TypeCode16::IMMUTABLE_VECTOR];
}

#[macro_export]
macro_rules! vector {
    ($mc: expr, $($value: expr),*) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let slice = &[$({
            let value = $value;
            value.into_value($mc)
        }),*];
        let length = slice.len();
        let vector = $crate::runtime::value::Vector::new(&$mc, length, $crate::runtime::value::Value::unspecified());
        vector.copy_from($mc, slice);
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

// ByteVector implementation starts here

pub const BYTE_VECTOR_MAX_LENGTH: usize = usize::MAX;

#[repr(align(8))]
pub struct ByteVector<'gc> {
    pd: PhantomData<&'gc ()>,
    length: usize,
    data: [u8; 0],
}

impl<'gc> ByteVector<'gc> {
    /// Creates a new byte vector with the given length.
    ///
    /// # Panics
    ///
    /// Panics if the length is greater than `BYTE_VECTOR_MAX_LENGTH`.
    pub fn new(mc: &Mutation<'gc>, length: usize, fill: u8) -> Gc<'gc, Self> {
        assert!(length <= BYTE_VECTOR_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<u8>(),
                std::mem::align_of::<u8>(),
            );

            let byte_vector = mc.allocate_with_layout::<Self>(layout);
            byte_vector.set_user_header(TypeCode16::MUTABLE_BYTEVECTOR.into());
            byte_vector.as_ptr().write(MaybeUninit::new(ByteVector {
                length,
                pd: PhantomData,
                data: [],
            }));
            let byte_vector_data = byte_vector.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();

            for i in 0..length {
                byte_vector_data.add(i).write(fill);
            }

            byte_vector.assume_init()
        }
    }

    pub fn from_iter(
        mc: &Mutation<'gc>,
        iter: impl IntoIterator<Item = u8, IntoIter: ExactSizeIterator<Item = u8>>,
    ) -> Gc<'gc, Self> {
        let iter = iter.into_iter();
        let length = iter.len();
        assert!(length <= BYTE_VECTOR_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<u8>(),
                std::mem::align_of::<u8>(),
            );

            let byte_vector = mc.allocate_with_layout::<Self>(layout);

            byte_vector.as_ptr().write(MaybeUninit::new(ByteVector {
                length,
                pd: PhantomData,
                data: [],
            }));
            let byte_vector_data = byte_vector.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();
            for (i, value) in iter.enumerate() {
                byte_vector_data.add(i).write(value);
            }
            byte_vector.assume_init()
        }
    }

    pub fn fill(self: Gc<'gc, Self>, mc: Context<'gc>, fill: u8) {
        let vec = Gc::write(&mc, self);
        let data_ptr = vec.data.as_ptr() as *mut u8;
        for i in 0..vec.len() {
            unsafe {
                data_ptr.add(i).write(fill);
            }
        }
    }

    pub fn copy_from(self: Gc<'gc, Self>, mc: Context<'gc>, other: impl AsRef<[u8]>) {
        let vec = Gc::write(&mc, self);
        let other = other.as_ref();

        assert!(other.len() <= vec.len());
        let data_ptr = vec.data.as_ptr() as *mut u8;
        for i in 0..other.len() {
            unsafe {
                data_ptr.add(i).write(other[i]);
            }
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }
}

impl<'gc> AsRef<[u8]> for ByteVector<'gc> {
    fn as_ref(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

unsafe impl<'gc> Trace for ByteVector<'gc> {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {
        // u8 does not need tracing
    }
}

impl<'gc> std::ops::Deref for ByteVector<'gc> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.length) }
    }
}

unsafe impl<'gc> AsRefWrite<[u8]> for ByteVector<'gc> {}
unsafe impl<'gc> IndexWrite<usize> for ByteVector<'gc> {}

impl<'gc> Index<usize> for ByteVector<'gc> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len());
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for ByteVector<'gc> {
    fn ensure_gc_info() -> rsgc::gc::GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<ByteVector<'gc>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_byte_vector_info(&registered_index.index)
    }
}

fn register_byte_vector_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: std::mem::align_of::<u8>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let vec = unsafe { object.as_address().as_ref::<ByteVector<'static>>() };
                    vec.len() * std::mem::size_of::<u8>()
                }),
                instance_size: std::mem::size_of::<ByteVector<'static>>(),
                trace: TraceCallback::TraceObject(|_object, _tracer| {
                    // No tracing needed for u8
                }),
            },
        },
    )
}

unsafe impl<'gc> Tagged for ByteVector<'gc> {
    const TC8: TypeCode8 = TypeCode8::BYTEVECTOR;
    const TC16: &'static [TypeCode16] = &[TypeCode16::MUTABLE_BYTEVECTOR, TypeCode16::IMMUTABLE_BYTEVECTOR];
}

#[macro_export]
macro_rules! bytevector {
    ($mc: expr, $($value: expr),*) => {{
        let mc = $mc;
        let slice = &[$($value),*];
        let length = slice.len();
        let byte_vector = $crate::runtime::value::ByteVector::new($mc, length, 0u8); // Fill with 0 initially
        byte_vector.copy_from($mc, slice);
        byte_vector
    }};

    ($mc: expr, $value: expr; $count: expr) => {{
        let mc = $mc;
        let value = $value;
        let count = $count;
        let byte_vector = $crate::runtime::value::ByteVector::new($mc, count, value);
        byte_vector
    }};

    ($mc: expr) => {
        $crate::runtime::value::ByteVector::new($mc, 0, 0u8)
    };
}

