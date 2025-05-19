use std::{
    mem::MaybeUninit,
    ops::Index,
    sync::atomic::{AtomicU16, Ordering},
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

use super::{ListIterator, Tagged, TypeCode8, Value, ValuesNamespace};

pub const VALUES_MAX_LENGTH: usize = i32::MAX as usize;

#[repr(align(8))]
pub struct Values<'gc> {
    /// Values length, encoded as int32 value
    length: Value<'gc>,
    data: [Value<'gc>; 0],
}

impl<'gc> Values<'gc> {
    /// Creates a new values with the given length.
    ///
    /// # Panics
    ///
    /// Panics if the length is greater than `VALUES_MAX_LENGTH`.
    pub fn new(mc: &Mutation<'gc>, length: usize, fill: Value) -> Gc<'gc, Self> {
        assert!(length <= VALUES_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let values = mc.allocate_with_layout::<Self>(layout);
            values.set_user_header(TypeCode16::VALUES.into());
            values.as_ptr().write(MaybeUninit::new(Values {
                length: Value::new(length as i32),
                data: [],
            }));
            let values_data = values.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();

            for i in 0..length {
                values_data.add(i).write(fill);
            }

            values.assume_init()
        }
    }

    pub fn from_iter(
        mc: &Mutation<'gc>,
        iter: impl IntoIterator<Item = Value<'gc>, IntoIter: ExactSizeIterator<Item = Value<'gc>>>,
    ) -> Gc<'gc, Self> {
        let iter = iter.into_iter();
        let length = iter.len();
        assert!(length <= VALUES_MAX_LENGTH);

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let values = mc.allocate_with_layout::<Self>(layout);

            values.as_ptr().write(MaybeUninit::new(Values {
                length: Value::new(length as i32),
                data: [],
            }));
            let values_data = values.as_ptr().as_mut().assume_init_mut().data.as_mut_ptr();
            for (i, value) in iter.enumerate() {
                values_data.add(i).write(value);
            }
            values.assume_init()
        }
    }

    pub fn fill(self: Gc<'gc, Self>, mc: &Mutation<'gc>, fill: Value<'gc>) {
        let vec = Gc::write(mc, self);

        for i in 0..vec.len() {
            vec[i].write(fill);
        }
    }

    pub fn copy_from(self: Gc<'gc, Self>, mc: &Mutation<'gc>, other: impl AsRef<[Value<'gc>]>) {
        let vec = Gc::write(mc, self);
        let other = other.as_ref();

        assert!(other.len() <= vec.len());

        for i in 0..other.len() {
            vec[i].write(other[i]);
        }
    }

    pub fn len(&self) -> usize {
        self.length.as_int32() as usize
    }

    pub fn from_list(ctx: Context<'gc>, list: Value<'gc>) -> Gc<'gc, Self> {
        let values = Values::new(&ctx, list.list_length(), Value::null());
        let mut length = 0;
        let wvalues = Gc::write(&ctx, values);
        for value in ListIterator::new(list) {
            wvalues[length].write(value);
            length += 1;
        }

        values
    }
}
impl<'gc> AsRef<[Value<'gc>]> for Values<'gc> {
    fn as_ref(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

unsafe impl<'gc> Trace for Values<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        for value in unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len()) } {
            value.trace(visitor);
        }
    }
}



impl<'gc> std::ops::Deref for Values<'gc> {
    type Target = [Value<'gc>];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.length.as_int32() as usize) }
    }
}

unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Values<'gc> {}
unsafe impl<'gc> IndexWrite<usize> for Values<'gc> {}

impl<'gc> Index<usize> for Values<'gc> {
    type Output = Value<'gc>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len());
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Values<'gc> {
    fn ensure_gc_info() -> rsgc::gc::GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<Values<'gc>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_values_info(&registered_index.index)
    }
}

fn register_values_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: align_of::<Value<'static>>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let vec = unsafe { object.as_address().as_ref::<Values<'static>>() };
                    vec.len() * size_of::<Value<'static>>()
                }),
                instance_size: size_of::<Values<'static>>(),
                trace: TraceCallback::TraceObject(|object, tracer| {
                    let vec = unsafe { object.as_address().as_mut_ref::<Values<'static>>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object.as_object_unchecked())) };
                    vec.trace(&mut visitor);
                }),
            },
        },
    )
}

unsafe impl<'gc> Tagged for Values<'gc> {
    const TC8: TypeCode8 = TypeCode8::VALUES;
}

#[macro_export]
macro_rules! values {
    ($mc: expr, $($value: expr),*) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let slice = &[$({
            let value = $value;
            value.into_value($mc)
        }),*];
        let length = slice.len();
        let values = $crate::runtime::value::Values::new($mc, length, Value::null());
        values.copy_from($mc, slice);
        values
    }};

    ($mc: expr, $value: expr; $count: expr) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let value = $value;
        let count = $count;
        let values = $crate::runtime::value::Values::new($mc, count, value.into_value($mc));
        values
    }};

    ($mc: expr) => {
        $crate::runtime::value::Values::new($mc, 0, Value::null())
    };
}
