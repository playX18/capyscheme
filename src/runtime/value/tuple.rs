// filepath: /var/home/adel/Projects/capyscheme/src/runtime/value/tuple.rs
use std::{
    mem::{MaybeUninit, align_of, size_of},
    ops::Index,
    ptr::NonNull,
    sync::atomic::{AtomicU16, Ordering},
};

use rsgc::{
    EnsureGCInfo, GCInfo, GCInfoIndex, GCInfoIndexForT, Gc, Trace, Visitor,
    barrier::{AsRefWrite, IndexWrite, Write},
    context::Mutation,
    gc::{GLOBAL_GC_INFO_TABLE, UserHeader},
    generic_static::Namespace,
    vmkit::prelude::{GCMetadata, TraceCallback},
};

use crate::runtime::{
    Context,
    value::{Tagged, TypeCode8, TypeCode16, Value, ValuesNamespace},
};

pub const TUPLE_MAX_LENGTH: usize = u16::MAX as usize;

#[repr(align(8))]
pub struct Tuple<'gc> {
    data: [Value<'gc>; 0],
}

fn tuple_header(len: usize) -> u32 {
    let type_code_byte = TypeCode8::TUPLE.0 as u8;
    (type_code_byte as u32) | ((len as u32) << 8)
}

fn tuple_length(header: u32) -> usize {
    ((header >> 8) & 0xFFFF) as usize
}

impl<'gc> Tuple<'gc> {
    pub fn new(ctx: Context<'gc>, length: usize, fill_value: Value<'gc>) -> Gc<'gc, Self> {
        assert!(length <= TUPLE_MAX_LENGTH, "Tuple length exceeds maximum");

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let tuple_gc = ctx.allocate_with_layout::<Self>(layout);

            let type_code_byte = Self::TC8.0;
            let header_val = (type_code_byte as u32) | ((length as u32) << 8);
            tuple_gc.set_user_header(header_val);

            let mut ptr = tuple_gc.as_ptr();
            ptr.write(MaybeUninit::new(Tuple { data: [] }));

            let data_ptr = ptr.as_mut().assume_init_mut().data.as_mut_ptr();
            for i in 0..length {
                data_ptr.add(i).write(fill_value);
            }

            tuple_gc.assume_init()
        }
    }

    pub fn from_iter(
        mc: Context<'gc>,
        iter: impl IntoIterator<Item = Value<'gc>, IntoIter: ExactSizeIterator<Item = Value<'gc>>>,
    ) -> Gc<'gc, Self> {
        let iter = iter.into_iter();
        let length = iter.len();
        assert!(length <= TUPLE_MAX_LENGTH, "Tuple length exceeds maximum");

        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(
                std::mem::size_of::<Self>() + length * std::mem::size_of::<Value<'gc>>(),
                std::mem::align_of::<Value<'gc>>(),
            );

            let tuple_gc = mc.allocate_with_layout::<Self>(layout);

            let type_code_byte = Self::TC8.0;
            let header_val = (type_code_byte as u32) | ((length as u32) << 8);
            tuple_gc.set_user_header(header_val);

            let mut ptr = tuple_gc.as_ptr();
            ptr.write(MaybeUninit::new(Tuple { data: [] }));

            let data_ptr = ptr.as_mut().assume_init_mut().data.as_mut_ptr();
            for (i, value) in iter.enumerate() {
                data_ptr.add(i).write(value);
            }
            tuple_gc.assume_init()
        }
    }

    pub fn len(&self) -> usize {
        unsafe {
            let gc = Gc::from_ptr(NonNull::new_unchecked(self as *const Self as *mut Self));
            let header_val = gc.user_header();
            tuple_length(header_val)
        }
    }

    pub fn fill(self: Gc<'gc, Self>, mc: Context<'gc>, fill_value: Value<'gc>) {
        let tuple_mut = Gc::write(&mc, self);
        for i in 0..self.len() {
            tuple_mut[i].write(fill_value);
        }
    }

    pub fn copy_from(self: Gc<'gc, Self>, mc: Context<'gc>, other_slice: impl AsRef<[Value<'gc>]>) {
        let tuple_mut = Gc::write(&mc, self);
        let other_slice = other_slice.as_ref();

        assert!(
            other_slice.len() <= tuple_mut.len(),
            "Source slice too large for tuple"
        );

        for (i, value) in other_slice.iter().enumerate() {
            tuple_mut[i].write(*value);
        }
    }
}

impl<'gc> AsRef<[Value<'gc>]> for Tuple<'gc> {
    fn as_ref(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

unsafe impl<'gc> Trace for Tuple<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        for value in unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len()) } {
            value.trace(visitor);
        }
    }
}

impl<'gc> std::ops::Deref for Tuple<'gc> {
    type Target = [Value<'gc>];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }
}

unsafe impl<'gc> AsRefWrite<[Value<'gc>]> for Tuple<'gc> {}
unsafe impl<'gc> IndexWrite<usize> for Tuple<'gc> {}

impl<'gc> Index<usize> for Tuple<'gc> {
    type Output = Value<'gc>;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "Tuple index out of bounds");
        unsafe { &*self.data.as_ptr().add(index) }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Tuple<'gc> {
    fn ensure_gc_info() -> rsgc::gc::GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<Tuple<'gc>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_tuple_info(&registered_index.index)
    }
}

fn register_tuple_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: align_of::<Value<'static>>(),
                compute_alignment: None,
                compute_size: Some(|object_header_access| unsafe {
                    let this = Gc::from_ptr(NonNull::from(
                        object_header_access
                            .as_address()
                            .as_mut_ref::<Tuple<'static>>(),
                    ));
                    let header_val = this.user_header();
                    let length = tuple_length(header_val); // Extract length from header
                    length * size_of::<Value<'static>>()
                }),
                instance_size: size_of::<Tuple<'static>>(),
                trace: TraceCallback::TraceObject(|object_ptr, tracer| {
                    let tuple_mut =
                        unsafe { object_ptr.as_address().as_mut_ref::<Tuple<'static>>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object_ptr.as_object_unchecked())) };
                    tuple_mut.trace(&mut visitor);
                }),
            },
        },
    )
}

unsafe impl<'gc> Tagged for Tuple<'gc> {
    const TC8: TypeCode8 = TypeCode8::TUPLE;
   
}

#[macro_export]
macro_rules! tuple {
    ($mc: expr, $($value: expr),*) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let slice = &[$({
            let val_expr = $value;
            val_expr.into_value($mc)
        }),*];
        let length = slice.len();
        let new_tuple = $crate::runtime::value::Tuple::new(mc, length, $crate::runtime::value::Value::null());
        new_tuple.copy_from($mc, slice);
        new_tuple
    }};

    ($mc: expr, $value: expr; $count: expr) => {{
        use $crate::runtime::value::IntoValue;
        let mc = $mc;
        let fill_value = $value.into_value($mc);
        let count = $count;
        $crate::runtime::value::Tuple::new($mc, count, fill_value)
    }};

    ($mc: expr) => {
        $crate::runtime::value::Tuple::new($mc, 0, $crate::runtime::value::Value::null())
    };
}

#[cfg(test)]
mod tests {
    use crate::runtime::Scheme;

    #[test]
    fn test_tuple() {
        let scm = Scheme::new();

        scm.enter(|ctx| {
            let t = tuple!(ctx, 1, 2, 3);

            assert_eq!(t.len(), 3);

            assert_eq!(t[0].as_int32(), 1);
            assert_eq!(t[1].as_int32(), 2);
            assert_eq!(t[2].as_int32(), 3);
        });
    }
}
