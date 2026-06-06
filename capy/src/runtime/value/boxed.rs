//! Mutable Scheme boxes.

use crate::{
    rsgc::{
        Gc, Trace,
        object::{HeapTypeInfo, VTableOf},
    },
    runtime::Context,
};

use super::{Tagged, TypeCode8, Value};

#[derive(Trace)]
#[collect(no_drop)]
/// A single-slot mutable container.
pub struct Boxed<'gc> {
    pub val: Value<'gc>,
}

static BOX_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, Boxed<'static>>::VT,
    TypeCode8::BOX.bits() as u16,
);
pub static BOX_INFO: &HeapTypeInfo = &BOX_INFO_VALUE;

impl<'gc> Boxed<'gc> {
    /// Allocates a box containing `val`.
    pub fn new(ctx: Context<'gc>, val: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new_with_info(*ctx, Self { val }, BOX_INFO)
    }
}

unsafe impl<'gc> Tagged for Boxed<'gc> {
    const TC8: TypeCode8 = TypeCode8::BOX;
    const TYPE_NAME: &'static str = "box";
}
