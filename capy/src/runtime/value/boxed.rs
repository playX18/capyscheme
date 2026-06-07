//! Mutable Scheme boxes.

use crate::{
    rsgc::{
        Gc, Trace,
        cell::Lock,
        object::{ClassId, builtin_class_ids, class_header_word},
    },
    runtime::Context,
};

use super::{ClassTagged, Value};

#[derive(Trace)]
#[collect(no_drop)]
/// A single-slot mutable container.
pub struct Boxed<'gc> {
    pub val: Lock<Value<'gc>>,
}

fn box_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::BOX).unwrap())
}

impl<'gc> Boxed<'gc> {
    /// Allocates a box containing `val`.
    pub fn new(ctx: Context<'gc>, val: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            *ctx,
            Self {
                val: Lock::new(val),
            },
            box_header_word(),
        )
    }

    pub fn get(&self) -> Value<'gc> {
        self.val.get()
    }
}

unsafe impl<'gc> ClassTagged for Boxed<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::BOX];
    const TYPE_NAME: &'static str = "box";
}
