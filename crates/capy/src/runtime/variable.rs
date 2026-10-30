//! Variable cells.
//!
//! A [`Variable`] is the cell behind every linklet slot and every top-level
//! name of a namespace. An unbound cell holds `Value::undefined()`.

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, barrier, cell::Lock};

use crate::runtime::{
    MutationContext,
    value::{ClassTagged, IntoValue, Symbol, Value},
};

/// A single mutable binding cell
#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Variable<'gc> {
    pub value: Lock<Value<'gc>>,
}

fn variable_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::VARIABLE).expect("builtin class id is nonzero"),
    )
}

impl<'gc> Variable<'gc> {
    /// Allocates a variable cell holding `value`.
    pub fn new(ctx: MutationContext<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                value: Lock::new(value),
            },
            variable_header_word(),
        )
    }

    /// Is the variable bound?
    pub fn is_bound(&self) -> bool {
        self.value.get() != Value::undefined()
    }

    /// Stores `value` in the cell
    pub fn set(self: Gc<'gc, Self>, ctx: MutationContext<'gc>, value: Value<'gc>) {
        barrier::field!(Gc::write(ctx, self), Variable, value)
            .unlock()
            .set(value);
    }

    /// Reads the cell's current value.
    pub fn get(&self) -> Value<'gc> {
        self.value.get()
    }
}

unsafe impl<'gc> ClassTagged for Variable<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::VARIABLE];
    const TYPE_NAME: &'static str = "variable";
}

/// Defines the primitive `name` to `value` in the `(#%capy)` instance and returns the
/// variable cell.
pub fn define<'gc>(
    ctx: MutationContext<'gc>,
    name: &str,
    value: impl IntoValue<'gc>,
) -> Gc<'gc, Variable<'gc>> {
    let value = value.into_value(ctx);
    let sym = Symbol::from_str(ctx, name);
    ctx.globals().capy_instance().define(ctx, sym.into(), value)
}

use crate::prelude::*;

#[scheme(path=capy)]
pub mod variable_ops {

    #[scheme(name = "variable-ref")]
    pub fn variable_ref(var: Gc<'gc, Variable<'gc>>) -> Value<'gc> {
        var.get()
    }

    #[scheme(name = "variable-set!")]
    pub fn variable_set(var: Gc<'gc, Variable<'gc>>, value: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(ctx, var), Variable, value)
            .unlock()
            .set(value);
        Value::undefined()
    }

    #[scheme(name = "make-variable")]
    pub fn make_variable(value: Option<Value<'gc>>) -> Gc<'gc, Variable<'gc>> {
        let value = value.unwrap_or(Value::undefined());
        let var = Variable::new(ctx, value);
        var
    }

    #[scheme(name = "variable-bound?")]
    pub fn variable_bound(var: Gc<'gc, Variable<'gc>>) -> bool {
        var.is_bound()
    }
}
