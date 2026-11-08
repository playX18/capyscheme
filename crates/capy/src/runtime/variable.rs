//! Variable cells.
//!
//! A [`Variable`] is the cell behind every linklet slot and every top-level
//! name of a namespace. An unbound cell holds `Value::undefined()`.

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, barrier, cell::Lock};

use crate::runtime::{
    MutationContext,
    objectmodel::{ClassTagged, IntoValue, Symbol, Value},
    watchpoint::WatchpointSet,
};

/// A single mutable binding cell
#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Variable<'gc> {
    pub value: Lock<Value<'gc>>,
    /// Tier 2 JIT folds global variables, and installs watchpoint on any folded variable.
    /// If the variable is modified, watchpoint fires and invalidates code.
    pub watch: Lock<Option<Gc<'gc, WatchpointSet<'gc>>>>,
}

pub const WATCH_OFFSET: i32 = 8;

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
                watch: Lock::new(None),
            },
            variable_header_word(),
        )
    }

    /// Is the variable bound?
    pub fn is_bound(&self) -> bool {
        self.value.get() != Value::undefined()
    }

    /// Stores `value` in the cell. A watched variable then fires its set, so
    /// the optimized code that folded the old value is invalidated.
    pub fn set(self: Gc<'gc, Self>, ctx: MutationContext<'gc>, value: Value<'gc>) {
        let old = self.value.get();
        barrier::field!(Gc::write(ctx, self), Variable, value)
            .unlock()
            .set(value);
        if old != value {
            self.fire_watch(ctx);
        }
    }

    /// Fires the watchpoint set of a watched variable. The caller stored the
    /// new value first (mutate, then fire).
    pub fn fire_watch(self: Gc<'gc, Self>, ctx: MutationContext<'gc>) {
        if let Some(set) = self.watch.get() {
            WatchpointSet::fire(set, ctx);
        }
    }

    /// The watchpoint set of this variable, created on first use.
    pub fn watch_set(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
    ) -> Gc<'gc, WatchpointSet<'gc>> {
        if let Some(set) = self.watch.get() {
            return set;
        }
        let fresh = WatchpointSet::new(ctx);
        let owner = Gc::write(ctx, self);
        let _ = owner;
        match self.watch.compare_exchange(
            None,
            Some(fresh),
            std::sync::atomic::Ordering::AcqRel,
            std::sync::atomic::Ordering::Acquire,
        ) {
            Ok(_) => fresh,
            Err(current) => current.expect("a failed exchange from None names a set"),
        }
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
        var.set(ctx, value);
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
