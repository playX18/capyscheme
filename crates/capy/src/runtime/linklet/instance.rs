use super::*;

/// A single instance of a [Linklet].
#[repr(C)]
pub struct LibraryInstance<'gc> {
    /// The instance's name as a list of symbols, or `#f` if unnamed.
    pub name: Lock<Value<'gc>>,
    /// Eq-hash table of symbol -> [`Variable`] bindings.
    pub vars: Gc<'gc, HashTable<'gc>>,
    /// Eq-hash table of symbol -> exported syntax transformer.
    pub syntax: Gc<'gc, HashTable<'gc>>,
    /// Arbitrary metadata attached to the instance, or `#f`.
    pub meta: Lock<Value<'gc>>,
    /// Whether variables may be added after the instance is created.
    pub mutable: Cell<bool>,
}

fn instance_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::INSTANCE).expect("builtin class id is nonzero"),
    )
}

unsafe impl<'gc> Trace for LibraryInstance<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.vars.trace(visitor);
            self.syntax.trace(visitor);
            self.meta.trace(visitor);
        }
    }

    // SAFETY: Instances hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for LibraryInstance<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::INSTANCE];
    const TYPE_NAME: &'static str = "instance";
}

impl<'gc> LibraryInstance<'gc> {
    /// Allocates an empty instance named `name` (a symbol list or `#f`).
    pub fn new(ctx: MutationContext<'gc>, name: Value<'gc>, mutable: bool) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name: Lock::new(name),
                vars: HashTable::new(ctx, HashTableType::Eq, 0, 0.75),
                syntax: HashTable::new(ctx, HashTableType::Eq, 0, 0.75),
                meta: Lock::new(Value::new(false)),
                mutable: Cell::new(mutable),
            },
            instance_header_word(),
        )
    }

    /// The instance's name, as a list of symbols or `#f`.
    pub fn name(&self) -> Value<'gc> {
        self.name.get()
    }

    /// Whether variables may be added after creation.
    pub fn is_mutable(&self) -> bool {
        self.mutable.get()
    }

    pub fn over_vars(
        ctx: MutationContext<'gc>,
        name: Value<'gc>,
        vars: Gc<'gc, HashTable<'gc>>,
        mutable: bool,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name: Lock::new(name),
                vars,
                syntax: HashTable::new(ctx, HashTableType::Eq, 0, 0.75),
                meta: Lock::new(Value::new(false)),
                mutable: Cell::new(mutable),
            },
            instance_header_word(),
        )
    }

    /// Returns the variable bound to `sym` in this instance, if it has one.
    pub fn variable(
        &self,
        ctx: MutationContext<'gc>,
        sym: Value<'gc>,
    ) -> Option<Gc<'gc, Variable<'gc>>> {
        self.vars.get(ctx, sym).map(|var| var.downcast())
    }

    /// Returns the variable for `sym`, creating an unbound one if the instance
    /// does not have it yet.
    pub fn ensure_variable(
        &self,
        ctx: MutationContext<'gc>,
        sym: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        if let Some(var) = self.variable(ctx, sym) {
            return var;
        }

        let var = Variable::new(ctx, Value::undefined());
        self.vars.put(ctx, sym, var);
        var
    }

    /// The value of `sym`, or `None` if the instance has no variable for it or
    /// the variable is unbound.
    pub fn get(&self, ctx: MutationContext<'gc>, sym: Value<'gc>) -> Option<Value<'gc>> {
        self.variable(ctx, sym)
            .filter(|var| var.is_bound())
            .map(|var| var.get())
    }

    /// Stores `value` in the variable for `sym`, creating the variable if needed.
    pub fn set(&self, ctx: MutationContext<'gc>, sym: Value<'gc>, value: Value<'gc>) {
        let var = self.ensure_variable(ctx, sym);
        var.set(ctx, value);
    }

    /// Defines `sym` as `value`, creating the variable if needed, and returns it.
    pub fn define(
        &self,
        ctx: MutationContext<'gc>,
        sym: Value<'gc>,
        value: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        let var = self.ensure_variable(ctx, sym);
        var.set(ctx, value);
        var
    }

    /// Like [`Self::define`], with the name as a Rust string slice and the value as any
    /// type convertible into a GC value.
    pub fn define_rs(
        &self,
        ctx: MutationContext<'gc>,
        name: impl AsRef<str>,
        value: impl IntoValue<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        let sym = Symbol::from_str(ctx, name.as_ref());
        let value = value.into_value(ctx);
        self.define(ctx, sym.into(), value)
    }

    /// Records `transformer` as the exported syntax for `sym`.
    pub fn set_syntax(&self, ctx: MutationContext<'gc>, sym: Value<'gc>, transformer: Value<'gc>) {
        self.syntax.put(ctx, sym, transformer);
    }

    /// The exported syntax transformer for `sym`, if any.
    pub fn syntax_ref(&self, ctx: MutationContext<'gc>, sym: Value<'gc>) -> Option<Value<'gc>> {
        self.syntax.get(ctx, sym)
    }
}

