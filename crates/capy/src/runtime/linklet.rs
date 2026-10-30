//! Linklet runtime objects.

use std::{cell::Cell, sync::OnceLock};

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, barrier, cell::Lock};

use crate::runtime::{
    MutationContext,
    value::{ClassTagged, HashTable, HashTableType, IntoValue, Symbol, Value},
    variable::Variable,
};

/// A single instance of a [Linklet].
#[repr(C)]
pub struct Instance<'gc> {
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

unsafe impl<'gc> Trace for Instance<'gc> {
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

unsafe impl<'gc> ClassTagged for Instance<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::INSTANCE];
    const TYPE_NAME: &'static str = "instance";
}

impl<'gc> Instance<'gc> {
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

/// A linklet: compiled body plus its import and export signature.
///
/// Immutable after construction. The layout is read by the Scheme accessors,
/// so keep the field order in sync with [`linklet_ops`].
#[repr(C)]
pub struct Linklet<'gc> {
    /// The linklet's name, or `#f`.
    pub name: Value<'gc>,
    /// List of `(key sym ...)` groups, `key` being a library-name list.
    pub imports: Value<'gc>,
    /// List of exported symbols.
    pub exports: Value<'gc>,
    /// Procedure taking one variable per import symbol, then one per export symbol.
    pub body: Value<'gc>,
    /// Arbitrary metadata attached to the linklet, or `#f`.
    pub meta: Value<'gc>,
}

fn linklet_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::LINKLET).expect("builtin class id is nonzero"),
    )
}
unsafe impl<'gc> Trace for Linklet<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.imports.trace(visitor);
            self.exports.trace(visitor);
            self.body.trace(visitor);
            self.meta.trace(visitor);
        }
    }

    // SAFETY: Linklets hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for Linklet<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::LINKLET];
    const TYPE_NAME: &'static str = "linklet";
}

impl<'gc> Linklet<'gc> {
    /// Allocates a linklet with the given signature and body.
    pub fn new(
        ctx: MutationContext<'gc>,
        name: Value<'gc>,
        imports: Value<'gc>,
        exports: Value<'gc>,
        body: Value<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name,
                imports,
                exports,
                body,
                meta,
            },
            linklet_header_word(),
        )
    }
}

/// A registry of library instances, keyed by library name.
///
/// A namespace owns one, so libraries imported in different registries are
/// instantiated separately. The process has a single root registry, see
/// [`root_registry`]; the Rust runtime only ever looks things up in that one.
#[repr(C)]
pub struct Registry<'gc> {
    /// The registry's name, for display.
    name: Lock<Value<'gc>>,
    instances: Gc<'gc, HashTable<'gc>>,
    /// Placeholder instances for keys that are referenced before their linklet
    /// is instantiated.
    pending: Gc<'gc, HashTable<'gc>>,
    /// Eq-hash table of [`Variable`] -> `(key . sym)`, recording the library
    /// instance and symbol each registered variable was exported under.
    origins: Gc<'gc, HashTable<'gc>>,
    /// Library key -> environment, owned by environments.scm.
    envs: Gc<'gc, HashTable<'gc>>,
    /// Library key -> `(meta . instance)` for libraries that have no environment yet,
    /// owned by environments.scm.
    lazy_envs: Gc<'gc, HashTable<'gc>>,
    /// The in-progress/done load state, owned by library-manager.scm.
    loading: Lock<Value<'gc>>,
    /// `#f` if the registry is sealed (an unknown library is an error), otherwise a
    /// procedure that loads the library of a key.
    resolver: Lock<Value<'gc>>,
}

fn registry_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::REGISTRY).expect("builtin class id is nonzero"),
    )
}

unsafe impl<'gc> Trace for Registry<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.instances.trace(visitor);
            self.pending.trace(visitor);
            self.origins.trace(visitor);
            self.envs.trace(visitor);
            self.lazy_envs.trace(visitor);
            self.loading.trace(visitor);
            self.resolver.trace(visitor);
        }
    }

    // SAFETY: The registry holds no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for Registry<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::REGISTRY];
    const TYPE_NAME: &'static str = "library-registry";
}

type RootedRegistry = crate::Rootable!(Gc<'_, Registry<'_>>);

/// The late-binding table, see [`late_variable`].
struct LateTable<'gc>(Gc<'gc, HashTable<'gc>>);

unsafe impl<'gc> Trace for LateTable<'gc> {
    // SAFETY: The only field is traced via `visitor`.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe { self.0.trace(visitor) }
    }

    // SAFETY: The table is strong, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

type RootedLateTable = crate::Rootable!(LateTable<'_>);

/// The process-wide root registry. Stored as a [`Global`](crate::heap::Global) root so the
/// collector keeps it and everything reachable from it alive. Set once by
/// [`init_linklets`].
static ROOT_REGISTRY: OnceLock<crate::heap::Global<RootedRegistry>> = OnceLock::new();

/// Eq-hash table of symbol -> [`Variable`] holding the bound variables of every `(boot ...)`
/// instance registered in the root registry. A boot linklet may use a name that a later boot
/// linklet defines without importing it; the expander compiles such a free reference to a
/// `late-variable` lookup, answered from this table, see [`late_variable`]. The Rust runtime
/// does not read it: it reaches Scheme through the hooks of `runtime/hooks.rs`.
static LATE: OnceLock<crate::heap::Global<RootedLateTable>> = OnceLock::new();

/// The root registry: the one the Rust runtime and the main namespace use.
pub fn root_registry<'gc>(ctx: MutationContext<'gc>) -> Gc<'gc, Registry<'gc>> {
    *ROOT_REGISTRY
        .get()
        .expect("linklet registry is initialized at runtime startup")
        .fetch(ctx)
}

fn registry_late<'gc>(ctx: MutationContext<'gc>) -> Gc<'gc, HashTable<'gc>> {
    LATE.get()
        .expect("linklet registry is initialized at runtime startup")
        .fetch(ctx)
        .0
}

/// Whether `key` names a boot library, `(boot ...)`.
fn is_boot_key(key: Value<'_>) -> bool {
    key.is_pair()
        && key.car().is::<Symbol>()
        && key.car().downcast::<Symbol>().to_string() == "boot"
}

impl<'gc> Registry<'gc> {
    /// Allocates an empty registry. With a `parent`, it starts out with `(#%capy)` and every
    /// `(boot ...)` instance of the parent registered, these being immutable core instances
    /// that every registry shares. `resolver` is `#f` for a sealed registry.
    pub fn new(
        ctx: MutationContext<'gc>,
        name: Value<'gc>,
        parent: Option<Gc<'gc, Registry<'gc>>>,
        resolver: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let reg = ctx.allocate_with(
            Self {
                name: Lock::new(name),
                instances: HashTable::new(ctx, HashTableType::Equal, 0, 0.75),
                pending: HashTable::new(ctx, HashTableType::Equal, 0, 0.75),
                origins: HashTable::new(ctx, HashTableType::Eq, 0, 0.75),
                envs: HashTable::new(ctx, HashTableType::Equal, 0, 0.75),
                lazy_envs: HashTable::new(ctx, HashTableType::Equal, 0, 0.75),
                loading: Lock::new(Value::new(false)),
                resolver: Lock::new(resolver),
            },
            registry_header_word(),
        );

        if let Some(parent) = parent {
            let capy = ctx.globals().capy_instance();
            registry_register(ctx, reg, capy.name(), capy);
            for key in parent.instances.keys().collect::<Vec<_>>() {
                if is_boot_key(key)
                    && let Some(inst) = registry_lookup(ctx, parent, key)
                {
                    registry_register(ctx, reg, key, inst);
                }
            }
        }
        reg
    }

    /// The registry's name.
    pub fn name(&self) -> Value<'gc> {
        self.name.get()
    }

    /// Eq-hash table of library key -> environment.
    pub fn envs(&self) -> Gc<'gc, HashTable<'gc>> {
        self.envs
    }

    /// Table of library key -> `(meta . instance)` for libraries without an environment yet.
    pub fn lazy_envs(&self) -> Gc<'gc, HashTable<'gc>> {
        self.lazy_envs
    }

    /// The load state owned by library-manager.scm.
    pub fn loading(&self) -> Value<'gc> {
        self.loading.get()
    }

    /// The library resolver, `#f` when the registry is sealed.
    pub fn resolver(&self) -> Value<'gc> {
        self.resolver.get()
    }
}

/// The `(key . sym)` a registered variable was exported under, if any.
pub fn variable_origin<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    var: Value<'gc>,
) -> Option<Value<'gc>> {
    reg.origins.get(ctx, var)
}

/// Finds the library instance registered under `key`.
pub fn registry_lookup<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    key: Value<'gc>,
) -> Option<Gc<'gc, Instance<'gc>>> {
    reg.instances.get(ctx, key).map(|inst| inst.downcast())
}

/// The registered library instance for `key`, or else the pending placeholder
/// for it, created on first use. The placeholder is empty and its variables are
/// created on demand by [`Instance::ensure_variable`]. The linklet for `key` takes
/// the placeholder over when it is instantiated.
pub fn registry_ensure_placeholder<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    key: Value<'gc>,
) -> Gc<'gc, Instance<'gc>> {
    if let Some(inst) = registry_lookup(ctx, reg, key) {
        return inst;
    }
    if let Some(inst) = reg.pending.get(ctx, key) {
        return inst.downcast();
    }
    let inst = Instance::new(ctx, key, false);
    reg.pending.put(ctx, key, inst);
    inst
}

/// Removes and returns the pending placeholder for `key`, if there is one.
pub fn registry_take_pending<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    key: Value<'gc>,
) -> Option<Gc<'gc, Instance<'gc>>> {
    let inst = reg.pending.get(ctx, key)?;
    reg.pending.remove(ctx, key);
    Some(inst.downcast())
}

/// Whether `inst` is the pending placeholder of its key, not a registered instance.
pub fn registry_is_pending<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    inst: Gc<'gc, Instance<'gc>>,
) -> bool {
    reg.pending
        .get(ctx, inst.name())
        .is_some_and(|pending| pending == Value::new(inst))
}

/// Registers `inst` as the library instance for `key`, replacing any previous entry.
///
/// Every variable of `inst` is also recorded with its origin, see [`variable_origin`].
/// Variables are only recorded once, the first registration of a variable wins.
/// A pending placeholder for `key` is dropped, `inst` replaces it.
pub fn registry_register<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    key: Value<'gc>,
    inst: Gc<'gc, Instance<'gc>>,
) {
    reg.pending.remove(ctx, key);
    reg.instances.put(ctx, key, inst);

    if is_boot_key(key) && Gc::ptr_eq(reg, root_registry(ctx)) {
        let late = registry_late(ctx);
        for sym in inst.vars.keys().collect::<Vec<_>>() {
            if let Some(var) = inst.variable(ctx, sym)
                && var.is_bound()
            {
                late.put(ctx, sym, var);
            }
        }
    }

    for sym in inst.vars.keys().collect::<Vec<_>>() {
        let Some(var) = inst.vars.get(ctx, sym) else {
            continue;
        };
        if reg.origins.get(ctx, var).is_none() {
            reg.origins.put(ctx, var, Value::cons(ctx, key, sym));
        }
    }
}

/// The variable a late-bound reference to `sym` sees: the bound exports of the boot linklets
/// first, then the native primitives of the `(#%capy)` instance. Unbound variables are
/// skipped, so a name that is declared but not yet defined is not found.
pub fn late_variable<'gc>(
    ctx: MutationContext<'gc>,
    sym: Value<'gc>,
) -> Option<Gc<'gc, Variable<'gc>>> {
    if let Some(var) = registry_late(ctx).get(ctx, sym) {
        let var = var.downcast::<Variable>();
        if var.is_bound() {
            return Some(var);
        }
    }
    ctx.globals()
        .capy_instance()
        .variable(ctx, sym)
        .filter(|var| var.is_bound())
}

/// Parses a space-separated library name such as `"capy syntax"` into the list of
/// symbols `(capy syntax)`.
pub fn library_name<'gc>(ctx: MutationContext<'gc>, name: &str) -> Value<'gc> {
    let mut result = Value::null();
    for part in name.split(' ').rev() {
        result = Value::cons(ctx, Symbol::from_str(ctx, part).into(), result);
    }
    result
}

/// Removes the registry entry for `key`, if present.
pub fn registry_remove<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    key: Value<'gc>,
) {
    reg.instances.remove(ctx, key);
}

/// The keys of every registered library instance, as a list.
pub fn registry_keys<'gc>(ctx: MutationContext<'gc>, reg: Gc<'gc, Registry<'gc>>) -> Value<'gc> {
    let keys = reg.instances.keys().collect::<Vec<_>>();
    Value::list_from_slice(ctx, keys)
}

/// Builds the slot vector for running `linklet` against the given imports and
/// target. Each import group `i` is resolved in `import_instances[i]`, then
/// every export symbol is ensured in `target`. Placeholders are the pending
/// instances of `reg`.
///
/// The result holds one [`Variable`] per slot, in the order the linklet body
/// expects. On failure the error is returned as a value: the imported symbol
/// that has no variable, or the linklet's name when `import_instances` does not
/// line up with the import groups. Callers should check the arity first.
pub fn instantiate_linklet<'gc>(
    ctx: MutationContext<'gc>,
    reg: Gc<'gc, Registry<'gc>>,
    linklet: Gc<'gc, Linklet<'gc>>,
    import_instances: &[Gc<'gc, Instance<'gc>>],
    target: Gc<'gc, Instance<'gc>>,
) -> Result<Vec<Value<'gc>>, Value<'gc>> {
    let mut slots = Vec::new();

    let mut groups = linklet.imports;
    let mut index = 0;
    while groups.is_pair() {
        let Some(inst) = import_instances.get(index) else {
            return Err(linklet.name);
        };

        // A placeholder creates the variables its importers ask for, the linklet
        // that defines them fills them in later.
        let placeholder = registry_is_pending(ctx, reg, *inst);
        let mut syms = groups.car().cdr();
        while syms.is_pair() {
            let sym = syms.car();
            let found = if placeholder {
                Some(inst.ensure_variable(ctx, sym))
            } else {
                inst.variable(ctx, sym)
            };
            let Some(var) = found else {
                return Err(sym);
            };
            slots.push(Value::new(var));
            syms = syms.cdr();
        }

        index += 1;
        groups = groups.cdr();
    }

    if index != import_instances.len() {
        return Err(linklet.name);
    }

    let mut exports = linklet.exports;
    while exports.is_pair() {
        let var = target.ensure_variable(ctx, exports.car());
        slots.push(Value::new(var));
        exports = exports.cdr();
    }

    Ok(slots)
}

/// The names of the `(#%capy)` variables that were bound when the runtime started: the native
/// procedures, before any boot code assigned a variable.
static NATIVE_PRIMITIVES: OnceLock<Vec<String>> = OnceLock::new();

/// Registers the `(#%capy)` library instance in the root registry, the one the native
/// primitives are defined in.
///
/// Call after all native primitives are installed.
pub fn install_capy_primitive_instance<'gc>(ctx: MutationContext<'gc>) {
    let inst = ctx.globals().capy_instance();
    // Boot files assign some of these variables (`(define (null? x) ...)`), so remember which
    // ones the runtime itself bound, see `native-primitive-names`.
    NATIVE_PRIMITIVES.get_or_init(|| {
        let mut names = inst
            .vars
            .keys()
            .filter(|sym| inst.variable(ctx, *sym).is_some_and(|var| var.is_bound()))
            .map(|sym| sym.to_string())
            .collect::<Vec<_>>();
        names.sort();
        names
    });
    registry_register(ctx, root_registry(ctx), inst.name(), inst);
}

/// Creates the root library registry and the late-binding table. Called once from
/// [`crate::runtime::init`].
pub fn init_linklets<'gc>(mc: MutationContext<'gc>) {
    let root = Registry::new(mc, Value::new(false), None, Value::new(false));
    let _ = ROOT_REGISTRY.set(crate::heap::Global::new(root));
    let _ = LATE.set(crate::heap::Global::new(LateTable(HashTable::new(
        mc,
        HashTableType::Eq,
        0,
        0.75,
    ))));
    linklet_ops::register(mc);
}

/// Converts a Scheme list of instances into a vector, reporting the first
/// element that is not an instance.
fn instance_list<'gc>(list: Value<'gc>) -> Result<Vec<Gc<'gc, Instance<'gc>>>, Value<'gc>> {
    let mut out = Vec::new();
    let mut ls = list;
    while ls.is_pair() {
        let item = ls.car();
        if !item.is::<Instance>() {
            return Err(item);
        }
        out.push(item.downcast());
        ls = ls.cdr();
    }
    Ok(out)
}

/// Number of `(key sym ...)` import groups in `imports`.
fn import_group_count(imports: Value<'_>) -> usize {
    let mut count = 0;
    let mut ls = imports;
    while ls.is_pair() {
        count += 1;
        ls = ls.cdr();
    }
    count
}

use crate::prelude::*;

#[scheme(path=capy)]
pub mod linklet_ops {
    #[scheme(name = "make-linklet-instance")]
    pub fn make_linklet_instance(
        name: Value<'gc>,
        mutable: Option<Value<'gc>>,
    ) -> Gc<'gc, Instance<'gc>> {
        let mutable = mutable.is_none_or(|v| v.as_bool());
        Instance::new(ctx, name, mutable)
    }

    /// An instance that shares the eq table `vars`
    #[scheme(name = "make-linklet-instance/vars")]
    pub fn make_linklet_instance_over_vars(
        name: Value<'gc>,
        vars: Gc<'gc, HashTable<'gc>>,
        mutable: Option<Value<'gc>>,
    ) -> Gc<'gc, Instance<'gc>> {
        let mutable = mutable.is_none_or(|v| v.as_bool());
        Instance::over_vars(ctx, name, vars, mutable)
    }

    #[scheme(name = "instance?")]
    pub fn instance_p(value: Value<'gc>) -> bool {
        value.is::<Instance>()
    }

    #[scheme(name = "instance-mutable?")]
    pub fn instance_mutable(inst: Gc<'gc, Instance<'gc>>) -> bool {
        inst.is_mutable()
    }

    #[scheme(name = "instance-name")]
    pub fn instance_name(inst: Gc<'gc, Instance<'gc>>) -> Value<'gc> {
        inst.name()
    }

    #[scheme(name = "instance-meta")]
    pub fn instance_meta(inst: Gc<'gc, Instance<'gc>>) -> Value<'gc> {
        inst.meta.get()
    }

    #[scheme(name = "set-instance-meta!")]
    pub fn set_instance_meta(inst: Gc<'gc, Instance<'gc>>, meta: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(ctx, inst), Instance, meta)
            .unlock()
            .set(meta);
        Value::undefined()
    }

    #[scheme(name = "instance-variable")]
    pub fn instance_variable(inst: Gc<'gc, Instance<'gc>>, sym: Value<'gc>) -> Value<'gc> {
        inst.variable(ctx, sym)
            .map(Value::new)
            .unwrap_or(Value::new(false))
    }

    #[scheme(name = "instance-ensure-variable!")]
    pub fn instance_ensure_variable(
        inst: Gc<'gc, Instance<'gc>>,
        sym: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        inst.ensure_variable(ctx, sym)
    }

    #[scheme(name = "instance-ref")]
    pub fn instance_ref(
        inst: Gc<'gc, Instance<'gc>>,
        sym: Value<'gc>,
        default: Option<Value<'gc>>,
    ) -> Value<'gc> {
        match inst.get(ctx, sym) {
            Some(value) => value,
            None => match default {
                Some(default) => default,
                None => call.raise_error("instance-ref", "unbound variable", &[sym]),
            },
        }
    }

    #[scheme(name = "instance-set!")]
    pub fn instance_set(
        inst: Gc<'gc, Instance<'gc>>,
        sym: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        inst.set(ctx, sym, value);
        Value::undefined()
    }

    #[scheme(name = "instance-syntax-ref")]
    pub fn instance_syntax_ref(inst: Gc<'gc, Instance<'gc>>, sym: Value<'gc>) -> Value<'gc> {
        inst.syntax_ref(ctx, sym).unwrap_or(Value::new(false))
    }

    #[scheme(name = "instance-syntax-set!")]
    pub fn instance_syntax_set(
        inst: Gc<'gc, Instance<'gc>>,
        sym: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        inst.set_syntax(ctx, sym, value);
        Value::undefined()
    }

    #[scheme(name = "instance-symbols")]
    pub fn instance_symbols(inst: Gc<'gc, Instance<'gc>>) -> Value<'gc> {
        let syms = inst.vars.keys().collect::<Vec<_>>();
        Value::list_from_slice(ctx, syms)
    }

    #[scheme(name = "instance-syntax-symbols")]
    pub fn instance_syntax_symbols(inst: Gc<'gc, Instance<'gc>>) -> Value<'gc> {
        let syms = inst.syntax.keys().collect::<Vec<_>>();
        Value::list_from_slice(ctx, syms)
    }

    #[scheme(name = "make-linklet")]
    pub fn make_linklet(
        name: Value<'gc>,
        imports: Value<'gc>,
        exports: Value<'gc>,
        body: Value<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Linklet<'gc>> {
        Linklet::new(ctx, name, imports, exports, body, meta)
    }

    #[scheme(name = "linklet?")]
    pub fn linklet_p(value: Value<'gc>) -> bool {
        value.is::<Linklet>()
    }

    #[scheme(name = "linklet-name")]
    pub fn linklet_name(linklet: Gc<'gc, Linklet<'gc>>) -> Value<'gc> {
        linklet.name
    }

    #[scheme(name = "linklet-imports")]
    pub fn linklet_imports(linklet: Gc<'gc, Linklet<'gc>>) -> Value<'gc> {
        linklet.imports
    }

    #[scheme(name = "linklet-exports")]
    pub fn linklet_exports(linklet: Gc<'gc, Linklet<'gc>>) -> Value<'gc> {
        linklet.exports
    }

    #[scheme(name = "linklet-body")]
    pub fn linklet_body(linklet: Gc<'gc, Linklet<'gc>>) -> Value<'gc> {
        linklet.body
    }

    #[scheme(name = "linklet-meta")]
    pub fn linklet_meta(linklet: Gc<'gc, Linklet<'gc>>) -> Value<'gc> {
        linklet.meta
    }

    /// Computes the slot variables for `linklet`, as a list. `import-instances`
    /// is a list with one instance per import group, `target` receives the exports.
    /// `reg` is the registry whose pending placeholders may appear among the imports.
    #[scheme(name = "linklet-slots")]
    pub fn linklet_slots(
        reg: Gc<'gc, Registry<'gc>>,
        linklet: Gc<'gc, Linklet<'gc>>,
        import_instances: Value<'gc>,
        target: Gc<'gc, Instance<'gc>>,
    ) -> Value<'gc> {
        let instances = match instance_list(import_instances) {
            Ok(instances) => instances,
            Err(bad) => {
                return call.wrong_argument_violation(
                    "linklet-slots",
                    "expected a list of instances",
                    Some(bad),
                    Some(3),
                    4,
                    &[reg.into(), linklet.into(), import_instances, target.into()],
                );
            }
        };

        let groups = import_group_count(linklet.imports);
        if groups != instances.len() {
            return call.raise_error(
                "linklet-slots",
                "import instance count does not match linklet imports",
                &[linklet.name],
            );
        }

        match instantiate_linklet(ctx, reg, linklet, &instances, target) {
            Ok(slots) => Value::list_from_slice(ctx, slots),
            Err(sym) => call.raise_error("linklet-slots", "unbound import", &[sym]),
        }
    }

    /// A new library registry. With a `parent` registry it starts out with `(#%capy)`
    /// and the parent's `(boot ...)` instances. `resolver` is `#f` for a sealed registry,
    /// otherwise a procedure taking a library key that loads the library.
    #[scheme(name = "make-library-registry")]
    pub fn make_library_registry(
        name: Value<'gc>,
        parent: Value<'gc>,
        resolver: Value<'gc>,
    ) -> Value<'gc> {
        let parent = if parent.is_bool() && !parent.as_bool() {
            None
        } else if parent.is::<Registry>() {
            Some(parent.downcast::<Registry>())
        } else {
            return call.wrong_argument_violation(
                "make-library-registry",
                "expected a library registry or #f",
                Some(parent),
                Some(1),
                3,
                &[name, parent, resolver],
            );
        };
        Value::new(Registry::new(ctx, name, parent, resolver))
    }

    #[scheme(name = "library-registry?")]
    pub fn library_registry_p(value: Value<'gc>) -> bool {
        value.is::<Registry>()
    }

    /// The process-wide root registry.
    #[scheme(name = "root-library-registry")]
    pub fn root_library_registry() -> Gc<'gc, Registry<'gc>> {
        root_registry(ctx)
    }

    #[scheme(name = "registry-name")]
    pub fn registry_name(reg: Gc<'gc, Registry<'gc>>) -> Value<'gc> {
        reg.name()
    }

    /// The table of library key -> environment, owned by environments.scm.
    #[scheme(name = "registry-envs")]
    pub fn registry_envs(reg: Gc<'gc, Registry<'gc>>) -> Gc<'gc, HashTable<'gc>> {
        reg.envs()
    }

    /// The table of library key -> `(meta . instance)` of libraries without an environment.
    #[scheme(name = "registry-lazy-envs")]
    pub fn registry_lazy_envs(reg: Gc<'gc, Registry<'gc>>) -> Gc<'gc, HashTable<'gc>> {
        reg.lazy_envs()
    }

    /// The load state owned by library-manager.scm, initially `#f`.
    #[scheme(name = "registry-loading")]
    pub fn registry_loading(reg: Gc<'gc, Registry<'gc>>) -> Value<'gc> {
        reg.loading()
    }

    #[scheme(name = "set-registry-loading!")]
    pub fn set_registry_loading(reg: Gc<'gc, Registry<'gc>>, state: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(ctx, reg), Registry, loading)
            .unlock()
            .set(state);
        Value::undefined()
    }

    /// The registry's resolver: `#f` when sealed, otherwise a procedure of a library key.
    #[scheme(name = "registry-resolver")]
    pub fn registry_resolver(reg: Gc<'gc, Registry<'gc>>) -> Value<'gc> {
        reg.resolver()
    }

    #[scheme(name = "set-registry-resolver!")]
    pub fn set_registry_resolver(reg: Gc<'gc, Registry<'gc>>, resolver: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(ctx, reg), Registry, resolver)
            .unlock()
            .set(resolver);
        Value::undefined()
    }

    #[scheme(name = "library-instance")]
    pub fn library_instance(reg: Gc<'gc, Registry<'gc>>, key: Value<'gc>) -> Value<'gc> {
        registry_lookup(ctx, reg, key)
            .map(Value::new)
            .unwrap_or(Value::new(false))
    }

    /// The registered instance for `key`, or its pending placeholder.
    #[scheme(name = "library-instance-or-placeholder")]
    pub fn library_instance_or_placeholder(
        reg: Gc<'gc, Registry<'gc>>,
        key: Value<'gc>,
    ) -> Gc<'gc, Instance<'gc>> {
        registry_ensure_placeholder(ctx, reg, key)
    }

    /// Removes the pending placeholder for `key` and returns it, or `#f`.
    #[scheme(name = "take-placeholder-instance!")]
    pub fn take_placeholder_instance(reg: Gc<'gc, Registry<'gc>>, key: Value<'gc>) -> Value<'gc> {
        registry_take_pending(ctx, reg, key)
            .map(Value::new)
            .unwrap_or(Value::new(false))
    }

    #[scheme(name = "register-library-instance!")]
    pub fn scheme_register_library_instance(
        reg: Gc<'gc, Registry<'gc>>,
        key: Value<'gc>,
        inst: Gc<'gc, Instance<'gc>>,
    ) -> Value<'gc> {
        registry_register(ctx, reg, key, inst);
        Value::undefined()
    }

    #[scheme(name = "unregister-library-instance!")]
    pub fn scheme_unregister_library_instance(
        reg: Gc<'gc, Registry<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        registry_remove(ctx, reg, key);
        Value::undefined()
    }

    /// The `(key . sym)` a variable was registered under, or `#f`.
    #[scheme(name = "variable-origin")]
    pub fn variable_origin_scheme(reg: Gc<'gc, Registry<'gc>>, var: Value<'gc>) -> Value<'gc> {
        super::variable_origin(ctx, reg, var).unwrap_or(Value::new(false))
    }

    #[scheme(name = "registered-library-keys")]
    pub fn registered_library_keys(reg: Gc<'gc, Registry<'gc>>) -> Value<'gc> {
        registry_keys(ctx, reg)
    }

    /// The names of the `(#%capy)` variables that hold a native procedure from the start.
    #[scheme(name = "native-primitive-names")]
    pub fn native_primitive_names() -> Value<'gc> {
        let names = NATIVE_PRIMITIVES.get().map(Vec::as_slice).unwrap_or(&[]);
        let syms = names
            .iter()
            .map(|name| Symbol::from_str(ctx, name).into())
            .collect::<Vec<Value<'gc>>>();
        Value::list_from_slice(ctx, syms)
    }
}
