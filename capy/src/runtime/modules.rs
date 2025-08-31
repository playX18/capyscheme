use std::{cell::Cell, mem::offset_of};

use rsgc::{Gc, Trace, barrier, cell::Lock, sync::monitor::Monitor};

use crate::{
    fluid, global, list,
    runtime::{
        Context,
        value::{
            Boxed, HashTable, HashTableType, IntoValue, ScmHeader, Str, Symbol, Tagged, TypeCode8,
            Value,
        },
    },
};

pub fn mangle_library_spec<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> Value<'gc> {
    let mut spec = spec;

    let mut result = String::new();

    while !spec.is_null() {
        let next = spec.cdr();
        result.push_str(&spec.car().to_string());
        if next.is_null() {
            result.push(':');
        } else {
            result.push('.');
        }
        spec = next;
    }

    Str::new(&ctx, result, true).into()
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Trace)]
#[collect(no_drop)]
pub enum ModuleKind {
    Directory,
    Interface,
    CustomInterface,
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Module<'gc> {
    pub header: ScmHeader,
    pub obarray: Lock<Gc<'gc, HashTable<'gc>>>,
    pub uses: Lock<Value<'gc>>,
    pub binder: Value<'gc>,
    pub declarative: Cell<bool>,
    pub transformer: Value<'gc>,
    pub name: Lock<Value<'gc>>,
    pub version: Lock<Value<'gc>>,
    pub kind: Cell<ModuleKind>,
    pub import_obarray: Gc<'gc, HashTable<'gc>>,
    pub submodules: Gc<'gc, HashTable<'gc>>,
    pub filename: Lock<Value<'gc>>,
    pub public_interface: Lock<Option<Gc<'gc, Self>>>,
    pub next_unique_id: Value<'gc>,
    pub replacements: Value<'gc>,
    pub inlinable_exports: Value<'gc>,
}

impl<'gc> Module<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        size: usize,
        uses: Value<'gc>,
        binder: impl IntoValue<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::MODULE.bits() as _),
                obarray: Lock::new(HashTable::new(&ctx, HashTableType::Eq, size, 0.75)),
                uses: Lock::new(uses),
                version: Lock::new(Value::null()),
                binder: binder.into_value(ctx),
                declarative: Cell::new(false),
                transformer: Value::new(false),
                name: Lock::new(Value::new(false)),
                kind: Cell::new(ModuleKind::Directory),
                import_obarray: HashTable::new(&ctx, HashTableType::Eq, 0, 0.75),
                submodules: HashTable::new(&ctx, HashTableType::Eq, 0, 0.75),
                filename: Lock::new(Value::new(false)),
                inlinable_exports: Value::new(false),
                next_unique_id: Value::new(0i32),
                public_interface: Lock::new(None),
                replacements: Value::new(false),
            },
        )
    }

    pub fn search<F, R>(self: Gc<'gc, Self>, f: F) -> Option<R>
    where
        F: Fn(Gc<'gc, Self>) -> Option<R>,
    {
        if let Some(result) = f(self) {
            return Some(result);
        }

        let mut uses = self.uses.get();

        while !uses.is_null() {
            let module = uses.car().downcast::<Module>();
            if let Some(result) = module.search(&f) {
                return Some(result);
            }

            uses = uses.cdr();
        }

        None
    }

    pub fn is_locally_bound(&self, ctx: Context<'gc>, sym: Value<'gc>) -> bool {
        self.local_variable(ctx, sym)
            .map_or(false, |v| v.is_bound())
    }

    pub fn is_bound(&self, ctx: Context<'gc>, sym: Value<'gc>) -> bool {
        self.variable(ctx, sym).map_or(false, |v| v.is_bound())
    }

    pub fn local_binding(&self, ctx: Context<'gc>, sym: Value<'gc>) -> Option<Value<'gc>> {
        self.local_variable(ctx, sym).map(|v| v.value.get())
    }

    pub fn binding(&self, ctx: Context<'gc>, sym: Value<'gc>) -> Option<Value<'gc>> {
        self.variable(ctx, sym).map(|v| v.value.get())
    }

    pub fn add(&self, ctx: Context<'gc>, sym: Value<'gc>, var: Gc<'gc, Variable<'gc>>) {
        self.obarray.get().put(ctx, sym, var);
    }

    pub fn ensure_local_variable(
        &self,
        ctx: Context<'gc>,
        sym: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        if let Some(var) = self.local_variable(ctx, sym) {
            return var;
        }

        let var = Variable::new(ctx, Value::undefined());
        self.obarray.get().put(ctx, sym, var);
        var
    }

    pub fn remove(&self, ctx: Context<'gc>, sym: Value<'gc>) {
        self.obarray.get().remove(ctx, sym);
    }

    pub fn clear(&self, ctx: Context<'gc>) {
        self.obarray.get().clear(ctx);
    }

    pub fn ref_submodule(
        &self,
        ctx: Context<'gc>,
        name: Value<'gc>,
    ) -> Option<Gc<'gc, Module<'gc>>> {
        self.submodules.get(ctx, name).map(|m| m.downcast())
    }

    pub fn define_submodule(
        &self,
        ctx: Context<'gc>,
        name: Value<'gc>,
        module: Gc<'gc, Module<'gc>>,
    ) {
        self.submodules.put(ctx, name, module);
    }

    pub fn get(&self, ctx: Context<'gc>, name: Value<'gc>) -> Option<Value<'gc>> {
        self.variable(ctx, name).map(|var| var.value.get())
    }

    pub fn set(&self, ctx: Context<'gc>, name: Value<'gc>, value: Value<'gc>) -> bool {
        if let Some(var) = self.variable(ctx, name) {
            barrier::field!(Gc::write(&ctx, var), Variable, value)
                .unlock()
                .set(value);
            true
        } else {
            false
        }
    }

    pub fn define(
        &self,
        ctx: Context<'gc>,
        name: Value<'gc>,
        value: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        let var = self.ensure_local_variable(ctx, name);
        barrier::field!(Gc::write(&ctx, var), Variable, value)
            .unlock()
            .set(value);
        var
    }

    /// Add `interface` to the list of interfaces used by `self`.
    pub fn use_iface(self: Gc<'gc, Self>, ctx: Context<'gc>, interface: Gc<'gc, Self>) {
        if Gc::ptr_eq(self, interface) || self.uses.get().memq(interface.into()) {
            return;
        }

        barrier::field!(Gc::write(&ctx, self), Self, uses)
            .unlock()
            .set(
                self.uses
                    .get()
                    .append(ctx, Value::cons(ctx, interface.into(), Value::null())),
            );
        self.import_obarray.clear(ctx);
    }

    pub fn variable(&self, ctx: Context<'gc>, sym: Value<'gc>) -> Option<Gc<'gc, Variable<'gc>>> {
        if let Some(var) = self.obarray.get().get(ctx, sym) {
            return Some(var.downcast());
        }

        self.imported_variable(ctx, sym)
    }

    pub fn imported_variable(
        &self,
        ctx: Context<'gc>,
        sym: Value<'gc>,
    ) -> Option<Gc<'gc, Variable<'gc>>> {
        let guard = IMPORT_OBARRAY_MUTEX.lock();
        let var = self.import_obarray.get(ctx, sym);
        drop(guard);

        if let Some(var) = var {
            return Some(var.downcast());
        }

        {
            let mut uses = self.uses.get();

            while uses.is_pair() {
                let iface = uses.car().downcast::<Module>();
                let var = iface.variable(ctx, sym);
                if let Some(var) = var {
                    self.import_obarray.put(ctx, sym, var);
                    return Some(var);
                }

                uses = uses.cdr();
            }
        }

        None
    }

    pub fn local_variable(
        &self,
        ctx: Context<'gc>,
        sym: Value<'gc>,
    ) -> Option<Gc<'gc, Variable<'gc>>> {
        if let Some(var) = self.obarray.get().get(ctx, sym) {
            return Some(var.downcast());
        }

        None
    }

    pub fn import_interface(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        sym: Value<'gc>,
    ) -> Option<Gc<'gc, Self>> {
        let Some(var) = self.variable(ctx, sym) else {
            return None;
        };

        if let Some(local_var) = self.obarray.get().get(ctx, sym)
            && Gc::ptr_eq(var, local_var.downcast())
        {
            return Some(self);
        }

        let mut uses = self.uses.get();

        while uses.is_pair() {
            if let Some(imported_var) = uses.car().downcast::<Module>().variable(ctx, sym)
                && Gc::ptr_eq(var, imported_var)
            {
                return Some(uses.car().downcast());
            }

            uses = uses.cdr();
        }

        None
    }

    pub fn beautify_user_module(self: Gc<'gc, Self>, ctx: Context<'gc>) {
        let interface = self.public_interface.get();

        if interface.map_or(true, |iface| Gc::ptr_eq(iface, self)) {
            let interface = Self::new(ctx, 0, Value::null(), Value::new(false));
            let wi = Gc::write(&ctx, interface);
            barrier::field!(wi, Self, name)
                .unlock()
                .set(self.name.get());
            barrier::field!(wi, Self, version)
                .unlock()
                .set(self.version.get());
            barrier::field!(Gc::write(&ctx, self), Self, public_interface)
                .unlock()
                .set(Some(interface));
        }

        if self.uses.get().memq((*scm_module(ctx)).into()) && !Gc::ptr_eq(self, *root_module(ctx)) {
            self.use_iface(ctx, *scm_module(ctx));
        }
    }

    pub fn export(self: Gc<'gc, Self>, ctx: Context<'gc>, names: Value<'gc>) {
        let public_i = self.public_interface.get().unwrap_or(self);

        let mut ls = names;
        while ls.is_pair() {
            let name = ls.car();
            ls = ls.cdr();

            let internal_name = if name.is_pair() { name.car() } else { name };

            let external_name = if name.is_pair() { name.cdr() } else { name };

            let var = self.ensure_local_variable(ctx, internal_name);
            public_i.add(ctx, external_name, var);
        }
    }

    pub fn nested_ref(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        names: Value<'gc>,
    ) -> Option<Value<'gc>> {
        if names.is_null() {
            return Some(self.into());
        }

        let mut cur = self;
        let mut head = names.car();
        let mut tail = names.cdr();

        loop {
            if tail.is_null() {
                return cur.get(ctx, head);
            }

            cur = cur.ref_submodule(ctx, head)?;
            head = tail.car();
            tail = tail.cdr();
        }
    }

    pub fn nested_set(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        names: Value<'gc>,
        value: Value<'gc>,
    ) -> bool {
        if !names.is_pair() {
            return false;
        }
        let mut cur = self;
        let mut head = names.car();
        let mut tail = names.cdr();

        loop {
            if tail.is_null() {
                return cur.set(ctx, head, value);
            }

            let Some(mcur) = cur.ref_submodule(ctx, head) else {
                return false;
            };
            cur = mcur;
            head = tail.car();
            tail = tail.cdr();
        }
    }

    pub fn nested_define(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        names: Value<'gc>,
        value: Value<'gc>,
    ) -> bool {
        if !names.is_pair() {
            return false;
        }
        let mut cur = self;
        let mut head = names.car();
        let mut tail = names.cdr();

        loop {
            if tail.is_null() {
                cur.define(ctx, head, value);
                return true;
            }

            let Some(mcur) = cur.ref_submodule(ctx, head) else {
                return false;
            };
            cur = mcur;
            head = tail.car();
            tail = tail.cdr();
        }
    }

    pub fn nested_remove(self: Gc<'gc, Self>, ctx: Context<'gc>, names: Value<'gc>) -> bool {
        if !names.is_pair() {
            return false;
        }
        let mut cur = self;
        let mut head = names.car();
        let mut tail = names.cdr();

        loop {
            if tail.is_null() {
                cur.remove(ctx, head);
                return true;
            }

            let Some(mcur) = cur.ref_submodule(ctx, head) else {
                return false;
            };
            cur = mcur;
            head = tail.car();
            tail = tail.cdr();
        }
    }

    pub fn nested_ref_module(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        names: Value<'gc>,
    ) -> Option<Gc<'gc, Self>> {
        let mut names = names;
        let mut cur = self;

        loop {
            if names.is_null() {
                return Some(cur);
            }

            cur = cur.ref_submodule(ctx, names.car())?;
            names = names.cdr();
        }
    }

    pub fn nested_define_module(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        names: Value<'gc>,
        module: Gc<'gc, Self>,
    ) -> bool {
        if names.is_null() {
            return false;
        }

        let mut cur = self;
        let mut head = names.car();
        let mut tail = names.cdr();

        loop {
            if tail.is_null() {
                cur.define_submodule(ctx, head, module);
                return true;
            }

            cur = cur.ref_submodule(ctx, head).unwrap_or_else(|| {
                let m = Module::new(ctx, 0, Value::null(), Value::new(false));
                m.kind.set(ModuleKind::Directory);
                let wm = Gc::write(&ctx, m);
                barrier::field!(wm, Module, name)
                    .unlock()
                    .set(cur.name.get().append(ctx, list!(ctx, head)));
                cur.define_submodule(ctx, head, m);
                m
            });

            head = tail.car();
            tail = tail.cdr();
        }
    }

    pub fn local_ref(ctx: Context<'gc>, name: Value<'gc>) -> Option<Value<'gc>> {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_ref(ctx, name)
    }

    pub fn local_set(ctx: Context<'gc>, name: Value<'gc>, value: Value<'gc>) -> bool {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_set(ctx, name, value)
    }

    pub fn local_define(ctx: Context<'gc>, name: Value<'gc>, value: Value<'gc>) -> bool {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_define(ctx, name, value)
    }

    pub fn local_remove(ctx: Context<'gc>, name: Value<'gc>) -> bool {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_remove(ctx, name)
    }

    pub fn local_ref_module(ctx: Context<'gc>, name: Value<'gc>) -> Option<Gc<'gc, Module<'gc>>> {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_ref_module(ctx, name)
    }

    pub fn local_define_module(
        ctx: Context<'gc>,
        name: Value<'gc>,
        module: Gc<'gc, Module<'gc>>,
    ) -> bool {
        current_module(ctx)
            .get(ctx)
            .downcast::<Module>()
            .nested_define_module(ctx, name, module)
    }
}

global!(
    resolve_module_root<'gc>: Gc<'gc, Module<'gc>> = (ctx) {
        let root = Module::new(ctx, 0, Value::null(), Value::new(false));

        root.define_submodule(ctx, Symbol::from_str(ctx, "capy").into(), *root_module(ctx));
        root
    };
);

pub fn resolve_module<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    autoload: bool,
    ensure: bool,
) -> Option<Gc<'gc, Module<'gc>>> {
    if let Some(loaded) = resolve_module_root(ctx).nested_ref_module(ctx, name) {
        if autoload {
            return Some(loaded);
        }

        return loaded.public_interface.get();
    }

    if ensure {
        Some(make_modules_in(ctx, *resolve_module_root(ctx), name))
    } else {
        None
    }
}

pub fn make_modules_in<'gc>(
    ctx: Context<'gc>,
    module: Gc<'gc, Module<'gc>>,
    name: Value<'gc>,
) -> Gc<'gc, Module<'gc>> {
    module.nested_ref_module(ctx, name).unwrap_or_else(|| {
        let m = Module::new(ctx, 0, Value::null(), Value::new(false));
        let wm = Gc::write(&ctx, m);
        wm.kind.set(ModuleKind::Directory);
        barrier::field!(wm, Module, name)
            .unlock()
            .set(m.name.get().append(ctx, name));

        module.nested_define_module(ctx, name, m);
        m
    })
}

pub fn define_module<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    filename: Value<'gc>,
    imports: Value<'gc>,
    exports: Value<'gc>,
) -> Result<Gc<'gc, Module<'gc>>, Value<'gc>> {
    let Some(module) = resolve_module(ctx, name, false, true) else {
        return Err(Str::new(
            &ctx,
            format!("Cannot create module with name {}", name),
            true,
        )
        .into());
    };

    module.beautify_user_module(ctx);
    let w = Gc::write(&ctx, module);
    barrier::field!(w, Module, filename).unlock().set(filename);
    module.export(ctx, exports);
    let _ = imports;
    Ok(module)
}

static IMPORT_OBARRAY_MUTEX: Monitor<()> = Monitor::new(());

unsafe impl<'gc> Tagged for Module<'gc> {
    const TC8: TypeCode8 = TypeCode8::MODULE;
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Variable<'gc> {
    pub header: ScmHeader,
    pub value: Lock<Value<'gc>>,
}

const _: () = {
    assert!(offset_of!(Variable, value) == offset_of!(Boxed, val));
};

impl<'gc> Variable<'gc> {
    pub fn new(ctx: Context<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::VARIABLE.bits() as _),
                value: Lock::new(value),
            },
        )
    }

    pub fn is_bound(&self) -> bool {
        self.value.get() != Value::undefined()
    }

    pub fn set(self: Gc<'gc, Self>, ctx: Context<'gc>, value: Value<'gc>) {
        barrier::field!(Gc::write(&ctx, self), Variable, value)
            .unlock()
            .set(value);
    }

    pub fn get(&self) -> Value<'gc> {
        self.value.get()
    }
}

unsafe impl<'gc> Tagged for Variable<'gc> {
    const TC8: TypeCode8 = TypeCode8::VARIABLE;
}

global!(
    pub root_module<'gc>: Gc<'gc, Module<'gc>> = (ctx) {
        let m = Module::new(ctx, 0, Value::null(), Value::new(false));
        let wm = Gc::write(&ctx, m);
        barrier::field!(wm, Module, name).unlock().set(Value::cons(ctx, Symbol::from_str(ctx, "capy").into(), Value::null()));
        barrier::field!(wm, Module, obarray).unlock().set(scm_module(ctx).obarray.get());
        barrier::field!(wm, Module, public_interface).unlock().set(Some(*scm_module(ctx)));
        m
    };

    pub scm_module<'gc>: Gc<'gc, Module<'gc>> = (ctx) {
        let m = Module::new(ctx, 0, Value::null(), Value::new(false));
        let wm = Gc::write(&ctx, m);
        barrier::field!(wm, Module, name).unlock().set(Value::cons(ctx, Symbol::from_str(ctx, "capy").into(), Value::null()));
        m.kind.set(ModuleKind::Interface);
        barrier::field!(wm, Module, public_interface).unlock().set(Some(m));
        m
    };
);

fluid!(
    pub current_module = Value::new(false);
);

pub fn define<'gc>(ctx: Context<'gc>, name: &str, value: Value<'gc>) -> Gc<'gc, Variable<'gc>> {
    current_module(ctx).get(ctx).downcast::<Module>().define(
        ctx,
        Symbol::from_str(ctx, name).into(),
        value,
    )
}
