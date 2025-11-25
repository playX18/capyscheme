use std::{cell::Cell, mem::offset_of, sync::atomic::AtomicUsize};

use crate::rsgc::{Gc, Trace, barrier, cell::Lock, sync::monitor::Monitor};

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

    Str::new(*ctx, result, true).into()
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Trace)]
#[collect(no_drop)]
pub enum ModuleKind {
    Directory,
    Interface,
    CustomInterface,
}

pub type ModuleRef<'gc> = Gc<'gc, Module<'gc>>;

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
    pub next_unique_id: AtomicUsize,
    pub replacements: Lock<Value<'gc>>,
    pub inlinable_exports: Lock<Value<'gc>>,
    pub environment: Lock<Value<'gc>>,
}

unsafe impl<'gc> Trace for Module<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        unsafe {
            self.obarray.trace(visitor);
            self.uses.trace(visitor);
            self.binder.trace(visitor);
            self.transformer.trace(visitor);
            self.name.trace(visitor);
            self.version.trace(visitor);
            self.import_obarray.trace(visitor);
            self.submodules.trace(visitor);
            self.filename.trace(visitor);
            self.public_interface.trace(visitor);
            self.replacements.trace(visitor);
            self.inlinable_exports.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Module<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        size: usize,
        uses: Value<'gc>,
        binder: impl IntoValue<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            *ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::MODULE.bits() as _),
                obarray: Lock::new(HashTable::new(*ctx, HashTableType::Eq, size, 0.75)),
                uses: Lock::new(uses),
                version: Lock::new(Value::null()),
                binder: binder.into_value(ctx),
                declarative: Cell::new(true),
                transformer: Value::new(false),
                name: Lock::new(Value::new(false)),
                kind: Cell::new(ModuleKind::Directory),
                import_obarray: HashTable::new(*ctx, HashTableType::Eq, 0, 0.75),
                submodules: HashTable::new(*ctx, HashTableType::Equal, 0, 0.75),
                filename: Lock::new(Value::new(false)),
                inlinable_exports: Lock::new(Value::new(false)),
                next_unique_id: AtomicUsize::new(0),
                public_interface: Lock::new(None),
                replacements: Lock::new(Value::new(HashTable::new(
                    *ctx,
                    HashTableType::Eq,
                    0,
                    0.75,
                ))),
                environment: Lock::new(Value::new(false)),
            },
        )
    }

    pub fn name(&self) -> Value<'gc> {
        self.name.get()
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

    pub fn get_str(&self, ctx: Context<'gc>, name: &str) -> Option<Value<'gc>> {
        self.variable(ctx, Symbol::from_str(ctx, name).into())
            .map(|var| var.value.get())
    }

    pub fn set(&self, ctx: Context<'gc>, name: Value<'gc>, value: Value<'gc>) -> bool {
        if let Some(var) = self.variable(ctx, name) {
            barrier::field!(Gc::write(*ctx, var), Variable, value)
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
        barrier::field!(Gc::write(*ctx, var), Variable, value)
            .unlock()
            .set(value);
        var
    }

    pub fn define_rs(
        &self,
        ctx: Context<'gc>,
        name: impl AsRef<str>,
        value: impl IntoValue<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        let sym = Symbol::from_str(ctx, name.as_ref());
        let val = value.into_value(ctx);
        self.define(ctx, sym.into(), val)
    }

    pub fn export_one(&self, ctx: Context<'gc>, name: Value<'gc>) {
        let public_i = self
            .public_interface
            .get()
            .expect("Module has no public interface");
        let var = self.ensure_local_variable(ctx, name);
        public_i.add(ctx, name, var);
    }

    /// Add `interface` to the list of interfaces used by `self`.
    pub fn use_iface(self: Gc<'gc, Self>, ctx: Context<'gc>, interface: Gc<'gc, Self>) {
        if Gc::ptr_eq(self, interface) || self.uses.get().memq(interface.into()) {
            return;
        }

        barrier::field!(Gc::write(*ctx, self), Self, uses)
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
                    if !var.is_bound() {
                        println!(
                            ";; Warning: imported unbound variable {sym} into {}",
                            self.name.get()
                        );
                    }

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
            let wi = Gc::write(*ctx, interface);
            barrier::field!(wi, Self, name)
                .unlock()
                .set(self.name.get());
            barrier::field!(wi, Self, version)
                .unlock()
                .set(self.version.get());
            barrier::field!(Gc::write(*ctx, self), Self, public_interface)
                .unlock()
                .set(Some(interface));
        }

        if self.uses.get().memq((ctx.globals().scm_module()).into())
            && !Gc::ptr_eq(self, ctx.globals().root_module())
        {
            self.use_iface(ctx, ctx.globals().scm_module());
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
                let wm = Gc::write(*ctx, m);
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

pub fn resolve_module<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    autoload: bool,
    ensure: bool,
) -> Option<Gc<'gc, Module<'gc>>> {
    if let Some(loaded) = ctx
        .globals()
        .loc_resolve_module_root()
        .get()
        .downcast::<Module>()
        .nested_ref_module(ctx, name)
        && (!autoload || loaded.public_interface.get().is_some())
    {
        return Some(loaded);
    }

    if ensure {
        Some(make_modules_in(
            ctx,
            ctx.globals().loc_resolve_module_root().get().downcast(),
            name,
        ))
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
        let wm = Gc::write(*ctx, m);
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
            *ctx,
            format!("Cannot create module with name {}", name),
            true,
        )
        .into());
    };

    module.beautify_user_module(ctx);
    let w = Gc::write(*ctx, module);
    barrier::field!(w, Module, filename).unlock().set(filename);
    module.export(ctx, exports);
    let _ = imports;
    Ok(module)
}

static IMPORT_OBARRAY_MUTEX: Monitor<()> = Monitor::new(());

unsafe impl<'gc> Tagged for Module<'gc> {
    const TC8: TypeCode8 = TypeCode8::MODULE;
    const TYPE_NAME: &'static str = "module";
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
            *ctx,
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
        barrier::field!(Gc::write(*ctx, self), Variable, value)
            .unlock()
            .set(value);
    }

    pub fn get(&self) -> Value<'gc> {
        self.value.get()
    }
}

unsafe impl<'gc> Tagged for Variable<'gc> {
    const TC8: TypeCode8 = TypeCode8::VARIABLE;
    const TYPE_NAME: &'static str = "variable";
}

fluid!(
    pub current_module = Value::new(false);
);

pub fn define<'gc>(
    ctx: Context<'gc>,
    name: &str,
    value: impl IntoValue<'gc>,
) -> Gc<'gc, Variable<'gc>> {
    let value = value.into_value(ctx);
    let sym = Symbol::from_str(ctx, name);
    current_module(ctx)
        .get(ctx)
        .downcast::<Module>()
        .define(ctx, sym.into(), value)
}

pub fn convert_module_name<'gc>(ctx: Context<'gc>, name: &str) -> Value<'gc> {
    let parts = name.split(' ').collect::<Vec<_>>();
    let mut result = Value::null();
    for part in parts.iter().rev() {
        result = Value::cons(ctx, Symbol::from_str(ctx, part).into(), result);
    }

    result
}

pub fn public_ref<'gc>(ctx: Context<'gc>, module_name: &str, name: &str) -> Option<Value<'gc>> {
    let module_name = convert_module_name(ctx, module_name);
    let module = resolve_module(ctx, module_name, false, false)?;
    module
        .public_interface
        .get()?
        .get(ctx, Symbol::from_str(ctx, name).into())
}

pub fn private_ref<'gc>(ctx: Context<'gc>, module_name: &str, name: &str) -> Option<Value<'gc>> {
    let module_name = convert_module_name(ctx, module_name);
    let module = resolve_module(ctx, module_name, false, false)?;
    module.get(ctx, Symbol::from_str(ctx, name).into())
}

use crate::prelude::*;

#[scheme(path=capy)]
pub mod module_ops {
    #[scheme(name = "variable-ref")]
    pub fn variable_ref(var: Gc<'gc, Variable<'gc>>) -> Value<'gc> {
        nctx.return_(var.get())
    }

    #[scheme(name = "variable-set!")]
    pub fn variable_set(var: Gc<'gc, Variable<'gc>>, value: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, var), Variable, value)
            .unlock()
            .set(value);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "make-variable")]
    pub fn make_variable(value: Option<Value<'gc>>) -> Gc<'gc, Variable<'gc>> {
        let value = value.unwrap_or(Value::undefined());
        let var = Variable::new(nctx.ctx, value);
        nctx.return_(var)
    }

    #[scheme(name = "variable-bound?")]
    pub fn variable_bound(var: Gc<'gc, Variable<'gc>>) -> bool {
        nctx.return_(var.is_bound())
    }

    #[scheme(name = "current-module")]
    pub fn current_module_fn(new_module: Option<Gc<'gc, Module<'gc>>>) -> Gc<'gc, Module<'gc>> {
        let fluid = current_module(nctx.ctx);
        let old = fluid.get(nctx.ctx);

        if let Some(new_module) = new_module {
            fluid.set(nctx.ctx, new_module.into());
        }

        nctx.return_(old.downcast())
    }

    #[scheme(name = "make-module")]
    pub fn make_module(size: Option<usize>, uses: Option<Value<'gc>>) -> Gc<'gc, Module<'gc>> {
        let size = size.unwrap_or(0);
        let uses = uses.unwrap_or(Value::null());
        let module = Module::new(nctx.ctx, size, uses, Value::new(false));
        nctx.return_(module)
    }

    #[scheme(name = "module?")]
    pub fn module_p(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<Module>())
    }

    #[scheme(name = "module-obarray")]
    pub fn module_obarray(module: Gc<'gc, Module<'gc>>) -> Gc<'gc, HashTable<'gc>> {
        nctx.return_(module.obarray.get())
    }

    #[scheme(name = "module-uses")]
    pub fn module_uses(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.uses.get())
    }

    #[scheme(name = "module-binder")]
    pub fn module_binder(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.binder)
    }

    #[scheme(name = "module-declarative?")]
    pub fn module_declarative(module: Gc<'gc, Module<'gc>>) -> bool {
        nctx.return_(module.declarative.get())
    }

    #[scheme(name = "module-transformer")]
    pub fn module_transformer(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.transformer)
    }
    #[scheme(name = "raw-module-name")]
    pub fn module_name(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.name.get())
    }

    #[scheme(name = "module-version")]
    pub fn module_version(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.version.get())
    }
    #[scheme(name = "module-kind")]
    pub fn module_kind(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        let kind = match module.kind.get() {
            ModuleKind::Directory => Symbol::from_str(nctx.ctx, "directory"),
            ModuleKind::Interface => Symbol::from_str(nctx.ctx, "interface"),
            ModuleKind::CustomInterface => Symbol::from_str(nctx.ctx, "custom-interface"),
        };
        nctx.return_(kind.into())
    }

    #[scheme(name = "module-import-obarray")]
    pub fn module_import_obarray(module: Gc<'gc, Module<'gc>>) -> Gc<'gc, HashTable<'gc>> {
        nctx.return_(module.import_obarray.clone())
    }

    #[scheme(name = "module-submodules")]
    pub fn module_submodules(module: Gc<'gc, Module<'gc>>) -> Gc<'gc, HashTable<'gc>> {
        nctx.return_(module.submodules.clone())
    }

    #[scheme(name = "module-filename")]
    pub fn module_filename(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.filename.get())
    }
    #[scheme(name = "module-public-interface")]
    pub fn module_public_interface(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(
            module
                .public_interface
                .get()
                .map(Value::new)
                .unwrap_or(Value::new(false)),
        )
    }

    #[scheme(name = "module-next-unique-id")]
    pub fn module_next_unique_id(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        let val = module
            .next_unique_id
            .load(std::sync::atomic::Ordering::SeqCst)
            .into_value(nctx.ctx);
        nctx.return_(val)
    }

    #[scheme(name = "module-replacements")]
    pub fn module_replacements(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.replacements.get())
    }

    #[scheme(name = "module-inlinable-exports")]
    pub fn module_inlinable_exports(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.inlinable_exports.get())
    }

    #[scheme(name = "set-module-obarray!")]
    pub fn set_module_obarray(
        module: Gc<'gc, Module<'gc>>,
        obarray: Gc<'gc, HashTable<'gc>>,
    ) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, obarray)
            .unlock()
            .set(obarray);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-uses!")]
    pub fn set_module_uses(module: Gc<'gc, Module<'gc>>, uses: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, uses)
            .unlock()
            .set(uses);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-declarative!")]
    pub fn set_module_declarative(module: Gc<'gc, Module<'gc>>, declarative: bool) -> Value<'gc> {
        module.declarative.set(declarative);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-name!")]
    pub fn set_module_name(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, name)
            .unlock()
            .set(name);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-version!")]
    pub fn set_module_version(module: Gc<'gc, Module<'gc>>, version: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, version)
            .unlock()
            .set(version);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-kind!")]
    pub fn set_module_kind(module: Gc<'gc, Module<'gc>>, kind: Gc<'gc, Symbol<'gc>>) -> Value<'gc> {
        let kind = match kind.to_string().as_str() {
            "directory" => ModuleKind::Directory,
            "interface" => ModuleKind::Interface,
            "custom-interface" => ModuleKind::CustomInterface,
            _ => {
                return nctx.wrong_argument_violation(
                    "set-module-kind!",
                    "expected 'directory, 'interface, or 'custom-interface",
                    Some(kind.into()),
                    Some(2),
                    2,
                    &[module.into(), kind.into()],
                );
            }
        };
        module.kind.set(kind);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-filename!")]
    pub fn set_module_filename(module: Gc<'gc, Module<'gc>>, filename: Value<'gc>) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, filename)
            .unlock()
            .set(filename);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-public-interface!")]
    pub fn set_module_public_interface(
        module: Gc<'gc, Module<'gc>>,
        public_interface: Option<Gc<'gc, Module<'gc>>>,
    ) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, public_interface)
            .unlock()
            .set(public_interface);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-next-unique-id!")]
    pub fn set_module_next_unique_id(
        module: Gc<'gc, Module<'gc>>,
        next_unique_id: usize,
    ) -> Value<'gc> {
        module
            .next_unique_id
            .store(next_unique_id, std::sync::atomic::Ordering::SeqCst);
        nctx.return_(Value::undefined())
    }
    #[scheme(name = "set-module-replacements!")]
    pub fn set_module_replacements(
        module: Gc<'gc, Module<'gc>>,
        replacements: Value<'gc>,
    ) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, replacements)
            .unlock()
            .set(replacements);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "set-module-inlinable-exports!")]
    pub fn set_module_inlinable_exports(
        module: Gc<'gc, Module<'gc>>,
        inlinable_exports: Value<'gc>,
    ) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, inlinable_exports)
            .unlock()
            .set(inlinable_exports);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "module-local-variable")]
    pub fn module_local_variable(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> Value<'gc> {
        let res = module
            .local_variable(nctx.ctx, name)
            .map(Value::new)
            .unwrap_or(Value::new(false));
        nctx.return_(res)
    }

    #[scheme(name = "module-locally-bound?")]
    pub fn module_locally_bound(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> bool {
        let res = module.is_locally_bound(nctx.ctx, name);
        nctx.return_(res)
    }

    #[scheme(name = "module-bound?")]
    pub fn module_bound(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> bool {
        let res = module.is_bound(nctx.ctx, name);
        nctx.return_(res)
    }

    #[scheme(name = "module-variable")]
    pub fn module_variable(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> Value<'gc> {
        let res = module
            .variable(nctx.ctx, name)
            .map(Value::new)
            .unwrap_or(Value::new(false));
        nctx.return_(res)
    }

    #[scheme(name = "module-symbol-binding")]
    pub fn module_symbol_binding(
        module: Gc<'gc, Module<'gc>>,
        name: Value<'gc>,
        opt_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let binding = module.binding(nctx.ctx, name);

        if binding.is_none() {
            if let Some(value) = opt_value {
                return nctx.return_(value);
            } else {
                return nctx.raise_error("module-binding", "unbound variable", &[name]);
            }
        }

        let binding = binding.unwrap();

        nctx.return_(binding)
    }

    #[scheme(name = "module-symbol-local-binding")]
    pub fn module_symbol_local_binding(
        module: Gc<'gc, Module<'gc>>,
        name: Value<'gc>,
        opt_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let binding = module.local_binding(nctx.ctx, name);

        if binding.is_none() {
            if let Some(value) = opt_value {
                return nctx.return_(value);
            } else {
                return nctx.raise_error(
                    "module-local-binding",
                    "locally unbound variable",
                    &[name],
                );
            }
        }

        let binding = binding.unwrap();

        nctx.return_(binding)
    }

    #[scheme(name = "module-make-local-var!")]
    pub fn module_make_local_var(
        module: Gc<'gc, Module<'gc>>,
        name: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        if let Some(var) = module.obarray.get().get(nctx.ctx, name) {
            if var.is::<Variable>() {
                return nctx.return_(var.downcast());
            }
        }

        let local_var = Variable::new(nctx.ctx, Value::undefined());
        module.add(nctx.ctx, name, local_var);
        nctx.return_(local_var)
    }

    #[scheme(name = "module-ensure-local-variable!")]
    pub fn module_ensure_local_variable(
        module: Gc<'gc, Module<'gc>>,
        name: Value<'gc>,
    ) -> Gc<'gc, Variable<'gc>> {
        let var = module.ensure_local_variable(nctx.ctx, name);
        nctx.return_(var)
    }

    #[scheme(name = "module-add!")]
    pub fn module_add(
        module: Gc<'gc, Module<'gc>>,
        name: Value<'gc>,
        var: Gc<'gc, Variable<'gc>>,
    ) -> Value<'gc> {
        module.add(nctx.ctx, name, var);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "module-remove!")]
    pub fn module_remove(module: Gc<'gc, Module<'gc>>, name: Value<'gc>) -> Value<'gc> {
        module.remove(nctx.ctx, name);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "module-clear!")]
    pub fn module_clear(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        module.clear(nctx.ctx);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "module-gensym")]
    pub fn module_gensym(
        id: Option<Gc<'gc, Str<'gc>>>,
        m: Option<Gc<'gc, Module<'gc>>>,
    ) -> Value<'gc> {
        let id = id
            .map(|x| x.to_string())
            .unwrap_or_else(|| " mg".to_owned());
        let m = m.unwrap_or_else(|| current_module(nctx.ctx).get(nctx.ctx).downcast());
        let unique_id = m
            .next_unique_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let sym = format!("{}-{}-{}", id, m.name.get().hash_equal(), unique_id);
        let str = Str::new(*nctx.ctx, sym, true);
        let sym = Symbol::gensym(nctx.ctx, Some(str));
        nctx.return_(sym.into())
    }

    #[scheme(name = "module-generate-unique-id!")]
    pub fn module_generate_unique_id(m: Option<Gc<'gc, Module<'gc>>>) -> usize {
        let m = m.unwrap_or_else(|| current_module(nctx.ctx).get(nctx.ctx).downcast());
        let unique_id = m
            .next_unique_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        nctx.return_(unique_id)
    }

    #[scheme(name = "module-environment")]
    pub fn module_environment(module: Gc<'gc, Module<'gc>>) -> Value<'gc> {
        nctx.return_(module.environment.get())
    }

    #[scheme(name = "set-module-environment!")]
    pub fn set_module_environment(
        module: Gc<'gc, Module<'gc>>,
        environment: Value<'gc>,
    ) -> Value<'gc> {
        barrier::field!(Gc::write(*nctx.ctx, module), Module, environment)
            .unlock()
            .set(environment);
        nctx.return_(Value::undefined())
    }
}

pub fn init_modules<'gc>(ctx: Context<'gc>) {
    module_ops::register(ctx);
}
