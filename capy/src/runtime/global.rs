use std::{cell::Cell, marker::PhantomData, sync::OnceLock};

use rsgc::{Gc, Root, Rootable, Trace, Visitor, barrier, sync::monitor::Monitor};

use crate::{
    expander::primitives::for_each_prim_name,
    prelude::{
        FromValue, HashTable, HashTableRef, HashTableType, IntoValue, Module, ModuleRef, Value,
        VariableRef,
    },
    runtime::{
        Context,
        modules::{ModuleKind, define},
    },
};

pub trait GlobalValue<'gc>: Trace + IntoValue<'gc> {}

type GlobalMap<'gc> = Vec<Value<'gc>>;

/// A table listing all global variables in the runtime.
///
/// Implemented as a vector of [`Value`]s, indexed by [`Global`]s which allows
/// us to serialize and deserialize it in a straightforward manner.
pub struct GlobalTable<'gc> {
    // TODO: Allow removing globals? Use a slab or similar data structure?
    globals: Monitor<GlobalMap<'gc>>,
}

unsafe impl<'gc> Trace for GlobalTable<'gc> {
    unsafe fn trace(&mut self, tracer: &mut Visitor<'_>) {
        for value in self.globals.get_mut().iter_mut() {
            tracer.trace(value);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Send for GlobalTable<'_> {}
unsafe impl Sync for GlobalTable<'_> {}

static GLOBALS: OnceLock<rsgc::Global<Rootable!(GlobalTable<'_>)>> = OnceLock::new();

/// The main difference between [`rsgc::Global`] and this type
/// is that this type does not store pointer, but just an index into
/// the side global table. Global table itself stores [`Value`]s directly
/// and `Global` provides typed access to them.
pub struct Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    index: usize,
    marker: PhantomData<*const R>,
}

pub fn globals<'gc>() -> &'static rsgc::Global<Rootable!(GlobalTable<'_>)> {
    GLOBALS.get_or_init(|| {
        rsgc::Global::new(GlobalTable {
            globals: Monitor::new(Vec::with_capacity(128)),
        })
    })
}

/// Set the global table to the given vector of values.
///
/// # Safety
///
/// While this function is not unsafe per-se, it can lead to wrong program
/// behaviour if it sets existing globals to incorrect indices.
pub unsafe fn globals_from_vec<'gc>(_ctx: impl AsRef<Context<'gc>>, vec: Vec<Value<'gc>>) {
    GLOBALS
        .set(rsgc::Global::new(GlobalTable {
            globals: Monitor::new(vec),
        }))
        .unwrap_or_else(|_| panic!("globals have already been initialized"));
}

pub fn for_each_global<'gc, F>(ctx: impl AsRef<Context<'gc>>, mut f: F)
where
    F: FnMut(usize, Value<'gc>),
{
    let ctx = ctx.as_ref();
    let globals = globals().fetch(ctx);
    let guard = globals.globals.lock();
    for (i, value) in guard.iter().enumerate() {
        f(i, *value);
    }
}

impl<R> Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    /// Create a new global variable initialized to `initial`.
    pub fn new<'gc>(ctx: &Context<'gc>, initial: Root<'gc, R>) -> Self
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        let index = guard.len();
        guard.push(initial.into_value(*ctx));
        Self {
            index,
            marker: PhantomData,
        }
    }

    /// Get the value of the global variable.
    pub fn fetch<'gc>(&self, ctx: &Context<'gc>) -> Root<'gc, R>
    where
        for<'a> Root<'a, R>: FromValue<'a>,
    {
        let globals = globals().fetch(&ctx);
        let guard = globals.globals.lock();
        let value = guard[self.index];
        Root::<R>::try_from_value(*ctx, value).expect("global variable has wrong type")
    }

    /// Set the value of the global variable.
    pub fn set<'gc>(&self, ctx: impl AsRef<Context<'gc>>, value: Root<'gc, R>)
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let ctx = ctx.as_ref();
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        guard[self.index] = value.into_value(*ctx);
    }

    pub unsafe fn from_index(index: usize) -> Self {
        Self {
            index,
            marker: PhantomData,
        }
    }
}

unsafe impl<R> Send for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

unsafe impl<R> Sync for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

#[macro_export]
macro_rules! global {
    ($($v: vis $name: ident <$l: lifetime>: $t: ty = ($ctx: ident) $init: expr;)*) => {
        $(
            paste::paste!{
                #[allow(unused)]
                #[unsafe(export_name = concat!("CAPY_GLOBAL_", stringify!($name)))]
                $v static [<$name: upper>]: std::sync::OnceLock<$crate::prelude::Global<$crate::rsgc::Rootable!($l => $t)>> = std::sync::OnceLock::new();

                #[allow(unused)]
                #[unsafe(export_name = concat!("capy_global_", stringify!($name)))]
                $v fn [<$name: lower>]<$l>($ctx: $crate::runtime::Context<$l>) -> $t {
                   [<$name: upper>]
                        .get_or_init(|| {
                            let init: $t = $init;
                            $crate::prelude::Global::new(&$ctx, init)
                        }).fetch(&$ctx)
                }


                #[allow(unused)]
                #[doc(hidden)]
                pub unsafe fn [<$name: lower _from_index>](index: usize) -> $crate::prelude::Global<$crate::rsgc::Rootable!($l => $t)> {
                    unsafe {
                        $crate::prelude::Global::from_index(index)
                    }
                }


            }
        )*
    };
}

#[derive(Trace)]
pub struct Globals<'gc> {
    pub capy_module_name: Cell<Value<'gc>>,

    // HashTableRef
    pub interesting_primitive_vars: Cell<Value<'gc>>,
    // VariableRef
    pub interesting_primitive_vars_loc: Cell<Value<'gc>>,
    pub empty_wrap: Cell<Value<'gc>>,
    // ModuleRef
    pub resolve_module_root: Cell<Value<'gc>>,

    // ModuleRef
    pub root_module: Cell<Value<'gc>>,
    pub scm_module: Cell<Value<'gc>>,

    pub loc_resolve_module_root: Cell<Value<'gc>>,
    pub loc_load_path: Cell<Value<'gc>>,
    pub loc_load_extensions: Cell<Value<'gc>>,
    pub loc_load_compiled_path: Cell<Value<'gc>>,
    pub loc_load_compiled_extensions: Cell<Value<'gc>>,
    pub loc_native_extension: Cell<Value<'gc>>,
    pub loc_compile_fallback_path: Cell<Value<'gc>>,
    pub loc_capy_root: Cell<Value<'gc>>,
    pub loc_fresh_auto_compile: Cell<Value<'gc>>,
    pub loc_the_root_module: Cell<Value<'gc>>,
    pub loc_the_scm_module: Cell<Value<'gc>>,
    pub loc_term_type: Cell<Value<'gc>>,
    pub loc_lref_type: Cell<Value<'gc>>,
    pub loc_lset_type: Cell<Value<'gc>>,
    pub loc_let_type: Cell<Value<'gc>>,
    pub loc_fix_type: Cell<Value<'gc>>,
    pub loc_module_ref_type: Cell<Value<'gc>>,
    pub loc_module_set_type: Cell<Value<'gc>>,
    pub loc_toplevel_ref_type: Cell<Value<'gc>>,
    pub loc_toplevel_set_type: Cell<Value<'gc>>,
    pub loc_toplevel_define_type: Cell<Value<'gc>>,
    pub loc_if_type: Cell<Value<'gc>>,
    pub loc_receive_type: Cell<Value<'gc>>,
    pub loc_application_type: Cell<Value<'gc>>,
    pub loc_primcall_type: Cell<Value<'gc>>,
    pub loc_primref_type: Cell<Value<'gc>>,
    pub loc_constant_type: Cell<Value<'gc>>,
    pub loc_void_type: Cell<Value<'gc>>,
    pub loc_values_type: Cell<Value<'gc>>,
    pub loc_sequence_type: Cell<Value<'gc>>,
    pub loc_proc_type: Cell<Value<'gc>>,
    pub loc_wcm_type: Cell<Value<'gc>>,
}

impl<'gc> Globals<'gc> {
    pub fn loc_resolve_module_root(&self) -> VariableRef<'gc> {
        self.loc_resolve_module_root.get().downcast()
    }

    pub fn loc_load_path(&self) -> VariableRef<'gc> {
        self.loc_load_path.get().downcast()
    }

    pub fn loc_load_extensions(&self) -> VariableRef<'gc> {
        self.loc_load_extensions.get().downcast()
    }

    pub fn loc_load_compiled_path(&self) -> VariableRef<'gc> {
        self.loc_load_compiled_path.get().downcast()
    }

    pub fn loc_load_compiled_extensions(&self) -> VariableRef<'gc> {
        self.loc_load_compiled_extensions.get().downcast()
    }

    pub fn loc_native_extension(&self) -> VariableRef<'gc> {
        self.loc_native_extension.get().downcast()
    }

    pub fn loc_compile_fallback_path(&self) -> VariableRef<'gc> {
        self.loc_compile_fallback_path.get().downcast()
    }

    pub fn loc_capy_root(&self) -> VariableRef<'gc> {
        self.loc_capy_root.get().downcast()
    }

    pub fn loc_fresh_auto_compile(&self) -> VariableRef<'gc> {
        self.loc_fresh_auto_compile.get().downcast()
    }

    pub fn loc_the_root_module(&self) -> VariableRef<'gc> {
        self.loc_the_root_module.get().downcast()
    }

    pub fn loc_the_scm_module(&self) -> VariableRef<'gc> {
        self.loc_the_scm_module.get().downcast()
    }

    pub fn loc_term_type(&self) -> VariableRef<'gc> {
        self.loc_term_type.get().downcast()
    }

    pub fn loc_lref_type(&self) -> VariableRef<'gc> {
        self.loc_lref_type.get().downcast()
    }

    pub fn loc_lset_type(&self) -> VariableRef<'gc> {
        self.loc_lset_type.get().downcast()
    }

    pub fn loc_let_type(&self) -> VariableRef<'gc> {
        self.loc_let_type.get().downcast()
    }

    pub fn loc_fix_type(&self) -> VariableRef<'gc> {
        self.loc_fix_type.get().downcast()
    }

    pub fn loc_module_ref_type(&self) -> VariableRef<'gc> {
        self.loc_module_ref_type.get().downcast()
    }

    pub fn loc_module_set_type(&self) -> VariableRef<'gc> {
        self.loc_module_set_type.get().downcast()
    }

    pub fn loc_toplevel_ref_type(&self) -> VariableRef<'gc> {
        self.loc_toplevel_ref_type.get().downcast()
    }

    pub fn loc_toplevel_set_type(&self) -> VariableRef<'gc> {
        self.loc_toplevel_set_type.get().downcast()
    }

    pub fn loc_toplevel_define_type(&self) -> VariableRef<'gc> {
        self.loc_toplevel_define_type.get().downcast()
    }

    pub fn loc_if_type(&self) -> VariableRef<'gc> {
        self.loc_if_type.get().downcast()
    }

    pub fn loc_receive_type(&self) -> VariableRef<'gc> {
        self.loc_receive_type.get().downcast()
    }

    pub fn loc_application_type(&self) -> VariableRef<'gc> {
        self.loc_application_type.get().downcast()
    }

    pub fn loc_primcall_type(&self) -> VariableRef<'gc> {
        self.loc_primcall_type.get().downcast()
    }

    pub fn loc_primref_type(&self) -> VariableRef<'gc> {
        self.loc_primref_type.get().downcast()
    }

    pub fn loc_constant_type(&self) -> VariableRef<'gc> {
        self.loc_constant_type.get().downcast()
    }

    pub fn loc_void_type(&self) -> VariableRef<'gc> {
        self.loc_void_type.get().downcast()
    }

    pub fn loc_values_type(&self) -> VariableRef<'gc> {
        self.loc_values_type.get().downcast()
    }

    pub fn loc_sequence_type(&self) -> VariableRef<'gc> {
        self.loc_sequence_type.get().downcast()
    }

    pub fn loc_proc_type(&self) -> VariableRef<'gc> {
        self.loc_proc_type.get().downcast()
    }

    pub fn loc_wcm_type(&self) -> VariableRef<'gc> {
        self.loc_wcm_type.get().downcast()
    }

    pub fn capy_module_name(&self) -> Value<'gc> {
        self.capy_module_name.get()
    }

    pub fn interesting_primitive_vars(&self) -> HashTableRef<'gc> {
        self.interesting_primitive_vars.get().downcast()
    }

    pub fn empty_wrap(&self) -> Value<'gc> {
        self.empty_wrap.get()
    }

    pub fn resolve_module_root(&self) -> ModuleRef<'gc> {
        self.resolve_module_root.get().downcast()
    }

    pub fn root_module(&self) -> ModuleRef<'gc> {
        self.root_module.get().downcast()
    }

    pub fn scm_module(&self) -> ModuleRef<'gc> {
        self.scm_module.get().downcast()
    }

    pub fn for_each_value(&self, mut f: impl FnMut(&Cell<Value<'gc>>)) {
        let Globals {
            capy_module_name,
            interesting_primitive_vars,
            interesting_primitive_vars_loc,
            empty_wrap,
            resolve_module_root,
            root_module,
            scm_module,
            loc_resolve_module_root,
            loc_load_path,
            loc_load_extensions,
            loc_load_compiled_path,
            loc_load_compiled_extensions,
            loc_native_extension,
            loc_compile_fallback_path,
            loc_capy_root,
            loc_fresh_auto_compile,
            loc_the_root_module,
            loc_the_scm_module,
            loc_term_type,
            loc_lref_type,
            loc_lset_type,
            loc_let_type,
            loc_fix_type,
            loc_module_ref_type,
            loc_module_set_type,
            loc_toplevel_ref_type,
            loc_toplevel_set_type,
            loc_toplevel_define_type,
            loc_if_type,
            loc_receive_type,
            loc_application_type,
            loc_primcall_type,
            loc_primref_type,
            loc_constant_type,
            loc_void_type,
            loc_values_type,
            loc_sequence_type,
            loc_proc_type,
            loc_wcm_type,
        } = self;

        f(capy_module_name);
        f(interesting_primitive_vars);
        f(interesting_primitive_vars_loc);
        f(empty_wrap);
        f(resolve_module_root);
        f(root_module);
        f(scm_module);
        f(loc_resolve_module_root);
        f(loc_load_path);
        f(loc_load_extensions);
        f(loc_load_compiled_path);
        f(loc_load_compiled_extensions);
        f(loc_native_extension);
        f(loc_compile_fallback_path);
        f(loc_capy_root);
        f(loc_fresh_auto_compile);
        f(loc_the_root_module);
        f(loc_the_scm_module);
        f(loc_term_type);
        f(loc_lref_type);
        f(loc_lset_type);
        f(loc_let_type);
        f(loc_fix_type);
        f(loc_module_ref_type);
        f(loc_module_set_type);
        f(loc_toplevel_ref_type);
        f(loc_toplevel_set_type);
        f(loc_toplevel_define_type);
        f(loc_if_type);
        f(loc_receive_type);
        f(loc_application_type);
        f(loc_primcall_type);
        f(loc_primref_type);
        f(loc_constant_type);
        f(loc_void_type);
        f(loc_values_type);
        f(loc_sequence_type);
        f(loc_proc_type);
        f(loc_wcm_type);
    }
}

impl<'gc> Globals<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let capy_module_name = crate::list!(ctx, ctx.intern("capy"));
        let empty_wrap = crate::list!(ctx, Value::null());

        /* modules */

        let scm_module = Module::new(ctx, 0, Value::null(), Value::new(false));
        let wm = Gc::write(&ctx, scm_module);
        barrier::field!(wm, Module, name).unlock().set(Value::cons(
            ctx,
            ctx.intern("capy"),
            Value::null(),
        ));
        scm_module.kind.set(ModuleKind::Interface);

        barrier::field!(wm, Module, public_interface)
            .unlock()
            .set(Some(scm_module));

        let root_module = Module::new(ctx, 0, Value::null(), Value::new(false));
        let wm = Gc::write(&ctx, root_module);
        barrier::field!(wm, Module, name).unlock().set(Value::cons(
            ctx,
            ctx.intern("capy"),
            Value::null(),
        ));
        barrier::field!(wm, Module, obarray)
            .unlock()
            .set(scm_module.obarray.get());
        barrier::field!(wm, Module, public_interface)
            .unlock()
            .set(Some(scm_module));

        let resolve_module_root = Module::new(ctx, 0, Value::null(), Value::new(false));
        barrier::field!(Gc::write(&ctx, resolve_module_root), Module, name)
            .unlock()
            .set(Value::null());
        resolve_module_root.define_submodule(ctx, ctx.intern("capy"), root_module);

        super::modules::current_module(ctx).set(ctx, root_module.into());

        let loc_resolve_module_root = define(ctx, "*resolve-module-root*", resolve_module_root);
        let loc_the_root_module = define(ctx, "the-root-module", root_module);
        let loc_the_scm_module = define(ctx, "the-scm-module", scm_module);

        let loc_term_type = define(ctx, "&term", Value::undefined());
        let loc_lref_type = define(ctx, "&lref", Value::undefined());
        let loc_lset_type = define(ctx, "&lset", Value::undefined());
        let loc_let_type = define(ctx, "&let", Value::undefined());
        let loc_fix_type = define(ctx, "&fix", Value::undefined());
        let loc_module_ref_type = define(ctx, "&module-ref", Value::undefined());
        let loc_module_set_type = define(ctx, "&module-set", Value::undefined());
        let loc_toplevel_ref_type = define(ctx, "&toplevel-ref", Value::undefined());
        let loc_toplevel_set_type = define(ctx, "&toplevel-set", Value::undefined());
        let loc_toplevel_define_type = define(ctx, "&toplevel-define", Value::undefined());
        let loc_if_type = define(ctx, "&if", Value::undefined());
        let loc_receive_type = define(ctx, "&receive", Value::undefined());
        let loc_application_type = define(ctx, "&application", Value::undefined());
        let loc_primcall_type = define(ctx, "&primcall", Value::undefined());
        let loc_primref_type = define(ctx, "&primref", Value::undefined());
        let loc_constant_type = define(ctx, "&constant", Value::undefined());
        let loc_void_type = define(ctx, "&void", Value::undefined());
        let loc_values_type = define(ctx, "&values", Value::undefined());
        let loc_sequence_type = define(ctx, "&sequence", Value::undefined());
        let loc_proc_type = define(ctx, "&proc", Value::undefined());
        let loc_wcm_type = define(ctx, "&wcm", Value::undefined());

        let loc_load_path = define(ctx, "%load-path", Value::null());
        let loc_load_extensions =
            define(ctx, "%load-extensions", crate::list!(ctx, ctx.str("scm")));
        let loc_load_compiled_path = define(ctx, "%load-compiled-path", Value::null());
        let loc_load_compiled_extensions = define(ctx, "%load-compiled-extensions", Value::null());
        let loc_native_extension = define(
            ctx,
            "%native-extension",
            ctx.str(std::env::consts::DLL_EXTENSION),
        );
        let loc_compile_fallback_path = define(ctx, "%compile-fallback-path", Value::null());
        let loc_capy_root = define(ctx, "%capy-root", ctx.str(env!("CARGO_MANIFEST_DIR")));
        let loc_fresh_auto_compile = define(ctx, "%fresh-auto-compile", Value::new(false));

        let interesting_primitive_vars = HashTable::new(&ctx, HashTableType::Eq, 128, 0.75);

        for_each_prim_name(ctx, |_name_str, name| {
            let var: Value = root_module.ensure_local_variable(ctx, name.into()).into();

            interesting_primitive_vars.put(ctx, name, var);
        });

        let interesting_primitive_vars_loc = define(
            ctx,
            "*interesting-primitive-vars*",
            interesting_primitive_vars.into_value(ctx),
        );

        Self {
            capy_module_name: Cell::new(capy_module_name),
            interesting_primitive_vars: Cell::new(interesting_primitive_vars.into()),
            interesting_primitive_vars_loc: Cell::new(interesting_primitive_vars_loc.into()),
            empty_wrap: Cell::new(empty_wrap),
            resolve_module_root: Cell::new(resolve_module_root.into()),
            root_module: Cell::new(root_module.into()),
            scm_module: Cell::new(scm_module.into()),
            loc_resolve_module_root: Cell::new(loc_resolve_module_root.into()),
            loc_load_path: Cell::new(loc_load_path.into()),
            loc_load_extensions: Cell::new(loc_load_extensions.into()),
            loc_load_compiled_path: Cell::new(loc_load_compiled_path.into()),
            loc_load_compiled_extensions: Cell::new(loc_load_compiled_extensions.into()),
            loc_native_extension: Cell::new(loc_native_extension.into()),
            loc_compile_fallback_path: Cell::new(loc_compile_fallback_path.into()),
            loc_capy_root: Cell::new(loc_capy_root.into()),
            loc_fresh_auto_compile: Cell::new(loc_fresh_auto_compile.into()),
            loc_the_root_module: Cell::new(loc_the_root_module.into()),
            loc_the_scm_module: Cell::new(loc_the_scm_module.into()),
            loc_term_type: Cell::new(loc_term_type.into()),
            loc_lref_type: Cell::new(loc_lref_type.into()),
            loc_lset_type: Cell::new(loc_lset_type.into()),
            loc_let_type: Cell::new(loc_let_type.into()),
            loc_fix_type: Cell::new(loc_fix_type.into()),
            loc_module_ref_type: Cell::new(loc_module_ref_type.into()),
            loc_module_set_type: Cell::new(loc_module_set_type.into()),
            loc_toplevel_ref_type: Cell::new(loc_toplevel_ref_type.into()),
            loc_toplevel_set_type: Cell::new(loc_toplevel_set_type.into()),
            loc_toplevel_define_type: Cell::new(loc_toplevel_define_type.into()),
            loc_if_type: Cell::new(loc_if_type.into()),
            loc_receive_type: Cell::new(loc_receive_type.into()),
            loc_application_type: Cell::new(loc_application_type.into()),
            loc_primcall_type: Cell::new(loc_primcall_type.into()),
            loc_primref_type: Cell::new(loc_primref_type.into()),
            loc_constant_type: Cell::new(loc_constant_type.into()),
            loc_void_type: Cell::new(loc_void_type.into()),
            loc_values_type: Cell::new(loc_values_type.into()),
            loc_sequence_type: Cell::new(loc_sequence_type.into()),
            loc_proc_type: Cell::new(loc_proc_type.into()),
            loc_wcm_type: Cell::new(loc_wcm_type.into()),
        }
    }

    pub fn undefined() -> Self {
        Self {
            capy_module_name: Cell::new(Value::undefined()),
            interesting_primitive_vars: Cell::new(Value::undefined()),
            interesting_primitive_vars_loc: Cell::new(Value::undefined()),
            empty_wrap: Cell::new(Value::undefined()),
            resolve_module_root: Cell::new(Value::undefined()),
            root_module: Cell::new(Value::undefined()),
            scm_module: Cell::new(Value::undefined()),
            loc_resolve_module_root: Cell::new(Value::undefined()),
            loc_load_path: Cell::new(Value::undefined()),
            loc_load_extensions: Cell::new(Value::undefined()),
            loc_load_compiled_path: Cell::new(Value::undefined()),
            loc_load_compiled_extensions: Cell::new(Value::undefined()),
            loc_native_extension: Cell::new(Value::undefined()),
            loc_compile_fallback_path: Cell::new(Value::undefined()),
            loc_capy_root: Cell::new(Value::undefined()),
            loc_fresh_auto_compile: Cell::new(Value::undefined()),
            loc_the_root_module: Cell::new(Value::undefined()),
            loc_the_scm_module: Cell::new(Value::undefined()),
            loc_term_type: Cell::new(Value::undefined()),
            loc_lref_type: Cell::new(Value::undefined()),
            loc_lset_type: Cell::new(Value::undefined()),
            loc_let_type: Cell::new(Value::undefined()),
            loc_fix_type: Cell::new(Value::undefined()),
            loc_module_ref_type: Cell::new(Value::undefined()),
            loc_module_set_type: Cell::new(Value::undefined()),
            loc_toplevel_ref_type: Cell::new(Value::undefined()),
            loc_toplevel_set_type: Cell::new(Value::undefined()),
            loc_toplevel_define_type: Cell::new(Value::undefined()),
            loc_if_type: Cell::new(Value::undefined()),
            loc_receive_type: Cell::new(Value::undefined()),
            loc_application_type: Cell::new(Value::undefined()),
            loc_primcall_type: Cell::new(Value::undefined()),
            loc_primref_type: Cell::new(Value::undefined()),
            loc_constant_type: Cell::new(Value::undefined()),
            loc_void_type: Cell::new(Value::undefined()),
            loc_values_type: Cell::new(Value::undefined()),
            loc_sequence_type: Cell::new(Value::undefined()),
            loc_proc_type: Cell::new(Value::undefined()),
            loc_wcm_type: Cell::new(Value::undefined()),
        }
    }
}

pub(crate) static VM_GLOBALS: OnceLock<rsgc::Global<Rootable!(Globals<'_>)>> = OnceLock::new();
