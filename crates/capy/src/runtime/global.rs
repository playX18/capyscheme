use std::{cell::Cell, marker::PhantomData, sync::OnceLock};

use crate::Rootable;
use crate::heap::{Gc, Root, Trace, Visitor, sync::monitor::Monitor};

use crate::{
    expander::primitives::for_each_prim_name,
    prelude::{FromValue, HashTable, HashTableRef, HashTableType, IntoValue, Value, VariableRef},
    runtime::{MutationContext, linklet::Instance},
};

pub trait GlobalValue<'gc>: Trace + IntoValue<'gc> {}

type GlobalMap<'gc> = Vec<Value<'gc>>;
type RootedGlobalTable = crate::Rootable!(GlobalTable<'_>);

/// A table listing all global variables in the runtime.
///
/// Implemented as a vector of [`Value`]s, indexed by [`Global`]s which allows
/// us to serialize and deserialize it in a straightforward manner.
pub struct GlobalTable<'gc> {
    // TODO(GC): Allow removing globals? Use a slab or similar data structure?
    globals: Monitor<GlobalMap<'gc>>,
}

// SAFETY: `gc` for `GlobalTable` upholds all trait invariants
unsafe impl<'gc> Trace for GlobalTable<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, tracer: &mut Visitor<'_>) {
        for value in self.globals.get_mut().iter_mut() {
            tracer.trace(value);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

// SAFETY: `GlobalTable` is `Send` because all mutable state is synchronized
unsafe impl Send for GlobalTable<'_> {}
// SAFETY: `GlobalTable` is `Sync` because all mutable access is serialized
unsafe impl Sync for GlobalTable<'_> {}

static GLOBALS: OnceLock<crate::heap::Global<RootedGlobalTable>> = OnceLock::new();

/// Strongly-typed index into the global variable table.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalIndex(pub(crate) usize);

/// The main difference between [`heap::Global`] and this type
/// is that this type does not store pointer, but just an index into
/// the side global table. Global table itself stores [`Value`]s directly
/// and `Global` provides typed access to them.
pub struct Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    index: GlobalIndex,
    marker: PhantomData<*const R>,
}

pub fn globals() -> &'static crate::heap::Global<RootedGlobalTable> {
    GLOBALS.get_or_init(|| {
        crate::heap::Global::new(GlobalTable {
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
// SAFETY: Caller must ensure preconditions are met (see fn docs)
pub unsafe fn globals_from_vec<'gc>(_ctx: impl AsRef<MutationContext<'gc>>, vec: Vec<Value<'gc>>) {
    GLOBALS
        .set(crate::heap::Global::new(GlobalTable {
            globals: Monitor::new(vec),
        }))
        .unwrap_or_else(|_| panic!("globals have already been initialized"));
}

pub fn for_each_global<'gc, F>(ctx: impl AsRef<MutationContext<'gc>>, mut f: F)
where
    F: FnMut(usize, Value<'gc>),
{
    let ctx = ctx.as_ref();
    let globals = globals().fetch(*ctx);
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
    pub fn new<'gc>(ctx: MutationContext<'gc>, initial: Root<'gc, R>) -> Self
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        let index = GlobalIndex(guard.len());
        guard.push(initial.into_value(ctx));
        Self {
            index,
            marker: PhantomData,
        }
    }

    /// Get the value of the global variable.
    pub fn fetch<'gc>(&self, ctx: MutationContext<'gc>) -> Root<'gc, R>
    where
        for<'a> Root<'a, R>: FromValue<'a>,
    {
        let globals = globals().fetch(ctx);
        let guard = globals.globals.lock();
        let value = guard[self.index.0];
        Root::<R>::try_from_value(ctx, value).expect("global variable has wrong type")
    }

    /// Set the value of the global variable.
    pub fn set<'gc>(&self, ctx: MutationContext<'gc>, value: Root<'gc, R>)
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        guard[self.index.0] = value.into_value(ctx);
    }

    /// Construct a global handle from an existing global table index.
    ///
    /// # Safety
    ///
    /// `index` must refer to an initialized entry in the runtime global table
    /// for values compatible with `R`.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn from_index(index: usize) -> Self {
        Self {
            index: GlobalIndex(index),
            marker: PhantomData,
        }
    }
}

// SAFETY: `R` for `Global` upholds all trait invariants
unsafe impl<R> Send for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

// SAFETY: `R` for `Global` upholds all trait invariants
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
                #[unsafe(export_name = concat!("CAPY_GLOBAL_", stringify!($name)))]
                $v static [<$name: upper>]: std::sync::OnceLock<$crate::prelude::Global<$crate::Rootable!($l => $t)>> = std::sync::OnceLock::new();

                #[unsafe(export_name = concat!("capy_global_", stringify!($name)))]
                $v fn [<$name: lower>]<$l>($ctx: $crate::runtime::MutationContext<$l>) -> $t {
                   [<$name: upper>]
                        .get_or_init(|| {
                            let init: $t = $init;
                            $crate::prelude::Global::new($ctx, init)
                        }).fetch($ctx)
                }


                #[doc(hidden)]
// SAFETY: Caller must ensure preconditions are met (see fn docs)
                pub unsafe fn [<$name: lower _from_index>](index: usize) -> $crate::prelude::Global<$crate::Rootable!($l => $t)> {
// SAFETY: Preconditions verified by the surrounding code
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
    /// The `(#%capy)` instance every native primitive is defined in.
    pub capy_instance: Cell<Value<'gc>>,

    // HashTableRef
    pub interesting_primitive_vars: Cell<Value<'gc>>,
    // VariableRef
    pub interesting_primitive_vars_loc: Cell<Value<'gc>>,
    pub empty_wrap: Cell<Value<'gc>>,

    pub keyword_map: Cell<Value<'gc>>,

    /// Nongenerative record type descriptors, keyed by their uid symbol.
    pub nongenerative_rtds: Cell<Value<'gc>>,

    /// The native R6RS condition record types.
    pub condition_types: Option<crate::runtime::conditions::ConditionTypes<'gc>>,

    /// Procedures and record types Rust calls through [`Hook`](crate::runtime::hooks::Hook).
    pub hooks: crate::runtime::hooks::Hooks<'gc>,

    pub loc_load_path: Cell<Value<'gc>>,
    pub loc_load_extensions: Cell<Value<'gc>>,
    pub loc_load_compiled_path: Cell<Value<'gc>>,
    pub loc_load_compiled_extensions: Cell<Value<'gc>>,
    pub loc_native_extension: Cell<Value<'gc>>,
    pub loc_compile_fallback_path: Cell<Value<'gc>>,
    pub loc_capy_root: Cell<Value<'gc>>,
    pub loc_capy_sysroot: Cell<Value<'gc>>,
    pub loc_fresh_auto_compile: Cell<Value<'gc>>,

    /// Tier-up jobs waiting for a JIT worker.
    pub jit_jobs: crate::runtime::codegen::jit::JobQueue<'gc>,
    pub compile_jobs: crate::runtime::compile_service::CompileQueue<'gc>,
}

impl<'gc> Globals<'gc> {
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

    pub fn capy_instance(&self) -> Gc<'gc, Instance<'gc>> {
        self.capy_instance.get().downcast()
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

    /// The native condition record types. Panics before [`Globals::new`] has run.
    pub fn condition_types(&self) -> &crate::runtime::conditions::ConditionTypes<'gc> {
        self.condition_types
            .as_ref()
            .expect("condition types are not initialized")
    }

    pub fn for_each_value(&self, mut f: impl FnMut(&Cell<Value<'gc>>)) {
        let Globals {
            keyword_map,
            nongenerative_rtds,
            // Traced by the derived `Trace` impl, not a value cell.
            condition_types: _,
            hooks,
            capy_module_name,
            capy_instance,
            interesting_primitive_vars,
            interesting_primitive_vars_loc,
            empty_wrap,
            loc_load_path,
            loc_load_extensions,
            loc_load_compiled_path,
            loc_load_compiled_extensions,
            loc_native_extension,
            loc_compile_fallback_path,
            loc_capy_root,
            loc_capy_sysroot,
            loc_fresh_auto_compile,
            // Traced by its own `Trace` impl, not a value cell.
            jit_jobs: _,
            compile_jobs: _,
        } = self;

        f(capy_module_name);
        f(capy_instance);
        f(interesting_primitive_vars);
        f(interesting_primitive_vars_loc);
        f(empty_wrap);
        f(keyword_map);
        f(nongenerative_rtds);
        hooks.for_each_slot(&mut f);
        f(loc_load_path);
        f(loc_load_extensions);
        f(loc_load_compiled_path);
        f(loc_load_compiled_extensions);
        f(loc_native_extension);
        f(loc_compile_fallback_path);
        f(loc_capy_root);
        f(loc_capy_sysroot);
        f(loc_fresh_auto_compile);
    }
}

impl<'gc> Globals<'gc> {
    pub fn new(ctx: MutationContext<'gc>) -> Self {
        let capy_module_name = crate::list!(ctx, ctx.intern("capy"));
        let empty_wrap = crate::list!(ctx, Value::null());

        let capy_instance = Instance::new(ctx, crate::list!(ctx, ctx.intern("#%capy")), true);
        let define = |ctx: MutationContext<'gc>, name: &str, value: Value<'gc>| {
            capy_instance.define(ctx, ctx.intern(name), value)
        };

        let keyword_map = HashTable::new(ctx, HashTableType::Eq, 64, 0.75);

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

        let interesting_primitive_vars = HashTable::new(ctx, HashTableType::Eq, 128, 0.75);

        for_each_prim_name(ctx, |_name_str, name| {
            let var: Value = capy_instance.ensure_variable(ctx, name).into();

            interesting_primitive_vars.put(ctx, name, var);
        });

        let interesting_primitive_vars_loc = define(
            ctx,
            "*interesting-primitive-vars*",
            interesting_primitive_vars.into_value(ctx),
        );

        let nongenerative_rtds = HashTable::new(ctx, HashTableType::Eq, 16, 0.75);

        let condition_types = crate::runtime::conditions::ConditionTypes::new(ctx);
        for ((name, _, _), wrapper) in crate::runtime::conditions::CONDITION_TYPES
            .iter()
            .zip(&condition_types.wrappers)
        {
            define(ctx, name, *wrapper);
        }

        Self {
            keyword_map: Cell::new(keyword_map.into_value(ctx)),
            nongenerative_rtds: Cell::new(nongenerative_rtds.into_value(ctx)),
            condition_types: Some(condition_types),
            hooks: crate::runtime::hooks::Hooks::new(),
            capy_module_name: Cell::new(capy_module_name),
            capy_instance: Cell::new(capy_instance.into()),
            interesting_primitive_vars: Cell::new(interesting_primitive_vars.into()),
            interesting_primitive_vars_loc: Cell::new(interesting_primitive_vars_loc.into()),
            empty_wrap: Cell::new(empty_wrap),
            loc_load_path: Cell::new(loc_load_path.into()),
            loc_load_extensions: Cell::new(loc_load_extensions.into()),
            loc_load_compiled_path: Cell::new(loc_load_compiled_path.into()),
            loc_load_compiled_extensions: Cell::new(loc_load_compiled_extensions.into()),
            loc_native_extension: Cell::new(loc_native_extension.into()),
            loc_compile_fallback_path: Cell::new(loc_compile_fallback_path.into()),
            loc_capy_root: Cell::new(loc_capy_root.into()),
            loc_capy_sysroot: Cell::new(loc_capy_root.into()),
            loc_fresh_auto_compile: Cell::new(loc_fresh_auto_compile.into()),
            jit_jobs: Default::default(),
            compile_jobs: Default::default(),
        }
    }

    pub fn undefined() -> Self {
        Self {
            keyword_map: Cell::new(Value::undefined()),
            nongenerative_rtds: Cell::new(Value::undefined()),
            condition_types: None,
            hooks: crate::runtime::hooks::Hooks::new(),
            capy_module_name: Cell::new(Value::undefined()),
            capy_instance: Cell::new(Value::undefined()),
            interesting_primitive_vars: Cell::new(Value::undefined()),
            interesting_primitive_vars_loc: Cell::new(Value::undefined()),
            empty_wrap: Cell::new(Value::undefined()),
            loc_load_path: Cell::new(Value::undefined()),
            loc_load_extensions: Cell::new(Value::undefined()),
            loc_load_compiled_path: Cell::new(Value::undefined()),
            loc_load_compiled_extensions: Cell::new(Value::undefined()),
            loc_native_extension: Cell::new(Value::undefined()),
            loc_compile_fallback_path: Cell::new(Value::undefined()),
            loc_capy_root: Cell::new(Value::undefined()),
            loc_capy_sysroot: Cell::new(Value::undefined()),
            loc_fresh_auto_compile: Cell::new(Value::undefined()),
            jit_jobs: Default::default(),
            compile_jobs: Default::default(),
        }
    }
}

type RootedGlobals = crate::Rootable!(Globals<'_>);

pub(crate) static VM_GLOBALS: OnceLock<crate::heap::Global<RootedGlobals>> = OnceLock::new();
