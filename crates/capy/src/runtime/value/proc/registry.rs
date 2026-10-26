//! Registry and builders for native closures.

use super::*;

type StaticClosureMap<'gc> = HashMap<Address, Gc<'gc, Closure<'gc>>>;
type RootedProcedureRegistry = crate::Rootable!(ProcedureRegistry<'_>);

#[derive(Clone, Copy)]
struct CachedNativeSite<'gc> {
    /// The registered proc wrapper, as the value for free slot 0.
    proc: Value<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    /// Metadata the cached block was built with, a hit requires identity.
    meta: Value<'gc>,
}

/// Caches native procedure wrappers and static native closures.
pub struct ProcedureRegistry<'gc> {
    registered: Monitor<HashMap<Address, Gc<'gc, NativeProc>>>,
    static_closures: Monitor<StaticClosureMap<'gc>>,
    native_sites: Monitor<HashMap<Address, CachedNativeSite<'gc>>>,
}

/// Native callable ABI variants supported by Scheme closures.
///
/// Both take `(ctx, sfp)`, the kind only selects the `CodeBlock` flag the
/// driver and `is_continuation` checks read.
pub enum NativeClosureKind<'gc> {
    /// A native procedure.
    Procedure(NativeFn<'gc>),
    /// A native continuation, entered through `subr_ret`.
    Continuation(NativeFn<'gc>),
}

impl<'gc> NativeClosureKind<'gc> {
    fn addr(&self) -> Address {
        match self {
            Self::Procedure(f) => Address::from_ptr(*f as *const ()),
            Self::Continuation(f) => Address::from_ptr(*f as *const ()),
        }
    }

    fn is_continuation(&self) -> bool {
        matches!(self, Self::Continuation(_))
    }
}

/// Builder for a native closure.
///
/// The built closure always stores the `NativeProc` wrapper in free slot `0`,
/// any extra free variables are appended after it.
pub struct NativeClosureBuilder<'a, 'gc> {
    registry: &'a ProcedureRegistry<'gc>,
    kind: NativeClosureKind<'gc>,
    free_vars: Vec<Value<'gc>>,
    meta: Value<'gc>,
    cached_static: bool,
}

impl<'a, 'gc> NativeClosureBuilder<'a, 'gc> {
    /// Adds captured values after the native procedure wrapper slot.
    pub fn free_vars(mut self, free_vars: impl IntoIterator<Item = Value<'gc>>) -> Self {
        self.free_vars.extend(free_vars);
        self
    }

    /// Sets the closure metadata alist.
    pub fn metadata(mut self, meta: Value<'gc>) -> Self {
        self.meta = meta;
        self
    }

    /// Reuses one closure per native function address.
    pub fn cached_static(mut self) -> Self {
        self.cached_static = true;
        self
    }

    /// Allocates or returns the requested native closure.
    pub fn build(self, ctx: MutationContext<'gc>) -> Gc<'gc, Closure<'gc>> {
        self.registry.build_native_closure(ctx, self)
    }
}

impl<'gc> ProcedureRegistry<'gc> {
    fn builder<'a>(&'a self, kind: NativeClosureKind<'gc>) -> NativeClosureBuilder<'a, 'gc> {
        NativeClosureBuilder {
            registry: self,
            kind,
            free_vars: Vec::new(),
            meta: Value::null(),
            cached_static: false,
        }
    }

    /// Starts building a native procedure closure.
    pub fn procedure(&self, f: NativeFn<'gc>) -> NativeClosureBuilder<'_, 'gc> {
        self.builder(NativeClosureKind::Procedure(f))
    }

    /// Starts building a native continuation closure.
    pub fn continuation(&self, f: NativeFn<'gc>) -> NativeClosureBuilder<'_, 'gc> {
        self.builder(NativeClosureKind::Continuation(f))
    }

    pub fn register_continuation(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
    ) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();

        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, true);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_procedure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
    ) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();
        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, false);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_static_closure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.procedure(f).metadata(meta).cached_static().build(ctx)
    }

    pub fn register_static_cont_closure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.continuation(f)
            .metadata(meta)
            .cached_static()
            .build(ctx)
    }

    pub fn make_closure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.procedure(f)
            .free_vars(free_vars)
            .metadata(meta)
            .build(ctx)
    }

    pub fn make_cont_closure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.continuation(f)
            .free_vars(free_vars)
            .metadata(meta)
            .build(ctx)
    }

    /// Builds a native procedure closure with exactly three free variables,
    /// reusing a cached proc wrapper and code block.
    ///
    /// The cache is keyed by function address and guarded by metadata
    /// identity: a caller passing a different metadata value for the same
    /// address falls back to [`ProcedureRegistry::make_closure`]
    pub fn make_cached_closure(
        &self,
        ctx: MutationContext<'gc>,
        f: NativeFn<'gc>,
        captured: [Value<'gc>; 3],
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let kind = NativeClosureKind::Procedure(f);
        let addr = kind.addr();
        let cached = {
            let sites = self.native_sites.lock();
            sites.get(&addr).filter(|site| site.meta == meta).copied()
        };
        if let Some(site) = cached {
            let free = [site.proc, captured[0], captured[1], captured[2]];
            return Closure::new_native(ctx, site.code_block, &free, kind.is_continuation());
        }
        // Miss (or metadata mismatch): build through the shared slow path
        // so validation and layout stay identical, then memoize the fresh
        // closure's site data for the next capture. First writer wins, a
        // mismatch keeps missing and keeps taking the slow path.
        let closure = self.make_closure(ctx, f, captured, meta);
        let mut sites = self.native_sites.lock();
        sites.entry(addr).or_insert_with(|| CachedNativeSite {
            proc: closure[0].get(),
            code_block: closure.code_block,
            meta,
        });
        closure
    }

    fn build_native_closure(
        &self,
        ctx: MutationContext<'gc>,
        builder: NativeClosureBuilder<'_, 'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let addr = builder.kind.addr();
        let is_cont = builder.kind.is_continuation();

        if builder.cached_static {
            if !builder.free_vars.is_empty() {
                panic!("static native closures cannot capture extra free variables");
            }
            let mut closures = self.static_closures.lock();
            if let Some(&closure) = closures.get(&addr) {
                return closure;
            }
            let proc = self.register_native_proc(ctx, builder.kind).into();
            let code_block =
                CodeBlock::new_native(ctx, CodeArity::variadic(0), is_cont, builder.meta);
            let closure = Closure::new_native(ctx, code_block, &[proc], is_cont);
            closures.insert(addr, closure);
            crate::runtime::profile::maps::note_native_closure(
                Value::from(closure).bits(),
                native_closure_name(ctx, builder.meta),
            );
            return closure;
        }

        if builder.meta != Value::new(false) && !builder.meta.is_alist() {
            panic!("native closures require alist metadata");
        }

        let proc = self.register_native_proc(ctx, builder.kind).into();
        let mut fv = Vec::with_capacity(1);
        fv.push(proc);
        fv.extend(builder.free_vars);

        let code_block = CodeBlock::new_native(ctx, CodeArity::variadic(0), is_cont, builder.meta);
        Closure::new_native(ctx, code_block, &fv, is_cont)
    }

    fn register_native_proc(
        &self,
        ctx: MutationContext<'gc>,
        kind: NativeClosureKind<'gc>,
    ) -> Gc<'gc, NativeProc> {
        match kind {
            NativeClosureKind::Procedure(f) => self.register_procedure(ctx, f),
            NativeClosureKind::Continuation(f) => self.register_continuation(ctx, f),
        }
    }
}

// SAFETY: ProcedureRegistry contains GC-managed Values in its maps.
// We trace all entries during GC. The Monitor lock ensures exclusive access.
unsafe impl<'gc> Trace for ProcedureRegistry<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for proc in self.registered.get_mut().values_mut() {
            visitor.trace(proc);
        }

        for clos in self.static_closures.get_mut().values_mut() {
            visitor.trace(clos);
        }

        for site in self.native_sites.get_mut().values_mut() {
            visitor.trace(&mut site.proc);
            visitor.trace(&mut site.code_block);
            visitor.trace(&mut site.meta);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

/// Procedure name from native metadata, for profiler labels.
fn native_closure_name<'gc>(ctx: MutationContext<'gc>, meta: Value<'gc>) -> String {
    if meta.is_pair() {
        if let Some(pair) = meta.assq(ctx.intern("name")) {
            let value = pair.cdr();
            if value.is::<Symbol>() {
                return value.downcast::<Symbol>().as_str().into_owned();
            }
            if value.is::<Str>() {
                return value.downcast::<Str>().to_string();
            }
        }
    }
    String::from("native")
}

#[unsafe(export_name = "CAPY_PROCEDURES")]
pub static PROCEDURES: LazyLock<Global<RootedProcedureRegistry>> = LazyLock::new(|| {
    Global::new(ProcedureRegistry {
        registered: Monitor::new(HashMap::new()),
        static_closures: Monitor::new(HashMap::new()),
        native_sites: Monitor::new(HashMap::new()),
    })
});
