//! Registry and builders for native closures.

use super::*;

type StaticClosureMap<'gc> = HashMap<Address, Gc<'gc, Closure<'gc>>>;
type RootedProcedureRegistry = crate::Rootable!(ProcedureRegistry<'_>);

/// Caches native procedure wrappers and static native closures.
pub struct ProcedureRegistry<'gc> {
    registered: Monitor<HashMap<Address, Gc<'gc, NativeProc>>>,
    static_closures: Monitor<StaticClosureMap<'gc>>,
}

/// Native callable ABI variants supported by Scheme closures.
pub enum NativeClosureKind<'gc> {
    /// A native procedure that receives a return continuation.
    Procedure(NativeFn<'gc>),
    /// A native continuation that returns directly to the VM trampoline.
    Continuation(NativeContinuation<'gc>),
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

    fn trampoline(&self) -> Address {
        match self {
            Self::Procedure(_) => get_trampoline_from_scheme(),
            Self::Continuation(_) => get_cont_trampoline_from_scheme(),
        }
    }
}

/// Builder for a native closure.
///
/// The built closure always stores the `NativeProc` wrapper in free slot `0`;
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
    pub fn build(self, ctx: Context<'gc>) -> Gc<'gc, Closure<'gc>> {
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
    pub fn continuation(&self, f: NativeContinuation<'gc>) -> NativeClosureBuilder<'_, 'gc> {
        self.builder(NativeClosureKind::Continuation(f))
    }

    pub fn register_continuation(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
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

    pub fn register_procedure(&self, ctx: Context<'gc>, f: NativeFn<'gc>) -> Gc<'gc, NativeProc> {
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
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.procedure(f).metadata(meta).cached_static().build(ctx)
    }

    pub fn register_static_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.continuation(f)
            .metadata(meta)
            .cached_static()
            .build(ctx)
    }

    pub fn make_closure(
        &self,
        ctx: Context<'gc>,
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
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.continuation(f)
            .free_vars(free_vars)
            .metadata(meta)
            .build(ctx)
    }

    fn build_native_closure(
        &self,
        ctx: Context<'gc>,
        builder: NativeClosureBuilder<'_, 'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let addr = builder.kind.addr();
        let is_cont = builder.kind.is_continuation();
        let trampoline = builder.kind.trampoline();

        if builder.cached_static {
            if !builder.free_vars.is_empty() {
                panic!("static native closures cannot capture extra free variables");
            }
            let mut closures = self.static_closures.lock();
            if let Some(&closure) = closures.get(&addr) {
                return closure;
            }
            let proc = self.register_native_proc(ctx, builder.kind).into();
            let code_block = CodeBlock::new_native(
                ctx,
                trampoline,
                CodeArity::variadic(0),
                is_cont,
                builder.meta,
            );
            let closure = Closure::new_native(ctx, code_block, &[proc], is_cont);
            closures.insert(addr, closure);
            return closure;
        }

        if builder.meta != Value::new(false) && !builder.meta.is_alist() {
            panic!("native closures require alist metadata");
        }

        let proc = self.register_native_proc(ctx, builder.kind).into();
        let mut fv = Vec::with_capacity(1);
        fv.push(proc);
        fv.extend(builder.free_vars);

        let code_block = CodeBlock::new_native(
            ctx,
            trampoline,
            CodeArity::variadic(0),
            is_cont,
            builder.meta,
        );
        Closure::new_native(ctx, code_block, &fv, is_cont)
    }

    fn register_native_proc(
        &self,
        ctx: Context<'gc>,
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
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for proc in self.registered.get_mut().values_mut() {
            visitor.trace(proc);
        }

        for clos in self.static_closures.get_mut().values_mut() {
            visitor.trace(clos);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[unsafe(export_name = "CAPY_PROCEDURES")]
pub static PROCEDURES: LazyLock<Global<RootedProcedureRegistry>> = LazyLock::new(|| {
    Global::new(ProcedureRegistry {
        registered: Monitor::new(HashMap::new()),
        static_closures: Monitor::new(HashMap::new()),
    })
});
