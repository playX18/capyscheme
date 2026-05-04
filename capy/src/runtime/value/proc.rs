use std::{collections::HashMap, ops::Index, sync::LazyLock};

use crate::{
    IndexWrite, WeakProcessor,
    object::VTable,
    rsgc::{Global, alloc::ArrayRef, cell::Lock, collection::Visitor, sync::monitor::Monitor},
};
use mmtk::AllocationSemantics;

use crate::rsgc::object::HeapTypeInfo;
use crate::runtime::{
    Context,
    vm::trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
};

use super::*;
pub enum Return<'gc> {
    Continue(Value<'gc>),
    Err(Value<'gc>),
}

pub type NativeFn<'gc> = extern "C-unwind" fn(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
) -> NativeReturn<'gc>;

pub type NativeContinuation<'gc> = extern "C-unwind" fn(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc>;

#[repr(C)]
pub struct NativeProc {
    pub proc: Address,
}

static NATIVE_PROC_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, NativeProc>::VT,
    TypeCode16::NATIVE_PROC.bits(),
);
pub static NATIVE_PROC_INFO: &'static HeapTypeInfo = &NATIVE_PROC_INFO_VALUE;

static NATIVE_K_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, NativeProc>::VT,
    TypeCode16::NATIVE_K.bits(),
);
pub static NATIVE_K_INFO: &'static HeapTypeInfo = &NATIVE_K_INFO_VALUE;

impl NativeProc {
    pub fn new<'gc>(ctx: Context<'gc>, proc: Address, is_k: bool) -> Gc<'gc, Self> {
        let tc = if is_k {
            NATIVE_K_INFO
        } else {
            NATIVE_PROC_INFO
        };

        Gc::new_with_info(*ctx, NativeProc { proc }, tc)
    }
}

// SAFETY: NativeProc contains no GC-managed fields — `proc` is a raw function pointer.
// No tracing or weak processing is needed.
unsafe impl Tagged for NativeProc {
    const TC8: TypeCode8 = TypeCode8::NATIVE_PROCEDURE;
    const TC16: &[TypeCode16] = &[TypeCode16::NATIVE_PROC, TypeCode16::NATIVE_K];
    const TYPE_NAME: &'static str = "native-procedure";
}

unsafe impl Trace for NativeProc {
    // SAFETY: No GC references in NativeProc; nothing to trace.
    unsafe fn trace(&mut self, _vis: &mut Visitor) {}

    // SAFETY: No weak references in NativeProc.
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

#[repr(C)]
pub struct Closure<'gc> {
    pub code: Address,
    pub code_block: Gc<'gc, CodeBlock<'gc>>,

    pub meta: Lock<Value<'gc>>,
    pub nfree: usize,
    pub free: [Lock<Value<'gc>>; 0],
}

static CLOSURE_PROC_INFO_VALUE: HeapTypeInfo =
    HeapTypeInfo::new(CLOSURE_VTABLE, TypeCode16::CLOSURE_PROC.bits());
pub static CLOSURE_PROC_INFO: &'static HeapTypeInfo = &CLOSURE_PROC_INFO_VALUE;

static CLOSURE_K_INFO_VALUE: HeapTypeInfo =
    HeapTypeInfo::new(CLOSURE_VTABLE, TypeCode16::CLOSURE_K.bits());
pub static CLOSURE_K_INFO: &'static HeapTypeInfo = &CLOSURE_K_INFO_VALUE;

static CLOSURE_NATIVE_PROC_INFO_VALUE: HeapTypeInfo =
    HeapTypeInfo::new(CLOSURE_VTABLE, TypeCode16::CLOSURE_PROC.bits());
pub static CLOSURE_NATIVE_PROC_INFO: &'static HeapTypeInfo = &CLOSURE_NATIVE_PROC_INFO_VALUE;

static CLOSURE_NATIVE_K_INFO_VALUE: HeapTypeInfo =
    HeapTypeInfo::new(CLOSURE_VTABLE, TypeCode16::CLOSURE_K.bits());
pub static CLOSURE_NATIVE_K_INFO: &'static HeapTypeInfo = &CLOSURE_NATIVE_K_INFO_VALUE;

impl<'gc> Index<usize> for Closure<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(
            index < self.nfree,
            "index out of bounds: index={index}, len={nfree}",
            nfree = self.nfree
        );
        // SAFETY: `free` is a flexible array member allocated with `nfree` elements.
        // The `debug_assert` above ensures `index < nfree`, so the pointer arithmetic is in-bounds.
        unsafe { self.free.as_ptr().add(index).as_ref_unchecked() }
    }
}

// SAFETY: Closure's Index impl already validates bounds; IndexWrite just permits mutable
// access to the same in-bounds slots. The GC write barrier is handled at the call site.
unsafe impl<'gc> IndexWrite<usize> for Closure<'gc> {}

extern "C" fn trace_closure(obj: GCObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is guaranteed by the GC to point to a valid `Closure` allocated with
    // `CLOSURE_VTABLE`. We iterate exactly `nfree` trailing elements.
    unsafe {
        let closure = obj.to_address().as_mut_ref::<Closure>();
        visitor.trace(&mut closure.code_block);
        visitor.trace(&mut closure.meta);
        for i in 0..closure.nfree {
            visitor.trace(closure.free.as_mut_ptr().add(i).as_mut().unwrap());
        }
    }
}

extern "C" fn process_weak(_obj: GCObject, _weak_processor: &mut crate::rsgc::WeakProcessor) {
    // No weak references in Closure, so do nothing.
}

extern "C" fn compute_closure_size(obj: GCObject) -> usize {
    // SAFETY: `obj` is a valid `Closure` allocated by the GC with `CLOSURE_VTABLE`.
    unsafe {
        let closure = obj.to_address().as_ref::<Closure>();
        size_of::<Value>() * closure.nfree
    }
}

pub static CLOSURE_VTABLE: &'static VTable = &VTable {
    alignment: align_of::<Closure>(),
    compute_alignment: None,
    instance_size: size_of::<Closure>(),
    compute_size: Some(compute_closure_size),
    trace: trace_closure,
    weak_proc: process_weak,
    type_name: "procedure",
};

pub type ClosureRef<'gc> = Gc<'gc, Closure<'gc>>;

impl<'gc> Closure<'gc> {
    /// Offset of the free variables array from the start of the Closure struct. Used for codegen.
    pub const DATA_OFFSET: isize = std::mem::offset_of!(Closure, free) as isize;

    pub fn new(
        ctx: Context<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        Self::new_inner(ctx, code_block, free, is_cont, false)
    }

    pub fn new_native(
        ctx: Context<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        Self::new_inner(ctx, code_block, free, is_cont, true)
    }

    fn new_inner(
        ctx: Context<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        free: &[Value<'gc>],
        is_cont: bool,
        is_native: bool,
    ) -> Gc<'gc, Self> {
        let meta = code_block.metadata.get();
        let info = if is_native {
            if is_cont {
                CLOSURE_NATIVE_K_INFO
            } else {
                CLOSURE_NATIVE_PROC_INFO
            }
        } else if is_cont {
            CLOSURE_K_INFO
        } else {
            CLOSURE_PROC_INFO
        };

        /*Gc::new(
            *ctx,
            Self {
                header,
                direct,
                code,
                free,
                meta: Lock::new(meta),
            },
        )*/

        let size = size_of::<Self>() + (size_of::<Value>() * free.len());
        // SAFETY: We raw-allocate a `Closure` + trailing `free` array via the GC allocator.
        // The VTable ensures proper tracing. We initialize all fields before returning the Gc handle.
        unsafe {
            let ptr = ctx.raw_allocate_with_info(
                size,
                align_of::<Self>(),
                info,
                AllocationSemantics::Default,
            );
            let this = ptr.to_address().as_mut_ref::<Self>();
            this.code = code_block.entrypoint;
            this.code_block = code_block;

            this.meta = Lock::new(meta);
            this.nfree = free.len();
            for i in 0..free.len() {
                this.free.as_mut_ptr().add(i).write(Lock::new(free[i]));
            }
            Gc::from_gcobj(ptr)
        }
    }

    pub fn documentation(&self, ctx: Context<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let doc = meta.assq(Symbol::from_str(ctx, "documentation").into());
        doc.map(|d| d.cdr())
    }

    pub fn source(&self, ctx: Context<'gc>) -> Option<(Value<'gc>, u32, u32)> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }

        let src = meta.assq(Symbol::from_str(ctx, "source").into())?;
        let src = src.cdr();

        if src.is::<Vector>() && src.downcast::<Vector>().len() == 3 {
            let vec = src.downcast::<Vector>();
            let file = vec[0].get();
            let line = vec[1].get();
            let column = vec[2].get();

            if file.is::<Str>() && line.is_int32() && column.is_int32() {
                return Some((file, line.as_int32() as u32, column.as_int32() as u32));
            }
        } else if src.is_pair() {
            let file = src.assq(Symbol::from_str(ctx, "file").into())?;
            let line = src.assq(Symbol::from_str(ctx, "line").into())?;
            let column = src.assq(Symbol::from_str(ctx, "column").into())?;

            let file = file.cdr();
            let line = line.cdr();
            let column = column.cdr();

            if file.is::<Str>() && line.is_int32() && column.is_int32() {
                return Some((file, line.as_int32() as u32, column.as_int32() as u32));
            }
        }

        return None;
    }

    pub fn name(&self, ctx: Context<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let name = meta.assq(Symbol::from_str(ctx, "name").into());
        name.map(|n| n.cdr())
    }

    pub fn patch_entrypoint(
        ctx: Context<'gc>,
        closure: Gc<'gc, Closure<'gc>>,
        entrypoint: Address,
    ) {
        let closure = Gc::write(*ctx, closure) as *const _ as *mut Closure<'gc>;
        unsafe {
            (*closure).code = entrypoint;
        }
    }

    pub fn patch_code_entrypoint(
        ctx: Context<'gc>,
        closure: Gc<'gc, Closure<'gc>>,
        entrypoint: Address,
    ) {
        CodeBlock::patch_entrypoint(ctx, closure.code_block, entrypoint);
        Self::patch_entrypoint(ctx, closure, entrypoint);
    }

    pub fn sync_entrypoint_from_code_block(ctx: Context<'gc>, closure: Gc<'gc, Closure<'gc>>) {
        Self::patch_entrypoint(ctx, closure, closure.code_block.entrypoint);
    }
}

// SAFETY: Closure stores its type code in the heap header selected at construction time.
unsafe impl<'gc> Tagged for Closure<'gc> {
    const TC8: TypeCode8 = TypeCode8::CLOSURE;

    const TC16: &'static [TypeCode16] = &[
        TypeCode16::CLOSURE_PROC,
        TypeCode16::CLOSURE_K,
        TypeCode16::CLOSURE_FOREIGN,
    ];

    const TYPE_NAME: &'static str = "procedure";
}

impl<'gc> Closure<'gc> {
    pub fn is_continuation(&self) -> bool {
        payload_type_bits(self) == TypeCode16::CLOSURE_K.bits()
    }

    /// Check if this closure is a continuation produced by `call/cc`.
    pub fn is_reified_continuation(&self, ctx: Context<'gc>) -> bool {
        let meta = self.meta.get();
        meta.is_list() && meta.assq(ctx.intern("continuation")).is_some()
    }

    /// Returns winders and continuation marks associated with this reified continuation.
    pub fn reified_continuation_attachments(
        &self,
        ctx: Context<'gc>,
    ) -> Option<(Value<'gc>, Value<'gc>)> {
        let meta = self.meta.get();
        if !meta.is_list() {
            return None;
        }
        let cont_info = meta.assq(ctx.intern("continuation"))?;
        let cont_info = cont_info.cdr().downcast::<Tuple>();
        let winders = cont_info[0].get();
        let cmarks = cont_info[1].get();

        Some((winders, cmarks))
    }

    pub fn is_foreign(&self) -> bool {
        let info_id = payload_info_id(self);
        info_id == CLOSURE_NATIVE_PROC_INFO.id() || info_id == CLOSURE_NATIVE_K_INFO.id()
    }
}

pub struct Procedures<'gc> {
    registered: Monitor<HashMap<Address, Gc<'gc, NativeProc>>>,
    static_closures: Monitor<HashMap<Address, Gc<'gc, Closure<'gc>>>>,
}

impl<'gc> Procedures<'gc> {
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
        self.register_static_closure_inner(
            ctx,
            Address::from_ptr(f as *const ()),
            false,
            meta,
            |s, ctx| s.register_procedure(ctx, f).into(),
            get_trampoline_from_scheme,
        )
    }

    pub fn register_static_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        self.register_static_closure_inner(
            ctx,
            Address::from_ptr(f as *const ()),
            true,
            meta,
            |s, ctx| s.register_continuation(ctx, f).into(),
            get_cont_trampoline_from_scheme,
        )
    }

    fn register_static_closure_inner(
        &self,
        ctx: Context<'gc>,
        addr: Address,
        is_cont: bool,
        meta: Value<'gc>,
        register_fn: impl FnOnce(&Self, Context<'gc>) -> Value<'gc>,
        trampoline_fn: impl FnOnce() -> Address,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut closures = self.static_closures.lock();
        if let Some(&clos) = closures.get(&addr) {
            return clos;
        }
        let proc = register_fn(self, ctx);
        let code_block =
            CodeBlock::new_native(ctx, trampoline_fn(), CodeArity::variadic(0), is_cont, meta);

        let clos = Closure::new_native(ctx, code_block, &[proc], is_cont);

        closures.insert(addr, clos);
        clos
    }

    pub fn make_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_procedure(ctx, f);
        self.make_closure_inner(
            ctx,
            proc.into(),
            false,
            free_vars,
            meta,
            get_trampoline_from_scheme,
        )
    }

    pub fn make_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_continuation(ctx, f);
        self.make_closure_inner(
            ctx,
            proc.into(),
            true,
            free_vars,
            meta,
            get_cont_trampoline_from_scheme,
        )
    }

    fn make_closure_inner(
        &self,
        ctx: Context<'gc>,
        proc: Value<'gc>,
        is_cont: bool,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
        trampoline_fn: impl FnOnce() -> Address,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut fv = Vec::with_capacity(1);
        fv.push(proc);
        fv.extend(free_vars);

        if !meta.is_alist() {
            panic!("Continuation closures must have alist metadata");
        }
        let code_block =
            CodeBlock::new_native(ctx, trampoline_fn(), CodeArity::variadic(0), is_cont, meta);
        Closure::new_native(ctx, code_block, &fv, is_cont)
    }
}

// SAFETY: Procedures contains GC-managed Values in its registered/static_closures maps.
// We trace all entries during GC. The Monitor lock ensures exclusive access.
unsafe impl<'gc> Trace for Procedures<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for (_, proc) in self.registered.get_mut().iter_mut() {
            visitor.trace(proc);
        }

        for (_, clos) in self.static_closures.get_mut().iter_mut() {
            visitor.trace(clos);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[unsafe(export_name = "CAPY_PROCEDURES")]
pub static PROCEDURES: LazyLock<Global<crate::Rootable!(Procedures<'_>)>> = LazyLock::new(|| {
    Global::new(Procedures {
        registered: Monitor::new(HashMap::new()),
        static_closures: Monitor::new(HashMap::new()),
    })
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ReturnCode {
    ReturnOk = 0,
    ReturnErr = 1,
    Yield = 2,
    Continue = 3,
}

#[repr(C)]
pub struct NativeReturn<'gc> {
    pub code: ReturnCode,
    pub value: Value<'gc>,
}

/// A saved call to Scheme code. This type is used to represent a suspended call
/// to a Scheme function, allowing it to be resumed later. Main user is yielding for GC.
#[derive(Trace, Clone, Copy)]
#[collect(no_drop)]
#[repr(C)]
pub struct SavedCall<'gc> {
    pub rator: Value<'gc>,
    pub from_procedure: bool,
    pub rands: ArrayRef<'gc, Value<'gc>>,
}

#[repr(C)]
pub struct CodeBlock<'gc> {
    pub entrypoint: Address,
    pub kind: CodeBlockKind,
    pub arity: CodeArity,
    pub flags: CodeBlockFlags,
    pub metadata: Lock<Value<'gc>>,
    code_len: u32,
    code: [u8; 0],
}

static CODE_BLOCK_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, CodeBlock<'static>>::VT,
    TypeCode8::CODE_BLOCK.bits() as u16,
);

static CODE_BLOCK_VTABLE_BYTECODE: &VTable = &VTable {
    alignment: align_of::<CodeBlock>(),
    compute_alignment: None,
    instance_size: size_of::<CodeBlock>(),
    compute_size: Some({
        extern "C" fn compute_code_block_size(obj: GCObject) -> usize {
            unsafe {
                let obj = obj.to_address().as_ref::<CodeBlock<'static>>();
                obj.code_len as usize
            }
        }

        compute_code_block_size
    }),
    trace: {
        extern "C" fn trace_code_block(obj: GCObject, vis: &mut Visitor) {
            unsafe {
                let obj = obj.to_address().as_mut_ref::<CodeBlock<'static>>();
                obj.trace(vis);
            }
        }

        trace_code_block
    },
    weak_proc: {
        extern "C" fn process_weak_code_block(obj: GCObject, weak_processor: &mut WeakProcessor) {
            unsafe {
                let obj = obj.to_address().as_mut_ref::<CodeBlock<'static>>();
                obj.process_weak_refs(weak_processor);
            }
        }

        process_weak_code_block
    },
    type_name: "code-block-bytecode",
};

static CODE_BLOCK_BYTECODE_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    CODE_BLOCK_VTABLE_BYTECODE,
    TypeCode8::CODE_BLOCK.bits() as u16,
);

pub static CODE_BLOCK_BYTECODE_INFO: &'static HeapTypeInfo = &CODE_BLOCK_BYTECODE_INFO_VALUE;

pub static CODE_BLOCK_INFO: &'static HeapTypeInfo = &CODE_BLOCK_INFO_VALUE;

impl<'gc> CodeBlock<'gc> {
    fn normalize_metadata(metadata: Value<'gc>) -> Value<'gc> {
        if metadata == Value::new(false) {
            Value::null()
        } else {
            metadata
        }
    }

    fn new_inner(
        ctx: Context<'gc>,
        entrypoint: Address,
        kind: CodeBlockKind,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new_with_info(
            *ctx,
            Self {
                entrypoint,
                kind,
                arity,
                flags: if is_cont {
                    CodeBlockFlags::CONTINUATION
                } else {
                    CodeBlockFlags::empty()
                },
                metadata: Lock::new(Self::normalize_metadata(metadata)),
                code_len: 0,
                code: [0; 0],
            },
            CODE_BLOCK_INFO,
        )
    }

    pub fn new_aot(
        ctx: Context<'gc>,
        entrypoint: Address,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Self::new_inner(
            ctx,
            entrypoint,
            CodeBlockKind::AOT,
            arity,
            is_cont,
            metadata,
        )
    }

    pub fn new_native(
        ctx: Context<'gc>,
        entrypoint: Address,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Self::new_inner(
            ctx,
            entrypoint,
            CodeBlockKind::NativeProc,
            arity,
            is_cont,
            metadata,
        )
    }

    pub fn new_lazy_jit(
        ctx: Context<'gc>,
        stub_entrypoint: Address,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
        state: *const (),
    ) -> Gc<'gc, Self> {
        Self::new_inner(
            ctx,
            stub_entrypoint,
            CodeBlockKind::LazyJit { state },
            arity,
            is_cont,
            metadata,
        )
    }

    pub fn new_bytecode(
        ctx: Context<'gc>,
        entrypoint: Address,
        arity: CodeArity,
        _is_cont: bool,
        metadata: Value<'gc>,
        code: &[u8],
    ) -> Gc<'gc, Self> {
        unsafe {
            let size = size_of::<Self>() + code.len();
            let alignment = align_of::<Self>();
            let info = CODE_BLOCK_BYTECODE_INFO;
            let semantics = AllocationSemantics::Default;
            let alloc = ctx.raw_allocate_with_info(size, alignment, info, semantics);
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.entrypoint = entrypoint;
            this.arity = arity;
            this.metadata = Lock::new(metadata);
            std::ptr::copy_nonoverlapping(code.as_ptr(), this.code.as_mut_ptr(), code.len());
            Gc::from_gcobj(alloc)
        }
    }

    pub fn code(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.code.as_ptr(), self.code_len as usize) }
    }

    pub fn code_len(&self) -> usize {
        self.code_len as usize
    }

    pub fn patch_entrypoint(
        ctx: Context<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        entrypoint: Address,
    ) {
        let code_block = Gc::write(*ctx, code_block) as *const _ as *mut CodeBlock<'gc>;
        unsafe {
            (*code_block).entrypoint = entrypoint;
        }
    }

    pub fn lazy_jit_state(&self) -> Option<*const ()> {
        match self.kind {
            CodeBlockKind::LazyJit { state } => Some(state),
            CodeBlockKind::AOT | CodeBlockKind::NativeProc => None,
        }
    }
}

unsafe impl<'gc> Tagged for CodeBlock<'gc> {
    const TC8: TypeCode8 = TypeCode8::CODE_BLOCK;

    const TYPE_NAME: &'static str = "code-block";
}

unsafe impl<'gc> Trace for CodeBlock<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.metadata);
        match &mut self.kind {
            CodeBlockKind::AOT | CodeBlockKind::NativeProc | CodeBlockKind::LazyJit { .. } => {}
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeArity(i32);

impl CodeArity {
    pub fn new(arity: i32) -> Self {
        Self(arity)
    }

    pub fn variadic(fixed_arity: usize) -> Self {
        Self(-((fixed_arity as i32) + 1))
    }

    pub fn is_variadic(&self) -> bool {
        self.0 < 0
    }

    pub fn fixed_arity(&self) -> usize {
        if self.is_variadic() {
            (-self.0 - 1) as usize
        } else {
            self.0 as usize
        }
    }
}

bitflags::bitflags! {
    pub struct CodeBlockFlags: u32 {
        const CONTINUATION = 1 << 0;

    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeBlockKind {
    AOT,
    NativeProc,
    LazyJit { state: *const () },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{rsgc::barrier, runtime::Scheme};

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    extern "C-unwind" fn test_native_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> NativeReturn<'gc> {
        NativeReturn {
            code: ReturnCode::ReturnOk,
            value: Value::undefined(),
        }
    }

    extern "C-unwind" fn test_compiled_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> NativeReturn<'gc> {
        NativeReturn {
            code: ReturnCode::ReturnOk,
            value: Value::undefined(),
        }
    }

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    #[test]
    fn closure_inherits_metadata_from_code_block() {
        with_ctx(|ctx| {
            let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("closure-a"));
            let code_block = CodeBlock::new_aot(
                ctx,
                Address::from_ptr(test_native_proc as *const ()),
                CodeArity::new(0),
                false,
                metadata,
            );
            let closure = Closure::new(ctx, code_block, &[], false);

            assert_eq!(closure.code, code_block.entrypoint);
            assert!(Gc::ptr_eq(closure.code_block, code_block));
            assert_eq!(closure.meta.get(), code_block.metadata.get());
        });
    }

    #[test]
    fn mutating_closure_metadata_does_not_mutate_code_block_default() {
        with_ctx(|ctx| {
            let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("prototype"));
            let code_block = CodeBlock::new_aot(
                ctx,
                Address::from_ptr(test_native_proc as *const ()),
                CodeArity::new(0),
                false,
                metadata,
            );

            let closure_a = Closure::new(ctx, code_block, &[], false);
            let updated = Value::cons(ctx, ctx.intern("name"), ctx.str("updated"));
            let wclosure = Gc::write(*ctx, closure_a);
            barrier::field!(wclosure, Closure, meta)
                .unlock()
                .set(updated);

            let closure_b = Closure::new(ctx, code_block, &[], false);

            assert_eq!(code_block.metadata.get(), metadata);
            assert_eq!(closure_a.meta.get(), updated);
            assert_eq!(closure_b.meta.get(), metadata);
        });
    }

    #[test]
    fn native_closure_keeps_native_proc_in_first_free_slot() {
        with_ctx(|ctx| {
            let closure = PROCEDURES.fetch(*ctx).make_closure(
                ctx,
                test_native_proc,
                [Value::new(true)],
                Value::null(),
            );

            assert_eq!(closure.code, closure.code_block.entrypoint);
            assert!(closure.code_block.metadata.get().is_null());
            assert!(closure[0].get().is::<NativeProc>());
            assert_eq!(closure[1].get(), Value::new(true));
        });
    }

    #[test]
    fn lazy_jit_code_block_uses_resolver_entrypoint() {
        with_ctx(|ctx| {
            let resolver = Address::from_ptr(test_native_proc as *const ());
            let code_block = CodeBlock::new_lazy_jit(
                ctx,
                resolver,
                CodeArity::new(2),
                false,
                Value::new(false),
                std::ptr::null(),
            );
            let closure = Closure::new(ctx, code_block, &[], false);

            assert_eq!(
                code_block.kind,
                CodeBlockKind::LazyJit {
                    state: std::ptr::null()
                }
            );
            assert_eq!(code_block.entrypoint, resolver);
            assert_eq!(closure.code, resolver);
            assert!(code_block.metadata.get().is_null());
        });
    }

    #[test]
    fn closure_and_code_block_entrypoints_can_be_patched_together() {
        with_ctx(|ctx| {
            let resolver = Address::from_ptr(test_native_proc as *const ());
            let compiled = Address::from_ptr(test_compiled_proc as *const ());
            let code_block = CodeBlock::new_lazy_jit(
                ctx,
                resolver,
                CodeArity::new(0),
                false,
                Value::null(),
                std::ptr::null(),
            );
            let closure = Closure::new(ctx, code_block, &[], false);

            Closure::patch_code_entrypoint(ctx, closure, compiled);

            assert_eq!(code_block.entrypoint, compiled);
            assert_eq!(closure.code, compiled);
        });
    }
}
