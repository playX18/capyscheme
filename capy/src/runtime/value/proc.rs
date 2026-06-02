use std::{
    cell::{Cell, UnsafeCell},
    collections::{HashMap, VecDeque},
    io,
    mem::MaybeUninit,
    ops::Index,
    sync::{Arc, LazyLock, Mutex as StdMutex},
};

use crate::{
    IndexWrite, WeakProcessor,
    object::VTable,
    rsgc::{
        Global, cell::Lock, collection::Visitor, finalizer::FinalizerQueue, sync::monitor::Monitor,
    },
};
use asmkit::core::jit_allocator::Span;
use mmtk::{AllocationSemantics, util::ObjectReference};

use crate::rsgc::object::{HeapTypeInfo, builtin_type_ids};
use crate::runtime::{
    Context,
    code_memory::CodeSpan,
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

static CLOSURE_PROC_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new_static(
    CLOSURE_VTABLE,
    TypeCode16::CLOSURE_PROC.bits(),
    builtin_type_ids::CLOSURE_PROC,
);
pub static CLOSURE_PROC_INFO: &'static HeapTypeInfo = &CLOSURE_PROC_INFO_VALUE;

static CLOSURE_K_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new_static(
    CLOSURE_VTABLE,
    TypeCode16::CLOSURE_K.bits(),
    builtin_type_ids::CLOSURE_K,
);
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
    Continue = 3,
}

#[repr(C)]
pub struct NativeReturn<'gc> {
    pub code: ReturnCode,
    pub value: Value<'gc>,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnlinkedRelocation {
    pub offset: u32,
    pub kind_tag: u8,
    pub target_tag: u8,
    pub target_payload: u32,
    pub addend: i64,
    pub aux_tag: u8,
}

#[repr(C)]
pub struct CodeBlock<'gc> {
    pub entrypoint: Address,
    pub kind: CodeBlockKind,
    pub arity: CodeArity,
    pub flags: CodeBlockFlags,
    pub metadata: Lock<Value<'gc>>,
    pub unlinked: Gc<'gc, UnlinkedCodeBlock<'gc>>,
    pub loaded_data_base: Address,
    pub loaded_data_slot_count: u32,
    span_initialized: Cell<bool>,
    span_finalized: Cell<bool>,
    span_borrowed: Cell<bool>,
    span: UnsafeCell<MaybeUninit<Span>>,
    code_len: u32,
    code: [usize; 0],
}

// Finalizer queues move `CodeBlock` object references between GC worker and
// scheduler threads. The queue only reads the span ownership guard and releases
// the JIT span through the runtime-wide `CodeMemory` lock.
unsafe impl<'gc> Send for CodeBlock<'gc> {}

static CODE_BLOCK_VTABLE: &VTable = &VTable {
    alignment: align_of::<CodeBlock>(),
    compute_alignment: None,
    instance_size: size_of::<CodeBlock>(),
    compute_size: Some({
        extern "C" fn compute_code_block_size(obj: GCObject) -> usize {
            let _ = obj;
            0
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
    type_name: "code-block",
};

static CODE_BLOCK_INFO_VALUE: HeapTypeInfo =
    HeapTypeInfo::new(CODE_BLOCK_VTABLE, TypeCode8::CODE_BLOCK.bits() as u16);

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

pub struct CodeBlockFinalizerQueue {
    finalizers: StdMutex<VecDeque<ObjectReference>>,
}

impl CodeBlockFinalizerQueue {
    fn new() -> Self {
        Self {
            finalizers: StdMutex::new(VecDeque::new()),
        }
    }

    fn pop(&self) -> Option<ObjectReference> {
        self.finalizers.lock().unwrap().pop_front()
    }
}

pub static CODE_BLOCK_FINALIZERS: LazyLock<Arc<CodeBlockFinalizerQueue>> =
    LazyLock::new(|| Arc::new(CodeBlockFinalizerQueue::new()));

unsafe impl FinalizerQueue for CodeBlockFinalizerQueue {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        self.finalizers.lock().unwrap().push_back(object);
    }

    fn schedule(&self) {
        while let Some(object) = self.pop() {
            unsafe {
                let gc_object = GCObject::from(object);
                let code_block = gc_object.to_address().as_ref::<CodeBlock<'static>>();
                if let Some(span) = code_block.take_span_for_finalization() {
                    // A dead CodeBlock can still have return addresses into its
                    // executable span on native stacks. Consume the owner here so
                    // the finalizer runs exactly once, but keep the mapping valid
                    // until the process exits.
                    std::mem::forget(span);
                }
            }
        }
    }
}

#[repr(C)]
pub struct UnlinkedCodeBlock<'gc> {
    pub entry_offset: u32,
    code_len: u32,
    relocation_count: u32,
    loaded_data_value_bitmap_word_count: u32,
    _marker: std::marker::PhantomData<&'gc ()>,
    payload: [UnlinkedRelocation; 0],
}

#[derive(Clone, Copy)]
struct UnlinkedCodeBlockLayout {
    relocation_offset: usize,
    bitmap_offset: usize,
    payload_size: usize,
}

fn align_payload_offset(offset: usize, alignment: usize) -> Option<usize> {
    debug_assert!(alignment.is_power_of_two());
    offset
        .checked_add(alignment - 1)
        .map(|offset| offset & !(alignment - 1))
}

fn unlinked_code_block_layout(
    code_len: usize,
    relocation_count: usize,
    loaded_data_value_bitmap_word_count: usize,
) -> Option<UnlinkedCodeBlockLayout> {
    let relocation_offset = align_payload_offset(code_len, align_of::<UnlinkedRelocation>())?;
    let relocation_size = relocation_count.checked_mul(size_of::<UnlinkedRelocation>())?;
    let bitmap_offset = align_payload_offset(
        relocation_offset.checked_add(relocation_size)?,
        align_of::<usize>(),
    )?;
    let bitmap_size = loaded_data_value_bitmap_word_count.checked_mul(size_of::<usize>())?;
    let payload_size = bitmap_offset.checked_add(bitmap_size)?;
    Some(UnlinkedCodeBlockLayout {
        relocation_offset,
        bitmap_offset,
        payload_size,
    })
}

static UNLINKED_CODE_BLOCK_VTABLE: &VTable = &VTable {
    alignment: align_of::<UnlinkedCodeBlock>(),
    compute_alignment: None,
    instance_size: size_of::<UnlinkedCodeBlock>(),
    compute_size: Some({
        extern "C" fn compute_unlinked_code_block_size(obj: GCObject) -> usize {
            unsafe {
                let obj = obj.to_address().as_ref::<UnlinkedCodeBlock<'static>>();
                unlinked_code_block_layout(
                    obj.code_len as usize,
                    obj.relocation_count as usize,
                    obj.loaded_data_value_bitmap_word_count as usize,
                )
                .expect("unlinked code block layout is too large")
                .payload_size
            }
        }

        compute_unlinked_code_block_size
    }),
    trace: {
        extern "C" fn trace_unlinked_code_block(obj: GCObject, vis: &mut Visitor) {
            unsafe {
                let obj = obj.to_address().as_mut_ref::<UnlinkedCodeBlock<'static>>();
                obj.trace(vis);
            }
        }

        trace_unlinked_code_block
    },
    weak_proc: {
        extern "C" fn process_weak_unlinked_code_block(
            obj: GCObject,
            weak_processor: &mut WeakProcessor,
        ) {
            unsafe {
                let obj = obj.to_address().as_mut_ref::<UnlinkedCodeBlock<'static>>();
                obj.process_weak_refs(weak_processor);
            }
        }

        process_weak_unlinked_code_block
    },
    type_name: "unlinked-code-block",
};

static UNLINKED_CODE_BLOCK_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    UNLINKED_CODE_BLOCK_VTABLE,
    TypeCode8::UNLINKED_CODEBLOCK.bits() as u16,
);

pub static UNLINKED_CODE_BLOCK_INFO: &'static HeapTypeInfo = &UNLINKED_CODE_BLOCK_INFO_VALUE;

impl<'gc> UnlinkedCodeBlock<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        code: &[u8],
        entry_offset: u32,
        relocations: &[UnlinkedRelocation],
        loaded_data_value_bitmap: &[usize],
    ) -> Gc<'gc, Self> {
        unsafe {
            let code_len =
                u32::try_from(code.len()).expect("unlinked code block code is too large");
            let relocation_count = u32::try_from(relocations.len())
                .expect("unlinked code block relocation table is too large");
            let bitmap_word_count = u32::try_from(loaded_data_value_bitmap.len())
                .expect("unlinked code block value bitmap is too large");
            let layout = unlinked_code_block_layout(
                code.len(),
                relocations.len(),
                loaded_data_value_bitmap.len(),
            )
            .expect("unlinked code block is too large");
            let size = size_of::<Self>()
                .checked_add(layout.payload_size)
                .expect("unlinked code block is too large");

            let alloc = ctx.raw_allocate_with_info(
                size,
                align_of::<Self>(),
                UNLINKED_CODE_BLOCK_INFO,
                AllocationSemantics::Default,
            );
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.entry_offset = entry_offset;
            this.code_len = code_len;
            this.relocation_count = relocation_count;
            this.loaded_data_value_bitmap_word_count = bitmap_word_count;
            this._marker = std::marker::PhantomData;

            let payload = this.payload.as_mut_ptr().cast::<u8>();
            if !code.is_empty() {
                std::ptr::copy_nonoverlapping(code.as_ptr(), payload, code.len());
            }
            if !relocations.is_empty() {
                std::ptr::copy_nonoverlapping(
                    relocations.as_ptr(),
                    payload
                        .add(layout.relocation_offset)
                        .cast::<UnlinkedRelocation>(),
                    relocations.len(),
                );
            }
            if !loaded_data_value_bitmap.is_empty() {
                std::ptr::copy_nonoverlapping(
                    loaded_data_value_bitmap.as_ptr(),
                    payload.add(layout.bitmap_offset).cast::<usize>(),
                    loaded_data_value_bitmap.len(),
                );
            }

            Gc::from_gcobj(alloc)
        }
    }

    pub fn empty(ctx: Context<'gc>) -> Gc<'gc, Self> {
        Self::new(ctx, &[], 0, &[], &[])
    }

    pub fn code(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.payload.as_ptr().cast(), self.code_len as usize) }
    }

    pub fn relocations(&self) -> &[UnlinkedRelocation] {
        let layout = unlinked_code_block_layout(
            self.code_len as usize,
            self.relocation_count as usize,
            self.loaded_data_value_bitmap_word_count as usize,
        )
        .expect("unlinked code block layout is too large");
        unsafe {
            std::slice::from_raw_parts(
                self.payload
                    .as_ptr()
                    .cast::<u8>()
                    .add(layout.relocation_offset)
                    .cast(),
                self.relocation_count as usize,
            )
        }
    }

    pub fn loaded_data_value_bitmap(&self) -> &[usize] {
        let layout = unlinked_code_block_layout(
            self.code_len as usize,
            self.relocation_count as usize,
            self.loaded_data_value_bitmap_word_count as usize,
        )
        .expect("unlinked code block layout is too large");
        unsafe {
            std::slice::from_raw_parts(
                self.payload
                    .as_ptr()
                    .cast::<u8>()
                    .add(layout.bitmap_offset)
                    .cast(),
                self.loaded_data_value_bitmap_word_count as usize,
            )
        }
    }
}

unsafe impl<'gc> Tagged for UnlinkedCodeBlock<'gc> {
    const TC8: TypeCode8 = TypeCode8::UNLINKED_CODEBLOCK;

    const TYPE_NAME: &'static str = "unlinked-code-block";
}

unsafe impl<'gc> Trace for UnlinkedCodeBlock<'gc> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {}

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

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
        let unlinked = UnlinkedCodeBlock::empty(ctx);
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
                unlinked,
                loaded_data_base: Address::ZERO,
                loaded_data_slot_count: 0,
                span_initialized: Cell::new(false),
                span_finalized: Cell::new(true),
                span_borrowed: Cell::new(false),
                span: UnsafeCell::new(MaybeUninit::uninit()),
                code_len: 0,
                code: [],
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

    pub fn new_loaded(
        ctx: Context<'gc>,
        entrypoint: Address,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
        unlinked: Gc<'gc, UnlinkedCodeBlock<'gc>>,
        span: CodeSpan,
    ) -> Gc<'gc, Self> {
        Self::new_loaded_with_data(
            ctx,
            entrypoint,
            arity,
            is_cont,
            metadata,
            unlinked,
            span,
            Address::ZERO,
            0,
        )
    }

    pub fn new_loaded_with_data(
        ctx: Context<'gc>,
        entrypoint: Address,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
        unlinked: Gc<'gc, UnlinkedCodeBlock<'gc>>,
        span: CodeSpan,
        loaded_data_base: Address,
        loaded_data_slot_count: u32,
    ) -> Gc<'gc, Self> {
        let code_block = Gc::new_with_info(
            *ctx,
            Self {
                entrypoint,
                kind: CodeBlockKind::Loaded,
                arity,
                flags: if is_cont {
                    CodeBlockFlags::CONTINUATION
                } else {
                    CodeBlockFlags::empty()
                },
                metadata: Lock::new(Self::normalize_metadata(metadata)),
                unlinked,
                loaded_data_base,
                loaded_data_slot_count,
                span_initialized: Cell::new(true),
                span_finalized: Cell::new(false),
                span_borrowed: Cell::new(false),
                span: UnsafeCell::new(MaybeUninit::new(span.into_raw())),
                code_len: 0,
                code: [],
            },
            CODE_BLOCK_INFO,
        );
        ctx.finalizers()
            .register_finalizer(&CODE_BLOCK_FINALIZERS, code_block);
        code_block
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
            let unlinked = UnlinkedCodeBlock::empty(ctx);
            let size = size_of::<Self>() + code.len();
            let alignment = align_of::<Self>();
            let info = CODE_BLOCK_BYTECODE_INFO;
            let semantics = AllocationSemantics::Default;
            let alloc = ctx.raw_allocate_with_info(size, alignment, info, semantics);
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.entrypoint = entrypoint;
            this.kind = CodeBlockKind::Bytecode;
            this.arity = arity;
            this.flags = if _is_cont {
                CodeBlockFlags::CONTINUATION
            } else {
                CodeBlockFlags::empty()
            };
            this.metadata = Lock::new(metadata);
            this.unlinked = unlinked;
            this.loaded_data_base = Address::ZERO;
            this.loaded_data_slot_count = 0;
            this.span_initialized = Cell::new(false);
            this.span_finalized = Cell::new(true);
            this.span_borrowed = Cell::new(false);
            this.span = UnsafeCell::new(MaybeUninit::uninit());
            this.code_len = code.len() as u32;
            std::ptr::copy_nonoverlapping(code.as_ptr(), this.code.as_mut_ptr().cast(), code.len());
            Gc::from_gcobj(alloc)
        }
    }

    pub fn code(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.code.as_ptr().cast(), self.code_len as usize) }
    }

    pub fn code_len(&self) -> usize {
        self.code_len as usize
    }

    #[cfg(test)]
    pub(crate) fn loaded_data_value_bitmap(&self) -> &[usize] {
        self.unlinked.loaded_data_value_bitmap()
    }

    pub(crate) fn with_live_span<R>(
        &self,
        f: impl FnOnce(&mut Span) -> io::Result<R>,
    ) -> io::Result<R> {
        if !self.has_live_span() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "code block has no live code span",
            ));
        }
        if self.span_borrowed.replace(true) {
            return Err(io::Error::other("code block span is already borrowed"));
        }
        struct SpanBorrowGuard<'a>(&'a Cell<bool>);
        impl Drop for SpanBorrowGuard<'_> {
            fn drop(&mut self) {
                self.0.set(false);
            }
        }
        let _guard = SpanBorrowGuard(&self.span_borrowed);
        let span = unsafe { &mut *(*self.span.get()).as_mut_ptr() };
        f(span)
    }

    pub fn has_live_span(&self) -> bool {
        self.span_initialized.get() && !self.span_finalized.get()
    }

    pub unsafe fn take_span_for_finalization(&self) -> Option<CodeSpan> {
        if self.span_initialized.get() && !self.span_finalized.get() && !self.span_borrowed.get() {
            self.span_finalized.set(true);
            Some(CodeSpan::from_raw(unsafe {
                (*self.span.get()).as_ptr().read()
            }))
        } else {
            None
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
        visitor.trace(&mut self.unlinked);
        if !self.loaded_data_base.is_zero() {
            let bitmap = self.unlinked.loaded_data_value_bitmap();
            let bits_per_word = usize::BITS as u32;
            let count = self.loaded_data_slot_count;
            for index in 0..count {
                let word_index = (index / bits_per_word) as usize;
                let bit_index = index % bits_per_word;
                let Some(word) = bitmap.get(word_index) else {
                    break;
                };
                if (word & (1usize << bit_index)) == 0 {
                    continue;
                }
                let slot = (self.loaded_data_base.as_usize()
                    + index as usize * std::mem::size_of::<Value<'static>>())
                    as *mut Value<'static>;
                unsafe {
                    visitor.trace(&mut *slot);
                }
            }
        }
        match &mut self.kind {
            CodeBlockKind::AOT
            | CodeBlockKind::NativeProc
            | CodeBlockKind::Loaded
            | CodeBlockKind::Bytecode => {}
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

pub enum CodeBlockKind {
    AOT,
    NativeProc,
    Loaded,
    Bytecode,
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
    fn loaded_code_block_owns_span_and_unlinked_code() {
        with_ctx(|ctx| {
            let mut memory = crate::runtime::code_memory::CodeMemory::new();
            let loaded = memory.allocate_copy(&[0xc3]).expect("allocate code span");
            let entrypoint = loaded.entrypoint;
            let unlinked = UnlinkedCodeBlock::new(ctx, &[0xc3], 0, &[], &[]);
            let metadata = Value::cons(ctx, ctx.intern("name"), ctx.str("loaded"));
            let code_block = CodeBlock::new_loaded_with_data(
                ctx,
                entrypoint,
                CodeArity::new(2),
                true,
                metadata,
                unlinked,
                loaded.into_span(),
                Address::ZERO,
                0,
            );

            assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
            assert!(code_block.has_live_span());
            assert_eq!(code_block.unlinked.code(), [0xc3]);
            assert!(code_block.flags.contains(CodeBlockFlags::CONTINUATION));
            assert_eq!(code_block.arity.fixed_arity(), 2);
            assert_eq!(code_block.metadata.get(), metadata);

            let span = unsafe {
                code_block
                    .take_span_for_finalization()
                    .expect("span is present")
            };
            assert!(!code_block.has_live_span());
            assert!(unsafe { code_block.take_span_for_finalization() }.is_none());
            memory.release_span(span).expect("release code span");
        });
    }

    #[test]
    fn native_code_block_does_not_initialize_span() {
        with_ctx(|ctx| {
            let code_block = CodeBlock::new_native(
                ctx,
                Address::from_ptr(test_native_proc as *const ()),
                CodeArity::new(0),
                false,
                Value::new(false),
            );

            assert!(!code_block.has_live_span());
            assert!(code_block.unlinked.code().is_empty());
        });
    }

    #[test]
    fn code_block_span_can_be_taken_for_finalization_once() {
        with_ctx(|ctx| {
            let mut memory = crate::runtime::code_memory::CodeMemory::new();
            let loaded = memory.allocate_copy(&[0xc3]).expect("allocate code");
            let entrypoint = loaded.entrypoint;
            let unlinked = UnlinkedCodeBlock::new(ctx, &[0xc3], 0, &[], &[]);
            let code_block = CodeBlock::new_loaded_with_data(
                ctx,
                entrypoint,
                CodeArity::new(0),
                false,
                Value::new(false),
                unlinked,
                loaded.into_span(),
                Address::ZERO,
                0,
            );

            let span = unsafe {
                code_block
                    .take_span_for_finalization()
                    .expect("span is present")
            };
            assert!(unsafe { code_block.take_span_for_finalization() }.is_none());
            memory.release_span(span).expect("release code span");
        });
    }

    #[test]
    fn bytecode_code_block_initializes_kind_flags_and_code_len() {
        with_ctx(|ctx| {
            let code = [1, 2, 3, 4];
            let code_block = CodeBlock::new_bytecode(
                ctx,
                Address::ZERO,
                CodeArity::variadic(1),
                true,
                Value::new(false),
                &code,
            );

            assert!(matches!(code_block.kind, CodeBlockKind::Bytecode));
            assert!(code_block.flags.contains(CodeBlockFlags::CONTINUATION));
            assert_eq!(code_block.code_len(), code.len());
            assert_eq!(code_block.code(), code);
            assert_eq!(code_block.metadata.get(), Value::new(false));
        });
    }

    #[test]
    fn unlinked_code_block_preserves_original_code_relocations_and_bitmap() {
        with_ctx(|ctx| {
            let code = [0xde, 0xad, 0xbe, 0xef, 0x01];
            let relocations = [
                UnlinkedRelocation {
                    offset: 4,
                    kind_tag: 1,
                    target_tag: 2,
                    target_payload: 3,
                    addend: -8,
                    aux_tag: 5,
                },
                UnlinkedRelocation {
                    offset: 12,
                    kind_tag: 6,
                    target_tag: 7,
                    target_payload: 8,
                    addend: 16,
                    aux_tag: 9,
                },
            ];
            let bitmap = [0b101usize, usize::MAX];

            let code_block = UnlinkedCodeBlock::new(ctx, &code, 2, &relocations, &bitmap);

            assert_eq!(code_block.entry_offset, 2);
            assert_eq!(code_block.code(), code);
            assert_eq!(code_block.relocations(), relocations);
            assert_eq!(code_block.loaded_data_value_bitmap(), bitmap);
        });
    }
}
