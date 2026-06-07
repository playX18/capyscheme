//! Executable code blocks and their finalization lifecycle.

use super::*;

pub struct CodeBlock<'gc> {
    pub entrypoint: Address,
    pub kind: CodeBlockKind,
    pub arity: CodeArity,
    pub flags: CodeBlockFlags,
    pub metadata: Lock<Value<'gc>>,
    pub unlinked: Gc<'gc, RelocatableCodeBlock<'gc>>,
    pub loaded_data_base: Address,
    pub loaded_data_slot_count: u32,
    span_initialized: Cell<bool>,
    span_finalized: Cell<bool>,
    span_borrowed: Cell<bool>,
    span: UnsafeCell<MaybeUninit<Span>>,
}

// Finalizer queues move `CodeBlock` object references between GC worker and
// scheduler threads. The queue only reads the span ownership guard and releases
// the JIT span through the runtime-wide `CodeMemory` lock.
unsafe impl<'gc> Send for CodeBlock<'gc> {}

fn code_block_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::CODE_BLOCK).unwrap())
}

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
                if code_block.take_span_for_finalization().is_some() {
                    // A dead CodeBlock can still have return addresses into its
                    // executable span on native stacks. Mark the span finalized,
                    // but keep the mapping valid until the process exits.
                }
            }
        }
    }
}

struct CodeBlockInit<'gc> {
    entrypoint: Address,
    kind: CodeBlockKind,
    arity: CodeArity,
    is_continuation: bool,
    metadata: Value<'gc>,
    unlinked: Gc<'gc, RelocatableCodeBlock<'gc>>,
    span: Option<CodeSpan>,
    loaded_data_base: Address,
    loaded_data_slot_count: u32,
}

pub struct LoadedCodeBlockInit<'gc> {
    pub entrypoint: Address,
    pub arity: CodeArity,
    pub is_continuation: bool,
    pub metadata: Value<'gc>,
    pub unlinked: Gc<'gc, RelocatableCodeBlock<'gc>>,
    pub span: CodeSpan,
    pub loaded_data_base: Address,
    pub loaded_data_slot_count: u32,
}

impl<'gc> CodeBlock<'gc> {
    fn normalize_metadata(metadata: Value<'gc>) -> Value<'gc> {
        if metadata == Value::new(false) {
            Value::null()
        } else {
            metadata
        }
    }

    fn allocate(ctx: Context<'gc>, init: CodeBlockInit<'gc>) -> Gc<'gc, Self> {
        let (span_initialized, span_finalized, span) = match init.span {
            Some(span) => (true, false, MaybeUninit::new(span.into_raw())),
            None => (false, true, MaybeUninit::uninit()),
        };

        let code_block = Gc::new_with_header_word(
            *ctx,
            Self {
                entrypoint: init.entrypoint,
                kind: init.kind,
                arity: init.arity,
                flags: if init.is_continuation {
                    CodeBlockFlags::CONTINUATION
                } else {
                    CodeBlockFlags::empty()
                },
                metadata: Lock::new(Self::normalize_metadata(init.metadata)),
                unlinked: init.unlinked,
                loaded_data_base: init.loaded_data_base,
                loaded_data_slot_count: init.loaded_data_slot_count,
                span_initialized: Cell::new(span_initialized),
                span_finalized: Cell::new(span_finalized),
                span_borrowed: Cell::new(false),
                span: UnsafeCell::new(span),
            },
            code_block_header_word(),
        );

        if code_block.has_live_span() {
            ctx.finalizers()
                .register_finalizer(&CODE_BLOCK_FINALIZERS, code_block);
        }

        code_block
    }

    fn new_inner(
        ctx: Context<'gc>,
        entrypoint: Address,
        kind: CodeBlockKind,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Self::allocate(
            ctx,
            CodeBlockInit {
                entrypoint,
                kind,
                arity,
                is_continuation: is_cont,
                metadata,
                unlinked: RelocatableCodeBlock::empty(ctx),
                span: None,
                loaded_data_base: Address::ZERO,
                loaded_data_slot_count: 0,
            },
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
        unlinked: Gc<'gc, RelocatableCodeBlock<'gc>>,
        span: CodeSpan,
    ) -> Gc<'gc, Self> {
        Self::new_loaded_with_data(
            ctx,
            LoadedCodeBlockInit {
                entrypoint,
                arity,
                is_continuation: is_cont,
                metadata,
                unlinked,
                span,
                loaded_data_base: Address::ZERO,
                loaded_data_slot_count: 0,
            },
        )
    }

    pub fn new_loaded_with_data(
        ctx: Context<'gc>,
        init: LoadedCodeBlockInit<'gc>,
    ) -> Gc<'gc, Self> {
        Self::allocate(
            ctx,
            CodeBlockInit {
                entrypoint: init.entrypoint,
                kind: CodeBlockKind::Loaded,
                arity: init.arity,
                is_continuation: init.is_continuation,
                metadata: init.metadata,
                unlinked: init.unlinked,
                span: Some(init.span),
                loaded_data_base: init.loaded_data_base,
                loaded_data_slot_count: init.loaded_data_slot_count,
            },
        )
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

    /// Take ownership of the code span during finalization.
    ///
    /// # Safety
    ///
    /// This must only be called by the finalization path when no other code is
    /// borrowing or executing from the span.
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

unsafe impl<'gc> ClassTagged for CodeBlock<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::CODE_BLOCK];

    const TYPE_NAME: &'static str = "code-block";
}

unsafe impl<'gc> Trace for CodeBlock<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.metadata);
        visitor.trace(&mut self.unlinked);
        if !self.loaded_data_base.is_zero() {
            let bitmap = self.unlinked.loaded_data_value_bitmap();
            let bits_per_word = usize::BITS;
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
            CodeBlockKind::AOT | CodeBlockKind::NativeProc | CodeBlockKind::Loaded => {}
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeBlockKind {
    AOT,
    NativeProc,
    Loaded,
}
