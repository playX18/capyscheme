//! Closure heap objects and closure metadata helpers.

use super::*;

#[repr(C)]
pub struct Closure<'gc> {
    pub code: Address,
    pub code_block: Gc<'gc, CodeBlock<'gc>>,

    pub meta: Lock<Value<'gc>>,
    pub nfree: usize,
    pub free: [Lock<Value<'gc>>; 0],
}

fn closure_header_word(is_cont: bool) -> u64 {
    let class_id = ClassId::new(builtin_class_ids::CLOSURE).expect("builtin class id is nonzero");

    if is_cont {
        class_header_word_with_private_variant_flag(class_id)
    } else {
        class_header_word(class_id)
    }
}

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

// SAFETY: Closure's Index impl already validates bounds. IndexWrite just permits mutable
// access to the same in-bounds slots. The GC write barrier is handled at the call site.
unsafe impl<'gc> IndexWrite<usize> for Closure<'gc> {}

extern "C" fn trace_closure(obj: GcObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is guaranteed by the GC to point to a valid `Closure` allocated with
    // a closure class header. We iterate exactly `nfree` trailing elements.
    unsafe {
        let closure = obj.to_address().as_mut_ref::<Closure>();
        visitor.trace(&mut closure.code_block);
        visitor.trace(&mut closure.meta);
        for i in 0..closure.nfree {
            visitor.trace(
                closure
                    .free
                    .as_mut_ptr()
                    .add(i)
                    .as_mut()
                    .expect("pointer in allocated entry range"),
            );
        }
    }
}

extern "C" fn process_weak(_obj: GcObject, _weak_processor: &mut crate::heap::WeakProcessor) {
    // No weak references in Closure, so do nothing.
}

extern "C" fn compute_closure_size(obj: GcObject) -> usize {
    // SAFETY: `obj` is a valid `Closure` allocated by the GC with a closure class header.
    unsafe {
        let closure = obj.to_address().as_ref::<Closure>();
        size_of::<Value>() * closure.nfree
    }
}

pub static CLOSURE_HOOKS: AllocationHooks = AllocationHooks {
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

    /// The machine entry of a new closure for `code_block`: `interpreter_enter_cp`
    /// for bytecode, `subr_call` for natives, once the template interpreter is loaded, else the
    /// bytecode entry (the Rust engine never reads `code`, so its value
    /// does not matter there).
    pub fn closure_entry(code_block: Gc<'gc, CodeBlock<'gc>>) -> Address {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        if code_block.is_native() {
            if let Some(addr) = crate::runtime::codegen::template_image::subr_call_if_loaded() {
                return Address::from_ptr(addr);
            }
        } else if let Some(addr) =
            crate::runtime::codegen::template_image::interpreter_enter_if_loaded()
        {
            return Address::from_ptr(addr);
        }
        code_block.entrypoint
    }

    pub fn new(
        ctx: MutationContext<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        nfree: usize,
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        Self::new_inner(ctx, code_block, nfree, None, is_cont)
    }

    pub fn new_with_free_vars(
        ctx: MutationContext<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        Self::new_inner(ctx, code_block, free.len(), Some(free), is_cont)
    }

    pub fn new_native(
        ctx: MutationContext<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        Self::new_with_free_vars(ctx, code_block, free, is_cont)
    }

    fn new_inner(
        ctx: MutationContext<'gc>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        nfree: usize,
        free: Option<&[Value<'gc>]>,
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        let meta = code_block.metadata.get();

        let size = size_of::<Self>() + size_of::<Value>() * nfree;
        // SAFETY: We raw-allocate a `Closure` + trailing `free` array via the GC allocator.
        // The closure class hooks ensure proper tracing. We initialize all fields before returning
        // the Gc handle.
        unsafe {
            let ptr = ctx.raw_allocate_with_header_word(
                size,
                align_of::<Self>(),
                closure_header_word(is_cont),
                AllocationSemantics::Default,
            );
            let this = ptr.to_address().as_mut_ref::<Self>();
            this.code = Self::closure_entry(code_block);
            this.code_block = code_block;

            this.meta = Lock::new(meta);
            this.nfree = nfree;
            for i in 0..nfree {
                let value = free.map_or(Value::undefined(), |free| free[i]);
                this.free.as_mut_ptr().add(i).write(Lock::new(value));
            }
            Gc::from_gc_object(ptr)
        }
    }

    pub fn documentation(&self, ctx: MutationContext<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let doc = meta.assq(Symbol::from_str(ctx, "documentation").into());
        doc.map(|d| d.cdr())
    }

    pub fn source(&self, ctx: MutationContext<'gc>) -> Option<(Value<'gc>, u32, u32)> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }

        let src = meta.assq(Symbol::from_str(ctx, "source").into())?;
        let src = src.cdr();

        if src.is::<Vector>() && src.downcast::<Vector>().len() >= 3 {
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

        None
    }

    pub fn name(&self, ctx: MutationContext<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let name = meta.assq(Symbol::from_str(ctx, "name").into());
        name.map(|n| n.cdr())
    }
}

// SAFETY: Closure stores its class ID in the heap header selected at construction time.
unsafe impl<'gc> ClassTagged for Closure<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::heap::object::builtin_class_ids::CLOSURE];

    const TYPE_NAME: &'static str = "procedure";
}

impl<'gc> Closure<'gc> {
    pub fn is_continuation(&self) -> bool {
        heap_header(self).private_variant_flag()
    }

    /// Check if this closure is a continuation produced by `call/cc`.
    pub fn is_reified_continuation(&self, _ctx: MutationContext<'gc>) -> bool {
        crate::runtime::vm::control::is_reified_continuation_closure(self)
    }

    /// Returns winders and continuation marks associated with this reified continuation.
    ///
    /// Marks are returned as a boxed [`ContinuationMarks`] object.
    pub fn reified_continuation_attachments(
        &self,
        ctx: MutationContext<'gc>,
    ) -> Option<(Value<'gc>, Value<'gc>)> {
        if !self.is_reified_continuation(ctx) {
            return None;
        }
        let winders = self[2].get();
        let raw_marks = self[3].get();
        let cmarks = crate::runtime::vm::control::box_continuation_marks(ctx, raw_marks);
        Some((winders, cmarks))
    }

    pub fn is_foreign(&self) -> bool {
        self.code_block.kind == CodeBlockKind::NativeProc
    }

    /// Free variable `index` of `clo`, for interpreter slot traffic.
    pub(crate) fn free_get(clo: Gc<'gc, Self>, index: usize) -> Value<'gc> {
        clo[index].get()
    }

    /// Store free variable `index` of `clo`, for interpreter slot traffic.
    /// The store goes through unchecked, exactly as before: setting a free
    /// variable never was a safepoint and takes no barrier.
    pub(crate) fn free_set(clo: Gc<'gc, Self>, index: usize, value: Value<'gc>) {
        // SAFETY: `clo[index]` is the in-range slot (bounds debug-checked by
        // the `Index` impl), `as_ptr` only re-exposes it for the store.
        unsafe { clo[index].as_ptr().write(value) }
    }

    /// The closure behind a closure value, debug-checked.
    pub(crate) fn from_value(value: Value<'gc>) -> Gc<'gc, Self> {
        value.downcast::<Self>()
    }

    /// The closure behind a closure value, without the class check.
    ///
    /// # Safety
    ///
    /// `value` is a closure, or the old copy of one a copying collection has
    /// already forwarded: its class header is then a forwarding pointer, but
    /// its fields are intact for the rest of the pause.
    pub(crate) unsafe fn from_value_unchecked(value: Value<'gc>) -> Gc<'gc, Self> {
        // SAFETY: by the contract above.
        unsafe { value.downcast_unchecked::<Self>() }
    }

    /// The code block of `clo`.
    pub(crate) fn code_block(clo: Gc<'gc, Self>) -> Gc<'gc, CodeBlock<'gc>> {
        clo.code_block
    }
}
