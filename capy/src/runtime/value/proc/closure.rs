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
    let class_id = ClassId::new(builtin_class_ids::CLOSURE).unwrap();

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

// SAFETY: Closure's Index impl already validates bounds; IndexWrite just permits mutable
// access to the same in-bounds slots. The GC write barrier is handled at the call site.
unsafe impl<'gc> IndexWrite<usize> for Closure<'gc> {}

extern "C" fn trace_closure(obj: GCObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is guaranteed by the GC to point to a valid `Closure` allocated with
    // a closure class header. We iterate exactly `nfree` trailing elements.
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
        _is_native: bool,
    ) -> Gc<'gc, Self> {
        let meta = code_block.metadata.get();

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

        let size = size_of::<Self>() + std::mem::size_of_val(free);
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
            this.code = code_block.entrypoint;
            this.code_block = code_block;

            this.meta = Lock::new(meta);
            this.nfree = free.len();
            for (i, &value) in free.iter().enumerate() {
                this.free.as_mut_ptr().add(i).write(Lock::new(value));
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

        None
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

// SAFETY: Closure stores its class ID in the heap header selected at construction time.
unsafe impl<'gc> ClassTagged for Closure<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::rsgc::object::builtin_class_ids::CLOSURE];

    const TYPE_NAME: &'static str = "procedure";
}

impl<'gc> Closure<'gc> {
    pub fn is_continuation(&self) -> bool {
        heap_header(self).private_variant_flag()
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
        self.code_block.kind == CodeBlockKind::NativeProc
    }
}
