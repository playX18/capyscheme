//! Code blocks: the compiled form of one procedure.

use super::*;
use crate::bytecode::def;
use crate::heap::Space;
use crate::heap::Trace;
use crate::runtime::codegen::addr::CodeEntry;
use crate::runtime::codegen::stubs::CallIcStub;
use crate::runtime::codegen::unit::{CodeUnit, Native};
use crate::runtime::interp::profile::GcCell;
use mmtk::util::Address;
use std::cell::UnsafeCell;
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU32, Ordering};

/// The heap owner of one piece of native code. The machine code itself lives in executable memory
/// outside the heap (`CodeMemory`). This object holds everything native that
/// belongs to it ([`Native`]) and ties its lifetime to the collector: whatever
/// refers to the code (a `CodeBlock`, a call site, a frame running it) refers
/// to the image, and the collector finds the code dead when the image is
/// unreachable. [`reclaim`](crate::runtime::codegen::reclaim) then drops
/// the native part in place.
#[repr(C)]
pub struct CodeImage<'gc> {
    owner: UnsafeCell<Value<'gc>>,
    released: AtomicBool,
    native: UnsafeCell<ManuallyDrop<Native>>,
}

// SAFETY: the native part is written once before the image is published and is
// only read afterwards, apart from the single `release` after the collector
// found the image dead. This is the one place the native code types
// (`CodeUnit`, `CallIcStub`, ...) are asserted thread-safe.
unsafe impl Send for CodeImage<'_> {}
unsafe impl Sync for CodeImage<'_> {}

impl<'gc> CodeImage<'gc> {
    pub(crate) fn header_word() -> u64 {
        class_header_word(
            ClassId::new(builtin_class_ids::CODE_IMAGE).expect("builtin class id is nonzero"),
        )
    }

    pub(crate) fn alloc(ctx: MutationContext<'gc>, native: Native) -> Gc<'gc, Self> {
        let image = ctx.allocate_in(
            Space::Los,
            Self {
                owner: UnsafeCell::new(Value::null()),
                released: AtomicBool::new(false),
                native: UnsafeCell::new(ManuallyDrop::new(native)),
            },
            Self::header_word(),
        );
        // SAFETY: the image is not shared yet, and never moves.
        unsafe { *image.owner.get() = Value::from(image) };
        ctx.finalizers()
            .register_finalizer(&crate::runtime::codegen::reclaim::FINALIZERS, image);
        image
    }

    /// The address of the slot that holds this image, for the code's header.
    pub(crate) fn owner_slot(&self) -> Address {
        Address::from_mut_ptr(self.owner.get())
    }

    fn native(&self) -> &Native {
        debug_assert!(
            !self.released.load(Ordering::Relaxed),
            "image used after release"
        );
        // SAFETY: the native part is dropped only when the image is dead, and
        // a live reference to the image proves it is not.
        unsafe { &*self.native.get() }
    }

    /// The compiled procedure this image owns, if it is one.
    pub(crate) fn unit(&self) -> Option<&CodeUnit> {
        match self.native() {
            Native::Unit(unit) => Some(unit),
            Native::Stub(_) => None,
        }
    }

    /// The call IC stub this image owns, if it is one.
    pub(crate) fn stub(&self) -> Option<&CallIcStub> {
        match self.native() {
            Native::Stub(stub) => Some(stub),
            Native::Unit(_) => None,
        }
    }

    /// Drops the native part, once.
    ///
    /// # Safety
    ///
    /// The collector found the image dead, and nothing refers to the code.
    pub(crate) unsafe fn release(&self) {
        if !self.released.swap(true, Ordering::AcqRel) {
            // SAFETY: by the contract, and dropped once by the swap above.
            unsafe { ManuallyDrop::drop(&mut *self.native.get()) };
        }
    }
}

// SAFETY: holds no heap references.
unsafe impl Trace for CodeImage<'_> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {}
}

// SAFETY: `CLASS_IDS` matches the header written by `alloc`.
unsafe impl<'gc> ClassTagged for CodeImage<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CODE_IMAGE];
    const TYPE_NAME: &'static str = "code-image";
}

/// Instruction stream and constant pool of one code block.
///
/// Layout, low to high addresses:
///
/// ```text
/// +-------------------------------+
/// | header: nconsts:u32           |
/// |         ncode:u32   (bytes)   |
/// |         npool:u32   (words)   |
/// |         mdsize:u32  (bytes)   |
/// +-------------------------------+  data[0]  <- md_ptr (null if mdsize == 0)
/// | metadata area (mdsize bytes)  |
/// +-------------------------------+
/// | const[nconsts-1]              |
/// | ...                           |
/// | const[0]                      |  entry - 24
/// | pool_ptr: *const u32          |  entry - 16   (raw address, NOT traced)
/// | md_ptr:   *mut u8             |  entry - 8    (raw address, NOT traced; null if mdsize == 0)
/// +-------------------------------+
/// | code bytes                    |  <- entry == constant_pointer
/// | pad to 4                      |
/// | aux pool (npool u32 words)    |  <- pool_ptr
/// +-------------------------------+
/// ```
///
/// `constant_pointer` (the interpreter register) is `entry` typed as `*const Value`.
/// Constant `i` is `constant_pointer.sub(3 + i)`. The pool pointer is
/// `*(constant_pointer.sub(2) as *const *const u32)`. The metadata pointer is
/// `*(constant_pointer.sub(1) as *const *mut u8)`. Both pointers point into the same
/// non-moving object, so they never need updating. The metadata area starts at `data[0]`,
/// so the buffer owning a metadata pointer is at a constant offset below it
/// ([`CodeBuffer::md_buffer`]).
#[repr(C)]
pub struct CodeBuffer<'gc> {
    pub nconsts: u32,
    /// Code bytes.
    pub ncode: u32,
    /// Aux pool words.
    pub npool: u32,
    /// Metadata area bytes.
    pub mdsize: u32,
    /// Whether the background compile of this code failed: the block is
    /// then never compiled. Set by the worker, read by the installer.
    pub build_failed: AtomicBool,
    /// The machine code a compile built for this code and no code block
    /// has installed yet, or the installed one.
    pub built: GcCell<CodeImage<'static>>,
    _marker: std::marker::PhantomData<&'gc ()>,
    data: [u64; 0],
}

extern "C" fn trace_code_buffer(obj: GcObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is a `CodeBuffer` allocated by `CodeBuffer::new`, its
    // first `nconsts` data words are initialized values, and its metadata
    // area (if any) was initialized by `MetadataLayout::init`.
    unsafe {
        let buffer = obj.to_address().as_mut_ref::<CodeBuffer>();
        buffer.built.trace(visitor);
        let consts = buffer.consts_base();
        for index in 0..buffer.nconsts as usize {
            visitor.trace(&mut *consts.add(index));
        }
        if buffer.mdsize > 0 {
            let md = buffer.metadata();
            macro_rules! trace_metadata {
                ($(($Op:ident, $Meta:ident, $kind:expr, { $($field:ident : $ty:ty),* })),* $(,)?) => {
                    $(
                        let count = def::metadata_count(md, $kind);
                        for id in 0..count {
                            let entry = &mut *def::$Op::metadata_ptr(md, def::MetadataId(id));
                            $( entry.$field.trace(visitor); )*
                        }
                    )*
                };
            }
            def::for_each_metadata!(trace_metadata);
        }
    }
}

extern "C" fn process_weak_code_buffer(_obj: GcObject, _weak_processor: &mut WeakProcessor) {}

extern "C" fn compute_code_buffer_size(obj: GcObject) -> usize {
    // SAFETY: `obj` is a `CodeBuffer`, the counts are written before the
    // object is reachable.
    unsafe {
        let buffer = obj.to_address().as_ref::<CodeBuffer>();
        CodeBuffer::payload_size(
            buffer.nconsts as usize,
            buffer.ncode as usize,
            buffer.npool as usize,
            buffer.mdsize as usize,
        )
    }
}

fn code_buffer_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CODE_BUFFER).expect("builtin class id is nonzero"),
    )
}

impl<'gc> CodeBuffer<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        alignment: align_of::<CodeBuffer>(),
        compute_alignment: None,
        instance_size: size_of::<CodeBuffer>(),
        compute_size: Some(compute_code_buffer_size),
        trace: trace_code_buffer,
        weak_proc: process_weak_code_buffer,
        type_name: "code-buffer",
    };

    fn payload_size(nconsts: usize, ncode: usize, npool: usize, mdsize: usize) -> usize {
        8 * nconsts + 16 + (ncode.next_multiple_of(4) + 4 * npool).next_multiple_of(8) + mdsize
    }

    /// Allocate a buffer holding `code`, `pool`, `consts` and a metadata area
    /// described by `md`.
    pub fn new(
        ctx: MutationContext<'gc>,
        code: &[u8],
        pool: &[u32],
        consts: &[Value<'gc>],
        md: &def::MetadataLayout,
    ) -> Gc<'gc, Self> {
        assert!(md.align <= 8, "metadata alignment exceeds 8");

        let mdsize = md.size.next_multiple_of(8);
        let nconsts = u32::try_from(consts.len()).expect("too many constants in a code block");
        let ncode = u32::try_from(code.len()).expect("code block is too large");
        let npool = u32::try_from(pool.len()).expect("aux pool is too large");
        let size =
            size_of::<Self>() + Self::payload_size(consts.len(), code.len(), pool.len(), mdsize);
        // SAFETY: the allocation is `size` bytes, the counts are written
        // before anything can trace the object, and every data word is
        // initialized below.
        unsafe {
            let object = ctx.allocate_raw(size, 8, code_buffer_header_word(), Space::NonMoving);
            let this = object.to_address().as_mut_ref::<Self>();
            this.nconsts = nconsts;
            this.ncode = ncode;
            this.npool = npool;
            this.mdsize = u32::try_from(mdsize).expect("metadata area is too large");
            std::ptr::addr_of_mut!(this.build_failed).write(AtomicBool::new(false));
            std::ptr::addr_of_mut!(this.built).write(GcCell::default());
            let md_ptr = this.data.as_mut_ptr().cast::<u8>();
            let data = this.consts_base().cast::<u64>();
            for (index, value) in consts.iter().enumerate() {
                data.add(consts.len() - 1 - index)
                    .cast::<Value>()
                    .write(*value);
            }
            let entry = data.add(consts.len()).add(2) as *mut u8;
            let code_padded = code.len().next_multiple_of(4);
            let pool_ptr = entry.add(code_padded) as *mut u32;
            let payload_end = entry.add((code_padded + 4 * pool.len()).next_multiple_of(8));
            (entry.sub(16) as *mut *const u32).write(pool_ptr);
            (entry.sub(8) as *mut *mut u8).write(if mdsize == 0 {
                std::ptr::null_mut()
            } else {
                md_ptr
            });
            std::ptr::copy_nonoverlapping(code.as_ptr(), entry, code.len());
            std::ptr::write_bytes(entry.add(code.len()), 0, code_padded - code.len());
            std::ptr::copy_nonoverlapping(pool.as_ptr(), pool_ptr, pool.len());
            let pool_end = pool_ptr.add(pool.len()) as *mut u8;
            std::ptr::write_bytes(pool_end, 0, payload_end.offset_from(pool_end) as usize);
            if md.size > 0 {
                md.init(md_ptr);
                Self::bind_call_links(md_ptr, md_ptr as usize - object.to_address().as_usize());
                std::ptr::write_bytes(md_ptr.add(md.size), 0, mdsize - md.size);
            }
            let buffer: Gc<'gc, Self> = Gc::from_gc_object(object);
            // The buffer is not a nursery object, so the constants just
            // stored have to be remembered.
            ctx.remember_new_object(object);
            buffer
        }
    }

    /// Tells every call link in the area at `md` how far it sits from the
    /// buffer start, `md_offset` being the area's own offset.
    ///
    /// # Safety
    /// `md` was just initialized by `MetadataLayout::init`.
    unsafe fn bind_call_links(md: *mut u8, md_offset: usize) {
        macro_rules! bind {
            ($($Op:ident),*) => {$(
                for id in 0..def::metadata_count(md, def::$Op::METADATA_KIND) {
                    let entry = def::$Op::metadata_ptr(md, def::MetadataId(id));
                    (*entry).link.bind_buffer(entry as usize - md as usize + md_offset);
                }
            )*};
        }
        // SAFETY: each id is below the kind's count, so it names an entry.
        unsafe {
            bind!(OpCall, OpTailCall, OpCallSelf, OpTailCallSelf);
        }
    }

    /// Address of the first instruction.
    pub fn entry(&self) -> *const u8 {
        // SAFETY: the data area holds `nconsts` values, two pointers, then
        // the code.
        unsafe {
            self.consts_base()
                .add(self.nconsts as usize + 2)
                .cast::<u8>()
        }
    }

    /// Start of the constant words, just past the metadata area.
    fn consts_base(&self) -> *mut Value<'gc> {
        // SAFETY: the metadata area (`mdsize` bytes) precedes the constants.
        unsafe {
            self.data
                .as_ptr()
                .cast::<u8>()
                .add(self.mdsize as usize)
                .cast::<Value<'gc>>()
                .cast_mut()
        }
    }

    pub fn code(&self) -> &[u8] {
        // SAFETY: `ncode` instruction bytes follow the entry.
        unsafe { std::slice::from_raw_parts(self.entry(), self.ncode as usize) }
    }

    pub fn pool(&self) -> &[u32] {
        // SAFETY: `npool` words follow the code, 4-aligned.
        unsafe {
            let entry = self.entry();
            let code_padded = (self.ncode as usize).next_multiple_of(4);
            std::slice::from_raw_parts(entry.add(code_padded).cast::<u32>(), self.npool as usize)
        }
    }

    /// The metadata area, or null when the code block has no metadata.
    pub fn metadata(&self) -> *mut u8 {
        // SAFETY: the pointer below the entry was written by `new`.
        unsafe { *(self.entry().sub(8) as *const *mut u8) }
    }

    /// The code buffer holding the metadata area at `md`, for the write
    /// barrier on cache fills. The area starts at the buffer's first data
    /// word and the buffer never moves, so this is a constant offset.
    ///
    /// # Safety
    ///
    /// `md` is a non-null area initialized by `MetadataLayout::init`
    /// inside a live `CodeBuffer` created by `new`, which outlives `'gc`.
    pub unsafe fn md_buffer(md: *const u8) -> Gc<'gc, Self> {
        // SAFETY: by the contract above the area is `data[0]` of a live buffer.
        unsafe {
            let addr = md.sub(std::mem::offset_of!(Self, data)) as usize;
            Gc::from_gc_object(GcObject::from_address(Address::from_usize(addr)))
        }
    }

    /// The metadata area of this buffer (the inverse of
    /// [`Self::md_buffer`]): its first data word.
    pub fn metadata_area(&self) -> *mut u8 {
        // SAFETY: `data` is the buffer's first data word, in bounds of the
        // allocation by construction.
        unsafe { (self as *const Self as *mut u8).add(std::mem::offset_of!(Self, data)) }
    }

    /// Slot of constant `index`.
    pub fn constant_slot(&self, index: usize) -> *mut Value<'gc> {
        debug_assert!(index < self.nconsts as usize, "constant index out of range");
        // SAFETY: bounds checked above, constants are stored in reverse.
        unsafe { self.consts_base().add(self.nconsts as usize - 1 - index) }
    }

    pub fn constant(&self, index: usize) -> Value<'gc> {
        // SAFETY: `constant_slot` returns an initialized slot.
        unsafe { *self.constant_slot(index) }
    }

    /// Whether `ip` points at an instruction of this buffer.
    pub fn contains(&self, ip: *const u8) -> bool {
        let start = self.entry();
        // SAFETY: one-past-the-end pointer of the same allocation.
        let end = unsafe { start.add(self.ncode as usize) };
        ip >= start && ip < end
    }
}

// SAFETY: CodeBuffer stores its class ID in the heap header selected at construction time.
unsafe impl<'gc> ClassTagged for CodeBuffer<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CODE_BUFFER];

    const TYPE_NAME: &'static str = "code-buffer";
}

/// Root holder for the shared empty buffer behind native code blocks.
struct NativeBufferRoot<'gc> {
    buffer: Gc<'gc, CodeBuffer<'gc>>,
}

// SAFETY: the field is traced.
unsafe impl<'gc> Trace for NativeBufferRoot<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.buffer);
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

type RootedNativeBuffer = crate::Rootable!(NativeBufferRoot<'_>);
static NATIVE_BUFFER: std::sync::OnceLock<crate::heap::Global<RootedNativeBuffer>> =
    std::sync::OnceLock::new();

pub struct CodeBlock<'gc> {
    /// Address of the entry instruction, the bytecode entry (`Closure::code`
    /// is the machine entry under the template interpreter).
    pub entrypoint: Address,
    pub kind: CodeBlockKind,
    pub arity: CodeArity,
    pub flags: CodeBlockFlags,
    /// Largest extent of a frame of this procedure, in slots: its registers
    /// plus the header and arguments of the largest call it makes.
    pub frame_size: u32,
    pub metadata: Lock<Value<'gc>>,
    pub buffer: Gc<'gc, CodeBuffer<'gc>>,
    /// Source locations: `#(#spans #(runs...))` (see [`pack_srcmap`]), or
    /// `#f` when stripped or absent. `spans` holds deduplicated span
    /// values (ids count from 1, id 0 means no span), `runs` holds
    /// `(start len flags id)` fixnum quadruples with disjoint,
    /// increasing byte-offset ranges.
    pub srcmap: Lock<Value<'gc>>,
    /// Entry address of JIT code for this procedure, none yet, or marked
    /// never to compile. Published with Release after [`machine`](Self::machine)
    /// is set, so a reader that sees the entry finds the owner.
    pub jit_entry: CodeEntry,
    /// The owner of that code (`codegen/jit.rs`), if any: it keeps the code
    /// alive.
    pub machine: Lock<Option<Gc<'gc, CodeImage<'gc>>>>,
    /// Heat counted by the `enter`/`loop_hint` fast paths: one counter fed
    /// by entries and back-edges (HotSpot's summed semantics). The
    /// generated-code bump is a plain load/add/store, so lost updates
    /// under threads only delay tier-up.
    pub hot_count: AtomicU32,
    /// Tier-up single-flight bit (HotSpot's `queued_for_compilation`): a
    /// [`Promotion`] discriminant. A threshold fire CASes `Interpreted` to
    /// `Submitted` and submits exactly one compile, duplicates are dropped.
    pub promotion: AtomicU8,
}

fn code_block_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CODE_BLOCK).expect("builtin class id is nonzero"),
    )
}

/// Everything needed to build a bytecode [`CodeBlock`].
pub struct CodeBlockInit<'a, 'gc> {
    pub code: &'a [u8],
    pub pool: &'a [u32],
    pub metadata: def::MetadataLayout,
    pub constants: &'a [Value<'gc>],
    pub arity: CodeArity,
    pub frame_size: u32,
    pub info: Value<'gc>,
    pub srcmap: Value<'gc>,
}

impl<'gc> CodeBlock<'gc> {
    fn normalize_metadata(metadata: Value<'gc>) -> Value<'gc> {
        if metadata == Value::new(false) {
            Value::null()
        } else {
            metadata
        }
    }

    /// Build a code block for compiled bytecode.
    pub fn new_bytecode(ctx: MutationContext<'gc>, init: CodeBlockInit<'_, 'gc>) -> Gc<'gc, Self> {
        let buffer = CodeBuffer::new(ctx, init.code, init.pool, init.constants, &init.metadata);
        if crate::runtime::profile::armed() {
            crate::runtime::profile::maps::publish_interp(
                ctx,
                buffer.entry() as u64,
                init.code,
                init.srcmap,
                init.info,
            );
        }
        ctx.allocate_with(
            Self {
                entrypoint: Address::from_ptr(buffer.entry()),
                kind: CodeBlockKind::Bytecode,
                arity: init.arity,
                flags: CodeBlockFlags::empty(),
                frame_size: init.frame_size,
                metadata: Lock::new(Self::normalize_metadata(init.info)),
                buffer,
                srcmap: Lock::new(init.srcmap),
                jit_entry: CodeEntry::new(),
                machine: Lock::new(None),
                hot_count: AtomicU32::new(0),
                promotion: AtomicU8::new(Promotion::Interpreted as u8),
            },
            code_block_header_word(),
        )
    }

    /// The shared buffer behind every native code block. Natives run on a Rust
    /// path chosen by closure kind, so the entry address is never executed by
    /// the Rust engine, the template interpreter's `interpreter_enter` still dispatches through it on
    /// its way to the bail handler, so it holds one `nop` rather than no
    /// bytes at all.
    fn native_buffer(ctx: MutationContext<'gc>) -> Gc<'gc, CodeBuffer<'gc>> {
        NATIVE_BUFFER
            .get_or_init(|| {
                let layout = def::MetadataLayout::new(&[0; def::NUM_METADATA_KINDS]);
                let nop = [def::OpcodeId::OpNop as u8];
                let buffer = CodeBuffer::new(ctx, &nop, &[], &[], &layout);
                crate::heap::Global::new(NativeBufferRoot { buffer })
            })
            .fetch(ctx)
            .buffer
    }

    /// Build the code block of a native procedure or continuation.
    pub fn new_native(
        ctx: MutationContext<'gc>,
        arity: CodeArity,
        is_cont: bool,
        metadata: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let buffer = Self::native_buffer(ctx);
        ctx.allocate_with(
            Self {
                entrypoint: Address::from_ptr(buffer.entry()),
                kind: CodeBlockKind::NativeProc,
                arity,
                flags: if is_cont {
                    CodeBlockFlags::CONTINUATION
                } else {
                    CodeBlockFlags::empty()
                },
                frame_size: 0,
                metadata: Lock::new(Self::normalize_metadata(metadata)),
                buffer,
                srcmap: Lock::new(Value::new(false)),
                jit_entry: CodeEntry::new(),
                machine: Lock::new(None),
                hot_count: AtomicU32::new(0),
                promotion: AtomicU8::new(Promotion::Interpreted as u8),
            },
            code_block_header_word(),
        )
    }

    /// A code block for a procedure of no arguments that returns its own
    /// closure, for tests that need a bytecode procedure to exist.
    #[cfg(test)]
    pub fn new_for_tests(ctx: MutationContext<'gc>, metadata: Value<'gc>) -> Gc<'gc, Self> {
        use crate::bytecode::def::{Count, Instruction, OpEnter, OpReturn, Register};
        let code = def::encode_all(&[
            Instruction::OpEnter(OpEnter {
                argc: Count(0),
                frame: Count(3),
            }),
            Instruction::OpReturn(OpReturn { src: Register(1) }),
        ]);
        Self::new_bytecode(
            ctx,
            CodeBlockInit {
                code: &code,
                pool: &[],
                metadata: def::MetadataLayout::new(&[0; def::NUM_METADATA_KINDS]),
                constants: &[],
                arity: CodeArity::new(0),
                frame_size: 3,
                info: metadata,
                srcmap: Value::new(false),
            },
        )
    }

    pub fn is_native(&self) -> bool {
        self.kind == CodeBlockKind::NativeProc
    }

    pub fn code(&self) -> &[u8] {
        self.buffer.code()
    }

    /// Byte offset of `ip` from the entry, if it points into this code block.
    pub fn pc_of(&self, ip: *const u8) -> Option<usize> {
        if !self.buffer.contains(ip) {
            return None;
        }
        Some(ip as usize - self.buffer.entry() as usize)
    }

    /// Source location recorded for the instruction containing `pc`: an
    /// exact containment hit, never the left neighbor. `None` means no
    /// run covers `pc` (a gap, or a stripped/absent map).
    pub fn source_at_exact(&self, pc: usize) -> Option<SpanHit<'gc>> {
        lookup_srcmap(self.srcmap.get(), pc)
    }
}

/// Look up `pc` in a packed [`CodeBlock::srcmap`] value without a code
/// block (shared by [`CodeBlock::source_at_exact`] and the disassembler).
pub fn lookup_srcmap(map: Value<'_>, pc: usize) -> Option<SpanHit<'_>> {
    if !map.is::<Vector>() {
        return None;
    }
    let map = map.downcast::<Vector>();
    if map.len() != 2 {
        return None;
    }
    let spans = map[0].get();
    let runs = map[1].get();
    if !spans.is::<Vector>() || !runs.is::<Vector>() {
        return None;
    }
    let (spans, runs) = (spans.downcast::<Vector>(), runs.downcast::<Vector>());
    if runs.len() % 4 != 0 {
        return None;
    }
    let count = runs.len() / 4;
    let (mut lo, mut hi) = (0usize, count);
    while lo < hi {
        let mid = (lo + hi) / 2;
        let Some(start) = runs[mid * 4].get().int32() else {
            return None;
        };
        if (start as usize) <= pc {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    if lo == 0 {
        return None;
    }
    let base = (lo - 1) * 4;
    let (Some(start), Some(len), Some(flags), Some(id)) = (
        runs[base].get().int32(),
        runs[base + 1].get().int32(),
        runs[base + 2].get().int32(),
        runs[base + 3].get().int32(),
    ) else {
        return None;
    };
    if start < 0 || len <= 0 || pc >= start as usize + len as usize {
        return None;
    }
    let span = if id == 0 {
        Value::new(false)
    } else if id >= 1 && (id as usize) <= spans.len() {
        spans[id as usize - 1].get()
    } else {
        return None;
    };
    Some(SpanHit {
        span,
        flags: flags as u8,
    })
}

/// Pack deduplicated `spans` and `(start, len, flags, id)` `runs`
/// into the [`CodeBlock::srcmap`] shape. Span ids count from 1,
/// id 0 with [`SRC_FLAG_SYNTHETIC`] marks generated code that
/// carries no span.
pub fn pack_srcmap<'gc>(
    ctx: MutationContext<'gc>,
    spans: &[Value<'gc>],
    runs: &[(u32, u32, u8, u32)],
) -> Value<'gc> {
    let mut flat = Vec::with_capacity(runs.len() * 4);
    for (start, len, flags, id) in runs {
        flat.push(Value::new(*start as i32));
        flat.push(Value::new(*len as i32));
        flat.push(Value::new(*flags as i32));
        flat.push(Value::new(*id as i32));
    }
    Vector::from_slice(
        ctx,
        &[
            Vector::from_slice(ctx, spans).into(),
            Vector::from_slice(ctx, &flat).into(),
        ],
    )
    .into()
}

/// Run flags in [`CodeBlock::srcmap`] rows (shared with the FASL wire
/// format and bytecode lowering).
/// Generated code with no span (span id 0): attributed to nothing,
/// never to a neighbor.
pub const SRC_FLAG_SYNTHETIC: u8 = 0x01;
/// Present but hidden from debugger stepping (reserved for P3b, never
/// set by lowering).
pub const SRC_FLAG_HIDDEN: u8 = 0x02;

/// An exact [`CodeBlock::source_at_exact`] hit: the recorded span (or
/// `#f` for synthetic runs) plus the run's flags.
pub struct SpanHit<'gc> {
    pub span: Value<'gc>,
    pub flags: u8,
}

/// `file:line:col` of a code block's own `source` metadata (the
/// procedure's span, as stored by `procedure_metadata`), for JIT image
/// symbol names. `None` when absent or malformed.
pub(crate) fn procedure_span_short<'gc>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
) -> Option<String> {
    let metadata = code_block.metadata.get();
    if !metadata.is_pair() {
        return None;
    }
    let source = metadata.assq(ctx.intern("source"))?.cdr();
    span_short(source)
}

/// `file:line:col` for a 9-vector span, or `None` when the value is not
/// one. Lines print as stored (1-based by the reader convention).
pub fn span_short(span: Value<'_>) -> Option<String> {
    if !span.is::<Vector>() {
        return None;
    }
    let span = span.downcast::<Vector>();
    if span.len() < 3 {
        return None;
    }
    let (file, line, col) = (span[0].get(), span[1].get(), span[2].get());
    if !file.is::<Str>() || !line.is_int32() || !col.is_int32() {
        return None;
    }
    Some(format!(
        "{}:{}:{}",
        file.downcast::<Str>(),
        line.as_int32(),
        col.as_int32()
    ))
}

// SAFETY: `gc` for `CodeBlock` upholds all trait invariants
unsafe impl<'gc> ClassTagged for CodeBlock<'gc> {
    const CLASS_IDS: &'static [u32] = &[crate::heap::object::builtin_class_ids::CODE_BLOCK];

    const TYPE_NAME: &'static str = "code-block";
}

// SAFETY: `gc` for `CodeBlock` upholds all trait invariants
unsafe impl<'gc> Trace for CodeBlock<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.metadata);
        visitor.trace(&mut self.buffer);
        visitor.trace(&mut self.srcmap);
        visitor.trace(&mut self.machine);
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}
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

    pub fn raw(&self) -> i32 {
        self.0
    }
}

bitflags::bitflags! {
    pub struct CodeBlockFlags: u32 {
        const CONTINUATION = 1 << 0;

    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CodeBlockKind {
    /// Compiled bytecode.
    Bytecode,
    /// Native procedure or continuation, the code is a shared stub.
    NativeProc,
}

/// Tier-up single-flight state, stored as a `u8` discriminant in
/// [`CodeBlock::promotion`]. `Submitted` covers the whole pipeline from
/// queueing to install: while it holds, further threshold fires are
/// dropped and the heat counter simply runs again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Promotion {
    /// Interpreting, a threshold fire may submit a compile.
    Interpreted = 0,
    /// A compile was submitted (queued, compiling, or finished and
    /// awaiting install), threshold fires are dropped.
    Submitted = 1,
    /// The compiled code is published: `jit_entry` is set (or the block is
    /// marked never to compile).
    Installed = 2,
}
