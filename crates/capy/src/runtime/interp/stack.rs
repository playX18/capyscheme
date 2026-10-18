//! The Scheme stack: stacks, frames, and first-class continuations.
//!
//! Frames grow upward inside a stack. A frame is
//!
//! ```text
//!   fp[0]   return address: a machine address with an `RpHeader` before it
//!   fp[1]   the closure being executed
//!   fp[2..] arguments, then locals
//! ```
//!
//! There is no saved frame pointer. An interpreted caller (`fp[0]` is
//! `INTERP_RETURN`) keeps its bytecode return point in `fp[-1]`, the reserved
//! `r[base-1]` slot, which records the size of the frame it returns into and
//! is also how this module walks the stack.
//!
//! Stack management and first-class continuations follow the design of Chez
//! Scheme. The design follows Hieb, Dybvig and Bruggeman,
//! "Representing Control in the Presence of First-Class Continuations" (PLDI 1990).
//!
//! The thing everything relies on: the live stack, `[scheme_stack,
//! scheme_stack + scheme_stack_size)`, is never part of a continuation
//! object. Frames of a multi-shot continuation are never run or written in
//! place, a one-shot continuation is resumed in place once and is then
//! shot.

use std::cell::Cell;
use std::sync::atomic::Ordering::Relaxed;

use mmtk::AllocationSemantics;

use super::frame::CallFrame;
use super::markers;
use crate::bytecode::def;
use crate::heap::barrier;
use crate::heap::cell::Lock;
use crate::heap::object::{
    AllocationHooks, ClassId, GcObject, builtin_class_ids, class_header_word,
};
use crate::heap::{Gc, collection::Visitor};
use crate::runtime::sabi::{INTERPRETER_FRAME_SIZE, Ra, entry_frame_size, entry_livemask};
use crate::runtime::value::{ByteVector, ClassTagged, Closure, Value};
use crate::runtime::{MutationContext, Tc};
use crate::{WeakProcessor, Write};

pub const DEFAULT_STACK_SIZE: usize = 64 * 1024;
/// Slots kept free above `esp`, for native argument staging
/// and for the frame pushed to run an exception handler.
pub const STACK_SLOP: usize = DEFAULT_STACK_SIZE / 64;
const ONE_SHOT_HEADROOM: usize = STACK_SLOP * 3;
/// `length` and `clength` of a shot one-shot
/// continuation.
const SHOT_1_SHOT_FLAG: isize = -1;
/// `length` of a one-shot continuation that is resumed in place only when it is contiguous with the
/// live stack.
const OPPORTUNISTIC_1_SHOT_FLAG: isize = -2;
/// Most slots copied by one return into a continuation.
const UNDERFLOW_LIMIT: usize = 96;

pub type Ip = *const u8;
pub type Slot<'gc> = *mut Value<'gc>;

pub struct StackState<'gc> {
    /// Frame of the running (or last running) procedure.
    pub fp: Cell<Slot<'gc>>,
    /// First free slot above the live part of the top frame.
    pub sp: Cell<Slot<'gc>>,
    /// Base of the live stack. The frame here returns
    /// through `dounderflow` or `scheme_exit`.
    pub scheme_stack: Cell<Slot<'gc>>,
    /// Slots in the live stack. `Tc::esp` is always
    /// `scheme_stack + scheme_stack_size - STACK_SLOP`.
    pub scheme_stack_size: Cell<usize>,
    /// The bytevector holding the live stack. Each stack is a region of a
    /// non-moving bytevector.
    pub segment: Cell<Value<'gc>>,
    /// The rest of the continuation below `scheme_stack`: a [`Continuation`], or `#f`
    /// at the bottom of an activation (`stack-link`).
    pub stack_link: Cell<Value<'gc>>,
    /// Stacks the thread has left, threaded through their first slots.
    /// Emptied by every collection.
    pub stack_cache: Cell<Slot<'gc>>,
    /// A merged opportunistic continuation object, reused by the next
    /// one-shot reification, or `#f`.
    pub cached_frame: Cell<Value<'gc>>,
    /// Address of the `loop_hint` the top frame is parked at, when the top
    /// frame's live registers come from its mask and not from `fp..sp`.
    pub top_ip: Cell<Ip>,
    /// Innermost Rust-to-Scheme entry.
    pub activation: Cell<*mut Activation<'gc>>,

    /// Call staged by a native procedure (`ReturnCode::Continue`).
    pub call_rator: Cell<Value<'gc>>,
    pub call_k: Cell<Value<'gc>>,
    pub call_argc: Cell<usize>,
    pub call_args: Cell<Slot<'gc>>,
}

impl Default for StackState<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'gc> StackState<'gc> {
    pub fn new() -> Self {
        Self {
            fp: Cell::new(std::ptr::null_mut()),
            sp: Cell::new(std::ptr::null_mut()),
            scheme_stack: Cell::new(std::ptr::null_mut()),
            scheme_stack_size: Cell::new(0),
            segment: Cell::new(Value::new(false)),
            stack_link: Cell::new(Value::new(false)),
            stack_cache: Cell::new(std::ptr::null_mut()),
            cached_frame: Cell::new(Value::new(false)),
            top_ip: Cell::new(std::ptr::null()),
            activation: Cell::new(std::ptr::null_mut()),
            call_rator: Cell::new(Value::undefined()),
            call_k: Cell::new(Value::undefined()),
            call_argc: Cell::new(0),
            call_args: Cell::new(std::ptr::null_mut()),
        }
    }

    /// End of the live stack.
    pub fn end(&self) -> Slot<'gc> {
        self.scheme_stack
            .get()
            .wrapping_add(self.scheme_stack_size.get())
    }
}

fn set_esp(tc: &Tc<'_>) {
    let esp = tc.stack.end().wrapping_sub(STACK_SLOP);
    tc.real_esp.set(esp);
    let current = tc.esp.load(Relaxed);
    if !current.is_null() {
        let _ = tc.esp.compare_exchange(current, esp, Relaxed, Relaxed);
    }
}

/// What a Rust-to-Scheme entry saves of the stack registers, so that leaving
/// the activation, normally or not, puts them back. Lives on the native stack
/// of the entry.
pub struct Activation<'gc> {
    pub prev: *mut Activation<'gc>,
    pub fp: Slot<'gc>,
    pub sp: Slot<'gc>,
    pub scheme_stack: Slot<'gc>,
    pub scheme_stack_size: usize,
    pub segment: Value<'gc>,
    pub stack_link: Value<'gc>,
    pub top_ip: Ip,
    pub marks: Value<'gc>,
}

/// Returns the return address stored in slot 0 of the frame at `fp`.
///
/// # Safety
///
/// `fp` points at a live frame, whose slot 0 holds a return address.
#[inline(always)]
pub unsafe fn return_address(fp: Slot) -> Ra {
    unsafe { CallFrame::create(fp).return_address() }
}

/// Stores `ra` as the return address of the frame at `fp`.
///
/// # Safety
///
/// `fp` points at a writable frame slot, and `ra` is a marker from
/// [`markers`].
#[inline(always)]
pub unsafe fn set_return_address(fp: Slot, ra: Ra) {
    unsafe { CallFrame::create(fp).set_return_address(ra) }
}

/// Returns the bytecode return point of a caller. See
/// [`CallFrame::resume_ip`].
///
/// # Safety
///
/// `fp` points at a live frame whose slot 0 is `INTERP_RETURN` or a machine
/// return address into JIT code.
#[inline(always)]
pub unsafe fn resume_ip(fp: Slot) -> Ip {
    unsafe { CallFrame::create(fp).resume_ip() }
}

/// Stores `ip` as the bytecode return point of the frame at `fp`.
///
/// # Safety
///
/// `fp` points at a writable frame whose slot 0 is (or is about to become)
/// `INTERP_RETURN`, and `ip` is the first byte of a return point.
#[inline(always)]
pub unsafe fn set_resume_ip(fp: Slot, ip: Ip) {
    unsafe { CallFrame::create(fp).set_resume_ip(ip) }
}

/// What a return address says about the frame it returns into.
enum Below {
    /// Bottom of an activation or of a segment: nothing below to walk here.
    Bottom,
    /// The caller's frame is `size` slots, live as `mask` says.
    Frame { size: usize, mask: Mask },
}

/// The live slots of a frame.
#[derive(Clone, Copy, Debug)]
enum Mask {
    /// Entry `n` of the liveness masks in the aux pool of the frame's
    /// procedure (an interpreted frame). Bit `i` is `fp[i]`. The closure is
    /// always live.
    Pool(u32),
    /// An `RpHeader` livemask: bit `i` is `fp[i + 1]`, one slot higher than
    /// `Pool`.
    Header(usize),
    /// Only the closure, `fp[1]`.
    ClosureOnly,
}

/// Frame size and mask index of the return point at `return-point`.
unsafe fn decode_return_point(retpt: Ip) -> (usize, u32) {
    unsafe {
        let (width, opcode, operands) = def::decode_header(retpt);
        match opcode {
            def::OpcodeId::OpReturnPoint => {
                let (op, _) = def::OpReturnPoint::decode_from_ptr(operands, width);
                (op.frame.0 as usize, op.mask.0)
            }
            def::OpcodeId::OpReturnPointForward => {
                let (op, _) = def::OpReturnPointForward::decode_from_ptr(operands, width);
                (op.frame.0 as usize, op.mask.0)
            }
            other => unreachable!("resume {retpt:?} is not a return point: {other:?}"),
        }
    }
}

/// Decode the return address `ret` of the frame at `fp` through its header.
/// An interpreted caller (`INTERPRETER_FRAME_SIZE`) reads its bytecode return point from `fp[-1]`.
/// A header with frame size 0 belongs to a stub and is decoded by its marker.
#[inline]
unsafe fn frame_below(fp: Slot, ret: Ra) -> Below {
    unsafe {
        match entry_frame_size(ret) {
            INTERPRETER_FRAME_SIZE => {
                let (size, mask) = decode_return_point(resume_ip(fp));
                Below::Frame {
                    size,
                    mask: Mask::Pool(mask),
                }
            }
            0 => {
                if ret == markers::scheme_exit() || ret == markers::underflow() {
                    return Below::Bottom;
                }
                if ret == markers::raise_ret() {
                    // The frame an error was raised in. Its size was stored below
                    // the handler's frame, nothing in it is used again.
                    let size = (*fp.sub(1)).bits() as usize;
                    return Below::Frame {
                        size,
                        mask: Mask::ClosureOnly,
                    };
                }
                unreachable!("unknown stub return address {ret:?}")
            }
            bytes => Below::Frame {
                size: bytes as usize / size_of::<Value>(),
                mask: Mask::Header(entry_livemask(ret)),
            },
        }
    }
}

/// Size of the frame the return address `ret` of the frame at `fp` returns
/// into, or `None` at the bottom of an activation or segment.
///
/// # Safety
///
/// `fp` is a live frame whose return address is `ret`, and the frames
/// below it are intact.
pub unsafe fn caller_frame_size(fp: Slot, ret: Ra) -> Option<usize> {
    match unsafe { frame_below(fp, ret) } {
        Below::Bottom => None,
        Below::Frame { size, .. } => Some(size),
    }
}

/// Call `visit` on every live slot of the frame at `fp` named by `mask`.
unsafe fn visit_masked<'gc>(fp: Slot<'gc>, mask: Mask, visit: &mut dyn FnMut(Slot<'gc>)) {
    unsafe {
        match mask {
            Mask::ClosureOnly => visit(fp.add(1)),
            Mask::Pool(mask) => {
                let pool = frame_pool(fp); // reads fp[1] before it is visited
                visit(fp.add(1));
                let entry = pool.add(mask as usize);
                let nwords = *entry as usize;
                for word_index in 0..nwords {
                    let mut bits = *entry.add(1 + word_index);
                    if word_index == 0 {
                        bits &= !0b11;
                    }
                    while bits != 0 {
                        visit(fp.add(word_index * 32 + bits.trailing_zeros() as usize));
                        bits &= bits - 1;
                    }
                }
            }
            Mask::Header(livemask) => {
                // A fixnum, or a pointer to `[nwords][words]`
                let mut visit_bits = |base: usize, mut bits: u64| {
                    while bits != 0 {
                        visit(fp.add(1 + base + bits.trailing_zeros() as usize));
                        bits &= bits - 1;
                    }
                };
                if livemask & 1 != 0 {
                    visit_bits(0, (livemask >> 1) as u64);
                } else {
                    let words = livemask as *const u64;
                    for index in 0..*words as usize {
                        visit_bits(index * 64, *words.add(1 + index));
                    }
                }
            }
        }
    }
}

/// Aux pool of the procedure running in the frame at `fp`, found through
/// its closure in `fp[1]`.
unsafe fn frame_pool(fp: Slot) -> *const u32 {
    unsafe {
        let closure = Closure::from_value_unchecked(CallFrame::create(fp).callee());
        Closure::code_block(closure).buffer.pool().as_ptr()
    }
}

#[inline]
unsafe fn visit_code_owner<'gc>(ret: Ra, mask: Mask, visit: &mut dyn FnMut(Slot<'gc>)) {
    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    unsafe {
        if let Mask::Header(_) = mask
            && crate::runtime::sabi::entry_offset(ret) != 0
        {
            visit(crate::runtime::codegen::unit::owner_slot(ret));
        }
    }
    #[cfg(not(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64"))))]
    let _ = (ret, mask, visit);
}

/// Visit the live slots of every frame below the frame at `fp`, whose return
/// address is `ret`, down to the bottom of the activation or segment.
/// Returns the frame pointer of the bottom frame.
unsafe fn visit_frames_below<'gc>(
    mut fp: Slot<'gc>,
    mut ret: Ra,
    visit: &mut dyn FnMut(Slot<'gc>),
) -> Slot<'gc> {
    unsafe {
        loop {
            match frame_below(fp, ret) {
                Below::Bottom => return fp,
                Below::Frame { size, mask } => {
                    visit_code_owner(ret, mask, visit);
                    fp = fp.sub(size);
                    visit_masked(fp, mask, visit);
                    ret = return_address(fp);
                }
            }
        }
    }
}

/// Visit the live slots of the frames of a continuation's stack: from the
/// frame at `stack + clength`, whose return address is `ret`, down to the
/// frame at `stack`.
unsafe fn sweep_stack<'gc>(
    stack: Slot<'gc>,
    clength: isize,
    mut ret: Ra,
    visit: &mut dyn FnMut(Slot<'gc>),
) {
    unsafe {
        let mut fp = stack.offset(clength);
        while fp != stack {
            assert!(fp > stack, "sweep_stack: malformed stack");
            let Below::Frame { size, mask } = frame_below(fp, ret) else {
                unreachable!("sweep_stack: bottom of an activation inside a continuation");
            };
            visit_code_owner(ret, mask, visit);
            fp = fp.sub(size);
            visit_masked(fp, mask, visit);
            ret = return_address(fp);
        }
    }
}

/// Visit every live slot of the Scheme stack of a thread: the running
/// activation and every outer one. Continuation objects below each
/// activation are reached through `stack_link`, which the caller traces.
///
/// # Safety
///
/// The world is stopped, or the caller is the thread that owns `stack`.
/// The registers of `stack` and of every saved activation describe
/// intact frames: each return address is a marker with an `RpHeader`, and
/// each `fp[1]` that a mask is read through holds a closure.
pub unsafe fn visit_stack<'gc>(stack: &StackState<'gc>, visit: &mut dyn FnMut(Slot<'gc>)) {
    unsafe {
        let mut fp = stack.fp.get();
        let mut sp = stack.sp.get();
        let mut top_ip = stack.top_ip.get();
        let mut activation = stack.activation.get();
        loop {
            if !fp.is_null() {
                if top_ip.is_null() {
                    // Entry or native call: closure and arguments.
                    let mut slot = fp.add(1);
                    while slot < sp {
                        visit(slot);
                        slot = slot.add(1);
                    }
                } else {
                    // Parked at a `loop_hint`: the live registers are the
                    // ones in its mask.
                    let (width, opcode, operands) = def::decode_header(top_ip);
                    debug_assert_eq!(opcode, def::OpcodeId::OpLoopHint);
                    let (hint, _) = def::OpLoopHint::decode_from_ptr(operands, width);
                    visit_masked(fp, Mask::Pool(hint.mask.0), visit);
                }
                visit_frames_below(fp, return_address(fp), visit);
            }
            if activation.is_null() {
                break;
            }
            let record = &*activation;
            fp = record.fp;
            sp = record.sp;
            top_ip = record.top_ip;
            activation = record.prev;
        }
    }
}

/// Trace the stack roots of a thread.
///
/// # Safety
///
/// Called by the collector while the world is stopped, with the same
/// conditions as [`visit_stack`].
pub unsafe fn trace_stack<'gc>(stack: &mut StackState<'gc>, visitor: &mut Visitor) {
    unsafe {
        visit_stack(stack, &mut |slot| visitor.trace(&mut *slot));
        visitor.trace(stack.segment.get_mut());
        visitor.trace(stack.stack_link.get_mut());
        visitor.trace(stack.cached_frame.get_mut());
        visitor.trace(stack.call_rator.get_mut());
        visitor.trace(stack.call_k.get_mut());
        stack.stack_cache.set(std::ptr::null_mut());
        let mut activation = stack.activation.get();
        while !activation.is_null() {
            let record = &mut *activation;
            visitor.trace(&mut record.segment);
            visitor.trace(&mut record.stack_link);
            visitor.trace(&mut record.marks);
            activation = record.prev;
        }
    }
}

/// Continuation object. Its frames are `[stack, stack +
/// clength)` in `segment`, it is resumed by returning to `return_address`
/// with `stack + clength` as the frame pointer of the returning frame.
///
/// `length` says how it may be resumed:
///
///  * `length == clength`: multi-shot. Its frames are never written and are
///    copied onto the live stack when resumed.
///  * `length > clength`: one-shot. It owns `[stack, stack + length)` and is
///    resumed in place, which shoots it.
///  * `OPPORTUNISTIC_1_SHOT_FLAG`: resumed in place only when its frames end
///    at the base of the live stack, and treated as multi-shot otherwise.
///  * `length == clength == SHOT_1_SHOT_FLAG`: shot, invoking it is an
///    error.
///
/// For an interpreted frame (`return_address` is `INTERP_RETURN`) the
/// bytecode resume point is `stack[clength - 1]`, the top slot of the
/// frames, so it belongs to the object that owns them and travels with every
/// copy of them.
#[repr(C)]
pub struct Continuation<'gc> {
    /// The bytevector holding `stack`.
    pub segment: Lock<Value<'gc>>,
    pub link: Lock<Value<'gc>>,
    /// The continuation marks of these frames, or `#f` when returning into
    /// them leaves the marks alone.
    pub attachments: Lock<Value<'gc>>,
    pub stack: Lock<Slot<'gc>>,
    pub length: Cell<isize>,
    pub clength: Cell<isize>,
    pub return_address: Lock<Ra>,
}

extern "C" fn trace_continuation(obj: GcObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is a `Continuation`, its frames lie in `segment`, which it keeps
    // alive, and are walked with the same rules as the live stack.
    unsafe {
        let continuation = obj.to_address().as_mut_ref::<Continuation>();
        visitor.trace(continuation.segment.get_mut());
        visitor.trace(continuation.link.get_mut());
        visitor.trace(continuation.attachments.get_mut());
        if continuation.length.get() != SHOT_1_SHOT_FLAG {
            sweep_stack(
                continuation.stack.get(),
                continuation.clength.get(),
                continuation.return_address.get(),
                &mut |slot| visitor.trace(&mut *slot),
            );
        }
    }
}

extern "C" fn process_weak_continuation(_obj: GcObject, _weak_processor: &mut WeakProcessor) {}

fn continuation_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONTINUATION).expect("builtin class id is nonzero"),
    )
}

impl<'gc> Continuation<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        alignment: align_of::<Continuation>(),
        compute_alignment: None,
        instance_size: size_of::<Continuation>(),
        compute_size: None,
        trace: trace_continuation,
        weak_proc: process_weak_continuation,
        type_name: "continuation",
    };

    fn alloc(ctx: MutationContext<'gc>) -> Gc<'gc, Self> {
        // SAFETY: the allocation is `size_of::<Continuation>()` bytes and is fully
        // written, as an empty continuation, before it can be traced.
        unsafe {
            let object = ctx.raw_allocate_with_header_word(
                size_of::<Self>(),
                align_of::<Self>(),
                continuation_header_word(),
                AllocationSemantics::Default,
            );
            object
                .to_address()
                .to_mut_ptr::<Self>()
                .write(Continuation {
                    segment: Lock::new(Value::new(false)),
                    link: Lock::new(Value::new(false)),
                    attachments: Lock::new(Value::new(false)),
                    stack: Lock::new(std::ptr::null_mut()),
                    length: Cell::new(0),
                    clength: Cell::new(0),
                    return_address: Lock::new(std::ptr::null()),
                });
            Gc::from_gc_object(object)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn make(
        ctx: MutationContext<'gc>,
        segment: Value<'gc>,
        stack: Slot<'gc>,
        length: isize,
        clength: isize,
        link: Value<'gc>,
        return_address: Ra,
        attachments: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let k = Self::alloc(ctx);
        // SAFETY: new allocations do not need write barriers
        let kw = unsafe { Write::assume(&*k) };
        barrier::field!(kw, Continuation, segment)
            .unlock()
            .set(segment);
        barrier::field!(kw, Continuation, stack).unlock().set(stack);
        k.length.set(length);
        k.clength.set(clength);
        barrier::field!(kw, Continuation, link).unlock().set(link);
        barrier::field!(kw, Continuation, return_address)
            .unlock()
            .set(return_address);
        barrier::field!(kw, Continuation, attachments)
            .unlock()
            .set(attachments);
        k
    }
}

// SAFETY: Continuation stores its class ID in the heap header selected at construction time.
unsafe impl<'gc> ClassTagged for Continuation<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONTINUATION];

    const TYPE_NAME: &'static str = "continuation";
}

/// True if `continuation` (a continuation object) was resumed in place and so cannot
/// be invoked again.
pub fn is_shot<'gc>(continuation: Value<'gc>) -> bool {
    continuation
        .try_as::<Continuation>()
        .is_some_and(|continuation| continuation.length.get() == SHOT_1_SHOT_FLAG)
}

/// Promote every one-shot continuation in the chain `k` to multi-shot.
pub fn promote_to_multishot<'gc>(mut k: Value<'gc>) {
    while let Some(continuation) = k.try_as::<Continuation>() {
        if continuation.length.get() == continuation.clength.get() {
            break;
        }
        continuation.length.set(continuation.clength.get());
        k = continuation.link.get();
    }
}

/// Split `k` at `s`. `k` must be a multi-shot continuation, and `s`
/// (the split point) must be strictly between the base and end of its
/// stack.
unsafe fn split<'gc>(ctx: MutationContext<'gc>, k: Gc<'gc, Continuation<'gc>>, s: Slot<'gc>) {
    unsafe {
        // set m to size of lower piece, n to size of upper piece
        let m = s.offset_from(k.stack.get());
        let n = k.clength.get() - m;

        // insert a new continuation between k and link(k)
        let lower = Continuation::make(
            ctx,
            k.segment.get(),
            k.stack.get(),
            m,
            m,
            k.link.get(),
            return_address(s),
            Value::new(false),
        );
        // Before the stores: `k`'s frames below `s` move to `lower`, and a
        // snapshot-at-the-beginning barrier must see them still in `k`.
        let kw = Gc::write(ctx, k);

        barrier::field!(kw, Continuation, link)
            .unlock()
            .set(lower.into());
        k.length.set(n);
        k.clength.set(n);
        barrier::field!(kw, Continuation, stack).unlock().set(s);
        set_return_address(s, markers::underflow());
    }
}

/// Before copying the multi-shot (or unresumable one-shot) continuation `k`
/// and `value_count` values onto the live stack, this splits off its top
/// frames when it exceeds the copy bound, promotes it if it was a one-shot,
/// and gets a larger stack when they do not fit.
unsafe fn split_and_resize<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    value_count: usize,
) {
    unsafe {
        if k.clength.get() > UNDERFLOW_LIMIT as isize {
            let front_stack_ptr = k.stack.get();
            let end_stack_ptr = front_stack_ptr.offset(k.clength.get());

            let guard = end_stack_ptr.sub(UNDERFLOW_LIMIT);

            // set split point to base of top frame
            if let Some(frame_size) = caller_frame_size(end_stack_ptr, k.return_address.get()) {
                let mut split_point = end_stack_ptr.sub(frame_size);

                // split only if we have more than one frame
                if split_point != front_stack_ptr {
                    // walk the stack to set split_point at first frame above
                    // guard, note that first frame may have put us below the
                    // guard already
                    loop {
                        let frame_size =
                            caller_frame_size(split_point, return_address(split_point))
                                .expect("a continuation frame above its base has a caller");
                        let p = split_point.sub(frame_size);
                        if p < guard {
                            break;
                        }
                        split_point = p;
                    }

                    // promote to multi-shot if necessary
                    promote_to_multishot(k.into());

                    // split
                    split(ctx, k, split_point);
                }
            }
        }

        // Make sure the stack is big enough to hold the continuation. This is
        // conservative. The 2 is the returning frame's header.
        let n = k.clength.get() as usize + 2 + value_count + STACK_SLOP;
        if n >= ctx.tc().stack.scheme_stack_size.get() {
            reset_scheme_stack(ctx, n);
        }
    }
}

/// The number of frames in the continuation `k` and the continuations below
/// it.
///
/// Terminates on a shot one-shot continuation (which the null continuation
/// is) and at the end of the chain (`#f`). An empty segment, which
/// reification makes to hide a link, holds no frames and is skipped.
///
/// # Safety
///
/// `k` is a chain of [`Continuation`]s ending in `#f`, each with intact frames in
/// `[stack, stack + clength)`: every return address the walk decodes (all
/// but each segment's bottom frame) is a marker with an `RpHeader`, and
/// every interpreted frame's `fp[-1]` is a return point, as in
/// [`sweep_stack`].
pub unsafe fn continuation_depth<'gc>(mut k: Value<'gc>) -> usize {
    unsafe {
        let mut n = 0;
        // terminate on shot 1-shot, which could be null_continuation
        while let Some(continuation) = k.try_as::<Continuation>() {
            if continuation.length.get() == SHOT_1_SHOT_FLAG {
                break;
            }
            let stack_base = continuation.stack.get();
            let mut stack_ptr = stack_base.offset(continuation.clength.get());
            if stack_ptr != stack_base {
                let mut ret = continuation.return_address.get();
                let mut frame_size = caller_frame_size(stack_ptr, ret)
                    .expect("a continuation frame above its base has a caller");
                loop {
                    stack_ptr = stack_ptr.sub(frame_size);
                    n += 1;
                    if stack_ptr == stack_base {
                        break;
                    }
                    ret = return_address(stack_ptr);
                    frame_size = caller_frame_size(stack_ptr, ret)
                        .expect("a continuation frame above its base has a caller");
                }
            }
            k = continuation.link.get();
        }
        n
    }
}

/// The continuation holding frame
/// `n` of `k`, counting the top frame as 0 and linked to the frames below
/// it, or `#f` when `k` holds fewer than `n + 1` frames.
///
/// Splits `k` above and below the frame, promoting to multi-shot first, so
/// the returned object and the rest of `k` are ordinary multi-shot
/// continuations, invoking any of them behaves as invoking `k` did.
///
/// # Safety
///
/// As [`continuation_depth`]: `k` is a chain of [`Continuation`]s ending in `#f`
/// with intact frames.
pub unsafe fn single_continuation<'gc>(
    ctx: MutationContext<'gc>,
    mut k: Value<'gc>,
    mut n: usize,
) -> Value<'gc> {
    unsafe {
        // bug out on shot 1-shots, which could be null_continuation
        while let Some(continuation) = k.try_as::<Continuation>() {
            if continuation.length.get() == SHOT_1_SHOT_FLAG {
                break;
            }
            let stack_base = continuation.stack.get();
            let stack_top = stack_base.offset(continuation.clength.get());
            let mut stack_ptr = stack_top;
            if stack_ptr == stack_base {
                // An empty segment holds no frames, the frame is below it.
                k = continuation.link.get();
                continue;
            }
            let mut ret = continuation.return_address.get();
            let mut frame_size = caller_frame_size(stack_ptr, ret)
                .expect("a continuation frame above its base has a caller");
            loop {
                if n == 0 {
                    // promote to multi-shot if necessary, even if we don't end
                    // up in split, since inspector assumes multi-shot
                    promote_to_multishot(k);
                    if stack_ptr != stack_top {
                        split(ctx, continuation, stack_ptr);
                        k = continuation.link.get();
                    }
                    stack_ptr = stack_ptr.sub(frame_size);
                    if stack_ptr != stack_base {
                        split(ctx, k.downcast::<Continuation>(), stack_ptr);
                    }
                    return k;
                } else {
                    n -= 1;
                    stack_ptr = stack_ptr.sub(frame_size);
                    if stack_ptr == stack_base {
                        break;
                    }
                    ret = return_address(stack_ptr);
                    frame_size = caller_frame_size(stack_ptr, ret)
                        .expect("a continuation frame above its base has a caller");
                }
            }
            k = continuation.link.get();
        }
        Value::new(false)
    }
}

/// Slots of a cached stack
const CACHED_STACK_SIZE: usize = 0;
const CACHED_STACK_LINK: usize = 1;
const CACHED_STACK_SEGMENT: usize = 2;

/// Put the stack `[stack, stack + size)` of `segment`, which nothing uses,
/// on the stack cache.
unsafe fn cache_stack<'gc>(tc: &Tc<'gc>, segment: Value<'gc>, stack: Slot<'gc>, size: usize) {
    unsafe {
        if size <= CACHED_STACK_SEGMENT {
            return;
        }
        *stack.add(CACHED_STACK_SIZE) = Value::from_raw(size as u64);
        *stack.add(CACHED_STACK_LINK) = Value::from_raw(tc.stack.stack_cache.get() as u64);
        *stack.add(CACHED_STACK_SEGMENT) =
            std::mem::transmute::<Value<'gc>, Value<'static>>(segment);
        tc.stack.stack_cache.set(stack);
    }
}

/// Make the live stack one of at least
/// `n` slots, from the stack cache or freshly allocated. The old live stack
/// is dropped.
unsafe fn reset_scheme_stack<'gc>(ctx: MutationContext<'gc>, n: usize) {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;

        let mut n = n + (ONE_SHOT_HEADROOM >> 1);

        let mut x = stack.stack_cache.get();
        loop {
            if x.is_null() {
                if n < DEFAULT_STACK_SIZE {
                    n = DEFAULT_STACK_SIZE;
                }
                // stacks are non-moving bytevectors
                let bytes = ByteVector::new::<false>(ctx, n * size_of::<Value>(), false);
                stack.segment.set(bytes.into());
                stack
                    .scheme_stack
                    .set(bytes.contents().to_mut_ptr::<Value<'gc>>());
                break;
            }
            let m = (*x.add(CACHED_STACK_SIZE)).bits() as usize;
            let next = (*x.add(CACHED_STACK_LINK)).bits() as usize as Slot;
            if m >= n {
                n = m;
                stack.segment.set(*x.add(CACHED_STACK_SEGMENT));
                stack.scheme_stack.set(x);
                // Smaller stacks before this one are dropped too.
                stack.stack_cache.set(next);
                break;
            }
            x = next;
        }
        stack.scheme_stack_size.set(n);
        set_esp(tc);
    }
}

/// Give the thread its first stack.
pub fn ensure_stack<'gc>(ctx: MutationContext<'gc>) {
    let stack = &ctx.tc().stack;
    if !stack.scheme_stack.get().is_null() {
        return;
    }
    // SAFETY: the thread has no stack yet, so nothing refers to one.
    unsafe {
        reset_scheme_stack(ctx, 0);
    }
    stack.sp.set(stack.scheme_stack.get());
    stack.stack_link.set(Value::new(false));
}

/// Continue on a new stack with
/// `frame_request` more slots than the part of the live stack above the
/// split point. The live stack below the frame at `fp`, from the first frame
/// above `fp - UNDERFLOW_LIMIT`, becomes a continuation, everything above
/// that is copied to the new stack. Returns the new `fp`, `sp` moves with it.
///
/// # Safety
///
/// The stack state's `fp` is a live frame of the live stack with its return
/// address written, and `sp` is at or above it.
pub unsafe fn overflow<'gc>(ctx: MutationContext<'gc>, frame_request: usize) -> Slot<'gc> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let sfp = stack.fp.get();
        let scheme_stack = stack.scheme_stack.get();
        let scheme_stack_size = stack.scheme_stack_size.get();

        let mut guard = sfp.wrapping_sub(UNDERFLOW_LIMIT);
        // leave at least stack_slop headroom in the old stack to reduce the
        // need for return-point overflow checks
        let other_guard = scheme_stack
            .wrapping_add(scheme_stack_size)
            .wrapping_sub(STACK_SLOP);
        if other_guard < guard {
            guard = other_guard;
        }

        let mut split_point = scheme_stack;
        // split only if old stack contains more than underflow_limit slots
        if guard > scheme_stack {
            // set split point to base of the frame below the current one
            let frame_size = caller_frame_size(sfp, return_address(sfp))
                .expect("a frame above the stack base has a caller");
            split_point = sfp.sub(frame_size);

            // split only if we have more than one frame
            if split_point != scheme_stack {
                // walk the stack to set split_point at first frame above
                // guard, note that first frame may have put us below the
                // guard already
                loop {
                    let frame_size = caller_frame_size(split_point, return_address(split_point))
                        .expect("a frame above the stack base has a caller");
                    let p = split_point.sub(frame_size);
                    if p < guard {
                        break;
                    }
                    split_point = p;
                }

                let split_stack_clength = split_point.offset_from(scheme_stack);

                // promote to multi-shot if current stack is shrimpy
                let split_stack_length = if scheme_stack_size < DEFAULT_STACK_SIZE / 4 {
                    promote_to_multishot(stack.stack_link.get());
                    split_stack_clength
                } else {
                    scheme_stack_size as isize
                };

                // create a continuation
                let k = Continuation::make(
                    ctx,
                    stack.segment.get(),
                    scheme_stack,
                    split_stack_length,
                    split_stack_clength,
                    stack.stack_link.get(),
                    return_address(split_point),
                    Value::new(false),
                );
                stack.stack_link.set(k.into());

                // overwrite old return address with dounderflow
                set_return_address(split_point, markers::underflow());
            }
        }

        let above_split_size = scheme_stack_size - split_point.offset_from(scheme_stack) as usize;

        // allocate a new stack, retaining same relative sfp
        let sfp_offset = sfp.offset_from(split_point) as usize;
        let sp_offset = stack.sp.get().offset_from(split_point) as usize;
        reset_scheme_stack(ctx, above_split_size + frame_request);
        let new_stack = stack.scheme_stack.get();
        let fp = new_stack.add(sfp_offset);
        stack.fp.set(fp);
        stack.sp.set(new_stack.add(sp_offset));

        // copy up everything above the split point. we don't know where the
        // current frame ends, so we copy through the end of the old stack
        std::ptr::copy_nonoverlapping(split_point, new_stack, above_split_size);
        fp
    }
}

/// Make sure `need` slots are free above `sp` in the live stack, moving the
/// top frame to a new stack if not. For use while a native procedure owns
/// the top frame.
///
/// # Safety
///
/// The stack state's `fp` and `sp` describe the top frame of the live stack.
pub unsafe fn reserve<'gc>(ctx: MutationContext<'gc>, need: usize) {
    unsafe {
        let stack = &ctx.tc().stack;
        if stack.sp.get().add(need) <= stack.end() {
            return;
        }
        overflow(ctx, need);
    }
}

/// Make the live stack below the frame
/// at `fp` a continuation object, move the live stack base up to `fp`, and
/// return the continuation. Nothing is copied. The frame at `fp` then
/// returns through `dounderflow`.
///
/// `one_shot` makes an opportunistic one-shot object (used for continuation
/// marks), reusing the cached frame, otherwise the chain below
/// is promoted to multi-shot first. When the frame already
/// returns through `dounderflow` the continuation below is returned as is.
///
/// # Safety
///
/// `fp` is a live frame of the live stack, at or above its base, with its
/// return address written.
pub unsafe fn reify_cc<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    one_shot: bool,
) -> Value<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        let td = stack.stack_link.get();
        if !one_shot {
            // Promote existing 1-shot to multishot before reifying
            promote_to_multishot(td);
        }

        let ref_ret = return_address(fp);
        if ref_ret == markers::underflow()
            && td.downcast::<Continuation>().attachments.get() != Value::new(false)
        {
            return td;
        }

        let xp = match stack.cached_frame.get() {
            cached if one_shot && cached != Value::new(false) => {
                stack.cached_frame.set(Value::new(false));
                cached.downcast::<Continuation>()
            }
            _ => Continuation::alloc(ctx),
        };
        // Before the stores: a cached frame is an existing object (see
        // `split`).
        let wxp = Gc::write(ctx, xp);
        barrier::field!(wxp, Continuation, return_address)
            .unlock()
            .set(ref_ret);
        barrier::field!(wxp, Continuation, attachments)
            .unlock()
            .set(ctx.state().current_marks());
        set_return_address(fp, markers::underflow());
        barrier::field!(wxp, Continuation, link).unlock().set(td);
        stack.stack_link.set(xp.into());
        let base = stack.scheme_stack.get();
        stack.scheme_stack.set(fp);
        barrier::field!(wxp, Continuation, stack).unlock().set(base);
        barrier::field!(wxp, Continuation, segment)
            .unlock()
            .set(stack.segment.get());
        let n = fp.offset_from(base);
        xp.length.set(if one_shot {
            OPPORTUNISTIC_1_SHOT_FLAG
        } else {
            n
        });
        // clength will be 0 if we're reifying to hide a continuation
        // link
        xp.clength.set(n);
        stack
            .scheme_stack_size
            .set(stack.scheme_stack_size.get() - n as usize);
        xp.into()
    }
}

/// Reify the live stack below the frame at
/// `fp` (the native frame of `call/1cc`, live up to `sp`) as a one-shot
/// continuation and continue on a new live stack. The new stack is carved
/// out of the old one, `ONE_SHOT_HEADROOM` slots up, when it is large
/// enough, the continuation then owns the slots in between. Otherwise it
/// owns the whole old stack, and the new one comes from the stack cache or
/// is allocated. The native frame moves to the base of the new stack, whose
/// `fp` and `sp` the stack state then holds.
///
/// # Safety
///
/// `fp` is the stack state's `fp`, a live frame of the live stack with its
/// return address written, live up to `sp`.
pub unsafe fn call1cc<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>, sp: Slot<'gc>) -> Value<'gc> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let td = stack.stack_link.get();
        let ref_ret = return_address(fp);
        if ref_ret == markers::underflow()
            && td.downcast::<Continuation>().attachments.get() != Value::new(false)
        {
            // The existing object is handed out as it is. An opportunistic
            // one is promoted first: once merged it may be reused as the
            // cached frame, which must never happen to an object Scheme
            // holds.
            if td.downcast::<Continuation>().length.get() == OPPORTUNISTIC_1_SHOT_FLAG {
                promote_to_multishot(td);
            }
            return td;
        }
        let xp = Continuation::alloc(ctx);
        // SAFETY: new allocations do not need write barriers
        let wxp = Write::assume(&*xp);

        barrier::field!(wxp, Continuation, return_address)
            .unlock()
            .set(ref_ret);
        barrier::field!(wxp, Continuation, attachments)
            .unlock()
            .set(ctx.state().current_marks());
        barrier::field!(wxp, Continuation, link).unlock().set(td);
        stack.stack_link.set(xp.into());
        let base = stack.scheme_stack.get();
        barrier::field!(wxp, Continuation, stack).unlock().set(base);
        barrier::field!(wxp, Continuation, segment)
            .unlock()
            .set(stack.segment.get());
        xp.clength.set(fp.offset_from(base));

        // we need to get ourselves a new stack. we carve it out of the old
        // one if the old one is large enough. if not, we look for one in the
        // cache. if the cache is empty, we allocate a new stack.
        let live = sp.offset_from(fp) as usize;
        let new_fp = if fp.wrapping_add(ONE_SHOT_HEADROOM * 2) <= tc.real_esp.get() {
            let new_fp = fp.add(ONE_SHOT_HEADROOM);
            let n = new_fp.offset_from(base);
            xp.length.set(n);
            stack.scheme_stack.set(new_fp);
            stack
                .scheme_stack_size
                .set(stack.scheme_stack_size.get() - n as usize);
            new_fp
        } else {
            // set continuation length to entire stack size
            xp.length.set(stack.scheme_stack_size.get() as isize);
            // The first cached stack is taken whatever its size, because the
            // native frame is copied and needs room for it.
            reset_scheme_stack(ctx, live);
            stack.scheme_stack.get()
        };
        // The carved branch copies from `fp` to `fp + ONE_SHOT_HEADROOM`, so
        // the ranges overlap unless the live frame fits in the headroom.
        debug_assert!(live <= ONE_SHOT_HEADROOM);
        std::ptr::copy_nonoverlapping(fp, new_fp, live);
        set_return_address(new_fp, markers::underflow());
        stack.fp.set(new_fp);
        stack.sp.set(new_fp.add(live));
        xp.into()
    }
}

/// Continue `k` with the `count` values at
/// `values`, abandoning the live stack above its base. Returns the frame
/// pointer of the frame that "returns", with the values in its slots 2 and
/// up, and the return address to return to, `None` if `k` is a shot one-shot
/// continuation.
///
unsafe fn nuate_help<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    values: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let sfp = stack.scheme_stack.get();
        let td = k.clength.get();
        let multishot = if k.length.get() == td {
            // length and clength match, so it is either multishot or
            // shot1shot
            if td == SHOT_1_SHOT_FLAG {
                return None;
            }
            true
        } else if k.length.get() == OPPORTUNISTIC_1_SHOT_FLAG {
            // opportunistic 1-shot, so merge with the current stack if
            // possible, otherwise just treat as multishot
            if sfp == k.stack.get().offset(td) {
                // Before the frames leave `k` for the live stack (see
                // `split`).
                let _ = Gc::write(ctx, k);
                stack
                    .scheme_stack_size
                    .set(stack.scheme_stack_size.get() + td as usize);
                stack.scheme_stack.set(k.stack.get());
                stack.stack_link.set(k.link.get());
                k.clength.set(0); // in case GC sees it
                stack.cached_frame.set(k.into()); // save for fast immediate realloc
                restore_attachments(ctx, k);
                std::ptr::copy(values, CallFrame::create(sfp).arguments_ptr(), count);
                return Some((sfp, k.return_address.get()));
            }
            true
        } else {
            // general 1-shot: treat as multishot if clength + size(values)  > length
            k.length.get() < td + 2 + count as isize
        };

        if !multishot {
            // Before the frames leave `k` for the live stack (see `split`).
            let _ = Gc::write(ctx, k);
            stack.stack_link.set(k.link.get());
            // grab saved stack, keeping the old one to cache it once the
            // values are off it
            let old_stack = sfp;
            let old_size = stack.scheme_stack_size.get();
            let old_segment = stack.segment.get();
            let new_stack = k.stack.get();
            stack.scheme_stack.set(new_stack);
            stack.segment.set(k.segment.get());
            stack.scheme_stack_size.set(k.length.get() as usize);
            set_esp(tc);
            // move stack return values to top of saved stack segment
            let fp = new_stack.offset(td);
            std::ptr::copy(values, CallFrame::create(fp).arguments_ptr(), count);
            // place old stack in stack cache
            cache_stack(tc, old_segment, old_stack, old_size);
            // The attachments are left alone here: marks are popped by every
            // return through a continuation object.
            restore_attachments(ctx, k);
            // mark continuation shot
            k.length.set(SHOT_1_SHOT_FLAG);
            k.clength.set(SHOT_1_SHOT_FLAG);
            return Some((fp, k.return_address.get()));
        }

        // multishot: split if clength > underflow-limit, resize unless
        // stack-base + clength + size(values) <= esp
        if td > UNDERFLOW_LIMIT as isize || sfp.offset(td).add(2 + count) > tc.real_esp.get() {
            split_and_resize(ctx, k, count);
        }
        let td = k.clength.get();
        let sfp = stack.scheme_stack.get();
        stack.stack_link.set(k.link.get());
        // potentially pop an attachment
        restore_attachments(ctx, k);
        let fp = sfp.offset(td);
        std::ptr::copy(values, CallFrame::create(fp).arguments_ptr(), count);
        std::ptr::copy_nonoverlapping(k.stack.get(), sfp, td as usize);
        Some((fp, k.return_address.get()))
    }
}

fn restore_attachments<'gc>(ctx: MutationContext<'gc>, k: Gc<'gc, Continuation<'gc>>) {
    let attachments = k.attachments.get();
    if attachments != Value::new(false) {
        // SAFETY: these are the marks of the frames being returned into.
        unsafe { ctx.state().set_current_marks(attachments) }
    }
}

/// `dounderflow`: the frame at `fp`, the base of the
/// live stack, returned `count` values in `fp[2..]`. Continue the
/// continuation below, see [`nuate_help`].
///
/// # Safety
///
/// `fp` is the base of the live stack with `count` values in `fp[2..]`, and
/// the stack link is a [`Continuation`].
pub unsafe fn dounderflow<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe {
        let stack = &ctx.tc().stack;
        debug_assert!(
            fp == stack.scheme_stack.get(),
            "dounderflow away from the stack base"
        );
        let link = stack.stack_link.get();
        assert!(
            link.is::<Continuation>(),
            "return through the bottom of a stack with no continuation below"
        );
        nuate_help(
            ctx,
            link.downcast::<Continuation>(),
            CallFrame::create(fp).arguments_ptr(),
            count,
        )
    }
}

/// `nuate`: invoke the continuation object `k` with the
/// `count` values at `args`, see [`nuate_help`].
///
/// # Safety
///
/// `args` points at `count` readable values.
pub unsafe fn nuate<'gc>(
    ctx: MutationContext<'gc>,
    k: Gc<'gc, Continuation<'gc>>,
    args: Slot<'gc>,
    count: usize,
) -> Option<(Slot<'gc>, Ra)> {
    unsafe { nuate_help(ctx, k, args, count) }
}

/// Save the stack registers into `record` and start a new activation whose
/// first frame is at the returned address. The frame's return address is the
/// `scheme_exit` marker.
///
/// A callback from C runs on the same stack, so reifying in it would take in
/// the frames of the Scheme code that called out to C. Activations are
/// delimited instead: one entered with Scheme frames live
/// below it runs on a stack of its own, which [`pop_activation`] hands back
/// to the stack cache, so that a continuation reified in it never holds
/// slots the frames below will write. One entered with nothing live runs on
/// the live stack from its base.
///
/// # Safety
///
/// `record` is valid for writes and stays at the same address until the
/// matching [`pop_activation`].
pub unsafe fn push_activation<'gc>(
    ctx: MutationContext<'gc>,
    record: *mut Activation<'gc>,
    need: usize,
) -> Slot<'gc> {
    unsafe {
        ensure_stack(ctx);
        let tc = ctx.tc();
        let stack = &tc.stack;
        record.write(Activation {
            prev: stack.activation.get(),
            fp: stack.fp.get(),
            sp: stack.sp.get(),
            scheme_stack: stack.scheme_stack.get(),
            scheme_stack_size: stack.scheme_stack_size.get(),
            segment: stack.segment.get(),
            stack_link: stack.stack_link.get(),
            top_ip: stack.top_ip.get(),
            marks: ctx.state().current_marks(),
        });
        stack.activation.set(record);

        if !stack.fp.get().is_null() || stack.scheme_stack.get().add(need) > tc.real_esp.get() {
            reset_scheme_stack(ctx, need);
        }
        let fp = stack.scheme_stack.get();
        stack.stack_link.set(Value::new(false));
        stack.top_ip.set(std::ptr::null());
        stack.fp.set(fp);
        stack.sp.set(fp);
        set_return_address(fp, markers::scheme_exit());
        fp
    }
}

/// Restore the stack registers saved by [`push_activation`]. An activation
/// that ran on a stack of its own puts its live stack on the stack cache, one
/// that ran on the live stack leaves it.
///
/// # Safety
///
/// `record` was filled by [`push_activation`], and the activation it
/// started has ended.
pub unsafe fn pop_activation<'gc>(ctx: MutationContext<'gc>, record: *mut Activation<'gc>) {
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let saved = &*record;
        stack.activation.set(saved.prev);
        if !saved.fp.is_null() {
            cache_stack(
                tc,
                stack.segment.get(),
                stack.scheme_stack.get(),
                stack.scheme_stack_size.get(),
            );
            stack.scheme_stack.set(saved.scheme_stack);
            stack.scheme_stack_size.set(saved.scheme_stack_size);
            stack.segment.set(saved.segment);
            set_esp(tc);
        }
        stack.fp.set(saved.fp);
        stack.sp.set(saved.sp);
        stack.stack_link.set(saved.stack_link);
        stack.top_ip.set(saved.top_ip);
        ctx.state().set_current_marks(saved.marks);
    }
}

/// First free slot above the frame at `fp`, for pushing a frame on top of it
/// outside the normal call protocol.
///
/// # Safety
///
/// `fp` is a live frame whose slot 1 holds the value it was called with.
pub unsafe fn frame_top<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>) -> Slot<'gc> {
    unsafe {
        let frame = CallFrame::create(fp);
        let closure = frame.callee();
        if closure.is::<Closure>() {
            let closure = closure.downcast::<Closure>();
            if !closure.code_block.is_native() {
                return fp.add((closure.code_block.frame_size as usize).max(2));
            }
        }
        ctx.tc().stack.sp.get().max(frame.arguments_ptr())
    }
}

/// Procedures on the stack, innermost first, with the bytecode address each
/// will resume at (null for stub frames, and for the running frame when no
/// slow-path exit staged one). For backtraces.
///
/// # Safety
///
/// Called by the thread that owns the stack, with the stack state's `fp`
/// either null or a live frame whose return-address chain is intact.
pub unsafe fn backtrace<'gc>(ctx: MutationContext<'gc>, limit: usize) -> Vec<(Value<'gc>, Ip)> {
    let mut frames = Vec::new();
    unsafe {
        let tc = ctx.tc();
        let stack = &tc.stack;
        let mut fp = stack.fp.get();
        if fp.is_null() {
            return frames;
        }
        frames.push((*fp.add(1), tc.ip.get()));
        let mut ret = return_address(fp);
        while frames.len() < limit {
            let Some(size) = caller_frame_size(fp, ret) else {
                break;
            };
            let resume = if ret == markers::raise_ret() || ret == markers::subr_ret() {
                std::ptr::null()
            } else {
                resume_ip(fp)
            };
            fp = fp.sub(size);
            let closure = *fp.add(1);
            frames.push((closure, resume));
            ret = return_address(fp);
        }
    }
    frames
}
