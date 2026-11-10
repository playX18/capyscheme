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

use crate::heap::Space;
use std::cell::Cell;
use std::sync::atomic::Ordering::Relaxed;

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
use crate::runtime::objectmodel::{ByteVector, ClassTagged, Closure, Value};
use crate::runtime::{MutationContext, ThreadContext};
use crate::{WeakProcessor, Write};

pub const DEFAULT_STACK_SIZE: usize = 64 * 1024;
/// Slots of the first stack of a spawned thread. Most threads are
/// shallow, and a full-size stack is a large object that costs a page-zeroing
/// pass and the large-object space lock. A deeper one grows through
/// [`overflow`], which allocates at least [`DEFAULT_STACK_SIZE`].
const THREAD_STACK_SIZE: usize = 4 * 1024;
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
    /// Slots in the live stack. `ThreadContext::esp` is always
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

fn set_esp(tc: &ThreadContext<'_>) {
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
            visit(crate::runtime::codegen::unit::owner_cell(ret));
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
                    // Entry or native call: closure and arguments. A compiled
                    // frame is only scanned here while parked at a poll, where
                    // the emitter sets `sp = fp + 1` so this range is empty and
                    // the real frame is walked through the parked return
                    // address's header (see `Deferred::Poll`).
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
        visit_stack(stack, &mut |slot| {
            crate::runtime::root::trace_slot_guarded("stack slot", slot, visitor);
        });
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


mod continuation;
mod pool;

pub use continuation::*;
pub use pool::*;
