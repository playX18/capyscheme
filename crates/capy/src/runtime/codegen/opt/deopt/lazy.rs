//! Lazy deoptimization of the live frames of an invalidated unit.
//!
//! A unit whose assumption broke (a folded global was written, a class it
//! allocated inline was redefined, a generic it dispatched changed its
//! methods) must not run again. New calls already avoid it: the
//! invalidation retargets its entry. A frame that is already running the
//! unit is evicted when control would return into it. Optimized code does
//! not check for that itself. Instead the runtime replaces each return address that
//! returns into one of the unit's call sites with that site's *shadow*
//! return address. The shadow is emitted next to the site with a copy of its
//! return header, so every stack walker reads the frame the same, and it
//! deopts to baseline at the call's return point with the callee's values.
//!
//! Which stacks are patched, and when:
//!
//! - The stack of each mutator's running virtual thread, by a soft handshake
//!   that [`evict`] requests. The current thread runs it inline, so its
//!   stack is patched before the write that fired the watchpoint returns.
//! - A virtual thread that was parked or queued, when it is switched in
//!   ([`on_switch_in`]): its stack is behind [`LAZY_EPOCH`].
//! - Frames inside continuation objects, when a reinstatement makes them
//!   live ([`patch_reinstated`]) or returns into them ([`lazy_return_address`]).
//!
//! The patch is one aligned word store of a return address with the same
//! header, so a concurrent reader (the SIGPROF profiler) sees either
//! address and decodes the same frame. The walk is idempotent: a shadow
//! return address is not a call site, so it is never patched again.
//!
//! The top frame, which is running and has no return address into the unit,
//! leaves at its next watchpoint helper or yield, which report the
//! invalidation as a lazy status (see `opt::glue`).

use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering::{AcqRel, Acquire, Relaxed};

use crate::heap::Gc;
use crate::heap::sync::handshake::{self, HandshakeOp};
use crate::heap::sync::thread::Thread;
use crate::runtime::codegen::unit::{CodeUnit, owner_cell};
use crate::runtime::interp::stack::{self, Slot, StackState};
use crate::runtime::objectmodel::{CodeImage, Value};
use crate::runtime::sabi::{self, Ra};
use crate::runtime::thread::ThreadContext;

/// Bumped by every invalidation that must evict live frames. A stack whose
/// [`ThreadContext::lazy_epoch`] is behind it may hold return addresses into
/// a unit invalidated since.
pub(crate) static LAZY_EPOCH: AtomicU64 = AtomicU64::new(0);

/// The shadow return address that replaces `ra`, when `ra` is the return
/// address of a call site in a Tier 2 unit that has been invalidated.
///
/// # Safety
///
/// `ra` is a compiled return address with a full or compact header whose
/// toplink leads to its image's owner word.
pub(crate) unsafe fn shadow_of(ra: Ra) -> Option<Ra> {
    // SAFETY: by the caller's contract, the toplink of `ra` leads to the
    // owner slot at the start of its image.
    let owner: Value<'static> = unsafe { *owner_cell::<'static>(ra) };
    if !owner.is::<CodeImage>() {
        return None;
    }
    let unit = CodeUnit::of_image(owner.downcast::<CodeImage>())?;
    let opt = unit.opt.as_ref()?;
    if opt.invalid.load(Acquire) == 0 {
        return None;
    }
    opt.shadow_for(unit.image.base(), ra as usize)
        .map(|shadow| shadow as Ra)
}

/// The `invalid` word of a unit whose assumption broke: its live frames are
/// being evicted. `1` is an invalidation that leaves them running (a spent
/// deopt budget, a cold retire), whose assumptions still hold.
pub(crate) const ASSUMPTION_BROKEN: u32 = 2;

/// Mark the unit of `image`, already invalidated, as one whose assumption
/// broke. Returns whether that is news and the unit has call sites whose
/// frames [`evict`] must patch.
pub(crate) fn break_assumption<'gc>(image: Gc<'gc, CodeImage<'gc>>) -> bool {
    let Some(unit) = CodeUnit::of_image(image) else {
        return false;
    };
    let Some(opt) = unit.opt.as_ref() else {
        return false;
    };
    opt.invalid.swap(ASSUMPTION_BROKEN, AcqRel) != ASSUMPTION_BROKEN && !opt.lazy_returns.is_empty()
}

/// Replace `fp[0]` by the shadow return address when it returns into a
/// call site of an invalidated unit.
///
/// # Safety
///
/// `fp` is a live frame whose return address `ra` returns into compiled
/// code.
unsafe fn patch_frame(fp: Slot<'_>, ra: Ra) {
    // SAFETY: `ra` is a compiled return address, per the walker.
    if let Some(shadow) = unsafe { shadow_of(ra) } {
        // SAFETY: `fp` is a live frame; `fp[0]` is its return address word.
        unsafe { stack::set_return_address_atomic(fp, shadow) };
        if crate::utils::flags::jit_stats() {
            crate::runtime::codegen::jit::config::OPT_LAZY_PATCHES.fetch_add(1, Relaxed);
        }
    }
}

/// Patch every live frame of `stack` that returns into an invalidated unit,
/// and record the epoch it is now current with.
///
/// # Safety
///
/// As [`stack::visit_compiled_returns`].
pub(crate) unsafe fn patch_stack(tc: &ThreadContext<'_>) {
    // Read the epoch first: an invalidation during the walk leaves the
    // stack behind, so the next check walks it again.
    let epoch = LAZY_EPOCH.load(Acquire);
    // SAFETY: by the caller's contract.
    unsafe { patch_stack_frames(&tc.stack) };
    tc.lazy_epoch.set(epoch);
}

unsafe fn patch_stack_frames(stack: &StackState<'_>) {
    // SAFETY: by the caller's contract.
    unsafe {
        stack::visit_compiled_returns(stack, &mut |fp, ra| patch_frame(fp, ra));
    }
}

/// Patch the stack of `tc` if an invalidation happened since it was last
/// patched.
///
/// # Safety
///
/// As [`patch_stack`].
pub(crate) unsafe fn catch_up(tc: &ThreadContext<'_>) {
    if tc.lazy_epoch.get() != LAZY_EPOCH.load(Acquire) {
        // SAFETY: by the caller's contract.
        unsafe { patch_stack(tc) };
    }
}

/// Called by the scheduler before it switches the virtual thread whose
/// context is `tc` in. A parked or queued thread missed the handshakes of
/// invalidations since it last ran.
///
/// # Safety
///
/// `tc` is the parked context of a thread that is about to run on this
/// worker. Its stack registers describe intact frames.
pub(crate) unsafe fn on_switch_in(tc: *const ThreadContext<'_>) {
    if tc.is_null() {
        return;
    }
    // SAFETY: by the caller's contract.
    unsafe { catch_up(&*tc) };
}

/// Patch the frames from `fp` down to `bottom` that a continuation
/// reinstatement just made live.
///
/// # Safety
///
/// As [`stack::visit_compiled_returns_in`].
pub(crate) unsafe fn patch_reinstated<'gc>(fp: Slot<'gc>, bottom: Slot<'gc>) {
    if LAZY_EPOCH.load(Relaxed) == 0 {
        return;
    }
    // SAFETY: by the caller's contract.
    unsafe {
        stack::visit_compiled_returns_in(fp, bottom, &mut |fp, ra| patch_frame(fp, ra));
    }
}

/// The return address to really return to for `ra`: its shadow when `ra`
/// returns into a call site of an invalidated unit, else `ra` itself. For
/// return addresses kept outside the stack, which no walk patches: a
/// continuation's.
///
/// # Safety
///
/// `ra` is a return address with a return header: compiled, or a marker.
pub(crate) unsafe fn lazy_return_address(ra: Ra) -> Ra {
    if LAZY_EPOCH.load(Relaxed) == 0 {
        return ra;
    }
    // SAFETY: `ra` has a header, per the caller.
    let compiled = unsafe {
        let size = sabi::entry_frame_size(ra);
        size != 0 && size != sabi::INTERPRETER_FRAME_SIZE && sabi::entry_offset(ra) != 0
    };
    if !compiled {
        return ra;
    }
    // SAFETY: a compiled return address.
    unsafe { shadow_of(ra) }.unwrap_or(ra)
}

/// An acquire operation (a mutex acquire, a condition wait, a join) just
/// synchronized with another thread. Writes that thread made before its
/// release are visible now, and so is an invalidation it caused. Its
/// handshake may still be queued for this thread, so the stack catches up
/// here, before any frame returns into the invalidated code with a value the
/// releasing thread wrote.
///
/// A program that shares data only through such operations never sees an
/// invalidated unit assume a value that it can observe was overwritten. A
/// racy reader is evicted at its next yieldpoint.
pub(crate) fn acquire_fence(ctx: crate::runtime::MutationContext<'_>) {
    // SAFETY: a native procedure runs with its frame published.
    unsafe { catch_up(ctx.tc()) };
}

/// Whether the unit whose image's owner word is at `owner` had an
/// assumption broken: a frame running it must leave at the next lazy point.
///
/// # Safety
///
/// `owner` is the owner word of a live code image.
pub(crate) unsafe fn assumption_broken(owner: *const u8) -> bool {
    // SAFETY: by the caller's contract.
    let owner: Value<'static> = unsafe { *(owner as *const Value<'static>) };
    if !owner.is::<CodeImage>() {
        return false;
    }
    CodeUnit::of_image(owner.downcast::<CodeImage>())
        .and_then(|unit| unit.opt.as_ref())
        .is_some_and(|opt| opt.invalid.load(Acquire) == ASSUMPTION_BROKEN)
}

static STRESS_TICKS: AtomicU64 = AtomicU64::new(0);

/// `CAPY_OPT_STRESS_LAZY_DEOPT=n`: at every n-th call, invalidate the unit
/// running in the optimized frame `fp` (whose image's owner word is
/// `owner`) as a broken assumption would, and evict its frames.
///
/// # Safety
///
/// `owner` is the owner word of the unit's image, `fp` an optimized frame
/// of that unit with its root closure in slot 1, and the current stack is
/// published.
pub(crate) unsafe fn stress_tick(
    ctx: crate::runtime::MutationContext<'_>,
    owner: *const u8,
    fp: Slot<'_>,
) {
    let every = crate::runtime::codegen::jit::config::opt().stress_lazy_deopt as u64;
    if every == 0 || STRESS_TICKS.fetch_add(1, Relaxed) % every != every - 1 {
        return;
    }
    let _ = ctx;
    // SAFETY: by the caller's contract.
    let (owner, closure) = unsafe { (*(owner as *const Value<'_>), *fp.add(1)) };
    if !owner.is::<CodeImage>() || !closure.is::<crate::runtime::objectmodel::Closure>() {
        return;
    }
    let image = owner.downcast::<CodeImage>();
    let block = crate::runtime::objectmodel::Closure::code_block(
        crate::runtime::objectmodel::Closure::from_value(closure),
    );
    let _ = crate::runtime::codegen::jit::compile::invalidate_optimized(block, image);
    if break_assumption(image) {
        evict();
    }
}

/// The handshake that patches each mutator's running stack.
struct PatchStacks;

impl HandshakeOp for PatchStacks {
    fn do_thread(&self, thread: &Thread) {
        if !thread.is_mutator_thread() {
            return;
        }
        let tc = thread.tc_ptr();
        if tc.is_null() {
            return;
        }
        // SAFETY: the op runs on the thread itself at a yieldpoint, or for
        // it while it cannot run Scheme. Either way its stack registers
        // describe intact frames. The op only stores return-address words.
        unsafe { patch_stack(&*tc) };
    }

    fn name(&self) -> &'static str {
        "lazy-deopt"
    }
}

/// Evict the live frames of the units that a broken assumption just
/// invalidated: their `invalid` words are already set. Patches the current
/// thread's stack before it returns, and every other mutator's at its next
/// yieldpoint. Other virtual threads catch up when they are switched in.
///
/// The current thread's stack registers must describe its frames: the
/// caller is a slow path that published its frame pointer.
pub(crate) fn evict() {
    LAZY_EPOCH.fetch_add(1, AcqRel);
    if handshake::in_handshake_op() {
        // No handshake from inside one. The current stack is patched now;
        // the others catch up at their next switch-in or invalidation.
        if let Some(thread) = Thread::try_current_thread() {
            PatchStacks.do_thread(thread);
        }
        return;
    }
    handshake::handshake_async(Arc::new(PatchStacks));
}
