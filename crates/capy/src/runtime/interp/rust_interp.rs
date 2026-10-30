//! The bytecode interpreter.
//!
//! Every instruction is a function with the same signature, and an
//! instruction finishes by tail-calling the next one (`become`). The seven
//! parameters are the machine state, with the `rust-preserve-none` calling
//! convention they stay in registers across the whole run:
//!
//! | name  | contents |
//! |-------|----------|
//! | `ip`  | first byte of the current instruction |
//! | `fp`  | the current frame |
//! | `th`  | the thread (`MutationContext`) |
//! | `clo` | the closure being executed (`None` where no closure is live) |
//! | `md`  | its metadata area (may be null) |
//! | `dt`  | the **narrow** dispatch table |
//! | `w`   | the argument or value count at an entry or return point, 0 otherwise |
//!
//! Handlers are generic over the operand width (`W = 0, 1, 2` for
//! narrow/wide16/wide32), three dispatch tables hold the three
//! instantiations. Anything that is not the common case of an instruction
//! lives in a separate `#[cold]` function reached by `become`, so the common
//! case keeps no stack frame.

use std::sync::atomic::Ordering::Relaxed;

use crate::bytecode::def::{self, *};
use crate::heap::Gc;
use crate::heap::barrier;
use crate::runtime::MutationContext;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
use crate::runtime::codegen::jit::{HotnessPolicy, ThresholdPolicy, request_tier_up};
use crate::runtime::interp::frame::CallFrame;
use crate::runtime::interp::markers;
use crate::runtime::interp::profile::ARRAY_FAST;
use crate::runtime::interp::runtime_procs;
use crate::runtime::interp::slow;
use crate::runtime::interp::stack::{self, Ip, Slot};
use crate::runtime::prims::calls as prims;
use crate::runtime::value::{
    Closure, EnvRecord, IntoValue, NativeReturn, Pair, ReturnCode, Value, Vector,
};
use crate::runtime::variable::Variable;
use crate::runtime::vm::exceptions::RaiseKind;

pub type Handler = for<'gc> unsafe extern "rust-preserve-none" fn(
    Ip,
    Slot<'gc>,
    MutationContext<'gc>,
    Option<Gc<'gc, Closure<'gc>>>,
    MetadataArea,
    Dt,
    u64,
) -> NativeReturn<'gc>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct MetadataArea(*mut u8);

const _: () = {
    assert!(
        core::mem::size_of::<Option<Gc<'static, Closure<'static>>>>()
            == core::mem::size_of::<*const ()>()
    );
    assert!(core::mem::size_of::<MetadataArea>() == core::mem::size_of::<*mut u8>());
    assert!(core::mem::size_of::<Dt>() == core::mem::size_of::<*const ()>());
};

impl MetadataArea {
    /// A null area, for code without metadata.
    #[inline(always)]
    pub fn null() -> Self {
        Self(std::ptr::null_mut())
    }

    /// Wrap the area at `ptr`.
    #[inline(always)]
    pub fn from_ptr(ptr: *mut u8) -> Self {
        Self(ptr)
    }

    /// The raw area, for shared slow paths that still take `*mut u8`.
    #[inline(always)]
    pub fn as_ptr(self) -> *mut u8 {
        self.0
    }

    /// Entry `id` of `Op`'s metadata array.
    ///
    /// # Safety
    ///
    /// `self` is an initialized area with more than `id` entries of `Op`'s
    /// kind.
    #[inline(always)]
    pub(crate) unsafe fn entry<Op: def::OpcodeWithMetadata>(
        self,
        id: def::MetadataId,
    ) -> *mut Op::Metadata {
        // SAFETY: by the caller's contract, the opcode's generated
        // accessor owns the area layout.
        unsafe { Op::metadata_entry(self.as_ptr(), id) }
    }
}

/// The dispatch table, as a register-sized handle.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Dt(*const Handler);

impl Dt {
    #[inline(always)]
    fn get(self, opcode: u8) -> Handler {
        // SAFETY: every `Dt` names one of the static 256-entry tables below,
        // and `opcode` is a byte.
        unsafe { *self.0.add(opcode as usize) }
    }
}

macro_rules! op {
    ($(#[$attr:meta])* fn $name:ident<$W:ident>($ip:ident, $fp:ident, $th:ident, $clo:ident, $md:ident, $dt:ident, $w:ident) $body:block) => {
        $(#[$attr])*
        #[allow(unused_variables, unused_mut, unused_unsafe)]
        unsafe extern "rust-preserve-none" fn $name<'gc, const $W: u8>(
            $ip: Ip, $fp: Slot<'gc>, $th: MutationContext<'gc>, $clo: Option<Gc<'gc, Closure<'gc>>>, $md: MetadataArea, $dt: Dt, $w: u64,
        ) -> NativeReturn<'gc> {
            unsafe { $body }
        }
    };
    ($(#[$attr:meta])* fn $name:ident($ip:ident, $fp:ident, $th:ident, $clo:ident, $md:ident, $dt:ident, $w:ident) $body:block) => {
        $(#[$attr])*
        #[allow(unused_variables, unused_mut, unused_unsafe)]
        unsafe extern "rust-preserve-none" fn $name<'gc>(
            $ip: Ip, $fp: Slot<'gc>, $th: MutationContext<'gc>, $clo: Option<Gc<'gc, Closure<'gc>>>, $md: MetadataArea, $dt: Dt, $w: u64,
        ) -> NativeReturn<'gc> {
            unsafe { $body }
        }
    };
}

/// Decode the operands of the `$Op` at `$ip`, encoded at width `$W`.
macro_rules! decode {
    ($Op:ident, $W:ident, $ip:expr) => {
        def::$Op::decode_from_ptr_static::<$W>($ip.add(def::$Op::header_size_static::<$W>())).0
    };
}

/// Run the instruction at `$ip`.
macro_rules! next {
    ($ip:expr, $fp:expr, $th:expr, $clo:expr, $md:expr, $dt:expr) => {{
        let ip: Ip = $ip;
        become ($dt.get(*ip))(ip, $fp, $th, $clo, $md, $dt, 0)
    }};
}

/// Continue after the `$Op` at `$ip`.
macro_rules! advance {
    ($Op:ident, $W:ident, $ip:expr, $fp:expr, $th:expr, $clo:expr, $md:expr, $dt:expr) => {
        next!(
            $ip.add(def::$Op::size_static::<$W>()),
            $fp,
            $th,
            $clo,
            $md,
            $dt
        )
    };
}

/// Enter `$callee` with `$argc` arguments already in `$fp[2..]` and the
/// callee in `$fp[1]`. Natives run on the Rust path for their kind.
macro_rules! call_closure {
    ($fp:expr, $th:expr, $callee:expr, $argc:expr, $dt:expr) => {{
        let callee = $callee;
        let fp = $fp;
        let argc: u64 = $argc as u64;
        if callee.is::<Closure>() {
            let clo = Closure::from_value(callee);
            if Closure::code_block(clo).is_native() {
                become call_native(
                    std::ptr::null(),
                    fp,
                    $th,
                    Some(clo),
                    MetadataArea::null(),
                    $dt,
                    argc,
                )
            }
            let ip = Closure::code_block(clo).entrypoint.to_ptr::<u8>();
            let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
            become ($dt.get(*ip))(ip, fp, $th, Some(clo), md, $dt, argc)
        }
        become call_non_closure(
            std::ptr::null(),
            fp,
            $th,
            None,
            MetadataArea::null(),
            $dt,
            argc,
        )
    }};
}

/// Return `$count` values, already in `$fp[2..]`, to the frame's return
/// address. A return to an interpreted caller resumes at the bytecode return
/// point in `fp[-1]`, anything else goes through `do_return`.
macro_rules! return_values {
    ($fp:expr, $th:expr, $clo:expr, $md:expr, $dt:expr, $count:expr) => {{
        let fp = $fp;
        let count = $count as u64;
        let ra = stack::return_address(fp);
        if ra == markers::interpreter_return() {
            // Read the resume point before dispatching: `return-point` writes `dst`,
            // which is often `r[base-1]` itself.
            let ip = stack::resume_ip(fp);
            if count == 1 {
                become ($dt.get(*ip))(ip, fp, $th, $clo, $md, $dt, 1)
            }
            become return_many_values(ip, fp, $th, $clo, $md, $dt, count)
        }
        become do_return(ra, fp, $th, $clo, $md, $dt, count)
    }};
}

/// Map a shared [`slow::Control`] decision back into the engine: dispatch
/// the resumed bytecode, or return the activation's result.
macro_rules! dispatch_control {
    ($c:expr, $th:expr, $dt:expr) => {{
        match $c {
            slow::Control::Resume {
                fp,
                cp,
                ac,
                rv: _,
                ip,
            } => {
                let clo = Closure::from_value(cp);
                let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
                become ($dt.get(*ip))(ip, fp, $th, Some(clo), md, $dt, ac)
            }
            slow::Control::Exit { code, value } => {
                return NativeReturn { code, value };
            }
            slow::Control::Jump { .. } => unreachable!("compiled code under the Rust engine"),
        }
    }};
}

/// Run the procedure in `fp[1]` with `argc` arguments in `fp[2..]` until the
/// activation exits.
///
/// # Safety
///
/// `fp` must be the first frame of an activation set up by
/// [`stack::push_activation`], or a frame pushed above a live one, with its
/// return address already written.
pub unsafe fn run<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>, argc: usize) -> NativeReturn<'gc> {
    unsafe {
        let dt = Dt(DISPATCH_NARROW.0.as_ptr());
        enter(
            std::ptr::null(),
            fp,
            ctx,
            None,
            MetadataArea::null(),
            dt,
            argc as u64,
        )
    }
}

op! { fn enter(ip, fp, th, clo, md, dt, argc) {
    let callee = *fp.add(1);
    call_closure!(fp, th, callee, argc, dt)
}}

/// Which bail protocol the instruction at `ip` uses: `call`/`tail-call` carry
/// the caller metadata area in `w` (see `emit_bail_handler`), and return
/// points run with no `clo`/`md` (their frame may be virtual after
/// `underflow`/`reinstate`, they pop before reading anything).
#[derive(PartialEq, Eq, Clone, Copy)]
enum BailShape {
    Call,
    Tcall,
    ReturnPoint,
    Other,
}

fn bail_shape_at(ip: Ip) -> BailShape {
    let opcode: u32 = unsafe {
        match *ip {
            def::WIDE16_PREFIX => ip.add(1).cast::<u16>().read_unaligned() as u32,
            def::WIDE32_PREFIX => ip.add(1).cast::<u32>().read_unaligned(),
            byte => byte as u32,
        }
    };
    if opcode == OpcodeId::OpCall as u32 || opcode == OpcodeId::OpCallSelf as u32 {
        BailShape::Call
    } else if opcode == OpcodeId::OpTailCall as u32 || opcode == OpcodeId::OpTailCallSelf as u32 {
        BailShape::Tcall
    } else if opcode == OpcodeId::OpReturnPoint as u32
        || opcode == OpcodeId::OpReturnPointForward as u32
        || opcode == OpcodeId::OpReturnPointValues as u32
        || opcode == OpcodeId::OpReturnPointValuesRest as u32
    {
        BailShape::ReturnPoint
    } else {
        BailShape::Other
    }
}

/// `fp[1]` is the running closure, except at `tail-call`, where it is the
/// callee (which may not be a closure at all): the running code for a
/// `call`/`tail-call` comes from `w` instead, which the template interpreter loaded with the caller
/// metadata area. At a return point the frame may be virtual (see
/// `exit_to_template`), so those handlers run with no `clo`/`md`. `w`
/// is the resume count everywhere else: the argument count at an entry, the
/// value count at a return point, ignored everywhere else. A `call`/`tail-call`
/// instruction is always bytecode, so the native-callee check below only
/// applies to other shapes (in practice the `nop` the template interpreter enters for a native
/// initial callee).
///
/// # Safety
///
/// `fp` is a live frame, and `ip` is the first byte of an instruction of
/// the running code block: `fp[1]`'s, except at `tail-call`, where `w` names
/// the running metadata area, and at return points, whose handlers pop
/// before reading anything.
unsafe fn resume_with<'gc>(
    ctx: MutationContext<'gc>,
    ip: Ip,
    fp: Slot<'gc>,
    w: u64,
    dt: Dt,
) -> NativeReturn<'gc> {
    unsafe {
        let shape = bail_shape_at(ip);
        // The running closure lives in `fp[1]`, except at `tail-call` (where the
        // frame holds the callee, which may not be a closure at all) and at
        // return points (where the frame may be virtual): those shapes resume
        // with no closure.
        let clo = match shape {
            BailShape::Tcall | BailShape::ReturnPoint => None,
            BailShape::Call | BailShape::Other => {
                debug_assert!((*fp.add(1)).is::<Closure>());
                Some(Closure::from_value(*fp.add(1)))
            }
        };
        match shape {
            BailShape::Call => {
                let md = MetadataArea::from_ptr(w as *mut u8);
                debug_assert_eq!(
                    md.as_ptr(),
                    Closure::code_block(clo.expect("call resume has a closure"))
                        .buffer
                        .metadata()
                );
                DISPATCH_NARROW.0[*ip as usize](ip, fp, ctx, clo, md, dt, w)
            }
            // No closure is available: the frame holds the callee in `r1`
            // (`tail-call`), or is virtual after `underflow`/`reinstate` (return
            // points). These handlers take no `clo`/`md` and must not read
            // them: `tail-call` uses `w` for the metadata area (its link
            // buffer comes from the area itself), and return points pop
            // before reading anything.
            BailShape::Tcall => {
                let md = MetadataArea::from_ptr(w as *mut u8);
                DISPATCH_NARROW.0[*ip as usize](ip, fp, ctx, None, md, dt, w)
            }
            BailShape::ReturnPoint => {
                DISPATCH_NARROW.0[*ip as usize](ip, fp, ctx, None, MetadataArea::null(), dt, w)
            }
            BailShape::Other => {
                let clo = clo.expect("resume has a closure");
                if Closure::code_block(clo).is_native() {
                    call_native(ip, fp, ctx, Some(clo), MetadataArea::null(), dt, w)
                } else {
                    let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
                    DISPATCH_NARROW.0[*ip as usize](ip, fp, ctx, Some(clo), md, dt, w)
                }
            }
        }
    }
}

/// Resume the Rust engine mid-activation at `ip`, after the template interpreter bailed.
///
/// See [`resume_with`] for the `fp[1]`/`w` protocol.
///
/// # Safety
///
/// Same contract as [`resume_with`].
pub unsafe fn resume_at<'gc>(
    ctx: MutationContext<'gc>,
    ip: Ip,
    fp: Slot<'gc>,
    w: u64,
) -> NativeReturn<'gc> {
    unsafe { resume_with(ctx, ip, fp, w, Dt(DISPATCH_NARROW.0.as_ptr())) }
}

/// Execute exactly one instruction at `ip` and return to the template interpreter.
///
/// # Safety
///
/// Same contract as [`resume_with`].
pub unsafe fn resume_one<'gc>(
    ctx: MutationContext<'gc>,
    ip: Ip,
    fp: Slot<'gc>,
    w: u64,
) -> NativeReturn<'gc> {
    unsafe { resume_with(ctx, ip, fp, w, Dt(EXIT_TO_TEMPLATE.0.as_ptr())) }
}

op! { fn op_invalid(ip, fp, th, clo, md, dt, w) {
    panic!("invalid opcode {:#x} at {ip:?}", *ip)
}}

op! { fn op_nop<W>(ip, fp, th, clo, md, dt, w) {
    advance!(OpNop, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_wide16(ip, fp, th, clo, md, dt, w) {
    // The opcode slot is u16 but always holds a byte (verified at load).
    let opcode = ip.add(1).cast::<u16>().read_unaligned() as u8;
    become DISPATCH_WIDE16.0[opcode as usize](ip, fp, th, clo, md, dt, w)
}}

op! { fn op_wide32(ip, fp, th, clo, md, dt, w) {
    let opcode = ip.add(1).cast::<u32>().read_unaligned() as u8;
    become DISPATCH_WIDE32.0[opcode as usize](ip, fp, th, clo, md, dt, w)
}}

op! { fn op_enter<W>(ip, fp, th, clo, md, dt, argc) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpEnter, W, ip);
    if argc != op.argc.0 as u64 {
        become enter_arity(ip, fp, th, clo, md, dt, argc)
    }
    let tc = th.tc();
    if fp.add(op.frame.0 as usize) > tc.esp.load(Relaxed) {
        become enter_overflow(ip, fp, th, clo, md, dt, argc)
    }
    // A poked `esp` fails the overflow check above, so entries need no
    // separate yield poll.
    // Hotness: count heat when the JIT is on, on fire ask the promotion
    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        let policy = ThresholdPolicy::current();
        if let Some(threshold) = policy.hot_threshold()
            && let Some(clo) = clo
        {
            let block = Closure::code_block(clo);
            if block.hot_count.fetch_add(1, Relaxed) + 1 >= threshold {
                request_tier_up(th, block, &policy);
            }
        }
    }
    advance!(OpEnter, W, ip, fp, th, clo, md, dt)
}}

op! { #[cold] #[inline(never)] fn enter_arity(ip, fp, th, clo, md, dt, argc) {
    dispatch_control!(
        slow::control_enter_arity(th, fp, clo.expect("entry has a closure"), argc as usize),
        th,
        dt
    )
}}

op! { #[cold] #[inline(never)] fn enter_overflow(ip, fp, th, clo, md, dt, argc) {
    dispatch_control!(
        slow::control_enter_overflow(th, fp, argc as usize, ip),
        th,
        dt
    )
}}

op! { fn op_enter_rest<W>(ip, fp, th, clo, md, dt, argc) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpEnterRest, W, ip);
    if argc < op.required.0 as u64 {
        become enter_arity(ip, fp, th, clo, md, dt, argc)
    }
    let extras = (argc - op.required.0 as u64) as usize;
    let tc = th.tc();
    if fp.add(op.frame.0 as usize + extras) > tc.esp.load(Relaxed) {
        become enter_overflow(ip, fp, th, clo, md, dt, argc)
    }
    // A poked `esp` fails the overflow check above, so entries need no
    // separate yield poll.
    // Hotness: count heat when the JIT is on, on fire ask the promotion
    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        let policy = ThresholdPolicy::current();
        if let Some(threshold) = policy.hot_threshold()
            && let Some(clo) = clo
        {
            let block = Closure::code_block(clo);
            if block.hot_count.fetch_add(1, Relaxed) + 1 >= threshold {
                request_tier_up(th, block, &policy);
            }
        }
    }
    // The extras leave the argument area so the registers below `base` are
    // free for locals.
    std::ptr::copy(fp.add(2 + op.required.0 as usize), fp.add(op.base.0 as usize), extras);
    frame[op.cnt] = Value::from_i32(extras as i32);
    advance!(OpEnterRest, W, ip, fp, th, clo, md, dt)
}}

// Enter a native closure. Every native takes `(ctx, sfp)` and reports a
// `ReturnCode`, a single result or error value sits in place at `sfp[2]`.
op! { fn call_native(ip, fp, th, clo, md, dt, argc) {
    let frame = CallFrame::create(fp);
    th.tc().ip.set(ip);
    dispatch_control!(
        slow::control_run_native(th, fp, clo.expect("native entry has a closure"), argc as usize),
        th,
        dt
    )
}}

// A native procedure asked for a call (`ReturnCode::Continue`). The request
// is staged in the stack state, `fp` there is the native's frame, which may
// have moved to a new segment while the arguments were staged.
op! { #[cold] #[inline(never)] fn native_continue(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(slow::control_native_continue(th), th, dt)
}}

// The callee of a `call` or `tail-call` is not a closure: try the call site's
// generic-function dispatch cache, else run the full slow path.
op! { #[cold] #[inline(never)] fn call_slow(ip, fp, th, clo, md, dt, argc) {
    become call_non_closure(ip, fp, th, clo, md, dt, argc)
}}

// The callee is not a closure: apply a generic function, or raise. A null
// `ip` means activation entry (`enter` passes no call site): then there is
// no link to probe or fill.
op! { #[cold] #[inline(never)] fn call_non_closure(ip, fp, th, clo, md, dt, argc) {
    let site = if ip.is_null() {
        None
    } else {
        Some(slow::EnterLink {
            link: slow::generic_link(ip, md.as_ptr()),
            buffer: Value::from_gc(crate::runtime::value::CodeBuffer::md_buffer(md.as_ptr())),
        })
    };
    dispatch_control!(
        slow::control_enter(th, fp, *fp.add(1), argc as usize, site),
        th,
        dt
    )
}}

// Call the exception handler with the condition in `exit_value`, as a tail
// call from the frame at `fp`: the handler returns to that frame's caller.
op! { #[cold] #[inline(never)] fn raise_in_frame(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(slow::control_raise_in_frame(th, fp), th, dt)
}}

// Call the exception handler with the condition in `exit_value` from a
// native frame that asked for a raise. The handler's result ends the
// activation.
op! { #[cold] #[inline(never)] fn raise_out_of_frame(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(slow::control_raise_out_of_frame(th), th, dt)
}}

pub use crate::runtime::interp::slow::push_raise_frame;

// Raise condition kind `w & 0xff` with the irritant in `exit_value`, at the
// instruction `ip`, whose source location goes into the condition.
op! { #[cold] #[inline(never)] fn raise_kind(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(
        slow::control_raise_kind(th, fp, clo, ip, (w & 0xff) as usize),
        th,
        dt
    )
}}

op! { fn op_raise<W>(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(slow::control_raise(th, fp, clo, ip), th, dt)
}}

// Single-value entry. The value is in `r2` of the returning frame, it is
// written to `r[dst]` of the caller's. The metadata belongs to the caller's
// code block, so it is read after `md` is reloaded.
op! { fn op_return_point<W>(ip, fp, th, clo, md, dt, count) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpReturnPoint, W, ip);
    let value = *fp.add(2);
    let mut frame = frame.sub(op.frame.0 as usize);
    let fp = frame.slot();
    let clo = Closure::from_value(frame.callee());
    let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
    // A count other than 1 arrives through `return_many_values` at an
    // `MV_IGNORE` return point. With no values, `r2` is a stale slot that may
    // point at freed memory, so only a single value is stored and profiled.
    if count == 1 {
        frame[op.dst] = value;
        (*md.entry::<def::OpReturnPoint>(op.metadata_id)).profile.observe(value);
    }
    advance!(OpReturnPoint, W, ip, fp, th, Some(clo), md, dt)
}}

// Single-value entry of a multiple-value return point: one value is a count
// of one at the multiple-value entry.
op! { fn op_return_point_forward<W>(ip, fp, th, clo, md, dt, count) {
    let target = ip.add(def::OpReturnPointForward::size_static::<W>());
    become (dt.get(*target))(target, fp, th, clo, md, dt, 1)
}}

op! { fn op_return_point_values<W>(ip, fp, th, clo, md, dt, count) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpReturnPointValues, W, ip);
    let frame = frame.sub(op.frame.0 as usize);
    let fp = frame.slot();
    if count != op.nvals.0 as u64 {
        become wrong_value_count(ip, fp, th, clo, md, dt, count)
    }
    let clo = Closure::from_value(frame.callee());
    let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
    advance!(OpReturnPointValues, W, ip, fp, th, Some(clo), md, dt)
}}

op! { fn op_return_point_values_rest<W>(ip, fp, th, clo, md, dt, count) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpReturnPointValuesRest, W, ip);
    let base = op.frame.0 as usize;
    let frame = frame.sub(base);
    let fp = frame.slot();
    if count < op.nvals.0 as u64 {
        become wrong_value_count(ip, fp, th, clo, md, dt, count)
    }
    *fp.add(base) = Value::from_i32((count - op.nvals.0 as u64) as i32);
    let clo = Closure::from_value(frame.callee());
    let md = MetadataArea::from_ptr(Closure::code_block(clo).buffer.metadata());
    advance!(OpReturnPointValuesRest, W, ip, fp, th, Some(clo), md, dt)
}}

// Return `count` values, already in `fp[2..]`, to the return address `ra`
// (`fp[0]`, passed in the `ip` slot). The single-value interpreted case is
// inline in `return_values!`, a count other than one follows `*(ra - MV_DISP)`.
op! { fn do_return(ra, fp, th, clo, md, dt, count) {
    dispatch_control!(slow::control_return(th, fp, ra, count), th, dt)
}}

// Return `count` values, already in `fp[2..]`, to the stub return address
// `ra`. A stub takes any count itself.
op! { fn do_return_stub(ra, fp, th, clo, md, dt, count) {
    let frame = CallFrame::create(fp);
    dispatch_control!(slow::control_return(th, fp, ra, count), th, dt)
}}

// Return `count` values (`count != 1`), already in `fp[2..]`, to the
// interpreted caller whose bytecode return point is `ip` (`fp[-1]`).
// A return of this kind resolves to the return point itself, and
// the point validates the count: an `MV_IGNORE` single point takes any count
// and drops the values, a forwarded point enters its multiple-values entry,
// anything else raises. `clo`/`md` are the callee's, every resumed point
// reloads them after popping, as on the single-value path.
op! { #[cold] #[inline(never)] fn return_many_values(ip, fp, th, clo, md, dt, count) {
    let frame = CallFrame::create(fp);
    let (width, opcode, operands) = def::decode_header(ip);
    match opcode {
        OpcodeId::OpReturnPoint => {
            let (op, _) = def::OpReturnPoint::decode_from_ptr(operands, width);
            if op.mv.0 == MV_IGNORE {
                become (dt.get(*ip))(ip, fp, th, clo, md, dt, count)
            }
            let fp = fp.sub(op.frame.0 as usize);
            become wrong_value_count(ip, fp, th, clo, md, dt, count)
        }
        OpcodeId::OpReturnPointForward => {
            let target = ip.add(def::Instruction::size_at_ptr(ip));
            become (dt.get(*target))(target, fp, th, clo, md, dt, count)
        }
        other => unreachable!("resume address {ip:?} is not a return point: {other:?}"),
    }
}}

// `fp` is already the frame the values were returned into.
op! { #[cold] #[inline(never)] fn wrong_value_count(ip, fp, th, clo, md, dt, count) {
    dispatch_control!(
        slow::control_wrong_count(th, fp, ip, count),
        th,
        dt
    )
}}

op! { fn op_move<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpMove, W, ip);
    frame[op.dst] = frame[op.src];
    advance!(OpMove, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_move_two<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpMoveTwo, W, ip);
    frame[op.dst1] = frame[op.src1];
    frame[op.dst2] = frame[op.src2];
    advance!(OpMoveTwo, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_load_constant<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpLoadConstant, W, ip);
    frame[op.dst] = frame.code_block().buffer.constant(op.constant.0 as usize);
    advance!(OpLoadConstant, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_load_immediate<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpLoadImmediate, W, ip);
    frame[op.dst] = Value::from_i32(op.value.0);
    advance!(OpLoadImmediate, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_load_raw<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpLoadRaw, W, ip);
    frame[op.dst] = Value::from_raw(op.bits.0 as u64);
    advance!(OpLoadRaw, W, ip, fp, th, clo, md, dt)
}}

// A cache cell is a one-slot box shared by every code block of a
// compilation unit, the constant pool holds the box.
op! { fn op_cell_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpCellRef, W, ip);
    let value = frame.code_block().buffer.constant(op.constant.0 as usize).downcast::<Variable>().value.get();
    (*md.entry::<def::OpCellRef>(op.metadata_id)).profile.observe(value);
    frame[op.dst] = value;
    advance!(OpCellRef, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_cell_set<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpCellSet, W, ip);
    let cell = frame.code_block().buffer.constant(op.constant.0 as usize);
    barrier::field!(Gc::write(th, cell.downcast::<Variable>()), Variable, value)
        .unlock()
        .set(frame[op.src]);
    advance!(OpCellSet, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_runtime_procedure<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRuntimeProcedure, W, ip);
    frame[op.dst] = runtime_procs::get(th, op.id.0);
    advance!(OpRuntimeProcedure, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_closure<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpClosure, W, ip);
    slow::closure_body(th, frame, &op);
    advance!(OpClosure, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_closure_env<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpClosureEnv, W, ip);
    slow::closure_env_body(th, frame, &op);
    advance!(OpClosureEnv, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_free_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpFreeRef, W, ip);
    let clo = clo.expect("free variable of a live closure");
    frame[op.dst] = Closure::free_get(clo, op.index.0 as usize);
    advance!(OpFreeRef, W, ip, fp, th, Some(clo), md, dt)
}}

op! { fn op_closure_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpClosureRef, W, ip);
    let closure = Closure::from_value(frame[op.closure]);
    frame[op.dst] = Closure::free_get(closure, op.index.0 as usize);
    advance!(OpClosureRef, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_closure_set<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpClosureSet, W, ip);
    let closure = Closure::from_value(frame[op.closure]);
    Closure::free_set(closure, op.index.0 as usize, frame[op.src]);
    advance!(OpClosureSet, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_make_env<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpMakeEnv, W, ip);
    slow::make_env_body(th, frame, &op);
    advance!(OpMakeEnv, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_env_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpEnvRef, W, ip);
    let env = EnvRecord::from_value(frame[op.env]);
    frame[op.dst] = EnvRecord::slot_get(env, op.index.0 as usize);
    advance!(OpEnvRef, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_env_set<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpEnvSet, W, ip);
    let env = EnvRecord::from_value(frame[op.env]);
    EnvRecord::slot_set(env, op.index.0 as usize, frame[op.src]);
    advance!(OpEnvSet, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_jump<W>(ip, fp, th, clo, md, dt, w) {
    let op = decode!(OpJump, W, ip);
    next!(ip.offset(op.target as isize), fp, th, clo, md, dt)
}}

op! { fn op_jump_if_true<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpJumpIfTrue, W, ip);
    let target = if frame[op.cond].as_bool() {
        ip.offset(op.target as isize)
    } else {
        ip.add(def::OpJumpIfTrue::size_static::<W>())
    };
    next!(target, fp, th, clo, md, dt)
}}

op! { fn op_jump_if_false<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpJumpIfFalse, W, ip);
    let target = if frame[op.cond].as_bool() {
        ip.add(def::OpJumpIfFalse::size_static::<W>())
    } else {
        ip.offset(op.target as isize)
    };
    next!(target, fp, th, clo, md, dt)
}}

macro_rules! type_branch {
    ($yes:ident, $YesOp:ident, $no:ident, $NoOp:ident, |$v:ident| $test:expr) => {
        op! { fn $yes<W>(ip, fp, th, clo, md, dt, w) {
            let frame = CallFrame::create(fp);
            let op = decode!($YesOp, W, ip);
            let $v = frame[op.src];
            let target = if $test {
                ip.offset(op.target as isize)
            } else {
                ip.add(def::$YesOp::size_static::<W>())
            };
            next!(target, fp, th, clo, md, dt)
        }}
        op! { fn $no<W>(ip, fp, th, clo, md, dt, w) {
            let frame = CallFrame::create(fp);
            let op = decode!($NoOp, W, ip);
            let $v = frame[op.src];
            let target = if $test {
                ip.add(def::$NoOp::size_static::<W>())
            } else {
                ip.offset(op.target as isize)
            };
            next!(target, fp, th, clo, md, dt)
        }}
    };
}

type_branch!(
    op_jump_if_null,
    OpJumpIfNull,
    op_jump_if_not_null,
    OpJumpIfNotNull,
    |v| v.is_null()
);
type_branch!(
    op_jump_if_pair,
    OpJumpIfPair,
    op_jump_if_not_pair,
    OpJumpIfNotPair,
    |v| v.is::<Pair>()
);
type_branch!(
    op_jump_if_fixnum,
    OpJumpIfFixnum,
    op_jump_if_not_fixnum,
    OpJumpIfNotFixnum,
    |v| v.is_int32()
);
type_branch!(
    op_jump_if_undefined,
    OpJumpIfUndefined,
    op_jump_if_not_undefined,
    OpJumpIfNotUndefined,
    |v| v.raw_i64() == Value::VALUE_UNDEFINED
);

op! { fn op_jump_if_eq<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpJumpIfEq, W, ip);
    let x = frame[op.lhs];
    let y = frame[op.rhs];
    let target = if x.bits() == y.bits() {
        ip.offset(op.target as isize)
    } else {
        ip.add(def::OpJumpIfEq::size_static::<W>())
    };
    next!(target, fp, th, clo, md, dt)
}}

op! { fn op_jump_if_not_eq<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpJumpIfNotEq, W, ip);
    let x = frame[op.lhs];
    let y = frame[op.rhs];
    let target = if x.bits() != y.bits() {
        ip.offset(op.target as isize)
    } else {
        ip.add(def::OpJumpIfNotEq::size_static::<W>())
    };
    next!(target, fp, th, clo, md, dt)
}}

macro_rules! numeric_jump {
    ($name:ident, $Op:ident, $op:tt, $sense:expr) => {
        op! { fn $name<W>(ip, fp, th, clo, md, dt, w) {
            let frame = CallFrame::create(fp);
            let op = decode!($Op, W, ip);
            let x = frame[op.lhs];
            let y = frame[op.rhs];
            if x.is_int32() && y.is_int32() {
                (*md.entry::<def::$Op>(op.metadata_id)).profile.observe_fixnums();
                let target = if (x.as_int32() $op y.as_int32()) == $sense {
                    ip.offset(op.target as isize)
                } else {
                    ip.add(def::$Op::size_static::<W>())
                };
                next!(target, fp, th, clo, md, dt)
            }
            become compare_slow(ip, fp, th, clo, md, dt, w)
        }}
    };
}

numeric_jump!(op_jump_if_less, OpJumpIfLess, <, true);
numeric_jump!(op_jump_if_less_equal, OpJumpIfLessEqual, <=, true);
numeric_jump!(op_jump_if_number_equal, OpJumpIfNumberEqual, ==, true);
numeric_jump!(op_jump_if_greater_equal, OpJumpIfGreaterEqual, >=, true);
numeric_jump!(op_jump_if_greater, OpJumpIfGreater, >, true);
numeric_jump!(op_jump_if_not_less, OpJumpIfNotLess, <, false);
numeric_jump!(op_jump_if_not_less_equal, OpJumpIfNotLessEqual, <=, false);
numeric_jump!(op_jump_if_not_number_equal, OpJumpIfNotNumberEqual, ==, false);
numeric_jump!(op_jump_if_not_greater_equal, OpJumpIfNotGreaterEqual, >=, false);
numeric_jump!(op_jump_if_not_greater, OpJumpIfNotGreater, >, false);

op! { #[cold] #[inline(never)] fn compare_slow(ip, fp, th, clo, md, dt, w) {
    let target = slow::compare_slow_body(th, fp, ip, md.as_ptr());
    next!(target, fp, th, clo, md, dt)
}}

op! { fn op_switch_fixnum<W>(ip, fp, th, clo, md, dt, w) {
    next!(slow::switch_body(th, fp, ip), fp, th, clo, md, dt)
}}

op! { fn op_switch_char<W>(ip, fp, th, clo, md, dt, w) {
    next!(slow::switch_body(th, fp, ip), fp, th, clo, md, dt)
}}

op! { fn op_switch_symbol<W>(ip, fp, th, clo, md, dt, w) {
    next!(slow::switch_body(th, fp, ip), fp, th, clo, md, dt)
}}

op! { fn op_loop_hint<W>(ip, fp, th, clo, md, dt, w) {
    if th.take_yieldpoint() > 0 {
        become loop_yield::<W>(ip, fp, th, clo, md, dt, w)
    }

    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        let policy = ThresholdPolicy::current();
        if let Some(threshold) = policy.hot_threshold()
            && let Some(clo) = clo
        {
            let block = Closure::code_block(clo);
            if block.hot_count.fetch_add(1, Relaxed) + 1 >= threshold {
                request_tier_up(th, block, &policy);
            }
        }
    }
    advance!(OpLoopHint, W, ip, fp, th, clo, md, dt)
}}

op! { #[cold] #[inline(never)] fn loop_yield<W>(ip, fp, th, clo, md, dt, w) {
    dispatch_control!(slow::control_loop_yield(th, fp, w, ip), th, dt)
}}

op! { fn op_call<W>(ip, fp, th, clo, md, dt, w) {
    let op = decode!(OpCall, W, ip);
    let callee_frame = fp.add(op.base.0 as usize);
    // The callee returns to `INTERP_RETURN`, the bytecode return point, the
    // `return-point` that follows this call, goes into the reserved `r[base-1]` slot.
    stack::set_resume_ip(callee_frame, ip.add(def::OpCall::size_static::<W>()));
    stack::set_return_address(callee_frame, markers::interpreter_return());
    let callee = *callee_frame.add(1);
    if callee.is::<Closure>() {
        let link = &(*md.entry::<def::OpCall>(op.metadata_id)).link;
        if link.callee.bits() != Value::from_gc(Closure::code_block(Closure::from_value(callee))).bits()
            && link.polymorphic.load(Relaxed) == 0
        {
            become record_callee(ip, callee_frame, th, clo, md, dt, op.argc.0 as u64)
        }
        call_closure!(callee_frame, th, callee, op.argc.0, dt)
    }
    become call_slow(ip, callee_frame, th, clo, md, dt, op.argc.0 as u64)
}}

op! { fn op_tail_call<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpTailCall, W, ip);
    let callee = frame.callee();
    if callee.is::<Closure>() {
        let link = &(*md.entry::<def::OpTailCall>(op.metadata_id)).link;
        if link.callee.bits() != Value::from_gc(Closure::code_block(Closure::from_value(callee))).bits()
            && link.polymorphic.load(Relaxed) == 0
        {
            become record_callee(ip, fp, th, clo, md, dt, op.argc.0 as u64)
        }
        call_closure!(fp, th, callee, op.argc.0, dt)
    }
    become call_slow(ip, fp, th, clo, md, dt, op.argc.0 as u64)
}}

op! { fn op_call_self<W>(ip, fp, th, clo, md, dt, w) {
    let op = decode!(OpCallSelf, W, ip);
    let callee_frame = fp.add(op.base.0 as usize);
    stack::set_resume_ip(callee_frame, ip.add(def::OpCallSelf::size_static::<W>()));
    stack::set_return_address(callee_frame, markers::interpreter_return());
    let callee = *fp.add(1);
    *callee_frame.add(1) = callee;
    if callee.is::<Closure>() {
        let link = &(*md.entry::<def::OpCallSelf>(op.metadata_id)).link;
        if link.callee.bits() != Value::from_gc(Closure::code_block(Closure::from_value(callee))).bits()
            && link.polymorphic.load(Relaxed) == 0
        {
            become record_callee(ip, callee_frame, th, clo, md, dt, op.argc.0 as u64)
        }
        call_closure!(callee_frame, th, callee, op.argc.0, dt)
    }
    become call_slow(ip, callee_frame, th, clo, md, dt, op.argc.0 as u64)
}}

op! { fn op_tail_call_self<W>(ip, fp, th, clo, md, dt, w) {
    let op = decode!(OpTailCallSelf, W, ip);
    let callee = *fp.add(1);
    if callee.is::<Closure>() {
        let link = &(*md.entry::<def::OpTailCallSelf>(op.metadata_id)).link;
        if link.callee.bits() != Value::from_gc(Closure::code_block(Closure::from_value(callee))).bits()
            && link.polymorphic.load(Relaxed) == 0
        {
            become record_callee(ip, fp, th, clo, md, dt, op.argc.0 as u64)
        }
        call_closure!(fp, th, callee, op.argc.0, dt)
    }
    become call_slow(ip, fp, th, clo, md, dt, op.argc.0 as u64)
}}

// The call site's cached callee differs from `*fp[1]`: store the new code
// block (or mark the site polymorphic) and enter the callee.
op! { #[cold] #[inline(never)] fn record_callee(ip, fp, th, clo, md, dt, argc) {
    let site = slow::EnterLink {
        link: slow::generic_link(ip, md.as_ptr()),
        buffer: Value::from_gc(crate::runtime::value::CodeBuffer::md_buffer(md.as_ptr())),
    };
    dispatch_control!(
        slow::control_enter(th, fp, *fp.add(1), argc as usize, Some(site)),
        th,
        dt
    )
}}

op! { fn op_return<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpReturn, W, ip);
    *fp.add(2) = frame[op.src];
    return_values!(fp, th, clo, md, dt, 1)
}}

op! { fn op_return_values<W>(ip, fp, th, clo, md, dt, w) {
    let op = decode!(OpReturnValues, W, ip);
    return_values!(fp, th, clo, md, dt, op.count.0)
}}

op! { fn op_rest_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRestRef, W, ip);
    let index = op.index.0 as i32;
    if index >= (frame[op.cnt]).as_int32() {
        // `car` of an exhausted rest list.
        th.state().exit_value.set(Value::null());
        become raise_kind(ip, fp, th, clo, md, dt, RaiseKind::CarNotPair.code() as u64)
    }
    frame[op.dst] = *fp.add(op.base.0 as usize + index as usize);
    advance!(OpRestRef, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_rest_len<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRestLen, W, ip);
    frame[op.dst] = Value::new(frame[op.cnt].as_int32() - op.skip.0 as i32);
    advance!(OpRestLen, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_rest_null<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRestNull, W, ip);
    frame[op.dst] = Value::new(frame[op.cnt].as_int32() == op.skip.0 as i32);
    advance!(OpRestNull, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_rest_pair<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRestPair, W, ip);
    frame[op.dst] = Value::new(frame[op.cnt].as_int32() > op.skip.0 as i32);
    advance!(OpRestPair, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_rest_list<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpRestList, W, ip);
    slow::rest_list_body(th, frame, &op);
    advance!(OpRestList, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_values_rest<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpValuesRest, W, ip);
    slow::values_rest_body(th, frame, &op);
    advance!(OpValuesRest, W, ip, fp, th, clo, md, dt)
}}

macro_rules! prim_op {
    ($name:ident) => {
        op! { fn $name<W>(ip, fp, th, clo, md, dt, w) {
            match slow::prim_step(th, fp, ip) {
                Ok(next_ip) => next!(next_ip, fp, th, clo, md, dt),
                Err(kind) => become raise_kind(ip, fp, th, clo, md, dt, kind as u64),
            }
        }}
    };
}

prim_op!(op_call_primitive0);
prim_op!(op_call_primitive1);
prim_op!(op_call_primitive2);
prim_op!(op_call_primitive3);
prim_op!(op_call_primitive_n);

op! { fn op_car<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpCar, W, ip);
    let pair = frame[op.src];
    if !pair.is::<Pair>() {
        th.state().exit_value.set(pair);
        become raise_kind(ip, fp, th, clo, md, dt, RaiseKind::CarNotPair.code() as u64)
    }
    let value = pair.car();
    (*md.entry::<def::OpCar>(op.metadata_id)).profile.observe(value);
    frame[op.dst] = value;
    advance!(OpCar, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_cdr<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpCdr, W, ip);
    let pair = frame[op.src];
    if !pair.is::<Pair>() {
        th.state().exit_value.set(pair);
        become raise_kind(ip, fp, th, clo, md, dt, RaiseKind::CdrNotPair.code() as u64)
    }
    let value = pair.cdr();
    (*md.entry::<def::OpCdr>(op.metadata_id)).profile.observe(value);
    frame[op.dst] = value;
    advance!(OpCdr, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_cons<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpCons, W, ip);
    slow::cons_body(th, frame, &op);
    advance!(OpCons, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_set_car<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpSetCar, W, ip);
    let pair = frame[op.pair];
    barrier::field!(Gc::write(th, pair.downcast::<Pair>()), Pair, car)
        .unlock()
        .set(frame[op.src]);
    advance!(OpSetCar, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_set_cdr<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpSetCdr, W, ip);
    let pair = frame[op.pair];
    barrier::field!(Gc::write(th, pair.downcast::<Pair>()), Pair, cdr)
        .unlock()
        .set(frame[op.src]);
    advance!(OpSetCdr, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_eq<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpEq, W, ip);
    frame[op.dst] = Value::new((frame[op.lhs]).bits() == (frame[op.rhs]).bits());
    advance!(OpEq, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_not<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpNot, W, ip);
    frame[op.dst] = Value::new(!frame[op.src]);
    advance!(OpNot, W, ip, fp, th, clo, md, dt)
}}

macro_rules! arith {
    ($name:ident, $Op:ident, $checked:ident) => {
        op! { fn $name<W>(ip, fp, th, clo, md, dt, w) {
            let mut frame = CallFrame::create(fp);
            let op = decode!($Op, W, ip);
            let x = frame[op.lhs];
            let y = frame[op.rhs];
            if x.is_int32() && y.is_int32() {
                let profile = &(*md.entry::<def::$Op>(op.metadata_id)).profile;
                profile.observe_fixnums();
                if let Some(result) = x.as_int32().$checked(y.as_int32()) {
                    frame[op.dst] = Value::from_i32(result);
                    advance!($Op, W, ip, fp, th, clo, md, dt)
                }
            }
            become arith_slow(ip, fp, th, clo, md, dt, w)
        }}
    };
}

arith!(op_add, OpAdd, checked_add);
arith!(op_sub, OpSub, checked_sub);
arith!(op_mul, OpMul, checked_mul);

fn div_fixnums(x: Value<'_>, y: Value<'_>) -> Option<(i32, i32)> {
    if !x.is_int32() || !y.is_int32() {
        return None;
    }
    let (lhs, rhs) = (x.as_int32(), y.as_int32());
    if rhs == 0 || (lhs == i32::MIN && rhs == -1) {
        return None;
    }
    Some((lhs, rhs))
}

op! { fn op_div<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpDiv, W, ip);
    if let Some((lhs, rhs)) = div_fixnums(frame[op.lhs], frame[op.rhs]) {
        if lhs % rhs == 0 {
            (*md.entry::<def::OpDiv>(op.metadata_id)).profile.observe_fixnums();
            frame[op.dst] = Value::from_i32(lhs / rhs);
            advance!(OpDiv, W, ip, fp, th, clo, md, dt)
        }
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_quotient<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpQuotient, W, ip);
    if let Some((lhs, rhs)) = div_fixnums(frame[op.lhs], frame[op.rhs]) {
        (*md.entry::<def::OpQuotient>(op.metadata_id)).profile.observe_fixnums();
        frame[op.dst] = Value::from_i32(lhs / rhs);
        advance!(OpQuotient, W, ip, fp, th, clo, md, dt)
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_remainder<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpRemainder, W, ip);
    if let Some((lhs, rhs)) = div_fixnums(frame[op.lhs], frame[op.rhs]) {
        (*md.entry::<def::OpRemainder>(op.metadata_id)).profile.observe_fixnums();
        frame[op.dst] = Value::from_i32(lhs % rhs);
        advance!(OpRemainder, W, ip, fp, th, clo, md, dt)
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_modulo<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpModulo, W, ip);
    if let Some((lhs, rhs)) = div_fixnums(frame[op.lhs], frame[op.rhs]) {
        // `Number::modulo`, fixnum arm: the remainder takes the divisor's
        // sign. `|rem| < |rhs|` keeps the adjustment in range.
        let mut rem = lhs % rhs;
        if rem != 0 && (rem > 0) != (rhs > 0) {
            rem = rem.wrapping_add(rhs);
        }
        (*md.entry::<def::OpModulo>(op.metadata_id)).profile.observe_fixnums();
        frame[op.dst] = Value::from_i32(rem);
        advance!(OpModulo, W, ip, fp, th, clo, md, dt)
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_add_immediate<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpAddImmediate, W, ip);
    let x = frame[op.src];
    if x.is_int32() {
        let profile = &(*md.entry::<def::OpAddImmediate>(op.metadata_id)).profile;
        profile.observe_fixnums();
        if let Some(result) = x.as_int32().checked_add(op.value.0) {
            frame[op.dst] = Value::from_i32(result);
            advance!(OpAddImmediate, W, ip, fp, th, clo, md, dt)
        }
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_add_immediate_move<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpAddImmediateMove, W, ip);
    let x = frame[op.src];
    if x.is_int32() {
        let profile = &(*md.entry::<def::OpAddImmediateMove>(op.metadata_id)).profile;
        profile.observe_fixnums();
        if let Some(result) = x.as_int32().checked_add(op.value.0) {
            let sum = Value::from_i32(result);
            frame[op.tmp] = sum;
            frame[op.dst] = sum;
            advance!(OpAddImmediateMove, W, ip, fp, th, clo, md, dt)
        }
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

macro_rules! numeric_cmp {
    ($name:ident, $Op:ident, $op:tt) => {
        op! { fn $name<W>(ip, fp, th, clo, md, dt, w) {
            let mut frame = CallFrame::create(fp);
            let op = decode!($Op, W, ip);
            let x = frame[op.lhs];
            let y = frame[op.rhs];
            if x.is_int32() && y.is_int32() {
                (*md.entry::<def::$Op>(op.metadata_id)).profile.observe_fixnums();
                frame[op.dst] = Value::new(x.as_int32() $op y.as_int32());
                advance!($Op, W, ip, fp, th, clo, md, dt)
            }
            become arith_slow(ip, fp, th, clo, md, dt, w)
        }}
    };
}

numeric_cmp!(op_less, OpLess, <);
numeric_cmp!(op_less_equal, OpLessEqual, <=);
numeric_cmp!(op_number_equal, OpNumberEqual, ==);
numeric_cmp!(op_greater_equal, OpGreaterEqual, >=);
numeric_cmp!(op_greater, OpGreater, >);

op! { fn op_negate<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpNegate, W, ip);
    let x = frame[op.src];
    if x.is_int32() {
        let profile = &(*md.entry::<def::OpNegate>(op.metadata_id)).profile;
        profile.observe_fixnums();
        if let Some(result) = x.as_int32().checked_neg() {
            frame[op.dst] = Value::from_i32(result);
            advance!(OpNegate, W, ip, fp, th, clo, md, dt)
        }
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_abs<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpAbs, W, ip);
    let x = frame[op.src];
    if x.is_int32() {
        // `Number::abs`, fixnum arm: only `MIN` needs the heap.
        let value = x.as_int32();
        if value != i32::MIN {
            let profile = &(*md.entry::<def::OpAbs>(op.metadata_id)).profile;
            profile.observe_fixnums();
            frame[op.dst] = Value::from_i32(value.abs());
            advance!(OpAbs, W, ip, fp, th, clo, md, dt)
        }
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_sqrt<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpSqrt, W, ip);
    let x = frame[op.src];

    if x.is_flonum() {
        let profile = &(*md.entry::<def::OpSqrt>(op.metadata_id)).profile;
        profile.observe_unary(x);
        frame[op.dst] = Value::from_f64(x.as_flonum().sqrt());
        advance!(OpSqrt, W, ip, fp, th, clo, md, dt)
    }
    become arith_slow(ip, fp, th, clo, md, dt, w)
}}

op! { #[cold] #[inline(never)] fn arith_slow(ip, fp, th, clo, md, dt, w) {
    let next_ip = slow::arith_slow_body(th, fp, ip, md.as_ptr());
    next!(next_ip, fp, th, clo, md, dt)
}}

op! { fn op_vector_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpVectorRef, W, ip);
    let vector = frame[op.vector];
    let index = frame[op.index];
    if let Some(value) = prims::vector_ref_fast(vector, index) {
        let meta = &*md.entry::<def::OpVectorRef>(op.metadata_id);
        meta.array.observe(ARRAY_FAST);
        meta.profile.observe(value);
        frame[op.dst] = value;
        advance!(OpVectorRef, W, ip, fp, th, clo, md, dt)
    }
    become vector_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_vector_ref_mul<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpVectorRefMul, W, ip);
    let vector = frame[op.vector];
    let index = frame[op.index];
    if let Some(value) = prims::vector_ref_fast(vector, index) {
        let meta = &*md.entry::<def::OpVectorRefMul>(op.metadata_id);
        meta.array.observe(ARRAY_FAST);
        meta.profile.observe(value);
        frame[op.vdst] = value;
        let x = frame[op.lhs];
        let y = frame[op.rhs];
        if x.is_int32() && y.is_int32() {
            meta.arith.observe_fixnums();
            if let Some(result) = x.as_int32().checked_mul(y.as_int32()) {
                frame[op.mdst] = Value::from_i32(result);
                advance!(OpVectorRefMul, W, ip, fp, th, clo, md, dt)
            }
        }
        become arith_slow(ip, fp, th, clo, md, dt, w)
    }
    become vector_slow(ip, fp, th, clo, md, dt, w)
}}

op! { fn op_vector_set<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpVectorSet, W, ip);
    let vector = frame[op.vector];
    let index = frame[op.index];
    let value = frame[op.src];
    if prims::vector_set_fast(th, vector, index, value) {
        (*md.entry::<def::OpVectorSet>(op.metadata_id)).array.observe(ARRAY_FAST);
        advance!(OpVectorSet, W, ip, fp, th, clo, md, dt)
    }
    become vector_slow(ip, fp, th, clo, md, dt, w)
}}

op! { #[cold] #[inline(never)] fn vector_slow(ip, fp, th, clo, md, dt, w) {
    let next_ip = slow::vector_slow_body(th, fp, ip, md.as_ptr());
    next!(next_ip, fp, th, clo, md, dt)
}}

op! { fn op_vector_length<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpVectorLength, W, ip);
    let vector = frame[op.src];
    if vector.is::<Vector>() {
        frame[op.dst] = vector.downcast::<Vector>().len().into_value(th);
        advance!(OpVectorLength, W, ip, fp, th, clo, md, dt)
    }
    become vector_length_slow(ip, fp, th, clo, md, dt, w)
}}

op! { #[cold] #[inline(never)] fn vector_length_slow(ip, fp, th, clo, md, dt, w) {
    match slow::vector_length_step(th, fp, ip) {
        Some(next_ip) => next!(next_ip, fp, th, clo, md, dt),
        None => become raise_in_frame(ip, fp, th, clo, md, dt, 0),
    }
}}

op! { fn op_variable_ref<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpVariableRef, W, ip);
    let value = frame[op.var].downcast::<Variable>().value.get();
    (*md.entry::<def::OpVariableRef>(op.metadata_id)).profile.observe(value);
    frame[op.dst] = value;
    advance!(OpVariableRef, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_variable_set<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpVariableSet, W, ip);
    let variable = frame[op.var];
    barrier::field!(
        Gc::write(th, variable.downcast::<Variable>()),
        Variable,
        value
    )
    .unlock()
    .set(frame[op.src]);
    advance!(OpVariableSet, W, ip, fp, th, clo, md, dt)
}}

op! { fn op_get_slot<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpGetSlot, W, ip);
    let obj = frame[op.obj];
    let meta = &*md.entry::<def::OpGetSlot>(op.metadata_id);
    if let Some(value) = slow::get_slot_fast(obj, meta) {
        meta.profile.observe(value);
        frame[op.dst] = value;
        advance!(OpGetSlot, W, ip, fp, th, clo, md, dt)
    }
    become get_slot_miss::<W>(ip, fp, th, clo, md, dt, w)
}}

op! { #[cold] #[inline(never)] fn get_slot_miss<W>(ip, fp, th, clo, md, dt, w) {
    let mut frame = CallFrame::create(fp);
    let op = decode!(OpGetSlot, W, ip);
    if let Some(value) = slow::get_slot_miss_body(th, frame, md.as_ptr(), &op) {
        frame[op.dst] = value;
        advance!(OpGetSlot, W, ip, fp, th, clo, md, dt)
    }
    // Uncacheable or unbound: run the out-of-line call to `slot-ref`.
    next!(ip.offset(op.slow as isize), fp, th, clo, md, dt)
}}

op! { fn op_put_slot<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpPutSlot, W, ip);
    let obj = frame[op.obj];
    let meta = &*md.entry::<def::OpPutSlot>(op.metadata_id);
    if slow::put_slot_fast(th, obj, frame[op.src], meta) {
        advance!(OpPutSlot, W, ip, fp, th, clo, md, dt)
    }
    become put_slot_miss::<W>(ip, fp, th, clo, md, dt, w)
}}

op! { #[cold] #[inline(never)] fn put_slot_miss<W>(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let op = decode!(OpPutSlot, W, ip);
    if slow::put_slot_miss_body(th, frame, md.as_ptr(), &op) {
        advance!(OpPutSlot, W, ip, fp, th, clo, md, dt)
    }
    // Uncacheable: run the out-of-line call to `slot-set!`.
    next!(ip.offset(op.slow as isize), fp, th, clo, md, dt)
}}

op! { fn exit_to_template(ip, fp, th, clo, md, dt, w) {
    let frame = CallFrame::create(fp);
    let tc = th.tc();
    tc.stack.fp.set(fp);
    tc.ip.set(ip);
    tc.ac.set(w);

    let (width, opcode, operands) = def::decode_header(ip);
    let frame_slots = match opcode {
        OpcodeId::OpReturnPoint => Some(def::OpReturnPoint::decode_from_ptr(operands, width).0.frame.0 as usize),
        OpcodeId::OpReturnPointForward => {
            Some(def::OpReturnPointForward::decode_from_ptr(operands, width).0.frame.0 as usize)
        }
        OpcodeId::OpReturnPointValues => {
            Some(def::OpReturnPointValues::decode_from_ptr(operands, width).0.frame.0 as usize)
        }
        OpcodeId::OpReturnPointValuesRest => {
            Some(def::OpReturnPointValuesRest::decode_from_ptr(operands, width).0.frame.0 as usize)
        }
        _ => None,
    };
    let cp = match frame_slots {
        Some(slots) => {
            let caller = frame.sub(slots);
            debug_assert!(caller.callee().is::<Closure>());
            caller.callee()
        }
        None => {
            debug_assert!(frame.callee().is::<Closure>());
            frame.callee()
        }
    };
    tc.cp.set(cp);
    if w == 1 && opcode == OpcodeId::OpReturnPoint {
        tc.rv.set(*fp.add(2));
    }
    return NativeReturn {
        code: ReturnCode::ResumeTemplate,
        value: Value::undefined(),
    };
}}

static EXIT_TO_TEMPLATE: DispatchTable = DispatchTable([exit_to_template; 256]);

#[repr(align(64))]
struct DispatchTable([Handler; 256]);

static DISPATCH_NARROW: DispatchTable = DispatchTable(build_dispatch_table::<0>());
static DISPATCH_WIDE16: DispatchTable = DispatchTable(build_dispatch_table::<1>());
static DISPATCH_WIDE32: DispatchTable = DispatchTable(build_dispatch_table::<2>());

const fn build_dispatch_table<const W: u8>() -> [Handler; 256] {
    let mut table: [Handler; 256] = [op_invalid; 256];
    macro_rules! set {
        ($($Op:ident => $handler:ident,)*) => {
            $( table[def::OpcodeId::$Op as usize] = $handler::<W>; )*
        };
    }
    set! {
        OpEnter => op_enter,
        OpEnterRest => op_enter_rest,
        OpReturnPoint => op_return_point,
        OpReturnPointForward => op_return_point_forward,
        OpReturnPointValues => op_return_point_values,
        OpReturnPointValuesRest => op_return_point_values_rest,
        OpMove => op_move,
        OpLoadConstant => op_load_constant,
        OpLoadImmediate => op_load_immediate,
        OpLoadRaw => op_load_raw,
        OpCellRef => op_cell_ref,
        OpCellSet => op_cell_set,
        OpRuntimeProcedure => op_runtime_procedure,
        OpClosure => op_closure,
        OpClosureEnv => op_closure_env,
        OpFreeRef => op_free_ref,
        OpClosureRef => op_closure_ref,
        OpClosureSet => op_closure_set,
        OpMakeEnv => op_make_env,
        OpEnvRef => op_env_ref,
        OpEnvSet => op_env_set,
        OpJump => op_jump,
        OpJumpIfTrue => op_jump_if_true,
        OpJumpIfFalse => op_jump_if_false,
        OpJumpIfNull => op_jump_if_null,
        OpJumpIfNotNull => op_jump_if_not_null,
        OpJumpIfPair => op_jump_if_pair,
        OpJumpIfNotPair => op_jump_if_not_pair,
        OpJumpIfFixnum => op_jump_if_fixnum,
        OpJumpIfNotFixnum => op_jump_if_not_fixnum,
        OpJumpIfUndefined => op_jump_if_undefined,
        OpJumpIfNotUndefined => op_jump_if_not_undefined,
        OpJumpIfEq => op_jump_if_eq,
        OpJumpIfNotEq => op_jump_if_not_eq,
        OpJumpIfLess => op_jump_if_less,
        OpJumpIfLessEqual => op_jump_if_less_equal,
        OpJumpIfNumberEqual => op_jump_if_number_equal,
        OpJumpIfGreaterEqual => op_jump_if_greater_equal,
        OpJumpIfGreater => op_jump_if_greater,
        OpJumpIfNotLess => op_jump_if_not_less,
        OpJumpIfNotLessEqual => op_jump_if_not_less_equal,
        OpJumpIfNotNumberEqual => op_jump_if_not_number_equal,
        OpJumpIfNotGreaterEqual => op_jump_if_not_greater_equal,
        OpJumpIfNotGreater => op_jump_if_not_greater,
        OpSwitchFixnum => op_switch_fixnum,
        OpSwitchChar => op_switch_char,
        OpSwitchSymbol => op_switch_symbol,
        OpLoopHint => op_loop_hint,
        OpCall => op_call,
        OpTailCall => op_tail_call,
        OpCallSelf => op_call_self,
        OpTailCallSelf => op_tail_call_self,
        OpReturn => op_return,
        OpReturnValues => op_return_values,
        OpRaise => op_raise,
        OpRestRef => op_rest_ref,
        OpRestLen => op_rest_len,
        OpRestNull => op_rest_null,
        OpRestPair => op_rest_pair,
        OpRestList => op_rest_list,
        OpValuesRest => op_values_rest,
        OpCallPrimitive0 => op_call_primitive0,
        OpCallPrimitive1 => op_call_primitive1,
        OpCallPrimitive2 => op_call_primitive2,
        OpCallPrimitive3 => op_call_primitive3,
        OpCallPrimitiveN => op_call_primitive_n,
        OpCar => op_car,
        OpCdr => op_cdr,
        OpCons => op_cons,
        OpSetCar => op_set_car,
        OpSetCdr => op_set_cdr,
        OpEq => op_eq,
        OpNot => op_not,
        OpAdd => op_add,
        OpSub => op_sub,
        OpMul => op_mul,
        OpAddImmediate => op_add_immediate,
        OpLess => op_less,
        OpLessEqual => op_less_equal,
        OpNumberEqual => op_number_equal,
        OpGreaterEqual => op_greater_equal,
        OpGreater => op_greater,
        OpDiv => op_div,
        OpQuotient => op_quotient,
        OpRemainder => op_remainder,
        OpModulo => op_modulo,
        OpNegate => op_negate,
        OpAbs => op_abs,
        OpSqrt => op_sqrt,
        OpVectorRef => op_vector_ref,
        OpVectorSet => op_vector_set,
        OpVectorLength => op_vector_length,
        OpVariableRef => op_variable_ref,
        OpVariableSet => op_variable_set,
        OpGetSlot => op_get_slot,
        OpPutSlot => op_put_slot,
        OpMoveTwo => op_move_two,
        OpAddImmediateMove => op_add_immediate_move,
        OpVectorRefMul => op_vector_ref_mul,
    }
    table[def::OpcodeId::OpNop as usize] = op_nop::<W>;
    if W == 0 {
        table[def::WIDE16_PREFIX as usize] = op_wide16;
        table[def::WIDE32_PREFIX as usize] = op_wide32;
    }
    table
}
