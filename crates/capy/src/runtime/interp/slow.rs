//! Slow paths shared by the Rust engine and the template interpreter.
//!
//! Both engines execute the same slow paths, and they differ only in how control continues afterwards:
//!
//! - value bodies compute a result (and park the frame, observe profiles,
//!   store it) and return the address of the next instruction. The Rust
//!   engine dispatches there, the template interpreter stores `Tc::ip`/`Tc::cp` and resumes at
//!   `interpreter_resume`.
//! - the control part ([`control_enter`], [`control_return`], ...) runs the
//!   enter/return/raise/native/continue logic in one iterative loop and
//!   returns a [`Control<'gc>`] decision. The Rust engine maps it back to `become`
//!   or `return` (`dispatch_control!`), while the template interpreter stores the `Tc` resume slots and
//!   jumps to the returned machine address.
//!
//! Native procedures run inside [`control_run_native`], wrapped in
//! `catch_unwind`: a panic becomes a Scheme error raised above the native's
//! frame. Thunk failures (`thunk_raise`) bypass all of this with a `longjmp` to
//! `run_activation`.

use std::sync::atomic::Ordering::{Acquire, Relaxed};

use super::frame::CallFrame;
use super::ic;
use super::markers;
use super::profile::{
    ARRAY_FAST, ARRAY_FIXNUM_INDEX, ARRAY_NON_FIXNUM_INDEX, ARRAY_NON_VECTOR, ARRAY_OUT_OF_BOUNDS,
    ARRAY_VECTOR, CallLinkInfo, LINK_POLY, SLOT_KIND_ACCESS, SlotIcEntry, SlotSite,
};
use crate::bytecode::def::{self, *};
use crate::heap::Gc;
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::object::builtin_class_ids;
use crate::runtime::MutationContext;
use crate::runtime::class::{
    GenericDescriptor, GenericDispatchError, MethodDescriptor, NextMethodDescriptor, SchemeInstance,
};
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
use crate::runtime::codegen::jit::{ThresholdPolicy, install, request_tier_up};
use crate::runtime::interp::stack::{self, Ip, Slot};
use crate::runtime::prims::calls::{self as prims, PrimError};
use crate::runtime::prims::def::Primitive;
use crate::runtime::value::{
    Closure, CodeBlock, CodeBuffer, EnvRecord, NativeFn, NativeProc, ReturnCode, Str, Symbol, Value,
};
use crate::runtime::vm::exceptions::make_raise_condition_with_source;
use crate::runtime::vm::thunks;
use crate::runtime::watchpoint::{Dependent, SiteOp, WatchpointSet};

/// Park the frame for the runtime before calling out of the interpreter,
/// through a [`MutationContext`]. Only the closure slot is reported live:
/// an outcall from the middle of a procedure body is not a safepoint for
/// its registers.
#[inline(always)]
pub(crate) unsafe fn park_frame<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>) {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2));
    }
}

pub(crate) fn source_at<'gc>(clo: Option<Gc<'gc, Closure<'gc>>>, ip: Ip) -> Value<'gc> {
    let Some(clo) = clo else {
        return Value::new(false);
    };
    let code_block = clo.code_block;
    code_block
        .pc_of(ip)
        .and_then(|pc| code_block.source_at_exact(pc))
        .map(|hit| hit.span)
        .unwrap_or(Value::new(false))
}

/// Push a frame for the exception handler above the frame an error was
/// raised in and return it. The handler returns to `raise_ret`, which ends
/// the activation with the handler's result.
///
/// # Safety
///
/// The stack state's `fp` must be the frame the error was raised in.
pub unsafe fn push_raise_frame<'gc>(ctx: MutationContext<'gc>, condition: Value<'gc>) -> Slot<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        let mut faulting = stack.fp.get();
        let mut top = stack::frame_top(ctx, faulting);
        if top.add(8) > stack.end() {
            stack.sp.set(top);
            faulting = stack::overflow(ctx, 16);
            top = stack.sp.get();
        }
        let handler_frame = top.add(1);
        *top = Value::from_raw(handler_frame.offset_from(faulting) as u64);
        stack::set_return_address(handler_frame, markers::raise_ret());
        *handler_frame.add(1) = thunks::exception_handler(ctx);
        *handler_frame.add(2) = condition;
        stack.fp.set(handler_frame);
        stack.sp.set(handler_frame.add(3));
        handler_frame
    }
}

/// Park for the collector if one was requested, honouring the sign
/// (`is_loop` takes only `+1`). The caller parks its frame first (`fp`
/// and `sp`, plus `top_ip` at a loop header).
#[cold]
pub(crate) unsafe fn yield_body<'gc>(ctx: MutationContext<'gc>, is_loop: bool) {
    let flag = ctx.take_yieldpoint();
    let take = if is_loop { flag > 0 } else { flag != 0 };
    if take {
        thunks::yieldpoint_block(ctx);
    }
}

/// The `CallLinkInfo` of the `call`/`tail-call`/`call-self`/`tail-call-self` at `ip`.
///
/// # Safety
/// `ip` points at a call of the running code block, and `md`
/// is its metadata area. The link borrows a non-moving code buffer the
/// running closure keeps alive, it must not outlive the current call.
pub(crate) unsafe fn generic_link(ip: Ip, md: *mut u8) -> &'static CallLinkInfo {
    unsafe {
        let (width, opcode, operands) = def::decode_header(ip);
        match opcode {
            OpcodeId::OpCall => {
                let (op, _) = def::OpCall::decode_from_ptr(operands, width);
                &(*def::OpCall::metadata_ptr(md, op.metadata_id)).link
            }
            OpcodeId::OpTailCall => {
                let (op, _) = def::OpTailCall::decode_from_ptr(operands, width);
                &(*def::OpTailCall::metadata_ptr(md, op.metadata_id)).link
            }
            OpcodeId::OpCallSelf => {
                let (op, _) = def::OpCallSelf::decode_from_ptr(operands, width);
                &(*def::OpCallSelf::metadata_ptr(md, op.metadata_id)).link
            }
            OpcodeId::OpTailCallSelf => {
                let (op, _) = def::OpTailCallSelf::decode_from_ptr(operands, width);
                &(*def::OpTailCallSelf::metadata_ptr(md, op.metadata_id)).link
            }
            _ => unreachable!("call site link away from a call"),
        }
    }
}

pub(crate) struct GenericCacheHit<'gc> {
    pub(crate) methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
}

/// The cached method chain when the call in `fp` hits `link`'s dispatch
/// cache: the same generic, an unchanged epoch, at least as many arguments
/// as the fill saw, and the same key classes.
pub(crate) unsafe fn check_generic_cache<'gc>(
    link: &CallLinkInfo,
    callee: Value<'gc>,
    fp: Slot<'gc>,
    argc: usize,
) -> Option<GenericCacheHit<'gc>> {
    unsafe {
        let entry = link.read_generic()?;
        let generic = callee.downcast::<GenericDescriptor>();
        if !Gc::ptr_eq(entry.generic, generic) {
            return None;
        }
        if entry.epoch != generic.epoch() || argc < entry.min_argc as usize {
            return None;
        }
        for (index, expected) in entry
            .class_ids
            .iter()
            .enumerate()
            .take(entry.key_len as usize)
        {
            let arg = *fp.add(2 + index);
            let class_id = GenericDescriptor::dispatch_class_id(arg)?;
            if *expected != class_id.bits() {
                return None;
            }
        }
        let methods = entry.methods;
        if methods.is_empty() {
            return None;
        }
        Some(GenericCacheHit { methods })
    }
}

/// Cache a dispatch the miss path computed. Skips generics the key cannot
/// describe: more than two dispatch arguments, or methods that specialize
/// or require beyond the key.
pub(crate) fn fill_generic_cache<'gc>(
    ctx: MutationContext<'gc>,
    link: &CallLinkInfo,
    buffer: Value<'gc>,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
    methods: ArrayRef<'gc, Gc<'gc, MethodDescriptor<'gc>>>,
    args: &[Value<'gc>],
) {
    let key_len = generic.max_required_dispatch_args();
    if key_len > 2 {
        return;
    }
    // Every method must be decided by the key: nothing specialized past
    // it, and nothing requiring more arguments than this call passed, so a
    // hit with at least as many arguments dispatches identically.
    for method in generic.methods().iter() {
        if method.specializers().len() > key_len || method.required_arg_count() > args.len() {
            return;
        }
    }
    let mut class_ids = [0u32; 2];
    for (index, arg) in args.iter().take(key_len).enumerate() {
        let Some(class_id) = GenericDescriptor::dispatch_class_id(*arg) else {
            return;
        };
        class_ids[index] = class_id.bits();
    }
    let barrier = Gc::write(ctx, buffer.downcast::<CodeBuffer>());
    link.fill_generic(
        barrier,
        generic,
        generic.epoch(),
        &class_ids[..key_len],
        args.len() as u32,
        methods,
    );
}

/// Record the called code block at a call site: store the first callee, mark
/// the site polymorphic on the second. The caller checked neither.
pub(crate) fn record_link<'gc>(
    ctx: MutationContext<'gc>,
    link: &CallLinkInfo,
    buffer: Value<'gc>,
    block: Value<'gc>,
) {
    let current = link.callee.bits();
    if current == 0 {
        let barrier = Gc::write(ctx, buffer.downcast::<CodeBuffer>());
        link.callee.set_value(barrier, Some(block));
    } else if current != block.bits() && link.polymorphic.load(Relaxed) == 0 {
        link.polymorphic.store(LINK_POLY, Relaxed);
    }
}

macro_rules! arith_slow_arm {
    ($op:ident, $thunk:expr, $cmp:expr, $ctx:expr, $fp:expr, $md:expr, $operands:expr, $width:expr) => {{
        let (op, _) = def::$op::decode_from_ptr($operands, $width);
        let x = *CallFrame::create($fp).register(op.lhs);
        let y = *CallFrame::create($fp).register(op.rhs);
        let profile = &(*def::$op::metadata_ptr($md, op.metadata_id)).profile;
        profile.observe_operands(x, y);
        let result = $thunk($ctx, x, y);
        let overflowed = x.is_int32() && y.is_int32();
        // Comparisons produce a boolean, which carries no operand
        // information, so only arithmetic records its result kind.
        profile.observe_result((!$cmp).then_some(result), overflowed);
        *CallFrame::create($fp).register(op.dst) = result;
    }};
}

/// One `arith_slow` arm for a unary arithmetic instruction (`neg`, `abs`):
/// decode it, run it through its thunk, record the profile with the
/// operand mirrored into both halves, and store the result.
macro_rules! arith_slow_unary_arm {
    ($op:ident, $thunk:expr, $ctx:expr, $fp:expr, $md:expr, $operands:expr, $width:expr) => {{
        let (op, _) = def::$op::decode_from_ptr($operands, $width);
        let x = *CallFrame::create($fp).register(op.src);
        let profile = &(*def::$op::metadata_ptr($md, op.metadata_id)).profile;
        profile.observe_unary(x);
        let result = $thunk($ctx, x);
        // Only a `MIN` fixnum reaches the slow path here.
        profile.observe_result(Some(result), x.is_int32());
        *CallFrame::create($fp).register(op.dst) = result;
    }};
}

/// The slow path of `add`, `sub`, `mul`, `add-immediate`, `lt`, `le`, `numeq`, `ge`,
/// `gt`, `div`, `quotient`, `remainder`, `modulo`, `neg`, `abs`, `sqrt`:
/// anything the inline fast paths decline. Parks the frame, runs the
/// operation through its thunk (which `longjmp`s on a type error), and
/// returns the next instruction.
pub(crate) unsafe fn arith_slow_body<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
    md: *mut u8,
) -> Ip {
    let frame = CallFrame::create(fp);
    unsafe {
        frame.park(ctx);
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        match opcode {
            OpcodeId::OpAdd => arith_slow_arm!(
                OpAdd,
                thunks::number_plus,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpSub => arith_slow_arm!(
                OpSub,
                thunks::number_minus,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpMul => arith_slow_arm!(
                OpMul,
                thunks::number_times,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpAddImmediate => {
                let (op, _) = def::OpAddImmediate::decode_from_ptr(operands, width);
                let x = *frame.register(op.src);
                let y = Value::from_i32(op.value.0);
                let profile = &(*def::OpAddImmediate::metadata_ptr(md, op.metadata_id)).profile;
                profile.observe_operands(x, y);
                let result = thunks::number_plus(ctx, x, y);
                // The immediate is always a fixnum, so a fixnum `x` here
                // means the fast path failed on overflow.
                profile.observe_result(Some(result), x.is_int32());
                *frame.register(op.dst) = result;
            }
            OpcodeId::OpAddImmediateMove => {
                let (op, _) = def::OpAddImmediateMove::decode_from_ptr(operands, width);
                let x = *frame.register(op.src);
                let y = Value::from_i32(op.value.0);
                let profile = &(*def::OpAddImmediateMove::metadata_ptr(md, op.metadata_id)).profile;
                profile.observe_operands(x, y);
                let result = thunks::number_plus(ctx, x, y);
                // As in `OpAddImmediate`: a fixnum `x` here means overflow.
                profile.observe_result(Some(result), x.is_int32());
                *frame.register(op.tmp) = result;
                *frame.register(op.dst) = result;
            }
            OpcodeId::OpVectorRefMul => {
                vector_ref_mul_slow_body(ctx, fp, md, operands, width);
            }
            OpcodeId::OpLess => {
                arith_slow_arm!(
                    OpLess,
                    thunks::number_lt,
                    true,
                    ctx,
                    fp,
                    md,
                    operands,
                    width
                )
            }
            OpcodeId::OpLessEqual => {
                arith_slow_arm!(
                    OpLessEqual,
                    thunks::number_le,
                    true,
                    ctx,
                    fp,
                    md,
                    operands,
                    width
                )
            }
            OpcodeId::OpNumberEqual => arith_slow_arm!(
                OpNumberEqual,
                thunks::number_eq,
                true,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpGreaterEqual => {
                arith_slow_arm!(
                    OpGreaterEqual,
                    thunks::number_ge,
                    true,
                    ctx,
                    fp,
                    md,
                    operands,
                    width
                )
            }
            OpcodeId::OpGreater => {
                arith_slow_arm!(
                    OpGreater,
                    thunks::number_gt,
                    true,
                    ctx,
                    fp,
                    md,
                    operands,
                    width
                )
            }
            OpcodeId::OpDiv => arith_slow_arm!(
                OpDiv,
                thunks::number_div,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpQuotient => arith_slow_arm!(
                OpQuotient,
                thunks::quotient,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpRemainder => arith_slow_arm!(
                OpRemainder,
                thunks::remainder,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpModulo => arith_slow_arm!(
                OpModulo,
                thunks::modulo,
                false,
                ctx,
                fp,
                md,
                operands,
                width
            ),
            OpcodeId::OpNegate => {
                arith_slow_unary_arm!(OpNegate, thunks::negate, ctx, fp, md, operands, width)
            }
            OpcodeId::OpAbs => {
                arith_slow_unary_arm!(OpAbs, thunks::abs, ctx, fp, md, operands, width)
            }
            OpcodeId::OpSqrt => {
                let (op, _) = def::OpSqrt::decode_from_ptr(operands, width);
                let x = *frame.register(op.src);
                let profile = &(*def::OpSqrt::metadata_ptr(md, op.metadata_id)).profile;
                profile.observe_unary(x);
                // Mirror `prims.rs`: flonums take hardware sqrt even when
                // negative (`NaN`, never complex). `Number::sqrt` would
                // complex a negative flonum, so the thunk only sees the
                // rest. Fixnums take the slow path however square they are,
                // so this never counts as overflow.
                let result = if x.is_flonum() {
                    Value::from_f64(x.as_flonum().sqrt())
                } else {
                    thunks::sqrt(ctx, x)
                };
                profile.observe_result(Some(result), false);
                *frame.register(op.dst) = result;
            }
            _ => unreachable!("arith_slow away from an arithmetic instruction"),
        }
        next_ip
    }
}

/// One `compare_slow` arm: decode a numeric jump, run the comparison through
/// its thunk, and return the jump target or the fall-through.
macro_rules! compare_slow_arm {
    ($op:ident, $thunk:expr, $sense:expr, $ctx:expr, $fp:expr, $md:expr, $ip:expr, $operands:expr, $width:expr, $next_ip:expr) => {{
        let (op, _) = def::$op::decode_from_ptr($operands, $width);
        let x = *CallFrame::create($fp).register(op.lhs);
        let y = *CallFrame::create($fp).register(op.rhs);
        let profile = &(*def::$op::metadata_ptr($md, op.metadata_id)).profile;
        profile.observe_operands(x, y);
        profile.observe_result(None, false);
        if !!$thunk($ctx, x, y) == $sense {
            $ip.offset(op.target as isize)
        } else {
            $next_ip
        }
    }};
}

/// The slow path of the numeric jumps: non-fixnum operands. Parks the frame
/// and returns where to continue.
pub(crate) unsafe fn compare_slow_body<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
    md: *mut u8,
) -> Ip {
    unsafe {
        park_frame(ctx, fp);
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        match opcode {
            OpcodeId::OpJumpIfLess => compare_slow_arm!(
                OpJumpIfLess,
                thunks::number_lt,
                true,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfLessEqual => compare_slow_arm!(
                OpJumpIfLessEqual,
                thunks::number_le,
                true,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNumberEqual => compare_slow_arm!(
                OpJumpIfNumberEqual,
                thunks::number_eq,
                true,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfGreaterEqual => compare_slow_arm!(
                OpJumpIfGreaterEqual,
                thunks::number_ge,
                true,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfGreater => compare_slow_arm!(
                OpJumpIfGreater,
                thunks::number_gt,
                true,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNotLess => compare_slow_arm!(
                OpJumpIfNotLess,
                thunks::number_lt,
                false,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNotLessEqual => compare_slow_arm!(
                OpJumpIfNotLessEqual,
                thunks::number_le,
                false,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNotNumberEqual => compare_slow_arm!(
                OpJumpIfNotNumberEqual,
                thunks::number_eq,
                false,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNotGreaterEqual => compare_slow_arm!(
                OpJumpIfNotGreaterEqual,
                thunks::number_ge,
                false,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            OpcodeId::OpJumpIfNotGreater => compare_slow_arm!(
                OpJumpIfNotGreater,
                thunks::number_gt,
                false,
                ctx,
                fp,
                md,
                ip,
                operands,
                width,
                next_ip
            ),
            _ => unreachable!("compare_slow away from a numeric jump"),
        }
    }
}

pub(crate) fn array_bits<'gc>(vector: Value<'gc>, index: Value<'gc>) -> u32 {
    // A fast path that already checked everything ORs `ARRAY_FAST` instead,
    // this classifies all three axes for the slow paths.
    if prims::has_heap_class(vector, builtin_class_ids::VECTOR) && index.is_int32() {
        let index = index.as_int32();
        // SAFETY: `vector` is a vector.
        let len = unsafe { prims::vector_len(vector) } as i32;
        if index >= 0 && index < len {
            return ARRAY_FAST;
        }
        return ARRAY_VECTOR | ARRAY_FIXNUM_INDEX | ARRAY_OUT_OF_BOUNDS;
    }
    let mut bits = if prims::has_heap_class(vector, builtin_class_ids::VECTOR) {
        ARRAY_VECTOR
    } else {
        ARRAY_NON_VECTOR
    };
    if index.is_int32() {
        bits |= ARRAY_FIXNUM_INDEX;
    } else {
        bits |= ARRAY_NON_FIXNUM_INDEX;
    }
    bits
}

/// The slow path of a fused `vector_ref` + `mul`: either half may miss,
/// so the pair re-runs whole through the thunks. The fast path may have
/// already run (and profiled) the vector half, profile observations are
/// monotonic, so re-observing is benign.
unsafe fn vector_ref_mul_slow_body<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    md: *mut u8,
    operands: *const u8,
    width: bytecodegen::OperandWidth,
) {
    let frame = CallFrame::create(fp);
    unsafe {
        let (op, _) = def::OpVectorRefMul::decode_from_ptr(operands, width);
        let vector = *frame.register(op.vector);
        let index = *frame.register(op.index);
        let meta = &*def::OpVectorRefMul::metadata_ptr(md, op.metadata_id);
        meta.array.observe(array_bits(vector, index));
        let value = thunks::vector_ref(ctx, vector, index);
        meta.profile.observe(value);
        *frame.register(op.vdst) = value;
        let x = *frame.register(op.lhs);
        let y = *frame.register(op.rhs);
        meta.arith.observe_operands(x, y);
        let result = thunks::number_times(ctx, x, y);
        meta.arith
            .observe_result(Some(result), x.is_int32() && y.is_int32());
        *frame.register(op.mdst) = result;
    }
}

/// The slow path of `vector_ref`/`vector_set`: a bad vector or index. The
/// thunks raise the proper condition (via `longjmp`), otherwise the profile
/// is observed and, for `ref`, the value stored. Returns the next
/// instruction.
pub(crate) unsafe fn vector_slow_body<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
    md: *mut u8,
) -> Ip {
    let frame = CallFrame::create(fp);
    unsafe {
        frame.park(ctx);
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        match opcode {
            OpcodeId::OpVectorRef => {
                let (op, _) = def::OpVectorRef::decode_from_ptr(operands, width);
                let vector = *frame.register(op.vector);
                let index = *frame.register(op.index);
                let meta = &*def::OpVectorRef::metadata_ptr(md, op.metadata_id);
                meta.array.observe(array_bits(vector, index));
                let value = thunks::vector_ref(ctx, vector, index);
                meta.profile.observe(value);
                *frame.register(op.dst) = value;
            }
            OpcodeId::OpVectorSet => {
                let (op, _) = def::OpVectorSet::decode_from_ptr(operands, width);
                let vector = *frame.register(op.vector);
                let index = *frame.register(op.index);
                let value = *frame.register(op.src);
                let meta = &*def::OpVectorSet::metadata_ptr(md, op.metadata_id);
                meta.array.observe(array_bits(vector, index));
                thunks::vector_set(ctx, vector, index, value);
            }
            OpcodeId::OpVectorRefMul => {
                vector_ref_mul_slow_body(ctx, fp, md, operands, width);
            }
            _ => unreachable!("vector_slow away from a vector instruction"),
        }
        next_ip
    }
}

/// The slow path of `vector_length`: recheck (the value may have been a
/// vector after all), else stage an assertion condition for an in-frame
/// raise. Returns the next instruction, or `None` to raise.
pub(crate) unsafe fn vector_length_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
) -> Option<Ip> {
    let frame = CallFrame::create(fp);
    unsafe {
        frame.park(ctx);
        let (width, opcode, operands) = def::decode_header(ip);
        debug_assert_eq!(opcode, OpcodeId::OpVectorLength);
        let (op, _) = def::OpVectorLength::decode_from_ptr(operands, width);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        let vector = *frame.register(op.src);
        if prims::has_heap_class(vector, builtin_class_ids::VECTOR) {
            *frame.register(op.dst) = Value::from_i32(prims::vector_len(vector) as i32);
            return Some(next_ip);
        }
        let condition = thunks::make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(ctx, "vector-length: not a vector", true).into(),
            &[vector],
        );
        ctx.state().exit_value.set(condition);
        None
    }
}

/// Binary search `count` `[key][disp]` pairs starting at `table`.
#[inline(always)]
pub(crate) unsafe fn switch_search(table: *const u32, count: usize, key: i32) -> Option<isize> {
    unsafe {
        let (mut lo, mut hi) = (0usize, count);
        while lo < hi {
            let mid = (lo + hi) / 2;
            let probe = table.add(mid * 2).read() as i32;
            if probe == key {
                return Some(table.add(mid * 2 + 1).read() as i32 as isize);
            }
            if probe < key {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        None
    }
}

/// The displacement for `key` from an int switch table at `table` (either
/// [`SWITCH_DENSE_FLAG`](def::SWITCH_DENSE_FLAG) layout), `default` on a
/// miss. The count word was already read by the caller.
#[inline(always)]
pub(crate) unsafe fn switch_int_disp(
    table: *const u32,
    count_word: u32,
    default: isize,
    key: i32,
) -> isize {
    unsafe {
        if def::switch_table_is_dense(count_word) {
            let span = def::switch_table_len(count_word);
            let min = table.add(2).read() as i32;
            let index = key.wrapping_sub(min) as u32 as usize;
            if index < span {
                table.add(3 + index).read() as i32 as isize
            } else {
                default
            }
        } else {
            switch_search(table.add(2), count_word as usize, key).unwrap_or(default)
        }
    }
}

/// A `switch-fixnum`/`switch_char`/`switch-symbol`: the displacement to continue at
/// (a table hit, the default, or the fall-through). Reads the aux pool
/// through the running closure.
pub(crate) unsafe fn switch_body<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>, ip: Ip) -> Ip {
    let frame = CallFrame::create(fp);
    unsafe {
        let _ = ctx;
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        let buffer = frame.code_block().buffer;
        let pool = buffer.pool();
        match opcode {
            OpcodeId::OpSwitchFixnum => {
                let (op, _) = def::OpSwitchFixnum::decode_from_ptr(operands, width);
                let v = *frame.register(op.src);
                if !v.is_int32() {
                    return next_ip;
                }
                let table = pool.as_ptr().add(op.table.0 as usize);
                let default = table.read() as i32 as isize;
                let count = table.add(1).read();
                let disp = switch_int_disp(table, count, default, v.as_int32());
                ip.offset(disp)
            }
            OpcodeId::OpSwitchChar => {
                let (op, _) = def::OpSwitchChar::decode_from_ptr(operands, width);
                let v = *frame.register(op.src);
                if !v.is_char() {
                    return next_ip;
                }
                let table = pool.as_ptr().add(op.table.0 as usize);
                let default = table.read() as i32 as isize;
                let count = table.add(1).read();
                let disp = switch_int_disp(table, count, default, (v.bits() >> 16) as i32);
                ip.offset(disp)
            }
            OpcodeId::OpSwitchSymbol => {
                let (op, _) = def::OpSwitchSymbol::decode_from_ptr(operands, width);
                let v = *frame.register(op.src);
                if !prims::has_heap_class(v, builtin_class_ids::SYMBOL) {
                    return next_ip;
                }
                let table = pool.as_ptr().add(op.table.0 as usize);
                let default = table.read() as i32 as isize;
                let count = table.add(1).read() as usize;
                let mask = table.add(2).read() as u64 | (table.add(3).read() as u64) << 32;
                let key = v.downcast_unchecked::<Symbol>().hash.get() & mask;
                let table = table.add(4);
                let (mut lo, mut hi) = (0usize, count);
                let mut disp = default;
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    let probe = table.add(mid * 3).read() as u64
                        | (table.add(mid * 3 + 1).read() as u64) << 32;
                    if probe == key {
                        disp = table.add(mid * 3 + 2).read() as i32 as isize;
                        break;
                    }
                    if probe < key {
                        lo = mid + 1;
                    } else {
                        hi = mid;
                    }
                }
                ip.offset(disp)
            }
            _ => unreachable!("switch away from a switch instruction"),
        }
    }
}

/// `cons`: allocate the pair and store it. Allocation never collects
/// mid-body, so no parking.
pub(crate) unsafe fn cons_body<'gc>(ctx: MutationContext<'gc>, frame: CallFrame<'gc>, op: &OpCons) {
    unsafe {
        *frame.register(op.dst) =
            Value::cons(ctx, *frame.register(op.car), *frame.register(op.cdr));
    }
}

pub(crate) unsafe fn closure_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    op: &OpClosure,
) {
    unsafe {
        let code = frame.code_block().buffer.constant(op.constant.0 as usize);
        let closure = Closure::new(
            ctx,
            code.downcast_unchecked::<CodeBlock>(),
            op.nfree.0 as usize,
            false,
        );
        *frame.register(op.dst) = closure.into();
    }
}

pub(crate) unsafe fn closure_env_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    op: &OpClosureEnv,
) {
    unsafe {
        let code = frame.code_block().buffer.constant(op.constant.0 as usize);
        let closure = Closure::new(
            ctx,
            code.downcast_unchecked::<CodeBlock>(),
            op.nfree.0 as usize,
            false,
        );
        Closure::free_set(closure, 0, *frame.register(op.env));
        *frame.register(op.dst) = closure.into();
    }
}

pub(crate) unsafe fn make_env_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    op: &OpMakeEnv,
) {
    unsafe {
        *frame.register(op.dst) =
            Value::from_raw(EnvRecord::new_undefined(ctx, op.size.0 as usize).as_ptr() as u64);
    }
}

/// Cons `values[from..to)` of the frame into a list.
pub(crate) unsafe fn cons_range<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    from: usize,
    to: usize,
) -> Value<'gc> {
    unsafe {
        let mut list = Value::null();
        let mut index = to;
        while index > from {
            index -= 1;
            list = Value::cons(ctx, *frame.register(Register(index as u32)), list);
        }
        list
    }
}

pub(crate) unsafe fn rest_list_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    op: &OpRestList,
) {
    unsafe {
        let base = op.base.0 as usize;
        let skip = op.skip.0 as usize;
        let count = (*frame.register(op.cnt)).as_int32().max(0) as usize;
        *frame.register(op.dst) = cons_range(ctx, frame, base + skip.min(count), base + count);
    }
}

pub(crate) unsafe fn values_rest_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    op: &OpValuesRest,
) {
    unsafe {
        let base = op.base.0 as usize;
        let from = base + 2 + op.from.0 as usize;
        let count = (*frame.register(op.base)).as_int32().max(0) as usize;
        *frame.register(op.base) = cons_range(ctx, frame, from, from + count);
    }
}

/// A `primitive0`..`primitive_n`: run the primitive. Parks the frame like the
/// interpreter handlers, on failure stages the irritant for an in-frame
/// raise and returns the kind code.
pub(crate) unsafe fn prim_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
) -> Result<Ip, usize> {
    let frame = CallFrame::create(fp);
    unsafe {
        frame.park(ctx);
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        macro_rules! run {
            ($op:expr, $dst:expr, $args:expr) => {{
                let prim = *Primitive::ALL.get_unchecked($op.primitive.0 as usize);
                match prims::call_prim(ctx, prim, $args) {
                    Ok(value) => {
                        *frame.register($dst) = value;
                        return Ok(next_ip);
                    }
                    Err(PrimError { kind, irritant }) => {
                        ctx.state().exit_value.set(irritant);
                        return Err(kind.code());
                    }
                }
            }};
        }
        match opcode {
            OpcodeId::OpCallPrimitive0 => {
                let (op, _) = def::OpCallPrimitive0::decode_from_ptr(operands, width);
                run!(op, op.dst, &[]);
            }
            OpcodeId::OpCallPrimitive1 => {
                let (op, _) = def::OpCallPrimitive1::decode_from_ptr(operands, width);
                let args = [*frame.register(op.a)];
                run!(op, op.dst, &args);
            }
            OpcodeId::OpCallPrimitive2 => {
                let (op, _) = def::OpCallPrimitive2::decode_from_ptr(operands, width);
                let args = [*frame.register(op.a), *frame.register(op.b)];
                run!(op, op.dst, &args);
            }
            OpcodeId::OpCallPrimitive3 => {
                let (op, _) = def::OpCallPrimitive3::decode_from_ptr(operands, width);
                let args = [
                    *frame.register(op.a),
                    *frame.register(op.b),
                    *frame.register(op.c),
                ];
                run!(op, op.dst, &args);
            }
            OpcodeId::OpCallPrimitiveN => {
                let (op, _) = def::OpCallPrimitiveN::decode_from_ptr(operands, width);
                let args =
                    std::slice::from_raw_parts(fp.add(op.first.0 as usize), op.argc.0 as usize);
                run!(op, op.dst, args);
            }
            _ => unreachable!("prim away from a prim instruction"),
        }
    }
}

/// The instance of `obj` when the published `entry` describes it: a
/// matching class and storage carrying the entry's layout stamp, so
/// `entry.index` is in bounds.
#[inline(always)]
unsafe fn slot_entry_instance<'gc>(
    obj: Value<'gc>,
    entry: &SlotIcEntry<'gc>,
) -> Option<Gc<'gc, SchemeInstance<'gc>>> {
    unsafe {
        if !obj.is_cell() {
            return None;
        }
        if obj.as_cell_raw().class_id().bits() != entry.class_id {
            return None;
        }
        // SAFETY: a Scheme class id on the header means a `SchemeInstance`
        // layout. The validated entry pairs `index` with `stamp`, and storage
        // carrying that stamp was built for the descriptor `index` came from.
        let instance: Gc<SchemeInstance> = Gc::from_gc_object(obj.as_cell_raw());
        (instance.storage().stamp() == entry.stamp).then_some(instance)
    }
}

/// The class id on `obj`'s header, if it is a heap cell.
#[inline(always)]
unsafe fn cell_class_bits(obj: Value<'_>) -> Option<u32> {
    unsafe { obj.is_cell().then(|| obj.as_cell_raw().class_id().bits()) }
}

/// The `get_slot` fast path: the site's entry for the object's class, the
/// instance carrying the entry's layout stamp, and a bound slot. Returns the
/// value without storing or profiling it.
pub(crate) unsafe fn get_slot_fast<'gc>(
    obj: Value<'gc>,
    meta: &OpGetSlotMetadata,
) -> Option<Value<'gc>> {
    unsafe {
        let entry = meta.cache.find(cell_class_bits(obj)?)?;
        let instance = slot_entry_instance(obj, &entry)?;
        let value = instance.slot_checked(entry.index as usize)?;
        (!value.is_empty()).then_some(value)
    }
}

/// Empty the cache of the `get_slot`/`put_slot` site `metadata_id` of
/// `buffer`: a watchpoint it depended on fired.
pub(crate) fn clear_slot_site<'gc>(buffer: Gc<'gc, CodeBuffer<'gc>>, metadata_id: u32, op: SiteOp) {
    let id = def::MetadataId(metadata_id);
    // SAFETY: the dependent was registered for a site of this buffer, so the
    // area holds an entry `id` of that op, and the buffer never moves.
    unsafe {
        let md = buffer.metadata_area();
        match op {
            SiteOp::GetSlot => (*def::OpGetSlot::metadata_ptr(md, id)).cache.clear(),
            SiteOp::PutSlot => (*def::OpPutSlot::metadata_ptr(md, id)).cache.clear(),
        }
    }
}

/// Fill a slot site after a lookup: publish an immutable entry, in the
/// inline slot when it is free or already this class, else among the extra
/// ones.
fn fill_slot_site<'gc>(
    ctx: MutationContext<'gc>,
    buffer: Gc<'gc, CodeBuffer<'gc>>,
    metadata_id: def::MetadataId,
    kind: SiteOp,
    site: &SlotSite,
    hit: &ic::SlotHit<'gc>,
) -> bool {
    // Depend on the class's slot protocol before publishing, and recheck
    // after: a fire in between may have swept before this entry existed.
    if !WatchpointSet::add(hit.wp, ctx, Dependent::site(buffer, metadata_id.0, kind)) {
        return false;
    }
    let entry = Gc::new(
        ctx,
        SlotIcEntry {
            class_id: hit.class_id,
            stamp: hit.stamp,
            index: hit.index,
            kind: SLOT_KIND_ACCESS,
            effective: Value::empty(),
        },
    );
    let stored = site.fill(ctx, &Gc::write(ctx, buffer), entry);
    if hit.wp.is_invalid() {
        site.clear();
        return false;
    }
    stored
}

/// The `get_slot` miss path: look the access up, fill the cache on a hit,
/// and return the value, or `None` to run the out-of-line `slot-ref`.
pub(crate) unsafe fn get_slot_miss_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    md: *mut u8,
    op: &OpGetSlot,
) -> Option<Value<'gc>> {
    unsafe {
        frame.park(ctx);
        let obj = *frame.register(op.obj);
        let buffer = frame.code_block().buffer;
        let meta = &*def::OpGetSlot::metadata_ptr(md, op.metadata_id);
        // A class served by one of the site's extra entries skips the lookup.
        if let Some(value) = get_slot_fast(obj, meta) {
            meta.profile.observe(value);
            return Some(value);
        }
        let name = buffer.constant(op.name.0 as usize);
        let hit = ic::slot_lookup(ctx, obj, name, false)?;
        // SAFETY: the lookup touched a live instance of the cached class.
        let instance: Gc<SchemeInstance> = Gc::from_gc_object(obj.as_cell_raw());
        let value = instance.slot_checked(hit.index as usize)?;
        if value.is_empty() {
            return None;
        }
        meta.profile.observe(value);
        fill_slot_site(
            ctx,
            buffer,
            op.metadata_id,
            SiteOp::GetSlot,
            &meta.cache,
            &hit,
        );
        Some(value)
    }
}

/// The `put_slot` fast path: like `get_slot`'s, storing `src` on a hit.
pub(crate) unsafe fn put_slot_fast<'gc>(
    ctx: MutationContext<'gc>,
    obj: Value<'gc>,
    src: Value<'gc>,
    meta: &OpPutSlotMetadata,
) -> bool {
    unsafe {
        let Some(class_id) = cell_class_bits(obj) else {
            return false;
        };
        let Some(entry) = meta.cache.find(class_id) else {
            return false;
        };
        let Some(instance) = slot_entry_instance(obj, &entry) else {
            return false;
        };
        SchemeInstance::set_slot_checked(ctx, instance, entry.index as usize, src)
    }
}

/// The `put_slot` miss path: look the access up, fill the cache, store, and
/// return whether the out-of-line `slot-set!` can be skipped.
pub(crate) unsafe fn put_slot_miss_body<'gc>(
    ctx: MutationContext<'gc>,
    frame: CallFrame<'gc>,
    md: *mut u8,
    op: &OpPutSlot,
) -> bool {
    unsafe {
        frame.park(ctx);
        let obj = *frame.register(op.obj);
        let buffer = frame.code_block().buffer;
        let meta = &*def::OpPutSlot::metadata_ptr(md, op.metadata_id);
        if put_slot_fast(ctx, obj, *frame.register(op.src), meta) {
            return true;
        }
        let name = buffer.constant(op.name.0 as usize);
        let Some(hit) = ic::slot_lookup(ctx, obj, name, true) else {
            return false;
        };
        // SAFETY: as in `get_slot_miss_body`.
        let instance: Gc<SchemeInstance> = Gc::from_gc_object(obj.as_cell_raw());
        let meta = &*def::OpPutSlot::metadata_ptr(md, op.metadata_id);
        fill_slot_site(
            ctx,
            buffer,
            op.metadata_id,
            SiteOp::PutSlot,
            &meta.cache,
            &hit,
        );
        SchemeInstance::set_slot_checked(ctx, instance, hit.index as usize, *frame.register(op.src))
    }
}

/// What the shared control core decided. Both engines map it to their own
/// transfer: the Rust engine dispatches (`become`) or returns, the template interpreter stages
/// the `Tc` resume slots and jumps to a machine address.
pub(crate) enum Control<'gc> {
    /// Resume bytecode at `ip` with `fp`, running closure `cp`, count `ac`,
    /// and, for a single value, `rv`.
    Resume {
        fp: Slot<'gc>,
        cp: Value<'gc>,
        ac: u64,
        rv: Option<Value<'gc>>,
        ip: Ip,
    },
    /// End the activation with `code`/`value` (through `scheme_exit`).
    Exit { code: ReturnCode, value: Value<'gc> },
    /// Continue at the machine address `addr` in compiled code, with `fp`,
    /// count `ac` and, for a single value, `rv`: a return to a compiled
    /// return point.
    Jump {
        fp: Slot<'gc>,
        addr: crate::runtime::sabi::Ra,
        ac: u64,
        rv: Option<Value<'gc>>,
    },
}

/// A call site's `CallLinkInfo` plus its code buffer (for the barrier).
pub(crate) struct EnterLink<'gc> {
    pub(crate) link: &'static CallLinkInfo,
    pub(crate) buffer: Value<'gc>,
}

/// One step of the control loop. `Enter` carries a link only on its first
/// iteration (from a `call`/`tail-call` instruction), everything the loop
/// reaches afterwards enters link-free.
enum Work<'gc> {
    Enter {
        fp: Slot<'gc>,
        callee: Value<'gc>,
        argc: usize,
        site: Option<EnterLink<'gc>>,
    },
    RunNative {
        fp: Slot<'gc>,
        clo: Gc<'gc, Closure<'gc>>,
        argc: usize,
    },
    NativeContinue,
    Return {
        fp: Slot<'gc>,
        ra: crate::runtime::sabi::Ra,
        count: u64,
    },
    /// The condition is staged in `State::exit_value`.
    RaiseInFrame {
        fp: Slot<'gc>,
    },
    RaiseOutOfFrame,
}

/// Drive `work` to completion: entering, returning, raising and running
/// natives until bytecode resumes or the activation exits. This loop is the
/// shared body behind `call_closure!`/`return_values!`/`finish_native!` and
/// the cold handlers `enter_overflow`, `enter_arity`,
/// `call_slow`, `call_non_closure`, `record_callee`, `call_native`,
/// `native_continue`, `raise_in_frame`, `raise_out_of_frame`, `raise_kind`,
/// `wrong_value_count`, `do_return` and `do_return_stub`.
fn drive<'gc>(ctx: MutationContext<'gc>, mut work: Work<'gc>) -> Control<'gc> {
    loop {
        work = match work {
            Work::Enter {
                fp,
                callee,
                argc,
                site,
            } => match enter_step(ctx, fp, callee, argc, site) {
                Ok(work) => work,
                Err(done) => return done,
            },
            Work::RunNative { fp, clo, argc } => match run_native(ctx, fp, clo, argc) {
                Ok(work) => work,
                Err(done) => return done,
            },
            Work::NativeContinue => unsafe { native_continue_step(ctx) },
            Work::Return { fp, ra, count } => match unsafe { return_step(ctx, fp, ra, count) } {
                Ok(work) => work,
                Err(done) => return done,
            },
            Work::RaiseInFrame { fp } => unsafe { raise_in_frame_step(ctx, fp) },
            Work::RaiseOutOfFrame => unsafe { raise_out_of_frame_step(ctx) },
        };
    }
}

/// Enter `callee` with `argc` arguments already in `fp[2..]` and the callee
/// in `fp[1]`. Records the call-site link when `site` is present.
fn enter_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    callee: Value<'gc>,
    argc: usize,
    site: Option<EnterLink<'gc>>,
) -> Result<Work<'gc>, Control<'gc>> {
    unsafe {
        if callee.is::<Closure>() {
            let clo = Closure::from_value(callee);
            let block = Closure::code_block(clo);
            // Natives link like bytecode: their machine entry is `subr_call`,
            // which the call template reaches through `closure.code`.
            if let Some(site) = site
                && site.link.callee.bits() != Value::from_gc(block).bits()
                && site.link.polymorphic.load(Relaxed) == 0
            {
                record_link(ctx, site.link, site.buffer, Value::from_gc(block));
            }
            if block.is_native() {
                return run_native(ctx, fp, clo, argc);
            }
            let ip = block.entrypoint.to_ptr::<u8>();
            return Err(Control::Resume {
                fp,
                cp: callee,
                ac: argc as u64,
                rv: None,
                ip,
            });
        }
        Ok(call_non_closure_step(ctx, fp, callee, argc, site))
    }
}

/// Enter a native closure. `fp[0]` is the native's return address and
/// `fp[1]` the closure, the arguments sit above at `fp[2..]`. The native
/// stages a follow-up call (`Continue`), raises out of frame (`Raise`, with
/// the condition already in `exit_value`), exits the activation with the
/// error at `fp[2]` (`ReturnErr`), or returns the value at `fp[2]` to its
/// caller (`ReturnOne`).
///
/// There is no `catch_unwind` here: natives are `extern "C"` and report
/// their own panics through `Call::invoke`.
fn run_native<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    clo: Gc<'gc, Closure<'gc>>,
    argc: usize,
) -> Result<Work<'gc>, Control<'gc>> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2 + argc));
        // Published for `Call::from_sfp`, which counts the frame's arguments
        // from it, the machine stub spills the same slot, and publishes
        // `sp` above like here (staging writes at `sp`).
        ctx.tc().ac.set(argc as u64);
        let native = Closure::free_get(clo, 0).downcast_unchecked::<NativeProc>();
        let proc: NativeFn<'gc> = std::mem::transmute(native.proc.to_ptr::<()>());
        match proc(ctx, fp) {
            ReturnCode::ReturnOne => Ok(Work::Return {
                fp,
                ra: stack::return_address(fp),
                count: 1,
            }),
            ReturnCode::Continue => Ok(Work::NativeContinue),
            // The condition is already in `exit_value` (see `Call::invoke`).
            ReturnCode::Raise => Ok(Work::RaiseOutOfFrame),
            ReturnCode::ReturnErr => Err(Control::Exit {
                code: ReturnCode::ReturnErr,
                value: *fp.add(2),
            }),
            ReturnCode::EnterBody => Ok(enter_body_work(ctx)),
            ReturnCode::ReturnOk => unreachable!("natives never return ReturnOk"),
            ReturnCode::ResumeTemplate => unreachable!("ResumeTemplate never escapes the bail"),
        }
    }
}

/// The native rewrote its frame into a call (`ReturnCode::EnterBody`): enter
/// the closure it left at `fp[1]` with `Tc::ac` arguments.
unsafe fn enter_body_work<'gc>(ctx: MutationContext<'gc>) -> Work<'gc> {
    unsafe {
        let fp = ctx.tc().stack.fp.get();
        Work::Enter {
            fp,
            callee: *fp.add(1),
            argc: ctx.tc().ac.get() as usize,
            site: None,
        }
    }
}

/// A native procedure asked for a call (`ReturnCode::Continue`). The request
/// is staged in the stack state, `fp` there is the native's frame, which may
/// have moved to a new segment while the arguments were staged.
unsafe fn native_continue_step<'gc>(ctx: MutationContext<'gc>) -> Work<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        let fp = stack.fp.get();
        let rator = stack.call_rator.get();
        let k = stack.call_k.get();
        let argc = stack.call_argc.get();
        let args = stack.call_args.get();
        stack.call_rator.set(Value::undefined());
        stack.call_k.set(Value::undefined());

        let k_is_current = k.raw_i64() == Value::VALUE_UNDEFINED || k.is_current_k();
        if k.raw_i64() == Value::VALUE_UNDEFINED {
            // A continuation is being invoked with values.
            if rator.is_current_k() {
                std::ptr::copy(args, fp.add(2), argc);
                return Work::Return {
                    fp,
                    ra: stack::return_address(fp),
                    count: argc as u64,
                };
            }
            if rator.is::<stack::Continuation>() {
                // Throw to a captured continuation: abandon the frames above
                // the base and continue it (`nuate`).
                let Some((fp, ra)) = stack::nuate(ctx, rator.downcast(), args, argc) else {
                    return shot_continuation(ctx, fp);
                };
                return Work::Return {
                    fp,
                    ra,
                    count: argc as u64,
                };
            }
        }
        if k_is_current {
            // Tail call from the native's frame.
            *fp.add(1) = rator;
            std::ptr::copy(args, fp.add(2), argc);
            return Work::Enter {
                fp,
                callee: rator,
                argc,
                site: None,
            };
        }
        // Call with a native continuation: the native's frame becomes the
        // continuation's callee_frame, and the callee returns to it through `subr_ret`.
        debug_assert!(
            k.is::<Closure>(),
            "return continuation is not a continuation: {k}"
        );
        *fp.add(1) = k;
        let callee_frame = fp.add(2);
        stack::set_return_address(callee_frame, markers::subr_ret());
        *callee_frame.add(1) = rator;
        std::ptr::copy(args, callee_frame.add(2), argc);
        Work::Enter {
            fp: callee_frame,
            callee: rator,
            argc,
            site: None,
        }
    }
}

/// The callee is not a closure: apply a generic function, or raise. `site`
/// is `None` at activation entry, where there is no link to probe or fill.
unsafe fn call_non_closure_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    callee: Value<'gc>,
    argc: usize,
    site: Option<EnterLink<'gc>>,
) -> Work<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2 + argc));
        if callee.is::<GenericDescriptor>() {
            let generic = callee.downcast::<GenericDescriptor>();
            let link = site.as_ref().map(|site| site.link);
            // The caller's arguments stay on the stack and are read through a
            // slice: the stack never moves except in `reserve`, after which
            // `fp` is reloaded, so no copy is needed.
            if let Some(link) = link
                && let Some(hit) = check_generic_cache(link, callee, fp, argc)
            {
                let body = hit.methods[0].body();
                if body.is::<Closure>() {
                    // Reserve first: nothing is held across it, and everything
                    // below only reads the reloaded frame. The shifted frame is
                    // written directly, no intermediate array is needed.
                    stack::reserve(ctx, argc + 1 + 8);
                    let fp = stack.fp.get();
                    let frame_args = std::slice::from_raw_parts(fp.add(2), argc);
                    let args_array = Array::from_slice(ctx, frame_args);
                    let next = NextMethodDescriptor::new(ctx, generic, hit.methods, args_array, 0);
                    let argc = argc + 1;
                    *fp.add(1) = body;
                    // Shift right by one first (`copy` tolerates the overlap),
                    // then plant the next method: writing it first would
                    // destroy the argument being shifted.
                    std::ptr::copy(frame_args.as_ptr(), fp.add(3), argc - 1);
                    *fp.add(2) = next.into();
                    return Work::Enter {
                        fp,
                        callee: body,
                        argc,
                        site: None,
                    };
                }
            }
            // `GenericDescriptor::invocation` inlined: the miss path needs the
            // method chain to fill the cache, and a fallback call must not
            // fill it.
            let frame_args = std::slice::from_raw_parts(fp.add(2), argc);
            let condition = match GenericDescriptor::next_method_chain(ctx, generic, frame_args) {
                Ok(next) => {
                    let body = next.body();
                    if body.is::<Closure>() {
                        if let Some(site) = site.as_ref() {
                            fill_generic_cache(
                                ctx,
                                site.link,
                                site.buffer,
                                generic,
                                next.methods(),
                                frame_args,
                            );
                        }
                        stack::reserve(ctx, argc + 1 + 8);
                        let fp = stack.fp.get();
                        // Re-read past the reserve: the stack may have moved.
                        let frame_args = std::slice::from_raw_parts(fp.add(2), argc);
                        let argc = argc + 1;
                        *fp.add(1) = body;
                        // Shift before planting the next method (see above).
                        std::ptr::copy(frame_args.as_ptr(), fp.add(3), argc - 1);
                        *fp.add(2) = next.into();
                        return Work::Enter {
                            fp,
                            callee: body,
                            argc,
                            site: None,
                        };
                    }
                    thunks::non_applicable(ctx, body)
                }
                Err(GenericDispatchError::NoApplicableMethod) if !generic.fallback().is_empty() => {
                    let body = generic.fallback();
                    if body.is::<Closure>() {
                        stack::reserve(ctx, argc + 8);
                        let fp = stack.fp.get();
                        // The arguments are already in place, only the callee
                        // changes.
                        *fp.add(1) = body;
                        return Work::Enter {
                            fp,
                            callee: body,
                            argc,
                            site: None,
                        };
                    }
                    thunks::non_applicable(ctx, body)
                }
                Err(GenericDispatchError::Arity) => thunks::make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::new(ctx, "not enough dispatch arguments", true).into(),
                    &[callee],
                ),
                Err(GenericDispatchError::NoApplicableMethod) => thunks::make_assertion_violation(
                    ctx,
                    Value::new(false),
                    Str::new(ctx, "no applicable method", true).into(),
                    &[callee],
                ),
            };
            ctx.state().exit_value.set(condition);
            return Work::RaiseInFrame { fp };
        }
        let condition = thunks::non_applicable(ctx, callee);
        ctx.state().exit_value.set(condition);
        Work::RaiseInFrame { fp }
    }
}

/// Call the exception handler with the condition in `exit_value`, as a tail
/// call from the frame at `fp`: the handler returns to that frame's caller.
unsafe fn raise_in_frame_step<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>) -> Work<'gc> {
    unsafe {
        let condition = ctx.state().exit_value.replace(Value::undefined());
        let handler = thunks::exception_handler(ctx);
        *fp.add(1) = handler;
        *fp.add(2) = condition;
        Work::Enter {
            fp,
            callee: handler,
            argc: 1,
            site: None,
        }
    }
}

/// Call the exception handler with the condition in `exit_value` from a
/// native frame that asked for a raise. The handler's result ends the
/// activation.
unsafe fn raise_out_of_frame_step<'gc>(ctx: MutationContext<'gc>) -> Work<'gc> {
    unsafe {
        let condition = ctx.state().exit_value.replace(Value::undefined());
        let fp = push_raise_frame(ctx, condition);
        let handler = *fp.add(1);
        Work::Enter {
            fp,
            callee: handler,
            argc: 1,
            site: None,
        }
    }
}

/// The caller through a return point: the frame below `fp` by the return
/// point's `frame` operand. Every `return-point-forward` shares its `frame` with the
/// multiple-value entry after it.
unsafe fn caller_cp<'gc>(fp: Slot<'gc>, ip: Ip) -> Value<'gc> {
    unsafe {
        let (width, opcode, operands) = def::decode_header(ip);
        let frame_size = match opcode {
            OpcodeId::OpReturnPoint => {
                def::OpReturnPoint::decode_from_ptr(operands, width)
                    .0
                    .frame
                    .0 as usize
            }
            OpcodeId::OpReturnPointForward => {
                def::OpReturnPointForward::decode_from_ptr(operands, width)
                    .0
                    .frame
                    .0 as usize
            }
            OpcodeId::OpReturnPointValues => {
                def::OpReturnPointValues::decode_from_ptr(operands, width)
                    .0
                    .frame
                    .0 as usize
            }
            OpcodeId::OpReturnPointValuesRest => {
                def::OpReturnPointValuesRest::decode_from_ptr(operands, width)
                    .0
                    .frame
                    .0 as usize
            }
            other => unreachable!("resume address {ip:?} is not a return point: {other:?}"),
        };
        *fp.sub(frame_size).add(1)
    }
}

/// Return `count` values, already in `fp[2..]`, to the return address `ra`
/// (`fp[0]`, except after `underflow`, which supplies its own: an in-place
/// resume leaves `fp[0]` holding the `underflow` marker).
unsafe fn return_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ra: crate::runtime::sabi::Ra,
    count: u64,
) -> Result<Work<'gc>, Control<'gc>> {
    unsafe {
        if count == 1 && ra == markers::interpreter_return() {
            let ip = stack::resume_ip(fp);
            return Err(Control::Resume {
                fp,
                cp: caller_cp(fp, ip),
                ac: 1,
                rv: Some(*fp.add(2)),
                ip,
            });
        }
        if count != 1 && markers::mv_target(ra) == Some(markers::interpreter_return()) {
            // A multiple-values return to an interpreted caller: route through
            // the mv operand of the bytecode return point in `fp[-1]`.
            let ip = stack::resume_ip(fp);
            let (width, opcode, operands) = def::decode_header(ip);
            match opcode {
                OpcodeId::OpReturnPoint => {
                    let (op, _) = def::OpReturnPoint::decode_from_ptr(operands, width);
                    if op.mv.0 == MV_IGNORE {
                        return Err(Control::Resume {
                            fp,
                            cp: caller_cp(fp, ip),
                            ac: count,
                            rv: None,
                            ip,
                        });
                    }
                    let fp = fp.sub(op.frame.0 as usize);
                    return Ok(wrong_count_work(ctx, ip, fp, count));
                }
                OpcodeId::OpReturnPointForward => {
                    let target = ip.add(def::Instruction::size_at_ptr(ip));
                    return Err(Control::Resume {
                        fp,
                        cp: caller_cp(fp, target),
                        ac: count,
                        rv: None,
                        ip: target,
                    });
                }
                other => unreachable!("resume address {ip:?} is not a return point: {other:?}"),
            }
        }
        return_stub_step(ctx, fp, ra, count)
    }
}

/// The frames below were a one-shot continuation that has already been
/// resumed in place: raise from the frame at `fp`.
unsafe fn shot_continuation<'gc>(ctx: MutationContext<'gc>, fp: Slot<'gc>) -> Work<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2));
        ctx.state().exit_value.set(thunks::make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(ctx, "attempt to invoke shot one-shot continuation", true).into(),
            &[],
        ));
        Work::RaiseOutOfFrame
    }
}

/// Return `count` values, already in `fp[2..]`, to the stub return address
/// `ra`. A stub takes any count itself.
unsafe fn return_stub_step<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ra: crate::runtime::sabi::Ra,
    count: u64,
) -> Result<Work<'gc>, Control<'gc>> {
    let frame = CallFrame::create(fp);
    unsafe {
        if ra == markers::scheme_exit() || ra == markers::raise_ret() {
            return Err(Control::Exit {
                code: ReturnCode::ReturnOk,
                value: frame.exit_value(ctx, count as usize),
            });
        }
        if ra == markers::underflow() {
            let Some((fp, ra)) = stack::dounderflow(ctx, fp, count as usize) else {
                *fp.add(1) = Value::undefined();
                return Ok(shot_continuation(ctx, fp));
            };
            return Ok(Work::Return { fp, ra, count });
        }
        if ra == markers::subr_ret() {
            // The continuation's frame is the two slots below, compact the
            // values down onto it so the continuation runs on a genuine
            // frame: `[ra, k, v0..]`.
            let stack = &ctx.tc().stack;
            let cont_frame = fp.sub(2);
            let k = *cont_frame.add(1);
            // The returning frame's header lies inside what is now reported as the
            // continuation's live slots, its return address must not look like a
            // value.
            *fp = Value::undefined();
            // Overlapping move down (`memmove`): the source sits two slots
            // above the destination.
            std::ptr::copy(fp.add(2), cont_frame.add(2), count as usize);
            stack.fp.set(cont_frame);
            stack.sp.set(cont_frame.add(2 + count as usize));
            ctx.tc().ac.set(count as u64);
            let native =
                Closure::free_get(Closure::from_value(k), 0).downcast_unchecked::<NativeProc>();
            let proc: NativeFn<'gc> = std::mem::transmute(native.proc.to_ptr::<()>());
            // No `catch_unwind`: see `run_native`.
            return match proc(ctx, cont_frame) {
                ReturnCode::ReturnOne => Ok(Work::Return {
                    fp: cont_frame,
                    ra: stack::return_address(cont_frame),
                    count: 1,
                }),
                ReturnCode::Continue => Ok(Work::NativeContinue),
                ReturnCode::Raise => Ok(Work::RaiseOutOfFrame),
                ReturnCode::ReturnErr => Err(Control::Exit {
                    code: ReturnCode::ReturnErr,
                    value: *cont_frame.add(2),
                }),
                ReturnCode::EnterBody => Ok(enter_body_work(ctx)),
                ReturnCode::ReturnOk => unreachable!("natives never return ReturnOk"),
                ReturnCode::ResumeTemplate => unreachable!("ResumeTemplate never escapes the bail"),
            };
        }
        // A return point in compiled code: its header names the code that
        // owns it. Any count but one follows `%mv-jump`.
        assert!(
            crate::runtime::sabi::entry_offset(ra) != 0,
            "return to unknown stub {ra:?}"
        );
        match if count == 1 {
            Some(ra)
        } else {
            markers::mv_target(ra)
        } {
            Some(addr) => Err(Control::Jump {
                fp,
                addr,
                ac: count,
                rv: (count == 1).then(|| *fp.add(2)),
            }),
            None => Err(control_values_error(ctx, fp, count)),
        }
    }
}

/// Stage a wrong-value-count condition for the return point at `ip` and
/// raise it from the already-popped frame `fp`.
unsafe fn wrong_count_work<'gc>(
    ctx: MutationContext<'gc>,
    ip: Ip,
    fp: Slot<'gc>,
    count: u64,
) -> Work<'gc> {
    unsafe {
        park_frame(ctx, fp);
        let (width, opcode, operands) = def::decode_header(ip);
        let message = match opcode {
            OpcodeId::OpReturnPoint => {
                format!("returned {count} values to single value return context")
            }
            OpcodeId::OpReturnPointValuesRest => {
                let (op, _) = def::OpReturnPointValuesRest::decode_from_ptr(operands, width);
                format!("expected at least {} values, got {count}", op.nvals.0)
            }
            OpcodeId::OpReturnPointValues => {
                let (op, _) = def::OpReturnPointValues::decode_from_ptr(operands, width);
                format!("expected {} value(s), got {count}", op.nvals.0)
            }
            _ => format!("expected 1 value(s), got {count}"),
        };
        let condition = thunks::make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(ctx, &message, true).into(),
            &[],
        );
        ctx.state().exit_value.set(condition);
        Work::RaiseInFrame { fp }
    }
}

/// `count` values were returned from the frame at `fp` to a return point
/// that takes exactly one (`values_error`): raise there.
pub(crate) unsafe fn control_values_error<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    count: u64,
) -> Control<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2 + count as usize));
        ctx.state().exit_value.set(thunks::make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(
                ctx,
                &format!("returned {count} values to single value return context"),
                true,
            )
            .into(),
            &[],
        ));
        drive(ctx, Work::RaiseOutOfFrame)
    }
}

/// Enter `callee` with `argc` arguments in `fp[2..]`. `site` links a
/// `call`/`tail-call` instruction, enters from anywhere else pass `None`.
pub(crate) fn control_enter<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    callee: Value<'gc>,
    argc: usize,
    site: Option<EnterLink<'gc>>,
) -> Control<'gc> {
    drive(
        ctx,
        Work::Enter {
            fp,
            callee,
            argc,
            site,
        },
    )
}

/// Run the native closure `clo` with `argc` arguments in `fp[2..]`.
pub(crate) fn control_run_native<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    clo: Gc<'gc, Closure<'gc>>,
    argc: usize,
) -> Control<'gc> {
    drive(ctx, Work::RunNative { fp, clo, argc })
}

/// Continue a native's staged `Continue` request.
pub(crate) fn control_native_continue<'gc>(ctx: MutationContext<'gc>) -> Control<'gc> {
    drive(ctx, Work::NativeContinue)
}

/// Return `count` values in `fp[2..]` to `ra`.
pub(crate) fn control_return<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ra: crate::runtime::sabi::Ra,
    count: u64,
) -> Control<'gc> {
    drive(ctx, Work::Return { fp, ra, count })
}

/// Raise the condition in `State::exit_value` from the frame at `fp`.
pub(crate) fn control_raise_in_frame<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
) -> Control<'gc> {
    drive(ctx, Work::RaiseInFrame { fp })
}

/// Raise the condition in `State::exit_value` above the parked frame, in a
/// fresh handler frame. The handler's result ends the activation.
pub(crate) fn control_raise_out_of_frame<'gc>(ctx: MutationContext<'gc>) -> Control<'gc> {
    drive(ctx, Work::RaiseOutOfFrame)
}

/// Raise condition kind `kind` with the irritant in `State::exit_value`, at
/// the instruction `ip`, whose source location goes into the condition.
pub(crate) unsafe fn control_raise_kind<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    clo: Option<Gc<'gc, Closure<'gc>>>,
    ip: Ip,
    kind: usize,
) -> Control<'gc> {
    unsafe {
        park_frame(ctx, fp);
        let irritant = ctx.state().exit_value.replace(Value::undefined());
        let source = source_at(clo, ip);
        let condition = make_raise_condition_with_source(ctx, kind, &[irritant], source);
        ctx.state().exit_value.set(condition);
        drive(ctx, Work::RaiseInFrame { fp })
    }
}

/// Raise the `raise` at `ip`, with its values in `fp[2..]`.
pub(crate) unsafe fn control_raise<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    clo: Option<Gc<'gc, Closure<'gc>>>,
    ip: Ip,
) -> Control<'gc> {
    unsafe {
        park_frame(ctx, fp);
        let (width, opcode, operands) = def::decode_header(ip);
        debug_assert_eq!(opcode, OpcodeId::OpRaise);
        let (op, _) = def::OpRaise::decode_from_ptr(operands, width);
        let values = std::slice::from_raw_parts(fp.add(2), op.count.0 as usize).to_vec();
        let source = source_at(clo, ip);
        let condition = make_raise_condition_with_source(ctx, op.kind.0 as usize, &values, source);
        ctx.state().exit_value.set(condition);
        drive(ctx, Work::RaiseInFrame { fp })
    }
}

/// Raise a wrong-value-count error for the return point at `ip` from the
/// already-popped frame `fp`.
pub(crate) unsafe fn control_wrong_count<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ip: Ip,
    count: u64,
) -> Control<'gc> {
    unsafe { drive(ctx, wrong_count_work(ctx, ip, fp, count)) }
}

/// The `enter` yield path: a cross-thread `esp = 0` poke landed here via
/// the overflow check. Restore the real limit, park the entry frame,
/// yield, and enter the callee again from its frame slot (it may have
/// moved).
pub(crate) unsafe fn control_enter_yield<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    argc: usize,
) -> Control<'gc> {
    unsafe {
        let tc = ctx.tc();
        tc.esp.store(tc.real_esp.get(), Relaxed);
        let stack = &tc.stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2 + argc));
        yield_body(ctx, false);
        // A compile finished while parked: install so the re-entry heals.
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            install(ctx, Closure::code_block(Closure::from_value(*fp.add(1))));
        }
        let callee = *fp.add(1);
        drive(
            ctx,
            Work::Enter {
                fp,
                callee,
                argc,
                site: None,
            },
        )
    }
}

/// The `enter` hot path: the block fired its heat threshold. Park the
/// entry frame, ask the broker to tier it up, and continue after the
/// `enter`: arity and overflow were already validated, so this never
/// re-enters.
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub(crate) unsafe fn control_enter_hot<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ac: u64,
    ip: Ip,
) -> Control<'gc> {
    unsafe {
        let argc = ac as usize;
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2 + argc));
        // The block is read from its frame slot: it may have moved.
        let code_block = Closure::code_block(Closure::from_value(*fp.add(1)));
        request_tier_up(ctx, code_block, &ThresholdPolicy::current());
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        Control::Resume {
            fp,
            cp: *fp.add(1),
            ac,
            rv: None,
            ip: next_ip,
        }
    }
}

/// The `loop_hint` yield path: park the frame under its mask, yield, and
/// resume after the hint.
pub(crate) unsafe fn control_loop_yield<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ac: u64,
    ip: Ip,
) -> Control<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2));
        // The live registers of this frame are the ones in the mask of the
        // instruction the frame is parked at.
        stack.top_ip.set(ip);
        yield_body(ctx, true);
        // A compile finished while parked: install so the resume below
        // can transfer into it.
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            install(ctx, Closure::code_block(Closure::from_value(*fp.add(1))));
        }
        stack.top_ip.set(std::ptr::null());
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        Control::Resume {
            fp,
            cp: *fp.add(1),
            ac,
            rv: None,
            ip: next_ip,
        }
    }
}

/// The `loop_hint` hot path: the block fired its heat threshold. Park the
/// frame under its mask, ask the broker to tier it up, and resume after
/// the hint.
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub(crate) unsafe fn control_loop_hot<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    ac: u64,
    ip: Ip,
) -> Control<'gc> {
    unsafe {
        let stack = &ctx.tc().stack;
        stack.fp.set(fp);
        stack.sp.set(fp.add(2));
        // The live registers of this frame are the ones in the mask of the
        // instruction the frame is parked at.
        stack.top_ip.set(ip);
        // The block is read from its frame slot: it may have moved.
        let code_block = Closure::code_block(Closure::from_value(*fp.add(1)));
        request_tier_up(ctx, code_block, &ThresholdPolicy::current());
        stack.top_ip.set(std::ptr::null());
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        Control::Resume {
            fp,
            cp: *fp.add(1),
            ac,
            rv: None,
            ip: next_ip,
        }
    }
}

/// The `enter` arity path: stage a wrong-arguments condition and raise it
/// from the callee frame.
pub(crate) unsafe fn control_enter_arity<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    clo: Gc<'gc, Closure<'gc>>,
    argc: usize,
) -> Control<'gc> {
    unsafe {
        park_frame(ctx, fp);
        let code_block = clo.code_block;
        let arity = code_block.arity;
        let rands = std::slice::from_raw_parts(fp.add(2), argc);
        let expected = if arity.is_variadic() {
            -(arity.fixed_arity() as isize)
        } else {
            arity.fixed_arity() as isize
        };
        let condition = thunks::wrong_number_of_args_impl(ctx, *fp.add(1), argc, expected, rands);
        ctx.state().exit_value.set(condition);
        drive(ctx, Work::RaiseInFrame { fp })
    }
}

/// The frame size an `enter`/`enter_rest` needs above `fp`, for the
/// overflow path.
unsafe fn enter_overflow_size(ip: Ip, argc: usize) -> usize {
    unsafe {
        let (width, opcode, operands) = def::decode_header(ip);
        match opcode {
            OpcodeId::OpEnter => {
                let (op, _) = def::OpEnter::decode_from_ptr(operands, width);
                op.frame.0 as usize
            }
            OpcodeId::OpEnterRest => {
                let (op, _) = def::OpEnterRest::decode_from_ptr(operands, width);
                op.frame.0 as usize + argc
            }
            _ => unreachable!("enter_overflow away from an entry"),
        }
    }
}

/// The `enter` overflow path: on an `esp = 0` poke from another thread take
/// the event path instead of growing the stack, else continue on a fresh
/// segment and enter again.
pub(crate) unsafe fn control_enter_overflow<'gc>(
    ctx: MutationContext<'gc>,
    fp: Slot<'gc>,
    argc: usize,
    ip: Ip,
) -> Control<'gc> {
    unsafe {
        let tc = ctx.tc();
        if tc.esp.load(Acquire).is_null() {
            // An `esp = 0` poke from another thread: yield instead
            // of growing the stack.
            return control_enter_yield(ctx, fp, argc);
        }
        let size = enter_overflow_size(ip, argc);
        tc.stack.fp.set(fp);
        tc.stack.sp.set(fp.add(2 + argc));
        let fp = stack::overflow(ctx, size + 8);
        let callee = *fp.add(1);
        drive(
            ctx,
            Work::Enter {
                fp,
                callee,
                argc,
                site: None,
            },
        )
    }
}
