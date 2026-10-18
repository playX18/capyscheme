//! The template interpreter's slow paths: the `capy_*` helpers.

use crate::bytecode::def::{self, OpcodeId};
use crate::heap::Gc;
use crate::heap::object::GcObject;
use crate::runtime::codegen::{gc, stubs};
use crate::runtime::interp::frame::CallFrame;
use crate::runtime::interp::markers;
use crate::runtime::interp::slow;
use crate::runtime::interp::stack::{self, Ip, Slot};
use crate::runtime::value::{Closure, CodeBuffer, ReturnCode, Value};
use crate::runtime::{MutationContext, Tc};
use crate::utils::flags;

type MutatorPointer = *const ();

unsafe fn stage_control(th: MutatorPointer, control: slow::Control<'static>) -> *const u8 {
    if flags::jit_stats() {
        super::jit::SLOW_CONTROL.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }

    unsafe { stage_control_with(th, control, None) }
}

/// Stage a [`slow::Control`] decision into the thread context and return the
/// machine address to continue at. A control without a value for `RV` leaves
/// it alone, or sets it to `default_rv` when that is provided
pub(super) unsafe fn stage_control_with(
    th: MutatorPointer,
    control: slow::Control<'static>,
    default_rv: Option<Value<'static>>,
) -> *const u8 {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        match control {
            slow::Control::Resume { fp, cp, ac, rv, ip } => {
                tc.stack.fp.set(fp);
                tc.cp.set(cp);
                tc.ac.set(ac);
                if let Some(rv) = rv.or(default_rv) {
                    tc.rv.set(rv);
                }
                tc.ip.set(ip);
                resume_addr(cp, ip)
            }
            slow::Control::Exit { code, value } => {
                tc.ac.set(code as usize as u64);
                tc.rv.set(value);
                tc.cp.set(Value::undefined());
                tc.ip.set(std::ptr::null());
                stubs::exit_glue_addr()
            }
            slow::Control::Jump { fp, addr, ac, rv } => {
                log::debug!(target: crate::heap::logging::JIT_LOG_TARGET, "jump: {addr:?} ac={ac}");

                tc.stack.fp.set(fp);
                tc.cp.set(*fp.add(1));
                tc.ac.set(ac);
                if let Some(rv) = rv.or(default_rv) {
                    tc.rv.set(rv);
                }
                addr
            }
        }
    }
}

unsafe fn resume_at(th: MutatorPointer, fp: Slot<'static>, ip: Ip) -> *const u8 {
    unsafe {
        if flags::jit_stats() {
            super::jit::SLOW_HELPER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        tc.cp.set(*fp.add(1));
        tc.ip.set(ip);
        resume_addr(*fp.add(1), ip)
    }
}

/// Where to resume the procedure `cp` at bytecode `ip`: its compiled code
/// when that has the instruction, else the interpreter.
pub(crate) fn resume_addr<'gc>(cp: Value<'gc>, ip: Ip) -> *const u8 {
    if flags::jit() && cp.is::<crate::runtime::value::Closure>() {
        let code_block = Closure::code_block(Closure::from_value(cp));
        if let Some(addr) = super::jit::resume(code_block, ip) {
            return addr;
        }
    }
    stubs::interpreter_resume_addr()
}

unsafe fn fault(th: MutatorPointer) -> (MutationContext<'static>, Slot<'static>, Ip, u64) {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        (mutator, tc.stack.fp.get(), tc.ip.get(), tc.ac.get())
    }
}

/// The `enter`/`enter_rest` hot path: the block fired its heat threshold.
pub(crate) unsafe extern "C" fn capy_hot_enter(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        stage_control(th, slow::control_enter_hot(mutator, fp, ac, ip))
    }
}

/// The `loop_hint` yield path.
pub(crate) unsafe extern "C" fn capy_yield_loop(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        stage_control(th, slow::control_loop_yield(mutator, fp, ac, ip))
    }
}

/// The `loop_hint` hot path: the block fired its heat threshold.
pub(crate) unsafe extern "C" fn capy_hot_loop(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        stage_control(th, slow::control_loop_hot(mutator, fp, ac, ip))
    }
}

/// The `enter`/`enter_rest` overflow path.
pub(crate) unsafe extern "C" fn capy_overflow(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        stage_control(
            th,
            slow::control_enter_overflow(mutator, fp, ac as usize, ip),
        )
    }
}

/// The `enter`/`enter_rest` arity path.
pub(crate) unsafe extern "C" fn capy_arity(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, _, ac) = fault(th);
        let clo = Closure::from_value(*fp.add(1));
        stage_control(th, slow::control_enter_arity(mutator, fp, clo, ac as usize))
    }
}

/// The `call`/`tail-call`/`call-self`/`tail-call-self` slow path: a link miss, a
/// native callee, or a non-closure callee. The template staged the metadata
/// area in `AC`, for `call`/`call-self` the callee frame is set up here
/// (the template has not moved `SFP` yet).
pub(crate) unsafe extern "C" fn capy_call_slow(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        let md = ac as usize as *mut u8;
        let (width, opcode, operands) = def::decode_header(ip);
        let next_ip = ip.add(def::Instruction::size_at_ptr(ip));
        let (frame, argc, link) = match opcode {
            OpcodeId::OpCall => {
                let (op, _) = def::OpCall::decode_from_ptr(operands, width);
                let frame = fp.add(op.base.0 as usize);
                stack::set_resume_ip(frame, next_ip);
                stack::set_return_address(frame, markers::interpreter_return());
                let link = &(*def::OpCall::metadata_ptr(md, op.metadata_id)).link;
                (frame, op.argc.0 as usize, link)
            }
            OpcodeId::OpTailCall => {
                let (op, _) = def::OpTailCall::decode_from_ptr(operands, width);
                let link = &(*def::OpTailCall::metadata_ptr(md, op.metadata_id)).link;
                (fp, op.argc.0 as usize, link)
            }
            OpcodeId::OpCallSelf => {
                let (op, _) = def::OpCallSelf::decode_from_ptr(operands, width);
                let frame = fp.add(op.base.0 as usize);
                // Idempotent: the template already wrote the same words.
                stack::set_resume_ip(frame, next_ip);
                stack::set_return_address(frame, markers::interpreter_return());
                let link = &(*def::OpCallSelf::metadata_ptr(md, op.metadata_id)).link;
                (frame, op.argc.0 as usize, link)
            }
            OpcodeId::OpTailCallSelf => {
                let (op, _) = def::OpTailCallSelf::decode_from_ptr(operands, width);
                let link = &(*def::OpTailCallSelf::metadata_ptr(md, op.metadata_id)).link;
                (fp, op.argc.0 as usize, link)
            }
            _ => unreachable!("capy_call_slow away from a call"),
        };
        let site = slow::EnterLink {
            link,
            buffer: Value::from_gc(CodeBuffer::md_buffer(md)),
        };
        let callee = *frame.add(1);
        stage_control(
            th,
            slow::control_enter(mutator, frame, callee, argc, Some(site)),
        )
    }
}

/// The return slow path: a stub return address at `ret`/`return-values`, a count
/// other than one, or a count mismatch at `return-point`/`return-point-values`/`return-point-values-rest`
pub(crate) unsafe extern "C" fn capy_return_mv(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, count) = fault(th);
        let (width, opcode, operands) = def::decode_header(ip);
        let _ = (width, operands);
        match opcode {
            OpcodeId::OpReturn | OpcodeId::OpReturnValues => {
                let ra = stack::return_address(fp);
                stage_control(th, slow::control_return(mutator, fp, ra, count))
            }
            OpcodeId::OpReturnPoint
            | OpcodeId::OpReturnPointValues
            | OpcodeId::OpReturnPointValuesRest => {
                stage_control(th, slow::control_wrong_count(mutator, fp, ip, count))
            }
            _ => unreachable!("capy_return_mv away from a return"),
        }
    }
}

/// A whole `raise`.
pub(crate) unsafe extern "C" fn capy_raise(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let clo = Closure::from_value(*fp.add(1));
        stage_control(th, slow::control_raise(mutator, fp, Some(clo), ip))
    }
}

/// An in-frame raise: the irritant is staged in `RV`, the kind code in `AC`.
pub(crate) unsafe extern "C" fn capy_raise_kind(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, kind) = fault(th);
        mutator.state().exit_value.set(mutator.tc().rv.get());
        let clo = Closure::from_value(*fp.add(1));
        stage_control(
            th,
            slow::control_raise_kind(mutator, fp, Some(clo), ip, kind as usize),
        )
    }
}

/// The slow path of `add`, `sub`, `mul`, `add-immediate`, `lt`, `le`, `numeq`, `ge`,
/// `gt`, `div`, `quotient`, `remainder`, `modulo`, `neg`, `abs`, `sqrt`,
/// and the fused `add-immediate-move`/`vector_ref_mul`.
pub(crate) unsafe extern "C" fn capy_number_arith(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let md = frame.code_block().buffer.metadata();
        let next = slow::arith_slow_body(mutator, fp, ip, md);
        resume_at(th, fp, next)
    }
}

/// The slow path of the numeric jumps.
pub(crate) unsafe extern "C" fn capy_compare_jump(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let md = frame.code_block().buffer.metadata();
        let target = slow::compare_slow_body(mutator, fp, ip, md);
        resume_at(th, fp, target)
    }
}

pub(crate) unsafe extern "C" fn capy_vector_op(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let md = frame.code_block().buffer.metadata();
        let next = slow::vector_slow_body(mutator, fp, ip, md);
        resume_at(th, fp, next)
    }
}

pub(crate) unsafe extern "C" fn capy_vector_length(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        match slow::vector_length_step(mutator, fp, ip) {
            Some(next) => resume_at(th, fp, next),
            None => stage_control(th, slow::control_raise_in_frame(mutator, fp)),
        }
    }
}

pub(crate) unsafe extern "C" fn capy_prim_n(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);

        match slow::prim_step(mutator, fp, ip) {
            Ok(next) => resume_at(th, fp, next),
            Err(kind) => {
                let clo = Closure::from_value(*fp.add(1));
                stage_control(
                    th,
                    slow::control_raise_kind(mutator, fp, Some(clo), ip, kind),
                )
            }
        }
    }
}

pub(crate) unsafe extern "C" fn capy_prim_error(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, kind) = fault(th);
        let clo = Closure::from_value(*fp.add(1));
        stage_control(
            th,
            slow::control_raise_kind(mutator, fp, Some(clo), ip, kind as usize),
        )
    }
}

pub(crate) unsafe extern "C" fn capy_op_alloc(th: MutatorPointer, header: u64, bytes: u64) -> u64 {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        if gc::alloc_stats() {
            gc::SLOW_ALLOCS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        let object = mutator.raw_allocate_with_header_word(
            bytes as usize - crate::runtime::sabi::OBJECT_HEADER_BYTES,
            8,
            header,
            mmtk::AllocationSemantics::Default,
        );
        object.to_address().as_usize() as u64
    }
}

pub(crate) unsafe extern "C" fn capy_op_rest_list(
    th: MutatorPointer,
    base: u64,
    count: u64,
    skip: u64,
) -> u64 {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let fp = mutator.tc().stack.fp.get();
        let (base, skip) = (base as usize, skip as usize);
        let count = Value::from_raw(count).as_int32().max(0) as usize;
        let frame = CallFrame::create(fp);
        slow::cons_range(mutator, frame, base + skip.min(count), base + count).bits()
    }
}

pub(crate) unsafe extern "C" fn capy_op_values_rest(
    th: MutatorPointer,
    base: u64,
    from: u64,
) -> u64 {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let fp = mutator.tc().stack.fp.get();
        let base = base as usize;
        let from = base + 2 + from as usize;
        let frame = CallFrame::create(fp);
        let count = (*frame.register(crate::bytecode::def::Register(base as u32)))
            .as_int32()
            .max(0) as usize;
        slow::cons_range(mutator, frame, from, from + count).bits()
    }
}

pub(crate) unsafe extern "C" fn capy_log_object(th: MutatorPointer, obj: u64) {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        // SAFETY: the stub's unlog-bit test guarantees a live heap object.
        let _ = Gc::write(
            mutator,
            Gc::<GcObject>::from_gc_object(Value::from_raw(obj).as_cell_raw()),
        );
    }
}

/// A whole `get_slot`: the inline-cache fast path, the miss path, or the
/// out-of-line `slow` target.
pub(crate) unsafe extern "C" fn capy_get_slot(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let (width, opcode, operands) = def::decode_header(ip);
        debug_assert_eq!(opcode, OpcodeId::OpGetSlot);
        let (op, _) = def::OpGetSlot::decode_from_ptr(operands, width);
        let md = frame.code_block().buffer.metadata();
        let meta = &*def::OpGetSlot::metadata_ptr(md, op.metadata_id);
        let obj = *frame.register(op.obj);
        if let Some(value) = slow::get_slot_fast(obj, meta) {
            meta.profile.observe(value);
            *frame.register(op.dst) = value;
        } else if let Some(value) = slow::get_slot_miss_body(mutator, frame, md, &op) {
            *frame.register(op.dst) = value;
        } else {
            return resume_at(th, fp, ip.offset(op.slow as isize));
        }
        let next = ip.add(def::Instruction::size_at_ptr(ip));
        resume_at(th, fp, next)
    }
}

/// A whole `put_slot`: like [`capy_get_slot`], storing instead of loading.
pub(crate) unsafe extern "C" fn capy_put_slot(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let (width, opcode, operands) = def::decode_header(ip);
        debug_assert_eq!(opcode, OpcodeId::OpPutSlot);
        let (op, _) = def::OpPutSlot::decode_from_ptr(operands, width);
        let md = frame.code_block().buffer.metadata();
        let meta = &*def::OpPutSlot::metadata_ptr(md, op.metadata_id);
        let obj = *frame.register(op.obj);
        let src = *frame.register(op.src);
        if !slow::put_slot_fast(mutator, obj, src, meta)
            && !slow::put_slot_miss_body(mutator, frame, md, &op)
        {
            return resume_at(th, fp, ip.offset(op.slow as isize));
        }
        let next = ip.add(def::Instruction::size_at_ptr(ip));
        resume_at(th, fp, next)
    }
}

/// A whole `runtime-procedure`.
pub(crate) unsafe extern "C" fn capy_runtime_proc(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, _) = fault(th);
        let frame = CallFrame::create(fp);
        let (width, opcode, operands) = def::decode_header(ip);
        debug_assert_eq!(opcode, OpcodeId::OpRuntimeProcedure);
        let (op, _) = def::OpRuntimeProcedure::decode_from_ptr(operands, width);
        let proc = crate::runtime::interp::runtime_procs::get(mutator, op.id.0);
        *frame.register(op.dst) = proc;
        let next = ip.add(def::Instruction::size_at_ptr(ip));
        resume_at(th, fp, next)
    }
}

/// A `nop`: native closures enter through their one-byte `nop` buffer, so a
/// `nop` with a native in `fp[1]` runs the native, anything else continues.
pub(crate) unsafe extern "C" fn capy_nop(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, ip, ac) = fault(th);
        let callee = *fp.add(1);
        if callee.is::<Closure>() {
            let clo = Closure::from_value(callee);
            if Closure::code_block(clo).is_native() {
                return stage_control(th, slow::control_run_native(mutator, fp, clo, ac as usize));
            }
        }
        let next = ip.add(def::Instruction::size_at_ptr(ip));
        resume_at(th, fp, next)
    }
}

/// What a stub saved: the frame, `AC` and `RV`.
unsafe fn stub_state(
    th: MutatorPointer,
) -> (MutationContext<'static>, Slot<'static>, u64, Value<'static>) {
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        (mutator, tc.stack.fp.get(), tc.ac.get(), tc.rv.get())
    }
}

/// A return of `AC` values to the stub return address `ra`, from the frame
/// in `SFP`. A single value arrives in `RV`, the shared core reads `fp[2]`.
unsafe fn stub_return(th: MutatorPointer, ra: crate::runtime::sabi::Ra) -> *const u8 {
    unsafe {
        let (mutator, fp, count, rv) = stub_state(th);
        if count == 1 {
            *fp.add(2) = rv;
        }
        stage_control(th, slow::control_return(mutator, fp, ra, count))
    }
}

/// `scheme_exit`: a return to the bottom of an activation.
pub(crate) unsafe extern "C" fn capy_exit(th: MutatorPointer) -> *const u8 {
    unsafe { stub_return(th, markers::scheme_exit()) }
}

/// `underflow`: a return into the continuation below the stack base.
pub(crate) unsafe extern "C" fn capy_underflow(th: MutatorPointer) -> *const u8 {
    unsafe { stub_return(th, markers::underflow()) }
}

/// `subr_ret`: a return to a native continuation.
pub(crate) unsafe extern "C" fn capy_subr_ret(th: MutatorPointer) -> *const u8 {
    unsafe { stub_return(th, markers::subr_ret()) }
}

/// `raise_ret`: a return from an exception handler run out of frame.
pub(crate) unsafe extern "C" fn capy_raise_ret(th: MutatorPointer) -> *const u8 {
    unsafe { stub_return(th, markers::raise_ret()) }
}

/// `subr_call` cold path: the machine entry called the native directly and
/// it came back with something other than `ReturnOne`. The frame in `Tc`'s
/// `fp` still holds the call, a single error value sits in place at
/// `fp[2]`, a `Raise` left its condition in `exit_value`.
pub(crate) unsafe extern "C" fn capy_subr_call(th: MutatorPointer, code: ReturnCode) -> *const u8 {
    unsafe {
        let (mutator, fp, _, _) = stub_state(th);
        match code {
            // Served inline by the glue, never reaches here.
            ReturnCode::ReturnOne => unreachable!("ReturnOne took the cold path"),
            ReturnCode::Continue => stage_control(th, slow::control_native_continue(mutator)),
            ReturnCode::Raise => stage_control(th, slow::control_raise_out_of_frame(mutator)),
            ReturnCode::ReturnErr => {
                let err = *fp.add(2);
                stage_control(
                    th,
                    slow::Control::Exit {
                        code: ReturnCode::ReturnErr,
                        value: err,
                    },
                )
            }
            ReturnCode::EnterBody => {
                let argc = mutator.tc().ac.get() as usize;
                let fp = mutator.tc().stack.fp.get();
                stage_control(th, slow::control_enter(mutator, fp, *fp.add(1), argc, None))
            }
            ReturnCode::ReturnOk => unreachable!("natives never return ReturnOk"),
            ReturnCode::ResumeTemplate => unreachable!("ResumeTemplate never escapes the bail"),
        }
    }
}

/// `values_error`: a return of `AC` values to a compact return point that
/// takes one.
pub(crate) unsafe extern "C" fn capy_values_error(th: MutatorPointer) -> *const u8 {
    unsafe {
        let (mutator, fp, count, _) = stub_state(th);
        stage_control(th, slow::control_values_error(mutator, fp, count))
    }
}
