//! Calls and returns: interpreter<->interpreter fast paths, link checks, value routing.

use core::mem::offset_of;

use onlineasm::onlineasm;

use crate::bytecode::def::{
    MV_IGNORE, OpCall, OpCallSelf, OpReturn, OpReturnPoint, OpReturnPointValues,
    OpReturnPointValuesRest, OpReturnValues, OpTailCall, OpTailCallSelf,
};
use crate::runtime::codegen::masm::{Masm, RuntimeExtern};
use crate::runtime::codegen::stubs;
use crate::runtime::codegen::template::{
    Operand, SlowPath, TemplateContext, operand_register, operand_register_in, tmp,
};
use crate::runtime::interp::profile::{CallLinkInfo, LINK_MEGA};
use crate::runtime::sabi::{TC_AC, builtin_class_ids, regs};
use crate::runtime::value::CodeBlockKind;

const _: () = {
    assert!(CodeBlockKind::Bytecode as u8 == 0);
    assert!(CodeBlockKind::NativeProc as u8 == 1);
};

/// The call-site link check both call shapes share: `code_block` (in `block`)
/// against `link.callee`, then a megamorphic site's generic path, then the
/// site's polymorphic stub, then the polymorphic fallback. `T2` is scratch
/// below, the stub is entered with the link address in `T0`. Falls through
/// with `block` intact, jumps to `miss` when the miss path must record the
/// callee. Natives link like bytecode: their machine entry is `subr_call`,
/// which `enter_callee` reaches through `closure.code`. Without
/// [`link_checks`](TemplateContext::link_checks) the whole check goes and
/// the caller enters directly, recording nothing.
fn link_check<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    kind: usize,
    link_field: usize,
    block: onlineasm::VReg,
    miss: onlineasm::Label,
) {
    if !context.link_checks() {
        return;
    }
    let link_ok = masm.label();
    let has_stub = masm.label();
    context.metadata(masm, kind, link_field, tmp::T0, tmp::T2);
    let callee_at = masm
        .assembler()
        .mem(tmp::T0, offset_of!(CallLinkInfo, callee) as i32);
    let stub_at = masm
        .assembler()
        .mem(tmp::T0, offset_of!(CallLinkInfo, stub) as i32);
    let poly_at = masm
        .assembler()
        .mem(tmp::T0, offset_of!(CallLinkInfo, polymorphic) as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {callee_at};
        bqeq {tmp::T2}, {block}, {link_ok};
        loadi {tmp::T2}, {poly_at};
        bieq {tmp::T2}, {LINK_MEGA as i64}, {link_ok};
        loadq {tmp::T2}, {stub_at};
        bqneq {tmp::T2}, 0, {has_stub};
        loadi {tmp::T2}, {poly_at};
        bqneq {tmp::T2}, 0, {link_ok};
        jmp {miss};
    );
    masm.bind(has_stub);
    onlineasm!(*masm.assembler();
        jmp {tmp::T2};
    );
    masm.bind(link_ok);
}

/// The miss path of a compiled call: the site in `link`, the callee frame
/// in `frame`, `argc` in `AC`. Calls the `(th, frame, link)` miss path and
/// continues where it says. Terminal.
fn jit_miss(masm: &mut Masm, miss: RuntimeExtern, link: onlineasm::VReg, frame: onlineasm::VReg) {
    let tc = regs::TC;
    let ac_slot = masm.assembler().mem(tc, TC_AC as i32);
    let (a0, a1, a2) = stubs::arg_regs();
    let path = masm.declare_rust_fn(miss);
    // The link moves first: it shares its register with a C argument
    // register on both targets, and the frame follows before
    // `save_scheme_state` repoints `SFP` on x86-64.
    onlineasm!(*masm.assembler();
        storeq {ac_slot}, {regs::AC};
        move {a2}, {link};
        move {a1}, {frame};
        move {a0}, {tc};
    );
    masm.save_scheme_state();
    onlineasm!(*masm.assembler();
        call {path};
    );
    masm.restore_scheme_state();
    stubs::emit_control_tail(masm);
}

pub fn op_call<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    let (sfp, cp, ac) = (regs::SFP, regs::CP, regs::AC);

    let jit = match context.operand(masm, OpCall::OPERAND_BASE, OpCall::SIGNED_BASE) {
        Operand::Immediate(base) => {
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {base};
            );
            true
        }
        Operand::Register(r) => {
            if r != tmp::T0 {
                onlineasm!(*masm.assembler();
                    move {tmp::T0}, {r};
                );
            }
            false
        }
    };
    let frame = masm.assembler().mem_indexed(sfp, tmp::T0, 8, 0);
    onlineasm!(*masm.assembler();
        leap {tmp::T1}, {frame};
    );
    let callee_at = masm.assembler().mem(tmp::T1, 8);
    let block_at = masm.assembler().mem(tmp::T2, 8);
    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {callee_at};
    );
    operand_register_in(
        masm,
        context,
        OpCall::OPERAND_ARGC,
        OpCall::SIGNED_ARGC,
        tmp::T0,
    );
    onlineasm!(*masm.assembler();
        move {ac}, {tmp::T0};
    );
    context.call_return_point(masm, tmp::T1);
    masm.branch_not_heap_class(tmp::T2, builtin_class_ids::CLOSURE, tmp::T0, miss);
    onlineasm!(*masm.assembler();
        move {cp}, {tmp::T2};
    );
    if context.link_checks() {
        onlineasm!(*masm.assembler();
            loadq {tmp::T3}, {block_at};
        );
    }
    link_check(
        masm,
        context,
        OpCall::METADATA_KIND,
        OpCall::METADATA_LINK,
        tmp::T3,
        miss,
    );
    onlineasm!(*masm.assembler();
        move {sfp}, {tmp::T1};
    );
    context.enter_callee(masm);
    masm.bind(miss);
    if jit {
        context.metadata(
            masm,
            OpCall::METADATA_KIND,
            OpCall::METADATA_LINK,
            tmp::T0,
            tmp::T2,
        );
        jit_miss(masm, RuntimeExtern::IcMissCall, tmp::T0, tmp::T1);
    } else {
        context.control(masm, SlowPath::CallSlow);
    }
}

pub fn op_tail_call<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    let (sfp, cp, ac) = (regs::SFP, regs::CP, regs::AC);
    let callee_at = masm.assembler().mem(sfp, 8);
    let block_at = masm.assembler().mem(tmp::T2, 8);
    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {callee_at};
    );

    let jit = match context.operand(masm, OpTailCall::OPERAND_ARGC, OpTailCall::SIGNED_ARGC) {
        Operand::Immediate(argc) => {
            onlineasm!(*masm.assembler();
                move {ac}, {argc};
            );
            true
        }
        Operand::Register(r) => {
            onlineasm!(*masm.assembler();
                move {ac}, {r};
            );
            false
        }
    };
    masm.branch_not_heap_class(tmp::T2, builtin_class_ids::CLOSURE, tmp::T0, miss);
    onlineasm!(*masm.assembler();
        move {cp}, {tmp::T2};
    );
    if context.link_checks() {
        onlineasm!(*masm.assembler();
            loadq {tmp::T3}, {block_at};
        );
    }
    link_check(
        masm,
        context,
        OpTailCall::METADATA_KIND,
        OpTailCall::METADATA_LINK,
        tmp::T3,
        miss,
    );
    context.enter_callee(masm);
    masm.bind(miss);
    if jit {
        context.metadata(
            masm,
            OpTailCall::METADATA_KIND,
            OpTailCall::METADATA_LINK,
            tmp::T0,
            tmp::T2,
        );
        jit_miss(masm, RuntimeExtern::IcMissTcall, tmp::T0, sfp);
    } else {
        context.control(masm, SlowPath::CallSlow);
    }
}

pub fn op_call_self<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    let (sfp, cp, ac) = (regs::SFP, regs::CP, regs::AC);
    // The context probe doubles as the base operand (see `op_call`).
    let jit = match context.operand(masm, OpCallSelf::OPERAND_BASE, OpCallSelf::SIGNED_BASE) {
        Operand::Immediate(base) => {
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {base};
            );
            true
        }
        Operand::Register(r) => {
            if r != tmp::T0 {
                onlineasm!(*masm.assembler();
                    move {tmp::T0}, {r};
                );
            }
            false
        }
    };
    let frame = masm.assembler().mem_indexed(sfp, tmp::T0, 8, 0);
    let closure_at = masm.assembler().mem(sfp, 8);
    let callee_at = masm.assembler().mem(tmp::T1, 8);
    let block_at = masm.assembler().mem(tmp::T2, 8);
    onlineasm!(*masm.assembler();
        leap {tmp::T1}, {frame};
        loadq {tmp::T2}, {closure_at};
        storeq {callee_at}, {tmp::T2};
    );
    operand_register_in(
        masm,
        context,
        OpCallSelf::OPERAND_ARGC,
        OpCallSelf::SIGNED_ARGC,
        tmp::T0,
    );
    onlineasm!(*masm.assembler();
        move {ac}, {tmp::T0};
    );
    context.call_return_point(masm, tmp::T1);
    onlineasm!(*masm.assembler();
        move {cp}, {tmp::T2};
    );
    if context.link_checks() {
        onlineasm!(*masm.assembler();
            loadq {tmp::T3}, {block_at};
        );
    }
    link_check(
        masm,
        context,
        OpCallSelf::METADATA_KIND,
        OpCallSelf::METADATA_LINK,
        tmp::T3,
        miss,
    );
    onlineasm!(*masm.assembler();
        move {sfp}, {tmp::T1};
    );
    context.enter_self(masm);
    if jit || context.link_checks() {
        masm.bind(miss);
        if jit {
            context.metadata(
                masm,
                OpCallSelf::METADATA_KIND,
                OpCallSelf::METADATA_LINK,
                tmp::T0,
                tmp::T2,
            );
            jit_miss(masm, RuntimeExtern::IcMissCall, tmp::T0, tmp::T1);
        } else {
            context.control(masm, SlowPath::CallSlow);
        }
    }
}

pub fn op_tail_call_self<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let miss = masm.label();
    let (sfp, cp, ac) = (regs::SFP, regs::CP, regs::AC);
    let closure_at = masm.assembler().mem(sfp, 8);
    let block_at = masm.assembler().mem(tmp::T2, 8);
    onlineasm!(*masm.assembler();
        loadq {tmp::T2}, {closure_at};
    );

    let jit = match context.operand(
        masm,
        OpTailCallSelf::OPERAND_ARGC,
        OpTailCallSelf::SIGNED_ARGC,
    ) {
        Operand::Immediate(argc) => {
            onlineasm!(*masm.assembler();
                move {ac}, {argc};
            );
            true
        }
        Operand::Register(r) => {
            onlineasm!(*masm.assembler();
                move {ac}, {r};
            );
            false
        }
    };
    onlineasm!(*masm.assembler();
        move {cp}, {tmp::T2};
    );
    if context.link_checks() {
        onlineasm!(*masm.assembler();
            loadq {tmp::T3}, {block_at};
        );
    }
    link_check(
        masm,
        context,
        OpTailCallSelf::METADATA_KIND,
        OpTailCallSelf::METADATA_LINK,
        tmp::T3,
        miss,
    );
    context.enter_self(masm);
    if jit || context.link_checks() {
        masm.bind(miss);
        if jit {
            context.metadata(
                masm,
                OpTailCallSelf::METADATA_KIND,
                OpTailCallSelf::METADATA_LINK,
                tmp::T0,
                tmp::T2,
            );
            jit_miss(masm, RuntimeExtern::IcMissTcall, tmp::T0, sfp);
        } else {
            context.control(masm, SlowPath::CallSlow);
        }
    }
}

pub fn op_return<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpReturn::OPERAND_SRC, tmp::T0);
    let (sfp, rv, ac) = (regs::SFP, regs::RV, regs::AC);
    let slot2 = masm.assembler().mem(sfp, 16);
    onlineasm!(*masm.assembler();
        storeq {slot2}, {tmp::T0};
        move {rv}, {tmp::T0};
        move {ac}, 1;
    );
    context.resume_caller(masm, slow);
    // A stub return address: jump to it, with the value in `RV` and `AC` 1.
    masm.bind(slow);
    let ra = masm.assembler().mem(sfp, 0);
    onlineasm!(*masm.assembler(); jmp {ra};);
}

pub fn op_return_values<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let many = masm.label();
    let ac = regs::AC;
    operand_register(
        masm,
        context,
        OpReturnValues::OPERAND_COUNT,
        OpReturnValues::SIGNED_COUNT,
    );
    onlineasm!(*masm.assembler();
        move {ac}, {tmp::T0};
        bqneq {tmp::T0}, 1, {many};
    );
    let (sfp, rv) = (regs::SFP, regs::RV);
    let slot2 = masm.assembler().mem(sfp, 16);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {slot2};
        move {rv}, {tmp::T0};
    );
    let stub = masm.label();
    context.resume_caller(masm, stub);
    // One value to a stub return address: jump to it.
    masm.bind(stub);
    let ra = masm.assembler().mem(sfp, 0);
    onlineasm!(*masm.assembler(); jmp {ra};);
    masm.bind(many);
    // Multiple values: an interpreted caller resumes inline with the count
    // in `AC` (its return point validates it), a compiled one jumps to its
    // `%mv-jump` target. Only runtime-owned markers and values-error
    // headers still leave generated code.
    let slow = masm.label();
    context.resume_caller_mv(masm, slow);
    masm.bind(slow);
    context.control(masm, SlowPath::ReturnMv);
}

/// Pop `frame` slots: `SFP -= 8*frame`
fn pop_frame<C: TemplateContext>(masm: &mut Masm, context: &mut C, field: usize, signed: bool) {
    let sfp = regs::SFP;
    match context.operand(masm, field, signed) {
        Operand::Immediate(frame) => {
            onlineasm!(*masm.assembler();
                subp {sfp}, {sfp}, {frame * 8};
            );
        }
        Operand::Register(r) => {
            onlineasm!(*masm.assembler();
                lshiftq {r}, 3;
                subp {sfp}, {sfp}, {r};
            );
        }
    }
}

pub fn op_return_point<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    pop_frame(
        masm,
        context,
        OpReturnPoint::OPERAND_FRAME,
        OpReturnPoint::SIGNED_FRAME,
    );
    context.reload_code(masm);
    // A multiple-values return resumes here with a count other than 1.
    let many = masm.label();
    let ac = regs::AC;
    onlineasm!(*masm.assembler();
        bqneq {ac}, 1, {many};
    );
    context.store_register(masm, OpReturnPoint::OPERAND_DST, regs::RV, tmp::T0);
    if context.profiling() {
        context.metadata(
            masm,
            OpReturnPoint::METADATA_KIND,
            OpReturnPoint::METADATA_PROFILE,
            tmp::T0,
            tmp::T1,
        );
    }
    masm.maybe_observe_value_profile(context.profiling(), tmp::T0, regs::RV, tmp::T1, tmp::T2);
    let resume = masm.label();
    masm.bind(resume);
    context.next(masm);
    masm.bind(many);
    // Route on the `mv` operand.
    match context.operand(masm, OpReturnPoint::OPERAND_MV, OpReturnPoint::SIGNED_MV) {
        Operand::Immediate(mv) if mv as u32 == MV_IGNORE => {
            onlineasm!(*masm.assembler();
                jmp {resume};
            );
        }
        Operand::Immediate(_) => {
            let slow = masm.label();
            onlineasm!(*masm.assembler();
                jmp {slow};
            );
            context.slow_control(masm, slow, SlowPath::ReturnMv);
        }
        Operand::Register(r) => {
            let slow = masm.label();
            onlineasm!(*masm.assembler();
                bqneq {r}, {MV_IGNORE as i64}, {slow};
                jmp {resume};
            );
            context.slow_control(masm, slow, SlowPath::ReturnMv);
        }
    }
}

pub fn op_return_point_forward<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.next(masm);
}

pub fn op_return_point_values<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    let ac = regs::AC;
    pop_frame(
        masm,
        context,
        OpReturnPointValues::OPERAND_FRAME,
        OpReturnPointValues::SIGNED_FRAME,
    );
    operand_register(
        masm,
        context,
        OpReturnPointValues::OPERAND_NVALS,
        OpReturnPointValues::SIGNED_NVALS,
    );
    onlineasm!(*masm.assembler();
        bqneq {tmp::T0}, {ac}, {slow};
    );
    context.reload_code(masm);
    context.next(masm);
    context.slow_control(masm, slow, SlowPath::ReturnMv);
}

pub fn op_return_point_values_rest<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    // Like `op_return_point_values`, but `count >= nvals`, and the extra count is boxed
    // into `r[frame]` of the popped frame.
    let slow = masm.label();
    let (sfp, ac) = (regs::SFP, regs::AC);
    pop_frame(
        masm,
        context,
        OpReturnPointValuesRest::OPERAND_FRAME,
        OpReturnPointValuesRest::SIGNED_FRAME,
    );
    operand_register(
        masm,
        context,
        OpReturnPointValuesRest::OPERAND_NVALS,
        OpReturnPointValuesRest::SIGNED_NVALS,
    );
    onlineasm!(*masm.assembler();
        bqb {ac}, {tmp::T0}, {slow};
        subq {tmp::T1}, {ac}, {tmp::T0};
    );
    masm.box_int32(tmp::T1, tmp::T1);
    operand_register_in(
        masm,
        context,
        OpReturnPointValuesRest::OPERAND_FRAME,
        OpReturnPointValuesRest::SIGNED_FRAME,
        tmp::T0,
    );
    let extra_at = masm.assembler().mem_indexed(sfp, tmp::T0, 8, 0);
    onlineasm!(*masm.assembler();
        storeq {extra_at}, {tmp::T1};
    );
    context.reload_code(masm);
    context.next(masm);
    context.slow_control(masm, slow, SlowPath::ReturnMv);
}
