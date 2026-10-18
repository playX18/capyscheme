//! Entry: arity and overflow checks, rest moves, and the `nop`
//! native-entry check.

use onlineasm::onlineasm;

use crate::bytecode::def::{OpEnter, OpEnterRest};
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{
    SlowPath, TemplateContext, operand_register, operand_register_in, tmp,
};
use crate::runtime::sabi::regs;

pub fn op_nop<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    let may_be_native = context.nop_native_check(masm, slow);
    context.next(masm);
    if may_be_native {
        context.slow_control(masm, slow, SlowPath::Nop);
    }
}

pub fn op_enter<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (arity_slow, overflow_slow, hot_slow) = (masm.label(), masm.label(), masm.label());
    let argc = operand_register(masm, context, OpEnter::OPERAND_ARGC, OpEnter::SIGNED_ARGC);
    let ac = regs::AC;
    onlineasm!(*masm.assembler();
        bqneq {ac}, {argc}, {arity_slow};
    );
    let frame = operand_register(masm, context, OpEnter::OPERAND_FRAME, OpEnter::SIGNED_FRAME);
    masm.check_frame_limit(frame, tmp::T1, tmp::T2, overflow_slow);
    masm.maybe_hot_check(tmp::T1, tmp::T2, hot_slow);
    context.next(masm);
    context.slow_control(masm, arity_slow, SlowPath::Arity);
    context.slow_control(masm, overflow_slow, SlowPath::Overflow);
    if context.profiling() {
        context.slow_control(masm, hot_slow, SlowPath::HotEnter);
    }
}

pub fn op_enter_rest<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (arity_slow, overflow_slow, hot_slow) = (masm.label(), masm.label(), masm.label());
    let ac = regs::AC;
    operand_register_in(
        masm,
        context,
        OpEnterRest::OPERAND_REQUIRED,
        OpEnterRest::SIGNED_REQUIRED,
        tmp::T2,
    );
    onlineasm!(*masm.assembler();
        bqb {ac}, {tmp::T2}, {arity_slow};
        subp {tmp::T1}, {ac}, {tmp::T2};
    );
    let frame = operand_register(
        masm,
        context,
        OpEnterRest::OPERAND_FRAME,
        OpEnterRest::SIGNED_FRAME,
    );
    onlineasm!(*masm.assembler();
        addp {frame}, {frame}, {tmp::T1};
    );
    masm.check_frame_limit(frame, tmp::T3, tmp::T0, overflow_slow);
    let sfp = regs::SFP;
    let src = masm.assembler().mem_indexed(sfp, tmp::T2, 8, 16);
    onlineasm!(*masm.assembler();
        leap {tmp::T2}, {src};
    );
    operand_register_in(
        masm,
        context,
        OpEnterRest::OPERAND_BASE,
        OpEnterRest::SIGNED_BASE,
        tmp::T3,
    );
    let dst = masm.assembler().mem_indexed(sfp, tmp::T3, 8, 0);
    onlineasm!(*masm.assembler();
        leap {tmp::T3}, {dst};
    );
    let src_at = masm.assembler().mem(tmp::T2, 0);
    let dst_at = masm.assembler().mem(tmp::T3, 0);
    let (fwd, bwd, done) = (masm.label(), masm.label(), masm.label());
    let src_end = masm.assembler().mem_indexed(tmp::T2, tmp::T1, 8, 0);
    let dst_end = masm.assembler().mem_indexed(tmp::T3, tmp::T1, 8, 0);
    onlineasm!(*masm.assembler();
        bqbeq {tmp::T3}, {tmp::T2}, {fwd};
        leap {tmp::T2}, {src_end};
        leap {tmp::T3}, {dst_end};
    );
    masm.bind(bwd);
    onlineasm!(*masm.assembler();
        btqz {tmp::T1}, {done};
        subp {tmp::T2}, {tmp::T2}, 8;
        subp {tmp::T3}, {tmp::T3}, 8;
        loadq {tmp::T0}, {src_at};
        storeq {dst_at}, {tmp::T0};
        subq {tmp::T1}, {tmp::T1}, 1;
        jmp {bwd};
    );
    masm.bind(fwd);
    onlineasm!(*masm.assembler();
        btqz {tmp::T1}, {done};
        loadq {tmp::T0}, {src_at};
        storeq {dst_at}, {tmp::T0};
        addp {tmp::T2}, {tmp::T2}, 8;
        addp {tmp::T3}, {tmp::T3}, 8;
        subq {tmp::T1}, {tmp::T1}, 1;
        jmp {fwd};
    );
    masm.bind(done);
    // The moves are done, so `T1`/`T2` are free and `AC` still holds `argc`:
    // recompute the extras (the loop consumed `T1`) and store the count.
    operand_register_in(
        masm,
        context,
        OpEnterRest::OPERAND_CNT,
        OpEnterRest::SIGNED_CNT,
        tmp::T2,
    );
    let cnt = masm.assembler().mem_indexed(sfp, tmp::T2, 8, 0);
    onlineasm!(*masm.assembler();
        leap {tmp::T2}, {cnt};
    );
    operand_register_in(
        masm,
        context,
        OpEnterRest::OPERAND_REQUIRED,
        OpEnterRest::SIGNED_REQUIRED,
        tmp::T0,
    );
    onlineasm!(*masm.assembler();
        subp {tmp::T0}, {ac}, {tmp::T0};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    let cnt_at = masm.assembler().mem(tmp::T2, 0);
    onlineasm!(*masm.assembler();
        storeq {cnt_at}, {tmp::T0};
    );
    // The hot bump runs after the moves: the hot path resumes forward
    // past the whole instruction, so the moves must already be done.
    masm.maybe_hot_check(tmp::T0, tmp::T3, hot_slow);
    context.next(masm);
    context.slow_control(masm, arity_slow, SlowPath::Arity);
    context.slow_control(masm, overflow_slow, SlowPath::Overflow);
    if context.profiling() {
        context.slow_control(masm, hot_slow, SlowPath::HotEnter);
    }
}
