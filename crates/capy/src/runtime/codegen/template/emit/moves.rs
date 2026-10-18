//! Moves, loads, and constants.

use onlineasm::onlineasm;

use crate::bytecode::def::{
    OpCellRef, OpClosureRef, OpEnvRef, OpFreeRef, OpLoadConstant, OpLoadImmediate, OpLoadRaw,
    OpMove, OpMoveTwo,
};
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{TemplateContext, operand_register, tmp};
use crate::runtime::sabi::{self, regs};

pub fn op_move<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpMove::OPERAND_SRC, tmp::T0);
    context.store_register(masm, OpMove::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_move_two<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpMoveTwo::OPERAND_SRC1, tmp::T0);
    context.store_register(masm, OpMoveTwo::OPERAND_DST1, tmp::T0, tmp::T1);
    context.load_register(masm, OpMoveTwo::OPERAND_SRC2, tmp::T0);
    context.store_register(masm, OpMoveTwo::OPERAND_DST2, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_load_constant<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.constant(masm, OpLoadConstant::OPERAND_CONSTANT, tmp::T0);
    context.store_register(masm, OpLoadConstant::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_load_immediate<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    operand_register(
        masm,
        context,
        OpLoadImmediate::OPERAND_VALUE,
        OpLoadImmediate::SIGNED_VALUE,
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpLoadImmediate::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_load_raw<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    operand_register(
        masm,
        context,
        OpLoadRaw::OPERAND_BITS,
        OpLoadRaw::SIGNED_BITS,
    );
    context.store_register(masm, OpLoadRaw::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_cell_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.constant(masm, OpCellRef::OPERAND_CONSTANT, tmp::T1);
    let car = masm.assembler().mem(tmp::T1, 0);
    onlineasm!(*masm.assembler();
        loadq {tmp::T1}, {car};
    );
    if context.profiling() {
        context.metadata(
            masm,
            OpCellRef::METADATA_KIND,
            OpCellRef::METADATA_PROFILE,
            tmp::T0,
            tmp::T3,
        );
    }
    masm.maybe_observe_value_profile(context.profiling(), tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    context.store_register(masm, OpCellRef::OPERAND_DST, tmp::T1, tmp::T0);
    context.next(masm);
}

pub fn op_closure_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_indexed_load(
        masm,
        context,
        OpClosureRef::OPERAND_CLOSURE,
        OpClosureRef::OPERAND_INDEX,
        OpClosureRef::SIGNED_INDEX,
        sabi::CLOSURE_FREE,
        OpClosureRef::OPERAND_DST,
    );
}

pub fn op_env_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_indexed_load(
        masm,
        context,
        OpEnvRef::OPERAND_ENV,
        OpEnvRef::OPERAND_INDEX,
        OpEnvRef::SIGNED_INDEX,
        sabi::ENV_DATA,
        OpEnvRef::OPERAND_DST,
    );
}

/// `dst = r[object][index]`, the slots starting at byte `offset`.
fn emit_indexed_load<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    object: usize,
    index: usize,
    index_signed: bool,
    offset: usize,
    dst: usize,
) {
    let (t0, t1) = (tmp::T0, tmp::T1);
    context.load_register(masm, object, t1);
    operand_register(masm, context, index, index_signed);
    let slot = masm.assembler().mem_indexed(t1, t0, 8, offset as i32);
    onlineasm!(*masm.assembler();
        loadq {t0}, {slot};
    );
    context.store_register(masm, dst, t0, t1);
    context.next(masm);
}

pub fn op_free_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (sfp, t0, t1) = (regs::SFP, tmp::T0, tmp::T1);
    let clo_at = masm.assembler().mem(sfp, 8);
    onlineasm!(*masm.assembler();
        loadq {t1}, {clo_at};
    );
    operand_register(
        masm,
        context,
        OpFreeRef::OPERAND_INDEX,
        OpFreeRef::SIGNED_INDEX,
    );
    let free_off = sabi::CLOSURE_FREE as i32;
    let slot = masm.assembler().mem_indexed(t1, t0, 8, free_off);
    onlineasm!(*masm.assembler();
        loadq {t0}, {slot};
    );
    context.store_register(masm, OpFreeRef::OPERAND_DST, t0, t1);
    context.next(masm);
}
