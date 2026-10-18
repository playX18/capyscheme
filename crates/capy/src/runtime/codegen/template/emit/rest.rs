//! Rest arguments: indexing, counts, and shape tests.

use onlineasm::onlineasm;

use crate::bytecode::def::{OpRestLen, OpRestNull, OpRestPair, OpRestRef};
use crate::runtime::codegen::masm::{Cmp, Masm};
use crate::runtime::codegen::template::{
    SlowPath, TemplateContext, operand_register, operand_register_in, tmp,
};
use crate::runtime::sabi::regs;
use crate::runtime::value::Value;
use crate::runtime::vm::exceptions::RaiseKind;

pub fn op_rest_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpRestRef::OPERAND_CNT, tmp::T1);
    masm.unbox_int32(tmp::T1, tmp::T1);
    // The base first: every operand decode lands in `T0`, so the index must
    // be decoded last to survive in `T0`.
    operand_register_in(
        masm,
        context,
        OpRestRef::OPERAND_BASE,
        OpRestRef::SIGNED_BASE,
        tmp::T2,
    );
    operand_register(
        masm,
        context,
        OpRestRef::OPERAND_INDEX,
        OpRestRef::SIGNED_INDEX,
    );
    onlineasm!(*masm.assembler();
        bqgteq {tmp::T0}, {tmp::T1}, {slow};
    );
    onlineasm!(*masm.assembler();
        addp {tmp::T2}, {tmp::T2}, {tmp::T0};
    );
    let slot = masm.assembler().mem_indexed(regs::SFP, tmp::T2, 8, 0);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {slot};
    );
    context.store_register(masm, OpRestRef::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    masm.bind(slow);
    onlineasm!(*masm.assembler();
        move {tmp::T1}, {Value::VALUE_NULL};
    );
    context.control(
        masm,
        SlowPath::RaiseKind(tmp::T1, RaiseKind::CarNotPair.code() as u8),
    );
}

pub fn op_rest_len<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpRestLen::OPERAND_CNT, tmp::T1);
    masm.unbox_int32(tmp::T1, tmp::T1);
    operand_register(
        masm,
        context,
        OpRestLen::OPERAND_SKIP,
        OpRestLen::SIGNED_SKIP,
    );
    onlineasm!(*masm.assembler();
        subq {tmp::T0}, {tmp::T1}, {tmp::T0};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpRestLen::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_rest_null<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpRestNull::OPERAND_CNT, tmp::T1);
    masm.unbox_int32(tmp::T1, tmp::T1);
    operand_register(
        masm,
        context,
        OpRestNull::OPERAND_SKIP,
        OpRestNull::SIGNED_SKIP,
    );
    masm.fixnum_cmp_bool(tmp::T0, tmp::T1, tmp::T0, Cmp::Eq);
    context.store_register(masm, OpRestNull::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_rest_pair<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpRestPair::OPERAND_CNT, tmp::T1);
    masm.unbox_int32(tmp::T1, tmp::T1);
    operand_register(
        masm,
        context,
        OpRestPair::OPERAND_SKIP,
        OpRestPair::SIGNED_SKIP,
    );
    masm.fixnum_cmp_bool(tmp::T0, tmp::T1, tmp::T0, Cmp::Gt);
    context.store_register(masm, OpRestPair::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}
