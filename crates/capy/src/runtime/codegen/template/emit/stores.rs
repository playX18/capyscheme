//! Stores: cells, variables, pairs, closures, environments, vectors.

use onlineasm::onlineasm;

use crate::bytecode::def::{
    OpCellSet, OpClosureSet, OpEnvSet, OpSetCar, OpSetCdr, OpVariableSet, OpVectorSet,
};
use crate::runtime::codegen::gc::GcCodegen;
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{SlowPath, TemplateContext, operand_register, tmp};
use crate::runtime::interp::profile::ARRAY_FAST;
use crate::runtime::sabi::{self, builtin_class_ids};

pub fn op_cell_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    context.constant(masm, OpCellSet::OPERAND_CONSTANT, tmp::T1);
    context.load_register(masm, OpCellSet::OPERAND_SRC, tmp::T2);
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    let slot = masm.assembler().mem(tmp::T1, 0);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T2};
    );
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
}

pub fn op_variable_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    context.load_register(masm, OpVariableSet::OPERAND_VAR, tmp::T1);
    context.load_register(masm, OpVariableSet::OPERAND_SRC, tmp::T2);
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    let slot = masm.assembler().mem(tmp::T1, 0);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T2};
    );
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
}

pub fn op_set_car<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    context.load_register(masm, OpSetCar::OPERAND_PAIR, tmp::T1);
    context.load_register(masm, OpSetCar::OPERAND_SRC, tmp::T2);
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    let slot = masm.assembler().mem(tmp::T1, sabi::PAIR_CAR as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T2};
    );
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
}

pub fn op_set_cdr<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    context.load_register(masm, OpSetCdr::OPERAND_PAIR, tmp::T1);
    context.load_register(masm, OpSetCdr::OPERAND_SRC, tmp::T2);
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    let slot = masm.assembler().mem(tmp::T1, sabi::PAIR_CDR as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T2};
    );
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
}

pub fn op_closure_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_raw_indexed_store(
        masm,
        context,
        OpClosureSet::OPERAND_CLOSURE,
        OpClosureSet::OPERAND_INDEX,
        OpClosureSet::SIGNED_INDEX,
        sabi::CLOSURE_FREE,
        OpClosureSet::OPERAND_SRC,
    );
}

pub fn op_env_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_raw_indexed_store(
        masm,
        context,
        OpEnvSet::OPERAND_ENV,
        OpEnvSet::OPERAND_INDEX,
        OpEnvSet::SIGNED_INDEX,
        sabi::ENV_DATA,
        OpEnvSet::OPERAND_SRC,
    );
}

/// `r[object][index] = src` without a barrier, the slots starting at byte
/// `offset`.
fn emit_raw_indexed_store<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    object: usize,
    index: usize,
    index_signed: bool,
    offset: usize,
    src: usize,
) {
    let (t0, t1, t2) = (tmp::T0, tmp::T1, tmp::T2);
    context.load_register(masm, object, t1);
    operand_register(masm, context, index, index_signed);
    context.load_register(masm, src, t2);
    let slot = masm.assembler().mem_indexed(t1, t0, 8, offset as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {t2};
    );
    context.next(masm);
}

pub fn op_vector_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpVectorSet::OPERAND_VECTOR, tmp::T1);
    context.load_register(masm, OpVectorSet::OPERAND_INDEX, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VECTOR, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::VECTOR_LENGTH as i32);

    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let gc = GcCodegen::new();
    // The barrier needs two scratches (`T0`, `T3`), `src` loads after it.
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    context.load_register(masm, OpVectorSet::OPERAND_SRC, tmp::T3);
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::VECTOR_DATA as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T3};
    );
    if context.profiling() {
        context.metadata(
            masm,
            OpVectorSet::METADATA_KIND,
            OpVectorSet::METADATA_ARRAY,
            tmp::T0,
            tmp::T2,
        );
    }
    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARRAY_FAST, tmp::T2);
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
    context.slow_control(masm, slow, SlowPath::VectorOp);
}
