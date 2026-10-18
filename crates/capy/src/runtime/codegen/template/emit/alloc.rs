//! Allocation: pairs, closures, environments, rest lists.
//!
//! Pairs, closures and environments allocate through
//! [`GcCodegen::emit_alloc`] and initialize after its join. Rest lists
//! cons a frame range, so they call their operation outright.

use onlineasm::onlineasm;
use smallvec::SmallVec;

use crate::bytecode::def::{OpClosure, OpClosureEnv, OpCons, OpMakeEnv, OpRestList, OpValuesRest};
use crate::runtime::codegen::gc::alloc::AllocSize;
use crate::runtime::codegen::gc::{GcCodegen, header_word};
use crate::runtime::codegen::masm::{Masm, RuntimeExtern};
use crate::runtime::codegen::template::{
    OpArg, OpContinue, OpResult, OpTarget, OperationCall, TemplateContext, operand_register, tmp,
};
use crate::runtime::sabi::{self, builtin_class_ids, regs};
use crate::runtime::value::Value;

pub fn op_cons<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    let (t0, t1, t2) = (tmp::T0, tmp::T1, tmp::T2);
    let pending = gc.emit_alloc(
        masm,
        AllocSize::Bytes(sabi::OBJECT_HEADER_BYTES + sabi::PAIR_SIZE),
        header_word(builtin_class_ids::PAIR),
        t0,
        t1,
        t2,
    );
    let car = masm.assembler().mem(t0, sabi::PAIR_CAR as i32);
    let cdr = masm.assembler().mem(t0, sabi::PAIR_CDR as i32);
    context.load_register(masm, OpCons::OPERAND_CAR, t1);
    onlineasm!(*masm.assembler(); storeq {car}, {t1};);
    context.load_register(masm, OpCons::OPERAND_CDR, t1);
    onlineasm!(*masm.assembler(); storeq {cdr}, {t1};);
    context.store_register(masm, OpCons::OPERAND_DST, t0, t1);
    context.next(masm);
    gc.emit_alloc_slow(masm, context, pending);
}

pub fn op_closure<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_closure(masm, context, false);
}

pub fn op_closure_env<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_closure(masm, context, true);
}

/// `closure dst, constant, nfree` and `closure_env dst, constant, nfree, env`
/// (`Closure::new`): the machine entry and the code block, `nfree`, the
/// code block's metadata, and free variables that are undefined, or the
/// environment in the first.
fn emit_closure<C: TemplateContext>(masm: &mut Masm, context: &mut C, env: bool) {
    let (constant, nfree, signed, dst) = if env {
        (
            OpClosureEnv::OPERAND_CONSTANT,
            OpClosureEnv::OPERAND_NFREE,
            OpClosureEnv::SIGNED_NFREE,
            OpClosureEnv::OPERAND_DST,
        )
    } else {
        (
            OpClosure::OPERAND_CONSTANT,
            OpClosure::OPERAND_NFREE,
            OpClosure::SIGNED_NFREE,
            OpClosure::OPERAND_DST,
        )
    };
    let gc = GcCodegen::new();
    let (t0, t1, t2, t3, rv) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3, regs::RV);
    operand_register(masm, context, nfree, signed);
    let pending = gc.emit_alloc(
        masm,
        AllocSize::Words {
            count: t0,
            base: sabi::OBJECT_HEADER_BYTES + sabi::CLOSURE_SIZE,
        },
        header_word(builtin_class_ids::CLOSURE),
        t1,
        t2,
        rv,
    );
    // The code block loads after the join: the slow path may move it.
    context.constant(masm, constant, t3);
    let meta = masm.assembler().mem(t3, sabi::CODE_BLOCK_METADATA as i32);
    let code_at = masm.assembler().mem(t1, sabi::CLOSURE_CODE as i32);
    let block_at = masm.assembler().mem(t1, sabi::CLOSURE_CODE_BLOCK as i32);
    let meta_at = masm.assembler().mem(t1, sabi::CLOSURE_META as i32);
    let nfree_at = masm.assembler().mem(t1, sabi::CLOSURE_NFREE as i32);
    context.machine_entry(masm, rv);
    onlineasm!(*masm.assembler();
        storep {code_at}, {rv};
        storeq {block_at}, {t3};
        loadq {rv}, {meta};
        storeq {meta_at}, {rv};
        storeq {nfree_at}, {t0};
        addp {t2}, {t1}, {sabi::CLOSURE_FREE as i64};
        move {rv}, {Value::undefined().bits() as i64};
    );
    gc.emit_fill(masm, t2, t0, rv);
    if env {
        let first = masm.assembler().mem(t1, sabi::CLOSURE_FREE as i32);
        context.load_register(masm, OpClosureEnv::OPERAND_ENV, t0);
        onlineasm!(*masm.assembler(); storeq {first}, {t0};);
    }
    context.store_register(masm, dst, t1, t0);
    context.next(masm);
    gc.emit_alloc_slow(masm, context, pending);
}

/// `make_env dst, size`
pub fn op_make_env<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let gc = GcCodegen::new();
    let (t0, t1, t2, rv) = (tmp::T0, tmp::T1, tmp::T2, regs::RV);
    operand_register(
        masm,
        context,
        OpMakeEnv::OPERAND_SIZE,
        OpMakeEnv::SIGNED_SIZE,
    );
    let pending = gc.emit_alloc(
        masm,
        AllocSize::Words {
            count: t0,
            base: sabi::OBJECT_HEADER_BYTES + sabi::ENV_SIZE,
        },
        header_word(builtin_class_ids::ENV_RECORD),
        t1,
        t2,
        rv,
    );
    let length_at = masm.assembler().mem(t1, sabi::ENV_LENGTH as i32);
    onlineasm!(*masm.assembler();
        storeq {length_at}, {t0};
        addp {t2}, {t1}, {sabi::ENV_DATA as i64};
        move {rv}, {Value::undefined().bits() as i64};
    );
    gc.emit_fill(masm, t2, t0, rv);
    context.store_register(masm, OpMakeEnv::OPERAND_DST, t1, t0);
    context.next(masm);
    gc.emit_alloc_slow(masm, context, pending);
}

pub fn op_rest_list<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    onlineasm!(*masm.assembler(); jmp {slow};);
    context.call_operation(
        masm,
        slow,
        OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::RestList),
            args: smallvec::smallvec![
                OpArg::Field {
                    field: OpRestList::OPERAND_BASE,
                    signed: false,
                },
                OpArg::Slot(OpRestList::OPERAND_CNT),
                OpArg::Field {
                    field: OpRestList::OPERAND_SKIP,
                    signed: OpRestList::SIGNED_SKIP,
                },
            ],
            preserve: SmallVec::new(),
            result: OpResult::Slot(OpRestList::OPERAND_DST),
            checks_error: false,
            cont: OpContinue::Next,
        },
    );
}

pub fn op_values_rest<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    onlineasm!(*masm.assembler(); jmp {slow};);
    context.call_operation(
        masm,
        slow,
        OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::ValuesRest),
            args: smallvec::smallvec![
                OpArg::Field {
                    field: OpValuesRest::OPERAND_BASE,
                    signed: false,
                },
                OpArg::Field {
                    field: OpValuesRest::OPERAND_FROM,
                    signed: OpValuesRest::SIGNED_FROM,
                },
            ],
            preserve: SmallVec::new(),
            result: OpResult::Slot(OpValuesRest::OPERAND_BASE),
            checks_error: false,
            cont: OpContinue::Next,
        },
    );
}
