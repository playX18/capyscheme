//! Typed memory primitives: `bytevector-<t>-native-ref/set!` and `pointer-ref/<t>`,
//! `pointer-set!/<t>`.
//!
//! Every template computes the element address into `T1` after its checks, then does one sized
//! load or store. Anything unusual (wrong class, non-fixnum index, out of range, unaligned float,
//! value that does not fit, NULL pointer) jumps to the slow label, which runs the out-of-line
//! `prims::memory` code and so raises the same errors. 64-bit integers and `u32` can need a
//! bignum, so they have no template and go through `prim_direct`.

use onlineasm::onlineasm;

use crate::bytecode::def::{OpCallPrimitive2, OpCallPrimitive3};
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{PrimSlowId, TemplateContext, ftmp, tmp};
use crate::runtime::prims::def::Primitive;
use crate::runtime::prims::memory::Elem;
use crate::runtime::sabi::{self, builtin_class_ids};
use crate::runtime::value::Value;

#[derive(Clone, Copy)]
enum Space {
    Bytevector,
    Pointer,
}

use Elem::*;
use Primitive as P;
use Space::*;

const REFS: &[(Primitive, Space, Elem)] = &[
    (P::BvS8Ref, Bytevector, S8),
    (P::BvU16Ref, Bytevector, U16),
    (P::BvS16Ref, Bytevector, S16),
    (P::BvS32Ref, Bytevector, S32),
    (P::BvF32Ref, Bytevector, F32),
    (P::BvF64Ref, Bytevector, F64),
    (P::PtrU8Ref, Pointer, U8),
    (P::PtrS8Ref, Pointer, S8),
    (P::PtrU16Ref, Pointer, U16),
    (P::PtrS16Ref, Pointer, S16),
    (P::PtrS32Ref, Pointer, S32),
    (P::PtrF32Ref, Pointer, F32),
    (P::PtrF64Ref, Pointer, F64),
];

const SETS: &[(Primitive, Space, Elem)] = &[
    (P::BvU8Set, Bytevector, U8),
    (P::BvS8Set, Bytevector, S8),
    (P::BvU16Set, Bytevector, U16),
    (P::BvS16Set, Bytevector, S16),
    (P::BvS32Set, Bytevector, S32),
    (P::BvF32Set, Bytevector, F32),
    (P::BvF64Set, Bytevector, F64),
    (P::PtrU8Set, Pointer, U8),
    (P::PtrS8Set, Pointer, S8),
    (P::PtrU16Set, Pointer, U16),
    (P::PtrS16Set, Pointer, S16),
    (P::PtrS32Set, Pointer, S32),
    (P::PtrF32Set, Pointer, F32),
    (P::PtrF64Set, Pointer, F64),
];

/// Emits the two-operand template for `id` when it has one.
pub fn emit_primitive2<C: TemplateContext>(masm: &mut Masm, context: &mut C, id: i64) -> bool {
    let Some(&(prim, space, elem)) = REFS.iter().find(|(p, ..)| *p as u16 as i64 == id) else {
        return false;
    };
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    address(masm, space, elem, slow);
    let at = masm.assembler().mem(tmp::T1, 0);
    match elem {
        U8 => onlineasm!(*masm.assembler(); loadb {tmp::T0}, {at};),
        S8 => onlineasm!(*masm.assembler(); loadbsi {tmp::T0}, {at};),
        U16 => onlineasm!(*masm.assembler(); loadh {tmp::T0}, {at};),
        S16 => onlineasm!(*masm.assembler(); loadhsi {tmp::T0}, {at};),
        S32 => onlineasm!(*masm.assembler(); loadi {tmp::T0}, {at};),
        F32 => onlineasm!(*masm.assembler();
            loadf {ftmp::F0}, {at};
            cf2d {ftmp::F0}, {ftmp::F0};
        ),
        F64 => onlineasm!(*masm.assembler(); loadd {ftmp::F0}, {at};),
        U32 | U64 | S64 => unreachable!("no template for {elem:?}"),
    }
    if matches!(elem, F32 | F64) {
        masm.box_double_fpr(tmp::T0, ftmp::F0);
    } else {
        masm.box_int32(tmp::T0, tmp::T0);
    }
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(prim as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
    true
}

/// Emits the three-operand template for `id` when it has one.
pub fn emit_primitive3<C: TemplateContext>(masm: &mut Masm, context: &mut C, id: i64) -> bool {
    let Some(&(prim, space, elem)) = SETS.iter().find(|(p, ..)| *p as u16 as i64 == id) else {
        return false;
    };
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive3::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive3::OPERAND_B, tmp::T2);
    address(masm, space, elem, slow);
    context.load_register(masm, OpCallPrimitive3::OPERAND_C, tmp::T3);
    let at = masm.assembler().mem(tmp::T1, 0);
    if matches!(elem, F32 | F64) {
        let (not_double, have) = (masm.label(), masm.label());
        masm.branch_if_not_double(tmp::T3, tmp::T0, not_double);
        masm.unbox_double_fpr(ftmp::F0, tmp::T3, tmp::T0);
        onlineasm!(*masm.assembler(); jmp {have};);
        masm.bind(not_double);
        masm.branch_if_not_fixnum(tmp::T3, tmp::T0, slow);
        masm.fixnum_to_double(ftmp::F0, tmp::T3);
        masm.bind(have);
        if elem == F32 {
            onlineasm!(*masm.assembler();
                cd2f {ftmp::F0}, {ftmp::F0};
                storef {at}, {ftmp::F0};
            );
        } else {
            onlineasm!(*masm.assembler(); stored {at}, {ftmp::F0};);
        }
    } else {
        masm.branch_if_not_fixnum(tmp::T3, tmp::T0, slow);
        masm.unbox_int32(tmp::T3, tmp::T3);
        // Sign-extended, so a negative value is huge as unsigned: one compare covers both
        // bounds of the unsigned types, and biasing by the minimum covers the signed ones.
        match elem {
            U8 => onlineasm!(*masm.assembler(); bqaeq {tmp::T3}, 256, {slow};),
            U16 => onlineasm!(*masm.assembler(); bqaeq {tmp::T3}, 65536, {slow};),
            S8 => onlineasm!(*masm.assembler();
                addq {tmp::T0}, {tmp::T3}, 128;
                bqaeq {tmp::T0}, 256, {slow};
            ),
            S16 => onlineasm!(*masm.assembler();
                addq {tmp::T0}, {tmp::T3}, 32768;
                bqaeq {tmp::T0}, 65536, {slow};
            ),
            _ => {}
        }
        match elem.size() {
            1 => onlineasm!(*masm.assembler(); storeb {at}, {tmp::T3};),
            2 => onlineasm!(*masm.assembler(); storeh {at}, {tmp::T3};),
            _ => onlineasm!(*masm.assembler(); storei {at}, {tmp::T3};),
        }
    }
    onlineasm!(*masm.assembler(); move {tmp::T0}, {Value::VALUE_UNDEFINED};);
    context.store_register(masm, OpCallPrimitive3::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(prim as u16),
        &[
            OpCallPrimitive3::OPERAND_A,
            OpCallPrimitive3::OPERAND_B,
            OpCallPrimitive3::OPERAND_C,
        ],
        OpCallPrimitive3::OPERAND_DST,
    );
    true
}

/// Checks the object in `T1` and the index in `T2` and leaves the element address in `T1`.
/// Clobbers `T0`, `T2` and `T3`.
fn address(masm: &mut Masm, space: Space, elem: Elem, slow: onlineasm::Label) {
    match space {
        Bytevector => {
            let (immutable, checked) = (masm.label(), masm.label());
            masm.branch_not_heap_class(
                tmp::T1,
                builtin_class_ids::MUTABLE_BYTEVECTOR,
                tmp::T0,
                immutable,
            );
            onlineasm!(*masm.assembler(); jmp {checked};);
            masm.bind(immutable);
            masm.branch_not_heap_class(
                tmp::T1,
                builtin_class_ids::IMMUTABLE_BYTEVECTOR,
                tmp::T0,
                slow,
            );
            masm.bind(checked);
            masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T2, tmp::T2);
            let len_at = masm.assembler().mem(tmp::T1, sabi::BYTEVECTOR_LENGTH as i32);
            let contents_at = masm
                .assembler()
                .mem(tmp::T1, sabi::BYTEVECTOR_CONTENTS as i32);
            let last = elem.size() as i64 - 1;
            onlineasm!(*masm.assembler();
                loadq {tmp::T0}, {len_at};
                bqaeq {tmp::T2}, {tmp::T0}, {slow};
                addq {tmp::T3}, {tmp::T2}, {last};
                bqaeq {tmp::T3}, {tmp::T0}, {slow};
            );
            if matches!(elem, F32 | F64) {
                onlineasm!(*masm.assembler();
                    andq {tmp::T3}, {tmp::T2}, {last};
                    btqnz {tmp::T3}, {slow};
                );
            }
            onlineasm!(*masm.assembler();
                loadq {tmp::T0}, {contents_at};
                addq {tmp::T1}, {tmp::T0}, {tmp::T2};
            );
        }
        Pointer => {
            masm.branch_not_heap_class(tmp::T1, builtin_class_ids::POINTER, tmp::T0, slow);
            masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T2, tmp::T2);
            let value_at = masm.assembler().mem(tmp::T1, sabi::POINTER_VALUE as i32);
            onlineasm!(*masm.assembler();
                loadq {tmp::T0}, {value_at};
                btqz {tmp::T0}, {slow};
                bqlt {tmp::T2}, 0, {slow};
                addq {tmp::T1}, {tmp::T0}, {tmp::T2};
                bqb {tmp::T1}, {tmp::T0}, {slow};
            );
        }
    }
}
