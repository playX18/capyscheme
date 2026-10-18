//! Inline primitives: pairs, identity, fixnum arithmetic, vectors.

use core::mem::{offset_of, size_of};

use onlineasm::{Label, onlineasm};

use crate::bytecode::def::{
    OpAbs, OpAdd, OpAddImmediate, OpAddImmediateMove, OpCar, OpCdr, OpDiv, OpEq, OpGreater,
    OpGreaterEqual, OpLess, OpLessEqual, OpModulo, OpMul, OpNegate, OpNot, OpNumberEqual,
    OpQuotient, OpRemainder, OpSqrt, OpSub, OpVariableRef, OpVectorLength, OpVectorRef,
    OpVectorRefMetadata, OpVectorRefMul, OpVectorRefMulMetadata,
};
use crate::runtime::codegen::masm::{Cmp, Masm};
use crate::runtime::codegen::template::{
    ArithShape, ArithSpec, Operand, SlowPath, TemplateContext, ftmp, tmp,
};
use crate::runtime::interp::profile::{
    ARITH_FIX_FLON, ARITH_FIXNUMS, ARITH_FLON_FIX, ARITH_FLON_FLON, ARRAY_FAST, ArrayProfile,
};
use crate::runtime::sabi::{self, builtin_class_ids};
use crate::runtime::value::Value;
use crate::runtime::vm::exceptions::RaiseKind;

macro_rules! pair_op {
    ($name:ident, $Op:ident, $off:expr, $kind:expr) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, $Op::OPERAND_SRC, tmp::T1);
            masm.branch_not_heap_class(tmp::T1, builtin_class_ids::PAIR, tmp::T0, slow);
            let slot = masm.assembler().mem(tmp::T1, $off);
            onlineasm!(*masm.assembler();
                loadq {tmp::T1}, {slot};
            );
            if context.profiling() {
                context.metadata(
                    masm,
                    $Op::METADATA_KIND,
                    $Op::METADATA_PROFILE,
                    tmp::T0,
                    tmp::T3,
                );
            }
            masm.maybe_observe_value_profile(context.profiling(), tmp::T0, tmp::T1, tmp::T2, tmp::T3);
            context.store_register(masm, $Op::OPERAND_DST, tmp::T1, tmp::T0);
            context.next(masm);
            context.slow_control(masm, slow, SlowPath::RaiseKind(tmp::T1, $kind));
        }
    };
}

pair_op!(
    op_car,
    OpCar,
    sabi::PAIR_CAR as i32,
    RaiseKind::CarNotPair.code() as u8
);
pair_op!(
    op_cdr,
    OpCdr,
    sabi::PAIR_CDR as i32,
    RaiseKind::CdrNotPair.code() as u8
);

pub fn op_eq<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpEq::OPERAND_LHS, tmp::T1);
    context.load_register(masm, OpEq::OPERAND_RHS, tmp::T2);
    let no = Value::VALUE_FALSE;
    onlineasm!(*masm.assembler();
        cqeq {tmp::T0}, {tmp::T1}, {tmp::T2};
        orq {tmp::T0}, {tmp::T0}, {no};
    );
    context.store_register(masm, OpEq::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

pub fn op_not<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpNot::OPERAND_SRC, tmp::T1);
    let no = Value::VALUE_FALSE;
    onlineasm!(*masm.assembler();
        cqeq {tmp::T0}, {tmp::T1}, {no};
        orq {tmp::T0}, {tmp::T0}, {no};
    );
    context.store_register(masm, OpNot::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// The inline fast paths of one binary arithmetic op: loads `lhs`/`rhs`,
/// observes `kind`/`field`, and joins every path at `done` with its
/// result in `T0`. The caller owns the shape check (deferred shapes
/// leave before emitting anything), the result store, `next`, and the
/// slow block. Shared by `arith_op!` and the fused multiply below.
macro_rules! arith_op_fast {
    ($masm:ident, $context:ident, $shape:expr, $lhs:expr, $rhs:expr, $kind:expr, $field:expr, $checked:ident, $flonum:ident, $slow:expr) => {{
        let both = $shape == ArithShape::Full;
        let fixnum = $masm.label();
        let done = $masm.label();
        let lhs_flonum = $masm.label();
        let rhs_fixnum = $masm.label();
        $context.load_register($masm, $lhs, tmp::T1);
        $context.load_register($masm, $rhs, tmp::T2);
        if $context.profiling() {
            $context.metadata($masm, $kind, $field, tmp::T0, tmp::T3);
        }
        // The fixnum path sits last so the hot path stays straight-line,
        // every path joins at `done` with its result in `T0`, keeping
        // `next` terminal for the JIT fallthrough
        if both {
            $masm.branch_if_fixnums(tmp::T1, tmp::T2, tmp::T3, fixnum);
        } else if $shape.emit_fixnum() {
            $masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, $slow);
        }
        if $shape.emit_flonum() {
            if both {
                // At least one side is a flonum, or neither side is a
                // number: test each side, converting fixnums, and record
                // the exact pair each branch saw.
                $masm.branch_if_not_fixnum(tmp::T1, tmp::T3, lhs_flonum);
                $masm.branch_if_not_double(tmp::T2, tmp::T3, $slow);
                $masm.maybe_or_profile_immediate($context.profiling(), tmp::T0, ARITH_FIX_FLON, tmp::T3);
                $masm.fixnum_to_double(ftmp::F0, tmp::T1);
                $masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                $masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                onlineasm!(*$masm.assembler();
                    jmp {done};
                );
                $masm.bind(lhs_flonum);
                $masm.branch_if_not_double(tmp::T1, tmp::T3, $slow);
                $masm.branch_if_fixnum(tmp::T2, tmp::T3, rhs_fixnum);
                // The fallthrough is the both-flonum leaf: the rhs must
                // be verified a double
                $masm.branch_if_not_double(tmp::T2, tmp::T3, $slow);
                $masm.maybe_or_profile_immediate($context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                $masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                $masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                $masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                onlineasm!(*$masm.assembler();
                    jmp {done};
                );
                $masm.bind(rhs_fixnum);
                $masm.maybe_or_profile_immediate($context.profiling(), tmp::T0, ARITH_FLON_FIX, tmp::T3);
                $masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                $masm.fixnum_to_double(ftmp::F1, tmp::T2);
                $masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                onlineasm!(*$masm.assembler();
                    jmp {done};
                );
            } else {
                // Flonum-only: both sides must be doubles, a fixnum
                // arriving later takes the slow path (and, once
                // repatching lands, upgrades the site).
                $masm.branch_if_not_double(tmp::T1, tmp::T3, $slow);
                $masm.branch_if_not_double(tmp::T2, tmp::T3, $slow);
                $masm.maybe_or_profile_immediate($context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                $masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                $masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                $masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
            }
        }
        if both {
            $masm.bind(fixnum);
        }
        if $shape.emit_fixnum() {
            $masm.maybe_or_profile_immediate($context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
            // The checked op reads only the low 32 bits, so the boxed
            // values go in directly with no unbox.
            $masm.$checked(tmp::T0, tmp::T1, tmp::T2, tmp::T3, $slow);
        }
        if both {
            $masm.bind(done);
        }
    }};
}

macro_rules! arith_op {
    ($name:ident, $Op:ident, $checked:ident, $flonum:ident) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() {
                // Nothing inline but a jump to a live site, which the
                // first miss specializes.
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            arith_op_fast!(
                masm,
                context,
                shape,
                $Op::OPERAND_LHS,
                $Op::OPERAND_RHS,
                $Op::METADATA_KIND,
                $Op::METADATA_PROFILE,
                $checked,
                $flonum,
                slow
            );
            context.store_register(masm, $Op::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
        }
    };
}

arith_op!(op_add, OpAdd, fixnum_add_checked, flonum_add);
arith_op!(op_sub, OpSub, fixnum_sub_checked, flonum_sub);
arith_op!(op_mul, OpMul, fixnum_mul_checked, flonum_mul);

/// Division with fixnum and flonum leaves, mirroring `arith_op!`: the
/// fixnum path sits last, every path joins at `done` with its result in
/// `T0`. `$fixleaf` selects the fixnum result (`fixnum_div_exact` keeps
/// only exact division, `fixnum_quotient` truncates). When `$guard_zeros`,
/// the flonum leaves send zeros to the thunk: a zero dividend stays a
/// fixnum there, flonum zeros divide to a canonical `+0.0`, and a zero
/// fixnum divisor is handled there. When `$flonum_leaves` is false (quotient)
/// there are no flonum leaves: hardware division does not truncate and the
/// thunk owns the integral-flonum check, so flonum operands go slow.
macro_rules! div_op {
    ($name:ident, $Op:ident, $fixleaf:ident, $guard_zeros:literal, $flonum_leaves:literal) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() {
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            if !$flonum_leaves && !shape.emit_fixnum() {
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            let both = shape == ArithShape::Full;
            let fixnum = masm.label();
            let done = masm.label();
            let lhs_flonum = masm.label();
            let rhs_fixnum = masm.label();
            context.load_register(masm, $Op::OPERAND_LHS, tmp::T1);
            context.load_register(masm, $Op::OPERAND_RHS, tmp::T2);
            if context.profiling() {
                context.metadata(
                    masm,
                    $Op::METADATA_KIND,
                    $Op::METADATA_PROFILE,
                    tmp::T0,
                    tmp::T3,
                );
            }
            if both {
                masm.branch_if_fixnums(tmp::T1, tmp::T2, tmp::T3, fixnum);
            } else if shape.emit_fixnum() {
                masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, slow);
            }
            if !$flonum_leaves && both {
                onlineasm!(*masm.assembler();
                    jmp {slow};
                );
            }
            if $flonum_leaves && shape.emit_flonum() {
                if both {
                    masm.branch_if_not_fixnum(tmp::T1, tmp::T3, lhs_flonum);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIX_FLON, tmp::T3);
                    masm.fixnum_to_double(ftmp::F0, tmp::T1);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    if $guard_zeros {
                        masm.branch_if_zero_fixnum(tmp::T1, slow);
                    }
                    masm.flonum_div(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                    masm.bind(lhs_flonum);
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_fixnum(tmp::T2, tmp::T3, rhs_fixnum);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    if $guard_zeros {
                        masm.branch_if_zero_double(ftmp::F0, ftmp::F2, tmp::T3, slow);
                    }
                    masm.flonum_div(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                    masm.bind(rhs_fixnum);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FIX, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.fixnum_to_double(ftmp::F1, tmp::T2);
                    if $guard_zeros {
                        masm.branch_if_zero_double(ftmp::F0, ftmp::F2, tmp::T3, slow);
                        masm.branch_if_zero_fixnum(tmp::T2, slow);
                    }
                    masm.flonum_div(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                } else {
                    // Flonum-only: both sides must be doubles.
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    if $guard_zeros {
                        masm.branch_if_zero_double(ftmp::F0, ftmp::F2, tmp::T3, slow);
                    }
                    masm.flonum_div(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
                }
            }
            if both {
                masm.bind(fixnum);
            }
            if shape.emit_fixnum() {
                masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
                // The top branch only proved fixnums, the divisor guards
                // run here, beside the divide they protect.
                masm.check_divisor(tmp::T1, tmp::T2, slow);
                masm.$fixleaf(tmp::T0, tmp::T1, tmp::T2, tmp::T3, slow);
            }
            if both {
                masm.bind(done);
            }
            context.store_register(masm, $Op::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
        }
    };
}

div_op!(op_div, OpDiv, fixnum_div_exact, false, true);
div_op!(op_quotient, OpQuotient, fixnum_quotient, true, false);

/// Remainder with a fixnum only

macro_rules! rem_op {
    ($name:ident, $Op:ident, $fixleaf:ident) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() || shape == ArithShape::FlonumOnly {
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            context.load_register(masm, $Op::OPERAND_LHS, tmp::T1);
            context.load_register(masm, $Op::OPERAND_RHS, tmp::T2);
            if context.profiling() {
                context.metadata(
                    masm,
                    $Op::METADATA_KIND,
                    $Op::METADATA_PROFILE,
                    tmp::T0,
                    tmp::T3,
                );
            }
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, slow);
            masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
            masm.check_divisor(tmp::T1, tmp::T2, slow);
            masm.$fixleaf(tmp::T0, tmp::T1, tmp::T2, tmp::T3, slow);
            context.store_register(masm, $Op::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
        }
    };
}

rem_op!(op_remainder, OpRemainder, fixnum_remainder);
rem_op!(op_modulo, OpModulo, fixnum_modulo);

/// Unary arithmetic with fixnum and flonum paths.
macro_rules! unary_op {
    ($name:ident, $Op:ident, $fixleaf:ident, $flonum:ident) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() {
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            let both = shape == ArithShape::Full;
            let fixnum = masm.label();
            let done = masm.label();
            context.load_register(masm, $Op::OPERAND_SRC, tmp::T1);
            if context.profiling() {
                context.metadata(
                    masm,
                    $Op::METADATA_KIND,
                    $Op::METADATA_PROFILE,
                    tmp::T0,
                    tmp::T3,
                );
            }
            if both {
                masm.branch_if_fixnum(tmp::T1, tmp::T3, fixnum);
            } else if shape.emit_fixnum() {
                masm.branch_if_not_fixnum(tmp::T1, tmp::T3, slow);
            }
            if shape.emit_flonum() {
                masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1);
                if both {
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                }
            }
            if both {
                masm.bind(fixnum);
            }
            if shape.emit_fixnum() {
                masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
                masm.$fixleaf(tmp::T0, tmp::T1, tmp::T3, slow);
            }
            if both {
                masm.bind(done);
            }
            context.store_register(masm, $Op::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
        }
    };
}

unary_op!(op_negate, OpNegate, fixnum_neg_checked, flonum_neg);
unary_op!(op_abs, OpAbs, fixnum_abs_checked, flonum_abs);

/// Square root
pub fn op_sqrt<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    let spec = context.arith_site(OpSqrt::METADATA_KIND, OpSqrt::METADATA_PROFILE);
    let shape = spec.shape;
    if shape.is_deferred() || shape == ArithShape::FixnumOnly {
        context.slow_entry(masm, &spec, SlowPath::NumberArith);
        return;
    }
    context.load_register(masm, OpSqrt::OPERAND_SRC, tmp::T1);
    if context.profiling() {
        context.metadata(
            masm,
            OpSqrt::METADATA_KIND,
            OpSqrt::METADATA_PROFILE,
            tmp::T0,
            tmp::T3,
        );
    }
    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
    masm.flonum_sqrt(tmp::T0, ftmp::F0, ftmp::F1);
    context.store_register(masm, OpSqrt::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
}

fn emit_addi_fast<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    spec: &ArithSpec,
    src: usize,
    value: usize,
    signed_value: bool,
    kind: usize,
    field: usize,
    slow: Label,
) -> bool {
    let shape = spec.shape;
    if shape.is_deferred() {
        return false;
    }
    let both = shape == ArithShape::Full;
    let fixnum = masm.label();
    let done = masm.label();
    context.load_register(masm, src, tmp::T1);
    if context.profiling() {
        context.metadata(masm, kind, field, tmp::T0, tmp::T3);
    }

    if both {
        masm.branch_if_fixnum(tmp::T1, tmp::T3, fixnum);
    } else if shape.emit_fixnum() {
        masm.branch_if_not_fixnum(tmp::T1, tmp::T3, slow);
    }
    if shape.emit_flonum() {
        masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
        masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FIX, tmp::T3);
        masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
        match context.operand(masm, value, signed_value) {
            Operand::Immediate(i) => {
                onlineasm!(*masm.assembler();
                    move {tmp::T2}, {i};
                );
                masm.fixnum_to_double(ftmp::F1, tmp::T2);
            }
            Operand::Register(r) => {
                masm.fixnum_to_double(ftmp::F1, r);
            }
        }
        masm.flonum_add(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
        if both {
            onlineasm!(*masm.assembler();
                jmp {done};
            );
        }
    }
    if both {
        masm.bind(fixnum);
    }
    if shape.emit_fixnum() {
        masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);

        match context.operand(masm, value, signed_value) {
            Operand::Immediate(i) => masm.fixnum_add_immediate_checked(tmp::T0, tmp::T1, i, slow),
            Operand::Register(r) => {
                if r != tmp::T2 {
                    onlineasm!(*masm.assembler();
                        move {tmp::T2}, {r};
                    );
                }
                masm.fixnum_add_checked(tmp::T0, tmp::T1, tmp::T2, tmp::T3, slow);
            }
        }
    }
    if both {
        masm.bind(done);
    }
    true
}

pub fn op_add_immediate<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    let spec = context.arith_site(
        OpAddImmediate::METADATA_KIND,
        OpAddImmediate::METADATA_PROFILE,
    );
    if !emit_addi_fast(
        masm,
        context,
        &spec,
        OpAddImmediate::OPERAND_SRC,
        OpAddImmediate::OPERAND_VALUE,
        OpAddImmediate::SIGNED_VALUE,
        OpAddImmediate::METADATA_KIND,
        OpAddImmediate::METADATA_PROFILE,
        slow,
    ) {
        context.slow_entry(masm, &spec, SlowPath::NumberArith);
        return;
    }
    context.store_register(masm, OpAddImmediate::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
}

pub fn op_add_immediate_move<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    let spec = context.arith_site(
        OpAddImmediateMove::METADATA_KIND,
        OpAddImmediateMove::METADATA_PROFILE,
    );
    if !emit_addi_fast(
        masm,
        context,
        &spec,
        OpAddImmediateMove::OPERAND_SRC,
        OpAddImmediateMove::OPERAND_VALUE,
        OpAddImmediateMove::SIGNED_VALUE,
        OpAddImmediateMove::METADATA_KIND,
        OpAddImmediateMove::METADATA_PROFILE,
        slow,
    ) {
        context.slow_entry(masm, &spec, SlowPath::NumberArith);
        return;
    }
    context.store_register(masm, OpAddImmediateMove::OPERAND_TMP, tmp::T0, tmp::T1);
    context.store_register(masm, OpAddImmediateMove::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
}

macro_rules! compare_op {
    ($name:ident, $Op:ident, $cmp:expr) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() {
                context.slow_entry(masm, &spec, SlowPath::NumberArith);
                return;
            }
            let both = shape == ArithShape::Full;
            let fixnum = masm.label();
            let done = masm.label();
            let lhs_flonum = masm.label();
            let rhs_fixnum = masm.label();
            context.load_register(masm, $Op::OPERAND_LHS, tmp::T1);
            context.load_register(masm, $Op::OPERAND_RHS, tmp::T2);
            if context.profiling() {
                context.metadata(
                    masm,
                    $Op::METADATA_KIND,
                    $Op::METADATA_PROFILE,
                    tmp::T0,
                    tmp::T3,
                );
            }
            // As in `arith_op`: the fixnum path sits last, every path joins
            // at `done` with its boolean in `T0`, and `next` stays terminal.
            if both {
                masm.branch_if_fixnums(tmp::T1, tmp::T2, tmp::T3, fixnum);
            } else if shape.emit_fixnum() {
                masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, slow);
            }
            if shape.emit_flonum() {
                if both {
                    masm.branch_if_not_fixnum(tmp::T1, tmp::T3, lhs_flonum);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIX_FLON, tmp::T3);
                    masm.fixnum_to_double(ftmp::F0, tmp::T1);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, $cmp);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                    masm.bind(lhs_flonum);
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_fixnum(tmp::T2, tmp::T3, rhs_fixnum);
                    // As in `arith_op`: the rhs must be verified a double.
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, $cmp);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                    masm.bind(rhs_fixnum);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FIX, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.fixnum_to_double(ftmp::F1, tmp::T2);
                    masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, $cmp);
                    onlineasm!(*masm.assembler();
                        jmp {done};
                    );
                } else {
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, $cmp);
                }
            }
            if both {
                masm.bind(fixnum);
            }
            if shape.emit_fixnum() {
                masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
                masm.fixnum_cmp_bool(tmp::T0, tmp::T1, tmp::T2, $cmp);
            }
            if both {
                masm.bind(done);
            }
            context.store_register(masm, $Op::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.slow_site(masm, slow, &spec, SlowPath::NumberArith);
        }
    };
}

compare_op!(op_less, OpLess, Cmp::Lt);
compare_op!(op_less_equal, OpLessEqual, Cmp::Le);
compare_op!(op_number_equal, OpNumberEqual, Cmp::Eq);
compare_op!(op_greater_equal, OpGreaterEqual, Cmp::Ge);
compare_op!(op_greater, OpGreater, Cmp::Gt);

/// The inline fast path of one vector load: `vector[index]` with its
/// value in `T3`, observing `kind`'s `array` and `profile` fields. The
/// caller asserts the value profile immediately follows the array
/// profile (the address bump below relies on it), owns the result
/// store, `next`, and the slow block. Shared by `op_vector_ref` and the
/// fused multiply below.
fn emit_vector_ref_fast<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    vector: usize,
    index: usize,
    kind: usize,
    array: usize,
    slow: Label,
) {
    context.load_register(masm, vector, tmp::T1);
    context.load_register(masm, index, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VECTOR, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::VECTOR_LENGTH as i32);
    // One unsigned compare: a negative index is huge unsigned, so it fails
    // against any valid length exactly like an over-long index does.
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::VECTOR_DATA as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T3}, {slot};
    );
    if context.profiling() {
        context.metadata(masm, kind, array, tmp::T0, tmp::T2);
    }
    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARRAY_FAST, tmp::T2);
    if context.profiling() {
        onlineasm!(*masm.assembler();
            addp {tmp::T0}, {tmp::T0}, {size_of::<ArrayProfile>() as i64};
        );
    }
    masm.maybe_observe_value_profile(context.profiling(), tmp::T0, tmp::T3, tmp::T1, tmp::T2);
}

pub fn op_vector_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    debug_assert_eq!(
        offset_of!(OpVectorRefMetadata, profile),
        offset_of!(OpVectorRefMetadata, array) + size_of::<ArrayProfile>()
    );
    let slow = masm.label();
    emit_vector_ref_fast(
        masm,
        context,
        OpVectorRef::OPERAND_VECTOR,
        OpVectorRef::OPERAND_INDEX,
        OpVectorRef::METADATA_KIND,
        OpVectorRef::METADATA_ARRAY,
        slow,
    );
    context.store_register(masm, OpVectorRef::OPERAND_DST, tmp::T3, tmp::T0);
    context.next(masm);
    context.slow_control(masm, slow, SlowPath::VectorOp);
}

pub fn op_vector_ref_mul<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    debug_assert_eq!(
        offset_of!(OpVectorRefMulMetadata, profile),
        offset_of!(OpVectorRefMulMetadata, array) + size_of::<ArrayProfile>()
    );
    let slow_vector = masm.label();
    let slow_mul = masm.label();
    // The shape first: a deferred multiply site leaves before emitting
    // anything, and its slow path runs the whole pair through the thunks.
    let spec = context.arith_site(
        OpVectorRefMul::METADATA_KIND,
        OpVectorRefMul::METADATA_ARITH,
    );
    let shape = spec.shape;
    if shape.is_deferred() {
        context.slow_entry(masm, &spec, SlowPath::NumberArith);
        return;
    }
    emit_vector_ref_fast(
        masm,
        context,
        OpVectorRefMul::OPERAND_VECTOR,
        OpVectorRefMul::OPERAND_INDEX,
        OpVectorRefMul::METADATA_KIND,
        OpVectorRefMul::METADATA_ARRAY,
        slow_vector,
    );
    context.store_register(masm, OpVectorRefMul::OPERAND_VDST, tmp::T3, tmp::T0);
    arith_op_fast!(
        masm,
        context,
        shape,
        OpVectorRefMul::OPERAND_LHS,
        OpVectorRefMul::OPERAND_RHS,
        OpVectorRefMul::METADATA_KIND,
        OpVectorRefMul::METADATA_ARITH,
        fixnum_mul_checked,
        flonum_mul,
        slow_mul
    );
    context.store_register(masm, OpVectorRefMul::OPERAND_MDST, tmp::T0, tmp::T1);
    context.next(masm);
    context.slow_control(masm, slow_vector, SlowPath::VectorOp);
    context.slow_site(masm, slow_mul, &spec, SlowPath::NumberArith);
}

pub fn op_vector_length<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpVectorLength::OPERAND_SRC, tmp::T1);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VECTOR, tmp::T0, slow);
    let len_at = masm.assembler().mem(tmp::T1, sabi::VECTOR_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpVectorLength::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.slow_control(masm, slow, SlowPath::VectorLength);
}

pub fn op_variable_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpVariableRef::OPERAND_VAR, tmp::T1);
    let car = masm.assembler().mem(tmp::T1, 0);
    onlineasm!(*masm.assembler();
        loadq {tmp::T1}, {car};
    );
    if context.profiling() {
        context.metadata(
            masm,
            OpVariableRef::METADATA_KIND,
            OpVariableRef::METADATA_PROFILE,
            tmp::T0,
            tmp::T3,
        );
    }
    masm.maybe_observe_value_profile(context.profiling(), tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    context.store_register(masm, OpVariableRef::OPERAND_DST, tmp::T1, tmp::T0);
    context.next(masm);
}
