//! Control flow: jumps, fused tests, numeric compares, switches, loop headers.

use onlineasm::onlineasm;

use crate::bytecode::def::{
    OpJump, OpJumpIfEq, OpJumpIfFalse, OpJumpIfFixnum, OpJumpIfGreater, OpJumpIfGreaterEqual,
    OpJumpIfLess, OpJumpIfLessEqual, OpJumpIfNotEq, OpJumpIfNotFixnum, OpJumpIfNotGreater,
    OpJumpIfNotGreaterEqual, OpJumpIfNotLess, OpJumpIfNotLessEqual, OpJumpIfNotNull,
    OpJumpIfNotNumberEqual, OpJumpIfNotPair, OpJumpIfNotUndefined, OpJumpIfNull,
    OpJumpIfNumberEqual, OpJumpIfPair, OpJumpIfTrue, OpJumpIfUndefined, OpSwitchChar,
    OpSwitchFixnum, OpSwitchSymbol,
};
use crate::runtime::codegen::masm::{Cmp, Masm};
use crate::runtime::codegen::template::{
    ArithShape, Condition, Operand, SlowPath, TemplateContext, ftmp, tmp,
};
use crate::runtime::interp::profile::{
    ARITH_FIX_FLON, ARITH_FIXNUMS, ARITH_FLON_FIX, ARITH_FLON_FLON,
};
use crate::runtime::sabi::{self, builtin_class_ids, regs};
use crate::runtime::value::Value;

pub fn op_jump<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.jump(masm, OpJump::OPERAND_TARGET);
}

pub fn op_jump_if_true<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfTrue::OPERAND_COND, tmp::T1);
    context.branch_if(
        masm,
        Condition::Ne,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_FALSE),
        OpJumpIfTrue::OPERAND_TARGET,
    );
}

pub fn op_jump_if_false<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfFalse::OPERAND_COND, tmp::T1);
    context.branch_if(
        masm,
        Condition::Eq,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_FALSE),
        OpJumpIfFalse::OPERAND_TARGET,
    );
}

pub fn op_jump_if_null<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNull::OPERAND_SRC, tmp::T1);
    context.branch_if(
        masm,
        Condition::Eq,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_NULL),
        OpJumpIfNull::OPERAND_TARGET,
    );
}

pub fn op_jump_if_not_null<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNotNull::OPERAND_SRC, tmp::T1);
    context.branch_if(
        masm,
        Condition::Ne,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_NULL),
        OpJumpIfNotNull::OPERAND_TARGET,
    );
}

pub fn op_jump_if_pair<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfPair::OPERAND_SRC, tmp::T1);
    let fall = masm.label();
    pair_test(masm, tmp::T1, tmp::T0, fall);
    context.jump(masm, OpJumpIfPair::OPERAND_TARGET);
    masm.bind(fall);
    context.next(masm);
}

pub fn op_jump_if_not_pair<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNotPair::OPERAND_SRC, tmp::T1);
    let taken = masm.label();
    pair_test(masm, tmp::T1, tmp::T0, taken);
    context.next(masm);
    context.slow_jump(masm, taken, OpJumpIfNotPair::OPERAND_TARGET);
}

/// Jump to `not_pair` unless `val` is a pair. Clobbers `tmp`.
fn pair_test(
    masm: &mut Masm,
    val: onlineasm::VReg,
    tmp: onlineasm::VReg,
    not_pair: onlineasm::Label,
) {
    let not_cell = regs::NOT_CELL_MASK;
    let class_mask = sabi::CLASS_ID_MASK as i64;
    let pair = builtin_class_ids::PAIR as i64;
    // x86-64 folds the mask into the test branch (single `test`), elsewhere
    // an `and` feeds the 2-operand form (the masked form would clobber
    // `val` on AArch64).
    #[cfg(target_arch = "x86_64")]
    {
        onlineasm!(*masm.assembler();
            bqeq {val}, 0, {not_pair};
            btqnz {val}, {not_cell}, {not_pair};
            loadq {tmp}, [{val} - 8];
            andq {tmp}, {tmp}, {class_mask};
            bqneq {tmp}, {pair}, {not_pair};
        );
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        onlineasm!(*masm.assembler();
            bqeq {val}, 0, {not_pair};
            andq {tmp}, {val}, {not_cell};
            btqnz {tmp}, {not_pair};
            loadq {tmp}, [{val} - 8];
            andq {tmp}, {tmp}, {class_mask};
            bqneq {tmp}, {pair}, {not_pair};
        );
    }
}

pub fn op_jump_if_fixnum<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfFixnum::OPERAND_SRC, tmp::T1);
    let taken = masm.label();
    let number_tag = regs::NUMBER_TAG;
    onlineasm!(*masm.assembler();
        bqaeq {tmp::T1}, {number_tag}, {taken};
    );
    context.next(masm);
    context.slow_jump(masm, taken, OpJumpIfFixnum::OPERAND_TARGET);
}

pub fn op_jump_if_not_fixnum<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNotFixnum::OPERAND_SRC, tmp::T1);
    let taken = masm.label();
    let number_tag = regs::NUMBER_TAG;
    onlineasm!(*masm.assembler();
        bqb {tmp::T1}, {number_tag}, {taken};
    );
    context.next(masm);
    context.slow_jump(masm, taken, OpJumpIfNotFixnum::OPERAND_TARGET);
}

pub fn op_jump_if_undefined<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfUndefined::OPERAND_SRC, tmp::T1);
    context.branch_if(
        masm,
        Condition::Eq,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_UNDEFINED),
        OpJumpIfUndefined::OPERAND_TARGET,
    );
}

pub fn op_jump_if_not_undefined<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNotUndefined::OPERAND_SRC, tmp::T1);
    context.branch_if(
        masm,
        Condition::Ne,
        Operand::Register(tmp::T1),
        Operand::Immediate(Value::VALUE_UNDEFINED),
        OpJumpIfNotUndefined::OPERAND_TARGET,
    );
}

pub fn op_jump_if_eq<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfEq::OPERAND_LHS, tmp::T1);
    context.load_register(masm, OpJumpIfEq::OPERAND_RHS, tmp::T2);
    context.branch_if(
        masm,
        Condition::Eq,
        Operand::Register(tmp::T1),
        Operand::Register(tmp::T2),
        OpJumpIfEq::OPERAND_TARGET,
    );
}

pub fn op_jump_if_not_eq<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpJumpIfNotEq::OPERAND_LHS, tmp::T1);
    context.load_register(masm, OpJumpIfNotEq::OPERAND_RHS, tmp::T2);
    context.branch_if(
        masm,
        Condition::Ne,
        Operand::Register(tmp::T1),
        Operand::Register(tmp::T2),
        OpJumpIfNotEq::OPERAND_TARGET,
    );
}

macro_rules! numeric_jump {
    ($name:ident, $Op:ident, $cond:expr, $taken:expr, $jump:expr) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            let spec = context.arith_site($Op::METADATA_KIND, $Op::METADATA_PROFILE);
            let shape = spec.shape;
            if shape.is_deferred() {
                context.slow_entry(masm, &spec, SlowPath::CompareJump);
                return;
            }
            let both = shape == ArithShape::Full;
            let flonum = masm.label();
            let lhs_flonum = masm.label();
            let rhs_fixnum = masm.label();
            let compared = masm.label();
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
                masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, flonum);
            } else if shape.emit_fixnum() {
                masm.check_fixnums(tmp::T1, tmp::T2, tmp::T3, slow);
            }
            if shape.emit_fixnum() {
                masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIXNUMS, tmp::T3);
                context.branch_if(
                    masm,
                    $cond,
                    Operand::Register(tmp::T1),
                    Operand::Register(tmp::T2),
                    $Op::OPERAND_TARGET,
                );
            }
            if both {
                masm.bind(flonum);
            }
            if shape.emit_flonum() {
                if both {
                    masm.branch_if_not_fixnum(tmp::T1, tmp::T3, lhs_flonum);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FIX_FLON, tmp::T3);
                    masm.fixnum_to_double(ftmp::F0, tmp::T1);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    onlineasm!(*masm.assembler();
                        jmp {compared};
                    );
                    masm.bind(lhs_flonum);
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_fixnum(tmp::T2, tmp::T3, rhs_fixnum);
                    // As in `arith_op`: the rhs must be verified a double.
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                    onlineasm!(*masm.assembler();
                        jmp {compared};
                    );
                    masm.bind(rhs_fixnum);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FIX, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.fixnum_to_double(ftmp::F1, tmp::T2);
                    masm.bind(compared);
                } else {
                    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
                    masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
                    masm.maybe_or_profile_immediate(context.profiling(), tmp::T0, ARITH_FLON_FLON, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
                    masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
                }
                masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, $taken);
                context.branch_if(
                    masm,
                    $jump,
                    Operand::Register(tmp::T0),
                    Operand::Immediate(Value::VALUE_FALSE),
                    $Op::OPERAND_TARGET,
                );
            }
            context.slow_site(masm, slow, &spec, SlowPath::CompareJump);
        }
    };
}

numeric_jump!(
    op_jump_if_less,
    OpJumpIfLess,
    Condition::Lt32,
    Cmp::Lt,
    Condition::Ne
);
numeric_jump!(
    op_jump_if_less_equal,
    OpJumpIfLessEqual,
    Condition::Le32,
    Cmp::Le,
    Condition::Ne
);
numeric_jump!(
    op_jump_if_number_equal,
    OpJumpIfNumberEqual,
    Condition::Eq32,
    Cmp::Eq,
    Condition::Ne
);
numeric_jump!(
    op_jump_if_greater_equal,
    OpJumpIfGreaterEqual,
    Condition::Ge32,
    Cmp::Ge,
    Condition::Ne
);
numeric_jump!(
    op_jump_if_greater,
    OpJumpIfGreater,
    Condition::Gt32,
    Cmp::Gt,
    Condition::Ne
);
numeric_jump!(
    op_jump_if_not_less,
    OpJumpIfNotLess,
    Condition::Ge32,
    Cmp::Lt,
    Condition::Eq
);
numeric_jump!(
    op_jump_if_not_less_equal,
    OpJumpIfNotLessEqual,
    Condition::Gt32,
    Cmp::Le,
    Condition::Eq
);
numeric_jump!(
    op_jump_if_not_number_equal,
    OpJumpIfNotNumberEqual,
    Condition::Ne32,
    Cmp::Eq,
    Condition::Eq
);
numeric_jump!(
    op_jump_if_not_greater_equal,
    OpJumpIfNotGreaterEqual,
    Condition::Lt32,
    Cmp::Ge,
    Condition::Eq
);
numeric_jump!(
    op_jump_if_not_greater,
    OpJumpIfNotGreater,
    Condition::Le32,
    Cmp::Gt,
    Condition::Eq
);

pub fn op_switch_fixnum<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.switch_int(
        masm,
        OpSwitchFixnum::OPERAND_SRC,
        OpSwitchFixnum::OPERAND_TABLE,
        false,
    );
}

pub fn op_switch_char<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.switch_int(
        masm,
        OpSwitchChar::OPERAND_SRC,
        OpSwitchChar::OPERAND_TABLE,
        true,
    );
}

pub fn op_switch_symbol<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.switch_sym(
        masm,
        OpSwitchSymbol::OPERAND_SRC,
        OpSwitchSymbol::OPERAND_TABLE,
    );
}

pub fn op_loop_hint<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    // The mask operand only matters to the stack walker, the fast path is
    // the yield poll plus, when the JIT is on, the heat bump.
    let (yield_slow, hot_slow) = (masm.label(), masm.label());
    masm.yieldpoint_check(tmp::T0, yield_slow, true);
    masm.maybe_hot_check(tmp::T1, tmp::T2, hot_slow);
    context.next(masm);
    context.slow_control(masm, yield_slow, SlowPath::YieldLoop);
    if context.profiling() {
        context.slow_control(masm, hot_slow, SlowPath::HotLoop);
    }
}
