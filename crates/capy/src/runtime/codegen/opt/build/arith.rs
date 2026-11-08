//! Lowering of arithmetic and comparisons, by the plan of each site.
//!
//! A site's [`ArithPlan`] comes from its profile word and its blocked state:
//!
//! - `Fixnum` and `Flonum` are the speculated paths. They check each operand
//!   and deopt before the instruction on any other kind. Two
//!   fixnums run the total int64 arithmetic and `box.int64` promotes the
//!   result to a fixnum or an inline one-limb bignum.
//! - `Wide` runs when the profile saw exact integers
//!   including a bignum. Each operand is widened through a fixnum test or
//!   `check.bigint.fits64`, the arithmetic is checked, and an i64 overflow
//!   deopts to baseline.
//! - `Dual` tests each operand. Two fixnums take an int32 path, which stays
//!   in int32 when the exact result (computed as a double, which holds every
//!   int32 sum, difference and product exactly as far as the range check
//!   needs) fits, and otherwise takes the generic path. Any other pair of
//!   fixnum and flonum takes the double path, with each fixnum converted
//!   exactly. Anything else takes the generic path.
//! - `Generic` calls [`Helper::Arith`], which runs the baseline's numeric
//!   operation. Only a non-number operand deopts it, and baseline raises.

use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Constant, Helper, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;

use super::profile::{ArithPlan, ProfileKind, plan_for, profile_words};
use super::{Arith, Builder, Flow};
use crate::bytecode::def::Instruction;
use crate::runtime::interp::profile::{
    ARITH_RHS_SHIFT, NUMBER_FIXNUM, NUMBER_FLONUM, NUMBER_KIND_MASK,
};
use crate::runtime::prims::def::Primitive;

/// The generic numeric operations the helper takes, as the fixnum it is
/// passed. The values are those of `glue::capy_opt_arith`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumOp {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Lt = 3,
    Le = 4,
    Eq = 5,
    Gt = 6,
    Ge = 7,
    Negate = 8,
}

impl NumOp {
    fn of_arith(arith: Arith) -> NumOp {
        match arith {
            Arith::Add => NumOp::Add,
            Arith::Sub => NumOp::Sub,
            Arith::Mul => NumOp::Mul,
        }
    }

    fn of_cmp(cmp: Cmp) -> Result<NumOp, String> {
        Ok(match cmp {
            Cmp::Lt => NumOp::Lt,
            Cmp::Le => NumOp::Le,
            Cmp::Eq => NumOp::Eq,
            Cmp::Gt => NumOp::Gt,
            Cmp::Ge => NumOp::Ge,
            Cmp::Ne => return Err("no numeric comparison is `!=`".to_string()),
        })
    }
}

/// The bounds of an int32 as doubles. An exact double of a sum, difference or
/// product of two int32s lies in them exactly when the int32 result does.
const INT32_MIN_F64: f64 = -2147483648.0;
const INT32_MAX_F64: f64 = 2147483647.0;

/// The arithmetic profile word of the site at `pc`, if the procedure keeps one.
fn arith_word(buffer: &crate::runtime::objectmodel::CodeBuffer<'_>, pc: usize) -> Option<u32> {
    profile_words(buffer, pc)
        .into_iter()
        .find_map(|(kind, word)| (kind == ProfileKind::Arith).then_some(word))
}

impl Builder<'_, '_> {
    /// The plan of the site at `pc`. A blocked pc stops repeating the narrow
    /// plan that failed but may still use the wider plan the profile now
    /// implies.
    pub(super) fn arith_plan(&self, pc: usize) -> ArithPlan {
        let plan = plan_for(arith_word(self.buffer, pc));

        if !(self.blocked)(pc) {
            return plan;
        }

        match plan {
            ArithPlan::Fixnum => ArithPlan::Wide,
            other => other,
        }
    }

    /// `dst = lhs op rhs`, for an add, subtract or multiply.
    pub(super) fn arith_binary(
        &mut self,
        pc: usize,
        dst: u32,
        lhs: u32,
        rhs: u32,
        arith: Arith,
    ) -> Result<Flow, String> {
        let plan = self.arith_plan(pc);
        let frame = self.frame(pc, pc)?;
        let (lhs, rhs) = (self.reg(lhs)?, self.reg(rhs)?);
        let value = self.binary_value(plan, arith, lhs, rhs, frame);
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// `dst = -src`.
    pub(super) fn negate(&mut self, pc: usize, dst: u32, src: u32) -> Result<Flow, String> {
        let plan = self.arith_plan(pc);
        let frame = self.frame(pc, pc)?;
        let src = self.reg(src)?;
        let value = match plan {
            ArithPlan::Fixnum => self.fixnum_negate(src, frame),
            ArithPlan::Flonum => self.flonum_negate(src, frame),
            ArithPlan::Dual => self.dual_negate(src, frame),
            ArithPlan::Wide => self.wide_negate(src, frame),
            ArithPlan::Rational | ArithPlan::RationalWide => {
                self.generic_numop(NumOp::Negate, src, src, frame)
            }
            ArithPlan::Generic => self.generic_numop(NumOp::Negate, src, src, frame),
        };
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// The boxed `lhs op rhs` on operands lowered by `plan`. `frame` is the
    /// state from before the instruction.
    pub(super) fn binary_value(
        &mut self,
        plan: ArithPlan,
        arith: Arith,
        lhs: Value,
        rhs: Value,
        frame: FrameState,
    ) -> Value {
        match plan {
            ArithPlan::Fixnum => self.fixnum_binary(arith, lhs, rhs, frame),
            ArithPlan::Flonum => self.flonum_binary(arith, lhs, rhs, frame),
            ArithPlan::Dual => self.dual_binary(arith, lhs, rhs, frame),
            ArithPlan::Wide => self.wide_binary(arith, lhs, rhs, frame),
            ArithPlan::Rational => self.rational_binary(arith, lhs, rhs, frame),
            ArithPlan::RationalWide => self.rational_binary_wide(arith, lhs, rhs, frame),
            ArithPlan::Generic => self.generic_numop(NumOp::of_arith(arith), lhs, rhs, frame),
        }
    }

    pub(super) fn divide(
        &mut self,
        ins: &Instruction,
        pc: usize,
        dst: u32,
        prim: Primitive,
        op: Op,
        lhs: u32,
        rhs: u32,
    ) -> Result<Flow, String> {
        let plan = self.arith_plan(pc);
        if matches!(plan, ArithPlan::Fixnum | ArithPlan::Wide) {
            let frame = self.frame(pc, pc)?;
            let (lhs, rhs) = (self.reg(lhs)?, self.reg(rhs)?);
            let (lhs, rhs) = if plan == ArithPlan::Wide {
                (self.wide_operand(lhs, frame), self.wide_operand(rhs, frame))
            } else {
                (
                    self.value_op(Op::CheckInt64, &[lhs], Type::Int64, Some(frame)),
                    self.value_op(Op::CheckInt64, &[rhs], Type::Int64, Some(frame)),
                )
            };
            let op = match op {
                Op::QuotI32 => Op::QuotI64,
                Op::RemI32 => Op::RemI64,
                _ => Op::ModI64,
            };
            let raw = self.value_op(op, &[lhs, rhs], Type::Int64, Some(frame));
            let value = self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, None);
            self.set(dst, value)?;
            return Ok(Flow::Next);
        }
        if !cfg!(target_arch = "aarch64") || plan != ArithPlan::Fixnum {
            return self.binary_prim(ins, pc, dst, prim, lhs, rhs);
        }
        let frame = self.frame(pc, pc)?;
        let (lhs, rhs) = (self.reg(lhs)?, self.reg(rhs)?);
        let lhs = self.value_op(Op::CheckInt32, &[lhs], Type::Int32, Some(frame));
        let rhs = self.value_op(Op::CheckInt32, &[rhs], Type::Int32, Some(frame));
        let raw = self.value_op(op, &[lhs, rhs], Type::Int32, Some(frame));
        let value = self.value_op(Op::Box(Repr::Int32), &[raw], Type::Any, None);
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// `dst = lhs cmp rhs` as a value.
    pub(super) fn compare_value(
        &mut self,
        pc: usize,
        dst: u32,
        cmp: Cmp,
        lhs: u32,
        rhs: u32,
    ) -> Result<Flow, String> {
        let flag = self.compare_flag(pc, cmp, lhs, rhs)?;
        let value = self.bool_value(flag);
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// A branch on `lhs cmp rhs`, taken when the comparison is `jump_when`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compare_branch(
        &mut self,
        ins: &Instruction,
        pc: usize,
        len: usize,
        cmp: Cmp,
        lhs: u32,
        rhs: u32,
        jump_when: bool,
    ) -> Result<Flow, String> {
        let flag = self.compare_flag(pc, cmp, lhs, rhs)?;
        self.cond_jump(ins, pc, len, flag, jump_when)
    }

    /// The flag of `lhs cmp rhs`, lowered by the plan of the site at `pc`.
    ///
    /// Flonum comparisons are ordered, so a NaN compares false, as the
    /// baseline's flonum fast path does.
    fn compare_flag(&mut self, pc: usize, cmp: Cmp, lhs: u32, rhs: u32) -> Result<Value, String> {
        let plan = self.arith_plan(pc);
        let frame = self.frame(pc, pc)?;
        let (lhs, rhs) = (self.reg(lhs)?, self.reg(rhs)?);
        Ok(match plan {
            ArithPlan::Fixnum => {
                let lhs = self.value_op(Op::CheckInt64, &[lhs], Type::Int64, Some(frame));
                let rhs = self.value_op(Op::CheckInt64, &[rhs], Type::Int64, Some(frame));
                self.value_op(Op::CmpI64(cmp), &[lhs, rhs], Type::Bool, None)
            }

            ArithPlan::Flonum => {
                let lhs = self.value_op(Op::CheckDouble, &[lhs], Type::Double, Some(frame));
                let rhs = self.value_op(Op::CheckDouble, &[rhs], Type::Double, Some(frame));
                self.value_op(Op::CmpF64(cmp), &[lhs, rhs], Type::Bool, None)
            }
            ArithPlan::Dual => self.dual_compare(cmp, lhs, rhs, frame)?,
            ArithPlan::Wide => {
                let lhs = self.wide_operand(lhs, frame);
                let rhs = self.wide_operand(rhs, frame);
                self.value_op(Op::CmpI64(cmp), &[lhs, rhs], Type::Bool, None)
            }
            ArithPlan::Rational => self.rational_compare(cmp, lhs, rhs, frame),
            ArithPlan::RationalWide => self.rational_compare_wide(cmp, lhs, rhs, frame),
            ArithPlan::Generic => {
                let helper = self.generic_numop(NumOp::of_cmp(cmp)?, lhs, rhs, frame);
                self.true_test(helper)
            }
        })
    }

    /// `dst = src + value` for an add-immediate.
    ///
    /// The immediate is a fixnum. A site whose source was seen only as a
    /// flonum adds it converted to a double, as the baseline's fast path does,
    /// and the conversion of an int32 is exact.
    pub(super) fn add_immediate(
        &mut self,
        pc: usize,
        src: u32,
        value: i32,
    ) -> Result<Value, String> {
        let word = if (self.blocked)(pc) {
            None
        } else {
            arith_word(self.buffer, pc)
        };
        let frame = self.frame(pc, pc)?;
        let src = self.reg(src)?;
        let plan = self.arith_plan(pc);
        if let Some(word) = word {
            let lhs_kind = word & NUMBER_KIND_MASK;
            let rhs_kind = (word >> ARITH_RHS_SHIFT) & NUMBER_KIND_MASK;
            if lhs_kind == NUMBER_FLONUM && rhs_kind == NUMBER_FIXNUM {
                let lhs = self.value_op(Op::CheckDouble, &[src], Type::Double, Some(frame));
                let imm = self.value_op(
                    Op::Const(Constant::Double((value as f64).to_bits())),
                    &[],
                    Type::Double,
                    None,
                );
                let raw = self.value_op(Op::AddF64, &[lhs, imm], Type::Double, None);
                return Ok(self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None));
            }
        }
        match plan {
            ArithPlan::Fixnum => {
                let lhs = self.value_op(Op::CheckInt64, &[src], Type::Int64, Some(frame));
                let imm = self.value_op(
                    Op::Const(Constant::Int64(value as i64)),
                    &[],
                    Type::Int64,
                    None,
                );
                let raw = self.value_op(Op::AddI64, &[lhs, imm], Type::Int64, None);
                Ok(self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, Some(frame)))
            }

            ArithPlan::Wide => {
                // A bignum operand plus a fixnum immediate: the sum can leave
                // i64, which deopts to the generic helper through baseline.
                let lhs = self.wide_operand(src, frame);
                let imm = self.value_op(
                    Op::Const(Constant::Int64(value as i64)),
                    &[],
                    Type::Int64,
                    None,
                );
                let raw = self.value_op(Op::AddI64Ovf, &[lhs, imm], Type::Int64, Some(frame));
                Ok(self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, None))
            }
            plan => {
                let imm = self.bits(SchemeValue::from_i32(value).bits());
                Ok(self.binary_value(plan, Arith::Add, src, imm, frame))
            }
        }
    }

    // The speculated paths, which deopt before the instruction.

    fn fixnum_binary(&mut self, arith: Arith, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let lhs = self.value_op(Op::CheckInt64, &[lhs], Type::Int64, Some(frame));
        let rhs = self.value_op(Op::CheckInt64, &[rhs], Type::Int64, Some(frame));
        let op = match arith {
            Arith::Add => Op::AddI64,
            Arith::Sub => Op::SubI64,
            Arith::Mul => Op::MulI64,
        };
        let raw = self.value_op(op, &[lhs, rhs], Type::Int64, None);
        return self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, Some(frame));
    }

    fn flonum_binary(&mut self, arith: Arith, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        // The check of each side deopts before the instruction, as the
        // fixnum path does, so the frame names the registers from before it.
        let lhs = self.value_op(Op::CheckDouble, &[lhs], Type::Double, Some(frame));
        let rhs = self.value_op(Op::CheckDouble, &[rhs], Type::Double, Some(frame));
        let op = match arith {
            Arith::Add => Op::AddF64,
            Arith::Sub => Op::SubF64,
            Arith::Mul => Op::MulF64,
        };
        let raw = self.value_op(op, &[lhs, rhs], Type::Double, None);
        self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None)
    }

    fn fixnum_negate(&mut self, src: Value, frame: FrameState) -> Value {
        let src = self.value_op(Op::CheckInt64, &[src], Type::Int64, Some(frame));
        let zero = self.value_op(Op::Const(Constant::Int64(0)), &[], Type::Int64, None);
        let raw = self.value_op(Op::SubI64, &[zero, src], Type::Int64, None);
        return self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, Some(frame));
    }

    /// Widens a Scheme value to an i64. A fixnum is sign-extended, a
    /// `BIGINT` of at most one limb is unboxed, and anything else (including
    /// a bignum that does not fit i64) deopts before the instruction.
    /// Widens an exact-integer operand to i64. `CheckBigIntFits64` accepts a
    /// fixnum's sign-extended payload and a one-limb bignum, so one guard
    /// serves both representations (and deopts for a wider bignum or a
    /// non-integer, which baseline handles).
    fn wide_operand(&mut self, v: Value, frame: FrameState) -> Value {
        self.value_op(Op::CheckBigIntFits64, &[v], Type::Int64, Some(frame))
    }

    /// Add, subtract or multiply for a site that saw exact integers including
    /// bignums. The operands are widened to i64; a result that leaves i64
    /// deopts to baseline, and `box.int64` promotes the rest (a fixnum or a
    /// one-limb bignum).
    fn wide_binary(&mut self, arith: Arith, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let lhs = self.wide_operand(lhs, frame);
        let rhs = self.wide_operand(rhs, frame);
        let op = match arith {
            Arith::Add => Op::AddI64Ovf,
            Arith::Sub => Op::SubI64Ovf,
            Arith::Mul => Op::MulI64Ovf,
        };
        let raw = self.value_op(op, &[lhs, rhs], Type::Int64, Some(frame));
        self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, None)
    }

    fn wide_negate(&mut self, src: Value, frame: FrameState) -> Value {
        let src = self.wide_operand(src, frame);
        let zero = self.value_op(Op::Const(Constant::Int64(0)), &[], Type::Int64, None);
        // `-i64::MIN` leaves i64, so it deopts to the generic helper.
        let raw = self.value_op(Op::SubI64Ovf, &[zero, src], Type::Int64, Some(frame));
        self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, None)
    }

    /// The flonum case multiplies by -1, which keeps the sign of zero.
    fn flonum_negate(&mut self, src: Value, frame: FrameState) -> Value {
        let src = self.value_op(Op::CheckDouble, &[src], Type::Double, Some(frame));
        let minus_one = self.value_op(
            Op::Const(Constant::Double((-1.0f64).to_bits())),
            &[],
            Type::Double,
            None,
        );
        let raw = self.value_op(Op::MulF64, &[src, minus_one], Type::Double, None);
        self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None)
    }

    // The dual paths. Each one branches; the join block takes the result.

    /// Ends the current block with a branch on `cond`.
    pub(super) fn branch_to(&mut self, cond: Value, then: Block, otherwise: Block) {
        let then = self.func.edge(then, &[]);
        let otherwise = self.func.edge(otherwise, &[]);
        self.terminate(Terminator::Branch {
            cond,
            then,
            otherwise,
        });
    }

    /// Ends the current block with a jump to `to`, passing `args`.
    pub(super) fn jump_with(&mut self, to: Block, args: &[Value]) {
        let edge = self.func.edge(to, args);
        self.terminate(Terminator::Jump(edge));
    }

    /// Continues in a block where the operand `v` is a double. A fixnum is
    /// converted exactly when `fixnums` is set, a flonum is loaded, and any
    /// other operand jumps to `generic`. The result is the parameter of the
    /// block the current code continues in.
    pub(super) fn operand_double(&mut self, v: Value, generic: Block, fixnums: bool) -> Value {
        let join = self.func.new_block();
        let result = self.func.append_param(join, Type::Double);
        let flonum = self.func.new_block();
        if fixnums {
            let is_fixnum = self.value_op(Op::TestInt32, &[v], Type::Bool, None);
            let fixnum = self.func.new_block();
            let next = self.func.new_block();
            self.branch_to(is_fixnum, fixnum, next);
            self.cur = fixnum;
            let raw = self.value_op(Op::Unbox(Repr::Int32), &[v], Type::Int32, None);
            let converted = self.value_op(Op::Int32ToDouble, &[raw], Type::Double, None);
            self.jump_with(join, &[converted]);
            self.cur = next;
        }
        let is_flonum = self.value_op(Op::TestDouble, &[v], Type::Bool, None);
        self.branch_to(is_flonum, flonum, generic);
        self.cur = flonum;
        let loaded = self.value_op(Op::Unbox(Repr::Double), &[v], Type::Double, None);
        self.jump_with(join, &[loaded]);
        self.cur = join;
        result
    }

    /// Branches to `generic` unless the double `s` lies in the int32 range.
    /// The current block continues in the in-range block.
    fn check_int32_range(&mut self, s: Value, generic: Block) {
        let lo = self.value_op(
            Op::Const(Constant::Double(INT32_MIN_F64.to_bits())),
            &[],
            Type::Double,
            None,
        );
        let hi = self.value_op(
            Op::Const(Constant::Double(INT32_MAX_F64.to_bits())),
            &[],
            Type::Double,
            None,
        );
        let not_below = self.value_op(Op::CmpF64(Cmp::Ge), &[s, lo], Type::Bool, None);
        let max_test = self.func.new_block();
        self.branch_to(not_below, max_test, generic);
        self.cur = max_test;
        let not_above = self.value_op(Op::CmpF64(Cmp::Le), &[s, hi], Type::Bool, None);
        let ok = self.func.new_block();
        self.branch_to(not_above, ok, generic);
        self.cur = ok;
    }

    /// The generic helper call for `op` on `lhs` and `rhs`, boxed.
    fn generic_numop(&mut self, op: NumOp, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let kind = self.bits(SchemeValue::from_i32(op as i32).bits());
        self.value_op(
            Op::CallOp(Helper::Arith.helper_ref()),
            &[kind, lhs, rhs],
            Type::Any,
            Some(frame),
        )
    }

    /// A Bool that is true when the boxed `value` is `#t`.
    fn true_test(&mut self, value: Value) -> Value {
        let true_value = self.bits(SchemeValue::VALUE_TRUE as u64);
        self.value_op(Op::EqBits, &[value, true_value], Type::Bool, None)
    }

    /// A join block with one parameter of type `ty`.
    pub(super) fn join_block(&mut self, ty: Type) -> (Block, Value) {
        let join = self.func.new_block();
        let param = self.func.append_param(join, ty);
        (join, param)
    }

    /// `lhs op rhs` for a site whose operands are fixnums or flonums.
    fn dual_binary(&mut self, arith: Arith, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let generic = self.func.new_block();
        let double = self.func.new_block();

        // Two fixnums: the int32 path. Its result stays in int32 when the
        // exact double of it does, which the range check tests, so the
        // overflowing pair takes the generic path instead of deopting.
        let lhs_int = self.is_int32_test(lhs);
        let int_rhs = self.func.new_block();
        self.branch_to(lhs_int, int_rhs, double);
        self.cur = int_rhs;
        let rhs_int = self.is_int32_test(rhs);
        let int_pair = self.func.new_block();
        self.branch_to(rhs_int, int_pair, double);
        self.cur = int_pair;
        let li = self.value_op(Op::Unbox(Repr::Int32), &[lhs], Type::Int32, None);
        let ri = self.value_op(Op::Unbox(Repr::Int32), &[rhs], Type::Int32, None);
        let la = self.value_op(Op::Int32ToDouble, &[li], Type::Double, None);
        let ra = self.value_op(Op::Int32ToDouble, &[ri], Type::Double, None);
        let (fop, iop) = match arith {
            Arith::Add => (Op::AddF64, Op::AddI32Ovf),
            Arith::Sub => (Op::SubF64, Op::SubI32Ovf),
            Arith::Mul => (Op::MulF64, Op::MulI32Ovf),
        };
        let exact = self.value_op(fop, &[la, ra], Type::Double, None);
        self.check_int32_range(exact, generic);
        let raw = self.value_op(iop, &[li, ri], Type::Int32, Some(frame));
        let boxed = self.value_op(Op::Box(Repr::Int32), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        // Otherwise, when each operand is a fixnum or a flonum, the double path.
        self.cur = double;
        let dl = self.operand_double(lhs, generic, true);
        let dr = self.operand_double(rhs, generic, true);
        let raw = self.value_op(fop, &[dl, dr], Type::Double, None);
        let boxed = self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = generic;
        let value = self.generic_numop(NumOp::of_arith(arith), lhs, rhs, frame);
        self.jump_with(join, &[value]);

        self.cur = join;
        result
    }

    pub(super) fn dual_div(&mut self, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let generic = self.func.new_block();
        let double = self.func.new_block();

        let lhs_int = self.is_int32_test(lhs);
        let int_rhs = self.func.new_block();
        self.branch_to(lhs_int, int_rhs, double);
        self.cur = int_rhs;
        let rhs_int = self.is_int32_test(rhs);
        let int_pair = self.func.new_block();
        self.branch_to(rhs_int, int_pair, double);
        self.cur = int_pair;
        let exact = {
            let li = self.value_op(Op::CheckInt64, &[lhs], Type::Int64, Some(frame));
            let ri = self.value_op(Op::CheckInt64, &[rhs], Type::Int64, Some(frame));
            self.reduce_to_value(li, ri, frame)
        };
        self.jump_with(join, &[exact]);

        self.cur = double;
        let dl = self.operand_double(lhs, generic, true);
        let dr = self.operand_double(rhs, generic, true);
        let raw = self.value_op(Op::DivF64, &[dl, dr], Type::Double, None);
        let boxed = self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = generic;
        let value = self.prim_value(Primitive::Div, &[lhs, rhs], frame);
        self.jump_with(join, &[value]);

        self.cur = join;
        result
    }

    fn dual_negate(&mut self, src: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let generic = self.func.new_block();
        let flonum = self.func.new_block();
        let not_int = self.func.new_block();
        let int_arm = self.func.new_block();

        let is_int = self.is_int32_test(src);
        self.branch_to(is_int, int_arm, not_int);
        self.cur = int_arm;
        let raw_int = self.value_op(Op::Unbox(Repr::Int32), &[src], Type::Int32, None);
        let as_double = self.value_op(Op::Int32ToDouble, &[raw_int], Type::Double, None);
        let minus_one = self.value_op(
            Op::Const(Constant::Double((-1.0f64).to_bits())),
            &[],
            Type::Double,
            None,
        );
        let exact = self.value_op(Op::MulF64, &[as_double, minus_one], Type::Double, None);
        self.check_int32_range(exact, generic);
        let zero = self.value_op(Op::Const(Constant::Int32(0)), &[], Type::Int32, None);
        let raw = self.value_op(Op::SubI32Ovf, &[zero, raw_int], Type::Int32, Some(frame));
        let boxed = self.value_op(Op::Box(Repr::Int32), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = not_int;
        let is_flo = self.value_op(Op::TestDouble, &[src], Type::Bool, None);
        self.branch_to(is_flo, flonum, generic);
        self.cur = flonum;
        let loaded = self.value_op(Op::Unbox(Repr::Double), &[src], Type::Double, None);
        let minus_one = self.value_op(
            Op::Const(Constant::Double((-1.0f64).to_bits())),
            &[],
            Type::Double,
            None,
        );
        let raw = self.value_op(Op::MulF64, &[loaded, minus_one], Type::Double, None);
        let boxed = self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = generic;
        let value = self.generic_numop(NumOp::Negate, src, src, frame);
        self.jump_with(join, &[value]);

        self.cur = join;
        result
    }

    fn dual_compare(
        &mut self,
        cmp: Cmp,
        lhs: Value,
        rhs: Value,
        frame: FrameState,
    ) -> Result<Value, String> {
        let (join, result) = self.join_block(Type::Bool);
        let generic = self.func.new_block();
        let double = self.func.new_block();

        let lhs_int = self.is_int32_test(lhs);
        let int_rhs = self.func.new_block();
        self.branch_to(lhs_int, int_rhs, double);
        self.cur = int_rhs;
        let rhs_int = self.is_int32_test(rhs);
        let int_pair = self.func.new_block();
        self.branch_to(rhs_int, int_pair, double);
        self.cur = int_pair;
        let li = self.value_op(Op::Unbox(Repr::Int32), &[lhs], Type::Int32, None);
        let ri = self.value_op(Op::Unbox(Repr::Int32), &[rhs], Type::Int32, None);
        let flag = self.value_op(Op::CmpI32(cmp), &[li, ri], Type::Bool, None);
        self.jump_with(join, &[flag]);

        self.cur = double;
        let dl = self.operand_double(lhs, generic, true);
        let dr = self.operand_double(rhs, generic, true);
        let flag = self.value_op(Op::CmpF64(cmp), &[dl, dr], Type::Bool, None);
        self.jump_with(join, &[flag]);

        self.cur = generic;
        let value = self.generic_numop(NumOp::of_cmp(cmp)?, lhs, rhs, frame);
        let flag = self.true_test(value);
        self.jump_with(join, &[flag]);

        self.cur = join;
        Ok(result)
    }

    /// Whether `v` is a fixnum, as a Bool.
    pub(super) fn is_int32_test(&mut self, v: Value) -> Value {
        self.value_op(Op::TestInt32, &[v], Type::Bool, None)
    }

    fn flonum_over_exact(&self, pc: usize) -> bool {
        use crate::runtime::interp::profile::{
            ARITH_RHS_SHIFT, NUMBER_BIGNUM, NUMBER_FIXNUM, NUMBER_FLONUM, NUMBER_KIND_MASK,
        };
        let Some(word) = arith_word(self.buffer, pc) else {
            return false;
        };
        let lhs = word & NUMBER_KIND_MASK;
        let rhs = (word >> ARITH_RHS_SHIFT) & NUMBER_KIND_MASK;
        lhs == NUMBER_FLONUM && rhs != 0 && rhs & !(NUMBER_FIXNUM | NUMBER_BIGNUM) == 0
    }

    fn flonum_div_exact(&mut self, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let generic = self.func.new_block();
        let fast = self.func.new_block();
        let dividend = self.value_op(Op::CheckDouble, &[lhs], Type::Double, Some(frame));
        let divisor = self.value_op(Op::CheckBigIntFits64, &[rhs], Type::Int64, Some(frame));
        let zero = self.value_op(Op::Const(Constant::Int64(0)), &[], Type::Int64, None);
        let is_zero = self.value_op(Op::CmpI64(Cmp::Eq), &[divisor, zero], Type::Bool, None);
        self.branch_to(is_zero, generic, fast);

        self.cur = fast;
        let as_double = self.value_op(Op::Int64ToDouble, &[divisor], Type::Double, None);
        let raw = self.value_op(Op::DivF64, &[dividend, as_double], Type::Double, None);
        let boxed = self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = generic;
        let value = self.prim_value(Primitive::Div, &[lhs, rhs], frame);
        self.jump_with(join, &[value]);

        self.cur = join;
        result
    }

    pub(super) fn divide_value(
        &mut self,
        ins: &Instruction,
        pc: usize,
        dst: u32,
        lhs: u32,
        rhs: u32,
    ) -> Result<Flow, String> {
        let plan = self.arith_plan(pc);

        let frame = self.frame(pc, pc)?;
        let (lhs_value, rhs_value) = (self.reg(lhs)?, self.reg(rhs)?);
        let value = match plan {
            ArithPlan::Fixnum => {
                if let Some(n) = self
                    .tagged_fixnum(lhs_value)
                    .filter(|n| matches!(n, 0 | 1 | -1))
                {
                    let rhs = self.value_op(Op::CheckInt64, &[rhs_value], Type::Int64, Some(frame));
                    self.reduce_known_numerator(n, rhs, true, frame)
                } else if let Some(d) = self
                    .tagged_fixnum(rhs_value)
                    .filter(|d| matches!(d, 1 | -1))
                {
                    let lhs = self.value_op(Op::CheckInt64, &[lhs_value], Type::Int64, Some(frame));
                    self.reduce_known_denominator(lhs, d)
                } else {
                    let lhs = self.value_op(Op::CheckInt64, &[lhs_value], Type::Int64, Some(frame));
                    let rhs = self.value_op(Op::CheckInt64, &[rhs_value], Type::Int64, Some(frame));
                    self.reduce_to_value(lhs, rhs, frame)
                }
            }
            ArithPlan::Flonum => {
                let lhs = self.value_op(Op::CheckDouble, &[lhs_value], Type::Double, Some(frame));
                let rhs = self.value_op(Op::CheckDouble, &[rhs_value], Type::Double, Some(frame));
                let raw = self.value_op(Op::DivF64, &[lhs, rhs], Type::Double, None);
                self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None)
            }
            ArithPlan::Rational => self.rational_div(lhs_value, rhs_value, frame),
            ArithPlan::RationalWide => self.rational_div_wide(lhs_value, rhs_value, frame),
            ArithPlan::Dual => self.dual_div(lhs_value, rhs_value, frame),
            _ if self.flonum_over_exact(pc) => self.flonum_div_exact(lhs_value, rhs_value, frame),
            _ => {
                return self.binary_prim(ins, pc, dst, Primitive::Div, lhs, rhs);
            }
        };
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    fn rational_binary(
        &mut self,
        arith: Arith,
        lhs: Value,
        rhs: Value,
        frame: FrameState,
    ) -> Value {
        let (n1, d1) = self.rational_operand(lhs, frame);
        let (n2, d2) = self.rational_operand(rhs, frame);
        let (nume, deno) = match arith {
            Arith::Add | Arith::Sub => {
                let lhs_prod = self.i64_op(Op::MulI64, n1, d2, None);
                let rhs_prod = self.i64_op(Op::MulI64, n2, d1, None);
                let op = if arith == Arith::Add {
                    Op::AddI64Ovf
                } else {
                    Op::SubI64Ovf
                };
                let nume = self.i64_op(op, lhs_prod, rhs_prod, Some(frame));
                let deno = self.i64_op(Op::MulI64, d1, d2, None);
                (nume, deno)
            }
            Arith::Mul => {
                let nume = self.i64_op(Op::MulI64, n1, n2, None);
                let deno = self.i64_op(Op::MulI64, d1, d2, None);
                (nume, deno)
            }
        };
        self.reduce_to_value(nume, deno, frame)
    }

    /// `lhs / rhs` for a rational site: `n1*d2 / d1*n2`, reduced. A zero
    /// divisor is `deno == 0` after the lift, so it deopts to baseline, which
    /// raises. A zero numerator is valid and reduces to `0`.
    fn rational_div(&mut self, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (n1, d1) = self.rational_operand(lhs, frame);
        let (n2, d2) = self.rational_operand(rhs, frame);
        let nume = self.i64_op(Op::MulI64, n1, d2, None);
        let deno = self.i64_op(Op::MulI64, d1, n2, None);
        self.reduce_to_value(nume, deno, frame)
    }

    /// `lhs cmp rhs` for a rational site. Denominators are positive, so
    /// `n1/d1` and `n2/d2` compare by `n1*d2` against `n2*d1`; both products
    /// are at most `2^62` in magnitude.
    fn rational_compare(&mut self, cmp: Cmp, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (n1, d1) = self.rational_operand(lhs, frame);
        let (n2, d2) = self.rational_operand(rhs, frame);
        let left = self.i64_op(Op::MulI64, n1, d2, None);
        let right = self.i64_op(Op::MulI64, n2, d1, None);
        self.value_op(Op::CmpI64(cmp), &[left, right], Type::Bool, None)
    }

    fn rational_binary_wide(
        &mut self,
        arith: Arith,
        lhs: Value,
        rhs: Value,
        frame: FrameState,
    ) -> Value {
        let (n1, d1) = self.rational_operand_wide(lhs, frame);
        let (n2, d2) = self.rational_operand_wide(rhs, frame);

        let (nume, deno) = match arith {
            Arith::Add | Arith::Sub => {
                let lhs_prod = self.mul128(n1, d2);
                let rhs_prod = self.mul128(n2, d1);
                let op = if arith == Arith::Add {
                    Op::Add128Ovf
                } else {
                    Op::Sub128Ovf
                };
                let nume = self.value_op(op, &[lhs_prod, rhs_prod], Type::Int128, Some(frame));
                let deno = self.mul128(d1, d2);
                (nume, deno)
            }
            Arith::Mul => (self.mul128(n1, n2), self.mul128(d1, d2)),
        };
        self.reduce_wide(nume, deno, frame)
    }

    fn rational_div_wide(&mut self, lhs: Value, rhs: Value, frame: FrameState) -> Value {
        let (n1, d1) = self.rational_operand_wide(lhs, frame);
        let (n2, d2) = self.rational_operand_wide(rhs, frame);
        let nume = self.mul128(n1, d2);
        let deno = self.mul128(d1, n2);
        self.reduce_wide(nume, deno, frame)
    }

    fn rational_compare_wide(
        &mut self,
        cmp: Cmp,
        lhs: Value,
        rhs: Value,
        frame: FrameState,
    ) -> Value {
        let (n1, d1) = self.rational_operand_wide(lhs, frame);
        let (n2, d2) = self.rational_operand_wide(rhs, frame);
        let left = self.mul128(n1, d2);
        let right = self.mul128(n2, d1);
        self.value_op(Op::Cmp128(cmp), &[left, right], Type::Bool, None)
    }

    fn reduce_wide(&mut self, nume: Value, deno: Value, frame: FrameState) -> Value {
        let zero = self.i128_zero();
        let zero_den = self.value_op(Op::Cmp128(Cmp::Eq), &[deno, zero], Type::Bool, None);
        self.deopt_if(zero_den, frame);
        let num_lo = self.value_op(Op::Int128Low, &[nume], Type::Int64, None);
        let num_hi = self.value_op(Op::Int128High, &[nume], Type::Int64, None);
        let den_lo = self.value_op(Op::Int128Low, &[deno], Type::Int64, None);
        let den_hi = self.value_op(Op::Int128High, &[deno], Type::Int64, None);
        self.value_op(
            Op::CallOp(Helper::ReduceRational128.helper_ref()),
            &[num_lo, num_hi, den_lo, den_hi],
            Type::Any,
            Some(frame),
        )
    }

    fn to_i128(&mut self, v: Value) -> Value {
        self.value_op(Op::ExtendI64To128, &[v], Type::Int128, None)
    }

    fn mul128(&mut self, a: Value, b: Value) -> Value {
        self.value_op(Op::Mul64x64To128, &[a, b], Type::Int128, None)
    }

    fn i128_zero(&mut self) -> Value {
        let zero = self.i64_const(0);
        self.to_i128(zero)
    }

    fn rational_operand_wide(&mut self, v: Value, frame: FrameState) -> (Value, Value) {
        let (join, num, den) = self.join_pair();
        let rational = self.func.new_block();
        let integer = self.func.new_block();

        let is_rational = self.value_op(Op::TestClass(ClassRef::RATIONAL), &[v], Type::Bool, None);
        self.branch_to(is_rational, rational, integer);

        self.cur = rational;
        let rational = self.value_op(
            Op::CheckClass(ClassRef::RATIONAL),
            &[v],
            Type::Heap(ClassRef::RATIONAL),
            Some(frame),
        );
        let numerator = self.value_op(
            Op::LoadRationalComponentWide { index: 0 },
            &[rational],
            Type::Int64,
            Some(frame),
        );
        let denominator = self.value_op(
            Op::LoadRationalComponentWide { index: 1 },
            &[rational],
            Type::Int64,
            Some(frame),
        );

        let zero = self.i64_const(0);
        let nonpositive =
            self.value_op(Op::CmpI64(Cmp::Le), &[denominator, zero], Type::Bool, None);
        let good = self.func.new_block();
        let bad = self.deopt_block(frame);
        self.branch_to(nonpositive, bad, good);
        self.cur = good;
        self.jump_with(join, &[numerator, denominator]);

        self.cur = integer;
        // A fixnum integer only. A `BIGINT` operand deopts (safe and slower);
        // baseline then computes the instruction, including its corrected
        // `BigInt - Rational` value.
        let numerator = self.value_op(Op::CheckInt64, &[v], Type::Int64, Some(frame));
        let one = self.i64_const(1);
        self.jump_with(join, &[numerator, one]);

        self.cur = join;
        (num, den)
    }

    fn rational_operand(&mut self, v: Value, frame: FrameState) -> (Value, Value) {
        let (join, num, den) = self.join_pair();
        let rational = self.func.new_block();
        let integer = self.func.new_block();

        let is_rational = self.value_op(Op::TestClass(ClassRef::RATIONAL), &[v], Type::Bool, None);
        self.branch_to(is_rational, rational, integer);

        self.cur = rational;
        let rational = self.value_op(
            Op::CheckClass(ClassRef::RATIONAL),
            &[v],
            Type::Heap(ClassRef::RATIONAL),
            Some(frame),
        );
        let numerator = self.value_op(
            Op::LoadRationalComponent { index: 0 },
            &[rational],
            Type::Int64,
            Some(frame),
        );
        let denominator = self.value_op(
            Op::LoadRationalComponent { index: 1 },
            &[rational],
            Type::Int64,
            Some(frame),
        );

        let zero = self.i64_const(0);
        let nonpositive =
            self.value_op(Op::CmpI64(Cmp::Le), &[denominator, zero], Type::Bool, None);
        let good = self.func.new_block();
        let bad = self.deopt_block(frame);
        self.branch_to(nonpositive, bad, good);
        self.cur = good;
        self.jump_with(join, &[numerator, denominator]);

        self.cur = integer;
        let numerator = self.value_op(Op::CheckInt64, &[v], Type::Int64, Some(frame));
        let one = self.i64_const(1);
        self.jump_with(join, &[numerator, one]);

        self.cur = join;
        (num, den)
    }

    fn reduce_known_numerator(
        &mut self,
        n: i32,
        deno: Value,
        deno_narrow: bool,
        frame: FrameState,
    ) -> Value {
        let zero = self.i64_const(0);
        let deno_zero = self.value_op(Op::CmpI64(Cmp::Eq), &[deno, zero], Type::Bool, None);
        self.deopt_if(deno_zero, frame);
        if n == 0 {
            return self.bits(SchemeValue::from_i32(0).bits());
        }
        if !deno_narrow {
            let min = self.i64_const(i64::MIN);
            let deno_min = self.value_op(Op::CmpI64(Cmp::Eq), &[deno, min], Type::Bool, None);
            self.deopt_if(deno_min, frame);
        }

        let negative = self.value_op(Op::CmpI64(Cmp::Lt), &[deno, zero], Type::Bool, None);
        let join = self.func.new_block();
        let den = self.func.append_param(join, Type::Int64);
        let num = self.func.append_param(join, Type::Any);
        let neg_block = self.func.new_block();
        let pos_block = self.func.new_block();
        self.branch_to(negative, neg_block, pos_block);

        self.cur = neg_block;
        let neg_den = self.i64_op(Op::SubI64, zero, deno, None);
        let neg_num = self.bits(SchemeValue::from_i32(-n).bits());
        self.jump_with(join, &[neg_den, neg_num]);

        self.cur = pos_block;
        let pos_num = self.bits(SchemeValue::from_i32(n).bits());
        self.jump_with(join, &[deno, pos_num]);

        self.cur = join;
        let one = self.i64_const(1);
        let den_one = self.value_op(Op::CmpI64(Cmp::Eq), &[den, one], Type::Bool, None);
        let (done, result) = self.join_block(Type::Any);
        let int_arm = self.func.new_block();
        let rat_arm = self.func.new_block();
        self.branch_to(den_one, int_arm, rat_arm);

        self.cur = int_arm;
        self.jump_with(done, &[num]);

        self.cur = rat_arm;
        let boxed_den = self.value_op(Op::Box(Repr::Int64), &[den], Type::Any, None);
        let rational = self.value_op(
            Op::AllocRational,
            &[num, boxed_den],
            Type::Rational,
            Some(frame),
        );
        self.jump_with(done, &[rational]);

        self.cur = done;
        result
    }

    fn reduce_known_denominator(&mut self, lhs: Value, d: i32) -> Value {
        let value = if d == 1 {
            lhs
        } else {
            let zero = self.i64_const(0);
            self.i64_op(Op::SubI64, zero, lhs, None)
        };
        self.value_op(Op::Box(Repr::Int64), &[value], Type::Any, None)
    }

    fn reduce_to_value(&mut self, nume: Value, deno: Value, frame: FrameState) -> Value {
        let zero = self.i64_const(0);
        let one = self.i64_const(1);
        let min = self.i64_const(i64::MIN);

        let nume_min = self.value_op(Op::CmpI64(Cmp::Eq), &[nume, min], Type::Bool, None);
        self.deopt_if(nume_min, frame);
        let deno_min = self.value_op(Op::CmpI64(Cmp::Eq), &[deno, min], Type::Bool, None);
        self.deopt_if(deno_min, frame);
        let deno_zero = self.value_op(Op::CmpI64(Cmp::Eq), &[deno, zero], Type::Bool, None);
        self.deopt_if(deno_zero, frame);

        let nume_neg = self.value_op(Op::CmpI64(Cmp::Lt), &[nume, zero], Type::Bool, None);
        let deno_neg = self.value_op(Op::CmpI64(Cmp::Lt), &[deno, zero], Type::Bool, None);
        let flip = self.value_op(Op::CmpI64(Cmp::Ne), &[nume_neg, deno_neg], Type::Bool, None);
        let nume_abs = self.select_negated(nume, nume_neg, zero);
        let deno_abs = self.select_negated(deno, deno_neg, zero);

        let gcd = self.gcd_i64(nume_abs, deno_abs, frame);
        let quotient_num = self.i64_op(Op::QuotI64, nume_abs, gcd, Some(frame));
        let quotient_den = self.i64_op(Op::QuotI64, deno_abs, gcd, Some(frame));
        let quotient_num = self.select_negated(quotient_num, flip, zero);

        // den == 1 collapses to an integer.
        let integer = self.value_op(Op::CmpI64(Cmp::Eq), &[quotient_den, one], Type::Bool, None);
        let (join, result) = self.join_block(Type::Any);
        let int_arm = self.func.new_block();
        let rat_arm = self.func.new_block();
        self.branch_to(integer, int_arm, rat_arm);

        self.cur = int_arm;
        let boxed = self.value_op(Op::Box(Repr::Int64), &[quotient_num], Type::Any, None);
        self.jump_with(join, &[boxed]);

        self.cur = rat_arm;
        let boxed_num = self.value_op(Op::Box(Repr::Int64), &[quotient_num], Type::Any, None);
        let boxed_den = self.value_op(Op::Box(Repr::Int64), &[quotient_den], Type::Any, None);
        let rational = self.value_op(
            Op::AllocRational,
            &[boxed_num, boxed_den],
            Type::Rational,
            Some(frame),
        );
        self.jump_with(join, &[rational]);

        self.cur = join;
        result
    }

    fn gcd_i64(&mut self, a: Value, b: Value, frame: FrameState) -> Value {
        let header = self.func.new_block();
        let p1 = self.func.append_param(header, Type::Int64);
        let p2 = self.func.append_param(header, Type::Int64);
        let body = self.func.new_block();
        let done = self.func.new_block();
        let result = self.func.append_param(done, Type::Int64);

        self.jump_with(header, &[a, b]);
        self.cur = header;
        let zero = self.i64_const(0);
        let is_zero = self.value_op(Op::CmpI64(Cmp::Eq), &[p2, zero], Type::Bool, None);
        let done_edge = self.func.edge(done, &[p1]);
        let body_edge = self.func.edge(body, &[]);
        self.terminate(Terminator::Branch {
            cond: is_zero,
            then: done_edge,
            otherwise: body_edge,
        });

        self.cur = body;
        let remainder = self.value_op(Op::RemI64, &[p1, p2], Type::Int64, Some(frame));
        self.jump_with(header, &[p2, remainder]);

        self.cur = done;
        result
    }

    /// `-v` when `cond` is true, otherwise `v`. `v` must not be `i64::MIN`.
    fn select_negated(&mut self, v: Value, cond: Value, zero: Value) -> Value {
        let (join, result) = self.join_block(Type::Int64);
        let negative = self.func.new_block();
        let positive = self.func.new_block();
        self.branch_to(cond, negative, positive);

        self.cur = negative;
        let negated = self.value_op(Op::SubI64, &[zero, v], Type::Int64, None);
        self.jump_with(join, &[negated]);

        self.cur = positive;
        self.jump_with(join, &[v]);

        self.cur = join;
        result
    }

    pub(super) fn deopt_if(&mut self, cond: Value, frame: FrameState) {
        let ok = self.func.new_block();
        let fail = self.deopt_block(frame);
        self.branch_to(cond, fail, ok);
        self.cur = ok;
    }

    pub(super) fn deopt_block(&mut self, frame: FrameState) -> Block {
        let block = self.func.new_block();
        let saved = self.cur;
        self.cur = block;
        self.terminate(Terminator::Deopt { frame });
        self.cur = saved;
        block
    }

    fn i64_op(&mut self, op: Op, lhs: Value, rhs: Value, frame: Option<FrameState>) -> Value {
        self.value_op(op, &[lhs, rhs], Type::Int64, frame)
    }

    fn i64_const(&mut self, n: i64) -> Value {
        self.value_op(Op::Const(Constant::Int64(n)), &[], Type::Int64, None)
    }

    fn tagged_fixnum(&self, value: Value) -> Option<i32> {
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return None;
        };
        match self.func.insts[inst].op {
            Op::Const(Constant::Bits(bits)) => SchemeValue::from_raw(bits).int32(),
            _ => None,
        }
    }

    fn join_pair(&mut self) -> (Block, Value, Value) {
        let join = self.func.new_block();
        let first = self.func.append_param(join, Type::Int64);
        let second = self.func.append_param(join, Type::Int64);
        (join, first, second)
    }
}
