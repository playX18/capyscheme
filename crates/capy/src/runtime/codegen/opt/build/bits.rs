//! Inline lowerings of the fixnum bitwise primitives, the flonum primitives,
//! `integer->char`, the multi-byte bytevector accessors and a few native
//! procedures (`bitwise-and`, `bytevector-u32-ref`, `fx+/wraparound`, ...).

use crate::heap::Gc;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{BitOp, BvElem, Cmp, Constant, Op, Repr, ShiftKind};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::proc::Closure;
use crate::runtime::objectmodel::{CodeBlock, Symbol, Value as SchemeValue};
use crate::runtime::prims::def::Primitive;

use super::profile::ArithPlan;
use super::profile::{self, CallShape};
use super::text::Text;
use super::{Arith, Builder};

/// Fixnums are 32-bit: a shift count is valid below this.
const FIXNUM_WIDTH: i32 = 32;

impl<'s, 'gc> Builder<'s, 'gc> {
    /// Whether the instruction being built may speculate. A pc whose last
    /// compile deopted too often keeps the plain call.
    pub(super) fn may_speculate(&self) -> bool {
        !(self.blocked)(self.pc)
    }

    /// The raw int32 of a fixnum operand, deopting on anything else.
    pub(super) fn fixnum_operand(&mut self, value: Value, frame: FrameState) -> Value {
        self.value_op(Op::CheckInt32, &[value], Type::Int32, Some(frame))
    }

    fn box_int32(&mut self, raw: Value) -> Value {
        self.value_op(Op::Box(Repr::Int32), &[raw], Type::Any, None)
    }

    fn box_int64(&mut self, raw: Value) -> Value {
        self.value_op(Op::Box(Repr::Int64), &[raw], Type::Any, None)
    }

    fn const_i64(&mut self, value: i64) -> Value {
        self.value_op(Op::Const(Constant::Int64(value)), &[], Type::Int64, None)
    }

    /// The flag `cond` as `#t`/`#f`, inverted when `negate` is set.
    fn flag_value(&mut self, cond: Value, negate: bool) -> Value {
        if !negate {
            return self.bool_value(cond);
        }
        let (join, result) = self.join_block(Type::Any);
        let yes = self.func.new_block();
        let no = self.func.new_block();
        self.branch_to(cond, yes, no);
        self.cur = yes;
        let f = self.bits(SchemeValue::VALUE_FALSE as u64);
        self.jump_with(join, &[f]);
        self.cur = no;
        let t = self.bits(SchemeValue::VALUE_TRUE as u64);
        self.jump_with(join, &[t]);
        self.cur = join;
        result
    }

    /// The inline lowering of primitive `prim` on `operands` from this
    /// module, or `None` to leave it to the caller.
    pub(super) fn inline_bits_prim(
        &mut self,
        prim: Primitive,
        operands: &[Value],
        frame: FrameState,
    ) -> Option<Value> {
        use Primitive as P;
        let speculate = self.may_speculate();
        Some(match (prim, operands) {
            // The fixnum operations deopt on a non-fixnum operand.
            (P::FxAnd, &[a, b]) if speculate => self.fx_bitwise(BitOp::And, a, b, frame),
            (P::FxIor, &[a, b]) if speculate => self.fx_bitwise(BitOp::Or, a, b, frame),
            (P::FxXor, &[a, b]) if speculate => self.fx_bitwise(BitOp::Xor, a, b, frame),
            (P::FxNot, &[a]) if speculate => {
                let a = self.fixnum_operand(a, frame);
                let ones = self.const_i32(-1);
                let raw = self.value_op(Op::BitI32(BitOp::Xor), &[a, ones], Type::Int32, None);
                self.box_int32(raw)
            }
            (P::FxAshl, &[a, n]) if speculate => match self.fixnum_constant(n) {
                Some(n) => {
                    // An out-of-range constant count raises: leave the call.
                    let n = Some(n).filter(|n| (0..FIXNUM_WIDTH).contains(n))?;
                    let a = self.fixnum_operand(a, frame);
                    let raw = self.value_op(Op::ShlI32Ovf(n as u8), &[a], Type::Int32, Some(frame));
                    self.box_int32(raw)
                }
                None => {
                    // A count outside `0..32` deopts with the overflow, and
                    // baseline raises.
                    let a = self.fixnum_operand(a, frame);
                    let n = self.fixnum_operand(n, frame);
                    let raw = self.value_op(Op::ShlI32OvfVar, &[a, n], Type::Int32, Some(frame));
                    self.box_int32(raw)
                }
            },
            (P::FxAshr, &[a, n]) if speculate && self.fixnum_constant(n).is_none() => {
                // The machine's 32-bit shift reads the low five bits of the
                // count, as the primitive's `wrapping_shr` does.
                let a = self.fixnum_operand(a, frame);
                let n = self.fixnum_operand(n, frame);
                let raw =
                    self.value_op(Op::ShiftI32Var(ShiftKind::Sar), &[a, n], Type::Int32, None);
                self.box_int32(raw)
            }
            (P::FxAshr, &[a, n]) if speculate => {
                // The primitive masks the count to the word: `wrapping_shr`.
                let n = self.fixnum_constant(n)? as u32 & 31;
                let a = self.fixnum_operand(a, frame);
                if n == 0 {
                    return Some(self.box_int32(a));
                }
                let raw = self.value_op(
                    Op::ShiftI32(ShiftKind::Sar, n as u8),
                    &[a],
                    Type::Int32,
                    None,
                );
                self.box_int32(raw)
            }
            (P::FxMin, &[a, b]) if speculate => self.fx_select(Cmp::Le, a, b, frame),
            (P::FxMax, &[a, b]) if speculate => self.fx_select(Cmp::Ge, a, b, frame),
            (P::FxOdd, &[a]) if speculate => self.fx_parity(a, true, frame),
            (P::FxEven, &[a]) if speculate => self.fx_parity(a, false, frame),
            // The generic integer operations accept bignums too, so only a
            // fixnum takes the inline path.
            (P::Logand, &[a, b]) => self.int_bitwise(prim, BitOp::And, a, b, frame),
            (P::Logior, &[a, b]) => self.int_bitwise(prim, BitOp::Or, a, b, frame),
            (P::Lognot, &[a]) => self.int_lognot(a, frame),
            (P::Ash, &[a, n]) => {
                let n = self.fixnum_constant(n)?;
                self.int_ash(prim, a, n, frame)?
            }
            // The flonum operations deopt on a non-flonum operand.
            (P::FlAdd, &[a, b]) if speculate => self.fl_arith(Op::AddF64, a, b, frame),
            (P::FlSub, &[a, b]) if speculate => self.fl_arith(Op::SubF64, a, b, frame),
            (P::FlMul, &[a, b]) if speculate => self.fl_arith(Op::MulF64, a, b, frame),
            (P::FlDiv, &[a, b]) if speculate => self.fl_arith(Op::DivF64, a, b, frame),
            (P::FlLt, &[a, b]) if speculate => self.fl_compare(Cmp::Lt, a, b, frame),
            (P::FlLe, &[a, b]) if speculate => self.fl_compare(Cmp::Le, a, b, frame),
            (P::FlGt, &[a, b]) if speculate => self.fl_compare(Cmp::Gt, a, b, frame),
            (P::FlGe, &[a, b]) if speculate => self.fl_compare(Cmp::Ge, a, b, frame),
            (P::FlEq, &[a, b]) if speculate => self.fl_compare(Cmp::Eq, a, b, frame),
            (P::FlZero, &[a]) if speculate => self.fl_sign(Cmp::Eq, a, frame),
            (P::FlPositive, &[a]) if speculate => self.fl_sign(Cmp::Gt, a, frame),
            (P::FlNegative, &[a]) if speculate => self.fl_sign(Cmp::Lt, a, frame),
            (P::FlNan, &[a]) if speculate => {
                // NaN is the only value unequal to itself.
                let x = self.value_op(Op::CheckDouble, &[a], Type::Double, Some(frame));
                let same = self.value_op(Op::CmpF64(Cmp::Eq), &[x, x], Type::Bool, None);
                self.flag_value(same, true)
            }
            (P::FlFloor, &[a]) if speculate => self.fl_unary(Op::FloorF64, a, frame),
            (P::FlCeiling, &[a]) if speculate => self.fl_unary(Op::CeilingF64, a, frame),
            (P::FlTruncate, &[a]) if speculate => self.fl_unary(Op::TruncateF64, a, frame),
            (P::FlSqrt, &[a]) if speculate => self.fl_unary(Op::SqrtF64, a, frame),
            (P::FlAbs, &[a]) if speculate => self.fl_unary(Op::AbsF64, a, frame),
            // AArch64 `fmin`/`fmax` match the primitives exactly: NaN wins
            // and -0.0 is below +0.0. x86 `minsd`/`maxsd` do not, so x86-64
            // keeps the primitive call.
            (P::FlMin, &[a, b]) if speculate && cfg!(target_arch = "aarch64") => {
                self.fl_arith(Op::MinF64, a, b, frame)
            }
            (P::FlMax, &[a, b]) if speculate && cfg!(target_arch = "aarch64") => {
                self.fl_arith(Op::MaxF64, a, b, frame)
            }
            (P::IsBytevector, &[x]) => self.bytevector_test(x),
            (P::IntegerToChar, &[x]) => self.integer_to_char(x, frame),
            // The native-endian accessors. Integer elements need no
            // alignment, only `index..index + size` inside the bytevector.
            (P::BvS8Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(1, true, false), bv, index, frame)
            }
            (P::BvU16Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(2, false, false), bv, index, frame)
            }
            (P::BvS16Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(2, true, false), bv, index, frame)
            }
            (P::BvU32Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(4, false, false), bv, index, frame)
            }
            (P::BvS32Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(4, true, false), bv, index, frame)
            }
            (P::BvU64Ref, &[bv, index]) if speculate => {
                self.bytevector_load(elem(8, false, false), bv, index, frame)
            }
            (P::BvU64Set, &[bv, index, value]) if speculate => {
                self.bytevector_store(elem(8, false, false), bv, index, value, frame)
            }
            _ => return None,
        })
    }

    fn fx_bitwise(&mut self, op: BitOp, a: Value, b: Value, frame: FrameState) -> Value {
        let a = self.fixnum_operand(a, frame);
        let b = self.fixnum_operand(b, frame);
        let raw = self.value_op(Op::BitI32(op), &[a, b], Type::Int32, None);
        self.box_int32(raw)
    }

    /// `fxmin` (`cmp` is `Le`) or `fxmax` (`Ge`): `a` when `a cmp b`, else `b`.
    fn fx_select(&mut self, cmp: Cmp, a: Value, b: Value, frame: FrameState) -> Value {
        let ra = self.fixnum_operand(a, frame);
        let rb = self.fixnum_operand(b, frame);
        let (join, result) = self.join_block(Type::Int32);
        let take_a = self.func.new_block();
        let take_b = self.func.new_block();
        let test = self.value_op(Op::CmpI32(cmp), &[ra, rb], Type::Bool, None);
        self.branch_to(test, take_a, take_b);
        self.cur = take_a;
        self.jump_with(join, &[ra]);
        self.cur = take_b;
        self.jump_with(join, &[rb]);
        self.cur = join;
        self.box_int32(result)
    }

    fn fx_parity(&mut self, a: Value, odd: bool, frame: FrameState) -> Value {
        let a = self.fixnum_operand(a, frame);
        let one = self.const_i32(1);
        let low = self.value_op(Op::BitI32(BitOp::And), &[a, one], Type::Int32, None);
        let test = self.value_op(Op::CmpI32(Cmp::Eq), &[low, one], Type::Bool, None);
        self.flag_value(test, !odd)
    }

    /// `logand`/`logior` on two fixnums inline; the primitive otherwise.
    fn int_bitwise(
        &mut self,
        prim: Primitive,
        op: BitOp,
        a: Value,
        b: Value,
        frame: FrameState,
    ) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let test_b = self.func.new_block();
        let fast = self.func.new_block();
        let slow = self.func.new_block();
        let a_ok = self.value_op(Op::TestInt32, &[a], Type::Bool, None);
        self.branch_to(a_ok, test_b, slow);
        self.cur = test_b;
        let b_ok = self.value_op(Op::TestInt32, &[b], Type::Bool, None);
        self.branch_to(b_ok, fast, slow);
        self.cur = fast;
        let ra = self.value_op(Op::Unbox(Repr::Int32), &[a], Type::Int32, None);
        let rb = self.value_op(Op::Unbox(Repr::Int32), &[b], Type::Int32, None);
        let raw = self.value_op(Op::BitI32(op), &[ra, rb], Type::Int32, None);
        let boxed = self.box_int32(raw);
        self.jump_with(join, &[boxed]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[a, b], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    fn int_lognot(&mut self, a: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let fast = self.func.new_block();
        let slow = self.func.new_block();
        let ok = self.value_op(Op::TestInt32, &[a], Type::Bool, None);
        self.branch_to(ok, fast, slow);
        self.cur = fast;
        let ra = self.value_op(Op::Unbox(Repr::Int32), &[a], Type::Int32, None);
        let ones = self.const_i32(-1);
        let raw = self.value_op(Op::BitI32(BitOp::Xor), &[ra, ones], Type::Int32, None);
        let boxed = self.box_int32(raw);
        self.jump_with(join, &[boxed]);
        self.cur = slow;
        let fallback = self.prim_value(Primitive::Lognot, &[a], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// `ash` of a fixnum by the constant `n`. A left shift of a fixnum by
    /// less than 32 fits an i64, which boxes to a fixnum or a bignum exactly
    /// as the primitive's result.
    fn int_ash(&mut self, prim: Primitive, a: Value, n: i32, frame: FrameState) -> Option<Value> {
        if n >= FIXNUM_WIDTH {
            return None;
        }
        let (join, result) = self.join_block(Type::Any);
        let fast = self.func.new_block();
        let slow = self.func.new_block();
        let ok = self.value_op(Op::TestInt32, &[a], Type::Bool, None);
        self.branch_to(ok, fast, slow);
        self.cur = fast;
        let boxed = if n == 0 {
            a
        } else if n > 0 {
            let wide = self.value_op(Op::Unbox(Repr::Int64), &[a], Type::Int64, None);
            let raw = self.value_op(
                Op::ShiftI64(ShiftKind::Shl, n as u8),
                &[wide],
                Type::Int64,
                None,
            );
            self.box_int64(raw)
        } else {
            // A right shift by 32 or more leaves only the sign.
            let count = (-n).min(31) as u8;
            let raw = self.value_op(Op::Unbox(Repr::Int32), &[a], Type::Int32, None);
            let shifted = self.value_op(
                Op::ShiftI32(ShiftKind::Sar, count),
                &[raw],
                Type::Int32,
                None,
            );
            self.box_int32(shifted)
        };
        self.jump_with(join, &[boxed]);
        self.cur = slow;
        let count = self.bits(SchemeValue::from_i32(n).bits());
        let fallback = self.prim_value(prim, &[a, count], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        Some(result)
    }

    fn fl_operand(&mut self, value: Value, frame: FrameState) -> Value {
        self.value_op(Op::CheckDouble, &[value], Type::Double, Some(frame))
    }

    fn fl_arith(&mut self, op: Op, a: Value, b: Value, frame: FrameState) -> Value {
        let a = self.fl_operand(a, frame);
        let b = self.fl_operand(b, frame);
        let raw = self.value_op(op, &[a, b], Type::Double, None);
        self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None)
    }

    fn fl_unary(&mut self, op: Op, a: Value, frame: FrameState) -> Value {
        let a = self.fl_operand(a, frame);
        let raw = self.value_op(op, &[a], Type::Double, None);
        self.value_op(Op::Box(Repr::Double), &[raw], Type::Any, None)
    }

    fn fl_compare(&mut self, cmp: Cmp, a: Value, b: Value, frame: FrameState) -> Value {
        let a = self.fl_operand(a, frame);
        let b = self.fl_operand(b, frame);
        let test = self.value_op(Op::CmpF64(cmp), &[a, b], Type::Bool, None);
        self.bool_value(test)
    }

    fn fl_sign(&mut self, cmp: Cmp, a: Value, frame: FrameState) -> Value {
        let a = self.fl_operand(a, frame);
        let zero = self.value_op(
            Op::Const(Constant::Double(0f64.to_bits())),
            &[],
            Type::Double,
            None,
        );
        let test = self.value_op(Op::CmpF64(cmp), &[a, zero], Type::Bool, None);
        self.bool_value(test)
    }

    /// `bytevector?`: either bytevector class.
    fn bytevector_test(&mut self, x: Value) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let yes = self.func.new_block();
        let other = self.func.new_block();
        let no = self.func.new_block();
        let mutable = self.value_op(
            Op::TestClass(ClassRef::MUTABLE_BYTEVECTOR),
            &[x],
            Type::Bool,
            None,
        );
        self.branch_to(mutable, yes, other);
        self.cur = other;
        let immutable = self.value_op(
            Op::TestClass(ClassRef::IMMUTABLE_BYTEVECTOR),
            &[x],
            Type::Bool,
            None,
        );
        self.branch_to(immutable, yes, no);
        self.cur = yes;
        let t = self.bits(SchemeValue::VALUE_TRUE as u64);
        self.jump_with(join, &[t]);
        self.cur = no;
        let f = self.bits(SchemeValue::VALUE_FALSE as u64);
        self.jump_with(join, &[f]);
        self.cur = join;
        result
    }

    /// `integer->char`: a fixnum that is a Unicode scalar value (below
    /// `#x110000` and outside the surrogates) tags inline, as the baseline
    /// template does; anything else calls the primitive, which raises.
    fn integer_to_char(&mut self, x: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let slow = self.func.new_block();
        let fixnum = self.func.new_block();
        let below_max = self.func.new_block();
        let below_surrogates = self.func.new_block();
        let check_high = self.func.new_block();
        let tag = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, fixnum, slow);

        self.cur = fixnum;
        let code = self.value_op(Op::Unbox(Repr::Int32), &[x], Type::Int32, None);
        let zero = self.const_i32(0);
        let negative = self.value_op(Op::CmpI32(Cmp::Lt), &[code, zero], Type::Bool, None);
        self.branch_to(negative, slow, below_max);

        self.cur = below_max;
        let max = self.const_i32(0x10FFFF);
        let too_big = self.value_op(Op::CmpI32(Cmp::Gt), &[code, max], Type::Bool, None);
        self.branch_to(too_big, slow, below_surrogates);

        self.cur = below_surrogates;
        let low = self.const_i32(0xD800);
        let below = self.value_op(Op::CmpI32(Cmp::Lt), &[code, low], Type::Bool, None);
        self.branch_to(below, tag, check_high);

        self.cur = check_high;
        let high = self.const_i32(0xDFFF);
        let above = self.value_op(Op::CmpI32(Cmp::Gt), &[code, high], Type::Bool, None);
        self.branch_to(above, tag, slow);

        self.cur = tag;
        let ch = self.value_op(Op::CharFromCode, &[code], Type::Any, None);
        self.jump_with(join, &[ch]);

        self.cur = slow;
        let fallback = self.prim_value(Primitive::IntegerToChar, &[x], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// Reads the integer element `elem` at fixnum byte offset `index` of the
    /// bytevector `bv`, boxed. A non-bytevector, a non-fixnum offset or an
    /// element past the end deopts.
    fn bytevector_load(
        &mut self,
        elem: BvElem,
        bv: Value,
        index: Value,
        frame: FrameState,
    ) -> Value {
        let bv = self.check_bytevector(bv, frame);
        let raw = self.value_op(Op::CheckInt64, &[index], Type::Int64, Some(frame));
        // `0 <= index < length`, then `index + size - 1 < length`. The
        // offset is a fixnum, so the add cannot wrap.
        let first = self.value_op(Op::CheckIndexRaw, &[bv, raw], Type::Int32, Some(frame));
        if elem.bytes > 1 {
            let extra = self.const_i64(i64::from(elem.bytes) - 1);
            let last = self.value_op(Op::AddI64, &[raw, extra], Type::Int64, None);
            self.value_op(Op::CheckIndexRaw, &[bv, last], Type::Int32, Some(frame));
        }
        let ty = elem.result_type();
        let loaded = self.value_op(Op::LoadBytevector(elem), &[bv, first], ty, None);
        if elem.bytes == 8 && !elem.signed {
            self.box_u64(loaded)
        } else if ty == Type::Int64 {
            self.box_int64(loaded)
        } else {
            self.box_int32(loaded)
        }
    }

    /// Writes `value`, an integer in `0..2^64`, as the word `elem` at fixnum
    /// byte offset `index` of the bytevector `bv`. A wrong operand deopts.
    fn bytevector_store(
        &mut self,
        elem: BvElem,
        bv: Value,
        index: Value,
        value: Value,
        frame: FrameState,
    ) -> Value {
        let bv = self.check_bytevector(bv, frame);
        let raw = self.value_op(Op::CheckInt64, &[index], Type::Int64, Some(frame));
        let first = self.value_op(Op::CheckIndexRaw, &[bv, raw], Type::Int32, Some(frame));
        let extra = self.const_i64(i64::from(elem.bytes) - 1);
        let last = self.value_op(Op::AddI64, &[raw, extra], Type::Int64, None);
        self.value_op(Op::CheckIndexRaw, &[bv, last], Type::Int32, Some(frame));
        let word = self.u64_operand(value, frame);
        self.effect_op(Op::StoreBytevector(elem), &[bv, first, word], None);
        self.undef()
    }

    /// Whether the raw i64 `raw` is non-negative by construction, so its
    /// signed and unsigned readings agree.
    fn known_nonnegative(&self, raw: Value) -> bool {
        let ValueDef::Inst(def) = self.func.value_def(raw) else {
            return false;
        };
        match self.func.insts[def].op {
            Op::Const(Constant::Int64(n)) => n >= 0,
            Op::ShiftI64(ShiftKind::Shr, n) => n > 0,
            Op::BitI64(BitOp::And) => {
                let args = self.func.args(def);
                self.known_nonnegative(args[0]) || self.known_nonnegative(args[1])
            }
            Op::LoadBytevector(elem) => !elem.signed && elem.bytes < 8,
            _ => false,
        }
    }

    /// The word of a u64-lane value, or `None` when `v` is not one.
    fn u64_word(&self, v: Value) -> Option<Value> {
        let ValueDef::Inst(def) = self.func.value_def(v) else {
            return None;
        };
        let raw = *self.func.args(def).first()?;
        match self.func.insts[def].op {
            Op::BoxU64 => Some(raw),
            Op::Box(Repr::Int64) if self.known_nonnegative(raw) => Some(raw),
            _ => None,
        }
    }

    /// Whether `shape` applied to `args` needs no operand guard: a u64-lane
    /// operation on words and constants.
    fn guard_free(&self, shape: &Native, args: &[Value]) -> bool {
        let word = |v: Value| self.u64_word(v).is_some() || self.u64_constant(v).is_some();
        match shape {
            Native::U64Bit(_) | Native::U64Arith(_) => args.iter().all(|&a| word(a)),
            Native::U64Shift(..) | Native::U64ToFlonum => word(args[0]),
            _ => false,
        }
    }

    /// Whether `v` is a `box.u64`.
    fn is_u64_box(&self, v: Value) -> bool {
        matches!(self.func.value_def(v), ValueDef::Inst(def) if self.func.insts[def].op == Op::BoxU64)
    }

    /// The u64 constant `v` holds, if it is an exact integer in `0..2^64`.
    fn u64_constant(&self, v: Value) -> Option<u64> {
        let value = self.constant_value(v)?;
        if let Some(n) = value.int32() {
            return u64::try_from(n).ok();
        }
        if value.is::<crate::runtime::objectmodel::number::BigInt>() {
            return value
                .downcast::<crate::runtime::objectmodel::number::BigInt>()
                .try_as_u64();
        }
        None
    }

    /// The raw word of the u64 operand `v`. A word or a constant is used
    /// directly; anything else is guarded to be an integer in `0..2^64`.
    pub(super) fn u64_operand(&mut self, v: Value, frame: FrameState) -> Value {
        if let Some(word) = self.u64_word(v) {
            return word;
        }
        if let Some(n) = self.u64_constant(v) {
            return self.const_i64(n as i64);
        }
        self.value_op(Op::CheckU64, &[v], Type::Int64, Some(frame))
    }

    /// Boxes the word `raw` as an unsigned integer. A word known to be
    /// non-negative boxes as an int64, which the other lanes read directly.
    fn box_u64(&mut self, raw: Value) -> Value {
        if self.known_nonnegative(raw) {
            self.box_int64(raw)
        } else {
            self.value_op(Op::BoxU64, &[raw], Type::Any, None)
        }
    }

    /// The flonum nearest the word `raw`, read as unsigned.
    fn u64_to_flonum(&mut self, raw: Value) -> Value {
        if self.known_nonnegative(raw) {
            let d = self.value_op(Op::Int64ToDouble, &[raw], Type::Double, None);
            return self.value_op(Op::Box(Repr::Double), &[d], Type::Any, None);
        }
        // A word at or above 2^63 halves with its low bit kept as a sticky
        // bit, converts, and doubles: the rounding matches the direct one.
        let (join, result) = self.join_block(Type::Double);
        let small = self.func.new_block();
        let large = self.func.new_block();
        let zero = self.const_i64(0);
        let nonnegative = self.value_op(Op::CmpI64(Cmp::Ge), &[raw, zero], Type::Bool, None);
        self.branch_to(nonnegative, small, large);
        self.cur = small;
        let d = self.value_op(Op::Int64ToDouble, &[raw], Type::Double, None);
        self.jump_with(join, &[d]);
        self.cur = large;
        let half = self.value_op(Op::ShiftI64(ShiftKind::Shr, 1), &[raw], Type::Int64, None);
        let one = self.const_i64(1);
        let low = self.value_op(Op::BitI64(BitOp::And), &[raw, one], Type::Int64, None);
        let sticky = self.value_op(Op::BitI64(BitOp::Or), &[half, low], Type::Int64, None);
        let d = self.value_op(Op::Int64ToDouble, &[sticky], Type::Double, None);
        let doubled = self.value_op(Op::AddF64, &[d, d], Type::Double, None);
        self.jump_with(join, &[doubled]);
        self.cur = join;
        self.value_op(Op::Box(Repr::Double), &[result], Type::Any, None)
    }

    /// The native procedure behind a callee, from a folded constant closure
    /// or the site's monomorphic profile, with its name.
    fn native_of_callee(
        &self,
        callee: Value,
        pc: usize,
    ) -> Option<(String, Gc<'gc, CodeBlock<'gc>>)> {
        let block = if let Some(closure) = self.constant_closure(callee) {
            Closure::code_block(closure.downcast::<Closure>())
        } else if let Some(CallShape::Mono(Some(target))) = profile::call_shape(self.buffer, pc) {
            target.callee
        } else {
            return None;
        };
        if !block.is_native() {
            return None;
        }
        let name = crate::runtime::objectmodel::proc::procedure_name(block)?;
        Some((name, block))
    }

    /// The `big`/`little` endianness a constant symbol operand names.
    fn constant_endianness(&self, value: Value) -> Option<bool> {
        let value = self.constant_value(value)?;
        if !value.is::<Symbol>() {
            return None;
        }
        let name = value.downcast::<Symbol>().to_string();
        match name.as_str() {
            "big" => Some(true),
            "little" => Some(false),
            _ => None,
        }
    }

    /// The inline value of a call to a recognized native procedure, guarded
    /// by the callee's code block (a constant callee needs no guard).
    /// `None` leaves the call in place.
    pub(super) fn native_value(
        &mut self,
        pc: usize,
        callee: Value,
        first_arg: u32,
        argc: u32,
    ) -> Result<Option<Value>, String> {
        let Some((name, block)) = self.native_of_callee(callee, pc) else {
            return Ok(None);
        };
        let mut args = Vec::with_capacity(argc as usize);
        for index in 0..argc {
            args.push(self.reg(first_arg + index)?);
        }
        let Some(shape) = self.native_shape(&name, &args) else {
            return Ok(None);
        };
        let frame = self.frame(pc, pc)?;
        // A folded constant is its own code identity.
        if self.constant_closure(callee).is_none() {
            let code = self.code_ref_value(SchemeValue::from(block));
            self.value_op(
                Op::CheckCode(code),
                &[callee],
                Type::Closure(Some(code)),
                Some(frame),
            );
        }
        Ok(Some(self.native_lower(shape, &args, frame)?))
    }

    /// The inline shape of a polymorphic arm: the native behind `block`
    /// called with `args`, or `None` when it has no lowering that the arm's
    /// code test alone makes safe.
    pub(super) fn native_arm_shape(
        &mut self,
        block: Gc<'gc, CodeBlock<'gc>>,
        args: &[Value],
    ) -> Option<Native> {
        if !block.is_native() {
            return None;
        }
        let name = crate::runtime::objectmodel::proc::procedure_name(block)?;
        match self.native_shape(&name, args)? {
            Native::Text(_) => None,
            shape => Some(shape),
        }
    }

    /// Decides the inline shape of a call to the native `name` before
    /// emitting anything, so a refused call leaves no guard behind.
    pub(super) fn native_shape(&mut self, name: &str, args: &[Value]) -> Option<Native> {
        // A pc that deopted too often still takes a u64-lane operation whose
        // operands are all words or constants: it has no operand guard left
        // to fail, only the callee check. Typically the pc was blocked by an
        // earlier compile that sent a word at or above 2^63 down the signed
        // lane.
        let blocked = !self.may_speculate();
        let u64_count = |builder: &Self, n: Value| {
            builder
                .fixnum_constant(n)
                .filter(|n| (0..64).contains(n))
                .map(|n| n as u8)
        };
        // Only a word that may be at or above 2^63 switches lanes: a
        // non-negative int64 already works in the signed lane, which also
        // takes a negative mask.
        let lane = args.iter().any(|&a| self.is_u64_box(a));
        let shape = match (name, args) {
            ("bitwise-and", &[_, _]) if lane => Native::U64Bit(BitOp::And),
            ("bitwise-ior", &[_, _]) if lane => Native::U64Bit(BitOp::Or),
            ("bitwise-xor", &[_, _]) if lane => Native::U64Bit(BitOp::Xor),
            ("bitwise-and", &[_, _]) => Native::Bit(BitOp::And),
            ("bitwise-ior", &[_, _]) => Native::Bit(BitOp::Or),
            ("bitwise-xor", &[_, _]) => Native::Bit(BitOp::Xor),
            ("u64+/wraparound", &[_, _]) => Native::U64Arith(Op::AddI64),
            ("u64-/wraparound", &[_, _]) => Native::U64Arith(Op::SubI64),
            ("u64*/wraparound", &[_, _]) => Native::U64Arith(Op::MulI64),
            ("u64sll/wraparound", &[_, n]) => {
                let n = u64_count(self, n)?;
                Native::U64Shift(ShiftKind::Shl, n)
            },
            ("u64srl", &[_, n]) => {
                let n = u64_count(self, n)?;
                Native::U64Shift(ShiftKind::Shr, n)
            },
            ("u64rotr", &[_, n]) => {
                let n = u64_count(self, n)?;
                Native::U64Shift(ShiftKind::Rotr, n)
            },
            ("u64rotl", &[_, n]) => {
                let n = u64_count(self, n)?;
                Native::U64Shift(ShiftKind::Rotr, (64 - n) % 64)
            },
            ("u64->flonum", &[_]) => Native::U64ToFlonum,
            // A right shift of a non-negative word is the logical one.
            ("bitwise-arithmetic-shift-right", &[_, n]) if lane => match self.fixnum_constant(n) {
                Some(n) if n >= 0 => Native::U64Shift(ShiftKind::Shr, n.min(63) as u8),
                _ => return None,
            },
            ("bitwise-arithmetic-shift", &[_, n]) if lane => match self.fixnum_constant(n) {
                Some(n) if n <= 0 => Native::U64Shift(ShiftKind::Shr, (-n).min(63) as u8),
                _ => return None,
            },
            ("div", &[_, _]) => Native::Divide(DivKind::Div),
            ("div0", &[_, _]) => Native::Divide(DivKind::Div0),
            ("fxdiv", &[_, _]) => Native::Divide(DivKind::FxDiv),
            ("fxdiv0", &[_, _]) => Native::Divide(DivKind::FxDiv0),
            ("fxmod", &[_, _]) => Native::Divide(DivKind::FxMod),
            ("record?", &[_]) => Native::RecordP,
            ("record-rtd", &[_]) => Native::RecordRtd,
            ("rtd-ancestor?", &[_, _]) => Native::RtdAncestor,
            ("fxand", &[_, _]) => Native::FxBit(BitOp::And),
            ("fxior", &[_, _]) => Native::FxBit(BitOp::Or),
            ("fxxor", &[_, _]) => Native::FxBit(BitOp::Xor),
            ("bitwise-not", &[_]) => Native::Not,
            ("bitwise-arithmetic-shift-left", &[_, n]) => {
                {
                    let n = self.fixnum_constant(n).filter(|n| (0..63).contains(n))?;
                    Native::Shl(n as u8)
                }
            }
            ("bitwise-arithmetic-shift-right", &[_, n]) => match self.fixnum_constant(n) {
                Some(n) if n >= 0 => Native::Sar(n.min(63) as u8),
                _ => return None,
            },
            ("bitwise-arithmetic-shift", &[_, n]) => match self.fixnum_constant(n) {
                Some(n) if (0..63).contains(&n) => Native::Shl(n as u8),
                Some(n) if n < 0 => Native::Sar((-n).min(63) as u8),
                _ => return None,
            },
            ("fx+/wraparound", &[_, _]) => Native::Wrap(Op::AddI32),
            ("fx-/wraparound", &[_, _]) => Native::Wrap(Op::SubI32),
            ("fx*/wraparound", &[_, _]) => Native::Wrap(Op::MulI32),
            ("fxsll/wraparound", &[_, n]) => {
                {
                    let n = self
                    .fixnum_constant(n)
                    .filter(|n| (0..FIXNUM_WIDTH).contains(n))?;
                    Native::Shift32(ShiftKind::Shl, n as u8)
                }
            }
            ("fxsrl", &[_, n]) => {
                {
                    let n = self
                    .fixnum_constant(n)
                    .filter(|n| (0..FIXNUM_WIDTH).contains(n))?;
                    Native::Shift32(ShiftKind::Shr, n as u8)
                }
            }
            ("fxrotr32", &[_, n]) => {
                {
                    let n = self
                    .fixnum_constant(n)
                    .filter(|n| (0..FIXNUM_WIDTH).contains(n))?;
                    Native::Shift32(ShiftKind::Rotr, n as u8)
                }
            }
            ("fxrotl32", &[_, n]) => {
                {
                    let n = self
                    .fixnum_constant(n)
                    .filter(|n| (0..FIXNUM_WIDTH).contains(n))?;
                    Native::Shift32(ShiftKind::Rotr, ((32 - n) % 32) as u8)
                }
            }
            ("bytevector-u64-set!", &[_, _, _, endian]) => {
                let big = self.constant_endianness(endian)?;
                Native::Store(elem(8, false, big))
            },
            (name, &[_, _, endian]) if bytevector_ref_elem(name, false).is_some() => {
                {
                    let big = self.constant_endianness(endian)?;
                    Native::Load(bytevector_ref_elem(name, big).unwrap())
                }
            }
            // The numeric natives, reached through a first-class use such as
            // `(sort xs <)`: the operand-dispatching lowering of an
            // unprofiled `+`, `<` and the like. A non-number deopts.
            ("+", &[_, _]) => Native::Num(Arith::Add),
            ("-", &[_, _]) => Native::Num(Arith::Sub),
            ("*", &[_, _]) => Native::Num(Arith::Mul),
            ("-", &[_]) => Native::Negate,
            ("<", &[_, _]) => Native::NumCmp(Cmp::Lt),
            ("<=", &[_, _]) => Native::NumCmp(Cmp::Le),
            (">", &[_, _]) => Native::NumCmp(Cmp::Gt),
            (">=", &[_, _]) => Native::NumCmp(Cmp::Ge),
            ("=", &[_, _]) => Native::NumCmp(Cmp::Eq),
            (name, args) => {
                let text = self.text_shape(name, args)?;
                Native::Text(text)
            },
        };
        if blocked && !self.guard_free(&shape, args) {
            return None;
        }
        Some(shape)
    }

    /// The inline code of `shape` over `args`, deopting at `frame`.
    pub(super) fn native_lower(
        &mut self,
        shape: Native,
        args: &[Value],
        frame: FrameState,
    ) -> Result<Value, String> {
        let value = match shape {
            Native::Bit(op) => {
                // The wide lane: a fixnum or a bignum that fits i64. A wider
                // operand deopts, and the pc ends up blocked.
                let a = self.value_op(Op::CheckBigIntFits64, &[args[0]], Type::Int64, Some(frame));
                let b = self.value_op(Op::CheckBigIntFits64, &[args[1]], Type::Int64, Some(frame));
                let raw = self.value_op(Op::BitI64(op), &[a, b], Type::Int64, None);
                self.box_int64(raw)
            }
            Native::FxBit(op) => self.fx_bitwise(op, args[0], args[1], frame),
            Native::Divide(kind) => self.euclidean_divide(kind, args[0], args[1], frame),
            Native::RecordP => self.record_p(args[0], frame),
            Native::RecordRtd => self.record_rtd_value(args[0], frame),
            Native::RtdAncestor => self.rtd_ancestor_value(args[0], args[1], frame),
            Native::Not => {
                let a = self.value_op(Op::CheckBigIntFits64, &[args[0]], Type::Int64, Some(frame));
                let ones = self.const_i64(-1);
                let raw = self.value_op(Op::BitI64(BitOp::Xor), &[a, ones], Type::Int64, None);
                self.box_int64(raw)
            }
            Native::Shl(n) => {
                // `x * 2^n` deopts when the product leaves i64.
                let a = self.value_op(Op::CheckBigIntFits64, &[args[0]], Type::Int64, Some(frame));
                let factor = self.const_i64(1i64 << n);
                let raw = self.value_op(Op::MulI64Ovf, &[a, factor], Type::Int64, Some(frame));
                self.box_int64(raw)
            }
            Native::Sar(n) => {
                let a = self.value_op(Op::CheckBigIntFits64, &[args[0]], Type::Int64, Some(frame));
                let raw = if n == 0 {
                    a
                } else {
                    self.value_op(Op::ShiftI64(ShiftKind::Sar, n), &[a], Type::Int64, None)
                };
                self.box_int64(raw)
            }
            Native::Wrap(op) => {
                let a = self.fixnum_operand(args[0], frame);
                let b = self.fixnum_operand(args[1], frame);
                let raw = self.value_op(op, &[a, b], Type::Int32, None);
                self.box_int32(raw)
            }
            Native::Shift32(kind, n) => {
                let a = self.fixnum_operand(args[0], frame);
                let raw = if n == 0 {
                    a
                } else {
                    self.value_op(Op::ShiftI32(kind, n), &[a], Type::Int32, None)
                };
                self.box_int32(raw)
            }
            Native::Load(elem) => self.bytevector_load(elem, args[0], args[1], frame),
            Native::Store(elem) => self.bytevector_store(elem, args[0], args[1], args[2], frame),
            Native::U64Bit(op) => {
                let a = self.u64_operand(args[0], frame);
                let b = self.u64_operand(args[1], frame);
                let raw = self.value_op(Op::BitI64(op), &[a, b], Type::Int64, None);
                self.box_u64(raw)
            }
            Native::U64Arith(op) => {
                let a = self.u64_operand(args[0], frame);
                let b = self.u64_operand(args[1], frame);
                let raw = self.value_op(op, &[a, b], Type::Int64, None);
                self.box_u64(raw)
            }
            Native::U64Shift(kind, n) => {
                let a = self.u64_operand(args[0], frame);
                let raw = if n == 0 {
                    a
                } else {
                    self.value_op(Op::ShiftI64(kind, n), &[a], Type::Int64, None)
                };
                self.box_u64(raw)
            }
            Native::U64ToFlonum => {
                let a = self.u64_operand(args[0], frame);
                self.u64_to_flonum(a)
            }
            Native::Num(arith) => {
                self.binary_value(ArithPlan::Dual, arith, args[0], args[1], frame)
            }
            Native::Negate => self.dual_negate(args[0], frame),
            Native::NumCmp(cmp) => {
                let flag = self.dual_compare(cmp, args[0], args[1], frame)?;
                self.bool_value(flag)
            }
            Native::Text(text) => self.text_lower(text, args, frame),
        };
        Ok(value)
    }
}

/// The R6RS integer division procedures that have an inline fixnum path.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum DivKind {
    /// `div`: `x = d*y + m` with `0 <= m < |y|`.
    Div,
    /// `div0`: `x = d*y + m` with `-|y|/2 <= m < |y|/2`.
    Div0,
    /// `fxdiv`, `div` on fixnums; a result that is no fixnum is an error.
    FxDiv,
    /// `fxdiv0`, `div0` on fixnums.
    FxDiv0,
    /// `fxmod`, the `m` of `fxdiv`.
    FxMod,
}

impl<'s, 'gc> Builder<'s, 'gc> {
    /// `div`, `div0`, `fxdiv`, `fxdiv0` or `fxmod` on two fixnums.
    ///
    /// A non-fixnum operand, a zero divisor, and for the fx forms a result
    /// beyond the fixnum range (only `(fxdiv -2^31 -1)`) deopt to the native
    /// call, which raises the condition. The arithmetic is done in the i64
    /// lane, where nothing overflows for fixnum operands: with
    /// `m = x mod |y|` (`mod` of the floor division, so `0 <= m < |y|`),
    /// `d = (x - m) / y` is exact.
    fn euclidean_divide(&mut self, kind: DivKind, x: Value, y: Value, frame: FrameState) -> Value {
        // A constant power of two divisor needs no division: the remainder is a
        // mask and the quotient a shift (both floor, as `mod` and `div` are for
        // a positive divisor). Any other positive constant takes one division
        // and a correction of the truncated quotient.
        let constant = self.fixnum_constant(y).filter(|y| *y >= 2);
        let shift = constant
            .filter(|y| y.count_ones() == 1)
            .map(|y| y.trailing_zeros() as u8);
        let x = self.value_op(Op::CheckInt64, &[x], Type::Int64, Some(frame));
        let y = self.value_op(Op::CheckInt64, &[y], Type::Int64, Some(frame));
        // `d` is only known here for the one-division form.
        let mut truncated = None;
        let (abs_y, mut m) = if let Some(shift) = shift {
            let mask = self.const_i64((1i64 << shift) - 1);
            let m = self.value_op(Op::BitI64(BitOp::And), &[x, mask], Type::Int64, None);
            (y, m)
        } else if constant.is_some() {
            // `x = q*y + r` with `r` of the sign of `x`: a negative `r` moves
            // up by `y`, and `q` down by one.
            let q = self.value_op(Op::QuotI64, &[x, y], Type::Int64, Some(frame));
            let product = self.value_op(Op::MulI64, &[q, y], Type::Int64, None);
            let r = self.value_op(Op::SubI64, &[x, product], Type::Int64, None);
            let neg = self.value_op(Op::ShiftI64(ShiftKind::Sar, 63), &[r], Type::Int64, None);
            let step = self.value_op(Op::BitI64(BitOp::And), &[y, neg], Type::Int64, None);
            let m = self.value_op(Op::AddI64, &[r, step], Type::Int64, None);
            truncated = Some((q, neg));
            (y, m)
        } else {
            let sign = self.value_op(Op::ShiftI64(ShiftKind::Sar, 63), &[y], Type::Int64, None);
            let flipped = self.value_op(Op::BitI64(BitOp::Xor), &[y, sign], Type::Int64, None);
            let abs_y = self.value_op(Op::SubI64, &[flipped, sign], Type::Int64, None);
            // Deopts on a zero divisor.
            let m = self.value_op(Op::ModI64, &[x, abs_y], Type::Int64, Some(frame));
            (abs_y, m)
        };
        let mut centered = None;
        if matches!(kind, DivKind::Div0 | DivKind::FxDiv0) {
            // Centered: `m >= |y|/2` moves down by `|y|`. The mask is all ones
            // exactly when `|y| - 2m - 1 < 0`.
            let twice = self.value_op(Op::AddI64, &[m, m], Type::Int64, None);
            let gap = self.value_op(Op::SubI64, &[abs_y, twice], Type::Int64, None);
            let one = self.const_i64(1);
            let gap = self.value_op(Op::SubI64, &[gap, one], Type::Int64, None);
            let mask = self.value_op(Op::ShiftI64(ShiftKind::Sar, 63), &[gap], Type::Int64, None);
            let step = self.value_op(Op::BitI64(BitOp::And), &[abs_y, mask], Type::Int64, None);
            m = self.value_op(Op::SubI64, &[m, step], Type::Int64, None);
            centered = Some(mask);
        }
        if kind == DivKind::FxMod {
            return self.value_op(Op::Box(Repr::Int64), &[m], Type::Any, None);
        }
        let d = match (shift, truncated) {
            (Some(shift), _) => {
                let rest = self.value_op(Op::SubI64, &[x, m], Type::Int64, None);
                self.value_op(
                    Op::ShiftI64(ShiftKind::Sar, shift),
                    &[rest],
                    Type::Int64,
                    None,
                )
            }
            (None, Some((q, neg))) => {
                // The divisor is positive: `d = q + neg`, and the centered
                // quotient one more when `m` moved down.
                let d = self.value_op(Op::AddI64, &[q, neg], Type::Int64, None);
                match centered {
                    Some(mask) => self.value_op(Op::SubI64, &[d, mask], Type::Int64, None),
                    None => d,
                }
            }
            (None, None) => {
                let rest = self.value_op(Op::SubI64, &[x, m], Type::Int64, None);
                self.value_op(Op::QuotI64, &[rest, y], Type::Int64, Some(frame))
            }
        };
        if matches!(kind, DivKind::FxDiv | DivKind::FxDiv0) {
            let max = self.const_i64(i32::MAX as i64);
            let fits = self.value_op(Op::CmpI64(Cmp::Le), &[d, max], Type::Bool, None);
            self.guard_true(fits, frame);
        }
        self.value_op(Op::Box(Repr::Int64), &[d], Type::Any, None)
    }
}

/// The inline shape of a recognized native call.
#[derive(Clone, Copy)]
pub(super) enum Native {
    Divide(DivKind),
    RecordP,
    RecordRtd,
    RtdAncestor,
    Bit(BitOp),
    FxBit(BitOp),
    Not,
    Shl(u8),
    Sar(u8),
    Wrap(Op),
    Shift32(ShiftKind, u8),
    Load(BvElem),
    Store(BvElem),
    U64Bit(BitOp),
    U64Arith(Op),
    U64Shift(ShiftKind, u8),
    U64ToFlonum,
    Num(Arith),
    Negate,
    NumCmp(Cmp),
    Text(Text),
}

const fn elem(bytes: u8, signed: bool, big_endian: bool) -> BvElem {
    BvElem {
        bytes,
        signed,
        big_endian,
    }
}

/// The element of an explicit-endianness bytevector accessor.
fn bytevector_ref_elem(name: &str, big: bool) -> Option<BvElem> {
    Some(match name {
        "bytevector-u16-ref" => elem(2, false, big),
        "bytevector-s16-ref" => elem(2, true, big),
        "bytevector-u32-ref" => elem(4, false, big),
        "bytevector-s32-ref" => elem(4, true, big),
        "bytevector-u64-ref" => elem(8, false, big),
        _ => return None,
    })
}
