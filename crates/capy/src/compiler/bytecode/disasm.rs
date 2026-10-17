//! Bytecode disassembler, for compiler dumps and debugging.

use std::fmt::Write;

use bytecodegen::OperandWidth;

use crate::bytecode::def::Instruction;
use crate::runtime::prims::def::Primitive;
use crate::runtime::value::proc::{SRC_FLAG_SYNTHETIC, SpanHit, span_short};

fn render_mnemonic_and_operands(ins: &Instruction, suffix: &str, pc: usize, out: &mut String) {
    match ins {
        Instruction::OpNop(_) => {
            out.push_str("(nop");
            out.push_str(suffix);
        }
        Instruction::OpEnter(op) => {
            out.push_str("(enter");
            out.push_str(suffix);
            let _ = write!(out, " #:argc {} #:frame {}", op.argc.0, op.frame.0);
        }
        Instruction::OpEnterRest(op) => {
            out.push_str("(enter-rest");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:required {} #:cnt r{} #:base r{} #:frame {}",
                op.required.0, op.cnt.0, op.base.0, op.frame.0
            );
        }
        Instruction::OpReturnPoint(op) => {
            out.push_str("(return-point");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:frame {} #:mask @{} #:mv {} #:dst r{} #:metadata-id {}",
                op.frame.0, op.mask.0, op.mv.0, op.dst.0, op.metadata_id.0
            );
        }
        Instruction::OpReturnPointForward(op) => {
            out.push_str("(return-point-forward");
            out.push_str(suffix);
            let _ = write!(out, " #:frame {} #:mask @{}", op.frame.0, op.mask.0);
        }
        Instruction::OpReturnPointValues(op) => {
            out.push_str("(return-point-values");
            out.push_str(suffix);
            let _ = write!(out, " #:nvals {} #:frame {}", op.nvals.0, op.frame.0);
        }
        Instruction::OpReturnPointValuesRest(op) => {
            out.push_str("(return-point-values-rest");
            out.push_str(suffix);
            let _ = write!(out, " #:nvals {} #:frame {}", op.nvals.0, op.frame.0);
        }
        Instruction::OpMove(op) => {
            out.push_str("(move");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:src r{}", op.dst.0, op.src.0);
        }
        Instruction::OpLoadConstant(op) => {
            out.push_str("(load-constant");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:constant c{}", op.dst.0, op.constant.0);
        }
        Instruction::OpLoadImmediate(op) => {
            out.push_str("(load-immediate");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:value {}", op.dst.0, op.value.0);
        }
        Instruction::OpLoadRaw(op) => {
            out.push_str("(load-raw");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:bits {}", op.dst.0, op.bits.0);
        }
        Instruction::OpCellRef(op) => {
            out.push_str("(cell-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:constant c{} #:metadata-id {}",
                op.dst.0, op.constant.0, op.metadata_id.0
            );
        }
        Instruction::OpCellSet(op) => {
            out.push_str("(cell-set");
            out.push_str(suffix);
            let _ = write!(out, " #:constant c{} #:src r{}", op.constant.0, op.src.0);
        }
        Instruction::OpRuntimeProcedure(op) => {
            out.push_str("(runtime-procedure");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:id {}", op.dst.0, op.id.0);
        }
        Instruction::OpClosure(op) => {
            out.push_str("(closure");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:constant c{} #:nfree {}",
                op.dst.0, op.constant.0, op.nfree.0
            );
        }
        Instruction::OpClosureEnv(op) => {
            out.push_str("(closure-env");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:constant c{} #:nfree {} #:env r{}",
                op.dst.0, op.constant.0, op.nfree.0, op.env.0
            );
        }
        Instruction::OpFreeRef(op) => {
            out.push_str("(free-ref");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:index {}", op.dst.0, op.index.0);
        }
        Instruction::OpClosureRef(op) => {
            out.push_str("(closure-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:closure r{} #:index {}",
                op.dst.0, op.closure.0, op.index.0
            );
        }
        Instruction::OpClosureSet(op) => {
            out.push_str("(closure-set");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:closure r{} #:index {} #:src r{}",
                op.closure.0, op.index.0, op.src.0
            );
        }
        Instruction::OpMakeEnv(op) => {
            out.push_str("(make-env");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:size {}", op.dst.0, op.size.0);
        }
        Instruction::OpEnvRef(op) => {
            out.push_str("(env-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:env r{} #:index {}",
                op.dst.0, op.env.0, op.index.0
            );
        }
        Instruction::OpEnvSet(op) => {
            out.push_str("(env-set");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:env r{} #:index {} #:src r{}",
                op.env.0, op.index.0, op.src.0
            );
        }
        Instruction::OpJump(op) => {
            out.push_str("(jump");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:target ->{}",
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfTrue(op) => {
            out.push_str("(jump-if-true");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:cond r{} #:target ->{}",
                op.cond.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfFalse(op) => {
            out.push_str("(jump-if-false");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:cond r{} #:target ->{}",
                op.cond.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNull(op) => {
            out.push_str("(jump-if-null");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNotNull(op) => {
            out.push_str("(jump-if-not-null");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfPair(op) => {
            out.push_str("(jump-if-pair");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNotPair(op) => {
            out.push_str("(jump-if-not-pair");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfFixnum(op) => {
            out.push_str("(jump-if-fixnum");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNotFixnum(op) => {
            out.push_str("(jump-if-not-fixnum");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfUndefined(op) => {
            out.push_str("(jump-if-undefined");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNotUndefined(op) => {
            out.push_str("(jump-if-not-undefined");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:src r{} #:target ->{}",
                op.src.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfEq(op) => {
            out.push_str("(jump-if-eq");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfNotEq(op) => {
            out.push_str("(jump-if-not-eq");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize
            );
        }
        Instruction::OpJumpIfLess(op) => {
            out.push_str("(jump-if-less");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfLessEqual(op) => {
            out.push_str("(jump-if-less-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNumberEqual(op) => {
            out.push_str("(jump-if-number-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfGreaterEqual(op) => {
            out.push_str("(jump-if-greater-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfGreater(op) => {
            out.push_str("(jump-if-greater");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNotLess(op) => {
            out.push_str("(jump-if-not-less");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNotLessEqual(op) => {
            out.push_str("(jump-if-not-less-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNotNumberEqual(op) => {
            out.push_str("(jump-if-not-number-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNotGreaterEqual(op) => {
            out.push_str("(jump-if-not-greater-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpJumpIfNotGreater(op) => {
            out.push_str("(jump-if-not-greater");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:lhs r{} #:rhs r{} #:target ->{} #:metadata-id {}",
                op.lhs.0,
                op.rhs.0,
                (pc as i64 + op.target as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpSwitchFixnum(op) => {
            out.push_str("(switch-fixnum");
            out.push_str(suffix);
            let _ = write!(out, " #:src r{} #:table @{}", op.src.0, op.table.0);
        }
        Instruction::OpSwitchChar(op) => {
            out.push_str("(switch-char");
            out.push_str(suffix);
            let _ = write!(out, " #:src r{} #:table @{}", op.src.0, op.table.0);
        }
        Instruction::OpSwitchSymbol(op) => {
            out.push_str("(switch-symbol");
            out.push_str(suffix);
            let _ = write!(out, " #:src r{} #:table @{}", op.src.0, op.table.0);
        }
        Instruction::OpLoopHint(op) => {
            out.push_str("(loop-hint");
            out.push_str(suffix);
            let _ = write!(out, " #:mask @{}", op.mask.0);
        }
        Instruction::OpCall(op) => {
            out.push_str("(call");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:argc {} #:base r{} #:metadata-id {}",
                op.argc.0, op.base.0, op.metadata_id.0
            );
        }
        Instruction::OpTailCall(op) => {
            out.push_str("(tail-call");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:argc {} #:metadata-id {}",
                op.argc.0, op.metadata_id.0
            );
        }
        Instruction::OpReturn(op) => {
            out.push_str("(return");
            out.push_str(suffix);
            let _ = write!(out, " #:src r{}", op.src.0);
        }
        Instruction::OpReturnValues(op) => {
            out.push_str("(return-values");
            out.push_str(suffix);
            let _ = write!(out, " #:count {}", op.count.0);
        }
        Instruction::OpRaise(op) => {
            out.push_str("(raise");
            out.push_str(suffix);
            let _ = write!(out, " #:kind {} #:count {}", op.kind.0, op.count.0);
        }
        Instruction::OpRestRef(op) => {
            out.push_str("(rest-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:cnt r{} #:base r{} #:index {}",
                op.dst.0, op.cnt.0, op.base.0, op.index.0
            );
        }
        Instruction::OpRestLen(op) => {
            out.push_str("(rest-len");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:cnt r{} #:skip {}",
                op.dst.0, op.cnt.0, op.skip.0
            );
        }
        Instruction::OpRestNull(op) => {
            out.push_str("(rest-null");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:cnt r{} #:skip {}",
                op.dst.0, op.cnt.0, op.skip.0
            );
        }
        Instruction::OpRestPair(op) => {
            out.push_str("(rest-pair");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:cnt r{} #:skip {}",
                op.dst.0, op.cnt.0, op.skip.0
            );
        }
        Instruction::OpRestList(op) => {
            out.push_str("(rest-list");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:cnt r{} #:base r{} #:skip {}",
                op.dst.0, op.cnt.0, op.base.0, op.skip.0
            );
        }
        Instruction::OpValuesRest(op) => {
            out.push_str("(values-rest");
            out.push_str(suffix);
            let _ = write!(out, " #:base r{} #:from {}", op.base.0, op.from.0);
        }
        Instruction::OpCallPrimitive0(op) => {
            out.push_str("(call-primitive-0");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:primitive p{}", op.dst.0, op.primitive.0);
        }
        Instruction::OpCallPrimitive1(op) => {
            out.push_str("(call-primitive-1");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:primitive p{} #:a r{}",
                op.dst.0, op.primitive.0, op.a.0
            );
        }
        Instruction::OpCallPrimitive2(op) => {
            out.push_str("(call-primitive-2");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:primitive p{} #:a r{} #:b r{}",
                op.dst.0, op.primitive.0, op.a.0, op.b.0
            );
        }
        Instruction::OpCallPrimitive3(op) => {
            out.push_str("(call-primitive-3");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:primitive p{} #:a r{} #:b r{} #:c r{}",
                op.dst.0, op.primitive.0, op.a.0, op.b.0, op.c.0
            );
        }
        Instruction::OpCallPrimitiveN(op) => {
            out.push_str("(call-primitive-n");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:primitive p{} #:first r{} #:argc {}",
                op.dst.0, op.primitive.0, op.first.0, op.argc.0
            );
        }
        Instruction::OpCar(op) => {
            out.push_str("(car");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:metadata-id {}",
                op.dst.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpCdr(op) => {
            out.push_str("(cdr");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:metadata-id {}",
                op.dst.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpCons(op) => {
            out.push_str("(cons");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:car r{} #:cdr r{}",
                op.dst.0, op.car.0, op.cdr.0
            );
        }
        Instruction::OpSetCar(op) => {
            out.push_str("(set-car");
            out.push_str(suffix);
            let _ = write!(out, " #:pair r{} #:src r{}", op.pair.0, op.src.0);
        }
        Instruction::OpSetCdr(op) => {
            out.push_str("(set-cdr");
            out.push_str(suffix);
            let _ = write!(out, " #:pair r{} #:src r{}", op.pair.0, op.src.0);
        }
        Instruction::OpEq(op) => {
            out.push_str("(eq");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{}",
                op.dst.0, op.lhs.0, op.rhs.0
            );
        }
        Instruction::OpNot(op) => {
            out.push_str("(not");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:src r{}", op.dst.0, op.src.0);
        }
        Instruction::OpAdd(op) => {
            out.push_str("(add");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpSub(op) => {
            out.push_str("(sub");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpMul(op) => {
            out.push_str("(mul");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpAddImmediate(op) => {
            out.push_str("(add-immediate");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:value {} #:metadata-id {}",
                op.dst.0, op.src.0, op.value.0, op.metadata_id.0
            );
        }
        Instruction::OpLess(op) => {
            out.push_str("(less");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpLessEqual(op) => {
            out.push_str("(less-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpNumberEqual(op) => {
            out.push_str("(number-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpGreaterEqual(op) => {
            out.push_str("(greater-equal");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpGreater(op) => {
            out.push_str("(greater");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpDiv(op) => {
            out.push_str("(div");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpQuotient(op) => {
            out.push_str("(quotient");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpRemainder(op) => {
            out.push_str("(remainder");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpModulo(op) => {
            out.push_str("(modulo");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.dst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpNegate(op) => {
            out.push_str("(negate");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:metadata-id {}",
                op.dst.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpAbs(op) => {
            out.push_str("(abs");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:metadata-id {}",
                op.dst.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpSqrt(op) => {
            out.push_str("(sqrt");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:src r{} #:metadata-id {}",
                op.dst.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpVectorRef(op) => {
            out.push_str("(vector-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:vector r{} #:index r{} #:metadata-id {}",
                op.dst.0, op.vector.0, op.index.0, op.metadata_id.0
            );
        }
        Instruction::OpVectorSet(op) => {
            out.push_str("(vector-set");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:vector r{} #:index r{} #:src r{} #:metadata-id {}",
                op.vector.0, op.index.0, op.src.0, op.metadata_id.0
            );
        }
        Instruction::OpVectorLength(op) => {
            out.push_str("(vector-length");
            out.push_str(suffix);
            let _ = write!(out, " #:dst r{} #:src r{}", op.dst.0, op.src.0);
        }
        Instruction::OpVariableRef(op) => {
            out.push_str("(variable-ref");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:var r{} #:metadata-id {}",
                op.dst.0, op.var.0, op.metadata_id.0
            );
        }
        Instruction::OpVariableSet(op) => {
            out.push_str("(variable-set");
            out.push_str(suffix);
            let _ = write!(out, " #:var r{} #:src r{}", op.var.0, op.src.0);
        }
        Instruction::OpGetSlot(op) => {
            out.push_str("(get-slot");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst r{} #:obj r{} #:name c{} #:slow ->{} #:metadata-id {}",
                op.dst.0,
                op.obj.0,
                op.name.0,
                (pc as i64 + op.slow as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpPutSlot(op) => {
            out.push_str("(put-slot");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:obj r{} #:name c{} #:src r{} #:slow ->{} #:metadata-id {}",
                op.obj.0,
                op.name.0,
                op.src.0,
                (pc as i64 + op.slow as i64) as usize,
                op.metadata_id.0
            );
        }
        Instruction::OpMoveTwo(op) => {
            out.push_str("(move-two");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:dst1 r{} #:src1 r{} #:dst2 r{} #:src2 r{}",
                op.dst1.0, op.src1.0, op.dst2.0, op.src2.0
            );
        }
        Instruction::OpAddImmediateMove(op) => {
            out.push_str("(add-immediate-move");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:tmp r{} #:src r{} #:value {} #:dst r{} #:metadata-id {}",
                op.tmp.0, op.src.0, op.value.0, op.dst.0, op.metadata_id.0
            );
        }
        Instruction::OpVectorRefMul(op) => {
            out.push_str("(vector-ref-mul");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:vdst r{} #:vector r{} #:index r{} #:mdst r{} #:lhs r{} #:rhs r{} #:metadata-id {}",
                op.vdst.0, op.vector.0, op.index.0, op.mdst.0, op.lhs.0, op.rhs.0, op.metadata_id.0
            );
        }
        Instruction::OpCallSelf(op) => {
            out.push_str("(call-self");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:argc {} #:base r{} #:metadata-id {}",
                op.argc.0, op.base.0, op.metadata_id.0
            );
        }
        Instruction::OpTailCallSelf(op) => {
            out.push_str("(tail-call-self");
            out.push_str(suffix);
            let _ = write!(
                out,
                " #:argc {} #:metadata-id {}",
                op.argc.0, op.metadata_id.0
            );
        }
    }
}

/// Render `code` one instruction per line, with pool entries expanded.
pub fn disassemble(code: &[u8], pool: &[u32]) -> String {
    let mut out = String::new();
    let mut pc = 0usize;
    while pc < code.len() {
        let Some((ins, width, len)) = Instruction::decode_slice(code, pc) else {
            let _ = writeln!(out, "{pc:6}  .byte {:#04x}", code[pc]);
            pc += 1;
            continue;
        };
        out.push_str(&disassemble_insn(&ins, width, pc, pool));
        out.push('\n');
        pc += len;
    }
    out
}

/// Render one decoded instruction: the listing row [`disassemble`] emits
/// for it, without the trailing newline. The JIT listing reuses it to
/// interleave bytecode with native code.
/// Render `code` one instruction per line like [`disassemble`], with a
/// trailing `; file:line:col` (or `; <generated>`) comment wherever
/// `span_at` reports a run covering the instruction's pc. Gaps print
/// no comment.
pub fn disassemble_with_spans<'gc>(
    code: &[u8],
    pool: &[u32],
    span_at: &dyn Fn(usize) -> Option<SpanHit<'gc>>,
) -> String {
    let mut out = String::new();
    let mut pc = 0usize;
    while pc < code.len() {
        let Some((ins, width, len)) = Instruction::decode_slice(code, pc) else {
            let _ = writeln!(out, "{pc:6}  .byte {:#04x}", code[pc]);
            pc += 1;
            continue;
        };
        let mut row = disassemble_insn(&ins, width, pc, pool);
        if let Some(hit) = span_at(pc) {
            if let Some(short) = span_short(hit.span) {
                row.push_str(" ; ");
                row.push_str(&short);
            } else if hit.flags & SRC_FLAG_SYNTHETIC != 0 {
                row.push_str(" ; <generated>");
            }
        }
        out.push_str(&row);
        out.push('\n');
        pc += len;
    }
    out
}

pub fn disassemble_insn(ins: &Instruction, width: OperandWidth, pc: usize, pool: &[u32]) -> String {
    let suffix = match width {
        OperandWidth::Narrow => "",
        OperandWidth::Wide16 => ".w16",
        OperandWidth::Wide32 => ".w32",
    };
    let mut row = format!("{pc:6}  ");
    render_mnemonic_and_operands(ins, suffix, pc, &mut row);
    for entry in switch_entries(ins, pc, pool) {
        row.push_str("\n");
        row.push_str(SWITCH_ENTRY_INDENT);
        row.push_str(&entry);
    }
    row.push(')');
    row.push_str(&extra_info(ins, pool));
    row
}

/// Column of a switch table entry: under the first operand of the row.
const SWITCH_ENTRY_INDENT: &str = "          ";

/// `; ...` commentary for one instruction: primitive names and mask bits.
fn extra_info(ins: &Instruction, pool: &[u32]) -> String {
    match ins {
        Instruction::OpCallPrimitive0(op) => prim_name(op.primitive.0),
        Instruction::OpCallPrimitive1(op) => prim_name(op.primitive.0),
        Instruction::OpCallPrimitive2(op) => prim_name(op.primitive.0),
        Instruction::OpCallPrimitive3(op) => prim_name(op.primitive.0),
        Instruction::OpCallPrimitiveN(op) => prim_name(op.primitive.0),
        Instruction::OpReturnPoint(op) => mask_bits(pool, op.mask.0),
        Instruction::OpReturnPointForward(op) => mask_bits(pool, op.mask.0),
        Instruction::OpLoopHint(op) => mask_bits(pool, op.mask.0),
        _ => String::new(),
    }
}

fn prim_name(id: u32) -> String {
    let name = u16::try_from(id)
        .ok()
        .and_then(Primitive::from_id)
        .map_or("?", Primitive::name);
    format!("  ; {name}")
}

fn mask_bits(pool: &[u32], at: u32) -> String {
    let Some(entry) = pool.get(at as usize) else {
        return "  ; mask @?".to_string();
    };
    let nwords = *entry as usize;
    if at as usize + 1 + nwords > pool.len() {
        return "  ; mask @?".to_string();
    }
    let words: Vec<String> = pool[at as usize + 1..at as usize + 1 + nwords]
        .iter()
        .map(|word| format!("{word:#x}"))
        .collect();
    format!("  ; mask [{}]", words.join(" "))
}

/// One `(key -> pc)` line per switch table entry, default first.
fn switch_entries(ins: &Instruction, pc: usize, pool: &[u32]) -> Vec<String> {
    match ins {
        Instruction::OpSwitchFixnum(op) => int_table(pool, pc, op.table.0),
        Instruction::OpSwitchChar(op) => int_table(pool, pc, op.table.0),
        Instruction::OpSwitchSymbol(op) => sym_table(pool, pc, op.table.0),
        _ => Vec::new(),
    }
}

fn unreadable_table() -> Vec<String> {
    vec!["(table @?)".to_string()]
}

fn int_table(pool: &[u32], pc: usize, at: u32) -> Vec<String> {
    use crate::bytecode::def::{switch_table_is_dense, switch_table_len};
    let at = at as usize;
    let Some(count_word) = pool.get(at + 1).copied() else {
        return unreadable_table();
    };
    let target = |disp: u32| pc as i64 + disp as i32 as i64;
    let mut entries = vec![format!("(default -> {})", target(pool[at]))];
    if switch_table_is_dense(count_word) {
        let span = switch_table_len(count_word);
        if at + 3 + span > pool.len() || pool.get(at + 2).is_none() {
            return unreadable_table();
        }
        let min = pool[at + 2] as i32;
        entries.push(format!("(dense #:min {min})"));
        let mut holes = 0usize;
        for entry in 0..span {
            let disp = pool[at + 3 + entry];
            if disp == pool[at] {
                holes += 1;
            } else {
                entries.push(format!(
                    "({} -> {})",
                    min.wrapping_add(entry as i32),
                    target(disp)
                ));
            }
        }
        if holes != 0 {
            entries.push(format!("(holes {holes} -> default)"));
        }
    } else {
        let count = count_word as usize;
        if at + 2 + 2 * count > pool.len() {
            return unreadable_table();
        }
        for case in 0..count {
            let key = pool[at + 2 + 2 * case] as i32;
            entries.push(format!(
                "({key} -> {})",
                target(pool[at + 2 + 2 * case + 1])
            ));
        }
    }
    entries
}

fn sym_table(pool: &[u32], pc: usize, at: u32) -> Vec<String> {
    let at = at as usize;
    let Some(count) = pool.get(at + 1).copied() else {
        return unreadable_table();
    };
    if at + 4 + 3 * count as usize > pool.len() {
        return unreadable_table();
    }
    let target = |disp: u32| pc as i64 + disp as i32 as i64;
    let mut entries = vec![format!("(default -> {})", target(pool[at]))];
    for case in 0..count as usize {
        let key = pool[at + 4 + 3 * case] as u64 | (pool[at + 4 + 3 * case + 1] as u64) << 32;
        entries.push(format!(
            "({key:#x} -> {})",
            target(pool[at + 4 + 3 * case + 2])
        ));
    }
    entries
}
