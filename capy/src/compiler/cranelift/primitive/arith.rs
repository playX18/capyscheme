use super::super::SSABuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::rsgc::object::builtin_class_ids;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use std::mem::offset_of;


pub fn lower_fx_eq<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        }

        let mut lhs = ssa.atom(args[0]);
        let mut result = ssa.builder.ins().iconst(types::I8, 1);
        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let eq = emit_fx_eq(ssa, lhs, rhs);
            result = ssa.builder.ins().band(result, eq);
            lhs = rhs;
        }
        PrimValue::Comparison(result)
    }

pub fn lower_ash<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);

        let result = ssa.handle_thunk_call_result(ssa.thunks.ash, &[ctx, lhs, rhs]);
        PrimValue::Value(result)
    }

pub fn lower_logand<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.logand, &[ctx, lhs, rhs]);
        PrimValue::Value(result)
    }

pub fn lower_logior<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.logior, &[ctx, lhs, rhs]);
        PrimValue::Value(result)
    }

pub fn lower_lognot<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.lognot, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_expt<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let base = ssa.atom(args[0]);
        let exp = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.expt, &[ctx, base, exp]);
        PrimValue::Value(result)
    }

pub fn lower_abs<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.abs, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_sqrt<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);

        let result = ssa.inline_float_unary_op(arg,
            |ssa, val| {

                ssa.builder.ins().sqrt(val)
            },
            |ssa, arg| {
                let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
                ssa.handle_thunk_call_result(ssa.thunks.sqrt, &[ctx, arg])
            }
        );

        PrimValue::Value(result)
    }

pub fn lower_cos<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.cos, &[ctx, arg]);

        PrimValue::Value(result)
    }

pub fn lower_sin<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.sin, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_tan<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.tan, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_atan<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        if args.len() == 2 {
            let arg2 = ssa.atom(args[1]);
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let result = ssa.handle_thunk_call_result(ssa.thunks.atan2, &[ctx, arg, arg2]);
            return PrimValue::Value(result);
        }
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.atan, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_asin<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.asin, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_acos<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.acos, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_ceiling<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.ceiling, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_floor<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.floor, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_truncate<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.truncate, &[ctx, arg]);
        PrimValue::Value(result)
    }

pub fn lower_plus<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]))
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_plus(ssa, acc, rhs);
        }

        PrimValue::Value(acc)
    }

pub fn lower_minus<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            let val = ssa.atom(args[0]);

            return PrimValue::Value(emit_negate(ssa, val));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_minus(ssa, acc, rhs);
        }

        PrimValue::Value(acc)
    }

pub fn lower_times<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_times(ssa, acc, rhs);
        }

        PrimValue::Value(acc)
    }

pub fn lower_div<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.number_div, &[ctx, a, b]);
        PrimValue::Value(result)
    }

pub fn lower_numeric_equal<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let eq = emit_icmp(ssa, lhs, rhs, IntCC::Equal);
            return PrimValue::Comparison(eq);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let eq = emit_icmp(ssa, acc,rhs, IntCC::Equal);
            if ssa.builder.func.dfg.value_type(eq) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, eq);
        }
        PrimValue::Comparison(acc)
    }

pub fn lower_numeric_lt<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let lt = emit_icmp(ssa, lhs, rhs, IntCC::SignedLessThan);
            return PrimValue::Comparison(lt);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let lt = emit_icmp(ssa, acc,rhs, IntCC::SignedLessThan);
            if ssa.builder.func.dfg.value_type(lt) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, lt);
        }
        PrimValue::Comparison(acc)
    }

pub fn lower_numeric_gt<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let gt = emit_icmp(ssa, lhs, rhs, IntCC::SignedGreaterThan);
            assert_eq!(ssa.builder.func.dfg.value_type(gt), types::I8);
            return PrimValue::Comparison(gt);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let gt = emit_icmp(ssa, acc,rhs, IntCC::SignedGreaterThan);
            if ssa.builder.func.dfg.value_type(gt) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, gt);
        }
        PrimValue::Comparison(acc)
    }

pub fn lower_numeric_gte<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let gte = emit_icmp(ssa, lhs, rhs, IntCC::SignedGreaterThanOrEqual);
            return PrimValue::Comparison(gte);
        }

        let mut acc = ssa.atom(args[0]);

        let end = ssa.builder.create_block();
        ssa.builder.append_block_param(end, types::I8);
        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let gte = emit_icmp(ssa, acc,rhs, IntCC::SignedGreaterThanOrEqual);
            if ssa.builder.func.dfg.value_type(gte) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, gte);
        }
        PrimValue::Comparison(acc)
    }

pub fn lower_numeric_lte<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let lte = ssa.handle_thunk_call_result(ssa.thunks.number_le, &[ctx, lhs, rhs]);
            //emit_icmp(ssa, lhs, rhs, IntCC::SignedLessThanOrEqual);
            return PrimValue::Value(lte);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let lte = emit_icmp(ssa, acc,rhs, IntCC::SignedLessThanOrEqual);
            if ssa.builder.func.dfg.value_type(lte) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!("type mismatch in <=: {:?} vs {:?}, args: {:?}", ssa.builder.func.dfg.value_type(lte), ssa.builder.func.dfg.value_type(acc), args)
            }
            acc = ssa.builder.ins().band(acc, lte);
        }
        PrimValue::Comparison(acc)
    }

pub fn lower_exact_to_inexact<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.exact2inexact, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_inexact_to_exact<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.inexact_to_exact, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_is_even<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let check_int = ssa.builder.create_block();
        let thunk_call = ssa.builder.create_block();
        let join = ssa.builder.create_block();
        ssa.builder.append_block_param(join, types::I8);
        ssa.branch_if_int32(val, check_int, &[], thunk_call, &[]);
        ssa.builder.switch_to_block(check_int);
        {
            let int32 = ssa.ireduce(types::I32, val);
            let mask = ssa.builder.ins().band_imm(int32, 1);
            let is_even = ssa.builder.ins().icmp_imm(IntCC::Equal, mask, 0);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_even)]);
        }

        ssa.builder.switch_to_block(thunk_call);
        {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let val = ssa.handle_thunk_call_result(ssa.thunks.evenp, &[ctx, val]);
            let is_even = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_even)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)
    }

pub fn lower_is_odd<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let check_int = ssa.builder.create_block();
        let thunk_call = ssa.builder.create_block();
        let join = ssa.builder.create_block();
        ssa.builder.append_block_param(join, types::I8);
        ssa.branch_if_int32(val, check_int, &[], thunk_call, &[]);
        ssa.builder.switch_to_block(check_int);
        {
            let int32 = ssa.ireduce(types::I32, val);
            let mask = ssa.builder.ins().band_imm(int32, 1);
            let is_odd = ssa.builder.ins().icmp_imm(IntCC::Equal, mask, 1);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_odd)]);
        }

        ssa.builder.switch_to_block(thunk_call);
        {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let val = ssa.handle_thunk_call_result(ssa.thunks.oddp, &[ctx, val]);
            let is_odd = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_odd)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)
    }

pub fn lower_is_zero<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        let val = ssa.atom(args[0]);

        let check_int = ssa.builder.create_block();
        let thunk_call = ssa.builder.create_block();
        let join = ssa.builder.create_block();
        ssa.builder.append_block_param(join, types::I8);
        ssa.branch_if_int32(val, check_int, &[], thunk_call, &[]);
        ssa.builder.switch_to_block(check_int);
        {
            let int32 = ssa.ireduce(types::I32, val);
            let is_zero = ssa.builder.ins().icmp_imm(IntCC::Equal, int32, 0);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_zero)]);
        }

        ssa.builder.switch_to_block(thunk_call);
        {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let val = ssa.handle_thunk_call_result(ssa.thunks.is_zero, &[ctx, val]);
            let is_zero = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_zero)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)

    }

pub fn lower_quotient<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.quotient, &[ctx, a, b]);
        PrimValue::Value(result)
    }

pub fn lower_remainder<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.remainder, &[ctx, a, b]);
        PrimValue::Value(result)
    }

pub fn lower_modulo<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.modulo, &[ctx, a, b]);
        PrimValue::Value(result)
    }


fn emit_plus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().sadd_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();
            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.number_plus, &[ctx, lhs, rhs])
        },
    )
}

fn emit_minus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().ssub_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();

            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.number_minus, &[ctx, lhs, rhs])
        },
    )
}

fn emit_negate<'gc, 'a, 'f>(ssa: &mut SSABuilder<'gc, 'a, 'f>, a: ir::Value) -> ir::Value {
    ssa.inline_unary_op(
        a,
        |ssa, val, slow| {
            let zero = ssa.builder.ins().iconst(types::I32, 0);
            let (result, ovf) = ssa.builder.ins().ssub_overflow(zero, val);
            let succ = ssa.builder.create_block();

            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, val| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.negate, &[ctx, val])
        },
    )
}

fn emit_times<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().smul_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();
            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.number_times, &[ctx, lhs, rhs])
        },
    )
}

fn emit_icmp<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    cond: IntCC,
) -> ir::Value {
    let result = ssa.inline_cmp_op(
        a,
        b,
        |ssa, lhs, rhs, _slow| ssa.builder.ins().icmp(cond, lhs, rhs),
        |ssa, a, b| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let thunk = match cond {
                IntCC::Equal => ssa.thunks.number_eq,
                IntCC::SignedGreaterThan => ssa.thunks.number_gt,
                IntCC::SignedGreaterThanOrEqual => ssa.thunks.number_ge,
                IntCC::SignedLessThan => ssa.thunks.number_lt,
                IntCC::SignedLessThanOrEqual => ssa.thunks.number_le,
                _ => panic!("unsupported comparison operator: {cond:?}"),
            };

            let result = ssa.handle_thunk_call_result(thunk, &[ctx, a, b]);
            ssa.to_boolean(result)
        },
    );
    assert_eq!(ssa.builder.func.dfg.value_type(result), types::I8);
    result
}

fn emit_fx_eq<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
) -> ir::Value {
    let result = ssa.inline_cmp_op(
        a,
        b,
        |ssa, lhs, rhs, _slow| ssa.builder.ins().icmp(IntCC::Equal, lhs, rhs),
        |ssa, a, b| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let result = ssa.handle_thunk_call_result(ssa.thunks.fxeq, &[ctx, a, b]);
            ssa.to_boolean(result)
        },
    );
    assert_eq!(ssa.builder.func.dfg.value_type(result), types::I8);
    result
}

pub(super) fn ensure_vector<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    val: ir::Value,
    on_vector: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value, ir::Block),
    slowpath: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value),
) {
    let bb_vector = ssa.builder.create_block();
    let bb_slow = ssa.builder.create_block();

    ssa.builder.func.layout.set_cold(bb_slow);

    ssa.branch_if_heap_class_id(val, builtin_class_ids::VECTOR, bb_vector, &[], bb_slow, &[]);
    ssa.builder.switch_to_block(bb_vector);
    {
        let length = ssa.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            val,
            offset_of!(Vector, length) as i32,
        );
        on_vector(ssa, length, bb_slow);
    }
    ssa.builder.switch_to_block(bb_slow);
    {
        slowpath(ssa, val);
    }
}

pub(super) fn fixnum_in_bounds_usize<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    ix: ir::Value,
    len: ir::Value,
    on_in_bounds: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value, ir::Block),
    slowpath: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value),
) {
    let fixnum_ix_block = ssa.builder.create_block();
    let bb_slowpath = ssa.builder.create_block();

    let is_int32 = ssa.is_int32(ix);
    ssa.builder
        .ins()
        .brif(is_int32, fixnum_ix_block, &[], bb_slowpath, &[]);
    ssa.builder.switch_to_block(fixnum_ix_block);
    {
        let ix = ssa.ireduce(types::I32, ix);
        let below0 = ssa.builder.ins().icmp_imm(IntCC::SignedLessThan, ix, 0);
        let check_bounds = ssa.builder.create_block();
        let in_bounds_block = ssa.builder.create_block();
        ssa.builder.append_block_param(in_bounds_block, types::I64);

        ssa.builder
            .ins()
            .brif(below0, bb_slowpath, &[], check_bounds, &[]);
        ssa.builder.switch_to_block(check_bounds);
        {
            let ix64 = ssa.zextend(types::I64, ix);
            let in_bounds = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, ix64, len);
            ssa.builder.ins().brif(
                in_bounds,
                in_bounds_block,
                &[BlockArg::Value(ix64)],
                bb_slowpath,
                &[],
            );
            ssa.builder.switch_to_block(in_bounds_block);
            {
                let ix64 = ssa.builder.block_params(in_bounds_block)[0];
                on_in_bounds(ssa, ix64, bb_slowpath);
            }
        }
    }

    ssa.builder.switch_to_block(bb_slowpath);
    {
        slowpath(ssa, ix);
    }
}
