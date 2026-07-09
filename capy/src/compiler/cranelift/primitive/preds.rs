use super::super::SSABuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::rsgc::object::builtin_class_ids;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir::BlockArg;


pub fn lower_is_procedure<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let is_proc = ssa.has_heap_class_id(val, builtin_class_ids::CLOSURE);
        PrimValue::Comparison(is_proc)
    }

pub fn lower_is_string<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.has_heap_class_id(val, builtin_class_ids::STRING))
    }

pub fn lower_is_boolean<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, (!1u64) as i64);
        PrimValue::Comparison(ssa.builder.ins().icmp_imm(IntCC::Equal, mask, Value::VALUE_FALSE))
    }

pub fn lower_is_symbol<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        PrimValue::Comparison(ssa.has_heap_class_id(val, builtin_class_ids::SYMBOL))
    }

pub fn lower_is_eq<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::Equal, x, y))
    }

pub fn lower_is_eqv<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.eqv, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    }

pub fn lower_is_equal<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.equal, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    }

pub fn lower_is_exact_integer<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.exact_integerp, &[ctx, val]);


        PrimValue::Value(check)
    }

pub fn lower_is_integer<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.builder.ins().call(ssa.thunks.integerp, &[ctx, val]);
        PrimValue::Comparison(ssa.builder.inst_results(check)[0])
    }

pub fn lower_is_char<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().iconst(types::I64, Value::CHAR_MASK);
        let tag = ssa.builder.ins().iconst(types::I64, Value::CHAR_TAG);
        if ssa.builder.func.dfg.value_type(val) != types::I64 {
            unreachable!()
        }
        let masked = ssa.builder.ins().band(val, mask);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, masked, tag);
        PrimValue::Comparison(cmp)
    }

pub fn lower_is_number<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::NUMBER_TAG);
        let is_inline_num = ssa.builder.ins().icmp_imm(IntCC::NotEqual, mask, 0);
        let succ = ssa.builder.create_block();
        let check_heap = ssa.builder.create_block();

        ssa.builder.append_block_param(succ, types::I8);
        ssa.builder.ins().brif(is_inline_num, succ, &[BlockArg::Value(is_inline_num)], check_heap, &[]);
        ssa.builder.switch_to_block(check_heap);
        {
            let check = ssa.has_any_heap_class_id(val, &[
                builtin_class_ids::BIGINT,
                builtin_class_ids::RATIONAL,
                builtin_class_ids::COMPLEX,
            ]);
            ssa.builder.ins().jump(succ, &[BlockArg::Value(check)]);
        }

        ssa.builder.switch_to_block(succ);
        PrimValue::Comparison(ssa.builder.block_params(succ)[0])
    }

pub fn lower_is_complex<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let res = ssa.builder.ins().call(ssa.thunks.complexp, &[val]);
        PrimValue::Comparison(ssa.builder.inst_results(res)[0])
    }

pub fn lower_is_nan<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.nanp, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_is_real<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);


        let call = ssa.builder.ins().call(ssa.thunks.realp, &[val]);
        let check = ssa.builder.inst_results(call)[0];
        PrimValue::Comparison(check)
    }

pub fn lower_is_rational<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);

        let check = ssa.builder.ins().call(ssa.thunks.is_rational, &[val]);
        PrimValue::Comparison(ssa.builder.inst_results(check)[0])
    }

pub fn lower_is_inexact<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.inexactp, &[ctx, val]);
        PrimValue::Value(check)
    }

pub fn lower_is_exact<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.exactp, &[ctx, val]);
        PrimValue::Value(check)
    }
