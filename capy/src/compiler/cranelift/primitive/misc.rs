use super::super::SSABuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::runtime::Context;
use crate::runtime::State;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use std::mem::offset_of;


pub fn lower_not<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let false_ = ssa.builder.ins().iconst(types::I64, Value::new(false).bits() as i64);
        let val = ssa.builder.ins().icmp(IntCC::Equal, val, false_);



        PrimValue::Comparison(val)
    }

pub fn lower_make_tuple<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let size = ssa.atom(args[0]);
        let fill = if args.len() > 1 {
            ssa.atom(args[1])
        } else {
            ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64)
        };

        let result = ssa.handle_thunk_call_result(
            ssa.thunks.make_tuple,
            &[ctx, size, fill],
        );

        PrimValue::Value(result)
    }

pub fn lower_make_vector<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let size = ssa.atom(args[0]);
        let fill = if args.len() == 2 {
            ssa.atom(args[1])
        } else {
            ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64)
        };
        let result = ssa.handle_thunk_call_result(
            ssa.thunks.make_vector,
            &[ctx, size, fill],
        );

        PrimValue::Value(result)
    }

pub fn lower_string_length<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let str = ssa.atom(args[0]);
        let len = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), str, offset_of!(Str, length) as i32);
        let fixnum = ssa.builder.ins().bor_imm(len, Value::NUMBER_TAG);
        PrimValue::Value(fixnum)
    }

pub fn lower_integer_to_char<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.integer_to_char, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_char_to_integer<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.char_to_integer, &[ctx, val]);
        PrimValue::Value(result)
    }

pub fn lower_breakpoint<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    _args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        ssa.builder.ins().debugtrap();
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::new(true).bits() as i64))
    }

pub fn lower_symbol_to_string<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let sym = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.symbol2string, &[ctx, sym]);
        PrimValue::Value(result)
    }

pub fn lower_string_to_symbol<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let str = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.string2symbol, &[ctx, str]);
        PrimValue::Value(result)
    }

pub fn lower_push_cframe<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let key = ssa.atom(args[0]);
        let value = ssa.atom(args[1]);
        let retk = ssa.atom(args[2]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let res = ssa.builder.ins().call(ssa.thunks.push_cframe, &[ctx, key, value, retk]);
        PrimValue::Value(ssa.builder.inst_results(res)[0])
    }

pub fn lower_current_continuation_marks<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    _args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.builder.ins().call(ssa.thunks.current_continuation_marks, &[ctx]);
        PrimValue::Value(ssa.builder.inst_results(result)[0])
    }

pub fn lower_set_attachments<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let attachments = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let res = ssa.handle_thunk_call_result(ssa.thunks.set_attachments, &[ctx, attachments]);
        PrimValue::Value(res)
    }

pub fn lower_winders<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        if args.is_empty() {
            let winders = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), ctx, (Context::OFFSET_OF_STATE + offset_of!(State, winders))as i32);
            return PrimValue::Value(winders);
        }

        let new_winders = ssa.atom(args[0]);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_winders, ctx, (Context::OFFSET_OF_STATE + offset_of!(State, winders)) as i32);
        PrimValue::Value(new_winders)
    }

pub fn lower_make_syntax<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let exp = ssa.atom(args[0]);
        let wrap = ssa.atom(args[1]);
        let module = ssa.atom(args[2]);
        let source = ssa.atom(args[3]);
        let properties = ssa.atom(args[4]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.builder.ins().call(ssa.thunks.make_syntax, &[ctx, exp, wrap, module, source, properties]);
        PrimValue::Value(ssa.builder.inst_results(result)[0])
    }

pub fn lower_default_retk<'gc_, 'a, 'f>(
    ssa: &mut SSABuilder<'gc_, 'a, 'f>,
    _args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.builder.ins().call(ssa.thunks.default_retk, &[ctx]);
        PrimValue::Value(ssa.builder.inst_results(result)[0])
    }
