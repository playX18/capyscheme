use super::super::SsaBuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::heap::object::builtin_class_ids;
use crate::runtime::value::*;
use crate::runtime::vm::exceptions::RaiseKind;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use std::mem::offset_of;

pub fn lower_set_car<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    let new_car = ssa.atom(args[1]);
    ssa.pre_write_barrier(pair, offset_of!(Pair, car) as i32, new_car);
    ssa.builder.ins().store(
        ir::MemFlagsData::trusted(),
        new_car,
        pair,
        offset_of!(Pair, car) as i32,
    );
    ssa.post_write_barrier(pair, offset_of!(Pair, car) as i32, new_car);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_set_cdr<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    let new_cdr = ssa.atom(args[1]);
    ssa.pre_write_barrier(pair, offset_of!(Pair, cdr) as i32, new_cdr);
    ssa.builder.ins().store(
        ir::MemFlagsData::trusted(),
        new_cdr,
        pair,
        offset_of!(Pair, cdr) as i32,
    );
    ssa.post_write_barrier(pair, offset_of!(Pair, cdr) as i32, new_cdr);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_cons<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    assert!(args.len() == 2, "cons expects 2 arguments, got: {:?}", args);
    let car = ssa.atom(args[0]);
    let cdr = ssa.atom(args[1]);

    PrimValue::Value(ssa.cons(car, cdr))
}

pub fn lower_reverse<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let list = ssa.atom(args[0]);
    let ctx = ssa.ctx;
    let call = ssa.builder.ins().call(ssa.thunks.reverse, &[ctx, list]);

    PrimValue::Value(ssa.builder.inst_results(call)[0])
}

pub fn lower_is_eof_object<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let eof = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::eof().bits() as i64);
    let is_eof = ssa.builder.ins().icmp(IntCC::Equal, val, eof);
    PrimValue::Comparison(is_eof)
}

pub fn lower_is_null<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let null = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::null().bits() as i64);
    let is_null = ssa.builder.ins().icmp(IntCC::Equal, val, null);
    PrimValue::Comparison(is_null)
}

pub fn lower_unspec<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    _args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::undefined().bits() as i64);
    PrimValue::Value(val)
}

pub fn lower_is_unspecified<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let unspecified = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::undefined().bits() as i64);
    let is_unspecified = ssa.builder.ins().icmp(IntCC::Equal, val, unspecified);
    PrimValue::Comparison(is_unspecified)
}

pub fn lower_is_pair<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let is_pair = ssa.has_heap_class_id(val, builtin_class_ids::PAIR);
    PrimValue::Comparison(is_pair)
}

pub fn lower_is_list<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let result = ssa.builder.ins().call(ssa.thunks.listp, &[arg]);
    let result = ssa.builder.inst_results(result)[0];
    PrimValue::Comparison(result)
}

pub fn lower_append<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let ls1 = ssa.atom(args[0]);
    let ls2 = ssa.atom(args[1]);
    let ctx = ssa.ctx;
    let result = ssa.handle_thunk_call_result(ssa.thunks.append, &[ctx, ls1, ls2]);

    PrimValue::Value(result)
}

pub fn lower_list<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let args = args.iter().map(|x| ssa.atom(*x)).collect::<Vec<_>>();
    let mut ls = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::null().bits() as i64);
    for arg in args.iter().rev().copied() {
        ls = ssa.cons(arg, ls);
    }
    PrimValue::Value(ls)
}

pub fn lower_memq<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let item = ssa.atom(args[0]);
    let list = ssa.atom(args[1]);
    let ctx = ssa.ctx;
    let result = ssa.handle_thunk_call_result(ssa.thunks.memq, &[ctx, item, list]);
    PrimValue::Value(result)
}

pub fn lower_memv<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let item = ssa.atom(args[0]);
    let list = ssa.atom(args[1]);
    let ctx = ssa.ctx;
    let result = ssa.handle_thunk_call_result(ssa.thunks.memv, &[ctx, item, list]);
    PrimValue::Value(result)
}

pub fn lower_car<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);

    let is_pair = ssa.builder.create_block();
    let not_pair = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(not_pair);

    ssa.branch_if_heap_class_id(pair, builtin_class_ids::PAIR, is_pair, &[], not_pair, &[]);
    ssa.builder.switch_to_block(is_pair);

    ssa.builder.switch_to_block(not_pair);
    {
        ssa.emit_raise(RaiseKind::CarNotPair, &[pair], _source);
    }
    ssa.builder.switch_to_block(is_pair);

    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlagsData::trusted().with_can_move(),
        pair,
        offset_of!(Pair, car) as i32,
    ))
}

pub fn lower_cdr<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);

    let is_pair = ssa.builder.create_block();
    let not_pair = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(not_pair);

    ssa.branch_if_heap_class_id(pair, builtin_class_ids::PAIR, is_pair, &[], not_pair, &[]);
    ssa.builder.switch_to_block(is_pair);

    ssa.builder.switch_to_block(not_pair);
    {
        ssa.emit_raise(RaiseKind::CdrNotPair, &[pair], _source);
    }
    ssa.builder.switch_to_block(is_pair);

    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlagsData::trusted().with_can_move(),
        pair,
        offset_of!(Pair, cdr) as i32,
    ))
}

pub fn lower_length<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let ctx = ssa.ctx;

    let result = ssa.handle_thunk_call_result(ssa.thunks.length, &[ctx, val]);
    PrimValue::Value(result)
}
