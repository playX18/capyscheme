use super::super::SsaBuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::types;
use cranelift_codegen::ir;

pub fn lower_class_idp<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let Atom::Constant(class_id) = args[1] else {
        panic!(
            "invalid %class-id?: expected constant class ID, got {:?}",
            args[1]
        )
    };
    let Some(class_id) = class_id.int32() else {
        panic!("invalid %class-id?: expected fixnum class ID, got {class_id}")
    };

    PrimValue::Comparison(ssa.has_specific_class_id(val, class_id as u32))
}

pub fn lower_refptr<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let offset = ssa.atom(args[1]);
    let i32_offset = ssa.ireduce(types::I32, offset);
    let offset = ssa.zextend(types::I64, i32_offset);
    let addr = ssa.builder.ins().iadd(val, offset);
    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlagsData::trusted().with_can_move(),
        addr,
        0,
    ))
}

pub fn lower_usize_to_value<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let val = ssa.ireduce(types::I32, val);
    let val = ssa.zextend(types::I64, val);
    let val = ssa.builder.ins().bor_imm_u(val, Value::NUMBER_TAG);
    PrimValue::Value(val)
}

pub fn lower_cache_ref<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    assert_eq!(args.len(), 1);

    let Atom::Constant(cache_key) = args[0] else {
        panic!(
            "invalid cache-ref: expected Atom::Constant, got {:?}",
            args[0]
        )
    };

    let cell = ssa.module_builder.intern_cache_cell(cache_key);
    PrimValue::Value(ssa.load_data_value(cell))
}

pub fn lower_cache_set<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    assert_eq!(args.len(), 2);

    let Atom::Constant(cache_key) = args[0] else {
        panic!(
            "invalid cache-set!: expected Atom::Constant, got {:?}",
            args[0]
        )
    };

    let value = ssa.atom(args[1]);

    let cell = ssa.module_builder.intern_cache_cell(cache_key);
    ssa.store_data_value(cell, value);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_is_immediate<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);

    PrimValue::Comparison(ssa.is_immediate(arg))
}

pub fn lower_is_heap_object<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let x = ssa.is_heap_object(arg);

    PrimValue::Comparison(x)
}
