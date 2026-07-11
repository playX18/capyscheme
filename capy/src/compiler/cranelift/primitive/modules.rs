use super::super::SsaBuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::rsgc::object::builtin_class_ids;
use crate::runtime::modules::Variable;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use std::mem::offset_of;

pub fn lower_variable_bound<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let val = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        arg,
        offset_of!(Variable, value) as i32,
    );
    let false_ = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::undefined().bits() as i64);
    let is_bound = ssa.builder.ins().icmp(IntCC::NotEqual, val, false_);

    PrimValue::Comparison(is_bound)
}

pub fn lower_variable_ref<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);

    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::new(),
        arg,
        offset_of!(Variable, value) as i32,
    ))
}

pub fn lower_variable_set<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let val = ssa.atom(args[1]);

    ssa.pre_write_barrier(arg, offset_of!(Variable, value) as i32, val);
    ssa.builder.ins().store(
        ir::MemFlags::new(),
        val,
        arg,
        offset_of!(Variable, value) as i32,
    );
    ssa.post_write_barrier(arg, offset_of!(Variable, value) as i32, val);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_make_box<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let call = ssa
        .builder
        .ins()
        .call(ssa.thunks.make_variable, &[ctx, arg]);

    PrimValue::Value(ssa.builder.inst_results(call)[0])
}

pub fn lower_ensure_local_var<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let module = ssa.atom(args[0]);
    let name = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let call = ssa.builder.ins().call(
        ssa.thunks.module_ensure_local_variable,
        &[ctx, module, name],
    );
    let var = ssa.builder.inst_results(call)[0];
    PrimValue::Value(var)
}

pub fn lower_lookup<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let module = ssa.atom(args[0]);
    let name = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

    PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup, &[ctx, module, name]))
}

pub fn lower_lookup_bound<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let module = ssa.atom(args[0]);
    let name = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

    PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup_bound, &[ctx, module, name]))
}

pub fn lower_lookup_bound_public<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let module = ssa.atom(args[0]);
    let name = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

    PrimValue::Value(
        ssa.handle_thunk_call_result(ssa.thunks.lookup_bound_public, &[ctx, module, name]),
    )
}

pub fn lower_lookup_bound_private<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let module = ssa.atom(args[0]);
    let name = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

    PrimValue::Value(
        ssa.handle_thunk_call_result(ssa.thunks.lookup_bound_private, &[ctx, module, name]),
    )
}

pub fn lower_current_module<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    if let Some(module) = args.first() {
        let module = ssa.atom(*module);
        let call = ssa
            .builder
            .ins()
            .call(ssa.thunks.set_current_module, &[ctx, module]);
        PrimValue::Value(ssa.builder.inst_results(call)[0])
    } else {
        let call = ssa.builder.ins().call(ssa.thunks.current_module, &[ctx]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    }
}

pub fn lower_define<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let key = ssa.atom(args[0]);
    let val = ssa.atom(args[1]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

    let call = ssa.builder.ins().call(ssa.thunks.define, &[ctx, key, val]);

    PrimValue::Value(ssa.builder.inst_results(call)[0])
}

pub fn lower_is_variable<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let is_variable = ssa.has_heap_class_id(val, builtin_class_ids::VARIABLE);
    PrimValue::Comparison(is_variable)
}
