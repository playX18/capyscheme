use super::super::{AllocationHeaderPreset, SsaBuilder};
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::heap::object::builtin_class_ids;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use std::mem::offset_of;

pub fn lower_is_vector<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let is_vector = ssa.has_heap_class_id(val, builtin_class_ids::VECTOR);

    PrimValue::Comparison(is_vector)
}

pub fn lower_is_bytevector<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    let is_bv = ssa.has_any_heap_class_id(
        val,
        &[
            builtin_class_ids::MUTABLE_BYTEVECTOR,
            builtin_class_ids::IMMUTABLE_BYTEVECTOR,
            builtin_class_ids::MAPPED_BYTEVECTOR,
        ],
    );
    PrimValue::Comparison(is_bv)
}

pub fn lower_vector<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let size = size_of::<Vector>() as i64 + args.len() as i64 * size_of::<Value>() as i64;
    let vec = ssa.alloc_with_header_word_preset(
        AllocationHeaderPreset::MutableVector,
        size as usize,
        None,
    );
    let len = ssa.builder.ins().iconst(types::I64, args.len() as i64);
    ssa.builder.ins().store(
        ir::MemFlagsData::trusted(),
        len,
        vec,
        offset_of!(Vector, length) as i32,
    );
    for (i, &arg) in args.iter().enumerate() {
        let arg = ssa.atom(arg);
        ssa.builder.ins().store(
            ir::MemFlagsData::trusted(),
            arg,
            vec,
            offset_of!(Vector, data) as i32 + i as i32 * size_of::<Value>() as i32,
        );
    }

    PrimValue::Value(vec)
}

pub fn lower_tuple<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let PrimValue::Value(tup) = super::misc::lower_make_tuple(
        ssa,
        &[Atom::Constant(Value::new(args.len() as i32))],
        _source,
    ) else {
        panic!(
            "tuple: make-tuple failed to return a Value for {} elements",
            args.len()
        )
    };

    for (i, &arg) in args.iter().enumerate() {
        let arg = ssa.atom(arg);
        ssa.builder.ins().store(
            ir::MemFlagsData::trusted(),
            arg,
            tup,
            offset_of!(Tuple, data) as i32 + i as i32 * size_of::<Value>() as i32,
        );
    }

    PrimValue::Value(tup)
}

pub fn lower_vector_ref<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let vec = ssa.atom(args[0]);
    let ix = ssa.atom(args[1]);

    let merge = ssa.builder.create_block();
    ssa.builder.append_block_param(merge, types::I64);

    super::arith::ensure_vector(
        ssa,
        vec,
        |ssa, ulen, slowpath| {
            super::arith::fixnum_in_bounds_usize(
                ssa,
                ix,
                ulen,
                |ssa, ix, _slowpath| {
                    let ix_offset = ssa.builder.ins().imul_imm_s(ix, size_of::<Value>() as i64);
                    let data_ptr = ssa
                        .builder
                        .ins()
                        .iadd_imm_s(vec, offset_of!(Vector, data) as i64);
                    let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
                    let elem = ssa.builder.ins().load(
                        types::I64,
                        ir::MemFlagsData::trusted().with_can_move(),
                        elem_ptr,
                        0,
                    );
                    ssa.builder.ins().jump(merge, &[BlockArg::Value(elem)]);
                },
                |ssa, _| {
                    ssa.builder.ins().jump(slowpath, &[]);
                },
            );
        },
        |ssa, _| {
            let ctx = ssa.ctx;
            let result = ssa.handle_thunk_call_result(ssa.thunks.vector_ref, &[ctx, vec, ix]);
            ssa.builder.ins().jump(merge, &[BlockArg::Value(result)]);
        },
    );
    ssa.builder.switch_to_block(merge);
    let result = ssa.builder.block_params(merge)[0];
    PrimValue::Value(result)
}

pub fn lower_vector_set<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let vec = ssa.atom(args[0]);
    let ix = ssa.atom(args[1]);
    let new_val = ssa.atom(args[2]);

    let merge = ssa.builder.create_block();

    super::arith::ensure_vector(
        ssa,
        vec,
        |ssa, ulen, slowpath| {
            super::arith::fixnum_in_bounds_usize(
                ssa,
                ix,
                ulen,
                |ssa, ix, _slowpath| {
                    let ix_offset = ssa.builder.ins().imul_imm_s(ix, size_of::<Value>() as i64);
                    let data_ptr = ssa
                        .builder
                        .ins()
                        .iadd_imm_s(vec, offset_of!(Vector, data) as i64);
                    let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
                    ssa.pre_write_barrier_n(vec, elem_ptr, new_val);
                    ssa.builder
                        .ins()
                        .store(ir::MemFlagsData::trusted(), new_val, elem_ptr, 0);
                    ssa.post_write_barrier_n(vec, elem_ptr, new_val);
                    ssa.builder.ins().jump(merge, &[]);
                },
                |ssa, _| {
                    ssa.builder.ins().jump(slowpath, &[]);
                },
            );
        },
        |ssa, _| {
            let ctx = ssa.ctx;
            let _ = ssa.handle_thunk_call_result(ssa.thunks.vector_set, &[ctx, vec, ix, new_val]);
            ssa.builder.ins().jump(merge, &[]);
        },
    );
    ssa.builder.switch_to_block(merge);

    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_tuple_size<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let arg = ssa.atom(args[0]);
    let ctx = ssa.ctx;
    let size = ssa.handle_thunk_call_result(ssa.thunks.tuple_size, &[ctx, arg]);
    PrimValue::Value(size)
}

pub fn lower_tuple_ref<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let tuple = ssa.atom(args[0]);
    let ix = ssa.atom(args[1]);

    let ix = ssa.ireduce(types::I32, ix);
    let offset = ssa.builder.ins().imul_imm_s(ix, size_of::<Value>() as i64);
    let offset = ssa
        .builder
        .ins()
        .iadd_imm_s(offset, offset_of!(Tuple, data) as i32 as i64);
    let offset = ssa.zextend(types::I64, offset);
    let addr = ssa.builder.ins().iadd(tuple, offset);
    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlagsData::trusted().with_can_move(),
        addr,
        0,
    ))
}

pub fn lower_tuple_set<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let tuple = ssa.atom(args[0]);
    let ix = ssa.atom(args[1]);
    let value = ssa.atom(args[2]);

    let ix = ssa.ireduce(types::I32, ix);
    let offset = ssa.builder.ins().imul_imm_s(ix, size_of::<Value>() as i64);
    let offset = ssa
        .builder
        .ins()
        .iadd_imm_s(offset, offset_of!(Tuple, data) as i32 as i64);
    let offset = ssa.zextend(types::I64, offset);
    let addr = ssa.builder.ins().iadd(tuple, offset);
    ssa.pre_write_barrier_n(tuple, addr, value);
    ssa.builder
        .ins()
        .store(ir::MemFlagsData::trusted(), value, addr, 0);
    ssa.post_write_barrier_n(tuple, addr, value);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_is_tuple<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let val = ssa.atom(args[0]);

    let res = ssa.has_heap_class_id(val, builtin_class_ids::TUPLE);

    PrimValue::Comparison(res)
}

pub fn lower_bytevector_length<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let bv = ssa.atom(args[0]);
    let ctx = ssa.ctx;
    let result = ssa.handle_thunk_call_result(ssa.thunks.bytevector_length, &[ctx, bv]);
    PrimValue::Value(result)
}

pub fn lower_bytevector_u8_ref<'gc_, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
    args: &[Atom<'gc_>],
    _source: Value<'gc_>,
) -> PrimValue {
    let bv = ssa.atom(args[0]);
    let ix = ssa.atom(args[1]);

    let merge = ssa.builder.create_block();
    ssa.builder.append_block_param(merge, types::I64);

    super::arith::ensure_bytevector(
        ssa,
        bv,
        |ssa, ulen, slowpath| {
            super::arith::fixnum_in_bounds_usize(
                ssa,
                ix,
                ulen,
                |ssa, ix, _slowpath| {
                    let data_base = ssa
                        .builder
                        .ins()
                        .iadd_imm_s(bv, size_of::<ByteVector>() as i64);
                    let addr = ssa.builder.ins().iadd(data_base, ix);
                    let byte = ssa.builder.ins().load(
                        types::I8,
                        ir::MemFlagsData::trusted().with_can_move(),
                        addr,
                        0,
                    );
                    let byte_i64 = ssa.builder.ins().uextend(types::I64, byte);
                    let result = ssa.builder.ins().bor_imm_u(byte_i64, Value::NUMBER_TAG);
                    ssa.builder.ins().jump(merge, &[BlockArg::Value(result)]);
                },
                |ssa, _| {
                    ssa.builder.ins().jump(slowpath, &[]);
                },
            );
        },
        |ssa, _| {
            let ctx = ssa.ctx;
            let result = ssa.handle_thunk_call_result(ssa.thunks.bytevector_u8_ref, &[ctx, bv, ix]);
            ssa.builder.ins().jump(merge, &[BlockArg::Value(result)]);
        },
    );
    ssa.builder.switch_to_block(merge);
    PrimValue::Value(ssa.builder.block_params(merge)[0])
}
