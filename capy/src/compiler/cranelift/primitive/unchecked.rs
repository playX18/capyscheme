//! Unchecked and overflow-checked primitive lowerings for SBBV.

use super::super::SsaBuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::rsgc::object::builtin_class_ids;
use crate::runtime::value::*;
use cranelift::prelude::FloatCC;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::MemFlags;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use std::mem::offset_of;

fn fixnum_i32<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
    ssa.builder.ins().ireduce(types::I32, v)
}

fn fixnum_from_i32<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
    // Fixnums are NaN-boxed: low 32 bits hold the i32, high bits are NUMBER_TAG.
    // Matching `Value::from(i32)` / `translate` / `lowlevel` — a bare sextend
    // yields raw 0 for sum 0, which prints as `#<unknown 0>` and fails `number?`.
    let wide = ssa.builder.ins().sextend(types::I64, v);
    ssa.builder.ins().bor_imm(wide, Value::NUMBER_TAG)
}

fn flonum_f64<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
    let f64_encode_off = ssa
        .builder
        .ins()
        .iconst(types::I64, Value::DOUBLE_ENCODE_OFFSET as i64);
    let value_bits = ssa.builder.ins().isub(v, f64_encode_off);
    ssa.builder
        .ins()
        .bitcast(types::F64, MemFlags::new(), value_bits)
}

fn flonum_from_f64<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
    let bits = ssa.builder.ins().bitcast(types::I64, MemFlags::new(), v);
    ssa.builder
        .ins()
        .iadd_imm(bits, Value::DOUBLE_ENCODE_OFFSET as i64)
}

fn false_value<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value {
    ssa.builder
        .ins()
        .iconst(types::I64, Value::new(false).bits() as i64)
}

pub fn lower_is_fixnum<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    PrimValue::Comparison(ssa.is_int32(val))
}

pub fn lower_is_flonum<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let val = ssa.atom(args[0]);
    PrimValue::Comparison(ssa.is_flonum(val))
}

fn atom_pair<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
) -> (ir::Value, ir::Value) {
    let a = ssa.atom(args[0]);
    let b = ssa.atom(args[1]);
    (a, b)
}

pub fn lower_fx_add_ovf<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let (sum, of) = ssa.builder.ins().sadd_overflow(lhs, rhs);
    let ok = fixnum_from_i32(ssa, sum);
    let false_val = false_value(ssa);
    let result = ssa.builder.ins().select(of, false_val, ok);
    PrimValue::Value(result)
}

pub fn lower_fx_sub_ovf<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let (diff, of) = ssa.builder.ins().ssub_overflow(lhs, rhs);
    let ok = fixnum_from_i32(ssa, diff);
    let false_val = false_value(ssa);
    let result = ssa.builder.ins().select(of, false_val, ok);
    PrimValue::Value(result)
}

pub fn lower_fx_mul_ovf<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let (prod, of) = ssa.builder.ins().smul_overflow(lhs, rhs);
    let ok = fixnum_from_i32(ssa, prod);
    let false_val = false_value(ssa);
    let result = ssa.builder.ins().select(of, false_val, ok);
    PrimValue::Value(result)
}

pub fn lower_fx_add<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let sum = ssa.builder.ins().iadd(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, sum))
}

pub fn lower_fx_sub<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let diff = ssa.builder.ins().isub(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, diff))
}

pub fn lower_fx_mul<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let prod = ssa.builder.ins().imul(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, prod))
}

macro_rules! fx_cmp {
    ($name:ident, $cc:expr) => {
        pub fn $name<'gc, 'a, 'f>(
            ssa: &mut SsaBuilder<'gc, 'a, 'f>,
            args: &[Atom<'gc>],
            _source: Value<'gc>,
        ) -> PrimValue {
            let (a0, a1) = atom_pair(ssa, args);
            let lhs = fixnum_i32(ssa, a0);
            let rhs = fixnum_i32(ssa, a1);
            PrimValue::Comparison(ssa.builder.ins().icmp($cc, lhs, rhs))
        }
    };
}

fx_cmp!(lower_fx_lt, IntCC::SignedLessThan);
fx_cmp!(lower_fx_le, IntCC::SignedLessThanOrEqual);
fx_cmp!(lower_fx_gt, IntCC::SignedGreaterThan);
fx_cmp!(lower_fx_ge, IntCC::SignedGreaterThanOrEqual);
fx_cmp!(lower_fx_eq_unchecked, IntCC::Equal);

pub fn lower_fx_quotient<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let quot = ssa.builder.ins().sdiv(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, quot))
}

pub fn lower_fx_remainder<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let rem = ssa.builder.ins().srem(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, rem))
}

pub fn lower_fl_sqrt<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let arg = flonum_f64(ssa, raw);
    let result = ssa.builder.ins().sqrt(arg);
    PrimValue::Value(flonum_from_f64(ssa, result))
}

pub fn lower_fl_atan<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.atan, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_add<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let sum = ssa.builder.ins().fadd(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, sum))
}

pub fn lower_fl_sub<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let diff = ssa.builder.ins().fsub(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, diff))
}

pub fn lower_fl_mul<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let prod = ssa.builder.ins().fmul(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, prod))
}

pub fn lower_fl_div<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let quot = ssa.builder.ins().fdiv(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, quot))
}

macro_rules! fl_cmp {
    ($name:ident, $cc:expr) => {
        pub fn $name<'gc, 'a, 'f>(
            ssa: &mut SsaBuilder<'gc, 'a, 'f>,
            args: &[Atom<'gc>],
            _source: Value<'gc>,
        ) -> PrimValue {
            let (a0, a1) = atom_pair(ssa, args);
            let lhs = flonum_f64(ssa, a0);
            let rhs = flonum_f64(ssa, a1);
            PrimValue::Comparison(ssa.builder.ins().fcmp($cc, lhs, rhs))
        }
    };
}

fl_cmp!(lower_fl_lt, FloatCC::LessThan);
fl_cmp!(lower_fl_le, FloatCC::LessThanOrEqual);
fl_cmp!(lower_fl_gt, FloatCC::GreaterThan);
fl_cmp!(lower_fl_ge, FloatCC::GreaterThanOrEqual);
fl_cmp!(lower_fl_eq, FloatCC::Equal);

pub fn lower_car_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        pair,
        offset_of!(Pair, car) as i32,
    ))
}

pub fn lower_cdr_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    PrimValue::Value(ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        pair,
        offset_of!(Pair, cdr) as i32,
    ))
}

pub fn lower_set_car_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    let new_car = ssa.atom(args[1]);
    ssa.pre_write_barrier(pair, offset_of!(Pair, car) as i32, new_car);
    ssa.builder.ins().store(
        ir::MemFlags::trusted(),
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

pub fn lower_set_cdr_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let pair = ssa.atom(args[0]);
    let new_cdr = ssa.atom(args[1]);
    ssa.pre_write_barrier(pair, offset_of!(Pair, cdr) as i32, new_cdr);
    ssa.builder.ins().store(
        ir::MemFlags::trusted(),
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

pub fn lower_vector_length_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let vec = ssa.atom(args[0]);
    let len = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        vec,
        offset_of!(Vector, length) as i32,
    );
    PrimValue::Value(len)
}

pub fn lower_vector_ref_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let vec = ssa.atom(args[0]);
    let ix_raw = ssa.atom(args[1]);
    // Match the checked path: reduce to i32 then widen before address math.
    let ix32 = fixnum_i32(ssa, ix_raw);
    let ix = ssa.zextend(types::I64, ix32);
    let ix_offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
    let data_ptr = ssa
        .builder
        .ins()
        .iadd_imm(vec, offset_of!(Vector, data) as i64);
    let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
    let elem = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        elem_ptr,
        0,
    );
    PrimValue::Value(elem)
}

pub fn lower_vector_set_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let vec = ssa.atom(args[0]);
    let ix_raw = ssa.atom(args[1]);
    let ix32 = fixnum_i32(ssa, ix_raw);
    let ix = ssa.zextend(types::I64, ix32);
    let val = ssa.atom(args[2]);
    let ix_offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
    let data_ptr = ssa
        .builder
        .ins()
        .iadd_imm(vec, offset_of!(Vector, data) as i64);
    let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
    ssa.pre_write_barrier(vec, offset_of!(Vector, data) as i32, val);
    ssa.builder
        .ins()
        .store(ir::MemFlags::trusted(), val, elem_ptr, 0);
    ssa.post_write_barrier(vec, offset_of!(Vector, data) as i32, val);
    PrimValue::Value(
        ssa.builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64),
    )
}

pub fn lower_char_to_int_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let ch = ssa.atom(args[0]);
    let code = ssa.builder.ins().ushr_imm(ch, 16);
    let code_i32 = ssa.builder.ins().ireduce(types::I32, code);
    PrimValue::Value(fixnum_from_i32(ssa, code_i32))
}

pub fn lower_string_length_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    super::misc::lower_string_length(ssa, args, _source)
}

pub fn lower_string_ref_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let str_val = ssa.atom(args[0]);
    let index_raw = ssa.atom(args[1]);

    let stringbuf = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        str_val,
        offset_of!(Str, stringbuf) as i32,
    );
    let start = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        str_val,
        offset_of!(Str, start) as i32,
    );
    let index = fixnum_i32(ssa, index_raw);
    let index_i64 = ssa.builder.ins().sextend(types::I64, index);
    let abs_ix = ssa.builder.ins().iadd(start, index_i64);
    let data_base = ssa
        .builder
        .ins()
        .iadd_imm(stringbuf, size_of::<Stringbuf>() as i64);

    let narrow_bb = ssa.builder.create_block();
    let wide_bb = ssa.builder.create_block();
    let join = ssa.builder.create_block();
    ssa.builder.append_block_param(join, types::I64);

    let is_narrow = ssa.has_heap_class_id(stringbuf, builtin_class_ids::STRINGBUF_NARROW);
    ssa.builder
        .ins()
        .brif(is_narrow, narrow_bb, &[], wide_bb, &[]);

    ssa.builder.switch_to_block(narrow_bb);
    {
        let addr = ssa.builder.ins().iadd(data_base, abs_ix);
        let ch_byte =
            ssa.builder
                .ins()
                .load(types::I8, ir::MemFlags::trusted().with_can_move(), addr, 0);
        let ch_i64 = ssa.builder.ins().uextend(types::I64, ch_byte);
        let shifted = ssa.builder.ins().ishl_imm(ch_i64, 16);
        let char_tag = ssa.builder.ins().iconst(types::I64, Value::CHAR_TAG);
        let result = ssa.builder.ins().bor(shifted, char_tag);
        ssa.builder.ins().jump(join, &[BlockArg::Value(result)]);
    }

    ssa.builder.switch_to_block(wide_bb);
    {
        let wide_off = ssa.builder.ins().imul_imm(abs_ix, size_of::<char>() as i64);
        let addr = ssa.builder.ins().iadd(data_base, wide_off);
        let ch_i32 =
            ssa.builder
                .ins()
                .load(types::I32, ir::MemFlags::trusted().with_can_move(), addr, 0);
        let ch_i64 = ssa.builder.ins().uextend(types::I64, ch_i32);
        let shifted = ssa.builder.ins().ishl_imm(ch_i64, 16);
        let char_tag = ssa.builder.ins().iconst(types::I64, Value::CHAR_TAG);
        let result = ssa.builder.ins().bor(shifted, char_tag);
        ssa.builder.ins().jump(join, &[BlockArg::Value(result)]);
    }

    ssa.builder.switch_to_block(join);
    PrimValue::Value(ssa.builder.block_params(join)[0])
}

pub fn lower_bytevector_length_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let bv = ssa.atom(args[0]);
    let len = ssa.builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        bv,
        offset_of!(ByteVector, len) as i32,
    );
    let len_i32 = ssa.builder.ins().ireduce(types::I32, len);
    PrimValue::Value(fixnum_from_i32(ssa, len_i32))
}

pub fn lower_bytevector_u8_ref_unchecked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let bv = ssa.atom(args[0]);
    let index_raw = ssa.atom(args[1]);
    let index = fixnum_i32(ssa, index_raw);
    let index_i64 = ssa.builder.ins().sextend(types::I64, index);
    let data_base = ssa
        .builder
        .ins()
        .iadd_imm(bv, size_of::<ByteVector>() as i64);
    let addr = ssa.builder.ins().iadd(data_base, index_i64);
    let byte = ssa
        .builder
        .ins()
        .load(types::I8, ir::MemFlags::trusted().with_can_move(), addr, 0);
    let byte_i64 = ssa.builder.ins().uextend(types::I64, byte);
    PrimValue::Value(ssa.builder.ins().bor_imm(byte_i64, Value::NUMBER_TAG))
}
