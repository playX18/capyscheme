//! Unchecked and overflow-checked primitive lowerings for SBBV.

use super::super::SsaBuilder;
use super::PrimValue;
use crate::compiler::cps::graph::Atom;
use crate::rsgc::object::builtin_class_ids;
use crate::runtime::value::*;
use crate::runtime::vm::exceptions::RaiseKind;
use cranelift::prelude::FloatCC;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::MemFlags;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use cranelift_codegen::ir::immediates::Ieee64;
use std::mem::offset_of;
use std::mem::size_of;

fn fixnum_i32<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
    ssa.builder.ins().ireduce(types::I32, v)
}

fn fixnum_from_i32<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, v: ir::Value) -> ir::Value {
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

fn true_value<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>) -> ir::Value {
    ssa.builder
        .ins()
        .iconst(types::I64, Value::new(true).bits() as i64)
}

fn bool_from_i8<'gc, 'a, 'f>(ssa: &mut SsaBuilder<'gc, 'a, 'f>, cond: ir::Value) -> ir::Value {
    let t = true_value(ssa);
    let f = false_value(ssa);
    ssa.builder.ins().select(cond, t, f)
}

fn atom_pair<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
) -> (ir::Value, ir::Value) {
    let a = ssa.atom(args[0]);
    let b = ssa.atom(args[1]);
    (a, b)
}

/// Type-check both args as fixnums; raise on failure, then run `body` on the hot path.
fn with_fixnums_2<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
    body: impl FnOnce(&mut SsaBuilder<'gc, 'a, 'f>, ir::Value, ir::Value) -> PrimValue,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let ok0 = ssa.builder.create_block();
    let ok1 = ssa.builder.create_block();
    let bad = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(bad);
    ssa.branch_if_int32(a0, ok0, &[], bad, &[]);
    ssa.builder.switch_to_block(bad);
    {
        ssa.emit_raise(RaiseKind::AssertionViolation, &[a0], source);
    }
    ssa.builder.switch_to_block(ok0);
    ssa.branch_if_int32(a1, ok1, &[], bad, &[]);
    ssa.builder.switch_to_block(ok1);
    body(ssa, a0, a1)
}

fn with_fixnum_1<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
    body: impl FnOnce(&mut SsaBuilder<'gc, 'a, 'f>, ir::Value) -> PrimValue,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let ok = ssa.builder.create_block();
    let bad = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(bad);
    ssa.branch_if_int32(a0, ok, &[], bad, &[]);
    ssa.builder.switch_to_block(bad);
    {
        ssa.emit_raise(RaiseKind::AssertionViolation, &[a0], source);
    }
    ssa.builder.switch_to_block(ok);
    body(ssa, a0)
}

fn with_flonums_2<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
    body: impl FnOnce(&mut SsaBuilder<'gc, 'a, 'f>, ir::Value, ir::Value) -> PrimValue,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let ok0 = ssa.builder.create_block();
    let ok1 = ssa.builder.create_block();
    let bad = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(bad);
    let is0 = ssa.is_flonum(a0);
    ssa.builder.ins().brif(is0, ok0, &[], bad, &[]);
    ssa.builder.switch_to_block(bad);
    {
        ssa.emit_raise(RaiseKind::AssertionViolation, &[a0], source);
    }
    ssa.builder.switch_to_block(ok0);
    let is1 = ssa.is_flonum(a1);
    ssa.builder.ins().brif(is1, ok1, &[], bad, &[]);
    ssa.builder.switch_to_block(ok1);
    body(ssa, a0, a1)
}

fn with_flonum_1<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
    body: impl FnOnce(&mut SsaBuilder<'gc, 'a, 'f>, ir::Value) -> PrimValue,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let ok = ssa.builder.create_block();
    let bad = ssa.builder.create_block();
    ssa.builder.func.layout.set_cold(bad);
    let is_f = ssa.is_flonum(a0);
    ssa.builder.ins().brif(is_f, ok, &[], bad, &[]);
    ssa.builder.switch_to_block(bad);
    {
        ssa.emit_raise(RaiseKind::AssertionViolation, &[a0], source);
    }
    ssa.builder.switch_to_block(ok);
    body(ssa, a0)
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

pub fn lower_fx_modulo<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let rem = ssa.builder.ins().srem(lhs, rhs);
    let zero = ssa.builder.ins().iconst(types::I32, 0);
    let is_zero = ssa.builder.ins().icmp(IntCC::Equal, rem, zero);
    let rem_pos = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, rem, zero);
    let rhs_pos = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, rhs, zero);
    let signs_differ = ssa.builder.ins().bxor(rem_pos, rhs_pos);
    let adjusted = ssa.builder.ins().iadd(rem, rhs);
    let with_adjust = ssa.builder.ins().select(signs_differ, adjusted, rem);
    let result = ssa.builder.ins().select(is_zero, zero, with_adjust);
    PrimValue::Value(fixnum_from_i32(ssa, result))
}


pub fn lower_fx_add_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().iadd(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_sub_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().isub(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_mul_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().imul(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_add_ovf_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let (r, of) = ssa.builder.ins().sadd_overflow(lhs, rhs);
        let ok = fixnum_from_i32(ssa, r);
        let false_val = false_value(ssa);
        PrimValue::Value(ssa.builder.ins().select(of, false_val, ok))
    })
}

pub fn lower_fx_sub_ovf_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let (r, of) = ssa.builder.ins().ssub_overflow(lhs, rhs);
        let ok = fixnum_from_i32(ssa, r);
        let false_val = false_value(ssa);
        PrimValue::Value(ssa.builder.ins().select(of, false_val, ok))
    })
}

pub fn lower_fx_mul_ovf_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let (r, of) = ssa.builder.ins().smul_overflow(lhs, rhs);
        let ok = fixnum_from_i32(ssa, r);
        let false_val = false_value(ssa);
        PrimValue::Value(ssa.builder.ins().select(of, false_val, ok))
    })
}

pub fn lower_fx_lt_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs))
    })
}

pub fn lower_fx_le_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs))
    })
}

pub fn lower_fx_gt_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs))
    })
}

pub fn lower_fx_ge_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs))
    })
}

pub fn lower_fx_and<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let r = ssa.builder.ins().band(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_ior<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let r = ssa.builder.ins().bor(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_xor<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let r = ssa.builder.ins().bxor(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_not<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let r = ssa.builder.ins().bnot(x);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_ashl<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let r = ssa.builder.ins().ishl(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_ashr<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let r = ssa.builder.ins().sshr(lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}

pub fn lower_fx_and_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().band(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_ior_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().bor(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_xor_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().bxor(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_not_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let r = ssa.builder.ins().bnot(x);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}
pub fn lower_fx_ashl_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().ishl(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}
pub fn lower_fx_ashr_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let r = ssa.builder.ins().sshr(lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_zero<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let z = ssa.builder.ins().icmp_imm(IntCC::Equal, x, 0);
    PrimValue::Comparison(z)
}
pub fn lower_fx_positive<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let z = ssa.builder.ins().icmp_imm(IntCC::SignedGreaterThan, x, 0);
    PrimValue::Comparison(z)
}
pub fn lower_fx_negative<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let z = ssa.builder.ins().icmp_imm(IntCC::SignedLessThan, x, 0);
    PrimValue::Comparison(z)
}
pub fn lower_fx_odd<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let masked = ssa.builder.ins().band_imm(x, 1);
    let z = ssa.builder.ins().icmp_imm(IntCC::Equal, masked, 1);
    PrimValue::Comparison(z)
}
pub fn lower_fx_even<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let a0 = ssa.atom(args[0]);
    let x = fixnum_i32(ssa, a0);
    let masked = ssa.builder.ins().band_imm(x, 1);
    let z = ssa.builder.ins().icmp_imm(IntCC::Equal, masked, 0);
    PrimValue::Comparison(z)
}
pub fn lower_fx_min<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let lt = ssa.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
    let r = ssa.builder.ins().select(lt, lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}
pub fn lower_fx_max<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = fixnum_i32(ssa, a0);
    let rhs = fixnum_i32(ssa, a1);
    let gt = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs);
    let r = ssa.builder.ins().select(gt, lhs, rhs);
    PrimValue::Value(fixnum_from_i32(ssa, r))
}

pub fn lower_fx_zero_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let z = ssa.builder.ins().icmp_imm(IntCC::Equal, x, 0);
        PrimValue::Comparison(z)
    })
}

pub fn lower_fx_positive_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let z = ssa.builder.ins().icmp_imm(IntCC::SignedGreaterThan, x, 0);
        PrimValue::Comparison(z)
    })
}

pub fn lower_fx_negative_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let z = ssa.builder.ins().icmp_imm(IntCC::SignedLessThan, x, 0);
        PrimValue::Comparison(z)
    })
}

pub fn lower_fx_odd_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let masked = ssa.builder.ins().band_imm(x, 1);
        let z = ssa.builder.ins().icmp_imm(IntCC::Equal, masked, 1);
        PrimValue::Comparison(z)
    })
}

pub fn lower_fx_even_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnum_1(ssa, args, source, |ssa, a0| {
        let x = fixnum_i32(ssa, a0);
        let masked = ssa.builder.ins().band_imm(x, 1);
        let z = ssa.builder.ins().icmp_imm(IntCC::Equal, masked, 0);
        PrimValue::Comparison(z)
    })
}

pub fn lower_fx_min_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let lt = ssa.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
        let r = ssa.builder.ins().select(lt, lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
}

pub fn lower_fx_max_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_fixnums_2(ssa, args, source, |ssa, a0, a1| {
        let lhs = fixnum_i32(ssa, a0);
        let rhs = fixnum_i32(ssa, a1);
        let gt = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs);
        let r = ssa.builder.ins().select(gt, lhs, rhs);
        PrimValue::Value(fixnum_from_i32(ssa, r))
    })
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

pub fn lower_fl_add_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_add(ssa, args, source))
}

pub fn lower_fl_sub_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_sub(ssa, args, source))
}

pub fn lower_fl_mul_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_mul(ssa, args, source))
}

pub fn lower_fl_div_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_div(ssa, args, source))
}

pub fn lower_fl_lt_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_lt(ssa, args, source))
}

pub fn lower_fl_le_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_le(ssa, args, source))
}

pub fn lower_fl_gt_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_gt(ssa, args, source))
}

pub fn lower_fl_ge_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_ge(ssa, args, source))
}

pub fn lower_fl_eq_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_eq(ssa, args, source))
}

pub fn lower_fl_zero<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let z = ssa.builder.ins().f64const(Ieee64::with_float(0.0));
    PrimValue::Comparison(ssa.builder.ins().fcmp(FloatCC::Equal, f, z))
}
pub fn lower_fl_positive<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let z = ssa.builder.ins().f64const(Ieee64::with_float(0.0));
    PrimValue::Comparison(ssa.builder.ins().fcmp(FloatCC::GreaterThan, f, z))
}
pub fn lower_fl_negative<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let z = ssa.builder.ins().f64const(Ieee64::with_float(0.0));
    PrimValue::Comparison(ssa.builder.ins().fcmp(FloatCC::LessThan, f, z))
}
pub fn lower_fl_nan<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    PrimValue::Comparison(ssa.builder.ins().fcmp(FloatCC::Unordered, f, f))
}
pub fn lower_fl_infinite<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let abs = ssa.builder.ins().fabs(f);
    let inf = ssa.builder.ins().f64const(Ieee64::with_float(f64::INFINITY));
    PrimValue::Comparison(ssa.builder.ins().fcmp(FloatCC::Equal, abs, inf))
}
pub fn lower_fl_finite<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let abs = ssa.builder.ins().fabs(f);
    let inf = ssa.builder.ins().f64const(Ieee64::with_float(f64::INFINITY));
    let is_inf = ssa.builder.ins().fcmp(FloatCC::Equal, abs, inf);
    let is_nan = ssa.builder.ins().fcmp(FloatCC::Unordered, f, f);
    let bad = ssa.builder.ins().bor(is_inf, is_nan);
    let one = ssa.builder.ins().iconst(types::I8, 1);
    let zero = ssa.builder.ins().iconst(types::I8, 0);
    PrimValue::Comparison(ssa.builder.ins().select(bad, zero, one))
}
pub fn lower_fl_min<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let r = ssa.builder.ins().fmin(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_max<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let (a0, a1) = atom_pair(ssa, args);
    let lhs = flonum_f64(ssa, a0);
    let rhs = flonum_f64(ssa, a1);
    let r = ssa.builder.ins().fmax(lhs, rhs);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_abs<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().fabs(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_floor<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().floor(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_ceiling<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().ceil(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_truncate<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().trunc(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_round<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().nearest(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}
pub fn lower_fl_sqrt<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let f = flonum_f64(ssa, raw);
    let r = ssa.builder.ins().sqrt(f);
    PrimValue::Value(flonum_from_f64(ssa, r))
}

pub fn lower_fl_sin<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.sin, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_cos<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.cos, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_tan<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.tan, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_exp<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.exp, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_log<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.log, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_asin<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.asin, &[ctx, raw]);
    PrimValue::Value(result)
}

pub fn lower_fl_acos<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    _source: Value<'gc>,
) -> PrimValue {
    let raw = ssa.atom(args[0]);
    let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
    let result = ssa.handle_thunk_call_result(ssa.thunks.acos, &[ctx, raw]);
    PrimValue::Value(result)
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

pub fn lower_fl_zero_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_zero(ssa, args, source))
}

pub fn lower_fl_positive_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_positive(ssa, args, source))
}

pub fn lower_fl_negative_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_negative(ssa, args, source))
}

pub fn lower_fl_nan_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_nan(ssa, args, source))
}

pub fn lower_fl_infinite_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_infinite(ssa, args, source))
}

pub fn lower_fl_finite_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_finite(ssa, args, source))
}

pub fn lower_fl_abs_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_abs(ssa, args, source))
}

pub fn lower_fl_floor_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_floor(ssa, args, source))
}

pub fn lower_fl_ceiling_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_ceiling(ssa, args, source))
}

pub fn lower_fl_truncate_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_truncate(ssa, args, source))
}

pub fn lower_fl_round_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_round(ssa, args, source))
}

pub fn lower_fl_sin_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_sin(ssa, args, source))
}

pub fn lower_fl_cos_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_cos(ssa, args, source))
}

pub fn lower_fl_tan_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_tan(ssa, args, source))
}

pub fn lower_fl_exp_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_exp(ssa, args, source))
}

pub fn lower_fl_log_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_log(ssa, args, source))
}

pub fn lower_fl_asin_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_asin(ssa, args, source))
}

pub fn lower_fl_acos_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_acos(ssa, args, source))
}

pub fn lower_fl_sqrt_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_sqrt(ssa, args, source))
}

pub fn lower_fl_atan_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonum_1(ssa, args, source, |ssa, _a0| lower_fl_atan(ssa, args, source))
}

pub fn lower_fl_min_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_min(ssa, args, source))
}

pub fn lower_fl_max_checked<'gc, 'a, 'f>(
    ssa: &mut SsaBuilder<'gc, 'a, 'f>,
    args: &[Atom<'gc>],
    source: Value<'gc>,
) -> PrimValue {
    with_flonums_2(ssa, args, source, |ssa, _a0, _a1| lower_fl_max(ssa, args, source))
}

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
