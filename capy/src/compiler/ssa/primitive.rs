use super::SSABuilder;
use crate::cps::term::Atom;
use crate::expander::core::LVarRef;
use crate::runtime::Context;
use crate::runtime::State;
use crate::runtime::modules::Variable;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use std::collections::HashMap;
use std::mem::offset_of;
macro_rules! prim {
    ($($sname: literal => $name: ident ($ssa: ident, $args: ident, $h: ident) $b: block),*) => {
        pub struct PrimitiveLowerer<'gc> {
            pub map: HashMap<Value<'gc>, for <'a, 'f>fn(&mut SSABuilder<'gc, 'a, 'f>, &[Atom<'gc>], LVarRef<'gc>) -> PrimValue>,
        }


        $(

            pub fn $name<'gc_, 'a, 'f>($ssa: &mut SSABuilder<'gc_, 'a, 'f>, $args: &[Atom<'gc_>], $h: LVarRef<'gc_>) -> PrimValue $b
        )*

        impl<'gc> PrimitiveLowerer<'gc> {
            pub fn new(ctx: Context<'gc>) -> Self {
                let mut map = HashMap::new();
                $(
                    map.insert(Symbol::from_str(ctx, $sname).into(), $name as for <'a, 'f>fn(&mut SSABuilder<'gc, 'a, 'f>, &[Atom<'gc>], LVarRef<'gc>) -> PrimValue);
                )*
                Self { map }
            }
        }

    };
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum PrimValue {
    Value(ir::Value),
    Comparison(ir::Value),
}

prim!(
    "s8=" => s8_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "s8>" => s8_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s8<" => s8_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s8>=" => s8_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s8<=" => s8_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s8+" => s8_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "s8-" => s8_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "s8*" => s8_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "s8/" => s8_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let quot = ssa.builder.ins().sdiv(a, b);
        PrimValue::Value(quot)
    },

    "s8%" => s8_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let rem = ssa.builder.ins().srem(a, b);
        PrimValue::Value(rem)
    },

    "s8<<" => s8_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "s8>>" => s8_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let shr = ssa.builder.ins().sshr(a, b);
        PrimValue::Value(shr)
    },

    "s8&" => s8_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "s8|" => s8_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "s8^" => s8_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "s8->s16" => s8_to_s16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.sextend(types::I16, val);
        PrimValue::Value(val)
    },

    "s8->s32" => s8_to_s32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.sextend(types::I32, val);
        PrimValue::Value(val)
    },

    "s8->s64" => s8_to_s64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.sextend(types::I64, val);
        PrimValue::Value(val)
    },

    "s16=" => s16_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "s16>" => s16_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s16<" => s16_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s16>=" => s16_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s16<=" => s16_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s16+" => s16_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "s16-" => s16_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "s16*" => s16_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "s16/" => s16_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let quot = ssa.builder.ins().sdiv(a, b);
        PrimValue::Value(quot)
    },

    "s16%" => s16_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let rem = ssa.builder.ins().srem(a, b);
        PrimValue::Value(rem)
    },

    "s16<<" => s16_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "s16>>" => s16_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let shr = ssa.builder.ins().sshr(a, b);
        PrimValue::Value(shr)
    },

    "s16&" => s16_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "s16|" => s16_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "s16^" => s16_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "s16->s8" => s16_to_s8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.ireduce(types::I8, val);
        PrimValue::Value(val)
    },

    "s16->s32" => s16_to_s32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.sextend(types::I32, val);
        PrimValue::Value(val)
    },

    "s16->s64" => s16_to_s64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.sextend(types::I64, val);
        PrimValue::Value(val)
    },

    "s32=" => s32_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "s32>" => s32_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s32<" => s32_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s32>=" => s32_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s32<=" => s32_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s32+" => s32_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "s32-" => s32_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "s32*" => s32_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "s32/" => s32_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let quot = ssa.builder.ins().sdiv(a, b);
        PrimValue::Value(quot)
    },

    "s32%" => s32_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let rem = ssa.builder.ins().srem(a, b);
        PrimValue::Value(rem)
    },

    "s32<<" => s32_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "s32>>" => s32_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let shr = ssa.builder.ins().sshr(a, b);
        PrimValue::Value(shr)
    },

    "s32&" => s32_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "s32|" => s32_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "s32^" => s32_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },



    "s32->s8" => s32_to_s8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.ireduce(types::I8, val);
        PrimValue::Value(val)
    },

    "s32->s16" => s32_to_s16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.ireduce(types::I16, val);
        PrimValue::Value(val)
    },

    "s32->s64" => s32_to_s64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.sextend(types::I64, val);
        PrimValue::Value(val)
    },

    "s32->u64" => s32_to_u64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.zextend(types::I64, val);
        PrimValue::Value(val)
    },

    "s64=" => s64_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I64, a);
        let b = ssa.ireduce(types::I64, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "s64>" => s64_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s64<" => s64_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "s64>=" => s64_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let cmp = ssa.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s64<=" => s64_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let cmp = ssa.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "s64+" => s64_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "s64-" => s64_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "s64*" => s64_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "s64/" => s64_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let quot = ssa.builder.ins().sdiv(a, b);
        PrimValue::Value(quot)
    },

    "s64%" => s64_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let rem = ssa.builder.ins().srem(a, b);
        PrimValue::Value(rem)
    },

    "s64<<" => s64_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "s64>>" => s64_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let shr = ssa.builder.ins().sshr(a, b);
        PrimValue::Value(shr)
    },

    "s64&" => s64_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "s64|" => s64_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "s64^" => s64_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);

        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "s64->s8" => s64_to_s8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        PrimValue::Value(val)
    },

    "s64->s16" => s64_to_s16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        PrimValue::Value(val)
    },

    "s64->s32" => s64_to_s32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        PrimValue::Value(val)
    },



    "u8=" => u8_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "u8>" => u8_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u8<" => u8_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u8>=" => u8_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u8<=" => u8_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u8+" => u8_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "u8-" => u8_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "u8*" => u8_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "u8/" => u8_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let quot = ssa.builder.ins().udiv(a, b);
        PrimValue::Value(quot)
    },

    "u8%" => u8_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let rem = ssa.builder.ins().urem(a, b);
        PrimValue::Value(rem)
    },

    "u8<<" => u8_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "u8>>" => u8_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let shr = ssa.builder.ins().ushr(a, b);
        PrimValue::Value(shr)
    },

    "u8&" => u8_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "u8|" => u8_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "u8^" => u8_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I8, a);
        let b = ssa.ireduce(types::I8, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "u8->u16" => u8_to_u16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.zextend(types::I16, val);
        PrimValue::Value(val)
    },

    "u8->u32" => u8_to_u32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.zextend(types::I32, val);
        PrimValue::Value(val)
    },

    "u8->u64" => u8_to_u64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        let val = ssa.zextend(types::I64, val);
        PrimValue::Value(val)
    },

    "u16=" => u16_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "u16>" => u16_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u16<" => u16_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u16>=" => u16_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u16<=" => u16_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u16+" => u16_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "u16-" => u16_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "u16*" => u16_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "u16/" => u16_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let quot = ssa.builder.ins().udiv(a, b);
        PrimValue::Value(quot)
    },

    "u16%" => u16_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let rem = ssa.builder.ins().urem(a, b);
        PrimValue::Value(rem)
    },

    "u16<<" => u16_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "u16>>" => u16_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let shr = ssa.builder.ins().ushr(a, b);
        PrimValue::Value(shr)
    },

    "u16&" => u16_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "u16|" => u16_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "u16^" => u16_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I16, a);
        let b = ssa.ireduce(types::I16, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "u16->u8" => u16_to_u8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.zextend(types::I8, val);
        PrimValue::Value(val)
    },

    "u16->u32" => u16_to_u32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.zextend(types::I32, val);
        PrimValue::Value(val)
    },

    "u16->u64" => u16_to_u64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        let val = ssa.zextend(types::I64, val);
        PrimValue::Value(val)
    },

    "u32=" => u32_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "u32>" => u32_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u32<" => u32_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u32>=" => u32_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u32<=" => u32_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u32+" => u32_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "u32-" => u32_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "u32*" => u32_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "u32/" => u32_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let quot = ssa.builder.ins().udiv(a, b);
        PrimValue::Value(quot)
    },

    "u32%" => u32_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let rem = ssa.builder.ins().urem(a, b);
        PrimValue::Value(rem)
    },

    "u32<<" => u32_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "u32>>" => u32_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let shr = ssa.builder.ins().ushr(a, b);
        PrimValue::Value(shr)
    },

    "u32&" => u32_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "u32|" => u32_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "u32^" => u32_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let a = ssa.ireduce(types::I32, a);
        let b = ssa.ireduce(types::I32, b);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "u32->u8" => u32_to_u8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        PrimValue::Value(val)
    },

    "u32->u16" => u32_to_u16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);

        PrimValue::Value(val)
    },

    "u32->u64" => u32_to_u64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.zextend(types::I64, val);
        PrimValue::Value(val)
    },

    "u64=" => u64_eq(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, a, b);
        PrimValue::Comparison(cmp)
    },

    "u64>" => u64_gt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u64<" => u64_lt(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        PrimValue::Comparison(cmp)
    },

    "u64>=" => u64_ge(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u64<=" => u64_le(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let cmp = ssa.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, a, b);
        PrimValue::Comparison(cmp)
    },

    "u64+" => u64_add(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let sum = ssa.builder.ins().iadd(a, b);
        PrimValue::Value(sum)
    },

    "u64-" => u64_sub(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let diff = ssa.builder.ins().isub(a, b);
        PrimValue::Value(diff)
    },

    "u64*" => u64_mul(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let prod = ssa.builder.ins().imul(a, b);
        PrimValue::Value(prod)
    },

    "u64/" => u64_div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let quot = ssa.builder.ins().udiv(a, b);
        PrimValue::Value(quot)
    },

    "u64%" => u64_rem(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let rem = ssa.builder.ins().urem(a, b);
        PrimValue::Value(rem)
    },

    "u64<<" => u64_shl(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let shl = ssa.builder.ins().ishl(a, b);
        PrimValue::Value(shl)
    },

    "u64>>" => u64_shr(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let shr = ssa.builder.ins().ushr(a, b);
        PrimValue::Value(shr)
    },

    "u64&" => u64_and(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let and = ssa.builder.ins().band(a, b);
        PrimValue::Value(and)
    },

    "u64|" => u64_or(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let or = ssa.builder.ins().bor(a, b);
        PrimValue::Value(or)
    },

    "u64^" => u64_xor(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let xor = ssa.builder.ins().bxor(a, b);
        PrimValue::Value(xor)
    },

    "u64->u8" => u64_to_u8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I8, val);
        PrimValue::Value(val)
    },

    "u64->u16" => u64_to_u16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        PrimValue::Value(val)
    },

    "u64->u32" => u64_to_u32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        PrimValue::Value(val)
    },

    "s64->u64" => s64_to_u64(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        PrimValue::Value(val)
    },

    "s64->u32" => s64_to_u32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        PrimValue::Value(val)
    },

    "s64->u16" => s64_to_u16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I16, val);
        PrimValue::Value(val)
    },

    "%typecode8" => typecode8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let tc8 = ssa.builder.ins().load(types::I8, ir::MemFlags::trusted().with_can_move(), val, 0);
        PrimValue::Value(tc8)
    },

    "%refptr" => refptr(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "%ref8" => ref8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I8, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "%ref16" => ref16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I16, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "%ref32" => ref32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I32, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "%ref64" => ref64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "%store8" => store8(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        let data = ssa.atom(args[2]);
        let data = ssa.ireduce(types::I8, data);
        ssa.builder.ins().store(ir::MemFlags::trusted().with_can_move(), data, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "%store16" => store16(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        let data = ssa.atom(args[2]);
        let data = ssa.ireduce(types::I16, data);
        ssa.builder.ins().store(ir::MemFlags::trusted().with_can_move(), data, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "%store32" => store32(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        let data = ssa.atom(args[2]);
        let data = ssa.ireduce(types::I32, data);
        ssa.builder.ins().store(ir::MemFlags::trusted().with_can_move(), data, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "%store64" => store64(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let offset = ssa.atom(args[1]);
        let i32_offset = ssa.ireduce(types::I32, offset);
        let offset = ssa.zextend(types::I64, i32_offset);
        let addr = ssa.builder.ins().iadd(val, offset);
        let data = ssa.atom(args[2]);
        ssa.builder.ins().store(ir::MemFlags::trusted().with_can_move(), data, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },


    "usize->value" => usize_to_value(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let val = ssa.ireduce(types::I32, val);
        let val = ssa.zextend(types::I64, val);
        let val = ssa.builder.ins().bor_imm(val, Value::NUMBER_TAG as i64);
        PrimValue::Value(val)
    },


    "cache-ref" => cache_ref(ssa, args, _h) {
        assert_eq!(args.len(), 1);

        let Atom::Constant(cache_key) = args[0] else {
            panic!("invalid cache-ref")
        };

        let cell = ssa.module_builder.intern_cache_cell(cache_key);
        let cell = ssa.import_data(cell);
        let addr = ssa.builder.ins().global_value(types::I64, cell);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::new(), addr, 0))
    },

    "cache-set!" => cache_set(ssa, args, _h) {
        assert_eq!(args.len(), 2);

        let Atom::Constant(cache_key) = args[0] else {
            panic!("invalid cache-set!")
        };

        let value = ssa.atom(args[1]);

        let cell = ssa.module_builder.intern_cache_cell(cache_key);
        let cell = ssa.import_data(cell);
        let addr = ssa.builder.ins().global_value(types::I64, cell);
        ssa.builder.ins().store(ir::MemFlags::new(), value, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "immediate?" => is_immediate(ssa, args, _h) {
        let arg = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.is_immediate(arg))
    },

    "heap-object?" => is_heap_object(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let x = ssa.is_heap_object(arg);


        PrimValue::Comparison(x)
    },

    "variable-bound?" => variable_bound(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let val = ssa.builder.ins().load(types::I64, ir::MemFlags::new(), arg, offset_of!(Variable, value) as i32);
        let false_ = ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64);
        let is_bound = ssa.builder.ins().icmp(IntCC::NotEqual, val, false_);

        PrimValue::Comparison(is_bound)
    },

    "variable-ref" => variable_ref(ssa, args, _h) {
        let arg = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::new(), arg, offset_of!(Variable, value) as i32))
    },

    "variable-set!" => variable_set(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let val = ssa.atom(args[1]);

        ssa.pre_write_barrier(arg, offset_of!(Variable, value) as i32, val);
        ssa.builder.ins().store(ir::MemFlags::new(), val, arg, offset_of!(Variable, value) as i32);
        ssa.post_write_barrier(arg, offset_of!(Variable, value) as i32, val);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "make-variable" => make_box(ssa, args, _h) {

        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.make_variable, &[ctx, arg]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "module-ensure-local-variable!" => ensure_local_var(ssa, args, _handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.module_ensure_local_variable, &[ctx, module, name]);
        let var = ssa.builder.inst_results(call)[0];
        PrimValue::Value(var)
    },

    "lookup" => lookup(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup, &[ctx, module, name], handler))
    },

    "lookup-bound" => lookup_bound(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup_bound, &[ctx, module, name], handler))
    },

    "lookup-bound-public" => lookup_bound_public(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup_bound_public, &[ctx, module, name], handler))
    },

    "lookup-bound-private" => lookup_bound_private(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup_bound_private, &[ctx, module, name], handler))
    },

    "current-module" => current_module(ssa, args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        if let Some(module) = args.get(0) {
            let module = ssa.atom(*module);
            let call = ssa.builder.ins().call(ssa.thunks.set_current_module, &[ctx, module]);
            PrimValue::Value(ssa.builder.inst_results(call)[0])
        } else {
            let call = ssa.builder.ins().call(ssa.thunks.current_module, &[ctx]);

            PrimValue::Value(ssa.builder.inst_results(call)[0])
        }
    },

    "define" => define(ssa, args, _h) {
        let key = ssa.atom(args[0]);
        let val = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        let call = ssa.builder.ins().call(ssa.thunks.define, &[ctx, key, val]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },


    "not" => not(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let false_ = ssa.builder.ins().iconst(types::I64, Value::new(false).bits() as i64);
        let val = ssa.builder.ins().icmp(IntCC::Equal, val, false_);



        PrimValue::Comparison(val)
    },


    "procedure?" => is_procedure(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_proc = ssa.has_typ8(val, TypeCode8::CLOSURE.bits());
        PrimValue::Comparison(is_proc)
    },

    "variable?" => is_variable(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_variable = ssa.has_typ8(val, TypeCode8::VARIABLE.bits());
        PrimValue::Comparison(is_variable)
    },

    "set-car!" => set_car(ssa, args, _h) {
        let pair = ssa.atom(args[0]);
        let new_car = ssa.atom(args[1]);
        ssa.pre_write_barrier(pair, offset_of!(Pair, car) as i32, new_car);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_car, pair, offset_of!(Pair, car) as i32);
        ssa.post_write_barrier(pair, offset_of!(Pair, car) as i32, new_car);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "set-cdr!" => set_cdr(ssa, args, _h) {
        let pair = ssa.atom(args[0]);
        let new_cdr = ssa.atom(args[1]);
        ssa.pre_write_barrier(pair, offset_of!(Pair, cdr) as i32, new_cdr);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_cdr, pair, offset_of!(Pair, cdr) as i32);
        ssa.post_write_barrier(pair, offset_of!(Pair, cdr) as i32, new_cdr);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "cons" => cons(ssa, args, _h) {
        assert!(args.len() == 2, "cons expects 2 arguments, got: {:?}", args);
        let car = ssa.atom(args[0]);
        let cdr = ssa.atom(args[1]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.cons, &[ctx, car, cdr]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "reverse" => reverse(ssa, args, _h) {
        let list = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.reverse, &[ctx, list]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "eof-object?" => is_eof_object(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let eof = ssa.builder.ins().iconst(types::I64, Value::eof().bits() as i64);
        let is_eof = ssa.builder.ins().icmp(IntCC::Equal, val, eof);
        PrimValue::Comparison(is_eof)
    },

    "null?" => is_null(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let null = ssa.builder.ins().iconst(types::I64, Value::null().bits() as i64);
        let is_null = ssa.builder.ins().icmp(IntCC::Equal, val, null);
        PrimValue::Comparison(is_null)
    },

    "unspecified?" => is_unspecified(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let unspecified = ssa.builder.ins().iconst(types::I64, Value::unspecified().bits() as i64);
        let is_unspecified = ssa.builder.ins().icmp(IntCC::Equal, val, unspecified);
        PrimValue::Comparison(is_unspecified)
    },

    "pair?" => is_pair(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_pair = ssa.has_typ8(val, TypeCode8::PAIR.bits());
        PrimValue::Comparison(is_pair)
    },

    "list?" => is_list(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let result = ssa.builder.ins().call(ssa.thunks.listp, &[arg]);
        let result = ssa.builder.inst_results(result)[0];
        PrimValue::Comparison(result)
    },

    "append" => append(ssa, args, _h) {
        let ls1 = ssa.atom(args[0]);
        let ls2 = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(
            ssa.thunks.append,
            &[ctx, ls1, ls2],
            _h
        );

        PrimValue::Value(result)
    },

    "list" => list(ssa, args, _h) {
        let args = args.iter().map(|x| ssa.atom(*x)).collect::<Vec<_>>();
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let mut ls = ssa.builder.ins().iconst(types::I64, Value::null().bits() as i64);
        for arg in args.iter().rev().copied() {
            let call = ssa.builder.ins().call(ssa.thunks.cons, &[ctx, arg, ls]);
            ls = ssa.builder.inst_results(call)[0];
        }
        PrimValue::Value(ls)
    },

    "vector?" => is_vector(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_vector = ssa.has_typ8(val, TypeCode8::VECTOR.bits());
        PrimValue::Comparison(is_vector)
    },

    "bytevector?" => is_bytevector(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_bv = ssa.has_typ8(val, TypeCode8::BYTEVECTOR.bits());
        PrimValue::Comparison(is_bv)
    },

    "vector" => vector(ssa, args, _h) {

        let size = size_of::<Vector>() as i64 + args.len() as i64 * size_of::<Value>() as i64;
        let size = ssa.builder.ins().iconst(types::I64, size);
        let vt = ssa.import_static("VECTOR_VTABLE", types::I64);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let tc16 = ssa.builder.ins().iconst(types::I16, TypeCode16::MUTABLE_VECTOR.bits() as i64);

        let call = ssa.builder.ins().call(ssa.thunks.alloc_tc16, &[ctx, vt, tc16, size]);
        let vec = ssa.builder.inst_results(call)[0];


        let hdr = ScmHeader::with_type_bits(TypeCode16::MUTABLE_VECTOR.bits()).word;
        let hdr = ssa.builder.ins().iconst(types::I64, hdr as i64);
        ssa.builder.ins().store(ir::MemFlags::trusted(), hdr, vec, offset_of!(Vector, hdr) as i32);
        ssa.builder.ins().store(ir::MemFlags::trusted(), size, vec, offset_of!(Vector, length) as i32);
        for (i, &arg) in args.iter().enumerate() {
            let arg = ssa.atom(arg);
            ssa.builder.ins().store(ir::MemFlags::trusted(), arg, vec, offset_of!(Vector, data) as i32 + i as i32 * size_of::<Value>() as i32);
        }

        PrimValue::Value(vec)
    },

    "tuple" => tuple(ssa, args, _h) {
        let PrimValue::Value(tup) = make_tuple(ssa, &[Atom::Constant(Value::new(args.len() as i32))], _h) else {
            panic!("tuple make-tuple failed")
        };

        for (i, &arg) in args.iter().enumerate() {
            let arg = ssa.atom(arg);
            ssa.builder.ins().store(ir::MemFlags::trusted(), arg, tup, offset_of!(Tuple, data) as i32 + i as i32 * size_of::<Value>() as i32);
        }

        PrimValue::Value(tup)
    },

    "make-tuple" => make_tuple(ssa, args, _h) {
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
            _h
        );

        PrimValue::Value(result)
    },

    "make-vector" => make_vector(ssa, args, _h) {
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
            _h
        );

        PrimValue::Value(result)
    },

    "vector-ref" => vector_ref(ssa, args, _h) {
        let vec = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);

        /*let check_fixnum = ssa.builder.create_block();
        let fixnum_ix_block = ssa.builder.create_block();
        let slowpath = ssa.builder.create_block();
        let merge = ssa.builder.create_block();

        ssa.builder.append_block_param(merge, types::I64);
        ssa.branch_if_has_typ8(vec, TypeCode8::VECTOR.bits(), check_fixnum, &[], slowpath, &[]);
        ssa.builder.switch_to_block(check_fixnum);
        {
            let is_int32 = ssa.is_int32(ix);
            ssa.builder.ins().brif(is_int32, fixnum_ix_block, &[], slowpath, &[]);
            ssa.builder.switch_to_block(fixnum_ix_block);
            {
                let ix = ssa.ireduce(types::I32, ix);
                let below0 = ssa.builder.ins().icmp_imm(IntCC::SignedLessThan, ix, 0);
                let check_bounds = ssa.builder.create_block();
                let in_bounds_block = ssa.builder.create_block();


                ssa.builder.ins().brif(below0, slowpath, &[], check_bounds, &[]);
                ssa.builder.switch_to_block(check_bounds);
                {
                    let len = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), vec, offset_of!(Vector, length) as i32);
                    let ix64 = ssa.zextend(types::I64, ix);
                    let in_bounds = ssa.builder.ins().icmp(IntCC::UnsignedLessThan, ix64, len);
                    ssa.builder.ins().brif(in_bounds, in_bounds_block, &[BlockArg::Value(ix64)], slowpath, &[]);
                    ssa.builder.switch_to_block(in_bounds_block);
                    {
                        let ix64 = ssa.builder.block_params(in_bounds_block)[0];
                        let data_ptr = ssa.builder.ins().iadd_imm(vec, offset_of!(Vector, data) as i64);
                        let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix64);
                        let elem = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), elem_ptr, 0);
                        ssa.builder.ins().jump(merge, &[BlockArg::Value(elem)]);
                    }
                }
            }
        }
        ssa.builder.switch_to_block(slowpath);
        {
            ssa.builder.func.layout.set_cold(slowpath);
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let result = ssa.handle_thunk_call_result(ssa.thunks.vector_ref, &[ctx, vec, ix], _h);
            ssa.builder.ins().jump(merge, &[BlockArg::Value(result)]);
        }

        ssa.builder.switch_to_block(merge);
        let result = ssa.builder.block_params(merge)[0];
        PrimValue::Value(result)*/

        let merge = ssa.builder.create_block();
        ssa.builder.append_block_param(merge, types::I64);

        ensure_vector(ssa, vec,
            |ssa, ulen, slowpath| {
                fixnum_in_bounds_usize(ssa, ix, ulen,
                    |ssa, ix, _slowpath| {
                        let ix_offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
                        let data_ptr = ssa.builder.ins().iadd_imm(vec, offset_of!(Vector, data) as i64);
                        let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
                        let elem = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), elem_ptr, 0);
                        ssa.builder.ins().jump(merge, &[BlockArg::Value(elem)]);
                    },
                    |ssa, _| {
                        ssa.builder.ins().jump(slowpath, &[]);
                    }
                );
            },
            |ssa, _| {
                let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
                let result = ssa.handle_thunk_call_result(ssa.thunks.vector_ref, &[ctx, vec, ix], _h);
                ssa.builder.ins().jump(merge, &[BlockArg::Value(result)]);
            }
        );
        ssa.builder.switch_to_block(merge);
        let result = ssa.builder.block_params(merge)[0];
        PrimValue::Value(result)
    },

    "vector-set!" => vector_set(ssa, args, _h) {
        let vec = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);
        let new_val = ssa.atom(args[2]);

        let merge = ssa.builder.create_block();

        ensure_vector(ssa, vec,
            |ssa, ulen, slowpath| {
                fixnum_in_bounds_usize(ssa, ix, ulen,
                    |ssa, ix, _slowpath| {
                        let ix_offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
                        let data_ptr = ssa.builder.ins().iadd_imm(vec, offset_of!(Vector, data) as i64);
                        let elem_ptr = ssa.builder.ins().iadd(data_ptr, ix_offset);
                        ssa.pre_write_barrier_n(vec, elem_ptr, new_val);
                        ssa.builder.ins().store(ir::MemFlags::trusted(), new_val, elem_ptr, 0);
                        ssa.post_write_barrier_n(vec, elem_ptr, new_val);
                        ssa.builder.ins().jump(merge, &[]);
                    },
                    |ssa, _| {
                        ssa.builder.ins().jump(slowpath, &[]);
                    }
                );
            },
            |ssa, _| {
                let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
                let _ = ssa.handle_thunk_call_result(ssa.thunks.vector_set, &[ctx, vec, ix, new_val], _h);
                ssa.builder.ins().jump(merge, &[]);
            }
        );
        ssa.builder.switch_to_block(merge);

        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "string?" => is_string(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.has_typ8(val, TypeCode8::STRING.bits()))
    },

    "string-length" => string_length(ssa, args, _h) {
        let str = ssa.atom(args[0]);
        let len = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), str, offset_of!(Str, length) as i32);
        let fixnum = ssa.builder.ins().bor_imm(len, Value::NUMBER_TAG as i64);
        PrimValue::Value(fixnum)
    },

    "boolean?" => is_boolean(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::BOOL_TAG as i64);
        PrimValue::Comparison(ssa.builder.ins().icmp_imm(IntCC::Equal, mask, Value::BOOL_TAG as i64))
    },

    "symbol?" => is_symbol(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        PrimValue::Comparison(ssa.has_typ8(val, TypeCode8::SYMBOL.bits()))
    },

    "eq?" => is_eq(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::Equal, x, y))
    },

    "eqv?" => is_eqv(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.eqv, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    },

    "equal?" => is_equal(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.equal, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    },

    "fixnum?" => is_fixnum(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::NUMBER_TAG as i64);
        let is_inline_num = ssa.builder.ins().icmp_imm(IntCC::Equal, mask, Value::NUMBER_TAG as i64);

        PrimValue::Comparison(is_inline_num)
    },

    "untag-fixnum" => untag_fixnum(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let untagged = ssa.ireduce(types::I32, val);
        PrimValue::Value(untagged)
    },

    "exact-integer?" => is_exact_integer(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.exact_integerp, &[ctx, val], _h);


        PrimValue::Value(check)
    },

    "integer?" => is_integer(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.builder.ins().call(ssa.thunks.integerp, &[ctx, val]);
        PrimValue::Comparison(ssa.builder.inst_results(check)[0])
    },

    "char?" => is_char(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().iconst(types::I64, Value::CHAR_MASK as i64);
        let tag = ssa.builder.ins().iconst(types::I64, Value::CHAR_TAG);
        if ssa.builder.func.dfg.value_type(val) != types::I64 {
            unreachable!()
        }
        let masked = ssa.builder.ins().band(val, mask);
        let cmp = ssa.builder.ins().icmp(IntCC::Equal, masked, tag);
        PrimValue::Comparison(cmp)
    },

    "number?" => is_number(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::NUMBER_TAG as i64);
        let is_inline_num = ssa.builder.ins().icmp_imm(IntCC::NotEqual, mask, 0);
        let succ = ssa.builder.create_block();
        let check_heap = ssa.builder.create_block();

        ssa.builder.append_block_param(succ, types::I8);
        ssa.builder.ins().brif(is_inline_num, succ, &[BlockArg::Value(is_inline_num)], check_heap, &[]);
        ssa.builder.switch_to_block(check_heap);
        {
            let check = ssa.has_typ8(val, TypeCode8::NUMBER.bits());
            ssa.builder.ins().jump(succ, &[BlockArg::Value(check)]);
        }

        ssa.builder.switch_to_block(succ);
        PrimValue::Comparison(ssa.builder.block_params(succ)[0])
    },

    "complex?" => is_complex(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let res = ssa.builder.ins().call(ssa.thunks.complexp, &[val]);
        PrimValue::Comparison(ssa.builder.inst_results(res)[0])
    },

    "nan?" => is_nan(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.nanp, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "ash" => ash(ssa, args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);

        let result = ssa.handle_thunk_call_result(ssa.thunks.ash, &[ctx, lhs, rhs], _h);
        PrimValue::Value(result)
    },
    "logand" => logand(ssa, args, _h) {
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.logand, &[ctx, lhs, rhs], _h);
        PrimValue::Value(result)
    },

    "logior" => logior(ssa, args, _h) {
        let lhs = ssa.atom(args[0]);
        let rhs = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.logior, &[ctx, lhs, rhs], _h);
        PrimValue::Value(result)
    },

    "lognot" => lognot(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.lognot, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "integer->char" => integer_to_char(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.integer_to_char, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "char->integer" => char_to_integer(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.char_to_integer, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "expt" => expt(ssa, args, _h) {
        let base = ssa.atom(args[0]);
        let exp = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.expt, &[ctx, base, exp], _h);
        PrimValue::Value(result)
    },

    "abs" => abs(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.abs, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "sqrt" => sqrt(ssa, args, _h) {
        let arg = ssa.atom(args[0]);

        let result = ssa.inline_float_unary_op(arg,
            |ssa, val| {
                let res = ssa.builder.ins().sqrt(val);
                res
            },
            |ssa, arg| {
                let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
                ssa.handle_thunk_call_result(ssa.thunks.sqrt, &[ctx, arg], _h)
            }
        );

        PrimValue::Value(result)
    },

    "cos" => cos(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.cos, &[ctx, arg], _h);

        PrimValue::Value(result)
    },

    "sin" => sin(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.sin, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "tan" => tan(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.tan, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "atan" => atan(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        if args.len() == 2 {
            let arg2 = ssa.atom(args[1]);
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let result = ssa.handle_thunk_call_result(ssa.thunks.atan2, &[ctx, arg, arg2], _h);
            return PrimValue::Value(result);
        }
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.atan, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "asin" => asin(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.asin, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "acos" => acos(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.acos, &[ctx, arg], _h);
        PrimValue::Value(result)
    },





    "ceiling" => ceiling(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.ceiling, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "floor" => floor(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.floor, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "truncate" => truncate(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.truncate, &[ctx, arg], _h);
        PrimValue::Value(result)
    },

    "+" => plus(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]))
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_plus(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },

    "-" => minus(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            let val = ssa.atom(args[0]);

            return PrimValue::Value(emit_negate(ssa, val, _h));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_minus(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },

    "*" => times(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_times(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },

    "/" => div(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.number_div, &[ctx, a, b], _h);
        PrimValue::Value(result)
    },

    "=" => numeric_equal(ssa, args, _h) {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let eq = emit_icmp(ssa, lhs, rhs, IntCC::Equal, _h);
            return PrimValue::Comparison(eq);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let eq = emit_icmp(ssa, acc,rhs, IntCC::Equal, _h);
            if ssa.builder.func.dfg.value_type(eq) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, eq);
        }
        PrimValue::Comparison(acc)
    },

    "<" => numeric_lt(ssa, args, _h) {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let lt = emit_icmp(ssa, lhs, rhs, IntCC::SignedLessThan,  _h);
            return PrimValue::Comparison(lt);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let lt = emit_icmp(ssa, acc,rhs, IntCC::SignedLessThan, _h);
            if ssa.builder.func.dfg.value_type(lt) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, lt);
        }
        PrimValue::Comparison(acc)
    },

    ">" => numeric_gt(ssa, args, _h) {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let gt = emit_icmp(ssa, lhs, rhs, IntCC::SignedGreaterThan,  _h);
            assert_eq!(ssa.builder.func.dfg.value_type(gt), types::I8);
            return PrimValue::Comparison(gt);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let gt = emit_icmp(ssa, acc,rhs, IntCC::SignedGreaterThan, _h);
            if ssa.builder.func.dfg.value_type(gt) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, gt);
        }
        PrimValue::Comparison(acc)
    },

    ">=" => numeric_gte(ssa, args, _h) {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let gte = emit_icmp(ssa, lhs, rhs, IntCC::SignedGreaterThanOrEqual,  _h);
            return PrimValue::Comparison(gte);
        }

        let mut acc = ssa.atom(args[0]);

        let end = ssa.builder.create_block();
        ssa.builder.append_block_param(end, types::I8);
        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let gte = emit_icmp(ssa, acc,rhs, IntCC::SignedGreaterThanOrEqual, _h);
            if ssa.builder.func.dfg.value_type(gte) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!()
            }
            acc = ssa.builder.ins().band(acc, gte);
        }
        PrimValue::Comparison(acc)
    },

    "<=" => numeric_lte(ssa, args, _h) {
        if args.len() == 1 {
            let _ = ssa.atom(args[0]);
            return PrimValue::Comparison(ssa.builder.ins().iconst(types::I8, 1));
        } else if args.len() == 2 {
            // handle 2 args without introducing more blocks
            let lhs = ssa.atom(args[0]);
            let rhs = ssa.atom(args[1]);
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let lte = ssa.handle_thunk_call_result(ssa.thunks.number_le, &[ctx, lhs, rhs], _h);
            //emit_icmp(ssa, lhs, rhs, IntCC::SignedLessThanOrEqual,  _h);
            return PrimValue::Value(lte);
        }

        let mut acc = ssa.atom(args[0]);


        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            let lte = emit_icmp(ssa, acc,rhs, IntCC::SignedLessThanOrEqual, _h);
            if ssa.builder.func.dfg.value_type(lte) != ssa.builder.func.dfg.value_type(acc) {
                unreachable!("type mismatch in <=: {:?} vs {:?}, args: {:?}", ssa.builder.func.dfg.value_type(lte), ssa.builder.func.dfg.value_type(acc), args)
            }
            acc = ssa.builder.ins().band(acc, lte);
        }
        PrimValue::Comparison(acc)
    },

    "memq" => memq(ssa, args, _h) {
        let item = ssa.atom(args[0]);
        let list = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.memq, &[ctx, item, list], _h);
        PrimValue::Value(result)
    },

    "memv" => memv(ssa, args, _h) {
        let item = ssa.atom(args[0]);
        let list = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.memv, &[ctx, item, list], _h);
        PrimValue::Value(result)
    },

    "exact->inexact" => exact_to_inexact(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.exact2inexact, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "inexact->exact" => inexact_to_exact(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.inexact_to_exact, &[ctx, val], _h);
        PrimValue::Value(result)
    },

    "real?" => is_real(ssa, args, _h) {
        let val = ssa.atom(args[0]);


        let call = ssa.builder.ins().call(ssa.thunks.realp, &[val]);
        let check = ssa.builder.inst_results(call)[0];
        PrimValue::Comparison(check)
    },

    "rational?" => is_rational(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let check = ssa.builder.ins().call(ssa.thunks.is_rational, &[val]);
        PrimValue::Comparison(ssa.builder.inst_results(check)[0])
    },

    "inexact?" => is_inexact(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.inexactp, &[ctx, val], _h);
        PrimValue::Value(check)
    },

    "exact?" => is_exact(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let check = ssa.handle_thunk_call_result(ssa.thunks.exactp, &[ctx, val], _h);
        PrimValue::Value(check)
    },

    "even?" => is_even(ssa, args, _h) {
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
            let val = ssa.handle_thunk_call_result(ssa.thunks.evenp, &[ctx, val], _h);
            let is_even = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_even)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)
    },

    "odd?" => is_odd(ssa, args, _h) {
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
            let val = ssa.handle_thunk_call_result(ssa.thunks.oddp, &[ctx, val], _h);
            let is_odd = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_odd)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)
    },

    "zero?" => is_zero(ssa, args, _h) {
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
            let val = ssa.handle_thunk_call_result(ssa.thunks.is_zero, &[ctx, val], _h);
            let is_zero = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_zero)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)

    },

    "car" => car(ssa, args, _h) {
        let pair = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), pair, offset_of!(Pair, car) as i32))
    },
    "cdr" => cdr(ssa, args, _h) {
        let pair = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), pair, offset_of!(Pair, cdr) as i32))
    },

    ".breakpoint" => breakpoint(ssa, _args, _h) {
        ssa.builder.ins().debugtrap();
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::new(true).bits() as i64))
    },

   /*  "tuple" => tuple(ssa, args, _h) {
        let mut hdr = ScmHeader::with_type_bits(TypeCode8::TUPLE.bits() as _);
        hdr.word |= TupleLengthBits::encode(args.len() as _);

        let size = size_of::<Tuple>() as i64 + args.len() as i64 * size_of::<Value>() as i64;

        let size = ssa.builder.ins().iconst(types::I64, size);
        let vt = ssa.import_static("TUPLE_VTABLE", types::I64);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let tc8 = ssa.builder.ins().iconst(types::I8, TypeCode8::TUPLE.bits() as i64);
        let call = ssa.builder.ins().call(ssa.thunks.alloc_tc8, &[ctx, vt, tc8, size]);
        let tuple = ssa.builder.inst_results(call)[0];
        let hdr = ssa.builder.ins().iconst(types::I64, hdr.word as i64);
        ssa.builder.ins().store(ir::MemFlags::trusted(), hdr, tuple, offset_of!(Tuple, hdr) as i32);
        for (i, arg) in args.iter().enumerate() {
            let val = ssa.atom(*arg);
            ssa.builder.ins().store(ir::MemFlags::trusted(), val, tuple, offset_of!(Tuple, data) as i32 + i as i32 * size_of::<Value>() as i32);
        }
        PrimValue::Value(tuple)
    },
*/

    "tuple-size" => tuple_size(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let size = ssa.handle_thunk_call_result(ssa.thunks.tuple_size, &[ctx, arg], _h);
        PrimValue::Value(size)
    },

    "tuple-ref" => tuple_ref(ssa, args, _h) {
        let tuple = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);

        let ix = ssa.ireduce(types::I32, ix);
        let offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
        let offset = ssa.builder.ins().iadd_imm(offset, offset_of!(Tuple, data) as i32 as i64);
        let offset = ssa.zextend(types::I64, offset);
        let addr = ssa.builder.ins().iadd(tuple, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "tuple-set!" => tuple_set(ssa, args, _h) {
        let tuple = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);
        let value = ssa.atom(args[2]);

        let ix = ssa.ireduce(types::I32, ix);
        let offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
        let offset = ssa.builder.ins().iadd_imm(offset, offset_of!(Tuple, data) as i32 as i64);
        let offset = ssa.zextend(types::I64, offset);
        let addr = ssa.builder.ins().iadd(tuple, offset);
        ssa.pre_write_barrier_n(tuple, addr, value);
        ssa.builder.ins().store(ir::MemFlags::trusted(), value, addr, 0);
        ssa.post_write_barrier_n(tuple, addr, value);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "tuple?" => is_tuple(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let res = ssa.has_typ8(val, TypeCode8::TUPLE.bits());

        PrimValue::Comparison(res)
    },

    "symbol->string" => symbol_to_string(ssa, args, _h) {
        let sym = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.symbol2string, &[ctx, sym], _h);
        PrimValue::Value(result)
    },

    "string->symbol" => string_to_symbol(ssa, args, _h) {
        let str = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.string2symbol, &[ctx, str], _h);
        PrimValue::Value(result)
    },



    "length" => length(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        let result = ssa.handle_thunk_call_result(ssa.thunks.length, &[ctx, val], _h);
        PrimValue::Value(result)
    },


    "quotient" => quotient(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.quotient, &[ctx, a, b], _h);
        PrimValue::Value(result)
    },

    "remainder" => remainder(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.remainder, &[ctx, a, b], _h);
        PrimValue::Value(result)
    },

    "modulo" => modulo(ssa, args, _h) {
        let a = ssa.atom(args[0]);
        let b = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.handle_thunk_call_result(ssa.thunks.modulo, &[ctx, a, b], _h);
        PrimValue::Value(result)
    },

    "push-cframe" => push_cframe(ssa, args, _h) {
        let key = ssa.atom(args[0]);
        let value = ssa.atom(args[1]);
        let retk = ssa.atom(args[2]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let res = ssa.builder.ins().call(ssa.thunks.push_cframe, &[ctx, key, value, retk]);
        PrimValue::Value(ssa.builder.inst_results(res)[0])
    },

    "current-continuation-marks" => current_continuation_marks(ssa, _args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.builder.ins().call(ssa.thunks.current_continuation_marks, &[ctx]);
        PrimValue::Value(ssa.builder.inst_results(result)[0])
    },

    "$set-attachments!" => set_attachments(ssa, args, _h) {
        let attachments = ssa.atom(args[0]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let res = ssa.handle_thunk_call_result(ssa.thunks.set_attachments, &[ctx, attachments], _h);
        PrimValue::Value(res)
    },

    "$winders" => winders(ssa, args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        if args.is_empty() {
            let winders = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), ctx, (Context::OFFSET_OF_STATE + offset_of!(State, winders))as i32);
            return PrimValue::Value(winders);
        }

        let new_winders = ssa.atom(args[0]);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_winders, ctx, (Context::OFFSET_OF_STATE + offset_of!(State, winders)) as i32);
        PrimValue::Value(new_winders)
    },

    "make-syntax" => make_syntax(ssa, args, _h) {
        let exp = ssa.atom(args[0]);
        let wrap = ssa.atom(args[1]);
        let module = ssa.atom(args[2]);
        let source = ssa.atom(args[3]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let result = ssa.builder.ins().call(ssa.thunks.make_syntax, &[ctx, exp, wrap, module, source]);
        PrimValue::Value(ssa.builder.inst_results(result)[0])
    }
);

fn emit_plus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
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
            ssa.handle_thunk_call_result(ssa.thunks.number_plus, &[ctx, lhs, rhs], h)
        },
    )
}

fn emit_minus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
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
            ssa.handle_thunk_call_result(ssa.thunks.number_minus, &[ctx, lhs, rhs], h)
        },
    )
}

fn emit_negate<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    h: LVarRef<'gc>,
) -> ir::Value {
    ssa.inline_unary_op(
        a,
        |ssa, val| {
            let zero = ssa.builder.ins().iconst(types::I32, 0);
            let (result, ovf) = ssa.builder.ins().ssub_overflow(zero, val);
            let succ = ssa.builder.create_block();

            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, succ, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, val| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.negate, &[ctx, val], h)
        },
    )
}

fn emit_times<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
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
            ssa.handle_thunk_call_result(ssa.thunks.number_times, &[ctx, lhs, rhs], h)
        },
    )
}

fn emit_icmp<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    cond: IntCC,
    h: LVarRef<'gc>,
) -> ir::Value {
    let result = ssa.inline_cmp_op(
        a,
        b,
        |ssa, lhs, rhs, _slow| {
            let cmp = ssa.builder.ins().icmp(cond, lhs, rhs);
            cmp
        },
        |ssa, a, b| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let thunk = match cond {
                IntCC::Equal => ssa.thunks.number_eq,
                IntCC::SignedGreaterThan => ssa.thunks.number_gt,
                IntCC::SignedGreaterThanOrEqual => ssa.thunks.number_ge,
                IntCC::SignedLessThan => ssa.thunks.number_lt,
                IntCC::SignedLessThanOrEqual => ssa.thunks.number_le,
                _ => panic!("unsupported comparison"),
            };

            let result = ssa.handle_thunk_call_result(thunk, &[ctx, a, b], h);
            ssa.to_boolean(result)
        },
    );
    assert_eq!(ssa.builder.func.dfg.value_type(result), types::I8);
    result
}

fn ensure_vector<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    val: ir::Value,
    on_vector: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value, ir::Block),
    slowpath: impl FnOnce(&mut SSABuilder<'gc, 'a, 'f>, ir::Value),
) {
    let bb_vector = ssa.builder.create_block();
    let bb_slow = ssa.builder.create_block();

    ssa.builder.func.layout.set_cold(bb_slow);

    ssa.branch_if_has_typ8(val, TypeCode8::VECTOR.bits(), bb_vector, &[], bb_slow, &[]);
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

fn fixnum_in_bounds_usize<'gc, 'a, 'f>(
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
