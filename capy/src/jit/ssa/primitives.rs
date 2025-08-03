use crate::runtime::value::Symbol;
use cranelift::prelude::{InstBuilder, types};
use cranelift_codegen::ir;
use rsgc::{Global, Rootable, Trace};
use std::{collections::HashMap, sync::OnceLock};

use crate::{
    jit::ssa::Current,
    runtime::{Context, value::Value},
};

pub type LowerPrimitive = for<'a, 'f, 'gc> fn(
    current: &mut Current<'gc, 'a, 'f>,
    proc: Value<'gc>,
    args: &[ir::Value],
) -> Option<ir::Value>;

pub struct PrimitiveLowererTable<'gc> {
    pub map: HashMap<Value<'gc>, LowerPrimitive>,
}

macro_rules! define_primitive_lowering {
    ($(
        $name: literal => $lower: ident ($current: ident, $proc: ident, $args: ident) $b: block
    )*) => {

        $(fn $lower<'a, 'f, 'gc>(
            $current: &mut Current<'gc, 'a, 'f>,
            $proc: Value<'gc>,
            $args: &[ir::Value],
        ) -> Option<ir::Value> {
            $b
        })*

        impl<'gc> PrimitiveLowererTable<'gc> {
            fn new(ctx: Context<'gc>) -> Self {
                let mut map: HashMap<Value<'gc>, LowerPrimitive> = HashMap::new();

                $(
                    map.insert(Symbol::from_str(ctx, $name).into(), $lower);
                )*

                Self { map }
            }
        }
    };
}

static PRIMS: OnceLock<Global<Rootable!(PrimitiveLowererTable<'_>)>> = OnceLock::new();

impl<'gc> PrimitiveLowererTable<'gc> {
    pub fn get(ctx: Context<'gc>) -> &'gc PrimitiveLowererTable<'gc> {
        PRIMS
            .get_or_init(|| Global::new(PrimitiveLowererTable::new(ctx)))
            .fetch(&ctx)
    }
}

unsafe impl<'gc> Trace for PrimitiveLowererTable<'gc> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        for key in self.map.keys() {
            unsafe {
                let key = key as *const Value<'gc> as *mut Value<'gc>;
                (*key).trace(visitor);
            }
        }
    }
}

define_primitive_lowering!(
    ".typecode8" => typecode8(current, _proc, args) {
        Some(current.typecode8(args[0]))
    }

    ".typecode16" => typecode16(current, _proc, args) {
        Some(current.typecode16(args[0]))
    }

    ".ref8" => ref8(current, _proc, args) {
        let p = args[0];
        let offset = args[1];

        let addr = current.builder.ins().iadd(p, offset);
        Some(current.builder.ins().load(types::I8, ir::MemFlags::new(), addr, 0))
    }

    ".ref16" => ref16(current, _proc, args) {
        let p = args[0];
        let offset = args[1];

        let addr = current.builder.ins().iadd(p, offset);
        Some(current.builder.ins().load(types::I16, ir::MemFlags::new(), addr, 0))
    }

    ".ref32" => ref32(current, _proc, args) {
        let p = args[0];
        let offset = args[1];

        let addr = current.builder.ins().iadd(p, offset);
        Some(current.builder.ins().load(types::I32, ir::MemFlags::new(), addr, 0))
    }

    ".ref64" => ref64(current, _proc, args) {
        let p = args[0];
        let offset = args[1];

        let addr = current.builder.ins().iadd(p, offset);
        Some(current.builder.ins().load(types::I64, ir::MemFlags::new(), addr, 0))
    }

    ".refptr" => refptr(current, _proc, args) {
        let p = args[0];
        let offset = args[1];

        let addr = current.builder.ins().iadd(p, offset);
        Some(current.builder.ins().load(types::I64, ir::MemFlags::new(), addr, 0))
    }

    ".set8" => set8(current, _proc, args) {
        let p = args[0];
        let offset = args[1];
        let value = args[2];

        let addr = current.builder.ins().iadd(p, offset);
        current.builder.ins().istore8(ir::MemFlags::new(), value, addr, 0);
        None
    }

    ".set16" => set16(current, _proc, args) {
        let p = args[0];
        let offset = args[1];
        let value = args[2];

        let addr = current.builder.ins().iadd(p, offset);
        current.builder.ins().istore16(ir::MemFlags::new(), value, addr, 0);
        None
    }

    ".set32" => set32(current, _proc, args) {
        let p = args[0];
        let offset = args[1];
        let value = args[2];

        let addr = current.builder.ins().iadd(p, offset);
        current.builder.ins().istore32(ir::MemFlags::new(), value, addr, 0);
        None
    }

    ".set64" => set64(current, _proc, args) {
        let p = args[0];
        let offset = args[1];
        let value = args[2];

        let addr = current.builder.ins().iadd(p, offset);
        let value = current.builder.ins().uextend(types::I64, value);
        current.builder.ins().store(ir::MemFlags::new(), value, addr, 0);
        None
    }


    "u8+" => u8_add(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().iadd(a, b))
    }

    "u16+" => u16_add(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().iadd(a, b))
    }

    "u32+" => u32_add(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().iadd(a, b))
    }

    "u64+" => u64_add(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().iadd(a, b))
    }

    "u8-" => u8_sub(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().isub(a, b))
    }

    "u16-" => u16_sub(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().isub(a, b))
    }

    "u32-" => u32_sub(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().isub(a, b))
    }

    "u64-" => u64_sub(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().isub(a, b))
    }

    "u8*" => u8_mul(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().imul(a, b))
    }

    "u16*" => u16_mul(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().imul(a, b))
    }

    "u32*" => u32_mul(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().imul(a, b))
    }

    "u64*" => u64_mul(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().imul(a, b))
    }

    "u8/" => u8_div(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().udiv(a, b))
    }

    "u16/" => u16_div(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().udiv(a, b))
    }

    "u32/" => u32_div(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().udiv(a, b))
    }

    "u64/" => u64_div(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().udiv(a, b))
    }

    "u8=" => u8_eq(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::Equal, a, b))
    }

    "u16=" => u16_eq(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::Equal, a, b))
    }

    "u32=" => u32_eq(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::Equal, a, b))
    }

    "u64=" => u64_eq(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::Equal, a, b))
    }

    "u8<" => u8_lt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, a, b))
    }

    "u16<" => u16_lt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, a, b))
    }

    "u32<" => u32_lt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, a, b))
    }

    "u64<" => u64_lt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, a, b))
    }

    "u8<=" => u8_le(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, a, b))
    }

    "u16<=" => u16_le(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, a, b))
    }

    "u32<=" => u32_le(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, a, b))
    }

    "u64<=" => u64_le(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, a, b))
    }

    "u8>" => u8_gt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, a, b))
    }

    "u16>" => u16_gt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, a, b))
    }

    "u32>" => u32_gt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, a, b))
    }

    "u64>" => u64_gt(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, a, b))
    }

    "u8>=" => u8_ge(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, a, b))
    }

    "u16>=" => u16_ge(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, a, b))
    }

    "u32>=" => u32_ge(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, a, b))
    }

    "u64>=" => u64_ge(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, a, b))
    }

    "u8and" => u8_and(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().band(a, b))
    }

    "u16and" => u16_and(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().band(a, b))
    }

    "u32and" => u32_and(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().band(a, b))
    }

    "u64and" => u64_and(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().band(a, b))
    }

    "u8or" => u8_or(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bor(a, b))
    }

    "u16or" => u16_or(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bor(a, b))
    }

    "u32or" => u32_or(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bor(a, b))
    }

    "u64or" => u64_or(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bor(a, b))
    }

    "u8xor" => u8_xor(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bxor(a, b))
    }

    "u16xor" => u16_xor(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bxor(a, b))
    }

    "u32xor" => u32_xor(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bxor(a, b))
    }

    "u64xor" => u64_xor(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().bxor(a, b))
    }

    "u8not" => u8_not(current, _proc, args) {
        let a = args[0];
        Some(current.builder.ins().bnot(a))
    }

    "u16not" => u16_not(current, _proc, args) {
        let a = args[0];
        Some(current.builder.ins().bnot(a))
    }

    "u32not" => u32_not(current, _proc, args) {
        let a = args[0];
        Some(current.builder.ins().bnot(a))
    }

    "u64not" => u64_not(current, _proc, args) {
        let a = args[0];
        Some(current.builder.ins().bnot(a))
    }

    "u8<<" => u8_shl(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ishl(a, b))
    }

    "u16<<" => u16_shl(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ishl(a, b))
    }

    "u32<<" => u32_shl(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ishl(a, b))
    }

    "u64<<" => u64_shl(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ishl(a, b))
    }

    "u8>>" => u8_shr(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ushr(a, b))
    }

    "u16>>" => u16_shr(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ushr(a, b))
    }

    "u32>>" => u32_shr(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ushr(a, b))
    }

    "u64>>" => u64_shr(current, _proc, args) {
        let a = args[0];
        let b = args[1];

        Some(current.builder.ins().ushr(a, b))
    }



);
