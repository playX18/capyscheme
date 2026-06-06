use std::mem::{offset_of, size_of};

use cranelift::prelude::{FunctionBuilder, InstBuilder, types};
use cranelift_codegen::{ir, isa::CallConv};

use crate::runtime::{COMPILED_ENTRY_ARG_COUNT, REGISTER_ARG_COUNT, State, value::Value};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDef {
    Value(ir::Value),
    Comparison(ir::Value),
}

#[derive(Clone, Copy)]
pub struct LinearRestSource {
    pub argc: ir::Value,
    pub args: [ir::Value; REGISTER_ARG_COUNT],
    pub overflow: ir::Value,
    pub first_rest: usize,
}

#[derive(Clone, Copy)]
pub struct RegisterCallArgs {
    pub argc: ir::Value,
    pub args: [ir::Value; REGISTER_ARG_COUNT],
    pub overflow: ir::Value,
}

pub(crate) fn overflow_base_from_argc(
    builder: &mut FunctionBuilder<'_>,
    state: ir::Value,
    argc: ir::Value,
) -> ir::Value {
    let overflow_count = builder.ins().iadd_imm(argc, -(REGISTER_ARG_COUNT as i64));
    let zero = builder.ins().iconst(types::I64, 0);
    let has_overflow = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        REGISTER_ARG_COUNT as i64,
    );
    let overflow_count = builder.ins().select(has_overflow, overflow_count, zero);
    let overflow_bytes = builder
        .ins()
        .imul_imm(overflow_count, size_of::<Value>() as i64);
    let runstack = builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        state,
        offset_of!(State, runstack) as i32,
    );
    builder.ins().isub(runstack, overflow_bytes)
}

pub(crate) fn compiled_scheme_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::Tail);
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(ir::AbiParam::new(types::I64));
    }
    sig.returns.push(ir::AbiParam::new(types::I64));
    sig.returns.push(ir::AbiParam::new(types::I64));
    sig
}

pub(crate) const MAX_RAISE_ARITY: usize = 4;
