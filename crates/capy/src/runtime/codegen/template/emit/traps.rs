//! Traps: opcodes with no inline fast path, always leaving for Rust.

use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::template::{SlowPath, TemplateContext};

pub fn op_raise<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.control(masm, SlowPath::Raise);
}

pub fn op_runtime_procedure<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.control(masm, SlowPath::RuntimeProc);
}
