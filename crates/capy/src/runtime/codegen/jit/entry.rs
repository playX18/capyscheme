//! Entering a callee, shared by both contexts.
//!
//! [`emit_enter_callee`] jumps to the closure's machine entry. An
//! interpreted closure lands in `interpreter_enter`, which is the only place
//! that loads the entrypoint/`MD`, checks `jit_entry` and heals
//! `closure.code` ([`emit_interpreter_entry`]), a healed closure of compiled
//! code goes direct.

use onlineasm::onlineasm;

use super::super::masm::Masm;
use super::super::template::tmp;
use crate::{
    runtime::sabi::{self, regs},
    utils::flags,
};

/// Enter the closure in `CP`, whose frame `SFP` points at: one indirect
/// jump through its machine entry. Shared by both contexts, which is what
/// keeps their call paths identical. Without the JIT nothing heals, so
/// every machine entry is `interpreter_enter`.
pub(crate) fn emit_enter_callee(masm: &mut Masm) {
    let code_at = masm.assembler().mem(regs::CP, sabi::CLOSURE_CODE as i32);
    onlineasm!(*masm.assembler();
        jmp {code_at};
    );
}

/// The interpreter entry for the closure in `CP`: `PC`/`CONSTANT_POINTER` from its code
/// block's bytecode entry, `MD`, then, when the JIT is on, its compiled
/// code when it has some (healing `closure.code` so its next call goes
/// direct), else the `fallthrough`. Only `interpreter_enter` runs this.
pub(crate) fn emit_interpreter_entry(masm: &mut Masm, fallthrough: impl FnOnce(&mut Masm)) {
    let (cp, pc, constant_pointer, md) = (regs::CP, regs::PC, regs::CONSTANT_POINTER, regs::MD);
    let block = masm.assembler().mem(cp, sabi::CLOSURE_CODE_BLOCK as i32);
    let entry = masm
        .assembler()
        .mem(tmp::T0, sabi::CODE_BLOCK_ENTRYPOINT as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {block};
        loadp {pc}, {entry};
        move {constant_pointer}, {pc};
        loadp {md}, [{constant_pointer} - 8];
    );
    if flags::jit() {
        let to_interpreter = masm.label();
        let jit_entry = masm
            .assembler()
            .mem(tmp::T0, sabi::CODE_BLOCK_JIT_ENTRY as i32);
        let code_at = masm.assembler().mem(cp, sabi::CLOSURE_CODE as i32);
        onlineasm!(*masm.assembler();
            loadp {tmp::T0}, {jit_entry};
            btpz {tmp::T0}, {to_interpreter};
            bpeq {tmp::T0}, -1, {to_interpreter};
            storep {code_at}, {tmp::T0};
            jmp {tmp::T0};
        );
        masm.bind(to_interpreter);
    }
    fallthrough(masm);
}
