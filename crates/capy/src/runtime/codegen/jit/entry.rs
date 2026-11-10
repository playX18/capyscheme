//! Entering a callee, shared by both contexts.
//!
//! [`emit_enter_callee`] jumps to the closure's machine entry. An
//! interpreted closure lands in `interpreter_enter`, which is the only place
//! that loads the entrypoint/`MD`, checks the block's `entry` and heals
//! `closure.code` ([`emit_interpreter_entry`]), a healed closure of compiled
//! code goes direct.

use onlineasm::{Label, VReg, onlineasm};

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

/// The entry of a baseline unit: jump to the block's current entry when it is
/// not this redirect, otherwise fall through to the baseline instructions.
pub(crate) fn emit_optimized_redirect(masm: &mut Masm, redirect: Label) {
    let (cp, t0, t1) = (regs::CP, tmp::T0, tmp::T1);
    let block = masm.assembler().mem(cp, sabi::CLOSURE_CODE_BLOCK as i32);
    let entry = masm.assembler().mem(t0, sabi::CODE_BLOCK_ENTRY as i32);
    let interpreted = masm.label();
    onlineasm!(*masm.assembler();
        loadq {t0}, {block};
        loadp {t1}, {entry};
        btpz {t1}, {interpreted};
        bpeq {t1}, -1, {interpreted};
    );

    onlineasm!(*masm.assembler(); leap {t0}, {redirect};);
    onlineasm!(*masm.assembler();
        bpeq {t1}, {t0}, {interpreted};
        jmp {t1};
    );
    masm.bind(interpreted);
}

/// Jump to the block's current compiled entry. It is the optimized entry
/// when one is installed, else the baseline entry.
pub(crate) fn emit_jump_compiled_entry(
    masm: &mut Masm,
    block: VReg,
    target: VReg,
    interpreted: Label,
) {
    let entry = masm.assembler().mem(block, sabi::CODE_BLOCK_ENTRY as i32);
    onlineasm!(*masm.assembler();
        loadp {target}, {entry};
        btpz {target}, {interpreted};
        bpeq {target}, -1, {interpreted};
        jmp {target};
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
        // Stable-entry mode heals `closure.code` to the baseline redirect,
        // which reads the current entry on every call. Otherwise the closure
        // is healed straight to the best entry.
        let entry_offset = if flags::jit_stable_entry() {
            sabi::CODE_BLOCK_BASELINE_ENTRY
        } else {
            sabi::CODE_BLOCK_ENTRY
        };
        let entry = masm.assembler().mem(tmp::T0, entry_offset as i32);
        let code_at = masm.assembler().mem(cp, sabi::CLOSURE_CODE as i32);
        onlineasm!(*masm.assembler();
            loadp {tmp::T0}, {entry};
            btpz {tmp::T0}, {to_interpreter};
            bpeq {tmp::T0}, -1, {to_interpreter};
            storep {code_at}, {tmp::T0};
            jmp {tmp::T0};
        );
        masm.bind(to_interpreter);
    }
    fallthrough(masm);
}
