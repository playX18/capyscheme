use super::*;

/// Where a values-error header sends a multiple-values return: the
/// `values_error` stub, as a label inside the interpreter image or as the
/// absolute address compiled code reaches it by.
#[derive(Clone, Copy)]
pub enum ValuesError {
    Label(Label),
    Address(usize),
}

impl Masm {
    /// Jumps to the multiple-values target of the return address in `ra`.
    /// Every return address carries a header with a usable target, the
    /// runtime's own markers included (their full headers name themselves),
    /// so there is no slow arm: a full header jumps to its mv return address,
    /// a compact one to `ra` itself, or to `values_error` when its
    /// values-error bit is set. Terminal: every arm jumps.
    /// Clobbers `tmp0`/`tmp1`.
    pub fn emit_mv_jump(&mut self, ra: VReg, tmp0: VReg, tmp1: VReg, values_error: ValuesError) {
        // The mode word at `ra - 8` in both headers: bit 0 set means a
        // compact header, and then bit 1 is the values-error bit
        // (`sabi::mv_target` reads the same words).
        let mode_at = self
            .asm
            .mem(ra, sabi::COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP as i32);
        // A full header's mv return address at `ra - 32`.
        let mv_at = self
            .asm
            .mem(ra, sabi::RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP as i32);
        let compact = self.label();
        let error = self.label();

        onlineasm!(self.asm;
            loadq {tmp0}, {mode_at};
            andq {tmp1}, {tmp0}, {sabi::COMPACT_HEADER_MASK as i64};
            btqnz {tmp1}, {compact};
            loadq {tmp0}, {mv_at};
            jmp {tmp0};
        );
        self.bind(compact);
        onlineasm!(self.asm;
            andq {tmp1}, {tmp0}, {sabi::COMPACT_HEADER_VALUES_ERROR_MASK as i64};
            btqnz {tmp1}, {error};
            jmp {ra};
        );
        self.bind(error);
        match values_error {
            ValuesError::Label(label) => onlineasm!(self.asm; jmp {label};),
            ValuesError::Address(addr) => onlineasm!(self.asm;
                move {tmp0}, {addr as i64};
                jmp {tmp0};
            ),
        }
    }

    /// [`Masm::emit_mv_jump`] split in two: leaves the multiple-values target
    /// of the return address in `ra` in `target` and falls through, so the
    /// caller can write the values first. A values-error header yields the
    /// `values_error` stub as the target. Clobbers `tmp`.
    pub fn emit_mv_target(
        &mut self,
        ra: VReg,
        target: VReg,
        tmp: VReg,
        values_error: ValuesError,
    ) {
        let mode_at = self
            .asm
            .mem(ra, sabi::COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP as i32);
        let mv_at = self
            .asm
            .mem(ra, sabi::RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP as i32);
        let compact = self.label();
        let error = self.label();
        let done = self.label();

        onlineasm!(self.asm;
            loadq {target}, {mode_at};
            andq {tmp}, {target}, {sabi::COMPACT_HEADER_MASK as i64};
            btqnz {tmp}, {compact};
            loadq {target}, {mv_at};
            jmp {done};
        );
        self.bind(compact);
        onlineasm!(self.asm;
            andq {tmp}, {target}, {sabi::COMPACT_HEADER_VALUES_ERROR_MASK as i64};
            btqnz {tmp}, {error};
            move {target}, {ra};
            jmp {done};
        );
        self.bind(error);
        match values_error {
            ValuesError::Label(label) => onlineasm!(self.asm; leap {target}, {label};),
            ValuesError::Address(addr) => onlineasm!(self.asm; move {target}, {addr as i64};),
        }
        self.bind(done);
    }

    /// Saves the Scheme state (frame pointer and allocation cursor) into the thread context.
    pub fn save_scheme_state(&mut self) {
        self.save_scheme_state_below(0);
    }

    /// [`Masm::save_scheme_state`] with `extra` bytes pushed below the
    /// entry frame (an operation call's spill area), so that x86-64's
    /// native frame pointer still lands on the entry frame.
    pub fn save_scheme_state_below(&mut self, extra: usize) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let fp = self.asm.mem(tc, sabi::TC_STACK_FP as i32);
        #[cfg(target_arch = "x86_64")]
        {
            let native = self.asm.mem(
                onlineasm::regs::sp,
                (sabi::NATIVE_FP_FROM_SP + extra) as i32,
            );
            onlineasm!(self.asm;
                storep {fp}, {sfp};
                leap {sfp}, {native};
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let _ = extra;
            let ap = regs::AP;
            let cursor = self.asm.mem(tc, sabi::TC_AP as i32);
            onlineasm!(self.asm;
                storep {fp}, {sfp};
                storep {cursor}, {ap};
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = (tc, sfp, fp);
            unsupported_target!();
        }
    }

    /// Restores the Scheme state saved by `save_scheme_state`. The frame may
    /// have moved, so `SFP` is reloaded, not swapped back.
    pub fn restore_scheme_state(&mut self) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let fp = self.asm.mem(tc, sabi::TC_STACK_FP as i32);
        #[cfg(target_arch = "x86_64")]
        onlineasm!(self.asm;
            loadp {sfp}, {fp};
        );
        #[cfg(target_arch = "aarch64")]
        {
            let ap = regs::AP;
            let cursor = self.asm.mem(tc, sabi::TC_AP as i32);
            onlineasm!(self.asm;
                loadp {sfp}, {fp};
                loadp {ap}, {cursor};
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = (tc, sfp, fp);
            unsupported_target!();
        }
    }

    /// Save Scheme state, `call` a Rust `extern "C"` function declared with
    /// [`Masm::declare_rust_fn`] (arguments already in C argument registers),
    /// restore state. The call clobbers caller-saved registers, on AArch64 it
    /// also clobbers `lr`, which frame-owning entries spill in their prologue.
    pub fn call_rust(&mut self, target: Extern) {
        self.save_scheme_state();
        onlineasm!(self.asm;
            call {target};
        );
        self.restore_scheme_state();
    }

    /// As [`Masm::call_rust`], but the target address is in `target`: for
    /// table-indexed calls (the template interpreter's cold prims), where
    /// each call site reaches a different function without a link-time
    /// reloc. Arguments already in C argument registers.
    pub fn call_rust_indirect(&mut self, target: VReg) {
        self.save_scheme_state();
        onlineasm!(self.asm;
            call {target};
        );
        self.restore_scheme_state();
    }

    /// The `scheme_entry`-style prologue: subtract the frame, save the SABI
    /// callee-saved registers (and `lr` on AArch64). Layout and CFI constants
    /// are `sabi::SCHEME_ENTRY_*`.
    pub fn scheme_entry_prologue(&mut self) {
        let frame = sabi::SCHEME_ENTRY_FRAME_BYTES as i32;
        #[cfg(target_arch = "x86_64")]
        {
            let (number_tag, constant_pointer, md, tc) =
                (regs::NUMBER_TAG, regs::CONSTANT_POINTER, regs::MD, regs::TC);
            let (not_cell, sfp) = (regs::NOT_CELL_MASK, regs::SFP);
            onlineasm!(self.asm;
                subp sp, {frame};
                storeq [sp], {number_tag};
                storeq [sp + 8], {constant_pointer};
                storeq [sp + 16], {md};
                storeq [sp + 24], {tc};
                storeq [sp + 32], {not_cell};
                storeq [sp + 48], {sfp};
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let (tc, sfp, ap) = (regs::TC, regs::SFP, regs::AP);
            // `x22` is the template interpreter's opcode-map base (`regs::DISPATCH`), set once
            // per activation in `scheme_entry`, the entry frame preserves it
            // across nested activations.
            let x22 = regs::DISPATCH;
            let (pc, constant_pointer, md) = (regs::PC, regs::CONSTANT_POINTER, regs::MD);
            let (number_tag, not_cell) = (regs::NUMBER_TAG, regs::NOT_CELL_MASK);
            let free = onlineasm::regs::csr9;
            onlineasm!(self.asm;
                subp sp, {frame};
                storeq [sp], {tc};
                storeq [sp + 8], {sfp};
                storeq [sp + 16], {ap};
                storeq [sp + 24], {x22};
                storeq [sp + 32], {pc};
                storeq [sp + 40], {constant_pointer};
                storeq [sp + 48], {md};
                storeq [sp + 56], {number_tag};
                storeq [sp + 64], {not_cell};
                storeq [sp + 72], {free};
                storeq [sp + 80], lr;
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = frame;
            unsupported_target!();
        }
    }

    /// The matching epilogue: reload the saves, free the frame, `ret`.
    pub fn scheme_exit_epilogue(&mut self) {
        let frame = sabi::SCHEME_ENTRY_FRAME_BYTES as i32;
        #[cfg(target_arch = "x86_64")]
        {
            let (number_tag, constant_pointer, md, tc) =
                (regs::NUMBER_TAG, regs::CONSTANT_POINTER, regs::MD, regs::TC);
            let (not_cell, sfp) = (regs::NOT_CELL_MASK, regs::SFP);
            onlineasm!(self.asm;
                loadq {number_tag}, [sp];
                loadq {constant_pointer}, [sp + 8];
                loadq {md}, [sp + 16];
                loadq {tc}, [sp + 24];
                loadq {not_cell}, [sp + 32];
                loadq {sfp}, [sp + 48];
                addp sp, {frame};
                ret;
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let (tc, sfp, ap) = (regs::TC, regs::SFP, regs::AP);
            let x22 = regs::DISPATCH;
            let (pc, constant_pointer, md) = (regs::PC, regs::CONSTANT_POINTER, regs::MD);
            let (number_tag, not_cell) = (regs::NUMBER_TAG, regs::NOT_CELL_MASK);
            let free = onlineasm::regs::csr9;
            onlineasm!(self.asm;
                loadq {tc}, [sp];
                loadq {sfp}, [sp + 8];
                loadq {ap}, [sp + 16];
                loadq {x22}, [sp + 24];
                loadq {pc}, [sp + 32];
                loadq {constant_pointer}, [sp + 40];
                loadq {md}, [sp + 48];
                loadq {number_tag}, [sp + 56];
                loadq {not_cell}, [sp + 64];
                loadq {free}, [sp + 72];
                loadq lr, [sp + 80];
                addp sp, {frame};
                ret;
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = frame;
            unsupported_target!();
        }
    }

    /// Branch to `target` when `val` is a char.
    pub(crate) fn branch_if_char(&mut self, val: VReg, tmp: VReg, scratch: VReg, target: Label) {
        let char_mask = Value::CHAR_MASK;
        let char_tag = Value::CHAR_TAG;

        onlineasm!(self.asm;
            andq {tmp}, {val}, {char_mask};
            bqneq {tmp}, {char_tag}, .not_char;
            move {tmp}, {val};
            urshiftq {tmp}, 16;
            andq {tmp}, {tmp}, 0xffffffff;
            move {scratch}, 0xd800;
            bqb {tmp}, {scratch}, {target};
            move {scratch}, 0x10ffff;
            bqa {tmp}, {scratch}, .not_char;
            move {scratch}, 0xdfff;
            bqa {tmp}, {scratch}, {target};
            .not_char:
        );
    }

    pub fn switch_int_key(
        &mut self,
        val: VReg,
        key: VReg,
        scratch: VReg,
        fallthrough: Label,
        is_char: bool,
    ) {
        if is_char {
            //let is_char = self.label();
            let char_mask = Value::CHAR_MASK;
            let char_tag = Value::CHAR_TAG;
            onlineasm!(self.asm;
                andq {scratch}, {val}, {char_mask};
                bqneq {scratch}, {char_tag}, {fallthrough};
                move {key}, {val};
                urshiftq {key}, 16;
                andq {key}, {key}, 0xffffffff;
                move {scratch}, 0xd800;
                bqb {key}, {scratch}, .is_char;
                move {scratch}, 0x10ffff;
                bqa {key}, {scratch}, {fallthrough};
                move {scratch}, 0xdfff;
                bqa {key}, {scratch}, .is_char;
                jmp {fallthrough};
                .is_char:
            );
        } else {
            self.branch_if_not_fixnum(val, scratch, fallthrough);
            onlineasm!(self.asm;
                sxi2q {key}, {val};
            );
        }
    }

    /// `switch-symbol` key prolog: type-test `val` for a symbol, loading its
    /// hash into `key`, else jump to `fallthrough`. `val` is preserved,
    /// `scratch` is clobbered. Shared by both contexts (identical emission).
    pub fn switch_sym_key(&mut self, val: VReg, key: VReg, scratch: VReg, fallthrough: Label) {
        use crate::runtime::objectmodel::Symbol;
        self.branch_not_heap_class(val, builtin_class_ids::SYMBOL, scratch, fallthrough);
        let hash_at = self
            .asm
            .mem(val, core::mem::offset_of!(Symbol<'static>, hash) as i32);
        onlineasm!(self.asm;
            loadq {key}, {hash_at};
        );
    }
}
