use super::abi::*;
use crate::{
    Gc,
    cps_ssa::{Module, Proc},
    jit::regalloc::{ProcRegalloc, allocate_proc},
    runtime::Context,
};
use asmkit::{
    core::buffer::{CodeBuffer, ConstantData},
    util::align_up,
    x86::*,
};

pub struct CompilerX64<'gc, 'a> {
    pub ctx: Context<'gc>,
    pub module: Gc<'gc, Module<'gc>>,
    pub proc: Gc<'gc, Proc<'gc>>,
    pub asm: Assembler<'a>,
    pub regalloc: ProcRegalloc<Gpq>,
}

impl<'gc, 'a> CompilerX64<'gc, 'a> {
    pub fn new(
        ctx: Context<'gc>,
        module: Gc<'gc, Module<'gc>>,
        proc: Gc<'gc, Proc<'gc>>,
        code_buffer: &'a mut CodeBuffer,
    ) -> Self {
        Self {
            ctx,

            module,
            proc,
            asm: Assembler::new(code_buffer),
            regalloc: allocate_proc(&proc, &X64Abi).unwrap(),
        }
    }

    pub fn entrypoint(&mut self) {
        let spill_slots = self.regalloc.spill_slots as usize;
        let stack_size = spill_slots * 8;
        let aligned_stack_size = align_up(stack_size, 16);

        // Prologue
    }

    fn emit_jumptable_for_rest(&mut self) {
        /* jumptable = [&&args0, &&args1, &&args2, ...]
        if (RAX > max args in regs) {initialize with args in stack; goto argsN }
        else {
         label = jumptable[RAX]
         RAX = NULL
         goto *label
        }
        argsN:
        ...
        args2:
          RAX = cons(RCX, RAX)
          // fallthrough
        args1:
          RAX = cons(RDI, RAX)
          // fallthrough
        args0:
          return RAX */

        let variadic_pos = self.proc.arg_params.len() + self.proc.retk_param.is_some() as usize;

        let max_args_in_regs = ARGUMENT_REGS.len();

        let box_from_stack = self.asm.get_label();
        let reg_labels = ARGUMENT_REGS
            .iter()
            .skip(variadic_pos)
            .map(|_| self.asm.get_label())
            .rev()
            .collect::<std::vec::Vec<_>>();

        // Compare RAX with max_args_in_regs
        self.asm.cmp(ARGC_REG, imm(max_args_in_regs as i32));
        self.asm.jg(box_from_stack);
        let jt = self
            .asm
            .add_constant(ConstantData::Bytes(vec![0; reg_labels.len() * 8]));
        self.asm.lea(R11, label_ptr(jt, 0, 8));

        for (i, &reg) in ARGUMENT_REGS.iter().enumerate().skip(variadic_pos).rev() {
            let label = reg_labels[i - variadic_pos];
            self.asm.bind_label(label);
            /* todo: cons(RAX, reg); fallthrough; */
        }
    }
}
