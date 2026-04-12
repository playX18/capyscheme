use crate::jit::x64::{abi::CALLER_SAVES, lower::CompilerX64};
use asmkit::x86::*;
impl<'gc, 'a> CompilerX64<'gc, 'a> {
    pub fn call_thunk(&mut self, addr: usize) {
        for &reg in CALLER_SAVES.iter() {
            self.asm.push(reg);
        }
        let total_size = CALLER_SAVES.len() * 8;
        let padding = (16 - (total_size % 16)) % 16;
        if padding > 0 {
            self.asm.sub(RSP, imm(padding as i64));
        }

        self.asm.mov(RAX, u64_ptr_abs(addr as _, 0));
        self.asm.call(RAX);

        if padding > 0 {
            self.asm.add(RSP, imm(padding as i64));
        }

        for &reg in CALLER_SAVES.iter().rev() {
            self.asm.pop(reg);
        }
    }
}
