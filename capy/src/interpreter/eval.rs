/*use super::frame::*;
use crate::runtime::value::proc::CodeBlock;

crate::bytecode::bytecode_list::generated::define_tail_interpreter!(
    fn dispatch<'gc>(
        ctx: Context<'gc>,
        sp: *mut Register<'gc>,
        fp: *mut Register<'gc>,
        pc: usize,
        cb: CodeBlock<'gc>,
    );
    ip {
        load = {
            cb.code().as_ptr().add(pc)
        };
        store(next_ip) = {
            let new_pc = next_ip as usize - cb.code().as_ptr() as usize;
            pc = new_pc;
        };
    }

    invalid(opcode, width) => panic!("Invalid opcode: {} at pc: {}", opcode, pc);
    Nop(op) => dispatch(ctx, sp, fp, pc, cb),
);
*/
