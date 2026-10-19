//! ## The interpreter context
//!
//! [`InterpreterTemplateContext`] is the
//! [`TemplateContext`](crate::runtime::codegen::template::TemplateContext)
//! for the template interpreter.

use bytecodegen::OperandWidth;
use onlineasm::{Extern, Label, VReg, onlineasm};

use crate::bytecode::def::{self, OpcodeId};
use crate::runtime::codegen::jit;
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::operation::{ERROR_REG, RESULT_REG, emit_operation_call};
use crate::runtime::codegen::stubs;
use crate::runtime::codegen::template::{
    ArithSpec, Condition, Flow, OpArg, OpContinue, OpResult, Operand, Operands, OperationCall,
    Slow, SlowPath, SlowPaths, Speculation, emit_branch_register_immediate,
    emit_branch_register_register, emit_control, emit_reload_code, metadata_entry_size,
    metadata_field_offset, metadata_id_operand, metadata_kind, op_len, tmp,
};
use crate::runtime::sabi::regs;
use crate::utils::flags;

/// The interpreter context
pub struct InterpreterTemplateContext {
    pub width: OperandWidth,
    pub op: OpcodeId,
    pub map: Label,
    /// The `interpreter_return`.
    pub interpreter_return: Label,
    pub bail: Extern,
    pub paths: SlowPaths,
}

pub const MAP_TABLE_SLOTS: usize = 3 * 256;
pub const MAP_RETURN_SLOT: usize = MAP_TABLE_SLOTS;
pub const MAP_RETURN_OFF: i32 = (MAP_RETURN_SLOT * 8) as i32;
pub const MAP_ENTER_SLOT: usize = MAP_TABLE_SLOTS + 1;
pub const MAP_ENTER_OFF: i32 = (MAP_ENTER_SLOT * 8) as i32;

impl InterpreterTemplateContext {
    /// Byte offset of operand `field` from `PC`.
    fn operand_offset(&self, field: usize) -> i32 {
        let header = match self.width {
            OperandWidth::Narrow => 1,
            OperandWidth::Wide16 => 3,
            OperandWidth::Wide32 => 5,
        };
        (header + field * self.width.bytes()) as i32
    }

    /// Decode operand `field` into `dst`, sign- or zero-extended.
    fn decode_into(&mut self, masm: &mut Masm, field: usize, signed: bool, dst: VReg) {
        let at = masm.assembler().mem(regs::PC, self.operand_offset(field));
        match (self.width, signed) {
            (OperandWidth::Narrow, false) => onlineasm!(*masm.assembler(); loadb {dst}, {at};),
            (OperandWidth::Narrow, true) => onlineasm!(*masm.assembler();
                loadb {dst}, {at};
                sxb2q {dst}, {dst};
            ),
            (OperandWidth::Wide16, false) => onlineasm!(*masm.assembler(); loadh {dst}, {at};),
            (OperandWidth::Wide16, true) => onlineasm!(*masm.assembler();
                loadh {dst}, {at};
                sxh2q {dst}, {dst};
            ),
            (OperandWidth::Wide32, false) => onlineasm!(*masm.assembler(); loadi {dst}, {at};),
            (OperandWidth::Wide32, true) => onlineasm!(*masm.assembler();
                loadi {dst}, {at};
                sxi2q {dst}, {dst};
            ),
        }
    }

    fn dispatch(&mut self, masm: &mut Masm) {
        stubs::emit_dispatch(masm, self.map);
    }
}

impl Operands for InterpreterTemplateContext {
    fn load_register(&mut self, masm: &mut Masm, field: usize, dst: VReg) {
        self.decode_into(masm, field, false, dst);
        let slot = masm.assembler().mem_indexed(regs::SFP, dst, 8, 0);
        onlineasm!(*masm.assembler();
            loadq {dst}, {slot};
        );
    }

    fn store_register(&mut self, masm: &mut Masm, field: usize, src: VReg, tmp: VReg) {
        self.decode_into(masm, field, false, tmp);
        let slot = masm.assembler().mem_indexed(regs::SFP, tmp, 8, 0);
        onlineasm!(*masm.assembler();
            storeq {slot}, {src};
        );
    }

    fn operand(&mut self, masm: &mut Masm, field: usize, signed: bool) -> Operand {
        self.decode_into(masm, field, signed, tmp::T0);
        Operand::Register(tmp::T0)
    }

    fn constant(&mut self, masm: &mut Masm, field: usize, dst: VReg) {
        // Constant `i` is `constant_pointer[-3 - i]`, and `-8 * (3 + i) == 8 * !i - 16`.
        self.decode_into(masm, field, false, dst);
        let slot = masm
            .assembler()
            .mem_indexed(regs::CONSTANT_POINTER, dst, 8, -16);
        onlineasm!(*masm.assembler();
            notq {dst};
            loadq {dst}, {slot};
        );
    }

    fn metadata(&mut self, masm: &mut Masm, kind: usize, field: usize, dst: VReg, tmp: VReg) {
        debug_assert_eq!(kind, metadata_kind(self.op));
        let entry_size = metadata_entry_size(self.op);
        let field_off = metadata_field_offset(self.op, field) as i32;
        // `md + u32 offset[kind] + id * size + field`.
        self.decode_into(masm, metadata_id_operand(self.op), false, dst);
        let md = regs::MD;
        let offset_at = masm.assembler().mem(md, (4 * kind) as i32);
        onlineasm!(*masm.assembler();
            loadi {tmp}, {offset_at};
            addp {tmp}, {tmp}, {md};
        );
        let scale = match entry_size {
            1 | 2 | 4 | 8 => entry_size as u8,
            48 => {
                // `id * 48 == ((id * 3) * 2) * 8`: `leap` triples, the shift doubles, the scale is 8.
                let triple = masm.assembler().mem_indexed(dst, dst, 2, 0);
                onlineasm!(*masm.assembler();
                    leap {dst}, {triple};
                    lshiftq {dst}, 1;
                );
                8
            }
            size => {
                onlineasm!(*masm.assembler(); mulq {dst}, {dst}, {size as i64};);
                1
            }
        };
        let entry = masm.assembler().mem_indexed(tmp, dst, scale, field_off);
        onlineasm!(*masm.assembler();
            leap {dst}, {entry};
        );
    }

    fn machine_entry(&mut self, masm: &mut Masm, dst: VReg) {
        let map = self.map;
        let slot = masm.assembler().mem(dst, MAP_ENTER_OFF);
        onlineasm!(*masm.assembler();
            leap {dst}, {map};
            loadq {dst}, {slot};
        );
    }
}

impl Flow for InterpreterTemplateContext {
    fn next(&mut self, masm: &mut Masm) {
        // The next opcode's address is `pc + len`, a per-handler constant,
        // so load it before bumping `PC`: the opcode load no longer waits
        // for the add.
        let pc = regs::PC;
        let len = op_len(self.op, self.width) as i32;
        let op_at = masm.assembler().mem(pc, len);
        onlineasm!(*masm.assembler();
            loadb {tmp::T2}, {op_at};
            addp {pc}, {pc}, {len as i64};
        );
        stubs::emit_dispatch_preloaded(masm, self.map);
    }

    fn jump(&mut self, masm: &mut Masm, field: usize) {
        self.decode_into(masm, field, true, tmp::T0);
        let (pc, t0) = (regs::PC, tmp::T0);
        onlineasm!(*masm.assembler();
            addp {pc}, {pc}, {t0};
        );
        self.dispatch(masm);
    }

    fn branch_if(
        &mut self,
        masm: &mut Masm,
        cond: Condition,
        a: Operand,
        b: Operand,
        field: usize,
    ) {
        let taken = masm.label();
        match (a, b) {
            (Operand::Register(a), Operand::Register(b)) => {
                emit_branch_register_register(masm, cond, a, b, taken)
            }
            (Operand::Register(a), Operand::Immediate(b))
                if matches!(cond, Condition::Eq | Condition::Ne) =>
            {
                emit_branch_register_immediate(masm, cond, a, b, taken)
            }
            _ => panic!("template: unsupported branch operands"),
        }
        self.next(masm);
        masm.bind(taken);
        self.jump(masm, field);
    }

    fn call_return_point(&mut self, masm: &mut Masm, frame: VReg) {
        let (pc, interpreter_return) = (regs::PC, self.interpreter_return);
        let len = op_len(self.op, self.width) as i32;
        let next = masm.assembler().mem(pc, len);
        let resume = masm.assembler().mem(frame, -8);
        let ra = masm.assembler().mem(frame, 0);
        onlineasm!(*masm.assembler();
            leap {tmp::T0}, {next};
            storep {resume}, {tmp::T0};
            leap {tmp::T0}, {interpreter_return};
            storep {ra}, {tmp::T0};
        );
    }

    fn enter_callee(&mut self, masm: &mut Masm) {
        jit::emit_enter_callee(masm);
    }

    fn enter_self(&mut self, masm: &mut Masm) {
        if flags::jit() {
            jit::emit_enter_callee(masm);
        } else {
            onlineasm!(*masm.assembler();
                move {regs::PC}, {regs::CONSTANT_POINTER};
            );
            self.dispatch(masm);
        }
    }

    fn resume_caller(&mut self, masm: &mut Masm, slow: Label) {
        let (sfp, pc, interpreter_return) = (regs::SFP, regs::PC, self.interpreter_return);
        let ra = masm.assembler().mem(sfp, 0);
        let resume = masm.assembler().mem(sfp, -8);
        onlineasm!(*masm.assembler();
            loadq {tmp::T0}, {ra};
            leap {tmp::T1}, {interpreter_return};
            bqneq {tmp::T0}, {tmp::T1}, {slow};
            loadp {pc}, {resume};
        );
        self.dispatch(masm);
    }

    fn reload_code(&mut self, masm: &mut Masm) {
        emit_reload_code(masm);
    }

    fn nop_native_check(&mut self, masm: &mut Masm, slow: Label) -> bool {
        onlineasm!(*masm.assembler();
            bqeq {regs::PC}, {regs::CONSTANT_POINTER}, {slow};
        );
        true
    }

    fn switch_int(&mut self, masm: &mut Masm, src: usize, table: usize, is_char: bool) {
        self.load_register(masm, src, tmp::T1);
        let fallthrough = masm.label();
        masm.switch_int_key(tmp::T1, tmp::T2, tmp::T0, fallthrough, is_char);
        // The table address: the code buffer's aux pool plus `4 * index`.
        // `T2` holds the key.
        self.decode_into(masm, table, false, tmp::T0);
        let pool_at = masm.assembler().mem(regs::CONSTANT_POINTER, -16);
        onlineasm!(*masm.assembler();
            loadq {tmp::T1}, {pool_at};
            lshiftq {tmp::T0}, 2;
            addp {tmp::T0}, {tmp::T0}, {tmp::T1};
        );
        let count_at = masm.assembler().mem(tmp::T0, 4);
        let dense = masm.label();
        let dense_flag = def::SWITCH_DENSE_FLAG as i64;
        onlineasm!(*masm.assembler();
            loadi {tmp::T3}, {count_at};
            bqaeq {tmp::T3}, {dense_flag}, {dense};
        );
        // Sparse: search `(entries, count, key)`, a miss shares the
        // default path below.
        let (search, miss) = (masm.label(), masm.label());
        onlineasm!(*masm.assembler();
            move {tmp::T1}, {tmp::T3};
            addp {tmp::T0}, {tmp::T0}, 8;
            call {search};
            btqz {tmp::T1}, {miss};
            addp {regs::PC}, {regs::PC}, {tmp::T2};
        );
        self.dispatch(masm);
        masm.bind(miss);
        self.decode_into(masm, table, false, tmp::T0);
        let pool_at = masm.assembler().mem(regs::CONSTANT_POINTER, -16);
        let default_at = masm.assembler().mem(tmp::T0, 0);
        onlineasm!(*masm.assembler();
            loadq {tmp::T1}, {pool_at};
            lshiftq {tmp::T0}, 2;
            addp {tmp::T0}, {tmp::T0}, {tmp::T1};
            loadis {tmp::T2}, {default_at};
            addp {regs::PC}, {regs::PC}, {tmp::T2};
        );
        self.dispatch(masm);
        // Dense: `idx = key - min`, bounds-checked against the span.
        masm.bind(dense);
        let span_mask = (def::SWITCH_DENSE_FLAG - 1) as i64;
        let min_at = masm.assembler().mem(tmp::T0, 8);
        let entry = masm.assembler().mem_indexed(tmp::T0, tmp::T2, 4, 12);
        onlineasm!(*masm.assembler();
            andq {tmp::T3}, {tmp::T3}, {span_mask};
            loadi {tmp::T1}, {min_at};
            sxi2q {tmp::T1}, {tmp::T1};
            subq {tmp::T2}, {tmp::T2}, {tmp::T1};
            bqaeq {tmp::T2}, {tmp::T3}, {miss};
            loadis {tmp::T1}, {entry};
            addp {regs::PC}, {regs::PC}, {tmp::T1};
        );
        self.dispatch(masm);
        masm.bind(search);
        stubs::emit_switch_search(masm, stubs::SwitchSearchKind::Int);
        masm.bind(fallthrough);
        self.next(masm);
    }

    fn switch_sym(&mut self, masm: &mut Masm, src: usize, table: usize) {
        self.load_register(masm, src, tmp::T1);
        let fallthrough = masm.label();
        masm.switch_sym_key(tmp::T1, tmp::T2, tmp::T0, fallthrough);
        self.decode_into(masm, table, false, tmp::T0);
        let pool_at = masm.assembler().mem(regs::CONSTANT_POINTER, -16);
        onlineasm!(*masm.assembler();
            loadq {tmp::T1}, {pool_at};
            lshiftq {tmp::T0}, 2;
            addp {tmp::T0}, {tmp::T0}, {tmp::T1};
        );
        // The key is `hash & mask`.
        let mask_lo_at = masm.assembler().mem(tmp::T0, 8);
        let mask_hi_at = masm.assembler().mem(tmp::T0, 12);
        let count_at = masm.assembler().mem(tmp::T0, 4);
        let (search, miss) = (masm.label(), masm.label());
        onlineasm!(*masm.assembler();
            loadi {tmp::T1}, {mask_lo_at};
            loadi {tmp::T3}, {mask_hi_at};
            lshiftq {tmp::T3}, 32;
            orq {tmp::T1}, {tmp::T1}, {tmp::T3};
            andq {tmp::T2}, {tmp::T2}, {tmp::T1};
            loadi {tmp::T1}, {count_at};
            addp {tmp::T0}, {tmp::T0}, 16;
            call {search};
            btqz {tmp::T1}, {miss};
            addp {regs::PC}, {regs::PC}, {tmp::T2};
        );
        self.dispatch(masm);
        masm.bind(miss);
        self.decode_into(masm, table, false, tmp::T0);
        let pool_at = masm.assembler().mem(regs::CONSTANT_POINTER, -16);
        let default_at = masm.assembler().mem(tmp::T0, 0);
        onlineasm!(*masm.assembler();
            loadq {tmp::T1}, {pool_at};
            lshiftq {tmp::T0}, 2;
            addp {tmp::T0}, {tmp::T0}, {tmp::T1};
            loadis {tmp::T2}, {default_at};
            addp {regs::PC}, {regs::PC}, {tmp::T2};
        );
        self.dispatch(masm);
        masm.bind(search);
        stubs::emit_switch_search(masm, stubs::SwitchSearchKind::Sym);
        masm.bind(fallthrough);
        self.next(masm);
    }
}

impl Slow for InterpreterTemplateContext {
    fn control(&mut self, masm: &mut Masm, path: SlowPath) {
        emit_control(masm, &self.paths, path);
    }

    fn slow_control(&mut self, masm: &mut Masm, label: Label, path: SlowPath) {
        masm.bind(label);
        emit_control(masm, &self.paths, path);
    }

    fn call_operation(&mut self, masm: &mut Masm, slow: Label, call: OperationCall) {
        masm.bind(slow);
        let live = [regs::CP, regs::PC, regs::AC];
        emit_operation_call(masm, &call, &live, &mut |masm, arg, dst| match arg {
            OpArg::Field { field, signed } => self.decode_into(masm, field, signed, dst),
            OpArg::Slot(field) => self.load_register(masm, field, dst),
            _ => unreachable!("emit_operation_call lowers {arg:?} itself"),
        });
        let error = masm.label();
        if call.checks_error {
            onlineasm!(*masm.assembler(); bqneq {ERROR_REG}, 0, {error};);
        }
        match call.result {
            OpResult::Slot(field) => self.store_register(masm, field, RESULT_REG, tmp::T2),
            OpResult::FrameSlot(_) => unreachable!("only the JIT resolves frame slots"),
            OpResult::None | OpResult::Reg(_) => {}
        }
        match call.cont {
            OpContinue::Join(label) => onlineasm!(*masm.assembler(); jmp {label};),
            OpContinue::Next => self.next(masm),
        }
        if call.checks_error {
            masm.bind(error);
            self.control(masm, SlowPath::PrimError(ERROR_REG));
        }
    }

    fn slow_entry(&mut self, masm: &mut Masm, spec: &ArithSpec, path: SlowPath) {
        let _ = spec;
        emit_control(masm, &self.paths, path);
    }

    fn slow_site(&mut self, masm: &mut Masm, label: Label, spec: &ArithSpec, path: SlowPath) {
        let _ = spec;
        self.slow_control(masm, label, path);
    }

    fn slow_jump(&mut self, masm: &mut Masm, label: Label, field: usize) {
        masm.bind(label);
        self.jump(masm, field);
    }
}

impl Speculation for InterpreterTemplateContext {
    fn profiling(&self) -> bool {
        flags::jit()
    }

    fn link_checks(&self) -> bool {
        flags::jit()
    }
}
