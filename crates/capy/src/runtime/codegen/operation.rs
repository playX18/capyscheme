//! Operation calls: typed Rust helpers that return to the fast path.

use onlineasm::{Label, VReg, onlineasm};
use smallvec::SmallVec;

use super::masm::{Masm, RuntimeExtern};
use super::stubs;
use super::template::tmp;
use crate::runtime::prims::calls::PRIM_WRAPPERS;
use crate::runtime::sabi::regs;

/// The function an operation call reaches.
#[derive(Clone, Copy, Debug)]
pub enum OpTarget {
    Runtime(RuntimeExtern),
    /// A prim wrapper, linked statically by prim id.
    Prim(u16),
    /// A prim wrapper through `PRIM_WRAPPERS`, by the id in a register.
    PrimIndexed(VReg),
}

/// One argument, after the leading `th`.
#[derive(Clone, Copy, Debug)]
pub enum OpArg {
    Imm(i64),
    Reg(VReg),
    /// `(reg << shift) + add`: a byte size from an element count.
    Scaled {
        reg: VReg,
        shift: u8,
        add: i64,
    },
    /// Operand field `field`, decoded.
    Field {
        field: usize,
        signed: bool,
    },
    /// `r[field]`.
    Slot(usize),
    /// Frame slot `index`, resolved from a field by the JIT.
    FrameSlot(i32),
}

/// Where the result goes.
#[derive(Clone, Copy, Debug)]
pub enum OpResult {
    None,
    Reg(VReg),
    /// `r[field]`.
    Slot(usize),
    /// Frame slot `index`, resolved from a field by the JIT.
    FrameSlot(i32),
}

/// Where control goes after a successful call.
#[derive(Clone, Copy, Debug)]
pub enum OpContinue {
    /// Back into the template at `label`.
    Join(Label),
    /// Past the instruction.
    Next,
}

/// A call to an operation, as a template describes it.
#[derive(Clone, Debug)]
pub struct OperationCall {
    pub target: OpTarget,
    pub args: SmallVec<[OpArg; 8]>,
    /// Template registers that must survive the call. The operation must
    /// not collect if any of them holds a heap reference.
    pub preserve: SmallVec<[VReg; 8]>,
    pub result: OpResult,
    /// Whether the operation returns `(value, error)`, a nonzero error
    /// being a [`RaiseKind`](crate::runtime::vm::exceptions::RaiseKind)
    /// code with the irritant in `state().exit_value`, as the prim
    /// wrappers do.
    pub checks_error: bool,
    pub cont: OpContinue,
}

/// The register [`emit_operation_call`] leaves the error code in.
pub const ERROR_REG: VReg = tmp::T1;
/// The register a slot result passes through.
pub const RESULT_REG: VReg = tmp::T0;

/// Spill `live` and the call's registers, stage and load the arguments,
/// call, and restore. Afterwards the result is in the `OpResult::Reg`
/// register, or in [`RESULT_REG`] for a slot result (the caller stores it,
/// as only it can address a field), and the error code in [`ERROR_REG`]
/// when the call checks errors. `load` lowers `Field` and `Slot`
/// arguments into its register, reading only `PC` and `SFP`.
pub(crate) fn emit_operation_call(
    masm: &mut Masm,
    call: &OperationCall,
    live: &[VReg],
    load: &mut dyn FnMut(&mut Masm, OpArg, VReg),
) {
    assert!(
        call.args.len() <= 4,
        "operations take at most four arguments after `th`"
    );
    let result = match call.result {
        OpResult::Reg(reg) => Some(reg),
        OpResult::Slot(_) | OpResult::FrameSlot(_) => Some(RESULT_REG),
        OpResult::None => None,
    };

    // The frame: every register to spill, each argument, then both
    // return words.
    let mut saved: Vec<VReg> = Vec::new();
    let mut save = |reg: VReg| {
        if !saved.contains(&reg) {
            saved.push(reg);
        }
    };
    live.iter()
        .chain(&call.preserve)
        .copied()
        .for_each(&mut save);
    for arg in &call.args {
        match *arg {
            OpArg::Reg(reg) | OpArg::Scaled { reg, .. } => save(reg),
            _ => {}
        }
    }
    if let OpTarget::PrimIndexed(reg) = call.target {
        save(reg);
    }
    let restored = live.len() + call.preserve.len();
    for reg in result.iter().chain(call.checks_error.then_some(&ERROR_REG)) {
        assert!(
            !live.iter().chain(&call.preserve).any(|r| r == reg),
            "an operation's result register must not be live across it"
        );
    }
    let arg_base = saved.len();
    let ret_base = arg_base + call.args.len();
    let bytes = (8 * (ret_base + 2)).next_multiple_of(16);
    let sp = onlineasm::regs::sp;
    let at = |masm: &mut Masm, index: usize| masm.assembler().mem(sp, 8 * index as i32);
    let saved_at = |masm: &mut Masm, reg: VReg| {
        let index = saved.iter().position(|r| *r == reg).unwrap();
        masm.assembler().mem(sp, 8 * index as i32)
    };

    onlineasm!(*masm.assembler(); subp sp, {bytes as i64};);
    for (index, reg) in saved.iter().enumerate() {
        let slot = at(masm, index);
        onlineasm!(*masm.assembler(); storeq {slot}, {*reg};);
    }

    // Stage: every register source reads its spilled copy, so the scratch
    // may be any of them.
    let scratch = tmp::T0;
    for (index, arg) in call.args.iter().enumerate() {
        match *arg {
            OpArg::Imm(value) => onlineasm!(*masm.assembler(); move {scratch}, {value};),
            OpArg::Reg(reg) => {
                let from = saved_at(masm, reg);
                onlineasm!(*masm.assembler(); loadq {scratch}, {from};);
            }
            OpArg::Scaled { reg, shift, add } => {
                let from = saved_at(masm, reg);
                onlineasm!(*masm.assembler(); loadq {scratch}, {from};);
                if shift != 0 {
                    onlineasm!(*masm.assembler(); lshiftq {scratch}, {shift as i64};);
                }
                if add != 0 {
                    onlineasm!(*masm.assembler(); addp {scratch}, {scratch}, {add};);
                }
            }
            OpArg::FrameSlot(slot) => {
                let from = masm.assembler().mem(regs::SFP, 8 * slot);
                onlineasm!(*masm.assembler(); loadq {scratch}, {from};);
            }
            OpArg::Field { .. } | OpArg::Slot(_) => load(masm, *arg, scratch),
        }
        let to = at(masm, arg_base + index);
        onlineasm!(*masm.assembler(); storeq {to}, {scratch};);
    }

    // Call. `T2`/`T3` are no C argument register on either target.
    masm.save_scheme_state_below(bytes);
    let (a0, a1, a2, a3, a4) = stubs::arg_regs5();
    onlineasm!(*masm.assembler(); move {a0}, {regs::TC};);
    for (index, reg) in [a1, a2, a3, a4]
        .into_iter()
        .take(call.args.len())
        .enumerate()
    {
        let from = at(masm, arg_base + index);
        onlineasm!(*masm.assembler(); loadq {reg}, {from};);
    }
    match call.target {
        OpTarget::Runtime(helper) => {
            let target = masm.declare_rust_fn(helper);
            onlineasm!(*masm.assembler(); call {target};);
        }
        OpTarget::Prim(id) => {
            let target = masm.declare_prim_fn(id);
            onlineasm!(*masm.assembler(); call {target};);
        }
        OpTarget::PrimIndexed(reg) => {
            let (table, target) = (tmp::T2, tmp::T3);
            let from = saved_at(masm, reg);
            let entry = masm.assembler().mem_indexed(table, target, 8, 0);
            onlineasm!(*masm.assembler();
                loadq {target}, {from};
                move {table}, {PRIM_WRAPPERS.as_ptr() as i64};
                loadq {target}, {entry};
                call {target};
            );
        }
    }
    masm.restore_scheme_state();

    // Restore.
    let (r0, r1) = stubs::ret_regs();
    let (ret0, ret1) = (at(masm, ret_base), at(masm, ret_base + 1));
    onlineasm!(*masm.assembler();
        storeq {ret0}, {r0};
        storeq {ret1}, {r1};
    );
    for (index, reg) in saved.iter().enumerate().take(restored) {
        let slot = at(masm, index);
        onlineasm!(*masm.assembler(); loadq {*reg}, {slot};);
    }
    if let Some(reg) = result {
        onlineasm!(*masm.assembler(); loadq {reg}, {ret0};);
    }
    if call.checks_error {
        onlineasm!(*masm.assembler(); loadq {ERROR_REG}, {ret1};);
    }
    onlineasm!(*masm.assembler(); addp sp, {bytes as i64};);
}
