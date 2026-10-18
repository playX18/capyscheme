//! Where a bytecode pc becomes a jump target.

use mmtk::util::Address;
use onlineasm::{Label, onlineasm};

use super::super::addr::imm;
use super::super::emission::Emission;
use super::super::masm::Masm;
use super::{
    Condition, Operand, emit_branch_register_immediate, emit_branch_register_register, holds,
    mirror, tmp,
};

/// Where control goes for one bytecode pc.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Target {
    /// A label of the image under construction.
    Label(Label),
    /// A machine address already known at emission time (another image).
    Absolute(Address),
    /// A label bound to a lazy edge: reaching it may materialize code.
    Edge(Label),
}

/// Where a context puts its slow and taken blocks.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Placement {
    /// At the end of the code, so fast paths fall through (a procedure
    /// body).
    Deferred,
    /// Right where the template emits them (a stub: one straight-line
    /// region).
    Inline,
}

/// How a compiled context resolves bytecode pcs.
pub(crate) trait Targets {
    /// The target of the instruction at `pc` (an offset into the
    /// procedure's bytecode, always a landing pc).
    fn target(&mut self, em: &mut Emission, masm: &mut Masm, pc: usize) -> Target;
    /// Whether `next` may emit nothing because the next instruction's
    /// target binds right after this template.
    fn falls_through(&self) -> bool {
        false
    }
    /// Where slow and taken blocks go.
    fn placement(&self) -> Placement;
}

/// Jump to `target`. An absolute target goes through `T0`, dead at every
/// exit: `next` runs after the result is stored, and branches materialize
/// targets only after their test consumed its operands.
pub(crate) fn emit_jump_to(masm: &mut Masm, target: Target) {
    match target {
        Target::Label(label) | Target::Edge(label) => {
            onlineasm!(*masm.assembler(); jmp {label};);
        }
        Target::Absolute(addr) => {
            let imm = imm(addr);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {imm};
                jmp {tmp::T0};
            );
        }
    }
}

/// Compare `a` and `b` and branch to `taken` when `cond` holds, else fall
/// through.
pub(crate) fn emit_branch_to(
    masm: &mut Masm,
    cond: Condition,
    a: Operand,
    b: Operand,
    taken: Label,
) -> bool {
    let (cond, a, b) = match (a, b) {
        (Operand::Immediate(a), Operand::Register(b)) => {
            (mirror(cond), Operand::Register(b), Operand::Immediate(a))
        }
        other => (cond, other.0, other.1),
    };
    match (a, b) {
        (Operand::Register(a), Operand::Register(b)) => {
            emit_branch_register_register(masm, cond, a, b, taken)
        }
        (Operand::Register(a), Operand::Immediate(b)) => {
            emit_branch_register_immediate(masm, cond, a, b, taken)
        }
        (Operand::Immediate(a), Operand::Immediate(b)) => {
            if !holds(cond, a, b) {
                return false;
            }
            onlineasm!(*masm.assembler(); jmp {taken};);
        }
        (Operand::Immediate(_), Operand::Register(_)) => unreachable!("mirrored above"),
    }
    true
}
