//! ABI definitions for x64.
//!
//! X64 has the following calling convention:
//!
//! - RCX, RDX, RBX, RBP, RSI, RDI, R8, R9, R10, R13 are used for passing arguments to functions.
//! - RAX is used to pass the number of arguments to a function
//! - R14 is used to pass the closure pointer to a function
//! - R15 is used to pass the context pointer to a function
//! - R11, R12 are used as a scratch registers.
//! - *No* callee-saved registers are used for passing arguments to functions, so all registers except R11 are allocatable.
//!
//! Calls to SystemV functions must follow the SystemV calling convention, which means that RDI, RSI, RDX, RCX, R8, and R9 are used for passing arguments to SystemV functions, and RAX is used for the return value.
//!
//! Function calls to Scheme never happen vai `call`, since we use CPS. So all calls end up being
//! `jmp` + epilogue for setting up arguments. If there are more arguments
//! than the number of argument registers, the caller is responsible to adjust the stack
//! pointer and push the rest of the arguments onto the stack.

use asmkit::x86::*;

use crate::{
    cps_ssa::{Inst, InstKind},
    jit::regalloc::{InstConstraints, RegallocAbi},
};

pub const RETURN0: Gpq = RAX;
pub const RETURN1: Gpq = RDX;

pub const ARGC_REG: Gpq = RAX;
pub const CLOSURE_POINTER_REG: Gpq = R14;
pub const CONTEXT_POINTER_REG: Gpq = R15;

pub const ALLOCATABLE_REGS: &[Gpq] = &[RAX, RCX, RDX, RBX, RBP, RSI, RDI, R8, R9, R10, R13];
pub const SCRATCH_REGISTER: Gpq = R11;
pub const SCRATCH_REGISTER_2: Gpq = R12;

pub const SYSV_ARGUMENT_REGS: &[Gpq] = &[RDI, RSI, RDX, RCX, R8, R9];

pub const ARGUMENT_REGS: &[Gpq] = &[RCX, RDX, RBX, RBP, RSI, RDI, R8, R9, R10, R13];

/// Callee-saved registers according to the SystemV calling convention.
///
/// These are saved on entry to Scheme, and restored before exiting Scheme.
#[cfg(unix)]
pub const CALLEE_SAVES: &[Gpq] = &[RBX, RBP, R12, R13, R14, R15];

/// Caller-saved registers according to the SystemV calling convention.
///
/// These are saved during calls to SystemV functions, and restored after the call returns.
#[cfg(unix)]
pub const CALLER_SAVES: &[Gpq] = &[RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11];

pub struct X64Abi;

impl RegallocAbi for X64Abi {
    type Reg = Gpq;

    fn allocatable_regs(&self) -> &[Self::Reg] {
        ALLOCATABLE_REGS
    }

    fn scratch_reg(&self) -> Self::Reg {
        SCRATCH_REGISTER
    }

    fn argc_reg(&self) -> Self::Reg {
        ARGC_REG
    }

    fn closure_reg(&self) -> Self::Reg {
        CLOSURE_POINTER_REG
    }

    fn context_reg(&self) -> Self::Reg {
        CONTEXT_POINTER_REG
    }

    fn argument_regs(&self) -> &[Self::Reg] {
        ARGUMENT_REGS
    }

    fn inst_constraints(&self, inst: &Inst<'_>) -> InstConstraints<Self::Reg> {
        match inst.kind {
            InstKind::PrimCall { .. } => InstConstraints {
                clobbers: vec![RDX],
                fixed_defs: vec![RAX],
                ..Default::default()
            },
            _ => InstConstraints::default(),
        }
    }
}
