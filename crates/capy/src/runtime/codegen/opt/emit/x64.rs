//! The x86-64 register file of the Tier 2 emitter.

use onlineasm::regs as r;
use onlineasm::regs::VReg;

use crate::runtime::codegen::opt::regalloc::{PhysReg, RegFile, Scratch};

/// The first FPR id. `PhysReg(FPR_BASE + n)` is `xmmN`.
pub const FPR_BASE: u16 = 32;

/// The temporary for a spilled FPR operand or result: `xmm7`.
pub(crate) const FPR_TEMP: VReg = r::ft7;

/// `CP`, the closure/code pointer role, which is `t3` (rcx).
pub const CP: PhysReg = PhysReg(1);
/// `AC`, the accumulator role, which is `t2` (rdx).
pub const AC: PhysReg = PhysReg(2);
/// `RV`, the return value role, which is `t0` (rax).
pub const RV: PhysReg = PhysReg(0);
/// `PC`, the program counter role, which is `t4` (r8). It is reclaimed.
pub const PC: PhysReg = PhysReg(8);
/// `CONSTANT_POINTER`, which is `csr1` (r12). It is reclaimed.
pub const CONSTANT_POINTER: PhysReg = PhysReg(12);
/// `MD`, which is `csr2` (r13). It is reclaimed.
pub const MD: PhysReg = PhysReg(13);

/// The helper argument registers, in `PhysRole::OpArg` order: `a1..a4` of
/// `stubs::arg_regs5` (rsi, rdx, rcx, r8). `a0` carries `TC` and is not here.
pub const OP_ARGS: [PhysReg; 4] = [PhysReg(6), PhysReg(2), PhysReg(1), PhysReg(8)];

/// The caller-saved registers of the pool, which an out-of-line helper may
/// destroy: rax, rcx, rdx and r8, followed by every pool FPR.
pub const SLOW_CLOBBERS: [PhysReg; 11] = [
    PhysReg(0),
    PhysReg(1),
    PhysReg(2),
    PhysReg(8),
    PhysReg(FPR_BASE),
    PhysReg(FPR_BASE + 1),
    PhysReg(FPR_BASE + 2),
    PhysReg(FPR_BASE + 3),
    PhysReg(FPR_BASE + 4),
    PhysReg(FPR_BASE + 5),
    PhysReg(FPR_BASE + 6),
];

/// The extra caller-saved registers that join the pool:
/// `xmm9` to `xmm15`. They are slow-path clobbers only while they are
/// allocatable. Not used on Windows, where xmm6-15 are callee-saved and
/// `scheme_entry` saves no FP registers.
const POOL_EXTRA_SLOW_CLOBBERS: [PhysReg; 7] = [
    PhysReg(FPR_BASE + 9),
    PhysReg(FPR_BASE + 10),
    PhysReg(FPR_BASE + 11),
    PhysReg(FPR_BASE + 12),
    PhysReg(FPR_BASE + 13),
    PhysReg(FPR_BASE + 14),
    PhysReg(FPR_BASE + 15),
];

/// The assembler's scratch registers: r11 and xmm7 (`ft7`). They are never
/// allocated. [`vreg`] returns `None` for them, because the portable table
/// has no spelling for r11.
pub const SCRATCH: Scratch = Scratch {
    gpr: PhysReg(11),
    fpr: PhysReg(FPR_BASE + 7),
};

/// The allocatable register file and the scratch pair. The register
/// allocator takes both.
pub fn reg_file() -> (RegFile, Scratch) {
    let gprs = vec![CP, AC, RV, PC, CONSTANT_POINTER, MD];
    let mut fprs: Vec<PhysReg> = (0..7).map(|n| PhysReg(FPR_BASE + n)).collect();
    let mut slow_clobbers = SLOW_CLOBBERS.to_vec();
    // xmm9-xmm15 are caller-saved under SysV. On Windows xmm6-15 are
    // callee-saved and `scheme_entry` does not save FP registers, so they
    // stay out of the pool there.
    if cfg!(not(windows)) {
        fprs.extend((9..=15).map(|n| PhysReg(FPR_BASE + n)));
        slow_clobbers.extend(POOL_EXTRA_SLOW_CLOBBERS);
    }
    let regs = RegFile {
        gprs,
        fprs,
        rv: RV,
        op_args: OP_ARGS.to_vec(),
        slow_clobbers,
        callee_saved: vec![CONSTANT_POINTER, MD],
    };
    (regs, SCRATCH)
}

/// The onlineasm register that spells `reg`. It is `None` for the scratch
/// registers and for any id the table does not name.
///
/// Each hardware register is spelled with one portable name, for example
/// rsi is `t1` (`csr5` also names it). The choice follows the table order in
/// offlineasm-ir `X86_VREG_GPR` and `arg_regs5`.
pub fn vreg(reg: PhysReg) -> Option<VReg> {
    Some(match reg.0 {
        // General-purpose registers.
        0 => r::t0,
        1 => r::t3,
        2 => r::t2,
        3 => r::csr0,
        4 => r::sp,
        5 => r::cfr,
        6 => r::t1,
        7 => r::t6,
        8 => r::t4,
        9 => r::t7,
        10 => r::t5,
        12 => r::csr1,
        13 => r::csr2,
        14 => r::csr3,
        15 => r::csr4,
        // xmm0 to xmm6 are spelled `ft0..ft6`. xmm7 is the scratch and
        // xmm8 the hard-coded second spilled-float temporary.
        n @ 32..=38 => {
            return Some(match n - FPR_BASE {
                0 => r::ft0,
                1 => r::ft1,
                2 => r::ft2,
                3 => r::ft3,
                4 => r::ft4,
                5 => r::ft5,
                _ => r::ft6,
            });
        }
        // xmm9 to xmm15, allocatable.
        n @ 41..=47 => match n - FPR_BASE {
            9 => r::ft9,
            10 => r::ft10,
            11 => r::ft11,
            12 => r::ft12,
            13 => r::ft13,
            14 => r::ft14,
            _ => r::ft15,
        },
        _ => return None,
    })
}
