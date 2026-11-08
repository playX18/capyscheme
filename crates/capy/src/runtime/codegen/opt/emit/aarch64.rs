//! The AArch64 register file of the Tier 2 emitter.

use onlineasm::regs as r;
use onlineasm::regs::VReg;

use crate::runtime::codegen::opt::regalloc::{PhysReg, RegFile, Scratch};

/// The first FPR id. `PhysReg(FPR_BASE + n)` is the `Fpr` number `n`.
pub const FPR_BASE: u16 = 32;

/// The temporary for a spilled FPR operand or result: `v7` (v23), outside
/// the pool and caller-saved, because v31 has no name.
pub(crate) const FPR_TEMP: VReg = r::v7;

/// `CP`, the closure/code pointer role, which is `t3` (x3).
pub const CP: PhysReg = PhysReg(3);
/// `AC`, the accumulator role, which is `t2` (x2).
pub const AC: PhysReg = PhysReg(2);
/// `RV`, the return value role, which is `t0` (x0).
pub const RV: PhysReg = PhysReg(0);
/// `PC`, the program counter role, which is `csr4` (x23). It is reclaimed.
pub const PC: PhysReg = PhysReg(23);
/// `CONSTANT_POINTER`, which is `csr5` (x24). It is reclaimed.
pub const CONSTANT_POINTER: PhysReg = PhysReg(24);
/// `MD`, which is `csr6` (x25). It is reclaimed.
pub const MD: PhysReg = PhysReg(25);

/// The helper argument registers, in `PhysRole::OpArg` order: `a1..a4` of
/// `stubs::arg_regs5` (x1, x2, x3, x4). `a0` carries `TC` and is not here.
pub const OP_ARGS: [PhysReg; 4] = [PhysReg(1), PhysReg(2), PhysReg(3), PhysReg(4)];

/// The caller-saved registers of the pool, which an out-of-line helper may
/// destroy: x0, x2 and x3, followed by every pool FPR.
pub const SLOW_CLOBBERS: [PhysReg; 22] = [
    PhysReg(0),
    PhysReg(2),
    PhysReg(3),
    PhysReg(8),
    PhysReg(9),
    PhysReg(10),
    PhysReg(11),
    PhysReg(12),
    PhysReg(FPR_BASE),
    PhysReg(FPR_BASE + 1),
    PhysReg(FPR_BASE + 2),
    PhysReg(FPR_BASE + 3),
    PhysReg(FPR_BASE + 4),
    PhysReg(FPR_BASE + 5),
    PhysReg(FPR_BASE + 6),
    PhysReg(FPR_BASE + 7),
    PhysReg(FPR_BASE + 16),
    PhysReg(FPR_BASE + 17),
    PhysReg(FPR_BASE + 18),
    PhysReg(FPR_BASE + 19),
    PhysReg(FPR_BASE + 20),
    PhysReg(FPR_BASE + 21),
];

/// The extra caller-saved registers that join the pool:
/// `x13-x15` and `d24-d30`. They are slow-path clobbers only while they are
/// allocatable.
const POOL_EXTRA_SLOW_CLOBBERS: [PhysReg; 10] = [
    PhysReg(13),
    PhysReg(14),
    PhysReg(15),
    PhysReg(FPR_BASE + 24),
    PhysReg(FPR_BASE + 25),
    PhysReg(FPR_BASE + 26),
    PhysReg(FPR_BASE + 27),
    PhysReg(FPR_BASE + 28),
    PhysReg(FPR_BASE + 29),
    PhysReg(FPR_BASE + 30),
];

/// The assembler's scratch registers: x16 and v31. They are never allocated.
/// The second GPR scratch, x17, is not in the pool either, so the pool does
/// not need it. [`vreg`] returns `None` for x16 and v31, because the portable
/// table has no spelling for them.
pub const SCRATCH: Scratch = Scratch {
    gpr: PhysReg(16),
    fpr: PhysReg(FPR_BASE + 31),
};

/// The allocatable register file and the scratch pair. The register
/// allocator takes both.
pub fn reg_file() -> (RegFile, Scratch) {
    // x8 to x15 (`t8..t15`) are caller-saved and no SABI role, template
    // temporary or stub uses them, so they join the pool after the roles.
    let mut gprs = vec![
        CP,
        AC,
        RV,
        PC,
        CONSTANT_POINTER,
        MD,
        PhysReg(8),
        PhysReg(9),
        PhysReg(10),
        PhysReg(11),
        PhysReg(12),
    ];
    // v22 and v23 (`v6`, `v7`) are the emitter's FPR temporaries for spilled
    // operands: caller-saved, so they need no save around an instruction.
    let mut fprs: Vec<PhysReg> = (0..8)
        .chain(16..22)
        .map(|n| PhysReg(FPR_BASE + n))
        .collect();
    let mut slow_clobbers = SLOW_CLOBBERS.to_vec();

    gprs.extend([PhysReg(13), PhysReg(14), PhysReg(15)]);
    fprs.extend((24..=30).map(|n| PhysReg(FPR_BASE + n)));
    slow_clobbers.extend(POOL_EXTRA_SLOW_CLOBBERS);

    let regs = RegFile {
        gprs,
        fprs,
        rv: RV,
        op_args: OP_ARGS.to_vec(),
        slow_clobbers,
        callee_saved: vec![PC, CONSTANT_POINTER, MD],
    };
    (regs, SCRATCH)
}

/// The onlineasm register that spells `reg`. It is `None` for the scratch
/// registers and for any id the table does not name.
///
/// Each hardware register is spelled with one portable name, from the
/// `AARCH64_VREG_GPR` and `AARCH64_VREG_FPR` tables.
pub fn vreg(reg: PhysReg) -> Option<VReg> {
    Some(match reg.0 {
        // General-purpose registers. x16 and x17 are the scratch and have no name.
        0 => r::t0,
        1 => r::t1,
        2 => r::t2,
        3 => r::t3,
        4 => r::t4,
        5 => r::t5,
        6 => r::t6,
        7 => r::t7,
        8 => r::t8,
        9 => r::t9,
        10 => r::t10,
        11 => r::t11,
        12 => r::t12,
        13 => r::t13,
        14 => r::t14,
        15 => r::t15,
        19 => r::csr0,
        20 => r::csr1,
        21 => r::csr2,
        22 => r::csr3,
        23 => r::csr4,
        24 => r::csr5,
        25 => r::csr6,
        26 => r::csr7,
        27 => r::csr8,
        28 => r::csr9,
        29 => r::cfr,
        30 => r::lr,
        31 => r::sp,
        // FPRs. d0 to d7 are `ft0..ft7`, d8 to d15 are `csfr0..csfr7`, and
        // v0 to v7 are Fpr 16 to 23.
        n @ 32..=47 => {
            let fpr = n - FPR_BASE;
            return match fpr {
                0 => Some(r::ft0),
                1 => Some(r::ft1),
                2 => Some(r::ft2),
                3 => Some(r::ft3),
                4 => Some(r::ft4),
                5 => Some(r::ft5),
                6 => Some(r::ft6),
                7 => Some(r::ft7),
                8 => Some(r::csfr0),
                9 => Some(r::csfr1),
                10 => Some(r::csfr2),
                11 => Some(r::csfr3),
                12 => Some(r::csfr4),
                13 => Some(r::csfr5),
                14 => Some(r::csfr6),
                15 => Some(r::csfr7),
                _ => None,
            };
        }
        48 => r::v0,
        49 => r::v1,
        50 => r::v2,
        51 => r::v3,
        52 => r::v4,
        53 => r::v5,
        54 => r::v6,
        55 => r::v7,
        56 => r::v24,
        57 => r::v25,
        58 => r::v26,
        59 => r::v27,
        60 => r::v28,
        61 => r::v29,
        62 => r::v30,
        _ => return None,
    })
}
