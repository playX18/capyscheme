//! Deopt metadata: where each value of an exit is written when the frame is
//! rebuilt, and how the save area that `deopt_common` writes is laid out.
//!
//! Save area, as `deopt_common` builds it on the native stack (word index,
//! 8 bytes each, word 0 at the stack pointer):
//!
//! - word 0: `SFP`, the fp of the optimized frame.
//! - word `1 + i`: the `i`-th GPR of [`RegFile::gprs`].
//! - word `1 + G + SAVE_TEMPS + j`: the `j`-th FPR of [`RegFile::fprs`], raw
//!   f64 bits. `G` is `gprs.len()`.
//!
//! The words between the GPRs and the FPRs are temporaries that the runtime
//! does not read. The emitter writes this layout.

use std::sync::OnceLock;

use crate::runtime::codegen::opt::emit;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::Func;
use crate::runtime::codegen::opt::regalloc::{Loc, PhysReg, RegFile};

/// Words `deopt_common` reserves between the GPRs and the FPRs.
pub(crate) const SAVE_TEMPS: usize = 8;

/// Where a value of a deopt exit is written when the frame is rebuilt.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DeoptDest {
    /// The closure of frame `frame`, in the frame's slot 1.
    Closure { frame: u32 },
    /// Bytecode register `reg` of frame `frame`.
    Register { frame: u32, reg: u32 },
    /// The rest vector of a variadic procedure: its elements go to the rest
    /// area `r[base..]` of frame `frame`.
    RestArea { frame: u32, base: u32 },
    /// Extra argument `index` of a scalar-mode variadic procedure: it goes to
    /// `r[base + index]` of frame `frame` when `index` is below the count in
    /// register `cnt`.
    RestScalar { frame: u32, base: u32, index: u32, cnt: u32 },
    /// The vector of the extras from index `from` on: element `i` goes to
    /// `r[base + from + i]` of frame `frame`.
    RestTail { frame: u32, base: u32, from: u32, cnt: u32 },
}

/// The destinations of the values of `frame`, in the order of
/// [`Func::frame_values`]: the innermost frame first, each frame's closure
/// before its registers. Frame index 0 is the outermost frame of the unit.
pub(crate) fn deopt_dests(func: &Func<'_>, frame: FrameState) -> Vec<DeoptDest> {
    let mut chain = Vec::new();
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        chain.push(fs);
        cur = func.frames[fs].parent;
    }
    let count = chain.len() as u32;
    let mut dests = Vec::new();
    for (depth, &fs) in chain.iter().enumerate() {
        let index = count - 1 - depth as u32;
        let data = &func.frames[fs];
        dests.push(DeoptDest::Closure { frame: index });
        // A case-lambda frame's rest area is that clause's; the frame's pc
        // names it.
        let rest = data.rest;
        dests.extend(data.registers.iter().map(|&(reg, _)| match rest {
            Some(rest) => match rest.scalars {
                None if rest.reg == reg => DeoptDest::RestArea { frame: index, base: rest.base },
                Some(n) if reg >= rest.reg && reg < rest.reg + n => DeoptDest::RestScalar {
                    frame: index,
                    base: rest.base,
                    index: reg - rest.reg,
                    cnt: rest.cnt,
                },
                Some(n) if reg == rest.reg + n => DeoptDest::RestTail {
                    frame: index,
                    base: rest.base,
                    from: n,
                    cnt: rest.cnt,
                },
                _ => DeoptDest::Register { frame: index, reg },
            },
            None => DeoptDest::Register { frame: index, reg },
        }));
    }
    dests
}

/// The register file of this host, which fixes the save area's order.
pub(crate) fn regfile() -> &'static RegFile {
    static REGS: OnceLock<RegFile> = OnceLock::new();
    REGS.get_or_init(|| emit::register_file().0)
}

/// A read-only view of the save area that `deopt_common` built.
#[derive(Clone, Copy)]
pub(crate) struct SaveArea {
    words: *const u64,
}

impl SaveArea {
    /// # Safety
    ///
    /// `words` points at the save area of a live `deopt_common` call, which
    /// holds the words the layout above names.
    pub(crate) unsafe fn new(words: *const u64) -> Self {
        Self { words }
    }

    fn word(self, index: usize) -> u64 {
        // SAFETY: the layout keeps every index in the save area.
        unsafe { *self.words.add(index) }
    }

    /// The optimized frame's fp (`SFP` when the exit was taken).
    pub(crate) fn sfp(self) -> u64 {
        self.word(0)
    }

    /// The saved word of GPR `reg`, if `reg` is in the pool.
    pub(crate) fn gpr(self, reg: PhysReg) -> Option<u64> {
        let regs = regfile();
        let index = regs.gprs.iter().position(|&r| r == reg)?;
        Some(self.word(1 + index))
    }

    /// The saved word of FPR `reg`, if `reg` is in the pool.
    pub(crate) fn fpr(self, reg: PhysReg) -> Option<u64> {
        let regs = regfile();
        let index = regs.fprs.iter().position(|&r| r == reg)?;
        Some(self.word(1 + regs.gprs.len() + SAVE_TEMPS + index))
    }
}

/// Which register file a [`Loc::Reg`] names, for a raw read.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeoptRegClass {
    Gpr,
    Fpr,
}

/// The raw word of a value in `loc`, with `fp` the optimized frame and
/// `frame_size` its bytecode size in words. Spill slot `s` is word
/// `frame_size + s`.
///
/// # Safety
///
/// `fp` is the optimized frame, and `save` the save area of its exit.
pub(crate) unsafe fn raw_at(
    loc: Loc,
    class: DeoptRegClass,
    save: SaveArea,
    fp: *const u64,
    frame_size: usize,
) -> Option<u64> {
    match loc {
        Loc::Reg(reg) => match class {
            DeoptRegClass::Gpr => save.gpr(reg),
            DeoptRegClass::Fpr => save.fpr(reg),
        },
        // SAFETY: a spill slot is inside the optimized frame.
        Loc::Slot(slot) => Some(unsafe { *fp.add(frame_size + slot as usize) }),
    }
}
