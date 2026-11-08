//! Machine code images for JIT: [`CodeUnit`]
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering::{Acquire, Relaxed};

use super::addr::{BytecodePc, CodeOffset};
use super::cache::{self, Partition};
use super::image::LoadedImage;
use super::install::Generation;
use super::jit::registry::{DataRange, InstructionRange, LIVE_UNITS, ResumeTable};
use super::opt::deopt::metadata::DeoptDest;
use super::opt::lir::GuardKind;
use super::opt::regalloc::{DeoptLoc, RematDesc};
use super::stubs::CallIcStub;
use crate::heap::Gc;
use crate::runtime::interp::stack::{Ip, Slot};
use crate::runtime::objectmodel::{CodeBlock, CodeImage};
use crate::runtime::sabi;
use mmtk::util::Address;

/// The compiler tier a [`CodeUnit`] came from.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum Tier {
    /// One image per procedure from the interpreter's templates.
    Baseline,
    /// Lazy basic block versioning: blocks compiled on first execution,
    /// specialized on what the code has seen.
    Lbbv,
    /// SSA to machine code, with deoptimization.
    Optimizing,
    /// A recorded hot path with side exits.
    Trace,
}

impl Tier {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Tier::Baseline => "baseline",
            Tier::Lbbv => "lbbv",
            Tier::Optimizing => "optimizing",
            Tier::Trace => "trace",
        }
    }

    /// Whether a compiler for this tier exists in this build.
    pub(crate) fn implemented(self) -> bool {
        matches!(self, Tier::Baseline | Tier::Optimizing)
    }
}

/// One machine-code offset and the LIR instruction emitted there, with the
/// IR values its registers came from. Kept only when `CAPY_OPT_ANNOTATE` is
/// set, for `%jit-disassembly` comments.
#[derive(Clone, Debug)]
pub(crate) struct Annotation {
    pub(crate) offset: usize,
    pub(crate) text: String,
}

/// Per-instruction layout of a unit's image, for listings and the profiler.
pub(crate) struct Listing {
    /// Per-instruction machine ranges, in pc order.
    pub(crate) ranges: Vec<InstructionRange>,
    /// Embedded data ranges, in offset order.
    pub(crate) data: Vec<DataRange>,
    /// Image offset of the deferred slows region.
    pub(crate) slows_start: usize,
    /// Image offset where the code ends.
    pub(crate) code_end: usize,
    /// Image offset where the code starts, past the owner word.
    pub(crate) code_start: usize,
    /// LIR/IR comments at machine-code offsets, sorted by offset. Empty
    /// unless `CAPY_OPT_ANNOTATE` is set.
    pub(crate) annotations: Vec<Annotation>,
}

/// Everything native that a [`CodeImage`] owns, dropped in place when the
/// collector finds the image dead.
#[allow(clippy::large_enum_variant)]
pub(crate) enum PublishedCode {
    /// The machine code of a compiled procedure.
    Unit(CodeUnit),
    /// A call inline cache stub.
    Stub(CallIcStub),
}

/// Deopt point describing where exit frame values are located, and the bytecode
/// frames to rebuild.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DeoptPoint {
    /// Bytecode pc of the innermost frame.
    pub(crate) pc: u32,
    /// Where each frame value lives at the exit, in the order of
    /// `Func::frame_values`.
    pub(crate) locs: Vec<DeoptLoc>,
    /// Where each value of `locs` is written in the rebuilt frames, in the
    /// same order.
    pub(crate) targets: Vec<DeoptDest>,
    /// The objects a `DeoptLoc::Materialize` in `locs`.
    pub(crate) remat: Vec<RematDesc>,
    /// The frames to rebuild, outermost first: each one's resume pc and the
    /// base register of the call in its caller's frame
    pub(crate) frames: Vec<DeoptFrame>,
    /// Set when the exit resumes at a multiple-value return point.
    pub(crate) returned: Option<ReturnedExit>,
    /// The check that failed. Diagnostics only (`CAPY_JIT_STATS=1`).
    pub(crate) guard: GuardKind,
    /// Set when the exit is inside an inlined body.
    pub(crate) inlined: Option<InlinedSite>,
}

/// Where an inlined exit's guard is
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct InlinedSite {
    /// The bytecode pc of the call instruction in the caller's frame.
    pub(crate) call_pc: u32,
    /// The code block containing the guard
    pub(crate) code: u32,
}

/// One bytecode frame of a deopt point.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct DeoptFrame {
    /// The bytecode pc the frame resumes at.
    pub(crate) pc: u32,
    /// The base register of the call in the caller's frame: the frame starts
    /// at the caller's `fp + 8 * base`. Zero for the outermost frame.
    pub(crate) base: u32,
}

/// How an exit into a multiple-value entry returns values.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ReturnedExit {
    /// The callee frame's base register in the rebuilt frame.
    pub(crate) base: u32,
    /// The value count when it is known at build time. `None` takes it from
    /// the thread context's `AC`, where the exit stub left it, and moves the
    /// values from the callee frame of the optimized frame (`callee_words`
    /// words above its fp) down to `r[base + 2..]`.
    pub(crate) count: Option<u32>,
    /// Words from the optimized frame's fp to the callee frame it called.
    pub(crate) callee_words: u32,
}

/// A watchpoint assumption that an optimized unit relies on: the unit folded
/// `value` as the content of `variable`, so a write to the variable
/// invalidates it. A build-to-install transfer value: the install registers
/// it, then the owner's reference box keeps both alive.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Dependency<'gc> {
    pub(crate) variable: crate::runtime::objectmodel::Value<'gc>,
    pub(crate) value: crate::runtime::objectmodel::Value<'gc>,
}

/// A class descriptor an optimized unit assumed was the class table's current
/// descriptor with the recorded layout.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ClassDependency<'gc> {
    pub(crate) class: crate::runtime::objectmodel::Value<'gc>,
    pub(crate) id: u32,
    pub(crate) stamp: u32,
}

pub(crate) struct OptInfo {
    /// The deopt point of each exit.
    pub(crate) deopt: Vec<DeoptPoint>,
    /// The deepest inlined reconstruction, in words. The prologue checks it
    /// beyond the frame.
    pub(crate) max_deopt_extent: u32,
    /// Words the prologue checks: the frame plus `max_deopt_extent`. An OSR
    /// transfer runs in the baseline frame, so the runtime checks this room
    /// before it enters.
    pub(crate) frame_words: u32,
    /// Loop headers an OSR entry can enter at, as (bytecode pc, code offset
    /// of the entry's prologue).
    pub(crate) osr: Vec<(BytecodePc, CodeOffset)>,
    /// The reference slot of each `Func::codes` entry.
    pub(crate) code_slots: Vec<u32>,
    /// The patchable address loads of constant-table slots, as the code offset
    /// of each and the reference slot its immediate takes.
    pub(crate) constant_sites: Vec<(CodeOffset, u32)>,
    pub(crate) invalid: Box<AtomicU32>,
    pub(crate) entry_jump: CodeOffset,
    pub(crate) reheal: CodeOffset,
}

/// One compiled procedure
pub(crate) struct CodeUnit {
    /// The tier that compiled it.
    pub(crate) tier: Tier,
    /// The machine code
    pub(crate) image: LoadedImage,
    /// The machine address a call enters the procedure at.
    pub(crate) entry: Address,
    /// Bytes of machine code, counted in `LIVE_CODE_BYTES`.
    pub(crate) bytes: usize,
    /// Bytecode offset -> machine address, for resumes.
    pub(crate) resume: ResumeTable,
    /// Live masks too large for a header word.
    pub(crate) masks: Vec<Box<[u64]>>,
    pub(crate) id: usize,
    /// The bytecode entry address this was compiled from.
    pub(crate) constant_pointer: Address,
    /// The install generation that published this unit. A late invalidation
    /// of an older unit must not touch a newer block state.
    pub(crate) generation: Generation,
    pub(crate) listing: Listing,
    /// Tier 2 metadata, `None` for baseline units.
    pub(crate) opt: Option<OptInfo>,
}

impl CodeUnit {
    /// A unit for the loaded `image`; counts it and its code in the live
    /// totals until it is dropped.
    pub(crate) fn new(
        tier: Tier,
        image: LoadedImage,
        entry: Address,
        resume: ResumeTable,
        masks: Vec<Box<[u64]>>,
        id: usize,
        constant_pointer: Address,
        listing: Listing,
    ) -> Self {
        let bytes = image.code_size();
        let partition = match tier {
            Tier::Baseline => Partition::Baseline,
            Tier::Lbbv | Tier::Optimizing | Tier::Trace => Partition::Optimizing,
        };
        cache::charge(partition, bytes);
        LIVE_UNITS.fetch_add(1, Relaxed);
        Self {
            tier,
            image,
            entry,
            bytes,
            resume,
            masks,
            id,
            constant_pointer,
            generation: 0,
            listing,
            opt: None,
        }
    }

    /// The unit that `code_block` runs, if it has compiled code. The block
    /// keeps the owning image, and so the unit, alive.
    pub(crate) fn of<'gc>(code_block: &CodeBlock<'gc>) -> Option<&'gc CodeUnit> {
        code_block.machine.load(Acquire)?.as_gc_ref().unit()
    }

    /// The unit that the image `owner` owns.
    pub(crate) fn of_image<'gc>(owner: Gc<'gc, CodeImage<'gc>>) -> Option<&'gc CodeUnit> {
        owner.as_gc_ref().unit()
    }

    /// This unit's lazy cells and the code materialized for them.
    pub(crate) fn cells(&self) -> &super::lazy::CellArena {
        self.resume.cells()
    }
}

impl Drop for CodeUnit {
    fn drop(&mut self) {
        log::info!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "release #{} ({} bytes, {} stubs)",
            self.id,
            self.bytes,
            self.cells().extra_count()
        );

        crate::runtime::profile::maps::remove_jit(self.image.base().as_usize() as u64);
        for extra in self.cells().take_extra() {
            cache::release(Partition::Lazy, extra.bytes);
        }
        let partition = match self.tier {
            Tier::Baseline => Partition::Baseline,
            Tier::Lbbv | Tier::Optimizing | Tier::Trace => Partition::Optimizing,
        };
        cache::release(partition, self.bytes);
        LIVE_UNITS.fetch_sub(1, Relaxed);
    }
}

/// The owner slot of the compiled code that the return address `ra` belongs
/// to, for the stack walker: through `toplink` to the start of the image,
/// whose first word is the slot's address.
///
/// # Safety
///
/// `ra` is a return address in compiled code.
pub(crate) unsafe fn owner_slot<'gc>(ra: Ip) -> Slot<'gc> {
    // SAFETY: `ra`'s toplink and image layout follow the runtime ABI by
    // the caller's contract, so the chunk start holds the owner slot.
    unsafe {
        let toplink_at = ra.offset(sabi::COMPACT_RETURN_ADDRESS_TOPLINK_DISP);
        let chunk = toplink_at.sub(sabi::entry_offset(ra));
        *(chunk as *const Slot)
    }
}
