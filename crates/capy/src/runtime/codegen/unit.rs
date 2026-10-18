//! [`CodeUnit`]: one published piece of compiled code.

use std::sync::atomic::Ordering::Relaxed;

use super::image::LoadedImage;
use super::jit::registry::{DataRange, InstructionRange, LIVE_CODE_BYTES, LIVE_UNITS, ResumeTable};
use super::stubs::CallIcStub;
use crate::heap::Gc;
use crate::runtime::interp::stack::{Ip, Slot};
use crate::runtime::sabi;
use crate::runtime::value::{CodeBlock, CodeImage};
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
    /// Every tier, cheapest first.
    pub(crate) const ALL: [Tier; 4] = [Tier::Baseline, Tier::Lbbv, Tier::Optimizing, Tier::Trace];

    pub(crate) fn name(self) -> &'static str {
        match self {
            Tier::Baseline => "baseline",
            Tier::Lbbv => "lbbv",
            Tier::Optimizing => "optimizing",
            Tier::Trace => "trace",
        }
    }

    pub(crate) fn from_name(name: &str) -> Option<Tier> {
        Tier::ALL
            .into_iter()
            .find(|tier| tier.name().eq_ignore_ascii_case(name))
    }

    /// Whether a compiler for this tier exists in this build.
    pub(crate) fn implemented(self) -> bool {
        matches!(self, Tier::Baseline)
    }
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
}

/// Everything native that a [`CodeImage`] owns, dropped in place when the
/// collector finds the image dead.
#[allow(clippy::large_enum_variant)]
pub(crate) enum Native {
    /// The machine code of a compiled procedure.
    Unit(CodeUnit),
    /// A call inline cache stub.
    Stub(CallIcStub),
}

/// One compiled procedure
pub(crate) struct CodeUnit {
    /// The tier that compiled it.
    pub(crate) tier: Tier,
    /// The machine code. Dropping it returns the span.
    pub(crate) image: LoadedImage,
    /// The machine address a call enters the procedure at.
    pub(crate) entry: Address,
    /// Bytes of machine code, counted in `LIVE_CODE_BYTES`.
    pub(crate) bytes: usize,
    /// Bytecode offset -> machine address, for resumes. Owns the unit's lazy
    /// cells and the images materialized for them.
    pub(crate) resume: ResumeTable,
    /// Live masks too large for a header word.
    pub(crate) masks: Vec<Box<[u64]>>,
    /// Numbers log lines and dump files.
    pub(crate) id: usize,
    /// The bytecode entry address this was compiled from.
    pub(crate) constant_pointer: Address,
    pub(crate) listing: Listing,
}

// The image is written once before publication and only read afterwards.
unsafe impl Send for CodeUnit {}
unsafe impl Sync for CodeUnit {}

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
        LIVE_CODE_BYTES.fetch_add(bytes, Relaxed);
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
            listing,
        }
    }

    /// The unit that `code_block` runs, if it has compiled code. The block
    /// keeps the owning image, and so the unit, alive.
    pub(crate) fn of<'gc>(code_block: &CodeBlock<'gc>) -> Option<&'gc CodeUnit> {
        code_block.machine.get()?.as_gc_ref().unit()
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
            LIVE_CODE_BYTES.fetch_sub(extra.bytes, Relaxed);
        }
        LIVE_CODE_BYTES.fetch_sub(self.bytes, Relaxed);
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
