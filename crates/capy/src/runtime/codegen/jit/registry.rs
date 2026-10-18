//! Resume tables and the live-code totals.
//!
//! A slow path that resumes its frame re-enters compiled code through
//! [`resume`], which is lock-free: the code block reaches its unit's
//! immutable [`ResumeTable`] through its owner, set before the entry is
//! published with `Release`. The unit, and with it the table, lives as long
//! as its owner
//! ([`CodeImage`](crate::runtime::value::CodeImage)) is reachable, and every
//! live code block with compiled code reaches its owner through
//! `CodeBlock::machine`, so no resume observes the release.

use std::sync::atomic::Ordering::{Acquire, Relaxed};

use super::super::addr::{BytecodePc, CodeOffset};
use super::super::lazy::{CellArena, ErasedCell};
use super::super::unit::{CodeUnit, Listing};
use super::config::{RESUME_HIT, RESUME_MISS, RESUME_UNCOMPILED};
use crate::heap::Gc;
use crate::runtime::interp::stack::Ip;
use crate::runtime::value::CodeBlock;
use crate::utils::flags;
use mmtk::util::Address;

/// One bytecode instruction's machine code: its pc and its byte range in
/// the image, for listings. Spans run to the next instruction's offset,
/// so a return-point header reads as part of the preceding span.
#[derive(Clone, Copy, Debug)]
pub(crate) struct InstructionRange {
    pub(crate) pc: usize,
    pub(crate) offset: usize,
    pub(crate) len: usize,
}

/// Data embedded in a compiled image: a return-point header or a dense
/// switch table. The listing prints these as words instead of decoding
/// them as instructions.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DataRange {
    pub(crate) offset: usize,
    pub(crate) len: usize,
}

/// A compiled procedure's bytes and layout, copied out for rendering
/// without holding the registry lock.
#[derive(Clone, Debug)]
pub(crate) struct CompiledSnapshot {
    pub(crate) id: usize,
    pub(crate) constant_pointer: Address,
    pub(crate) base: Address,
    pub(crate) bytes: Vec<u8>,
    pub(crate) ranges: Vec<InstructionRange>,
    pub(crate) data: Vec<DataRange>,
    pub(crate) slows_start: usize,
    pub(crate) code_end: usize,
}

/// Bytecode offsets without an instruction (mid-instruction bytes) map
/// to this: a resume there misses. A real offset can only equal it in a
/// 4 GiB image, where the miss safely falls back to the interpreter.
const RESUME_GAP: u32 = u32::MAX;

/// Immutable, dense `pc -> machine address` table of one compiled
/// procedure, plus the procedure's lazy cells. The unit owns it, and lives
/// in a heap owner that never moves, so every cell's `origin` can hold its
/// address ([`bind`](Self::bind)).
pub(crate) struct ResumeTable {
    /// The lowest machine address of the image -- offsets are relative to it.
    base: Address,
    /// One machine offset per bytecode byte from 0 through the highest
    /// instruction offset ([`RESUME_GAP`] where no instruction starts).
    offsets: Box<[u32]>,
    /// The procedure's lazy cells and the code materialized for them. A
    /// cell finds its unit through its `origin`, so a hit takes no lock.
    cells: CellArena,
}

impl ResumeTable {
    /// Build the table from `pc -> address` rows and the unit's linked
    /// cells. All addresses belong to one image, so every offset from the
    /// lowest fits a `u32`. The cells do not know the table yet: call
    /// [`bind`](Self::bind) once it sits at its final address.
    pub(crate) fn new(rows: Vec<(BytecodePc, Address)>, cells: Box<[ErasedCell]>) -> Self {
        let base = rows
            .iter()
            .map(|(_, addr)| *addr)
            .min()
            .unwrap_or(Address::ZERO);
        let len = rows
            .iter()
            .map(|(pc, _)| pc.get())
            .max()
            .map_or(0, |pc| pc + 1);
        let mut offsets = vec![RESUME_GAP; len].into_boxed_slice();
        for (pc, addr) in rows {
            let offset = CodeOffset::between(base, addr).expect("image exceeds u32");
            offsets[pc.get()] = offset.0;
        }
        Self {
            base,
            offsets,
            cells: CellArena::new(cells),
        }
    }

    /// Point the unit's cells at this table. Once, after the unit reached
    /// the heap owner that keeps the table in place, and before the code is
    /// published.
    pub(crate) fn bind(&self) {
        self.cells.bind(self);
    }

    /// The machine address of bytecode `pc`, if an instruction starts
    /// there.
    pub(crate) fn lookup(&self, pc: BytecodePc) -> Option<Address> {
        match *self.offsets.get(pc.get())? {
            RESUME_GAP => None,
            offset => Some(self.base + CodeOffset(offset)),
        }
    }

    /// This table's lazy cells.
    pub(crate) fn cells(&self) -> &CellArena {
        &self.cells
    }
}

pub(crate) fn origin_exit_addr(origin: &ResumeTable, pc: usize) -> Address {
    let pc = BytecodePc::new(pc).expect("bytecode fits in u32");
    origin.lookup(pc).expect("landing pc has origin code")
}

pub(crate) static NEXT_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);

pub(crate) static LIVE_CODE_BYTES: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

pub(crate) static LIVE_UNITS: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

/// Number of live compiled procedures
pub fn compiled_count() -> usize {
    super::super::reclaim::drain();
    LIVE_UNITS.load(Relaxed)
}

/// A copy of the compiled code of `code_block`, for listings and dumps.
pub(crate) fn snapshot_of(code_block: &CodeBlock<'_>) -> Option<CompiledSnapshot> {
    let unit = CodeUnit::of(code_block)?;
    let Listing {
        ranges,
        data,
        slows_start,
        code_end,
    } = &unit.listing;
    // SAFETY: the unit owns the image, and executable memory stays
    // readable while it is alive.
    let bytes =
        unsafe { std::slice::from_raw_parts(unit.image.base().to_ptr(), unit.image.code_size()) }
            .to_vec();
    Some(CompiledSnapshot {
        id: unit.id,
        constant_pointer: unit.constant_pointer,
        base: unit.image.base(),
        bytes,
        ranges: ranges.clone(),
        data: data.clone(),
        slows_start: *slows_start,
        code_end: *code_end,
    })
}

/// The machine address of bytecode `ip` in the compiled code of
/// `code_block`, if it has any: where a slow path that resumes the frame at
/// `ip` continues.
pub fn resume<'gc>(code_block: Gc<'gc, CodeBlock<'gc>>, ip: Ip) -> Option<*const u8> {
    // Acquire pairs with the Release in `publish`, after which the owner is set.
    let unit = code_block
        .jit_entry
        .entry(Acquire)
        .and_then(|_| CodeUnit::of(&code_block));
    let Some(unit) = unit else {
        if flags::jit_stats() {
            RESUME_UNCOMPILED.fetch_add(1, Relaxed);
        }
        return None;
    };
    let pc = (ip as usize).wrapping_sub(code_block.entrypoint.as_usize());
    let pc32 = BytecodePc::new(pc)?;
    let addr = unit.resume.lookup(pc32).map(|addr| addr.to_ptr::<u8>());
    if flags::jit_stats() {
        (if addr.is_some() {
            &RESUME_HIT
        } else {
            &RESUME_MISS
        })
        .fetch_add(1, Relaxed);
    }

    log::trace!(
        target: crate::heap::logging::JIT_RESUME_LOG_TARGET,
        "entry={:?} pc={pc} -> {addr:?}",
        code_block.entrypoint
    );
    addr
}
