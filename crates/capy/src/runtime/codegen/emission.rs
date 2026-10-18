use std::collections::HashMap;
use std::sync::atomic::Ordering::Relaxed;

use onlineasm::{Label, PatchHandle, PatchLocation, onlineasm};

use super::addr::CodeOffset;
use super::image::{self, LoadedImage, UnloadedImage};
use super::jit::registry::{DataRange, InstructionRange};
use super::lazy::edge::{self, EdgeKind};
use super::lazy::{ErasedCell, Lazies, Lazy};
use super::masm::Masm;
use super::operation::{ERROR_REG, RESULT_REG, emit_operation_call};
use super::reloc::{Reloc, patch_imm64};
use super::template::{
    OpContinue, OpResult, OperationCall, SiteOperand, SlowPath, SlowPaths, Target, emit_jump_to,
    emit_staged_control,
};
use super::template_image::resolve_slow_path;
use crate::runtime::interp::stack::Ip;
use crate::runtime::sabi::regs;
use mmtk::util::Address;

/// A deferred slow/taken block
#[derive(Clone)]
pub(crate) enum Deferred {
    /// Stage `ip` (and `md` for calls) and leave through `path`.
    Control { path: SlowPath, ip: Ip, md: usize },
    /// Jump to the already-decoded `target`.
    Jump { target: Label },
    /// Jump to a machine address known at emission time.
    JumpAbsolute { addr: Address },
    /// A dense switch jump table: one machine address per bytecode-pc
    /// `target`, reserved here and patched at link.
    Table { targets: Vec<usize> },
    /// A lazy indirect edge: load pending cell `pending`'s address (patched
    /// at link) and jump through its `target` word, which points at the
    /// edge's hit block until code is generated.
    LazyEdge { pending: usize },
    /// The direct jump of a [`Direct`](EdgeKind::Direct) edge: patch-aligned,
    /// to the edge's `thunk`. Its location is recorded for pending cell
    /// `pending`; link binds it into the cell.
    LazyDirect { pending: usize, thunk: Label },
    /// A lazy edge's hit block: stage `ip` and leave through
    /// [`capy_lazy_hit`](super::lazy::capy_lazy_hit), passing the cell
    /// (inherited in [`SITE_TMP`](edge::SITE_TMP) from its edge) in the
    /// second C argument.
    LazyHit { ip: Ip },
    /// An operation call, its fields already resolved to frame slots and
    /// immediates: the drain has no instruction context. `Next` resumes
    /// past `ip` through [`SlowPath::Nop`], errors leave through
    /// [`SlowPath::PrimError`].
    Operation { call: OperationCall, ip: Ip },
}

/// A lazy cell declared before load: the cell, and the label of its edge's
/// hit block, which link makes its initial target.
pub(crate) struct PendingCell {
    pub(crate) cell: ErasedCell,
    pub(crate) hit: Label,
}

/// The per-compile record of everything that is not yet machine code.
#[derive(Default)]
pub(crate) struct Emission {
    /// Slow/taken blocks deferred to the end of the code, in template
    /// order. [`emit_deferred`](Self::emit_deferred) binds each label and
    /// emits its block.
    pub(crate) slows: Vec<(Label, Deferred)>,
    /// Values patched after load.
    pub(crate) relocs: Vec<Reloc>,
    /// Lazy cells declared by edges, in declaration order. Link allocates
    /// nothing more: it points each at its hit block and bakes its address
    /// into the edge ([`Reloc::Cell`]).
    pub(crate) cells: Vec<PendingCell>,
    /// Fallthrough proofs, checked at link: `(point, next)` where
    /// `point` is bound where `next` fell through.
    pub(crate) fallthrough_checks: Vec<(Label, Label)>,
    /// The patchable jumps of direct edges, by pending cell.
    direct_jumps: Vec<(usize, PatchHandle)>,
    /// Data embedded in the deferred blocks: `(start, bytes)`.
    data: Vec<(Label, usize)>,
}

/// What [`Emission::link`] needs to know about the finished masm besides
/// the relocations.
pub(crate) struct Layout<'a> {
    /// The label of each instruction, by bytecode pc.
    pub(crate) labels: &'a HashMap<usize, Label>,
    /// Instruction pcs, in code order.
    pub(crate) pcs: &'a [usize],
    /// Where the deferred blocks start, and where the code ends.
    pub(crate) slows_start: Label,
    pub(crate) end: Label,
}

/// A loaded, relocated image.
pub(crate) struct Linked {
    pub(crate) loaded: LoadedImage,
    /// Image offset of each instruction, by bytecode pc.
    pub(crate) offsets: HashMap<usize, usize>,
    pub(crate) cells: Box<[ErasedCell]>,
    pub(crate) data: Vec<DataRange>,
    pub(crate) ranges: Vec<InstructionRange>,
    pub(crate) slows_start_off: usize,
    pub(crate) end_off: usize,
    /// Image offset of the owner word, patched at publish.
    pub(crate) owner_at: usize,
}

impl Emission {
    /// Defer `block` to the end of the code, bound at `label`.
    pub(crate) fn defer(&mut self, label: Label, block: Deferred) {
        self.slows.push((label, block));
    }

    /// Record a value to patch after load.
    pub(crate) fn reloc(&mut self, reloc: Reloc) {
        self.relocs.push(reloc);
    }

    /// Emit the deferred blocks: bind each label and emit its block, in
    /// template order. Runs after the last instruction, inside the
    /// function. Tables hold data, never executed: every preceding block
    /// jumps away.
    pub(crate) fn emit_deferred(&mut self, masm: &mut Masm, paths: SlowPaths) {
        for (label, slow) in std::mem::take(&mut self.slows) {
            masm.bind(label);
            match slow {
                Deferred::Control { path, ip, md } => {
                    emit_staged_control(masm, &paths, path, ip, md)
                }
                Deferred::Jump { target } => {
                    onlineasm!(*masm.assembler(); jmp {target};);
                }
                Deferred::JumpAbsolute { addr } => emit_jump_to(masm, Target::Absolute(addr)),
                Deferred::Table { targets } => {
                    masm.reserve_words(targets.len() * 8);
                    self.data.push((label, targets.len() * 8));
                    for (index, pc) in targets.into_iter().enumerate() {
                        self.relocs.push(Reloc::TableWord {
                            table: label,
                            index,
                            pc,
                        });
                    }
                }
                Deferred::LazyEdge { pending } => edge::emit_indirect_edge(masm, self, pending),
                Deferred::LazyDirect { pending, thunk } => {
                    let jump = edge::emit_direct_jump(masm, thunk);
                    self.direct_jumps.push((pending, jump));
                }
                Deferred::LazyHit { ip } => {
                    edge::emit_hit_block(masm, ip, SiteOperand::Register(edge::SITE_TMP))
                }
                Deferred::Operation { call, ip } => {
                    // `AC` is the only SABI register live in compiled code that
                    // a call clobbers.
                    emit_operation_call(masm, &call, &[regs::AC], &mut |_, arg, _| {
                        unreachable!("the JIT resolves {arg:?} when it defers the call")
                    });
                    let error = masm.label();
                    if call.checks_error {
                        onlineasm!(*masm.assembler(); bqneq {ERROR_REG}, 0, {error};);
                    }
                    if let OpResult::FrameSlot(slot) = call.result {
                        let out = masm.assembler().mem(regs::SFP, 8 * slot);
                        onlineasm!(*masm.assembler(); storeq {out}, {RESULT_REG};);
                    }
                    match call.cont {
                        OpContinue::Join(label) => onlineasm!(*masm.assembler(); jmp {label};),
                        OpContinue::Next => emit_staged_control(masm, &paths, SlowPath::Nop, ip, 0),
                    }
                    if call.checks_error {
                        masm.bind(error);
                        emit_staged_control(masm, &paths, SlowPath::PrimError(ERROR_REG), ip, 0);
                    }
                }
            }
        }
    }

    /// Verify the finished masm, load it and resolve every relocation.
    ///
    /// Takes `memory` only after the verification, so a worker's last
    /// yieldpoint comes before the lock. Pre-publication and
    /// single-threaded: nothing executes the image yet. Returns the
    /// loaded image with its cells, as the driver publishes it.
    pub(crate) fn link(self, image: UnloadedImage, layout: &Layout<'_>) -> std::io::Result<Linked> {
        let off = |label: Label| image.finished.offset(label).expect("bound label") as usize;
        // Every fallen-through `next` must sit exactly at the next label:
        // anything emitted after it (a stale `next_may_elide` entry) drifts
        // the points apart.
        for &(point, next) in &self.fallthrough_checks {
            let (point, next) = (off(point), off(next));
            if point != next {
                return Err(std::io::Error::other(format!(
                    "jit: fallthrough at {point:#x} does not reach its next label at {next:#x}"
                )));
            }
        }
        let offsets: HashMap<usize, usize> = layout
            .labels
            .iter()
            .map(|(pc, label)| (*pc, off(*label)))
            .collect();
        // Lazy cells: each hit block's image offset (based after load, with
        // the rest).
        let hit_blocks: Vec<usize> = self.cells.iter().map(|pending| off(pending.hit)).collect();
        // The direct jumps' words, by pending cell.
        let direct_ats: Vec<(usize, usize)> = self
            .direct_jumps
            .iter()
            .map(|&(pending, jump)| match image.finished.location(jump) {
                Some(PatchLocation::Jump(jump)) => Ok((pending, jump.offset() as usize)),
                _ => Err(std::io::Error::other(
                    "jit: a direct jump is not a jump site",
                )),
            })
            .collect::<std::io::Result<_>>()?;
        // Relocations, as image offsets. Every target pc has a label
        let mut label_addrs = Vec::new();
        let mut table_words = Vec::new();
        let mut cell_loads = Vec::new();
        let mut owner_at = None;
        for reloc in &self.relocs {
            match *reloc {
                Reloc::LabelAddr { at, target } => label_addrs.push((off(at), off(target))),
                Reloc::TableWord { table, index, pc } => {
                    table_words.push((off(table) + index * 8, offsets[&pc]))
                }
                Reloc::Cell { at, pending } => cell_loads.push((off(at), pending)),
                Reloc::OwnerSlot { at } => owner_at = Some(off(at)),
            }
        }
        let owner_at = owner_at.expect("an image reserves its owner word");

        let mut data = Vec::with_capacity(self.data.len() + image.headers.len());
        for &(start, len) in &self.data {
            data.push(DataRange {
                offset: off(start),
                len,
            });
        }
        for header in &image.headers {
            let start = off(header.header);
            data.push(DataRange {
                offset: start,
                len: off(header.ra) - start,
            });
        }
        data.sort_by_key(|range| range.offset);
        // Per-instruction machine spans, in pc order, for listings: each runs
        // to the next instruction's offset, the last to the slows region.
        let slows_start_off = off(layout.slows_start);
        let end_off = off(layout.end);
        let mut ranges = Vec::with_capacity(layout.pcs.len());
        for (index, &pc) in layout.pcs.iter().enumerate() {
            let offset = offsets[&pc];
            let next = match layout.pcs.get(index + 1) {
                Some(next) => offsets[next],
                None => slows_start_off,
            };
            ranges.push(InstructionRange {
                pc,
                offset,
                len: next - offset,
            });
        }

        let mut loaded = image::load(image, |name| resolve_slow_path(name))?;
        let base = loaded.base();
        for (at, target) in label_addrs {
            patch_imm64(&mut loaded, at, (base + target).as_usize() as u64)?;
        }
        for (at, target) in table_words {
            loaded.patch(at, &((base + target).as_usize() as u64).to_ne_bytes())?;
        }
        // Lazy cells: point each at its hit block, then bake the cell
        // addresses into the edges.
        let cells: Box<[ErasedCell]> = self
            .cells
            .into_iter()
            .zip(hit_blocks)
            .map(|(pending, hit)| {
                pending.cell.header().target.store(base + hit, Relaxed);
                pending.cell
            })
            .collect();
        for (at, pending) in cell_loads {
            patch_imm64(&mut loaded, at, cells[pending].addr().as_usize() as u64)?;
        }
        for (pending, at) in direct_ats {
            cells[pending]
                .header()
                .bind_direct(loaded.span(), CodeOffset(at as u32));
        }
        Ok(Linked {
            loaded,
            offsets,
            cells,
            data,
            ranges,
            slows_start_off,
            end_off,
            owner_at,
        })
    }
}

impl Lazies for Emission {
    fn edge_at<T: Lazy>(&mut self, masm: &mut Masm, label: Label, kind: EdgeKind, value: T) {
        let ip = value.ip();
        let hit = masm.label();
        let pending = self.cells.len();
        self.cells.push(PendingCell {
            cell: ErasedCell::new(kind, value),
            hit,
        });
        match kind {
            EdgeKind::Indirect => self.defer(label, Deferred::LazyEdge { pending }),
            EdgeKind::Direct => {
                // `label` is the patchable jump, and the thunk behind it an
                // indirect edge.
                let thunk = masm.label();
                self.defer(label, Deferred::LazyDirect { pending, thunk });
                self.defer(thunk, Deferred::LazyEdge { pending });
            }
        }
        self.defer(hit, Deferred::LazyHit { ip });
    }
}
