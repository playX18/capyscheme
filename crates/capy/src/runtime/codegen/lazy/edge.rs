//! Lazy edges: how generated code reaches a [`Lazy`](super::Lazy).
//!
//! An edge is a label that guard failures (and conditional branches) jump
//! to. Behind it sits the lazy's cell, and what the edge does with the cell
//! depends on its [`EdgeKind`].
//!
//! An [`Indirect`](EdgeKind::Indirect) edge loads the cell's address and
//! jumps through the cell's [`target`](super::CellHeader::target) word, which
//! starts at the edge's own hit block and is retargeted at generated code.
//! Repatching rewrites the data word, never code, so any number of threads
//! can be inside the edge while it changes.
//!
//! A [`Direct`](EdgeKind::Direct) edge is, at its label, one patch-aligned
//! `jmp rel32` / `b imm26` to a local *thunk*, and the thunk is an indirect
//! edge. It is resolved once: when the cell gets its code, the runtime writes
//! `cell.target` first and then, if the code is reachable
//! ([`direct_patch_policy`]), rewrites the jump in place with one atomic
//! 4-byte store so later executions skip the thunk. A thread already inside
//! the thunk (or one that read the old jump) lands at `cell.target`, which is
//! right either way, and a target out of the jump's range just keeps going
//! through the thunk.

use mmtk::util::Address;
use onlineasm::asmkit::Span;
use onlineasm::{Label, PatchHandle, VReg, onlineasm};

use super::super::addr::CodeOffset;
use super::super::masm::{Masm, RuntimeExtern};
use super::super::import::ImportKey;
use super::super::template::{SiteOperand, emit_site_slow_call, tmp};
use super::{CellHeader, CellState};
use crate::runtime::code_memory::code_memory;
use crate::runtime::interp::stack::Ip;
use crate::runtime::sabi::regs;

/// How an edge reaches its cell's target.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub(crate) enum EdgeKind {
    /// `move SITE_TMP, cell; jmp [SITE_TMP]`: retargeted any number of
    /// times (arithmetic sites, inline caches, side exits).
    Indirect = 0,
    /// A patch-aligned direct jump to a local thunk, resolved once: the jump is rewritten to the
    /// new code when it is reachable.
    Direct = 1,
}

pub(crate) const SITE_TMP: VReg = tmp::T3;

pub(crate) fn emit_indirect_edge(masm: &mut Masm, cell: usize) {
    // The jump reads the cell's first word.
    masm.load_import(SITE_TMP, ImportKey::Cell(cell as u32));
    let slot = masm.assembler().mem(SITE_TMP, 0);
    onlineasm!(*masm.assembler(); jmp {slot};);
}

/// An indirect edge to the unit's arena cell `cell`.
pub(crate) fn emit_indirect_edge_to(masm: &mut Masm, cell: usize) {
    emit_indirect_edge(masm, cell);
}

/// The direct jump of a [`Direct`](EdgeKind::Direct) edge: patch-aligned, to
/// the edge's `thunk`. Returns the handle that locates the word to rewrite in
/// the finished image ([`UnloadedStub::jump_offset`](super::builder::UnloadedStub::jump_offset)).
pub(crate) fn emit_direct_jump(masm: &mut Masm, thunk: Label) -> PatchHandle {
    masm.align_patchable_jump();
    onlineasm!(*masm.assembler(); #[patch = jump] jmp {thunk};);
    jump
}

/// The span of the image holding a direct edge's jump, kept in its cell.
pub(crate) struct DirectSpan(pub(crate) Span);

/// Where the direct jump of an edge lives: its image's span and the offset
/// of the word to rewrite.
#[derive(Clone, Copy)]
pub(crate) struct DirectSite {
    pub(crate) span: Span,
    pub(crate) at: CodeOffset,
}

/// What to do with a direct jump when its edge gets code.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum DirectPatch {
    /// Store this word at the jump: it now goes straight to the code.
    Patch(u32),
    /// Leave the jump alone: execution keeps going through the thunk, which
    /// reads `cell.target` and is always correct, only slower.
    Thunk(&'static str),
}

/// The one place that decides whether a resolved [`Direct`](EdgeKind::Direct)
/// edge's jump is rewritten to go straight to its new code, or stays on its
/// thunk. Self-contained on purpose: change the policy here and nothing else
/// moves.
///
/// - `state`: the state the cell reaches. Only terminal code
///   ([`Full`](CellState::Full)) is patched in: a specialization that can
///   still grow (`Specialized`) is retargeted through `cell.target` later,
///   which a jump already patched past the thunk would miss.
/// - `field_addr`: the machine address of the word to rewrite (the `b` on
///   AArch64, the rel32 field on x86-64). It must be 4-byte aligned, which
///   [`Masm::align_patchable_jump`] provides: the store is one aligned word.
/// - `target`: the machine address of the new code.
///
/// Reach: the displacement must fit the jump, `b imm26` (+-128 MB, 4-byte
/// aligned targets) or `jmp rel32` (+-2 GB). Allocator blocks can land
/// anywhere, so an out-of-range target is expected and not an error.
pub(crate) fn direct_patch_policy(
    state: CellState,
    field_addr: Address,
    target: Address,
) -> DirectPatch {
    if state != CellState::Full {
        return DirectPatch::Thunk("not terminal");
    }
    if !field_addr.is_aligned_to(4) {
        return DirectPatch::Thunk("jump field unaligned");
    }
    match encode_jump(field_addr, target) {
        Some(word) => DirectPatch::Patch(word),
        None => DirectPatch::Thunk("out of range"),
    }
}

#[cfg(target_arch = "aarch64")]
pub(crate) fn encode_jump(field_addr: Address, target: Address) -> Option<u32> {
    const B: u32 = 0x1400_0000;
    let delta = target.as_usize() as i128 - field_addr.as_usize() as i128;
    // imm26 counts instructions: +-2^25 of them, +-128 MB.
    if delta % 4 != 0 || !(-(1i128 << 27)..(1i128 << 27)).contains(&delta) {
        return None;
    }
    Some(B | ((delta >> 2) as u32 & 0x03ff_ffff))
}

#[cfg(not(target_arch = "aarch64"))]
pub(crate) fn encode_jump(field_addr: Address, target: Address) -> Option<u32> {
    let delta = target.as_usize() as i128 - (field_addr.as_usize() as i128 + 4);
    i32::try_from(delta).ok().map(|rel| rel as u32)
}

/// A [`Direct`](EdgeKind::Direct) cell got `entry` as `to`: `cell.target` is
/// already written, so every path is correct. Rewrite the jump in place when
/// [`direct_patch_policy`] allows it. Never fails the resolve: a jump that
/// cannot be patched just keeps its thunk.
pub(crate) fn resolve_direct(header: &CellHeader, to: CellState, entry: Address) {
    if header.kind() != EdgeKind::Direct {
        return;
    }
    let Some(site) = header.direct_site() else {
        return;
    };
    let mut span = site.span;
    let field_addr = Address::from_ptr(span.rx()) + site.at;
    match direct_patch_policy(to, field_addr, entry) {
        DirectPatch::Patch(word) => {
            let patched = code_memory().patch_live_u32(&mut span, site.at.get(), word);
            if let Err(err) = patched {
                log::warn!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "{}: direct jump stays on its thunk: {err}",
                    header.name()
                );
            } else {
                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "{}: pc={} direct jump {field_addr} -> {entry}",
                    header.name(),
                    header.pc()
                );
            }
        }
        DirectPatch::Thunk(reason) => {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "{}: pc={} direct jump stays on its thunk ({reason})",
                header.name(),
                header.pc()
            );
        }
    }
}

/// A hit block: stage `ip` and leave through [`capy_lazy_hit`](super::capy_lazy_hit),
/// passing the cell in the second C argument. Terminal: calls and jumps where
/// the hit says.
pub(crate) fn emit_hit_block(masm: &mut Masm, ip: Ip, cell: SiteOperand) {
    masm.load_ip(regs::PC, ip as usize);
    let hit = masm.declare_rust_fn(RuntimeExtern::LazyHit);
    emit_site_slow_call(masm, hit, cell);
}
