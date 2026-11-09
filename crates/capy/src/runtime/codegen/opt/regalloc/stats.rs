//! Compile-time statistics for the register allocators.

use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use super::weights::Weights;
use super::{Allocation, Loc};
use crate::runtime::codegen::jit::config::RegAllocKind;

struct Row {
    compiles: AtomicU64,
    failures: AtomicU64,
    spill_slots: AtomicU64,
    moves: AtomicU64,
    /// The same moves split by what they touch: register-to-register, spill,
    /// reload and slot-to-slot.
    moves_reg_reg: AtomicU64,
    moves_spill: AtomicU64,
    moves_reload: AtomicU64,
    moves_slot_slot: AtomicU64,
    /// Moves weighted by the frequency of the block they run in.
    moves_weighted: AtomicU64,
    /// Accesses that read or write a slot directly, weighted by block
    /// frequency: the memory operands that no move accounts for.
    slot_operands: AtomicU64,
    pieces_reg: AtomicU64,
    pieces_slot: AtomicU64,
    weight: AtomicU64,
    micros: AtomicU64,
}

const fn row() -> Row {
    Row {
        compiles: AtomicU64::new(0),
        failures: AtomicU64::new(0),
        spill_slots: AtomicU64::new(0),
        moves: AtomicU64::new(0),
        moves_reg_reg: AtomicU64::new(0),
        moves_spill: AtomicU64::new(0),
        moves_reload: AtomicU64::new(0),
        moves_slot_slot: AtomicU64::new(0),
        moves_weighted: AtomicU64::new(0),
        slot_operands: AtomicU64::new(0),
        pieces_reg: AtomicU64::new(0),
        pieces_slot: AtomicU64::new(0),
        weight: AtomicU64::new(0),
        micros: AtomicU64::new(0),
    }
}

static ROWS: [Row; 4] = [row(), row(), row(), row()];

fn index(kind: RegAllocKind) -> usize {
    match kind {
        RegAllocKind::LinearScan => 0,
        RegAllocKind::Greedy => 1,
    }
}

/// Whether statistics are being collected.
pub(crate) fn enabled() -> bool {
    crate::utils::flags::jit_stats()
}

/// Records one completed allocation.
pub(crate) fn record(kind: RegAllocKind, alloc: &Allocation, weights: &Weights, micros: u64) {
    let row = &ROWS[index(kind)];
    let mut reg = 0u64;
    let mut slot = 0u64;
    let mut weight = 0u64;
    for v in alloc.pieces.keys() {
        for piece in &alloc.pieces[v] {
            match piece.loc {
                Loc::Reg(_) => reg += 1,
                Loc::Slot(_) => slot += 1,
            }
            weight += weights.spill_weight(v, &piece.ranges);
        }
    }
    let mut slot_operands = 0u64;
    for v in alloc.pieces.keys() {
        if !alloc.pieces[v]
            .iter()
            .any(|p| matches!(p.loc, Loc::Slot(_)))
        {
            continue;
        }
        for piece in &alloc.pieces[v] {
            if !matches!(piece.loc, Loc::Slot(_)) {
                continue;
            }
            for &(lo, hi) in &piece.ranges {
                slot_operands += weights
                    .free_accesses_in(v, lo, hi)
                    .iter()
                    .map(|&(_, w)| w)
                    .sum::<u64>();
            }
        }
    }
    let moves: u64 = alloc.moves.iter().map(|m| m.len() as u64).sum();
    let mut reg_reg = 0u64;
    let mut spill = 0u64;
    let mut reload = 0u64;
    let mut slot_slot = 0u64;
    let mut weighted = 0u64;
    for (g, group) in alloc.moves.iter().enumerate() {
        weighted += group.len() as u64 * weights.boundary_cost(super::pos::pre(g as u32));
        for m in group {
            match (m.src, m.dst) {
                (Loc::Reg(_), Loc::Reg(_)) => reg_reg += 1,
                (Loc::Reg(_), Loc::Slot(_)) => spill += 1,
                (Loc::Slot(_), Loc::Reg(_)) => reload += 1,
                (Loc::Slot(_), Loc::Slot(_)) => slot_slot += 1,
            }
        }
    }
    if std::env::var_os("CAPY_STATS_PER").is_some() {
        eprintln!("PER {} {weighted} {moves} {slot_operands}", kind.name());
    }
    row.slot_operands.fetch_add(slot_operands, Relaxed);
    row.compiles.fetch_add(1, Relaxed);
    row.spill_slots
        .fetch_add(u64::from(alloc.spill_slots), Relaxed);
    row.moves.fetch_add(moves, Relaxed);
    row.moves_reg_reg.fetch_add(reg_reg, Relaxed);
    row.moves_spill.fetch_add(spill, Relaxed);
    row.moves_reload.fetch_add(reload, Relaxed);
    row.moves_slot_slot.fetch_add(slot_slot, Relaxed);
    row.moves_weighted.fetch_add(weighted, Relaxed);
    row.pieces_reg.fetch_add(reg, Relaxed);
    row.pieces_slot.fetch_add(slot, Relaxed);
    row.weight.fetch_add(weight, Relaxed);
    row.micros.fetch_add(micros, Relaxed);
}

/// Records an allocation that failed to validate.
pub(crate) fn record_failure(kind: RegAllocKind) {
    ROWS[index(kind)].failures.fetch_add(1, Relaxed);
}

/// The `CAPY_JIT_STATS` line, or an empty string when nothing ran.
pub(crate) fn report() -> String {
    let mut out = String::new();
    for kind in [RegAllocKind::LinearScan, RegAllocKind::Greedy] {
        let row = &ROWS[index(kind)];
        let compiles = row.compiles.load(Relaxed);
        if compiles == 0 {
            continue;
        }
        if !out.is_empty() {
            out.push('\n');
        }
        let failures = row.failures.load(Relaxed);
        out.push_str(&format!(
            "regalloc {}: {compiles} compiles, {} spill slots, {} moves \
             ({} reg-reg, {} spill, {} reload, {} slot-slot), \
             {} reg pieces, {} slot pieces, {} weight, {} weighted moves, {} weighted slot operands, {} us{}",
            kind.name(),
            row.spill_slots.load(Relaxed),
            row.moves.load(Relaxed),
            row.moves_reg_reg.load(Relaxed),
            row.moves_spill.load(Relaxed),
            row.moves_reload.load(Relaxed),
            row.moves_slot_slot.load(Relaxed),
            row.pieces_reg.load(Relaxed),
            row.pieces_slot.load(Relaxed),
            row.weight.load(Relaxed),
            row.moves_weighted.load(Relaxed),
            row.slot_operands.load(Relaxed),
            row.micros.load(Relaxed),
            if failures == 0 {
                String::new()
            } else {
                format!(", {failures} invalid")
            },
        ));
    }
    out
}
