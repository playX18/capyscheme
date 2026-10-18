//! Listings and dumps of compiled code: bytecode interleaved with native
//! disassembly.
//!
//! [`listing_for`] renders one procedure for `%jit-disassembly`,
//! [`maybe_dump`] writes its listing and ELF to `CAPY_JIT_DUMP_DIR`. Images
//! embed data, the chunk header, return-point headers, switch tables,
//! whose ranges the compiler retains
//! ([`DataRange`](super::registry::DataRange)), the listing prints those
//! as words and disassembles only the code between them, so embedded data
//! never decodes as instructions. Bytes capstone still refuses print as
//! raw bytes instead of failing the listing.

use capstone::Capstone;
use std::fmt::Write;
use std::path::Path;

use capstone::arch::BuildsCapstone;

use super::registry::{CompiledSnapshot, snapshot_of};
use crate::bytecode::def::Instruction;
use crate::compiler::bytecode::disasm::disassemble_insn;
use crate::heap::Gc;
use crate::runtime::gdb_jit::{JitSymbol, build_symfile};
use crate::runtime::value::{CodeBlock, CodeBlockKind};
use crate::utils::flags;

/// The listing of `code_block`'s compiled code, or `None` when it has
/// none: bytecode interleaved with native disassembly, with embedded
/// data as words.
pub fn listing_for(code_block: Gc<'_, CodeBlock<'_>>) -> Option<String> {
    if code_block.kind != CodeBlockKind::Bytecode {
        return None;
    }
    let code = code_block.code();
    let snapshot = snapshot_of(&code_block)?;
    Some(render(&snapshot, code, &code_block.buffer.pool()))
}

/// Write `code_block`'s listing and ELF to `CAPY_JIT_DUMP_DIR`, if set.
/// Failures warn and leave compilation alone: dumps are debugging aids.
pub(crate) fn maybe_dump(code_block: Gc<'_, CodeBlock<'_>>) {
    let Some(dir) = flags::jit_dump_dir() else {
        return;
    };
    if code_block.kind != CodeBlockKind::Bytecode {
        return;
    }
    let code = code_block.code();
    let Some(snapshot) = snapshot_of(&code_block) else {
        return;
    };
    if let Err(err) = dump_to_dir(dir, &snapshot, code, &code_block.buffer.pool()) {
        log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " cannot write dump to {}: {err}", dir.display());
    }
}

/// Write the listing (`{stem}.txt`) and an objdump-able ELF
/// (`{stem}.elf`) for `snapshot` to `dir`, creating it.
pub(crate) fn dump_to_dir(
    dir: &Path,
    snapshot: &CompiledSnapshot,
    bytecode: &[u8],
    pool: &[u32],
) -> std::io::Result<()> {
    std::fs::create_dir_all(dir)?;
    let stem = format!(
        "jit-{:04}-{:x}",
        snapshot.id,
        snapshot.constant_pointer.as_usize()
    );
    std::fs::write(
        dir.join(format!("{stem}.txt")),
        render(snapshot, bytecode, pool),
    )?;
    let name = format!("jit:{:#x}", snapshot.constant_pointer.as_usize());
    let entry = snapshot.ranges.first().map_or(0, |range| range.offset);
    let symbols = [JitSymbol {
        name: &name,
        address: snapshot.base.as_usize() + entry,
        size: snapshot.bytes.len() - entry,
    }];
    let elf = build_symfile(snapshot.base.as_usize(), &snapshot.bytes, &symbols, None);
    std::fs::write(dir.join(format!("{stem}.elf")), elf)?;

    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "#{}: dumped to {}/{stem}.{{elf,txt}}",
        snapshot.id,
        dir.display()
    );

    Ok(())
}

fn render(snapshot: &CompiledSnapshot, bytecode: &[u8], pool: &[u32]) -> String {
    let mut out = String::new();
    let entry = snapshot.base.as_usize() + snapshot.ranges.first().map_or(0, |range| range.offset);
    let instructions = if snapshot.ranges.len() == 1 {
        "instruction"
    } else {
        "instructions"
    };
    let _ = writeln!(
        out,
        "jit #{} jit:{:#x}: {} bytecode {instructions}, {} machine bytes at {:#x} (entry {entry:#x})",
        snapshot.id,
        snapshot.constant_pointer.as_usize(),
        snapshot.ranges.len(),
        snapshot.bytes.len(),
        snapshot.base.as_usize(),
    );
    // The chunk header: the owner slot's address.
    if let Some(word) = snapshot.bytes.get(0..8) {
        let slot = u64::from_le_bytes(word.try_into().expect("eight bytes"));
        let _ = writeln!(
            out,
            "header {:#x}: owner slot at {slot:#x}",
            snapshot.base.as_usize()
        );
    }
    let capstone = native_disassembler();
    for range in &snapshot.ranges {
        if let Some((ins, width, _)) = Instruction::decode_slice(bytecode, range.pc) {
            let _ = writeln!(out, "  {}", disassemble_insn(&ins, width, range.pc, pool));
        } else {
            let _ = writeln!(out, "  bc {}: <undecodable>", range.pc);
        }
        render_span(
            &mut out,
            &capstone,
            snapshot,
            range.offset,
            range.offset + range.len,
        );
    }
    if snapshot.slows_start < snapshot.code_end {
        let _ = writeln!(
            out,
            "  slows ({} bytes of deferred blocks and tables):",
            snapshot.code_end - snapshot.slows_start
        );
        render_span(
            &mut out,
            &capstone,
            snapshot,
            snapshot.slows_start,
            snapshot.code_end,
        );
    }
    out
}

/// Render the image bytes in `[start, end)`: embedded data as words,
/// the code between them disassembled. Bounds clamp to the image, so
/// every slice below is safe.
fn render_span(
    out: &mut String,
    capstone: &Capstone,
    snapshot: &CompiledSnapshot,
    start: usize,
    end: usize,
) {
    let end = end.min(snapshot.bytes.len());
    let start = start.min(end);
    let overlapping: Vec<(usize, usize)> = snapshot
        .data
        .iter()
        .map(|data| (data.offset.max(start), (data.offset + data.len).min(end)))
        .filter(|(from, to)| from < to)
        .collect();
    let mut at = start;
    for (from, to) in overlapping {
        let from = from.max(at);
        if to <= from {
            continue;
        }
        if from > at {
            render_code(out, capstone, snapshot, at, from);
        }
        render_words(out, snapshot, from, to);
        at = to;
    }
    if at < end {
        render_code(out, capstone, snapshot, at, end);
    }
}

/// Disassemble the code in `[start, end)`. Bytes capstone refuses, or
/// past where it stops, print as raw bytes, so the listing degrades
/// instead of failing.
fn render_code(
    out: &mut String,
    capstone: &Capstone,
    snapshot: &CompiledSnapshot,
    start: usize,
    end: usize,
) {
    let mut decoded = start;
    if let Ok(instructions) = capstone.disasm_all(
        &snapshot.bytes[start..end],
        (snapshot.base.as_usize() + start) as u64,
    ) {
        for instruction in instructions.iter() {
            let at = instruction.address() as usize - snapshot.base.as_usize();
            if at > decoded {
                render_bytes(out, snapshot, decoded, at);
            }
            decoded = decoded.max(at + instruction.len());
            let _ = match instruction.op_str() {
                Some(ops) if !ops.is_empty() => writeln!(
                    out,
                    "    {:#x}: {} {}",
                    instruction.address(),
                    instruction.mnemonic().unwrap_or("?"),
                    ops
                ),
                _ => writeln!(
                    out,
                    "    {:#x}: {}",
                    instruction.address(),
                    instruction.mnemonic().unwrap_or("?")
                ),
            };
        }
    }
    if decoded < end {
        render_bytes(out, snapshot, decoded, end);
    }
}

/// Print `[start, end)` as eight-byte words, with a byte tail when the
/// length is not a multiple of eight.
fn render_words(out: &mut String, snapshot: &CompiledSnapshot, mut start: usize, end: usize) {
    while start + 8 <= end {
        let word = u64::from_le_bytes(snapshot.bytes[start..start + 8].try_into().expect("slice"));
        let _ = writeln!(
            out,
            "    {:#x}: .quad {word:#x}",
            snapshot.base.as_usize() + start
        );
        start += 8;
    }
    if start < end {
        render_bytes(out, snapshot, start, end);
    }
}

/// Print `[start, end)` as raw bytes, eight per line.
fn render_bytes(out: &mut String, snapshot: &CompiledSnapshot, start: usize, end: usize) {
    for (index, line) in snapshot.bytes[start..end].chunks(8).enumerate() {
        let bytes: Vec<String> = line.iter().map(|byte| format!("{byte:#04x}")).collect();
        let _ = writeln!(
            out,
            "    {:#x}: .byte {}",
            snapshot.base.as_usize() + start + index * 8,
            bytes.join(", ")
        );
    }
}

fn native_disassembler() -> Capstone {
    #[cfg(target_arch = "x86_64")]
    let builder = Capstone::new()
        .x86()
        .mode(capstone::arch::x86::ArchMode::Mode64);
    #[cfg(target_arch = "aarch64")]
    let builder = Capstone::new()
        .arm64()
        .mode(capstone::arch::arm64::ArchMode::Arm);
    builder
        .build()
        .expect("capstone supports the host architecture")
}
