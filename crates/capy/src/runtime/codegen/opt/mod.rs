//! The Tier 2 optimizing compiler.
//!
//! The pipeline is build (bytecode to SSA), lower (SSA to LIR), allocate
//! (linear scan), and emit, then the unit's entry and
//! re-heal stub are added and the image is linked.
use std::collections::HashMap;
use std::io;
use std::sync::atomic::AtomicU32;

use super::addr::{BytecodePc, CodeOffset};
use super::emission::{Emission, Layout, Linked};
use super::jit::compile::{BuiltUnit, CompileRequest, TierCompiler};
use super::masm::Masm;
use super::template::tmp;
use super::unit::{Annotation, CodeUnit, DeoptPoint, InlinedSite, OptInfo, ReturnedExit, Tier};
use crate::bytecode::def::{self, Instruction};
use crate::runtime::objectmodel::CodeBlock;
use crate::runtime::sabi::{self, regs};
use crate::utils::flags;
use cranelift_entity::EntityRef;
use onlineasm::{PatchLocation, onlineasm};

pub(crate) mod build;
pub(crate) mod constants;
pub(crate) mod deopt;
pub(crate) mod emit;
pub(crate) mod glue;
pub mod ir;
pub(crate) mod lir;
pub(crate) mod passes;
pub(crate) mod regalloc;

pub(crate) fn file_safe(label: &str) -> String {
    label
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_') {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// The Tier 2 compiler.
pub(crate) struct OptimizingCompiler;

impl TierCompiler for OptimizingCompiler {
    fn tier(&self) -> Tier {
        Tier::Optimizing
    }

    fn build<'a, 'gc>(
        &self,
        req: &CompileRequest<'a, 'gc>,
        masm: &mut Masm,
    ) -> io::Result<Option<BuiltUnit<'gc>>> {
        // `CAPY_JIT_STATS` attributes the whole Tier 2 compile, and the
        // register allocator inside it, as a fraction of an end-to-end run.
        let started = std::time::Instant::now();
        let result = self.build_inner(req, masm);
        if flags::jit_stats() {
            let micros = u64::try_from(started.elapsed().as_micros()).unwrap_or(u64::MAX);
            super::jit::config::OPT_BUILD_MICROS
                .fetch_add(micros, std::sync::atomic::Ordering::Relaxed);
        }
        result
    }
}

impl OptimizingCompiler {
    fn build_inner<'a, 'gc>(
        &self,
        req: &CompileRequest<'a, 'gc>,
        masm: &mut Masm,
    ) -> io::Result<Option<BuiltUnit<'gc>>> {
        let name = crate::runtime::objectmodel::proc::procedure_label(req.code_block);
        // Deopt rebuilds baseline frames, so there must be a
        // baseline unit to rebuild them for.
        let baseline_entry = baseline_entry(&req.code_block)?;
        let mut func =
            build::build(req.mutation, req.code_block, req.closure).map_err(io::Error::other)?;
        passes::run(&mut func, &name).map_err(io::Error::other)?;

        let (table, code_slots) =
            constants::for_codes(&func, req.code_block).map_err(io::Error::other)?;

        let mut block_addrs = Vec::with_capacity(func.codes.len());
        for (_, source) in func.codes.iter() {
            let buffer = match *source {
                ir::types::CodeSource::Root => req.code_block.buffer,
                ir::types::CodeSource::Heap(value) => {
                    value
                        .downcast::<crate::runtime::objectmodel::CodeBlock>()
                        .buffer
                }
                ir::types::CodeSource::Pool(index) => {
                    req.code_block
                        .buffer
                        .constant(index as usize)
                        .downcast::<crate::runtime::objectmodel::CodeBlock>()
                        .buffer
                }
            };
            block_addrs.push(crate::runtime::codegen::import::BlockAddrs {
                bytecode: buffer.entry() as usize,
                metadata: buffer.metadata() as usize,
                pool: buffer.pool().as_ptr() as usize,
            });
        }
        let mut lir = lir::lower::lower(&func, &name).map_err(io::Error::other)?;
        // A packed primitive call lays its arguments out above the frame.
        let max_deopt_extent = lir
            .packed_prim_extent()
            .max(inlined_extent(&func, req.code_block.frame_size));
        let (regs, scratch) = emit::register_file();

        let alloc_started = std::time::Instant::now();
        let mut alloc = regalloc::allocate(&mut lir, &regs, scratch).map_err(io::Error::other)?;
        if flags::jit_stats() {
            let micros = u64::try_from(alloc_started.elapsed().as_micros()).unwrap_or(u64::MAX);
            super::jit::config::OPT_REGALLOC_MICROS
                .fetch_add(micros, std::sync::atomic::Ordering::Relaxed);
        }

        if super::jit::config::opt().layout {
            passes::layout::reorder(&lir, &mut alloc.order);
        }
        regalloc::dump_if_enabled(&lir, &alloc, &name);

        // Install-quality budgets: a unit that outgrew the tuning target is
        // rejected here, before anything is emitted or installed, so the
        // block stays on baseline.
        let max_spills = flags::opt_max_spills();
        let max_instrs = flags::opt_max_unit_instrs();
        if max_spills != 0 || max_instrs != 0 {
            let instrs: usize = lir
                .blocks
                .values()
                .map(|block| block.insts.len() + usize::from(block.term.is_some()))
                .sum();
            let spills = alloc.spill_slots_before_polls as usize;
            let over_instrs = max_instrs != 0 && instrs > max_instrs;
            let over_spills = max_spills != 0 && spills > max_spills;
            if over_instrs || over_spills {
                crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
                log::warn!(
                    target: crate::heap::logging::DUMP_LOG_TARGET,
                    "tier2 install guard: unit {name}: {instrs} instructions (limit {max_instrs}), \
                     {spills} spill slots (limit {max_spills}); leaving the block on baseline"
                );
                return Err(io::Error::other(format!(
                    "optimizing unit {name} exceeds the install quality budget: \
                     {instrs} instructions (limit {max_instrs}), \
                     {spills} spill slots (limit {max_spills})"
                )));
            }
        }

        let buffer = req.code_block.buffer;
        let argc = entry_argc(req.code).ok_or_else(|| io::Error::other("no entry instruction"))?;
        let constant_slot = |index: u32| buffer.constant_slot(index as usize) as usize;

        let invalid = Box::new(AtomicU32::new(0));
        let osr_selectors: Vec<u32> = func.osr.iter().map(|osr| osr.selector).collect();
        let emit::Emitted {
            entry,
            osr: osr_labels,
            owner: _,
            relocs,
            data,
            frame_words,
            annotations: annotation_labels,
            ..
        } = emit::emit(
            masm,
            emit::Input {
                func: &lir,
                alloc: &alloc,
                regs: &regs,
                scratch,
                entry_regs: &func.entry_regs,
                frame_size: req.code_block.frame_size,
                argc,
                deopt_extent: max_deopt_extent,
                baseline_entry,
                invalid_word: &*invalid as *const AtomicU32 as usize,
                constant_slot: &constant_slot,
                constants: table.len(),
                code_slots: &code_slots,
                osr: &osr_selectors,
                rest: func.rest,
                case_clauses: &func.case_clauses,
                rest_reg: func.rest_reg,
                has_selector: func.entry_selector.is_some(),
                annotate: super::jit::config::opt().annotate,
            },
        )
        .map_err(io::Error::other)?;

        let deopt: Vec<DeoptPoint> = alloc
            .deopt
            .iter()
            .map(|(exit, locs)| {
                let frame = &func.frames[lir.exits[exit].frame];
                DeoptPoint {
                    pc: frame.pc,
                    locs: locs.clone(),
                    targets: deopt::metadata::deopt_dests(&func, lir.exits[exit].frame),
                    remat: alloc.remat[exit].clone(),
                    frames: deopt_frames(&func, lir.exits[exit].frame),
                    returned: frame.returned.map(|returned| ReturnedExit {
                        base: returned.base,
                        count: returned.count,
                        callee_words: frame_words,
                    }),
                    guard: lir.exits[exit].kind,
                    inlined: frame.inline_site.map(|site| InlinedSite {
                        call_pc: site.call_pc,
                        code: frame.code.index() as u32,
                    }),
                }
            })
            .collect();

        masm.align_patchable_jump();
        let unit_entry = masm.label();
        masm.bind(unit_entry);
        onlineasm!(*masm.assembler(); #[patch = entry_jump] jmp {entry};);
        let reheal = masm.label();
        masm.bind(reheal);
        let code_at = masm.assembler().mem(regs::CP, sabi::CLOSURE_CODE as i32);
        let baseline = baseline_entry as i64;
        onlineasm!(*masm.assembler();
            move {tmp::T0}, {baseline};
            storep {code_at}, {tmp::T0};
            jmp {tmp::T0};
        );
        let slows_start = masm.label();
        masm.bind(slows_start);
        let end = masm.label();
        masm.bind(end);

        let image = masm.finish().map_err(io::Error::other)?;

        let annotations = if annotation_labels.is_empty() {
            Vec::new()
        } else {
            let mut texts = vec![String::new(); alloc.moves.len()];
            for (block, data) in lir.blocks.iter() {
                let base = alloc.base[block] as usize;
                for (k, inst) in data.insts.iter().enumerate() {
                    texts[base + k] = annotate_inst(inst, &lir);
                }
                if let Some(term) = &data.term {
                    texts[base + data.insts.len()] = annotate_term(term, &lir);
                }
            }
            let mut sites: Vec<(usize, u32)> = annotation_labels
                .into_iter()
                .map(|(label, index)| {
                    let at = image.finished.offset(label).expect("bound label");
                    (at as usize, index)
                })
                .collect();

            sites.sort_by_key(|&(at, _)| at);
            sites
                .into_iter()
                .map(|(offset, index)| Annotation {
                    offset,
                    text: std::mem::take(&mut texts[index as usize]),
                })
                .collect()
        };
        let entry_off = image.finished.offset(unit_entry).expect("bound label") as usize;
        let reheal_off = image.finished.offset(reheal).expect("bound label") as usize;

        let mut osr = Vec::with_capacity(osr_labels.len());
        for (selector, label) in osr_labels {
            let at = image.finished.offset(label).expect("bound label");
            let Some(entry) = func.osr.iter().find(|osr| osr.selector == selector) else {
                return Err(io::Error::other("an OSR prologue has no OSR entry"));
            };
            let pc = BytecodePc::new(entry.pc as usize)
                .ok_or_else(|| io::Error::other("an OSR pc does not fit in a word"))?;
            osr.push((pc, CodeOffset(at as u32)));
        }
        let jump_at = match image.finished.location(entry_jump) {
            Some(PatchLocation::Jump(jump)) => jump.offset() as usize,
            _ => return Err(io::Error::other("the entry jump is not a jump site")),
        };
        if flags::jit_dump_dir().is_some()
            && crate::runtime::codegen::jit::config::opt()
                .dump
                .iter()
                .any(|stage| stage == "asm")
        {
            dump_code(&name, image.finished.data());
        }

        // The return addresses are patchable loads (AArch64, `LabelAddr`),
        // which only link applies. Without them every call site jumps
        // through the placeholder.
        let mut emission = Emission::default();
        emission.relocs = relocs;
        // Dense switch tables are the optimizer's own data islands.
        for (start, len) in data {
            emission.data_range(start, len);
        }
        let labels = HashMap::new();
        // The prologue, before the image is moved into the link. The body is
        // emitted before the unit's entry jump, so a perf range that starts at
        // the entry jump would miss almost all of the code.
        let prologue_off = image
            .finished
            .offset(entry)
            .ok_or_else(|| io::Error::other("the unit prologue was never bound"))?
            as usize;
        let Linked {
            loaded,
            cells,
            data,
            slows_start_off,
            end_off,
            owner_at,
            ..
        } = emission.link(
            image,
            &Layout {
                labels: &labels,
                pcs: &[],
                slows_start,
                end,
                entry: None,
            },
            &crate::runtime::codegen::import::block_resolver(
                block_addrs,
                Some(&*invalid as *const AtomicU32 as usize),
                Some(baseline_entry),
            ),
        )?;
        let entry_addr = loaded.base() + entry_off;
        // The perf-map range is published by the install, not by the build:
        // a unit that is discarded or raced out of installation must not
        // leave a symbol behind. `code_start` is the prologue offset.

        let opt = OptInfo {
            deopt,
            max_deopt_extent,
            frame_words: frame_words + max_deopt_extent,
            osr,
            code_slots,
            invalid,
            entry_jump: CodeOffset(jump_at as u32),
            reheal: CodeOffset(reheal_off as u32),
        };
        Ok(Some(BuiltUnit {
            tier: Tier::Optimizing,
            loaded,
            rows: Vec::new(),
            entry_addr,
            cells,
            ranges: Vec::new(),
            code_start: prologue_off,
            annotations,
            data,
            slows_start_off,
            end_off,
            owner_at,
            opt: Some(opt),
            constants: table.into_values(),
            dependencies: func
                .deps
                .iter()
                .map(|dep| super::unit::Dependency {
                    variable: dep.variable,
                    value: dep.value,
                })
                .collect(),
            class_dependencies: func
                .class_deps
                .iter()
                .map(|dep| super::unit::ClassDependency {
                    class: dep.class,
                    id: dep.id,
                    stamp: dep.stamp,
                })
                .collect(),
        }))
    }
}

/// The entry of `code_block`'s baseline unit, which Tier 2 deopts to.
fn baseline_entry(code_block: &CodeBlock<'_>) -> io::Result<usize> {
    let owner = code_block
        .machine
        .get()
        .ok_or_else(|| io::Error::other("no baseline unit to deoptimize to"))?;
    let unit = CodeUnit::of_image(owner)
        .filter(|unit| unit.tier == Tier::Baseline)
        .ok_or_else(|| io::Error::other("no baseline unit to deoptimize to"))?;
    // The baseline entry is the redirect that enters optimized code, which
    // would bounce a fallback back into this unit. The fallback is the first
    // instruction, past the redirect.
    let first = crate::runtime::codegen::addr::BytecodePc::new(0)
        .and_then(|pc| unit.resume.lookup(pc))
        .ok_or_else(|| io::Error::other("no baseline unit to deoptimize to"))?;
    Ok(first.as_usize())
}

/// The listing text of one LIR instruction: its own `Display`, followed by
/// the IR values its registers came from, once per register in operand order.
fn annotate_inst(inst: &lir::LirInst, func: &lir::LirFunc) -> String {
    annotate_with_ir(inst.to_string(), func)
}

/// The listing text of one LIR terminator, with the same IR annotations.
fn annotate_term(term: &lir::LirTerm, func: &lir::LirFunc) -> String {
    annotate_with_ir(term.to_string(), func)
}

/// Append the IR value behind each `vrN` the display names, once per
/// register. Frame-state values are deliberately left out: they belong to
/// the exit, not to the instruction's own semantics.
fn annotate_with_ir(text: String, func: &lir::LirFunc) -> String {
    use std::fmt::Write as _;
    let mut out = text.clone();
    let mut seen: Vec<u32> = Vec::new();
    let mut at = 0;
    while let Some(found) = text[at..].find("vr") {
        let start = at + found;
        let digits = start + 2;
        let mut end = digits;
        while end < text.len() && text.as_bytes()[end].is_ascii_digit() {
            end += 1;
        }
        at = end.max(start + 1);
        if end == digits {
            continue;
        }
        let Ok(index) = text[digits..end].parse::<u32>() else {
            continue;
        };
        if seen.contains(&index) {
            continue;
        }
        seen.push(index);
        if let Some(value) = func
            .vregs
            .get(lir::VReg::new(index as usize))
            .and_then(|data| data.value)
        {
            let _ = write!(out, " [{value}]");
        }
    }
    out
}

/// The argument count of the procedure, from its leading `enter`.
fn entry_argc(code: &[u8]) -> Option<u32> {
    match def::Instruction::decode_slice(code, 0)? {
        (Instruction::OpEnter(op), _, _) => Some(op.argc.0),
        // A variadic entry takes at least `required` arguments.
        (Instruction::OpEnterRest(op), _, _) => Some(op.required.0),
        // A multi-entry block selects its clause from `AC`; nothing uses a
        // single count.
        (Instruction::OpEnterCase(_), _, _) => Some(0),
        _ => None,
    }
}

/// Monotonic id so two units with the same procedure label (anonymous
/// lambdas all render as `_unknown_procedure_`) do not overwrite each other's
/// dump files.
static NEXT_DUMP_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

pub(crate) fn next_dump_id() -> usize {
    NEXT_DUMP_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1
}

/// The dump file name for a procedure label. `procedure_label` carries a
/// name and a `file:line:col` span, so spaces, `:`, `/` and angle brackets
/// are common; `file_safe` keeps them from changing the directory or making
/// the open fail silently.
fn dump_file_name(name: &str) -> String {
    format!("opt-{:04}-{}.bin", next_dump_id(), file_safe(name))
}

/// Writes the raw machine code for `CAPY_OPT_DUMP=asm`, into `CAPY_JIT_DUMP_DIR`.
fn dump_code(name: &str, code: &[u8]) {
    let Some(dir) = flags::jit_dump_dir() else {
        return;
    };
    let file = dir.join(dump_file_name(name));
    if let Err(err) = std::fs::create_dir_all(dir).and_then(|()| std::fs::write(&file, code)) {
        log::warn!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            " cannot write machine code dump to {}: {err}",
            file.display()
        );
    }
}

/// The frames of `frame`'s chain, outermost first.
fn deopt_frames(
    func: &ir::func::Func,
    frame: ir::frame_state::FrameState,
) -> Vec<crate::runtime::codegen::unit::DeoptFrame> {
    let mut chain = Vec::new();
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        chain.push(crate::runtime::codegen::unit::DeoptFrame {
            pc: data.pc,
            base: if data.parent.is_some() {
                data.call_base.unwrap_or(0)
            } else {
                0
            },
        });
        cur = data.parent;
    }
    chain.reverse();
    chain
}

/// The words past the optimized frame's bytecode frame that rebuilding the
/// deepest inlined chain of `func` writes: each callee frame starts at its
/// call's base register in the caller's frame and spans its own frame size.
fn inlined_extent(func: &ir::func::Func<'_>, root_size: u32) -> u32 {
    use ir::types::CodeSource;
    let size_of_code = |code: ir::types::CodeRef| -> u32 {
        match func.codes[code] {
            CodeSource::Root => root_size,
            CodeSource::Heap(value) => {
                value
                    .downcast::<crate::runtime::objectmodel::CodeBlock>()
                    .frame_size
            }
            CodeSource::Pool(_) => root_size,
        }
    };
    let mut extent = 0;
    for (fs, data) in func.frames.iter() {
        if data.parent.is_none() {
            continue;
        }
        let mut chain = Vec::new();
        let mut cur = Some(fs);
        while let Some(f) = cur {
            chain.push(f);
            cur = func.frames[f].parent;
        }
        let mut offset = 0;
        let mut end = 0;
        for &f in chain.iter().rev() {
            let frame = &func.frames[f];
            if frame.parent.is_some() {
                offset += frame.call_base.unwrap_or(0);
            }
            end = offset + size_of_code(frame.code);
            // A variadic frame's extras sit at `r[base..]`, possibly past its
            // frame; an inlined site passes at most `MAX_INLINED_EXTRAS`.
            if let (Some(rest), true) = (frame.rest, frame.parent.is_some()) {
                end = end.max(offset + rest.base + build::MAX_INLINED_EXTRAS);
            }
        }
        extent = extent.max(end.saturating_sub(root_size));
    }
    extent
}

#[cfg(test)]
mod tests {
    #[test]
    fn dump_file_names_are_file_safe() {
        let name = "<anonymous> at lib/foo bar.scm:12:3";
        let file = super::dump_file_name(name);
        for bad in ['/', ' ', ':', '<', '>'] {
            assert!(!file.contains(bad), "{file:?} contains {bad:?}");
        }
        assert!(file.starts_with("opt-"), "{file:?}");
        assert!(file.ends_with(".bin"), "{file:?}");
    }
}
