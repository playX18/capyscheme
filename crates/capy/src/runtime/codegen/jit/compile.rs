//! The tier-up driver and the compiler.

use std::collections::HashMap;
use std::io;
use std::sync::atomic::Ordering::{AcqRel, Acquire, Relaxed};

use bytecodegen::OperandWidth;
use onlineasm::Label;

use super::super::addr::BytecodePc;
use super::super::emission::{Emission, Layout, Linked};
use super::super::image::LoadedImage;
use super::super::lazy::ErasedCell;
use super::super::masm::{HeaderTarget, Masm, SymbolKind, TOPLINK_TO_IMAGE_START};
use super::super::reloc::Reloc;
use super::super::template::emit;
use super::super::template_image::{declare_slow_paths, ensure_loaded};
use super::super::unit::{CodeUnit, Listing, Native, Tier};
use super::config::{Bisect, COMPILES, bisect, template_interpreter};
use super::context::{DecodedInstruction, JitTemplateContext, ProcTargets, ProfileSpec};
use super::registry::{DataRange, InstructionRange, LIVE_CODE_BYTES, NEXT_ID, ResumeTable};
use crate::bytecode::def::{self, MV_ERROR, OpcodeId};
use crate::heap::{Gc, barrier};
use crate::runtime::MutationContext;
use crate::runtime::sabi::{self, RpCompactHeader};
use crate::runtime::value::{CodeBlock, CodeBuffer, CodeImage, Promotion};
use crate::utils::flags;
use mmtk::util::Address;

pub(crate) fn compile_allowed<'gc>(code_block: Gc<'gc, CodeBlock<'gc>>) -> bool {
    let constant_pointer = Address::from_ptr(code_block.buffer.entry());

    if LIVE_CODE_BYTES.load(Relaxed) >= flags::jit_max_code() {
        super::super::reclaim::drain();
    }
    if LIVE_CODE_BYTES.load(Relaxed) >= flags::jit_max_code() {
        log::info!(target: crate::heap::logging::JIT_LOG_TARGET, "skip constant_pointer={constant_pointer} (over the code budget)");

        return false;
    }
    match bisect(NEXT_ID.load(Relaxed), || {
        code_block.metadata.get().to_string()
    }) {
        Bisect::Allowed => true,
        Bisect::PastLimit(limit) => {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "skip constant_pointer={constant_pointer} (past CAPY_JIT_LIMIT={limit})"
            );

            false
        }
        Bisect::FilteredOut => {
            log::info!(target: crate::heap::logging::JIT_LOG_TARGET, "jit: skip constant_pointer={constant_pointer} (filtered out)");

            false
        }
    }
}

pub(crate) fn log_tier_up_request<'gc>(
    code_block: Gc<'gc, CodeBlock<'gc>>,
    heat: u32,
    threshold: u32,
    id: usize,
) {
    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "tier-up constant_pointer={} {} (heat={heat} threshold={threshold})",
        code_block.buffer.entry() as usize,
        code_block.metadata.get(),
    );
    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "#{id}: {} ({} bytes of bytecode)",
        code_block.metadata.get(),
        code_block.code().len()
    );

    if log::log_enabled!(target: crate::heap::logging::JIT_LOG_TARGET, log::Level::Debug) {
        let code = code_block.code();
        let mut pc = 0;
        while let Some((instruction, _, len)) = def::Instruction::decode_slice(code, pc) {
            log::debug!(target: crate::heap::logging::JIT_LOG_TARGET, "  {pc:4}: {instruction:?}");
            pc += len;
            if pc >= code.len() {
                break;
            }
        }
    }
}

/// The live mask of return point `mask` (an aux-pool entry: bit `j` for
/// `fp[j]`, bits 0 and 1 unused) in header form: bit `i` for `fp[i + 1]`,
/// the closure always live.
fn header_mask(pool: &[u32], mask: u32) -> Vec<u64> {
    // A pool entry's first word counts the words that follow.
    let entry = &pool[mask as usize..];
    let nwords = entry[0] as usize;
    let mut bits = vec![1u64];
    for (word, &word_bits) in entry[1..=nwords].iter().enumerate() {
        let mut word_bits = word_bits;
        if word == 0 {
            word_bits &= !0b11;
        }
        while word_bits != 0 {
            let slot = word * 32 + word_bits.trailing_zeros() as usize;
            let bit = slot - 1;
            if bits.len() <= bit / 64 {
                bits.resize(bit / 64 + 1, 0);
            }
            bits[bit / 64] |= 1 << (bit % 64);
            word_bits &= word_bits - 1;
        }
    }
    bits
}

fn bit_length(bits: &[u64]) -> usize {
    bits.iter()
        .enumerate()
        .rev()
        .find(|(_, word)| **word != 0)
        .map_or(0, |(index, word)| {
            index * 64 + 64 - word.leading_zeros() as usize
        })
}

fn is_return_point(op: OpcodeId) -> bool {
    matches!(op, OpcodeId::OpReturnPoint | OpcodeId::OpReturnPointForward)
}

fn next_may_elide(op: OpcodeId) -> bool {
    !matches!(op, OpcodeId::OpRestRef | OpcodeId::OpReturnPoint)
}

fn falls_through(instructions: &[(usize, OpcodeId, OperandWidth, usize)], index: usize) -> bool {
    let (pc, op, _, len) = instructions[index];
    match instructions.get(index + 1) {
        Some((next_pc, next_op, _, _)) => {
            *next_pc == pc + len && !is_return_point(*next_op) && next_may_elide(op)
        }
        None => false,
    }
}

pub(crate) struct CompileRequest<'a> {
    /// The bytecode.
    pub(crate) code: &'a [u8],
    /// The metadata area, 0 for none.
    pub(crate) md: Address,
    /// The aux pool.
    pub(crate) pool: &'a [u32],
    /// The procedure's `file:line:col` for the image symbol.
    pub(crate) label: Option<&'a str>,
}

pub(crate) trait TierCompiler: Send + Sync {
    /// The tier this compiler produces.
    fn tier(&self) -> Tier;

    /// Compile `req`
    fn build(&self, req: &CompileRequest<'_>) -> io::Result<Option<BuiltUnit>>;
}

/// The baseline compiler: the interpreter's templates with operands as
/// immediates, one image per procedure ([`build_image`]).
pub(crate) struct BaselineCompiler;

impl TierCompiler for BaselineCompiler {
    fn tier(&self) -> Tier {
        Tier::Baseline
    }

    fn build(&self, req: &CompileRequest<'_>) -> io::Result<Option<BuiltUnit>> {
        build_image(req.code, req.md, req.pool, req.label)
    }
}

/// The compiler for `tier`, if this build has one.
pub(crate) fn compiler_for(tier: Tier) -> Option<Box<dyn TierCompiler>> {
    match tier {
        Tier::Baseline => Some(Box::new(BaselineCompiler)),
        Tier::Lbbv | Tier::Optimizing | Tier::Trace => None,
    }
}

/// A compiled unit awaiting attachment: built by a [`TierCompiler`] on a
/// worker (or the requesting mutator when workers are off), then
/// [`attach`]ed to its code buffer on a mutator thread.
pub(crate) struct BuiltUnit {
    tier: Tier,
    loaded: LoadedImage,
    rows: Vec<(BytecodePc, Address)>,
    entry_addr: Address,
    cells: Box<[ErasedCell]>,
    masks: Vec<Box<[u64]>>,
    ranges: Vec<InstructionRange>,
    data: Vec<DataRange>,
    slows_start_off: usize,
    end_off: usize,
    /// Image offset of the owner word, which [`attach`] patches.
    owner_at: usize,
}

unsafe impl Send for BuiltUnit {}

pub(crate) fn attach<'gc>(
    mutator: MutationContext<'gc>,
    buffer: Gc<'gc, CodeBuffer<'gc>>,
    built: BuiltUnit,
    id: usize,
) -> io::Result<()> {
    // Release what the last collections found dead
    super::super::reclaim::drain();
    let BuiltUnit {
        tier,
        loaded,
        rows,
        entry_addr,
        cells,
        masks,
        ranges,
        data,
        slows_start_off,
        end_off,
        owner_at,
    } = built;
    let unit = CodeUnit::new(
        tier,
        loaded,
        entry_addr,
        ResumeTable::new(rows, cells),
        masks,
        id,
        Address::from_ptr(buffer.entry()),
        Listing {
            ranges,
            data,
            slows_start: slows_start_off,
            code_end: end_off,
        },
    );

    // The owner holds the unit. Its slot never moves, and its address goes
    // into the code's header before the code is published.
    let owner = CodeImage::alloc(mutator, Native::Unit(unit));
    let unit = CodeUnit::of_image(owner).expect("an owner of a unit holds one");
    // The unit sits where it stays: its cells can point at its table.
    unit.resume.bind();
    let slot_addr = owner.owner_slot().as_usize() as u64;
    unit.image
        .patch_unpublished(owner_at, &slot_addr.to_ne_bytes())?;
    buffer.built.set(Gc::write(mutator, buffer), Some(owner));
    Ok(())
}

/// Install the code attached to `code_block`'s buffer
fn publish<'gc>(mutator: MutationContext<'gc>, code_block: Gc<'gc, CodeBlock<'gc>>) -> bool {
    let Some(owner) = code_block.buffer.built.get() else {
        return false;
    };
    let unit = CodeUnit::of_image(owner).expect("a built owner holds a unit");
    if crate::runtime::profile::armed() {
        let base = unit.image.base().as_usize() as u64;
        crate::runtime::profile::maps::publish_jit(
            mutator,
            base,
            base.wrapping_add(unit.listing.code_end as u64),
            unit.listing.slows_start as u32,
            unit.listing
                .ranges
                .iter()
                .map(|range| (range.offset as u32, range.pc as u32))
                .collect(),
            unit.constant_pointer.as_usize() as u64,
            code_block.srcmap.get(),
            code_block.metadata.get(),
        );
    }
    barrier::field!(Gc::write(mutator, code_block), CodeBlock, machine)
        .unlock()
        .set(Some(owner));
    // Publish the resume table before the entry: a resume that observes the
    // entry then always finds the table.
    code_block.jit_entry.publish(unit.entry);
    if flags::jit_stats() {
        COMPILES.fetch_add(1, Relaxed);
    }
    super::dump::maybe_dump(code_block);
    true
}

/// Mark `code_block` never to compile.
pub(crate) fn poison(code_block: Gc<'_, CodeBlock<'_>>) {
    code_block.jit_entry.poison();
}

/// Take the right to install `code_block`'s attached code, once.
fn claim_install(code_block: Gc<'_, CodeBlock<'_>>) -> bool {
    code_block
        .promotion
        .compare_exchange(
            Promotion::Submitted as u8,
            Promotion::Installed as u8,
            AcqRel,
            Relaxed,
        )
        .is_ok()
}

/// Install the code a compile attached to `code_block`'s buffer, if the
/// compile is done. Returns whether the block has code now (installed here
/// or before), or is marked never to get any. Never blocks: an unfinished
/// compile is merely slow, and a lost install race resolves at the next
/// window.
pub(crate) fn install<'gc>(
    mutator: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
) -> bool {
    if code_block.jit_entry.is_decided(Acquire) {
        return true;
    }
    let buffer = code_block.buffer;
    if buffer.build_failed.load(Acquire) {
        poison(code_block);
        return true;
    }
    if buffer.built.is_none() || !claim_install(code_block) {
        return code_block.jit_entry.is_decided(Acquire);
    }
    if !publish(mutator, code_block) {
        poison(code_block);
    }
    true
}

/// Compile `code_block` on the calling thread and install its code. Returns
/// `Ok` without compiling when the procedure already has code, is marked
/// never to get any, or has a compile pending elsewhere.
pub fn compile<'gc>(
    mutator: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
) -> io::Result<()> {
    if code_block.jit_entry.is_decided(Acquire) || !claim_compile(code_block) {
        return Ok(());
    }
    let id = NEXT_ID.fetch_add(1, Relaxed);
    let result = compile_claimed(mutator, code_block, &BaselineCompiler, id);
    if result.is_err() {
        poison(code_block);
    }
    result
}

/// Take the right to compile `code_block`: one claim per procedure, held
/// until its code is installed. A second image would replace the first's
/// owner while closures healed to the first still jump into it.
pub(crate) fn claim_compile(code_block: Gc<'_, CodeBlock<'_>>) -> bool {
    code_block
        .promotion
        .compare_exchange(
            Promotion::Interpreted as u8,
            Promotion::Submitted as u8,
            AcqRel,
            Relaxed,
        )
        .is_ok()
}

/// Build, attach and install `code_block`'s code on the calling thread.
/// The caller holds the compile claim. An `Err` leaves the block unbuilt
/// for the caller to poison.
pub(crate) fn compile_claimed<'gc>(
    mutator: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    compiler: &dyn TierCompiler,
    id: usize,
) -> io::Result<()> {
    // Compiled code jumps into the interpreter image.
    ensure_loaded();
    let buffer = code_block.buffer;
    let label = crate::runtime::value::proc::procedure_span_short(mutator, code_block);
    let request = CompileRequest {
        code: code_block.code(),
        md: Address::from_ptr(buffer.metadata()),
        pool: buffer.pool(),
        label: label.as_deref(),
    };
    let Some(built) = compiler.build(&request)? else {
        // Over the per-block limit: never retry.
        poison(code_block);
        return Ok(());
    };
    attach(mutator, buffer, built, id)?;
    if claim_install(code_block) && !publish(mutator, code_block) {
        poison(code_block);
    }
    Ok(())
}

/// Build (but do not publish) the image for live code
fn build_image(
    code: &[u8],
    md: Address,
    pool: &[u32],
    label: Option<&str>,
) -> std::io::Result<Option<BuiltUnit>> {
    let entry = code.as_ptr();
    let constant_pointer = Address::from_ptr(entry);

    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "constant_pointer={constant_pointer} md={md} pool={} words",
        pool.len()
    );

    let mut instructions = Vec::new();
    let mut pc = 0;
    while pc < code.len() {
        // SAFETY: the code was verified when it was loaded.
        let (width, op, _) = unsafe { def::decode_header(entry.add(pc)) };
        let len = unsafe { def::Instruction::size_at_ptr(entry.add(pc)) };
        instructions.push((pc, op, width, len));
        pc += len;
    }

    let mut masm = Masm::new();
    let paths = declare_slow_paths(&mut masm);
    let start = masm.label();
    masm.bind(start);
    // The chunk header: the owner slot's address, patched at publish.
    let owner = masm.label();
    masm.bind(owner);
    masm.reserve_words(8);
    let labels: HashMap<usize, Label> = instructions
        .iter()
        .map(|(pc, ..)| (*pc, masm.label()))
        .collect();
    let mut masks = Vec::new();

    let &(first_pc, first_op, first_width, first_len) = instructions
        .first()
        .expect("a procedure compiles at least one instruction");
    // SAFETY: `first_pc` is a decoded instruction start within the code.
    let first_ip = unsafe { entry.add(first_pc) };
    let mut emission = Emission::default();
    let mut context = JitTemplateContext::new(
        DecodedInstruction {
            op: first_op,
            width: first_width,
            ip: first_ip,
            len: first_len,
            entry,
            constant_pointer: constant_pointer.as_usize(),
            md: md.as_usize(),
        },
        ProcTargets::new(labels, falls_through(&instructions, 0)),
        ProfileSpec,
        &mut emission,
        paths,
    );
    for (index, &(pc, op, width, len)) in instructions.iter().enumerate() {
        let label = context.label_at(pc);
        let fallthrough = falls_through(&instructions, index);
        // SAFETY: `pc` is a decoded instruction start within the code.
        let ip = unsafe { entry.add(pc) };
        // SAFETY: `ip` is the instruction at `pc`.
        let operands = unsafe { def::decode_header(ip).2 };
        let return_point = match op {
            OpcodeId::OpReturnPoint => {
                // SAFETY: `operands` points at this `return-point`'s operands.
                let (instruction, _) =
                    unsafe { def::OpReturnPoint::decode_from_ptr(operands, width) };
                Some((
                    instruction.frame.0,
                    instruction.mask.0,
                    instruction.mv.0 == MV_ERROR,
                    None,
                ))
            }
            OpcodeId::OpReturnPointForward => {
                // SAFETY: `operands` points at this `return-point-forward`'s operands.
                let (instruction, _) =
                    unsafe { def::OpReturnPointForward::decode_from_ptr(operands, width) };
                // Any count but one goes on to the `retpt_mv*` after it.
                Some((
                    instruction.frame.0,
                    instruction.mask.0,
                    false,
                    Some(pc + len),
                ))
            }
            _ => None,
        };
        match return_point {
            Some((frame, mask, values_error, mv)) => {
                let bits = header_mask(pool, mask);
                let length = bit_length(&bits);
                let frame = frame as usize;
                if mv.is_none()
                    && frame <= sabi::COMPACT_FRAME_MAX_WORDS
                    && length < 63 - sabi::COMPACT_FRAME_MASK_OFFSET as usize
                {
                    masm.emit_rp_compact_header_at(
                        label,
                        RpCompactHeader::new(
                            TOPLINK_TO_IMAGE_START,
                            values_error,
                            frame,
                            bits[0] as usize,
                        ),
                    );
                } else {
                    let livemask = if length < 63 {
                        sabi::livemask_fixnum(bits[0] as usize)
                    } else {
                        let mut words = vec![bits.len() as u64];
                        words.extend_from_slice(&bits);
                        let words = words.into_boxed_slice();
                        let addr = words.as_ptr() as usize;
                        masks.push(words);
                        addr
                    };
                    let target = match mv {
                        Some(pc) => HeaderTarget::Label(context.label_at(pc)),
                        None if values_error => {
                            HeaderTarget::Absolute(template_interpreter().values_error)
                        }
                        None => HeaderTarget::Label(label),
                    };
                    masm.emit_rp_header_at(
                        label,
                        target,
                        livemask,
                        TOPLINK_TO_IMAGE_START,
                        (frame * 8) as isize,
                    );
                }
            }
            None => masm.bind(label),
        }
        // Reposition at every instruction, so the body treats all
        // uniformly, a no-op the first time, where the context starts.
        context.set_instruction(
            DecodedInstruction {
                op,
                width,
                ip,
                len,
                entry,
                constant_pointer: constant_pointer.as_usize(),
                md: md.as_usize(),
            },
            fallthrough,
        );
        emit::emit_template(&mut masm, &mut context, op);
    }
    let labels = context.into_targets().into_labels();
    emission.reloc(Reloc::OwnerSlot { at: owner });
    // The deferred slow/taken blocks, in template order: every fast path
    // above falls through to the next instruction.
    let slows_start = masm.label();
    masm.bind(slows_start);
    emission.emit_deferred(&mut masm, paths);
    let end = masm.label();
    masm.bind(end);
    masm.cfi_region(start, end, sabi::SCHEME_ENTRY_CFA_OFFSET as u32);
    // Span-annotated when the block carries its procedure's source, so
    // `CAPY_PERF_MAP` lines attribute JIT code to `file:line:col`, bare
    // otherwise (yesterday's format, still parseable).
    let name = match label {
        Some(span) => format!("jit:{constant_pointer} {span}"),
        None => format!("jit:{constant_pointer}"),
    };
    masm.define_symbol(&name, start, end, SymbolKind::Function);

    let image = masm.finish().map_err(std::io::Error::other)?;
    // Over the per-block limit: build nothing, the caller poisons the block
    // so it never retries. Silent like the other limit skips, only the log
    // names it.
    let bytes = image.finished.data().len();
    let block_limit = flags::jit_max_code_per_block();
    if bytes > block_limit {
        log::info!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "{constant_pointer} exceeds the per-block code limit ({bytes} > {block_limit} bytes)"
        );

        return Ok(None);
    }
    // The last poll is behind us: link takes the code memory lock.
    let pcs: Vec<usize> = instructions.iter().map(|&(pc, ..)| pc).collect();
    let Linked {
        loaded,
        offsets,
        cells,
        data,
        ranges,
        slows_start_off,
        end_off,
        owner_at,
    } = emission.link(
        image,
        &Layout {
            labels: &labels,
            pcs: &pcs,
            slows_start,
            end,
        },
    )?;
    let base = loaded.base();

    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "constant_pointer={constant_pointer} sites={}",
        cells.len()
    );

    let rows: Vec<(BytecodePc, Address)> = offsets
        .iter()
        .map(|(pc, offset)| {
            (
                BytecodePc::new(*pc).expect("bytecode fits in u32"),
                base + *offset,
            )
        })
        .collect();
    let entry_addr = base + offsets[&0];

    log::info!(target: crate::heap::logging::JIT_LOG_TARGET, "base={base}, entry={entry_addr}");
    if log::log_enabled!(target: crate::heap::logging::JIT_LOG_TARGET, log::Level::Debug) {
        let mut sorted = rows.clone();
        sorted.sort();
        log::debug!(target: crate::heap::logging::JIT_LOG_TARGET, "labels={sorted:x?}")
    }

    Ok(Some(BuiltUnit {
        tier: Tier::Baseline,
        loaded,
        rows,
        entry_addr,
        cells,
        masks,
        ranges,
        data,
        slows_start_off,
        end_off,
        owner_at,
    }))
}
