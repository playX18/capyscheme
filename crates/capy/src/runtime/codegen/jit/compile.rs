//! The tier-up driver and the compiler.

use std::collections::HashMap;
use std::io;
use std::mem::size_of;
use std::sync::atomic::Ordering::{AcqRel, Acquire, Relaxed, Release};

use bytecodegen::OperandWidth;
use onlineasm::Label;

use super::super::addr::BytecodePc;
use super::super::emission::{Emission, Layout, Linked};
use super::super::image::LoadedImage;
use super::super::lazy::ErasedCell;
use super::super::lazy::edge::encode_jump;
use super::super::masm::{HeaderTarget, Masm, SymbolKind, TOPLINK_TO_IMAGE_START};
use super::super::opt::OptimizingCompiler;
use super::super::reloc::Reloc;
use super::super::template::emit;
use super::super::template_image::{declare_slow_paths, ensure_loaded};
use super::super::unit::{Annotation, CodeUnit, Listing, OptInfo, PublishedCode, Tier};
use super::config::{
    Bisect, COMPILES, OPT_BUILD_FAILURES, OPT_INSTALLS, bisect, template_interpreter,
};
use super::context::{DecodedInstruction, JitTemplateContext, ProcTargets, ProfileSpec};
use super::entry::emit_optimized_redirect;
use super::registry::{DataRange, InstructionRange, NEXT_ID, ResumeTable};
use crate::bytecode::def::{self, MV_ERROR, OpcodeId};
use crate::heap::{Gc, barrier};
use crate::runtime::MutationContext;
use crate::runtime::code_memory::code_memory;
use crate::runtime::objectmodel::{CodeBlock, CodeBuffer, CodeImage, Value};
use crate::runtime::sabi::{self, RpCompactHeader};
use crate::utils::flags;
use mmtk::util::Address;

pub(crate) fn compile_allowed<'gc>(
    code_block: Gc<'gc, CodeBlock<'gc>>,
    partition: super::super::cache::Partition,
) -> bool {
    let label = crate::runtime::objectmodel::proc::procedure_label(code_block);

    if !super::super::cache::admit(partition, 0) {
        log::info!(target: crate::heap::logging::JIT_LOG_TARGET, "skip {label} (over the code budget)");

        return false;
    }
    match bisect(NEXT_ID.load(Relaxed), || {
        code_block.metadata.get().to_string()
    }) {
        Bisect::Allowed => true,
        Bisect::PastLimit(limit) => {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "skip {label} (past CAPY_JIT_LIMIT={limit})"
            );

            false
        }
        Bisect::FilteredOut => {
            log::info!(target: crate::heap::logging::JIT_LOG_TARGET, "jit: skip {label} (filtered out)");

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
    let label = crate::runtime::objectmodel::proc::procedure_label(code_block);
    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "tier-up {label} (heat={heat} threshold={threshold})"
    );
    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "#{id}: {label} ({} bytes of bytecode)",
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

pub(crate) struct CompileRequest<'a, 'gc> {
    /// The bytecode.
    pub(crate) code: &'a [u8],
    /// The metadata area, 0 for none.
    pub(crate) md: Address,
    /// The aux pool.
    pub(crate) pool: &'a [u32],
    /// The procedure's `file:line:col` for the image symbol.
    pub(crate) label: Option<&'a str>,
    /// The procedure, rooted by the caller for the whole build.
    pub(crate) code_block: Gc<'gc, CodeBlock<'gc>>,
    /// The closure whose entry requested the compile, when known. Only
    /// Tier 2 reads it (to fold globals through its free variables).
    pub(crate) closure: Option<Gc<'gc, crate::runtime::objectmodel::proc::Closure<'gc>>>,
    /// The mutation scope the build runs in.
    pub(crate) mutation: MutationContext<'gc>,
}

pub(crate) trait TierCompiler: Send + Sync {
    /// The tier this compiler produces.
    fn tier(&self) -> Tier;

    /// Compile `req` into `masm`
    fn build<'a, 'gc>(
        &self,
        req: &CompileRequest<'a, 'gc>,
        masm: &mut Masm,
    ) -> io::Result<Option<BuiltUnit<'gc>>>;
}

/// The baseline compiler: the interpreter's templates with operands as
/// immediates, one image per procedure ([`build_image`]).
pub(crate) struct BaselineCompiler;

impl TierCompiler for BaselineCompiler {
    fn tier(&self) -> Tier {
        Tier::Baseline
    }

    fn build<'a, 'gc>(
        &self,
        req: &CompileRequest<'a, 'gc>,
        masm: &mut Masm,
    ) -> io::Result<Option<BuiltUnit<'gc>>> {
        build_image(masm, req.code, req.md, req.pool, req.label)
    }
}

/// The compiler for `tier`, if this build has one.
pub(crate) fn compiler_for(tier: Tier) -> Option<Box<dyn TierCompiler>> {
    match tier {
        Tier::Baseline => Some(Box::new(BaselineCompiler)),
        Tier::Optimizing => Some(Box::new(OptimizingCompiler)),
        Tier::Lbbv | Tier::Trace => None,
    }
}

/// Record the outcome of a Tier 2 build for `code_block`. A finished unit is
/// installed as the block's optimized code. Any other outcome leaves the
/// block on baseline code, and `Failed` is terminal.
pub(crate) fn finish_optimized<'gc>(
    mutator: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    id: usize,
    generation: super::super::install::Generation,
    result: Result<BuiltUnit<'gc>, ()>,
) {
    let installed = match result {
        Ok(built) => install_optimized(mutator, code_block, built, id, generation).map_err(|err| {
            log::warn!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "job #{id} cannot install optimized code: {err}"
            );
        }),
        Err(()) => Err(()),
    };
    if installed.is_err() {
        // A failed build may be transient (an install race, a worker panic,
        // a worker evicted at the wrong moment). Retry it up to
        // `MAX_RECOMPILES` times instead of dropping the procedure to
        // baseline forever
        if flags::jit_stats() {
            OPT_BUILD_FAILURES.fetch_add(1, Relaxed);
        }
        let next = code_block.fail_opt(generation);
        log::warn!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "job #{id}: optimized build failed; state {next:?}"
        );
    }
}

/// Install a finished Tier 2 unit as `code_block`'s optimized code.
fn install_optimized<'gc>(
    mutator: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    built: BuiltUnit<'gc>,
    id: usize,
    generation: super::super::install::Generation,
) -> io::Result<()> {
    let BuiltUnit {
        tier,
        loaded,
        rows,
        entry_addr,
        cells,
        masks,
        ranges,
        code_start,
        annotations,
        data,
        slows_start_off,
        end_off,
        owner_at,
        opt,
        constants,
        dependencies,
        class_dependencies,
    } = built;
    let opt: OptInfo = opt.ok_or_else(|| io::Error::other("an optimized build has no OptInfo"))?;
    if code_block.machine.load(Acquire).is_none() {
        return Err(io::Error::other("no baseline unit to fall back to"));
    }
    let sites = opt.constant_sites.clone();

    let mut refs = vec![Value::null(); 1 + constants.len() + 1];
    let refs_base = refs.as_ptr() as usize;
    let perf_start = loaded.base().as_usize() + code_start;
    let perf_end = loaded.base().as_usize() + end_off;
    let mut unit = CodeUnit::new(
        tier,
        loaded,
        entry_addr,
        ResumeTable::new(rows, cells),
        masks,
        id,
        Address::from_ptr(code_block.buffer.entry()),
        Listing {
            code_start,
            ranges,
            annotations,
            data,
            slows_start: slows_start_off,
            code_end: end_off,
        },
    );
    for &(at, slot) in &sites {
        if slot == 0 || slot as usize > constants.len() {
            return Err(io::Error::other(format!(
                "constant table slot {slot} is missing"
            )));
        }
        let addr = refs_base + slot as usize * size_of::<Value>();
        unit.image.patch(at.get(), &(addr as u64).to_ne_bytes())?;
    }
    unit.generation = generation;
    unit.opt = Some(opt);
    let owner = CodeImage::alloc(mutator, PublishedCode::Unit(unit));
    let unit = CodeUnit::of_image(owner).expect("an owner of a unit holds one");
    unit.resume.bind();
    // The baseline image is read now, after the allocation, from the block
    // that keeps it alive.
    let baseline = code_block
        .machine
        .get()
        .ok_or_else(|| io::Error::other("no baseline unit to fall back to"))?;
    refs[0] = Value::from(baseline);
    for (slot, constant) in refs[1..].iter_mut().zip(&constants) {
        *slot = *constant;
    }

    if !flags::jit_stable_entry()
        && let Some(previous) = code_block.optimized.load(Acquire)
    {
        let last = refs.len() - 1;
        refs[last] = Value::from(previous);
    }
    let refs = refs.into_boxed_slice();
    if refs.as_ptr() as usize != refs_base {
        return Err(io::Error::other(
            "the reference box moved before it was owned",
        ));
    }
    // SAFETY: the owner was allocated just above and is unpublished, and the
    // mutation scope is the one it was allocated in.
    unsafe { owner.set_refs(refs) };
    let slot_addr = owner.owner_slot().as_usize() as u64;
    unit.image
        .patch_unpublished(owner_at, &slot_addr.to_ne_bytes())?;

    // Register the unit with each global it folded
    for dependency in &dependencies {
        let (variable, folded) = (dependency.variable, dependency.value);
        let variable = variable.downcast::<crate::runtime::variable::Variable>();
        let set = variable.watch_set(mutator);
        let registered = crate::runtime::watchpoint::WatchpointSet::add(
            set,
            mutator,
            crate::runtime::watchpoint::Dependent::code(code_block, owner),
        );
        std::sync::atomic::fence(AcqRel);
        if !registered || variable.get() != folded {
            return Err(io::Error::other(
                "a global that the unit folded changed during the compile",
            ));
        }
    }

    // Register the unit with each class layout it assumed
    for dependency in &class_dependencies {
        let value = dependency.class;
        let Some(class) = value.try_as::<crate::runtime::objectmodel::class::ClassDescriptor>()
        else {
            return Err(io::Error::other(
                "a class dependency is not a class descriptor",
            ));
        };
        let id = crate::heap::object::ClassId::new(dependency.id)
            .ok_or_else(|| io::Error::other("a class dependency names no class"))?;
        let current = crate::runtime::objectmodel::class::class_table(mutator).lookup(id);
        if current.map(|current| current.as_ptr()) != Some(class.as_ptr())
            || class.layout_stamp() != dependency.stamp
        {
            return Err(io::Error::other(
                "a class that the unit specialized changed during the compile",
            ));
        }
        let set = class.redefinition_wp();

        let registered = crate::runtime::watchpoint::WatchpointSet::add(
            set,
            mutator,
            crate::runtime::watchpoint::Dependent::code(code_block, owner),
        );
        std::sync::atomic::fence(AcqRel);
        if !registered || class.layout_stamp() != dependency.stamp {
            return Err(io::Error::other(
                "a class that the unit specialized was redefined during the compile",
            ));
        }
    }
    barrier::field!(Gc::write(mutator, code_block), CodeBlock, optimized)
        .atomic_swap(Some(owner), Release);
    code_block.entry.publish(unit.entry);
    if unit
        .opt
        .as_ref()
        .is_some_and(|opt| opt.invalid.load(Acquire) != 0)
    {
        if let Some(baseline) = code_block.baseline_entry.entry(Acquire) {
            let _ = code_block.entry.replace(unit.entry, baseline);
        } else {
            code_block.entry.reset_if(unit.entry);
        }
        return Err(io::Error::other(
            "the unit was invalidated while it was installed",
        ));
    }
    if !code_block.commit_opt(generation) {
        if let Some(baseline) = code_block.baseline_entry.entry(Acquire) {
            let _ = code_block.entry.replace(unit.entry, baseline);
        } else {
            code_block.entry.reset_if(unit.entry);
        }
        return Err(io::Error::other("the install was retired while it ran"));
    }

    super::super::image::publish_perf_map_range(
        &crate::runtime::objectmodel::proc::procedure_label(code_block),
        perf_start,
        perf_end,
    );
    if flags::jit_stats() {
        COMPILES.fetch_add(1, Relaxed);
        OPT_INSTALLS.fetch_add(1, Relaxed);
    }
    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "#{id}: installed optimized code for {}",
        crate::runtime::objectmodel::proc::procedure_label(code_block)
    );
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "#{id}: {}: optimized entry at {:#x} ({} bytes)",
        crate::runtime::objectmodel::proc::procedure_label(code_block),
        unit.entry,
        unit.image.code_size()
    );
    Ok(())
}

/// Invalidate `code_block`'s optimized code, whose assumption broke.
pub(crate) fn invalidate_optimized<'gc>(
    code_block: Gc<'gc, CodeBlock<'gc>>,
    owner: Gc<'gc, CodeImage<'gc>>,
) -> Option<super::super::install::Generation> {
    let Some(unit) = CodeUnit::of_image(owner) else {
        return None;
    };
    let Some(opt) = unit.opt.as_ref() else {
        return None;
    };
    if opt.invalid.load(Acquire) != 0 {
        return None;
    }
    opt.invalid.store(1, std::sync::atomic::Ordering::Release);
    let generation = unit.generation;
    let mut retired = None;

    if code_block
        .optimized
        .load(Acquire)
        .map(|current| current.as_ptr())
        == Some(owner.as_ptr())
    {
        match code_block.baseline_entry.entry(Acquire) {
            Some(baseline) => {
                let _ = code_block.entry.replace(unit.entry, baseline);
            }
            None => code_block.entry.reset_if(unit.entry),
        }

        if flags::jit_stable_entry() {
            let _ = code_block
                .optimized
                .compare_exchange(Some(owner), None, AcqRel, Acquire);
        }

        if code_block.retire_opt(generation) {
            retired = Some(generation);
        }
    }
    let base = unit.image.base();
    let word = encode_jump(base + opt.entry_jump, base + opt.reheal)
        .expect("the re-heal stub is within a jump of the entry");
    let mut span = unit.image.span();
    // The one store that retargets the entry. A failure would leave the
    // entry running invalid code, so it is not recoverable.
    code_memory()
        .patch_live_u32(&mut span, opt.entry_jump.get(), word)
        .expect("the entry jump is patchable");
    retired
}

/// A compiled unit awaiting attachment: built by a [`TierCompiler`] on a
/// worker (or the requesting mutator when workers are off), then
/// [`attach`]ed to its code buffer on a mutator thread. An optimized unit
/// carries its [`OptInfo`] and is installed by `install_optimized` instead.
pub(crate) struct BuiltUnit<'gc> {
    pub(crate) tier: Tier,
    pub(crate) loaded: LoadedImage,
    pub(crate) rows: Vec<(BytecodePc, Address)>,
    pub(crate) entry_addr: Address,
    pub(crate) cells: Box<[ErasedCell]>,
    pub(crate) masks: Vec<Box<[u64]>>,
    pub(crate) ranges: Vec<InstructionRange>,
    /// Image offset where the code starts, past the owner word.
    pub(crate) code_start: usize,
    /// LIR/IR comments at machine-code offsets, sorted by offset.
    pub(crate) annotations: Vec<Annotation>,
    pub(crate) data: Vec<DataRange>,
    pub(crate) slows_start_off: usize,
    pub(crate) end_off: usize,
    /// Image offset of the owner word, which [`attach`] patches.
    pub(crate) owner_at: usize,
    /// Tier 2 metadata, `None` for baseline units.
    pub(crate) opt: Option<OptInfo>,
    /// Constant-table values, in reference-slot order. Empty for baseline
    /// units.
    pub(crate) constants: Vec<Value<'gc>>,
    /// Watchpoint assumptions the install registers. Empty for baseline.
    pub(crate) dependencies: Vec<super::super::unit::Dependency<'gc>>,
    /// Class layout assumptions the install registers. Empty for baseline.
    pub(crate) class_dependencies: Vec<super::super::unit::ClassDependency<'gc>>,
}

pub(crate) fn attach<'gc>(
    mutator: MutationContext<'gc>,
    buffer: Gc<'gc, CodeBuffer<'gc>>,
    built: BuiltUnit<'gc>,
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
        code_start,
        annotations,
        data,
        slows_start_off,
        end_off,
        owner_at,
        opt,
        constants,
        dependencies,
        class_dependencies,
    } = built;
    debug_assert!(
        constants.is_empty() && dependencies.is_empty() && class_dependencies.is_empty(),
        "baseline units carry no constant table or watchpoint assumptions"
    );
    let mut unit = CodeUnit::new(
        tier,
        loaded,
        entry_addr,
        ResumeTable::new(rows, cells),
        masks,
        id,
        Address::from_ptr(buffer.entry()),
        Listing {
            code_start,
            ranges,
            annotations,
            data,
            slows_start: slows_start_off,
            code_end: end_off,
        },
    );
    unit.opt = opt;

    // The owner holds the unit. Its slot never moves, and its address goes
    // into the code's header before the code is published.
    let owner = CodeImage::alloc(mutator, PublishedCode::Unit(unit));
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
        .atomic_swap(Some(owner), Release);
    // Publish the resume table before the entry: a resume that observes the
    // entry then always finds the table.
    code_block.baseline_entry.publish(unit.entry);
    code_block.entry.publish(unit.entry);
    if flags::jit_stats() {
        COMPILES.fetch_add(1, Relaxed);
    }
    super::dump::maybe_dump(code_block);
    true
}

/// Mark `code_block` never to compile.
pub(crate) fn poison(code_block: Gc<'_, CodeBlock<'_>>) {
    code_block.entry.poison();
}

/// Take the right to install `code_block`'s attached code, once.
fn claim_install(code_block: Gc<'_, CodeBlock<'_>>) -> bool {
    code_block.commit_baseline(code_block.install.generation())
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
    if code_block.entry.is_decided(Acquire) {
        return true;
    }
    let buffer = code_block.buffer;
    if buffer.build_failed.load(Acquire) {
        poison(code_block);
        return true;
    }
    if buffer.built.is_none() || !claim_install(code_block) {
        return code_block.entry.is_decided(Acquire);
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
    if code_block.entry.is_decided(Acquire) || !claim_compile(code_block) {
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
    code_block.claim_baseline_compile().is_some()
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
    let label = Some(crate::runtime::objectmodel::proc::procedure_label(
        code_block,
    ));
    let request = CompileRequest {
        code: code_block.code(),
        md: Address::from_ptr(buffer.metadata()),
        pool: buffer.pool(),
        label: label.as_deref(),
        code_block,
        closure: None,
        mutation: mutator,
    };
    let mut masm = Masm::new();
    let Some(built) = compiler.build(&request, &mut masm)? else {
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
fn build_image<'gc>(
    masm: &mut Masm,
    code: &[u8],
    md: Address,
    pool: &[u32],
    label: Option<&str>,
) -> std::io::Result<Option<BuiltUnit<'gc>>> {
    let entry = code.as_ptr();
    let constant_pointer = Address::from_ptr(entry);
    let display = label.unwrap_or("<unknown procedure>");

    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "{display}: pool={} words",
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

    masm.reset();
    let paths = declare_slow_paths(masm);
    let start = masm.label();
    masm.bind(start);
    // The chunk header: the owner slot's address, patched at publish.
    let owner = masm.label();
    masm.bind(owner);
    masm.reserve_words(8);

    let redirect = masm.label();
    masm.bind(redirect);
    let mut emission = Emission::default();
    emit_optimized_redirect(masm, &mut emission, redirect);
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
        emit::emit_template(masm, &mut context, op);
    }
    let labels = context.into_targets().into_labels();
    emission.reloc(Reloc::OwnerSlot { at: owner });
    // The deferred slow/taken blocks, in template order: every fast path
    // above falls through to the next instruction.
    let slows_start = masm.label();
    masm.bind(slows_start);
    emission.emit_deferred(masm, paths);
    let end = masm.label();
    masm.bind(end);
    masm.cfi_region(start, end, sabi::SCHEME_ENTRY_CFA_OFFSET as u32);

    let name = format!("jit:{}", display.replace(char::is_whitespace, "_"));
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
            "{display} exceeds the per-block code limit ({bytes} > {block_limit} bytes)"
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
        entry_off,
        owner_at,
    } = emission.link(
        image,
        &Layout {
            labels: &labels,
            pcs: &pcs,
            slows_start,
            end,
            entry: Some(redirect),
        },
    )?;
    let base = loaded.base();

    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "{display}: sites={}",
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
    let entry_addr = base + entry_off.expect("a baseline unit names its entry");

    log::info!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "{display}: loaded {bytes} bytes"
    );
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        "{display}: base={base}, entry={entry_addr}"
    );
    if log::log_enabled!(target: crate::heap::logging::JIT_LOG_TARGET, log::Level::Debug) {
        let mut sorted = rows.clone();
        sorted.sort();
        log::debug!(target: crate::heap::logging::JIT_LOG_TARGET, "{display}: labels={sorted:x?}")
    }

    Ok(Some(BuiltUnit {
        tier: Tier::Baseline,
        loaded,
        rows,
        entry_addr,
        cells,
        masks,
        code_start: ranges.first().map_or(0, |range| range.offset),
        ranges,
        annotations: Vec::new(),
        data,
        slows_start_off,
        end_off,
        owner_at,
        opt: None,
        constants: Vec::new(),
        dependencies: Vec::new(),
        class_dependencies: Vec::new(),
    }))
}
