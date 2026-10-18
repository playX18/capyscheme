//! The compiled template context: immediates, labels, and branch emitters.
//!
//! [`CompiledContext`] is the [`TemplateContext`](super::super::template::TemplateContext)
//! for code emitted from one decoded instruction at a time: the driver
//! builds it once, positions it at each instruction in turn, and emits that
//! instruction's template through it. Operands are decoded once, at compile
//! time: frame slots become `[SFP + 8r]` addresses, constants and metadata
//! become absolute addresses, and control becomes jumps to targets.
//!
//! The contexts differ in only two respects, each a type parameter:
//!
//! - [`Targets`]: how a bytecode pc becomes a jump target and where slow
//!   blocks go. [`ProcTargets`] maps pcs to the labels of one procedure
//!   image and defers its slow blocks (the baseline JIT), [`OriginExits`]
//!   resolves them through the origin image's resume table to absolute
//!   addresses and emits inline (arithmetic stubs).
//! - [`SpecSource`]: where an arithmetic site's [`ArithSpec`] comes from.
//!   [`ProfileSpec`] reads the live profile, [`FixedSpec`] replays the
//!   shape a stub is being generated for.
//!
//! Comparisons against two constants fold away ([`holds`]), the rest emit
//! register-register or register-immediate branches.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering::Relaxed};

use bytecodegen::OperandWidth;
use onlineasm::{Label, VReg, onlineasm};

use super::super::addr::imm;
use super::super::emission::{Deferred, Emission};
use super::super::lazy::Lazies;
use super::super::lazy::edge::{EdgeKind, emit_hit_block};
use super::super::masm::Masm;
#[cfg(target_arch = "aarch64")]
use super::super::reloc::{Reloc, masm_move_patchable};
use super::super::stubs;
use super::super::template::{
    ArithShape, ArithSpec, Condition, Flow, OpArg, OpResult, Operand, Operands, OperationCall,
    Placement, SiteLink, SiteOperand, Slow, SlowPath, SlowPaths, Speculation, Target, Targets,
    emit_branch_to, emit_jump_to, emit_staged_control, metadata_entry_size, metadata_field_offset,
    metadata_id_operand, tmp,
};
use super::super::template_image::interpreter_enter_if_loaded;
use super::arith_site::ArithSite;
use super::config::template_interpreter;
use super::entry::emit_enter_callee;
use super::registry::{ResumeTable, origin_exit_addr};
use crate::bytecode::def::OpcodeId;
use crate::runtime::interp::stack::Ip;
use crate::runtime::sabi::{self, regs};
use mmtk::util::Address;

/// One instruction's decoded operands, shared by the JIT and arithmetic
/// stub contexts: both decode once, at emission time, from stable
/// bytecode and metadata addresses.
pub(crate) struct DecodedInstruction {
    pub(crate) op: OpcodeId,
    pub(crate) width: OperandWidth,
    pub(crate) ip: Ip,
    pub(crate) len: usize,
    pub(crate) entry: Ip,
    pub(crate) constant_pointer: usize,
    pub(crate) md: usize,
}

impl DecodedInstruction {
    pub(crate) fn pc(&self) -> usize {
        self.ip as usize - self.entry as usize
    }

    /// Address of metadata `field` of kind `kind` for this instruction.
    pub(crate) fn metadata_addr(&self, kind: usize, field: usize) -> usize {
        let id = self.raw(metadata_id_operand(self.op), false) as usize;
        // SAFETY: the metadata area starts with one u32 offset per kind,
        // and the id names this instruction's entry.
        let offset = unsafe { *((self.md + 4 * kind) as *const u32) } as usize;
        self.md + offset + id * metadata_entry_size(self.op) + metadata_field_offset(self.op, field)
    }

    /// Operand `field`, decoded now.
    pub(crate) fn raw(&self, field: usize, signed: bool) -> i64 {
        let header = match self.width {
            OperandWidth::Narrow => 1,
            OperandWidth::Wide16 => 3,
            OperandWidth::Wide32 => 5,
        };
        let size = self.width.bytes();
        // SAFETY: the operand lies inside the instruction.
        let bytes = unsafe { std::slice::from_raw_parts(self.ip.add(header + field * size), size) };
        let mut buf = [0u8; 8];
        buf[..size].copy_from_slice(bytes);
        let value = u64::from_le_bytes(buf);
        if signed {
            let shift = 64 - 8 * size as u32;
            ((value << shift) as i64) >> shift
        } else {
            value as i64
        }
    }

    pub(crate) fn slot(&self, masm: &mut Masm, field: usize) -> onlineasm::Mem {
        let r = self.raw(field, false) as i32;
        masm.assembler().mem(regs::SFP, 8 * r)
    }

    /// `r[field]` -> `dst`.
    pub(crate) fn load_register(&self, masm: &mut Masm, field: usize, dst: VReg) {
        let slot = self.slot(masm, field);
        onlineasm!(*masm.assembler(); loadq {dst}, {slot};);
    }

    /// `src` -> `r[field]`.
    pub(crate) fn store_register(&self, masm: &mut Masm, field: usize, src: VReg) {
        let slot = self.slot(masm, field);
        onlineasm!(*masm.assembler(); storeq {slot}, {src};);
    }

    /// The raw operand value: a register index, count, immediate or
    /// displacement.
    pub(crate) fn operand(&self, field: usize, signed: bool) -> Operand {
        Operand::Immediate(self.raw(field, signed))
    }

    /// Constant `field` -> `dst`.
    pub(crate) fn constant(&self, masm: &mut Masm, field: usize, dst: VReg) {
        // Constant `i` is `constant_pointer[-3 - i]`, the slot is read, because the
        // collector updates it.
        let index = self.raw(field, false) as usize;
        let addr = self.constant_pointer - 8 * (3 + index);
        // On x86-64 a slot in the low 2 GiB folds into the access (one
        // absolute `[disp32]` load instead of `movabs` plus a dependent
        // load). AArch64 has no absolute addressing mode: its backend
        // would materialize the address into a scratch register anyway,
        // so it keeps the unfolded form.
        #[cfg(target_arch = "x86_64")]
        if let Ok(disp) = i32::try_from(addr) {
            let slot = onlineasm::Mem::absolute(onlineasm::Disp::Const(disp));
            onlineasm!(*masm.assembler();
                loadq {dst}, {slot};
            );
            return;
        }
        let slot = masm.assembler().mem(dst, 0);
        onlineasm!(*masm.assembler();
            move {dst}, {addr as i64};
            loadq {dst}, {slot};
        );
    }

    /// Address of metadata `field` of kind `kind` -> `dst`.
    pub(crate) fn metadata(&self, masm: &mut Masm, kind: usize, field: usize, dst: VReg) {
        let addr = self.metadata_addr(kind, field);
        onlineasm!(*masm.assembler(); move {dst}, {addr as i64};);
    }
}

/// The baseline procedure's targets: every landing pc has a label in the
/// image under construction, slow blocks are deferred to the end of the
/// code.
pub(crate) struct ProcTargets {
    labels: HashMap<usize, Label>,
    /// The next instruction's label is bound immediately after this
    /// template, with no return-point header in between, and this
    /// opcode's template emits nothing after `next`: `next` falls
    /// through and emits nothing. Set by the compiler (see
    /// `next_may_elide`): most templates bind slow blocks after `next`,
    /// and for those the jump is the terminator that skips them.
    fallthrough: bool,
}

impl ProcTargets {
    pub(crate) fn new(labels: HashMap<usize, Label>, fallthrough: bool) -> Self {
        Self {
            labels,
            fallthrough,
        }
    }

    /// The label of the instruction at `pc`. The driver uses it for the
    /// return-point headers it emits itself, templates use it through
    /// `next`/`jump`/`branch_if`.
    pub(crate) fn label_at(&self, pc: usize) -> Label {
        *self
            .labels
            .get(&pc)
            .unwrap_or_else(|| panic!("no instruction at {pc}"))
    }

    /// The per-pc labels, for the driver to link against.
    pub(crate) fn into_labels(self) -> HashMap<usize, Label> {
        self.labels
    }
}

impl Targets for ProcTargets {
    fn target(&mut self, _em: &mut Emission, _masm: &mut Masm, pc: usize) -> Target {
        Target::Label(self.label_at(pc))
    }

    fn falls_through(&self) -> bool {
        self.fallthrough
    }

    fn placement(&self) -> Placement {
        Placement::Deferred
    }
}

/// An arithmetic stub's targets: every exit is an address in the origin
/// image, resolved through its resume table (the origin is loaded by the
/// time a stub is generated). Slow blocks are emitted inline.
pub(crate) struct OriginExits<'t> {
    pub(crate) origin_table: &'t ResumeTable,
}

impl Targets for OriginExits<'_> {
    fn target(&mut self, _em: &mut Emission, _masm: &mut Masm, pc: usize) -> Target {
        Target::Absolute(origin_exit_addr(self.origin_table, pc))
    }

    fn placement(&self) -> Placement {
        Placement::Inline
    }
}

/// Where an arithmetic site's [`ArithSpec`] comes from.
pub(crate) trait SpecSource {
    /// The spec of the site in metadata `field` of `kind` of `insn`.
    fn arith_site(&mut self, insn: &DecodedInstruction, kind: usize, field: usize) -> ArithSpec;
}

/// The baseline JIT's source: the shape is selected from the site's live
/// `ArithProfile`, and slow edges link to a live site.
pub(crate) struct ProfileSpec;

impl SpecSource for ProfileSpec {
    fn arith_site(&mut self, insn: &DecodedInstruction, kind: usize, field: usize) -> ArithSpec {
        if insn.md == 0 {
            // No metadata area, so no site asked: emit everything.

            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "arith pc={} op={:?} shape=Full (no metadata)",
                insn.pc(),
                insn.op,
            );

            return ArithSpec::full();
        }
        let addr = insn.metadata_addr(kind, field);
        // SAFETY: `addr` is this site's live profile cell: code buffers
        // never move, the procedure is alive (it is being compiled), and
        // the cell is an `AtomicU32`, so a relaxed load races safely with
        // executing threads (a stale shape only costs a slow path).
        let word = unsafe { &*(addr as *const AtomicU32) }.load(Relaxed);
        let shape = ArithShape::from_profile(word);

        log::info!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "arith pc={} op={:?} profile={word:#x} shape={shape:?}",
            insn.pc(),
            insn.op,
        );

        ArithSpec {
            shape,
            profile_cell: Some(addr),
            link: SiteLink::Live,
        }
    }
}

/// An arithmetic stub's source: the shape the driver decided from the
/// fresh profile. A specialized stub's slow blocks still hit its `cell`
/// (the address of the lazy cell being materialized), a full stub's are
/// terminal (`cell` is `None`).
pub(crate) struct FixedSpec {
    pub(crate) shape: ArithShape,
    pub(crate) cell: Option<Address>,
}

impl SpecSource for FixedSpec {
    fn arith_site(&mut self, _insn: &DecodedInstruction, _kind: usize, _field: usize) -> ArithSpec {
        ArithSpec {
            shape: self.shape,
            profile_cell: None,
            link: self
                .cell
                .map_or(SiteLink::None, |cell| SiteLink::Resident(cell.as_usize())),
        }
    }
}

/// The compiled template context: the current instruction, a [`Targets`]
/// resolving pcs, a [`SpecSource`] for arithmetic sites, and the
/// [`Emission`] it records deferred blocks and relocations into. The
/// driver positions it at each instruction ([`set_instruction`]), emits
/// the template, and takes the targets back with [`into_targets`].
///
/// [`set_instruction`]: CompiledContext::set_instruction
/// [`into_targets`]: CompiledContext::into_targets
pub(crate) struct CompiledContext<'e, T: Targets, S: SpecSource> {
    insn: DecodedInstruction,
    targets: T,
    spec: S,
    em: &'e mut Emission,
    paths: SlowPaths,
}

/// The baseline JIT context.
pub(crate) type JitTemplateContext<'e> = CompiledContext<'e, ProcTargets, ProfileSpec>;

/// An arithmetic stub's context.
pub(crate) type StubContext<'e, 't> = CompiledContext<'e, OriginExits<'t>, FixedSpec>;

impl<'e, T: Targets, S: SpecSource> CompiledContext<'e, T, S> {
    /// A context positioned at `insn`, recording into `em`.
    pub(crate) fn new(
        insn: DecodedInstruction,
        targets: T,
        spec: S,
        em: &'e mut Emission,
        paths: SlowPaths,
    ) -> Self {
        Self {
            insn,
            targets,
            spec,
            em,
            paths,
        }
    }

    /// The targets, at the end of emission.
    pub(crate) fn into_targets(self) -> T {
        self.targets
    }

    /// The target of the instruction at `pc`.
    fn target_of(&mut self, masm: &mut Masm, pc: usize) -> Target {
        self.targets.target(&mut *self.em, masm, pc)
    }

    /// The pc of the jump target of `field`, decoded now.
    fn jump_pc(&self, field: usize) -> usize {
        (self.insn.pc() as i64 + self.insn.raw(field, true)) as usize
    }

    /// Jump to the instruction at `pc`.
    fn jump_to_pc(&mut self, masm: &mut Masm, pc: usize) {
        let target = self.target_of(masm, pc);
        emit_jump_to(masm, target);
    }

    /// A block that jumps to `target`, bound at `label`: deferred to the
    /// end of the code or emitted right here, as the targets place it.
    fn jump_block(&mut self, masm: &mut Masm, label: Label, target: Target) {
        match self.targets.placement() {
            Placement::Inline => {
                masm.bind(label);
                emit_jump_to(masm, target);
            }
            Placement::Deferred => match target {
                Target::Label(target) | Target::Edge(target) => {
                    self.em.defer(label, Deferred::Jump { target })
                }
                Target::Absolute(addr) => self.em.defer(label, Deferred::JumpAbsolute { addr }),
            },
        }
    }

    /// This instruction's arithmetic site, as the lazy value of an edge.
    /// `path` selects the slow body the site runs on a hit.
    fn arith_site_value(&self, spec: &ArithSpec, path: SlowPath) -> ArithSite {
        log::debug!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "site #{} pc={} op={:?} shape={:?}",
            self.em.cells.len(),
            self.insn.pc(),
            self.insn.op,
            spec.shape,
        );

        ArithSite::new(
            &self.insn,
            spec.profile_cell
                .expect("an arithmetic site read its profile cell"),
            matches!(path, SlowPath::CompareJump),
        )
    }

    /// Declare this instruction's arithmetic site as a lazy edge bound at
    /// `edge`: the guards keep targeting `edge`, whose indirect jump the
    /// first miss retargets at the generated stub.
    fn push_site(&mut self, masm: &mut Masm, edge: Label, spec: &ArithSpec, path: SlowPath) {
        debug_assert_eq!(self.targets.placement(), Placement::Deferred);
        let site = self.arith_site_value(spec, path);
        self.em.edge_at(masm, edge, EdgeKind::Indirect, site);
    }

    /// Continue in the interpreter at `PC`.
    fn dispatch_interpreter(masm: &mut Masm) {
        let (t5, t7, pc) = (onlineasm::regs::t5, onlineasm::regs::t7, regs::PC);
        let slot = masm.assembler().mem_indexed(t7, t5, 8, 0);
        onlineasm!(*masm.assembler();
            loadb {t5}, [{pc}];
            move {t7}, {template_interpreter().map as i64};
            jmp {slot};
        );
    }

    /// Resume an interpreted caller inline at the bytecode return point in
    /// `fp[-1]`, any other return address goes to `slow` in `tmp::T0`.
    fn resume_interpreted(masm: &mut Masm, slow: Label) {
        let (sfp, pc) = (regs::SFP, regs::PC);
        let ra = masm.assembler().mem(sfp, 0);
        let resume = masm.assembler().mem(sfp, -8);
        onlineasm!(*masm.assembler();
            loadq {tmp::T0}, {ra};
            move {tmp::T1}, {template_interpreter().interpreter_return as i64};
            bqneq {tmp::T0}, {tmp::T1}, {slow};
            loadp {pc}, {resume};
        );
        Self::dispatch_interpreter(masm);
    }

    /// The aux pool of the procedure under compile.
    fn switch_pool(&self) -> *const u32 {
        // SAFETY: as in `compile`: `constant_pointer - 16` is the pool pointer of a live
        // code buffer.
        unsafe { *((self.insn.constant_pointer - 16) as *const *const u32) }
    }

    /// The int switch table of operand `table`, read now: the default pc
    /// plus the cases (sparse) or the span (dense), as pcs. Every target
    /// was verified a landing pc, so each has a target.
    fn read_int_table(&self, table: usize) -> (usize, JitIntTable) {
        use crate::bytecode::def::{switch_table_is_dense, switch_table_len};
        let at = self.insn.raw(table, false) as usize;
        let pool = self.switch_pool();
        // SAFETY: the table lies inside the verified pool.
        unsafe {
            let default = pool.add(at).read() as i32;
            let count_word = pool.add(at + 1).read();
            let pc = self.insn.pc() as i64;
            let default_pc = (pc + default as i64) as usize;
            if switch_table_is_dense(count_word) {
                let span = switch_table_len(count_word);
                let min = pool.add(at + 2).read() as i32;
                let mut targets = Vec::with_capacity(span);
                for entry in 0..span {
                    let disp = pool.add(at + 3 + entry).read() as i32;
                    targets.push((pc + disp as i64) as usize);
                }
                (
                    default_pc,
                    JitIntTable::Dense {
                        min: min as i64,
                        targets,
                    },
                )
            } else {
                let count = count_word as usize;
                let mut cases = Vec::with_capacity(count);
                for case in 0..count {
                    let key = pool.add(at + 2 + 2 * case).read() as i32;
                    let disp = pool.add(at + 2 + 2 * case + 1).read() as i32;
                    cases.push((key as i64, (pc + disp as i64) as usize));
                }
                (default_pc, JitIntTable::Sparse { cases })
            }
        }
    }

    /// The symbol switch table of operand `table`, read now: the default
    /// pc, the hash mask, and the `(masked hash, pc)` buckets.
    fn read_sym_table(&self, table: usize) -> (usize, u64, Vec<(u64, usize)>) {
        let at = self.insn.raw(table, false) as usize;
        let pool = self.switch_pool();
        // SAFETY: the table lies inside the verified pool.
        unsafe {
            let default = pool.add(at).read() as i32;
            let count = pool.add(at + 1).read() as usize;
            let mask = pool.add(at + 2).read() as u64 | (pool.add(at + 3).read() as u64) << 32;
            let pc = self.insn.pc() as i64;
            let default_pc = (pc + default as i64) as usize;
            let mut cases = Vec::with_capacity(count);
            for case in 0..count {
                let key = pool.add(at + 4 + 3 * case).read() as u64
                    | (pool.add(at + 4 + 3 * case + 1).read() as u64) << 32;
                let disp = pool.add(at + 4 + 3 * case + 2).read() as i32;
                cases.push((key, (pc + disp as i64) as usize));
            }
            (default_pc, mask, cases)
        }
    }

    /// A linear `bieq` chain over at most a few int cases, then the
    /// default. Every path jumps, hits are collected for binding by the
    /// caller.
    fn emit_int_chain(
        &mut self,
        masm: &mut Masm,
        cases: &[(i64, usize)],
        default_pc: usize,
        hits: &mut Vec<(Label, usize)>,
    ) {
        for (key, pc) in cases {
            let hit = masm.label();
            onlineasm!(*masm.assembler();
                bieq {tmp::T2}, {*key}, {hit};
            );
            hits.push((hit, *pc));
        }
        self.jump_to_pc(masm, default_pc);
    }

    /// A binary `bieq`/`bilt` tree over sorted int cases. Empty leaves
    /// jump to `miss`, hits are collected for binding by the caller.
    fn emit_int_tree(
        &self,
        masm: &mut Masm,
        cases: &[(i64, usize)],
        miss: Label,
        hits: &mut Vec<(Label, usize)>,
    ) {
        match cases.len() {
            0 => {
                onlineasm!(*masm.assembler(); jmp {miss};);
            }
            1 => {
                let hit = masm.label();
                onlineasm!(*masm.assembler();
                    bieq {tmp::T2}, {cases[0].0}, {hit};
                    jmp {miss};
                );
                hits.push((hit, cases[0].1));
            }
            _ => {
                let mid = cases.len() / 2;
                let (hit, left) = (masm.label(), masm.label());
                onlineasm!(*masm.assembler();
                    bieq {tmp::T2}, {cases[mid].0}, {hit};
                    bilt {tmp::T2}, {cases[mid].0}, {left};
                );
                hits.push((hit, cases[mid].1));
                self.emit_int_tree(masm, &cases[mid + 1..], miss, hits);
                masm.bind(left);
                self.emit_int_tree(masm, &cases[..mid], miss, hits);
            }
        }
    }

    /// A linear `bqeq` chain over at most a few symbol buckets.
    fn emit_sym_chain(
        &mut self,
        masm: &mut Masm,
        cases: &[(u64, usize)],
        default_pc: usize,
        hits: &mut Vec<(Label, usize)>,
    ) {
        for (key, pc) in cases {
            let hit = masm.label();
            let key = *key as i64;
            onlineasm!(*masm.assembler();
                bqeq {tmp::T2}, {key}, {hit};
            );
            hits.push((hit, *pc));
        }
        self.jump_to_pc(masm, default_pc);
    }

    /// A binary `bqeq`/`bqb` (unsigned) tree over sorted symbol buckets.
    fn emit_sym_tree(
        &self,
        masm: &mut Masm,
        cases: &[(u64, usize)],
        miss: Label,
        hits: &mut Vec<(Label, usize)>,
    ) {
        match cases.len() {
            0 => {
                onlineasm!(*masm.assembler(); jmp {miss};);
            }
            1 => {
                let hit = masm.label();
                let key = cases[0].0 as i64;
                onlineasm!(*masm.assembler();
                    bqeq {tmp::T2}, {key}, {hit};
                    jmp {miss};
                );
                hits.push((hit, cases[0].1));
            }
            _ => {
                let mid = cases.len() / 2;
                let key = cases[mid].0 as i64;
                let (hit, left) = (masm.label(), masm.label());
                onlineasm!(*masm.assembler();
                    bqeq {tmp::T2}, {key}, {hit};
                    bqb {tmp::T2}, {key}, {left};
                );
                hits.push((hit, cases[mid].1));
                self.emit_sym_tree(masm, &cases[mid + 1..], miss, hits);
                masm.bind(left);
                self.emit_sym_tree(masm, &cases[..mid], miss, hits);
            }
        }
    }

    /// Bind each collected hit and jump to its pc.
    fn bind_hits(&mut self, masm: &mut Masm, hits: Vec<(Label, usize)>) {
        for (hit, pc) in hits {
            masm.bind(hit);
            self.jump_to_pc(masm, pc);
        }
    }
}

impl<'e> CompiledContext<'e, ProcTargets, ProfileSpec> {
    /// The label of the instruction at `pc`, for the return-point headers
    /// the driver emits itself.
    pub(crate) fn label_at(&self, pc: usize) -> Label {
        self.targets.label_at(pc)
    }

    /// Position the context at `insn`: replaces the current decode and
    /// whether its `next` falls through.
    pub(crate) fn set_instruction(&mut self, insn: DecodedInstruction, fallthrough: bool) {
        self.insn = insn;
        self.targets.fallthrough = fallthrough;
    }
}

/// An int switch table read at JIT-compile time, with displacements
/// resolved to pcs.
enum JitIntTable {
    Sparse { cases: Vec<(i64, usize)> },
    Dense { min: i64, targets: Vec<usize> },
}

impl<T: Targets, S: SpecSource> Operands for CompiledContext<'_, T, S> {
    fn load_register(&mut self, masm: &mut Masm, field: usize, dst: VReg) {
        self.insn.load_register(masm, field, dst);
    }

    fn store_register(&mut self, masm: &mut Masm, field: usize, src: VReg, _tmp: VReg) {
        self.insn.store_register(masm, field, src);
    }

    fn operand(&mut self, _m: &mut Masm, field: usize, signed: bool) -> Operand {
        self.insn.operand(field, signed)
    }

    fn constant(&mut self, masm: &mut Masm, field: usize, dst: VReg) {
        self.insn.constant(masm, field, dst);
    }

    fn metadata(&mut self, masm: &mut Masm, kind: usize, field: usize, dst: VReg, _tmp: VReg) {
        self.insn.metadata(masm, kind, field, dst);
    }

    fn machine_entry(&mut self, masm: &mut Masm, dst: VReg) {
        let enter =
            interpreter_enter_if_loaded().expect("the JIT runs on the template interpreter") as i64;
        onlineasm!(*masm.assembler(); move {dst}, {enter};);
    }
}

impl<T: Targets, S: SpecSource> Flow for CompiledContext<'_, T, S> {
    fn next(&mut self, masm: &mut Masm) {
        // The assembler always emits `jump`, even to the immediately
        // following instruction (at emission time the target is an
        // unbound forward label, so it cannot know): fall through
        // instead when the compiler proves the next label binds here
        // with nothing in between. The elided jump is never patchable
        // (stub trampolines live in the deferred slows), and skipping
        // it changes no label offsets, only code size.
        let next = self.target_of(masm, self.insn.pc() + self.insn.len);
        if self.targets.falls_through() {
            let point = masm.label();
            masm.bind(point);
            let (Target::Label(next) | Target::Edge(next)) = next else {
                unreachable!("only a label can be fallen through to");
            };
            self.em.fallthrough_checks.push((point, next));
            return;
        }
        emit_jump_to(masm, next);
    }

    fn jump(&mut self, masm: &mut Masm, field: usize) {
        let pc = self.jump_pc(field);
        self.jump_to_pc(masm, pc);
    }

    fn branch_if(
        &mut self,
        masm: &mut Masm,
        cond: Condition,
        a: Operand,
        b: Operand,
        field: usize,
    ) {
        let taken = masm.label();
        if !emit_branch_to(masm, cond, a, b, taken) {
            // Never taken: no taken block to defer.
            self.next(masm);
            return;
        }
        // The taken block goes to the end of the code, so the not-taken
        // path falls through whenever the layout allows.
        self.next(masm);
        let pc = self.jump_pc(field);
        let target = self.target_of(masm, pc);
        self.jump_block(masm, taken, target);
    }

    fn call_return_point(&mut self, masm: &mut Masm, frame: VReg) {
        // The return address is the return point's target, which has a
        // header, `frame[-1]` gets the bytecode return point as well, which
        // only the interpreter's glue reads, but which keeps the slot raw.
        let next = self.insn.pc() + self.insn.len;
        let target = self.target_of(masm, next);
        let resume = masm.assembler().mem(frame, -8);
        let ra = masm.assembler().mem(frame, 0);
        onlineasm!(*masm.assembler();
            move {tmp::T0}, {self.insn.entry as i64 + next as i64};
            storep {resume}, {tmp::T0};
        );
        match target {
            Target::Label(label) | Target::Edge(label) => {
                // onlineasm's `leap` of a label is `adrp`/`add` on AArch64,
                // resolved as if the image started a page, this image need
                // not.
                #[cfg(target_arch = "aarch64")]
                {
                    let at = masm_move_patchable(masm, tmp::T0);
                    self.em.reloc(Reloc::LabelAddr { at, target: label });
                }
                #[cfg(not(target_arch = "aarch64"))]
                onlineasm!(*masm.assembler(); leap {tmp::T0}, {label};);
            }
            Target::Absolute(addr) => {
                let addr = imm(addr);
                onlineasm!(*masm.assembler(); move {tmp::T0}, {addr};);
            }
        }
        onlineasm!(*masm.assembler(); storep {ra}, {tmp::T0};);
    }

    fn enter_callee(&mut self, masm: &mut Masm) {
        emit_enter_callee(masm);
    }

    fn resume_caller(&mut self, masm: &mut Masm, slow: Label) {
        // One value returns through the return address alone: an interpreted
        // caller's is `interpreter_return`, which loads `fp[-1]` and
        // dispatches, so compiled callers skip the interpreter test.
        let _ = slow;
        let ra = masm.assembler().mem(regs::SFP, 0);
        onlineasm!(*masm.assembler(); jmp {ra};);
    }

    fn resume_caller_mv(&mut self, masm: &mut Masm, slow: Label) {
        // Multiple values must tell the callers apart: a compiled one
        // resumes at its `%mv-jump` target, not its return address.
        let compiled = masm.label();
        Self::resume_interpreted(masm, compiled);
        masm.bind(compiled);
        masm.emit_mv_jump(tmp::T0, tmp::T1, tmp::T2, slow);
    }

    fn reload_code(&mut self, masm: &mut Masm) {
        // `CONSTANT_POINTER`/`MD` are not live in compiled code: metadata
        // folds to immediates and slow paths stage `MD` themselves.
        let _ = masm;
    }

    fn nop_native_check(&mut self, masm: &mut Masm, slow: Label) -> bool {
        // Compiled code is never a native entry (only bytecode compiles, and
        // `PC`/`CONSTANT_POINTER` are not live here anyway), so there is no check and no
        // slow block.
        let _ = (masm, slow);
        false
    }

    fn switch_int(&mut self, masm: &mut Masm, src: usize, table: usize, is_char: bool) {
        self.load_register(masm, src, tmp::T1);
        let fallthrough = masm.label();
        masm.switch_int_key(tmp::T1, tmp::T2, tmp::T0, fallthrough, is_char);
        // The table is read now, every path below jumps to a target.
        let (default_pc, mut parsed) = self.read_int_table(table);
        // A jump table holds labels of this image, so only deferred
        // placement keeps one: an inline context searches the same cases
        // instead (a dense table lists them in key order).
        if self.targets.placement() == Placement::Inline {
            if let JitIntTable::Dense { min, targets } = parsed {
                let cases = targets
                    .into_iter()
                    .enumerate()
                    .map(|(index, pc)| (min + index as i64, pc))
                    .collect();
                parsed = JitIntTable::Sparse { cases };
            }
        }
        let mut hits = Vec::new();
        match parsed {
            JitIntTable::Sparse { cases } if cases.len() <= 4 => {
                self.emit_int_chain(masm, &cases, default_pc, &mut hits);
            }
            JitIntTable::Sparse { cases } => {
                let miss = masm.label();
                self.emit_int_tree(masm, &cases, miss, &mut hits);
                masm.bind(miss);
                self.jump_to_pc(masm, default_pc);
            }
            JitIntTable::Dense { min, targets } => {
                let span = targets.len() as i64;
                let miss = masm.label();
                let table_label = masm.label();
                onlineasm!(*masm.assembler();
                    subq {tmp::T2}, {tmp::T2}, {min};
                    bqaeq {tmp::T2}, {span}, {miss};
                );
                // The table base: RIP-relative on x86-64, on AArch64 a
                // label address would assume a page start, so a patched
                // immediate like `call_return_point`.
                #[cfg(target_arch = "aarch64")]
                {
                    let at = masm_move_patchable(masm, tmp::T0);
                    self.em.reloc(Reloc::LabelAddr {
                        at,
                        target: table_label,
                    });
                }
                #[cfg(not(target_arch = "aarch64"))]
                onlineasm!(*masm.assembler(); leap {tmp::T0}, {table_label};);
                let slot = masm.assembler().mem_indexed(tmp::T0, tmp::T2, 8, 0);
                onlineasm!(*masm.assembler();
                    jmp {slot};
                );
                masm.bind(miss);
                self.jump_to_pc(masm, default_pc);
                self.em.defer(table_label, Deferred::Table { targets });
            }
        }
        self.bind_hits(masm, hits);
        masm.bind(fallthrough);
        self.next(masm);
    }

    fn switch_sym(&mut self, masm: &mut Masm, src: usize, table: usize) {
        self.load_register(masm, src, tmp::T1);
        let fallthrough = masm.label();
        masm.switch_sym_key(tmp::T1, tmp::T2, tmp::T0, fallthrough);
        let (default_pc, mask, cases) = self.read_sym_table(table);
        let mask = mask as i64;
        // The key prolog above already loaded the hash into `T2`.
        onlineasm!(*masm.assembler();
            andq {tmp::T2}, {tmp::T2}, {mask};
        );
        let mut hits = Vec::new();
        if cases.len() <= 4 {
            self.emit_sym_chain(masm, &cases, default_pc, &mut hits);
        } else {
            let miss = masm.label();
            self.emit_sym_tree(masm, &cases, miss, &mut hits);
            masm.bind(miss);
            self.jump_to_pc(masm, default_pc);
        }
        self.bind_hits(masm, hits);
        masm.bind(fallthrough);
        self.next(masm);
    }
}

impl<T: Targets, S: SpecSource> Slow for CompiledContext<'_, T, S> {
    fn prim_direct(&mut self, masm: &mut Masm, id: i64, args: &[usize], dst: usize) {
        // `AC` round-trips through its slot: the call clobbers
        // caller-saved registers (`a2` is `AC` itself), and both other
        // engines preserve `AC` across a prim. `CP`/`PC` double as the
        // fourth/fifth argument registers, both are dead across a prim
        // in JIT code. The wrapper is this prim's own: no id travels,
        // no dispatch follows.
        let (tc, ac) = (regs::TC, regs::AC);
        let ac_slot = masm.assembler().mem(tc, sabi::TC_AC as i32);
        let (a0, a1, a2, a3, a4) = stubs::arg_regs5();
        // Only `a2..=a4` carry prim arguments; more would be dropped silently.
        assert!(
            args.len() <= 3,
            "jit: prim call with {} arguments",
            args.len()
        );
        let argc = args.len() as i64;
        onlineasm!(*masm.assembler();
            storeq {ac_slot}, {ac};
            move {a0}, {tc};
            move {a1}, {argc};
        );
        for (field, register) in args.iter().zip([a2, a3, a4]) {
            self.insn.load_register(masm, *field, register);
        }
        let target = masm.declare_prim_fn(id as u16);
        masm.call_rust(target);
        let (r0, r1) = stubs::ret_regs();
        let error = masm.label();
        onlineasm!(*masm.assembler();
            bqneq {r1}, 0, {error};
        );
        self.insn.store_register(masm, dst, r0);
        onlineasm!(*masm.assembler();
            loadq {ac}, {ac_slot};
        );
        self.next(masm);
        self.slow_control(masm, error, SlowPath::PrimError(r1));
    }

    fn call_operation(&mut self, masm: &mut Masm, slow: Label, mut call: OperationCall) {
        // Deferred like `slow_control`: the drain binds the label and
        // emits the call. Fields resolve now, against this instruction.
        for arg in &mut call.args {
            *arg = match *arg {
                OpArg::Field { field, signed } => OpArg::Imm(self.insn.raw(field, signed)),
                OpArg::Slot(field) => OpArg::FrameSlot(self.insn.raw(field, false) as i32),
                other => other,
            };
        }
        if let OpResult::Slot(field) = call.result {
            call.result = OpResult::FrameSlot(self.insn.raw(field, false) as i32);
        }
        if let OperationCall {
            target: super::super::operation::OpTarget::PrimIndexed(_),
            ..
        } = call
        {
            unreachable!("the JIT decodes prim ids statically");
        }
        let ip = self.insn.ip;
        self.em.defer(slow, Deferred::Operation { call, ip });
        if self.targets.placement() == Placement::Inline {
            // Nothing else is pending in an inline context: drain it here.
            self.em.emit_deferred(masm, self.paths);
        }
    }

    fn control(&mut self, masm: &mut Masm, path: SlowPath) {
        emit_staged_control(masm, &self.paths, path, self.insn.ip, self.insn.md);
    }

    fn slow_control(&mut self, masm: &mut Masm, label: Label, path: SlowPath) {
        match self.targets.placement() {
            // Deferred: the drain binds the label and stages the captured
            // instruction values. Emits nothing here.
            Placement::Deferred => self.em.defer(
                label,
                Deferred::Control {
                    path,
                    ip: self.insn.ip,
                    md: self.insn.md,
                },
            ),
            // Inline: the stub is one straight-line region with no deferral.
            Placement::Inline => {
                masm.bind(label);
                self.control(masm, path);
            }
        }
    }

    fn slow_jump(&mut self, masm: &mut Masm, label: Label, field: usize) {
        // The target decodes now, against this instruction.
        let pc = self.jump_pc(field);
        let target = self.target_of(masm, pc);
        self.jump_block(masm, label, target);
    }

    fn slow_entry(&mut self, masm: &mut Masm, spec: &ArithSpec, path: SlowPath) {
        if spec.link != SiteLink::Live {
            // No live site to enter: if a stub ever has no inline leaves
            // it is one terminal slow call.
            self.control(masm, path);
            return;
        }
        // A lone inline jump to a live slow edge: the site's first miss
        // specializes it. Terminal, like `control`.
        debug_assert!(
            matches!(path, SlowPath::NumberArith | SlowPath::CompareJump),
            "only arithmetic sites repatch"
        );
        debug_assert_eq!(self.targets.placement(), Placement::Deferred);
        let site = self.arith_site_value(spec, path);
        let edge = self.em.edge(masm, EdgeKind::Indirect, site);
        onlineasm!(*masm.assembler(); jmp {edge};);
    }

    fn slow_site(&mut self, masm: &mut Masm, label: Label, spec: &ArithSpec, path: SlowPath) {
        // Only the arithmetic half repatches: a fused site's other slow
        // paths (the vector half of a fused multiply) leave through the
        // shared slow path, which runs the whole pair through the
        // thunks. They have no shape to grow into.
        let arith = matches!(path, SlowPath::NumberArith | SlowPath::CompareJump);
        match spec.link {
            // A narrow shape goes through a slow edge, so its first miss
            // can generate a stub, a `Full` site has nothing to grow into
            // and stays on the shared slow path.
            SiteLink::Live if arith && spec.shape.is_specialized() => {
                self.push_site(masm, label, spec, path);
            }
            // A specialized stub's slow block still hits its cell.
            SiteLink::Resident(cell) if arith => {
                debug_assert_eq!(self.targets.placement(), Placement::Inline);
                masm.bind(label);
                emit_hit_block(masm, self.insn.ip, SiteOperand::Immediate(cell));
            }
            _ => self.slow_control(masm, label, path),
        }
    }
}

impl<T: Targets, S: SpecSource> Speculation for CompiledContext<'_, T, S> {
    fn arith_site(&mut self, kind: usize, field: usize) -> ArithSpec {
        self.spec.arith_site(&self.insn, kind, field)
    }
}
