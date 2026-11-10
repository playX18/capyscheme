//! LIR to machine code for Tier 2.
//!
//! [`emit`] takes a [`LirFunc`] with its [`Allocation`] and writes one
//! onlineasm image: the entry prologue, every block in allocation order with
//! its sequenced moves, then the out-of-line slow blocks (yieldpoint polls,
//! allocation and barrier slow paths, deopt exits) and `deopt_common`.
//!
//! Frame layout: `fp[0]` is the return address, `fp[1]` the
//! closure, and bytecode registers `r` sit at `fp[r]`. Spill slot `s` is
//! `fp[frame_size + s]`. The frame is `frame_size + spill_slots` words. A call
//! writes its callee frame at `fp[frame_words..]`, with its own return point
//! header carrying the caller's livemask.
//!
//! Register names: the allocator works on [`PhysReg`] ids. [`arch::vreg`]
//! maps them to onlineasm.
//!
//! Calls: `Known` and `Generic` build a callee frame. `Helper` runs through
//! `emit_operation_call`. `Prim` calls the prim wrapper the way the baseline's
//! `prim_direct` does, and an error deopts before the call.
//!
//! Relocations: an AArch64 label address is a [`Reloc::LabelAddr`] that the
//! installer applies after load ([`Emitted::relocs`]). x86-64 uses `leap`.
//!
//! The deopt exit saves the frame's registers in the layout that
//! [`SaveArea`](super::deopt::metadata::SaveArea) reads, and resumes at the
//! address the runtime returns, with the registers reloaded. A constant-table slot is loaded through the owner import, the
//! address of the code image, at a fixed offset into its inline references.
//! [`HELPERS`] lists the runtime helpers that a `CallOp` names by index. The
//! builder emits those calls.

#[cfg(target_arch = "aarch64")]
mod aarch64;
mod slots;
#[cfg(target_arch = "x86_64")]
mod x64;
#[cfg(target_arch = "aarch64")]
use aarch64 as arch;
#[cfg(target_arch = "x86_64")]
use x64 as arch;

use std::collections::HashMap;

use cranelift_entity::EntityRef;

use onlineasm::{Label, VReg as MReg, onlineasm};

use crate::heap::object::builtin_class_ids;
use crate::runtime::codegen::gc::alloc::AllocSize;
use crate::runtime::codegen::import::{ImportKey, RuntimeSym, Section};
use crate::runtime::codegen::gc::{GcCodegen, header_word};
use crate::runtime::codegen::jit::entry::emit_enter_callee;
use crate::runtime::codegen::masm::{
    HeaderTarget, Livemask, Masm, RuntimeExtern, TOPLINK_TO_IMAGE_START, ValuesError,
};
use crate::runtime::codegen::operation::{
    ERROR_REG, OpArg, OpContinue, OpResult, OpTarget, OperationCall, emit_operation_call,
};
use crate::runtime::codegen::reloc::Reloc;
use crate::runtime::codegen::stubs;
use crate::runtime::codegen::template::tmp;
use crate::runtime::sabi;
use crate::runtime::sabi::regs::{self, AC, CP, RV, SFP, TC};

use super::deopt::exits::stress_deopt_due;
use super::deopt::metadata::SAVE_TEMPS;
use super::ir::func::{CaseClause, RestInfo};
use super::ir::inst::{Cmp, Constant, HelperRef, PrimRef, ReturnValues, SwitchDensity};
use super::ir::types::{ClassRef, CodeRef};
use super::lir::{
    AllocLayout, CallTarget, DeoptExit, FloatOp, IntOp, LBlock, LEdge, LirFunc, LirInst, LirTerm,
    NumberStoreKind, RegClass, ReturnedKind, Role, SafePoint, Test, VReg, WideShiftOp,
};
use super::regalloc::{Allocation, Loc, Move, PhysReg, RegFile, Scratch};

/// The runtime helpers the IR can call, by [`HelperRef`] index, in the order of
/// [`Helper`]. Each returns `(value, status)`, and a nonzero status deopts.
const HELPERS: &[RuntimeExtern] = &[
    RuntimeExtern::OptRuntimeProc,
    RuntimeExtern::OptSwitchSymbol,
    RuntimeExtern::OptGetSlot,
    RuntimeExtern::OptPutSlot,
    RuntimeExtern::OptCollectRest,
    RuntimeExtern::OptRestList,
    RuntimeExtern::OptVariableSet,
    RuntimeExtern::OptArith,
    RuntimeExtern::OptLength,
    RuntimeExtern::OptAppend,
    RuntimeExtern::OptReduceRational128,
    RuntimeExtern::OptGenericSlotGet,
    RuntimeExtern::OptGenericSlotPut,
    RuntimeExtern::OptGenericMethod,
    RuntimeExtern::OptStringSet,
    RuntimeExtern::OptValuesRest,
];

/// The `(loop, switch)` block alignment in bytes from `CAPY_OPT_ALIGN`.
///
/// Entries are `kindNN` pairs separated by commas, where `kind` is `loop` or
/// `switch` and `NN` is the byte count. A count below 4 (or a malformed
/// entry) is ignored; an empty string disables alignment.
fn block_alignment() -> (u32, u32) {
    let mut loop_align = 0;
    let mut switch_align = 0;
    let Some(raw) = crate::utils::flags::opt_align() else {
        return (loop_align, switch_align);
    };
    for entry in raw
        .split(',')
        .map(str::trim)
        .filter(|entry| !entry.is_empty())
    {
        let split = entry
            .find(|c: char| !c.is_ascii_alphabetic())
            .unwrap_or(entry.len());
        let (kind, amount) = entry.split_at(split);
        let Ok(amount) = amount.trim().parse::<u32>() else {
            continue;
        };
        match kind.trim() {
            "loop" => loop_align = amount,
            "switch" => switch_align = amount,
            _ => {}
        }
    }
    (loop_align, switch_align)
}

/// Everything the emitter needs from the pipeline.
pub(crate) struct Input<'a> {
    pub(crate) func: &'a LirFunc,
    pub(crate) alloc: &'a Allocation,
    pub(crate) regs: &'a RegFile,
    pub(crate) scratch: Scratch,
    /// The bytecode register of each entry parameter, in parameter order. A
    /// selector parameter, when there is one, follows them.
    pub(crate) entry_regs: &'a [u32],
    /// The bytecode frame size, in words.
    pub(crate) frame_size: u32,
    /// The argument count the entry takes.
    pub(crate) argc: u32,
    /// Words the prologue checks beyond the frame: the deepest deopt
    /// reconstruction.
    pub(crate) deopt_extent: u32,
    /// The baseline entry, which takes over on an arity or overflow mismatch.
    pub(crate) baseline_entry: usize,
    /// The address of the unit's invalid word.
    pub(crate) invalid_word: usize,
    /// The address of constant `index` of the root's non-moving buffer.
    pub(crate) constant_slot: &'a dyn Fn(u32) -> usize,
    /// The number of constant-table slots: `Constant::Table` and the code
    /// slots name reference slots `1..=constants`.
    pub(crate) constants: u32,
    /// The table slot of each code block's `CodeBlock` pointer, by
    /// [`CodeRef`] index. `Test::Code` and `AllocLayout::Closure` read it.
    pub(crate) code_slots: &'a [u32],
    /// The selector value of each OSR entry. Each gets a prologue that sets
    /// the entry's selector parameter to it. Empty when there are none, and
    /// then the entry has no selector parameter.
    pub(crate) osr: &'a [u32],
    /// The variadic entry, if the procedure takes a rest argument.
    pub(crate) rest: Option<RestInfo>,
    /// The clauses of a multi-entry `case-lambda`, in table order.
    pub(crate) case_clauses: &'a [CaseClause],
    /// The shared rest-vector pseudo register of a case-lambda's variadic
    /// clauses.
    pub(crate) rest_reg: Option<u32>,
    /// Whether the entry block ends with a selector parameter (OSR or a
    /// case-lambda clause dispatch).
    pub(crate) has_selector: bool,
    /// Record a label before each LIR instruction, so the unit can annotate
    /// its machine code with LIR/IR text (`CAPY_OPT_ANNOTATE`).
    pub(crate) annotate: bool,
}

/// The emitted image, not yet linked. The assembler stays with the caller
/// (see [`emit`]) so it can be reused for the next compilation.
pub(crate) struct Emitted {
    /// The unit's entry: the prologue.
    pub(crate) entry: Label,
    /// The prologue of each OSR entry, as its selector value and label.
    pub(crate) osr: Vec<(u32, Label)>,
    /// The owner word at the image's start, which the owner patches at publish
    /// (the stack walker reads it through the return points' toplink).
    pub(crate) owner: Label,
    /// Words in the frame: bytecode frame plus spill slots.
    pub(crate) frame_words: u32,
    /// Patches the installer applies after load: AArch64 label addresses,
    /// and on both targets the words of dense switch tables.
    pub(crate) relocs: Vec<Reloc>,
    /// The optimizer's own data islands: the start label and byte length of
    /// each dense switch table. The registry prints them as words, never
    /// as instructions.
    pub(crate) data: Vec<(Label, usize)>,
    /// A label bound before each LIR instruction, with its global index.
    /// Sorted by the instruction's position in the block order; empty when
    /// annotation is off.
    pub(crate) annotations: Vec<(Label, u32)>,
}

/// The register file of this host, for the allocator.
pub(crate) fn register_file() -> (RegFile, Scratch) {
    arch::reg_file()
}

/// Emits `input` as one image into `masm`, which the caller keeps: the
/// assembler is reset first and returned to the caller through the borrow,
/// so its buffer capacity survives across compilations.
pub(crate) fn emit(masm: &mut Masm, input: Input<'_>) -> Result<Emitted, String> {
    let mut emitter = Emitter::new(input, masm);
    emitter.prologue()?;
    emitter.blocks()?;
    emitter.deferred()?;
    emitter.deopt_common()?;
    let frame_words = emitter.frame_words;
    let relocs = std::mem::take(&mut emitter.relocs);
    let data = std::mem::take(&mut emitter.data);
    let osr = std::mem::take(&mut emitter.osr);
    let annotations = std::mem::take(&mut emitter.annotations);
    let entry = emitter.entry;
    let owner = emitter.owner;
    Ok(Emitted {
        entry,
        osr,
        owner,
        frame_words,
        relocs,
        data,
        annotations,
    })
}

/// The byte size of a deferred allocation.
#[derive(Clone, Copy, Debug)]
enum AllocDeferred {
    /// A compile-time byte count.
    Bytes(usize),
    /// `base + 8 * count`, with `count` in a register that survives both
    /// allocation paths.
    Words { count: MReg, base: usize },
}

/// A slow block, emitted after the code that jumps to it.
enum Deferred {
    /// A deopt exit: `move SITE_TMP, id; jmp deopt_common`.
    Exit { exit: DeoptExit, label: Label },
    /// The multiple-value return of a forwarded return point: the callee
    /// jumps here with the value count in `AC` and its frame at `SFP`. The
    /// exit pops the frame, keeps the count in the thread context, and
    /// deopts to the entry, where baseline reads both.
    Returned {
        exit: DeoptExit,
        label: Label,
        /// The callee frame's offset from this frame's fp, in bytes.
        callee: i32,
    },
    /// The allocation slow path. It returns to `join`.
    Alloc {
        slow: Label,
        join: Label,
        obj: MReg,
        size: AllocDeferred,
        header: u64,
    },
    /// The wide path of `Box(Repr::Int64)`: allocate a one-limb `BIGINT`,
    /// write its payload and continue at `done`. The fixnum path falls
    /// through and never reaches this block.
    BoxBigInt {
        wide: Label,
        done: Label,
        obj: MReg,
        /// The destination's spill slot, when it is spilled.
        slot: Option<u32>,
        /// Where the overflowing i64 lives on entry to the block.
        src: BoxSrc,
        bytes: usize,
        header: u64,
    },
    /// The two-limb path of `Box(Repr::Int128)`: allocate a 32-byte
    /// sign-magnitude `BIGINT` with `count = 2`, write its payload and
    /// continue at `done`. Both limbs are saved on the native stack across
    /// the non-collecting allocation slow path.
    BoxInt128Two {
        wide: Label,
        done: Label,
        obj: MReg,
        slot: Option<u32>,
        lo: BoxSrc,
        hi: BoxSrc,
        bytes: usize,
        header: u64,
    },
    /// The barrier slow path, which logs `obj`.
    /// `preserve` names emitter temporaries the code after the barrier
    /// still reads, which the call would otherwise clobber.
    Barrier {
        slow: Label,
        join: Label,
        obj: MReg,
        preserve: &'static [MReg],
    },
    /// The yieldpoint poll. Its frame's return address is `ra`, whose header
    /// carries the livemask.
    Poll {
        slow: Label,
        join: Label,
        ra: Label,
        livemask: Livemask,
        /// The GPRs the poll keeps across the yield, with their frame slots.
        /// The slow path stores them before the call and reloads them after.
        saves: Vec<(MReg, u32)>,
    },
    /// A dense switch jump table: one 8-byte target address per key, from
    /// `min` up. Link patches each word with its block's loaded address.
    SwitchTable { label: Label, targets: Vec<Label> },
}

/// Where the overflowing i64 of an out-of-line `Box(Repr::Int64)` lives when
/// the fixnum range check fails.
#[derive(Clone, Copy)]
enum BoxSrc {
    /// A pool register with the raw value.
    Reg(MReg),
    /// A spill slot holding the raw value.
    Slot(u32),
}

/// A physical location, as a move endpoint.
#[derive(Clone, Copy, PartialEq, Eq)]
enum End {
    /// A GPR, or the raw bits of a value routed through a GPR temp.
    G(MReg),
    /// An FPR.
    F(MReg),
    /// A spill slot.
    M(u32),
}

/// Where an instruction writes a result.
#[derive(Clone, Copy)]
struct Target {
    reg: MReg,
    /// The slot to store `reg` into afterwards, if the value is spilled.
    slot: Option<u32>,
    class: RegClass,
}

/// A value produced only to feed the block's branch, folded into the branch
/// itself. The position is where the producing instruction reads its operands.
#[derive(Clone, Copy)]
enum Fused {
    /// `cmp lhs, rhs`, then a conditional branch.
    Cmp(Cmp, VReg, VReg, u32),
    /// `test src`, then a conditional branch.
    Test(Test, VReg, u32),
}

struct Emitter<'a> {
    func: &'a LirFunc,
    alloc: &'a Allocation,
    regs: &'a RegFile,
    scratch: Scratch,
    entry_regs: &'a [u32],
    frame_size: u32,
    argc: u32,
    deopt_extent: u32,
    baseline_entry: usize,
    invalid_word: usize,
    constant_slot: &'a dyn Fn(u32) -> usize,

    masm: &'a mut Masm,
    gc: GcCodegen,
    frame_words: u32,
    entry: Label,
    owner: Label,
    deopt_common: Label,
    blocks: HashMap<LBlock, Label>,
    exits: HashMap<DeoptExit, Label>,
    deferred: Vec<Deferred>,
    relocs: Vec<Reloc>,
    constants: u32,
    /// Dense switch tables: `(start label, byte length)`.
    data: Vec<(Label, usize)>,
    code_slots: &'a [u32],
    /// The global index of the instruction being emitted.
    g: u32,
    /// The guards emitted so far, which `CAPY_OPT_STRESS_DEOPT` counts.
    guards: u64,
    /// The block emitted after the current one, so a jump to it falls through.
    next_block: Option<LBlock>,
    /// A compare or tag test folded into the block's branch.
    fused: Option<Fused>,
    /// How many operands name each register, to find single-use flags.
    use_counts: HashMap<VReg, u32>,
    /// The selector value and prologue label of each OSR entry.
    osr: Vec<(u32, Label)>,
    /// Whether the entry block ends with a selector parameter.
    has_selector: bool,
    /// The variadic entry: the glue accepts any argument count of at least
    /// `required`, and collects the extras into the rest vector first.
    rest: Option<RestInfo>,
    /// The clauses of a multi-entry `case-lambda`, in table order.
    case_clauses: &'a [CaseClause],
    /// The shared rest-vector pseudo register of a case-lambda's variadic
    /// clauses.
    rest_reg: Option<u32>,
    /// Whether to bind a label before each LIR instruction for listing
    /// annotations.
    annotate: bool,
    /// A label bound before each LIR instruction, with its global index.
    annotations: Vec<(Label, u32)>,
}

/// The GPR temporary that holds a spilled operand or an instruction result.
const T0: MReg = tmp::T0;
const T1: MReg = tmp::T1;
const T2: MReg = tmp::T2;
const T3: MReg = tmp::T3;

impl<'a> Emitter<'a> {
    fn new(input: Input<'a>, masm: &'a mut Masm) -> Self {
        let frame_words = input.frame_size + input.alloc.spill_slots;
        masm.reset();
        // The owner word comes first: the image starts with it, as baseline
        // images do, so return points' toplinks reach the owner slot.
        let owner = masm.import_inline(ImportKey::Owner);
        let entry = masm.label();
        let deopt_common = masm.label();
        let osr = input
            .osr
            .iter()
            .map(|&selector| (selector, masm.label()))
            .collect();
        Self {
            func: input.func,
            alloc: input.alloc,
            regs: input.regs,
            scratch: input.scratch,
            entry_regs: input.entry_regs,
            frame_size: input.frame_size,
            argc: input.argc,
            deopt_extent: input.deopt_extent,
            baseline_entry: input.baseline_entry,
            invalid_word: input.invalid_word,
            constant_slot: input.constant_slot,
            masm,
            gc: GcCodegen::new(),
            frame_words,
            entry,
            owner,
            deopt_common,
            blocks: HashMap::new(),
            exits: HashMap::new(),
            deferred: Vec::new(),
            relocs: Vec::new(),
            constants: input.constants,
            data: Vec::new(),
            code_slots: input.code_slots,
            g: 0,
            guards: 0,
            next_block: None,
            fused: None,
            use_counts: HashMap::new(),
            osr,
            has_selector: input.has_selector,
            rest: input.rest,
            case_clauses: input.case_clauses,
            rest_reg: input.rest_reg,
            annotate: input.annotate,
            annotations: Vec::new(),
        }
    }

    /// Bind a label at the current point and tie it to LIR instruction
    /// `g`, when annotation is on.
    fn mark(&mut self) {
        if self.annotate {
            let label = self.masm.label();
            self.masm.bind(label);
            self.annotations.push((label, self.g));
        }
    }

    fn asm(&mut self) -> &mut onlineasm::Assembler {
        self.masm.assembler()
    }

    fn use_pos(&self) -> u32 {
        super::regalloc::pos::early(self.g)
    }

    fn def_pos(&self) -> u32 {
        super::regalloc::pos::late(self.g)
    }

    /// The machine register of an allocator register, which must be a GPR.
    fn gpr_of(&self, p: PhysReg) -> Result<MReg, String> {
        if p == self.scratch.gpr {
            return Ok(T2);
        }
        arch::vreg(p).ok_or_else(|| format!("{p} has no onlineasm name"))
    }

    /// The machine register of an allocator register, which must be an FPR.
    fn fpr_of(&self, p: PhysReg) -> Result<MReg, String> {
        arch::vreg(p).ok_or_else(|| format!("{p} has no onlineasm name"))
    }

    /// The temporary for a spilled FPR operand or result. It is `xmm7` on
    /// x86-64 and `v7` (v23) on AArch64; see each arch module.
    fn fpr_temp(&self) -> MReg {
        arch::FPR_TEMP
    }

    /// Loads the address of `target` into `dst`, PC-relative (`adrp`/`add`
    /// with a load-time relocation on AArch64).
    fn label_addr(&mut self, dst: MReg, target: Label) {
        onlineasm!(*self.asm(); leap {dst}, {target};);
    }

    /// Loads the address of constant-table slot `slot` into `dst`. The address
    /// lives in a data word next to the code, which the installer points at
    /// the owner's reference box. Code then loads the slot's
    /// value through that address, so the collector's updates are seen.
    fn load_constant_addr(&mut self, dst: MReg, slot: u32) -> Result<(), String> {
        if slot == 0 || slot > self.constants {
            return Err(format!("constant table slot {slot} is missing"));
        }
        // The owner word holds the address of the code image, which keeps its
        // references inline.
        self.masm.load_import(dst, ImportKey::Owner);
        let mut offset = crate::runtime::objectmodel::CodeImage::ref_offset(slot as usize);
        while offset > 0 {
            let step = offset.min(4095);
            onlineasm!(*self.asm(); addq {dst}, {dst}, {step as i64};);
            offset -= step;
        }
        Ok(())
    }

    /// `dst = bits` from a shared, PC-relative constant island. One
    /// instruction instead of `movabs` / `movz`+`movk*`, and identical on
    /// both targets.
    fn const_pool_load(&mut self, dst: MReg, bits: u64) {
        let label = self.masm.embed_u64(bits);
        self.data.push((label, 8));
        let at = self.masm.assembler().mem_label(label);
        onlineasm!(*self.asm(); loadq {dst}, {at};);
    }

    /// The constant-table slot of the `CodeBlock` pointer of `code`.
    fn code_slot(&self, code: CodeRef) -> Result<u32, String> {
        self.code_slots
            .get(code.index())
            .copied()
            .ok_or_else(|| format!("{code} has no code-block slot in the constant table"))
    }

    fn slot_disp(&self, slot: u32) -> i32 {
        (8 * (self.frame_size + slot)) as i32
    }

    fn slot_mem(&mut self, slot: u32) -> onlineasm::Mem {
        let disp = self.slot_disp(slot);
        self.masm.assembler().mem(SFP, disp)
    }

    fn loc(&self, v: VReg, pos: u32) -> Result<Loc, String> {
        self.alloc
            .loc_at(v, pos)
            .ok_or_else(|| format!("{v} has no location at {pos}"))
    }

    /// The register a use reads: its own register, or `tmp` loaded from its
    /// slot.
    fn read(&mut self, v: VReg, tmp: MReg) -> Result<MReg, String> {
        let class = self.func.class(v);
        match self.loc(v, self.use_pos())? {
            Loc::Reg(p) => match class {
                RegClass::Gpr => self.gpr_of(p),
                RegClass::Fpr => self.fpr_of(p),
            },
            Loc::Slot(s) => {
                let mem = self.slot_mem(s);
                match class {
                    RegClass::Gpr => onlineasm!(*self.asm(); loadq {tmp}, {mem};),
                    RegClass::Fpr => onlineasm!(*self.asm(); loadd {tmp}, {mem};),
                }
                Ok(tmp)
            }
        }
    }

    /// Sets `CP` to the call's callee, which is argument 0. The value is read
    /// from its argument location instead of the callee slot the call just
    /// wrote; when it already lives in `CP`, no instruction is emitted.
    fn set_cp_from_closure_arg(&mut self, args: &[VReg]) -> Result<(), String> {
        let Some(&callee) = args.first() else {
            return Ok(());
        };
        let cp = self.gpr_of(arch::CP)?;
        let from = self.read(callee, cp)?;
        if from != cp {
            onlineasm!(*self.asm(); move {cp}, {from};);
        }
        Ok(())
    }

    /// The register a result is written to: its own register, or the
    /// temporary that [`Emitter::commit`] stores to its slot.
    fn write(&mut self, v: VReg) -> Result<Target, String> {
        let class = self.func.class(v);
        let temp = match class {
            RegClass::Gpr => T2,
            RegClass::Fpr => self.fpr_temp(),
        };
        match self.loc(v, self.def_pos())? {
            Loc::Reg(p) => {
                let reg = match class {
                    RegClass::Gpr => self.gpr_of(p)?,
                    RegClass::Fpr => self.fpr_of(p)?,
                };
                Ok(Target {
                    reg,
                    slot: None,
                    class,
                })
            }
            Loc::Slot(s) => Ok(Target {
                reg: temp,
                slot: Some(s),
                class,
            }),
        }
    }

    /// Stores a spilled result to its slot.
    fn commit(&mut self, t: Target) {
        if let Some(s) = t.slot {
            let mem = self.slot_mem(s);
            match t.class {
                RegClass::Gpr => onlineasm!(*self.asm(); storeq {mem}, {t.reg};),
                RegClass::Fpr => onlineasm!(*self.asm(); stored {mem}, {t.reg};),
            }
        }
    }

    /// Opens the scope of an instruction that uses the FPR temporary. The
    /// temporaries (`v7`, and `v6` for a second spilled operand) are
    /// caller-saved and outside the pool, so scopes are empty.
    fn fpr_scope_begin(&mut self, second: bool) {
        // The temporaries are caller-saved registers outside the pool, so
        // there is nothing to save.
        let _ = second;
    }

    fn fpr_scope_end(&mut self, second: bool) {
        let _ = second;
    }

    fn block_label(&mut self, b: LBlock) -> Label {
        if let Some(&label) = self.blocks.get(&b) {
            return label;
        }
        let label = self.masm.label();
        self.blocks.insert(b, label);
        label
    }

    fn exit_label(&mut self, exit: DeoptExit) -> Label {
        if let Some(&label) = self.exits.get(&exit) {
            return label;
        }
        let label = self.masm.label();
        self.exits.insert(exit, label);
        self.deferred.push(Deferred::Exit { exit, label });
        label
    }

    /// Counts the guard being emitted for `CAPY_OPT_STRESS_DEOPT`. When it
    /// is the n-th, this emits an unconditional jump to `fail` and returns
    /// true, and the caller emits nothing else for the guard.
    fn stress_guard(&mut self, fail: Label) -> bool {
        let due = stress_deopt_due(self.guards);
        self.guards += 1;
        if due {
            onlineasm!(*self.asm(); jmp {fail};);
        }
        due
    }

    /// The live mask of safepoint `sp` in header form: bit `i` for `fp[i + 1]`.
    /// The closure is always live. A spill slot `s` is `fp[frame_size + s]`.
    /// Returns the header word, or a mask block embedded in the image.
    fn livemask(&mut self, sp: SafePoint) -> Result<Livemask, String> {
        let data = &self.func.safepoints[sp];
        let mask = data
            .livemask
            .as_ref()
            .ok_or_else(|| format!("{sp} has no livemask"))?;
        let mut bits = vec![1u64];
        for s in 0..self.alloc.spill_slots {
            let set = mask
                .words
                .get(s as usize / 64)
                .is_some_and(|w| (w >> (s % 64)) & 1 == 1);
            if !set {
                continue;
            }
            let bit = (self.frame_size + s - 1) as usize;
            if bits.len() <= bit / 64 {
                bits.resize(bit / 64 + 1, 0);
            }
            bits[bit / 64] |= 1 << (bit % 64);
        }
        let length = bits
            .iter()
            .enumerate()
            .rev()
            .find(|(_, w)| **w != 0)
            .map_or(0, |(i, w)| i * 64 + 64 - w.leading_zeros() as usize);
        if length < 63 {
            return Ok(Livemask::Word(sabi::livemask_fixnum(bits[0] as usize)));
        }
        let mut words = vec![bits.len() as u64];
        words.extend_from_slice(&bits);
        Ok(self.masm.embed_mask(&words))
    }

    /// Emits the return point header for `ra` (binding it) with `livemask`,
    /// describing this frame.
    fn rp_header(&mut self, ra: Label, target: HeaderTarget, livemask: Livemask) {
        let bytes = (8 * self.frame_words) as isize;
        self.masm
            .emit_rp_header_at(ra, target, livemask, TOPLINK_TO_IMAGE_START, bytes);
    }

    /// Runs a move list in order. `resolve` has already sequentialized each
    /// group, and the groups of one instruction are concatenated in time
    /// order, so a move may read a location that an earlier move in the list
    /// wrote. Reordering the list as if it were a parallel move changes the
    /// values read, and it can loop forever when a cycle runs through the
    /// scratch register.
    fn sequence(&mut self, moves: &[Move], consts: &[Constant]) -> Result<(), String> {
        for m in moves {
            if let Some(i) = m.konst {
                // A reload of a constant: rebuild it from its immediate.
                let Loc::Reg(p) = m.dst else {
                    return Err("a constant is rebuilt into a slot".to_string());
                };
                let reg = self.gpr_of(p)?;
                let bits = match consts[i as usize] {
                    Constant::Int32(v) => v as i64,
                    Constant::Int64(v) => v,
                    Constant::Bits(b) => b as i64,
                    _ => return Err("an unsupported constant is rebuilt".to_string()),
                };
                if i32::try_from(bits).is_ok() {
                    onlineasm!(*self.asm(); move {reg}, {bits};);
                } else {
                    self.const_pool_load(reg, bits as u64);
                }
            } else if m.src != m.dst {
                self.move_loc(m.src, m.dst)?;
            }
        }
        Ok(())
    }

    /// The machine endpoint of a location. The scratch GPR is `T2`, and the
    /// scratch FPR is `T3` holding raw bits.
    fn endpoint(&self, loc: Loc) -> Result<End, String> {
        Ok(match loc {
            Loc::Slot(s) => End::M(s),
            Loc::Reg(p) if p == self.scratch.gpr => End::G(T2),
            Loc::Reg(p) if p == self.scratch.fpr => End::G(T3),
            Loc::Reg(p) if p.0 >= arch::FPR_BASE => End::F(self.fpr_of(p)?),
            Loc::Reg(p) => End::G(self.gpr_of(p)?),
        })
    }

    fn move_loc(&mut self, src: Loc, dst: Loc) -> Result<(), String> {
        let (s, d) = (self.endpoint(src)?, self.endpoint(dst)?);
        self.move_end(d, s)
    }

    fn move_end(&mut self, dst: End, src: End) -> Result<(), String> {
        match (dst, src) {
            (End::G(d), End::G(s)) => {
                if d != s {
                    onlineasm!(*self.asm(); move {d}, {s};);
                }
            }
            (End::F(d), End::F(s)) => {
                if d != s {
                    onlineasm!(*self.asm(); moved {d}, {s};);
                }
            }
            (End::G(d), End::F(s)) => onlineasm!(*self.asm(); fd2q {d}, {s};),
            (End::F(d), End::G(s)) => onlineasm!(*self.asm(); fq2d {d}, {s};),
            (End::G(d), End::M(s)) => {
                let mem = self.slot_mem(s);
                onlineasm!(*self.asm(); loadq {d}, {mem};);
            }
            (End::F(d), End::M(s)) => {
                let mem = self.slot_mem(s);
                onlineasm!(*self.asm(); loadd {d}, {mem};);
            }
            (End::M(d), End::G(s)) => {
                let mem = self.slot_mem(d);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            (End::M(d), End::F(s)) => {
                let mem = self.slot_mem(d);
                onlineasm!(*self.asm(); stored {mem}, {s};);
            }
            (End::M(d), End::M(s)) => {
                if d != s {
                    // Raw bits, so one GPR temp serves both classes.
                    let from = self.slot_mem(s);
                    onlineasm!(*self.asm(); loadq {T1}, {from};);
                    let to = self.slot_mem(d);
                    onlineasm!(*self.asm(); storeq {to}, {T1};);
                }
            }
        }
        Ok(())
    }

    /// The prologues: the normal entry, then one per OSR entry. The normal
    /// entry checks the argument count. An OSR entry runs in the baseline
    /// frame that is already there, so it skips that check. The runtime
    /// checks the frame room before it transfers (see `osr_entry` in
    /// `interp/slow.rs`), so its own check traps.
    fn prologue(&mut self) -> Result<(), String> {
        let entry = self.entry;
        let arity_fail = self.masm.label();
        let argc = self.argc as i64;
        self.masm.bind(entry);
        if !self.case_clauses.is_empty() {
            self.case_prologue(arity_fail)?;
        } else {
            if let Some(rest) = self.rest {
                let required = rest.required as i64;
                onlineasm!(*self.asm(); bqb {AC}, {required}, {arity_fail};);
            } else {
                onlineasm!(*self.asm(); bqneq {AC}, {argc}, {arity_fail};);
            }
            self.enter_block(0, arity_fail, true)?;
        }
        // Arity or overflow mismatch: the baseline entry checks again and
        // takes over. Nothing has been written yet (a case-lambda replaces
        // r1 only past its frame check). The baseline image resumes at
        // bytecode entry 0, so restore the entry state it expects: `PC` at
        // the code buffer's entry, the constant pointer there, and `MD`.
        // A direct call (`enter_known`) reaches this prologue without
        // setting them.
        self.masm.bind(arity_fail);
        let block_at = self
            .masm
            .assembler()
            .mem(CP, sabi::CLOSURE_CODE_BLOCK as i32);
        let entry_at = self
            .masm
            .assembler()
            .mem(T0, sabi::CODE_BLOCK_ENTRYPOINT as i32);
        onlineasm!(*self.asm();
            loadq {T0}, {block_at};
            loadp {regs::PC}, {entry_at};
            move {regs::CONSTANT_POINTER}, {regs::PC};
        );
        let md_at = self.masm.assembler().mem(regs::CONSTANT_POINTER, -8);
        onlineasm!(*self.asm(); loadp {regs::MD}, {md_at};);
        self.masm.load_import(T0, ImportKey::BaselineEntry);
        onlineasm!(*self.asm(); jmp {T0};);

        for (selector, label) in self.osr.clone() {
            self.masm.bind(label);
            let room = self.masm.label();
            self.enter_block(selector, room, false)?;
            self.masm.bind(room);
            onlineasm!(*self.asm(); break;);
        }
        Ok(())
    }

    /// The prologue of a multi-entry `case-lambda`: one arity test per clause
    /// row, then per-clause entrance code that checks room (the clause frame
    /// plus its extras), collects a variadic clause's rest vector, and loads
    /// the entry parameters. Each clause sets the entry's selector to its row
    /// index; the entry block's `Switch` then passes the clause its
    /// parameters.
    fn case_prologue(&mut self, fail: Label) -> Result<(), String> {
        let clauses = self.case_clauses.to_vec();
        let mut entrances = Vec::with_capacity(clauses.len());
        for clause in &clauses {
            let next = self.masm.label();
            if clause.min > 0 {
                let min = clause.min as i64;
                onlineasm!(*self.asm(); bqb {AC}, {min}, {next};);
            }
            if clause.max < u32::MAX {
                let max = clause.max as i64;
                onlineasm!(*self.asm(); bqa {AC}, {max}, {next};);
            }
            let entrance = self.masm.label();
            onlineasm!(*self.asm(); jmp {entrance};);
            self.masm.bind(next);
            entrances.push(entrance);
        }
        onlineasm!(*self.asm(); jmp {fail};);

        let frame_regs = self.entry_regs.len() - usize::from(self.rest_reg.is_some());
        for (index, clause) in clauses.iter().enumerate() {
            self.masm.bind(entrances[index]);
            // Room: the frame (max over clauses) plus this clause's extras.
            let total = (self.frame_words + self.deopt_extent) as i64;
            if let Some(rest) = clause.rest {
                let required = rest.required as i64;
                onlineasm!(*self.asm();
                    subp {T0}, {AC}, {required};
                    addp {T0}, {T0}, {total};
                );
            } else {
                onlineasm!(*self.asm(); move {T0}, {total};);
            }
            self.masm.check_frame_limit(T0, T1, T2, fail);
            // Only now, past the point that can fall back to baseline, replace
            // r1 with this clause's closure: the clause body's free variables
            // are the clause closure's, not the group's. Baseline's `enter_case`
            // needs the group closure on the fallback path.
            {
                let clo_at = self.masm.assembler().mem(SFP, 8);
                let free = (sabi::CLOSURE_FREE + 8 * index) as i32;
                onlineasm!(*self.asm();
                    loadq {T1}, {clo_at};
                );
                let slot = self.masm.assembler().mem(T1, free);
                onlineasm!(*self.asm();
                    loadq {T2}, {slot};
                    storeq {clo_at}, {T2};
                );
            }
            // A variadic clause collects its extras first; its count lands in
            // the frame slot the clause reads. A fixed clause leaves a valid
            // dummy in the shared rest parameter (no clause it selects uses it).
            if let Some(rest) = clause.rest {
                self.collect_rest(rest)?;
            } else {
                let empty = crate::runtime::objectmodel::Value::null().bits() as i64;
                onlineasm!(*self.asm(); move {T0}, {empty};);
            }
            // Load the union of the clauses' frame registers; the entry
            // passes only this clause's subset on.
            let params = self.func.blocks[self.func.entry].params.clone();
            let base = super::regalloc::pos::pre(self.alloc.base[self.func.entry]);
            for (i, &reg) in self.entry_regs[..frame_regs].iter().enumerate() {
                let from = self.masm.assembler().mem(SFP, 8 * reg as i32);
                match self.loc(params[i], base)? {
                    Loc::Reg(q) => {
                        let to = self.gpr_of(q)?;
                        onlineasm!(*self.asm(); loadq {to}, {from};);
                    }
                    Loc::Slot(s) => {
                        onlineasm!(*self.asm(); loadq {T1}, {from};);
                        let to = self.slot_mem(s);
                        onlineasm!(*self.asm(); storeq {to}, {T1};);
                    }
                }
            }
            let mut next_param = frame_regs;
            if let Some(rest_reg) = self.rest_reg {
                let slot = params
                    .get(next_param)
                    .copied()
                    .ok_or("the entry has no rest vector parameter")?;
                match self.loc(slot, base)? {
                    Loc::Reg(q) => {
                        let to = self.gpr_of(q)?;
                        onlineasm!(*self.asm(); move {to}, {T0};);
                    }
                    Loc::Slot(s) => {
                        let to = self.slot_mem(s);
                        onlineasm!(*self.asm(); storeq {to}, {T0};);
                    }
                }
                let _ = rest_reg;
                next_param += 1;
            }
            // The selector parameter is last.
            let selector = *params
                .get(next_param)
                .ok_or("the entry has no selector parameter")?;
            let value = index as i64;
            match self.loc(selector, base)? {
                Loc::Reg(q) => {
                    let to = self.gpr_of(q)?;
                    onlineasm!(*self.asm(); move {to}, {value};);
                }
                Loc::Slot(s) => {
                    onlineasm!(*self.asm(); move {T0}, {value};);
                    let to = self.slot_mem(s);
                    onlineasm!(*self.asm(); storeq {to}, {T0};);
                }
            }
            let target = self.block_label(self.func.entry);
            onlineasm!(*self.asm(); jmp {target};);
        }
        Ok(())
    }

    /// Checks the frame room, loads the entry parameters from the frame, sets
    /// the selector parameter to `selector`, and jumps to the entry block.
    /// On a normal fixed-arity entry (`count_entry`), bytecode register 1 is
    /// taken from `CP`, which already holds the running closure, instead of
    /// being reloaded from `fp[1]`.
    /// Jumps to `fail` when the frame does not fit, before anything is
    /// written.
    fn enter_block(&mut self, selector: u32, fail: Label, count_entry: bool) -> Result<(), String> {
        let entry_block = self.func.entry;
        let total = (self.frame_words + self.deopt_extent) as i64;
        if let Some(rest) = self.rest {
            // The extras need room too: baseline's `enter_rest` checks the
            // frame plus the extra count, and a deopt rebuilds that frame.
            let required = rest.required as i64;
            onlineasm!(*self.asm(); subp {T0}, {AC}, {required}; addp {T0}, {T0}, {total};);
        } else {
            onlineasm!(*self.asm(); move {T0}, {total};);
        }
        // The normal entry's check is also its yieldpoint: `TC_ESP` is nulled
        // to ask for a yield, and the slow path handles it. An OSR entry has
        // already transferred, cannot fall back (its failure branch traps),
        // and must not confuse that yield request with a full frame: it checks
        // the owning thread's `real_esp` limit instead, which only it writes.
        let limit = if count_entry {
            crate::runtime::sabi::TC_ESP
        } else {
            crate::runtime::sabi::TC_REAL_ESP
        };
        self.masm.check_frame_limit_at(T0, T1, T2, fail, limit);
        if count_entry && crate::utils::flags::jit_stats() {
            // Tier 2 entry counter (`CAPY_JIT_STATS`). Emitted only when stats
            // are on, so a run without them carries no counter. The room
            // check above has passed, so the entry is taken.
            self.masm
                .load_import(T0, ImportKey::Runtime(RuntimeSym::OptEntries));
            let at = self.masm.assembler().mem(T0, 0);
            onlineasm!(*self.asm(); loadq {T1}, {at}; addq {T1}, {T1}, 1; storeq {at}, {T1};);
        }
        let params = self.func.blocks[entry_block].params.clone();
        let regs = self.entry_regs.len();
        let expected = regs + usize::from(self.has_selector);
        if params.len() != expected {
            return Err("entry parameters and their bytecode registers differ".to_string());
        }
        let base = super::regalloc::pos::pre(self.alloc.base[entry_block]);
        // The extras are still in the incoming frame, where the parameter
        // loads below may write spill slots, so collect them first.
        let mut rest_param = None;
        let mut cnt_param = None;
        if let Some(rest) = self.rest {
            // The vector, or in scalar mode the tail vector.
            let vector_reg = match rest.scalars {
                Some(n) => {
                    self.collect_rest_scalar(rest, n);
                    rest.reg + n
                }
                None => {
                    self.collect_rest(rest)?;
                    rest.reg
                }
            };
            rest_param = self.entry_regs.iter().position(|&reg| reg == vector_reg);
            if rest.scalars.is_some() {
                // The count goes straight to its parameter. Both are moved
                // before any other parameter load, which may use `T0`/`T1`.
                cnt_param = self.entry_regs.iter().position(|&reg| reg == rest.cnt);
            }
            let Some(index) = rest_param else {
                return Err("the entry has no rest vector parameter".to_string());
            };
            match self.loc(params[index], base)? {
                Loc::Reg(q) => {
                    let to = self.gpr_of(q)?;
                    onlineasm!(*self.asm(); move {to}, {T0};);
                }
                Loc::Slot(s) => {
                    let to = self.slot_mem(s);
                    onlineasm!(*self.asm(); storeq {to}, {T0};);
                }
            }
            if let Some(index) = cnt_param {
                match self.loc(params[index], base)? {
                    Loc::Reg(q) => {
                        let to = self.gpr_of(q)?;
                        onlineasm!(*self.asm(); move {to}, {T1};);
                    }
                    Loc::Slot(s) => {
                        let to = self.slot_mem(s);
                        onlineasm!(*self.asm(); storeq {to}, {T1};);
                    }
                }
            }
        }
        // `CP` holds the running closure on every normal call entry, so
        // bytecode reg 1 is taken from it before any other parameter load can
        // overwrite `CP`. OSR entries (`count_entry == false`) cannot rely on
        // it, and a variadic entry's `collect_rest` calls a Rust helper that
        // clobbers the caller-saved `CP` before the parameter loads.
        let closure_param = (count_entry && self.rest.is_none())
            .then(|| self.entry_regs.iter().position(|&reg| reg == 1))
            .flatten();
        if let Some(i) = closure_param {
            match self.loc(params[i], base)? {
                Loc::Reg(q) if q == arch::CP => {}
                Loc::Reg(q) => {
                    let to = self.gpr_of(q)?;
                    onlineasm!(*self.asm(); move {to}, {CP};);
                }
                Loc::Slot(s) => {
                    let to = self.slot_mem(s);
                    onlineasm!(*self.asm(); storeq {to}, {CP};);
                }
            }
        }
        for (i, &p) in params[..regs].iter().enumerate() {
            if rest_param == Some(i) || closure_param == Some(i) || cnt_param == Some(i) {
                continue;
            }
            let reg = self.entry_regs[i];
            // A scalar extra is read from the argument area, where the call
            // put it (and `collect_rest_scalar` cleaned it).
            let from = match self.rest {
                Some(rest)
                    if rest
                        .scalars
                        .is_some_and(|n| reg >= rest.reg && reg < rest.reg + n) =>
                {
                    let slot = 2 + rest.required + (reg - rest.reg);
                    self.masm.assembler().mem(SFP, 8 * slot as i32)
                }
                _ => self.masm.assembler().mem(SFP, 8 * reg as i32),
            };
            match self.loc(p, base)? {
                Loc::Reg(q) => {
                    let to = self.gpr_of(q)?;
                    onlineasm!(*self.asm(); loadq {to}, {from};);
                }
                Loc::Slot(s) => {
                    onlineasm!(*self.asm(); loadq {T0}, {from};);
                    let to = self.slot_mem(s);
                    onlineasm!(*self.asm(); storeq {to}, {T0};);
                }
            }
        }
        if self.has_selector {
            let value = selector as i64;
            match self.loc(params[regs], base)? {
                Loc::Reg(q) => {
                    let to = self.gpr_of(q)?;
                    onlineasm!(*self.asm(); move {to}, {value};);
                }
                Loc::Slot(s) => {
                    onlineasm!(*self.asm(); move {T0}, {value};);
                    let to = self.slot_mem(s);
                    onlineasm!(*self.asm(); storeq {to}, {T0};);
                }
            }
        }
        let target = self.block_label(entry_block);
        onlineasm!(*self.asm(); jmp {target};);
        Ok(())
    }

    /// Collects the extra arguments of a variadic entry into a vector, which
    /// is left in `T0`, and stores their count in `r[cnt]` as a fixnum.
    /// The helper returns the count as its status. It allocates without
    /// collecting, so the frame needs no livemask yet.
    ///
    /// An entry with exactly the required arguments
    /// has no extras: the rest register takes the canonical empty list (the
    /// marker `capy_opt_rest_list` and the deopt rest-area rebuild understand)
    /// and the count is a fixnum zero, without calling the helper or allocating
    /// a vector. The collecting body is the slow path.
    fn collect_rest(&mut self, rest: RestInfo) -> Result<(), String> {
        let required = rest.required as i64;

        let (slow, done) = (self.masm.label(), self.masm.label());

        onlineasm!(*self.asm(); bqneq {AC}, {required}, {slow};);
        let empty = crate::runtime::objectmodel::Value::null().bits() as i64;
        onlineasm!(*self.asm(); move {T0}, {empty};);
        onlineasm!(*self.asm(); move {T2}, 0;);
        self.masm.box_int32(T2, T2);
        let cnt_at = self.masm.assembler().mem(SFP, 8 * rest.cnt as i32);
        onlineasm!(*self.asm(); storeq {cnt_at}, {T2};);
        onlineasm!(*self.asm(); jmp {done};);
        self.masm.bind(slow);

        let _first = 8 * (2 + rest.required) as i64;

        self.collect_rest_inline(rest);
        self.masm.bind(done);

        return Ok(());
        /*
        onlineasm!(*self.asm();
            subp {T2}, {AC}, {required};
            addp {T3}, {SFP}, {first};
        );
        let call = OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::OptCollectRest),
            args: smallvec::smallvec![OpArg::Reg(T3), OpArg::Reg(T2)],
            preserve: smallvec::SmallVec::new(),
            result: OpResult::Reg(T0),
            checks_error: true,
            cont: OpContinue::Next,
        };
        emit_operation_call(
            self.masm,
            &call,
            &[],
            &mut |_: &mut Masm, _: OpArg, _: MReg| {
                unreachable!("the collect call's arguments are registers")
            },
        );
        self.masm.box_int32(T2, ERROR_REG);
        let cnt_at = self.masm.assembler().mem(SFP, 8 * rest.cnt as i32);
        onlineasm!(*self.asm(); storeq {cnt_at}, {T2};);
        if let Some(done) = done {
            self.masm.bind(done);
        }
        Ok(())
        */
    }

    /// The entry of a scalar-mode variadic procedure (`n` scalars). It reads
    /// every extra before the entry parameters are stored (a spill slot can lie
    /// over an extra): the slots of the first `n` extras that the call did not
    /// fill get `undefined` in place, so the parameter loads that follow read
    /// valid words from `fp[2 + required + i]`, and the extras from index `n`
    /// on go to a fresh vector, left in `T0` (the empty-list marker when there
    /// are none). The boxed count is left in `T1` for the entry to hand to the
    /// `cnt` parameter; `r[cnt]` is not written (a deopt rebuilds it from the
    /// state, and its slot may be the first extra's).
    fn collect_rest_scalar(&mut self, rest: RestInfo, n: u32) {
        let required = rest.required as i64;
        let first = 8 * (2 + rest.required) as i64;
        let undefined = crate::runtime::objectmodel::Value::undefined().bits() as i64;
        onlineasm!(*self.asm();
            subp {T3}, {AC}, {required};
            move {T1}, {undefined};
        );
        for i in 0..n {
            let filled = self.masm.label();
            let at = self
                .masm
                .assembler()
                .mem(SFP, (first + 8 * i64::from(i)) as i32);
            onlineasm!(*self.asm();
                bqaeq {T3}, {i64::from(i) + 1}, {filled};
                storeq {at}, {T1};
            );
            self.masm.bind(filled);
        }
        let (big, done) = (self.masm.label(), self.masm.label());
        onlineasm!(*self.asm(); bqa {T3}, {i64::from(n)}, {big};);
        let empty = crate::runtime::objectmodel::Value::null().bits() as i64;
        self.masm.box_int32(T1, T3);
        onlineasm!(*self.asm();
            move {T0}, {empty};
            jmp {done};
        );
        self.masm.bind(big);
        self.collect_rest_tail(rest, n, false);
        self.masm.bind(done);
    }

    /// The collecting body of [`Emitter::collect_rest`] without the helper
    /// call: bump-allocate a vector of `AC - required` words, copy the extras
    /// out of the incoming frame and store their count as a fixnum in
    /// `r[cnt]`. The vector is left in `T0`. Only the allocator's slow path
    /// (a full LAB or a large object) calls out, through the deferred block
    /// that `alloc_slow` emits; it keeps the count in `T3`. `AC` is free to
    /// use once the count is taken: nothing reads it after the entry.
    fn collect_rest_inline(&mut self, rest: RestInfo) {
        self.collect_rest_tail(rest, 0, true);
    }

    /// [`Emitter::collect_rest_inline`] for the extras from index `skip` on
    /// (there must be some): the vector holds only those, and `r[cnt]` still
    /// gets the full count.
    /// With `store_count` false `r[cnt]` is left alone and the boxed count is
    /// only left in `T1`.
    fn collect_rest_tail(&mut self, rest: RestInfo, skip: u32, store_count: bool) {
        let required = rest.required as i64 + i64::from(skip);
        let first = 8 * (2 + rest.required + skip) as i64;
        onlineasm!(*self.asm(); subp {T3}, {AC}, {required};);
        let header = header_word(ClassRef::VECTOR.class_id());
        let base = sabi::OBJECT_HEADER_BYTES + sabi::VECTOR_SIZE;
        let pending = self.gc.emit_alloc(
            self.masm,
            AllocSize::Words { count: T3, base },
            header,
            T0,
            T1,
            T2,
        );
        let (slow, join) = pending.labels();
        self.deferred.push(Deferred::Alloc {
            slow,
            join,
            obj: T0,
            size: AllocDeferred::Words { count: T3, base },
            header,
        });
        let length_at = self.masm.assembler().mem(T0, sabi::VECTOR_LENGTH as i32);
        onlineasm!(*self.asm(); storeq {length_at}, {T3};);
        // Copy the extras: `AC` walks the frame, `T2` the vector data.
        let (top, done) = (self.masm.label(), self.masm.label());
        let from = self.masm.assembler().mem(AC, 0);
        let to = self.masm.assembler().mem(T2, 0);
        onlineasm!(*self.asm();
            addp {AC}, {SFP}, {first};
            addp {T2}, {T0}, {sabi::VECTOR_DATA as i64};
        );
        self.masm.bind(top);
        onlineasm!(*self.asm();
            btqz {T3}, {done};
            loadq {T1}, {from};
            storeq {to}, {T1};
            addp {AC}, {AC}, 8;
            addp {T2}, {T2}, 8;
            subq {T3}, {T3}, 1;
            jmp {top};
        );
        self.masm.bind(done);
        // The count goes to `r[cnt]` last: that register may share a slot with
        // an extra, which the loop above had to read first.
        onlineasm!(*self.asm(); loadq {T1}, {length_at};);
        if skip != 0 {
            onlineasm!(*self.asm(); addp {T1}, {T1}, {i64::from(skip)};);
        }
        self.masm.box_int32(T1, T1);
        if store_count {
            let cnt_at = self.masm.assembler().mem(SFP, 8 * rest.cnt as i32);
            onlineasm!(*self.asm(); storeq {cnt_at}, {T1};);
        }
    }

    fn blocks(&mut self) -> Result<(), String> {
        let func = self.func;
        let alloc = self.alloc;
        for data in func.blocks.values() {
            for inst in &data.insts {
                for op in inst
                    .operands(func)
                    .into_iter()
                    .filter(|o| o.role == Role::Use)
                {
                    *self.use_counts.entry(op.vreg).or_default() += 1;
                }
            }
            if let Some(term) = &data.term {
                for op in term
                    .operands(func)
                    .into_iter()
                    .filter(|o| o.role == Role::Use)
                {
                    *self.use_counts.entry(op.vreg).or_default() += 1;
                }
            }
        }
        // Alignment policy (`CAPY_OPT_ALIGN`). Indirect-branch destinations
        // and loop headers are where placement decides throughput: the
        // opcode dispatch benchmark swings between 0.56 s and 0.87 s across
        // register assignments with identical code, purely from where its 64
        // handlers land. The defaults give loop headers a cache line and
        // switch targets a stable 16-byte start; both are tunable because the
        // best boundary is microarchitecture-specific.
        let (loop_align, switch_align) = block_alignment();
        let position: HashMap<LBlock, usize> = alloc
            .order
            .iter()
            .enumerate()
            .map(|(i, &block)| (block, i))
            .collect();
        let mut switch_targets: std::collections::HashSet<LBlock> =
            std::collections::HashSet::new();
        let mut loop_headers: std::collections::HashSet<LBlock> = std::collections::HashSet::new();
        for (block, data) in func.blocks.iter() {
            let Some(term) = &data.term else {
                continue;
            };
            if let LirTerm::Switch { cases, default, .. } = term {
                switch_targets.insert(default.block);
                for (_, edge) in cases {
                    switch_targets.insert(edge.block);
                }
            }
            let Some(&here) = position.get(&block) else {
                continue;
            };
            for edge in term.edges() {
                if position
                    .get(&edge.block)
                    .is_some_and(|&there| there <= here)
                {
                    loop_headers.insert(edge.block);
                }
            }
        }
        for (i, &b) in alloc.order.iter().enumerate() {
            self.next_block = alloc.order.get(i + 1).copied();
            let label = self.block_label(b);
            if loop_headers.contains(&b) && loop_align >= 4 {
                self.masm.align(loop_align);
            } else if switch_targets.contains(&b) && switch_align >= 4 {
                self.masm.align(switch_align);
            }
            self.masm.bind(label);
            let data = &func.blocks[b];
            let n = data.insts.len();
            // A compare that only feeds this block's branch folds into it,
            // when no move sits between the two.
            self.fused = None;
            if let (Some(last), Some(LirTerm::Branch { cond, .. })) =
                (data.insts.last(), data.term.as_ref())
                && alloc.moves_before(b, n as u32).is_empty()
                && self.use_counts.get(cond).copied() == Some(1)
            {
                let at = alloc.index(b, n as u32 - 1);
                self.fused = match last {
                    LirInst::IntCmp { cmp, dst, lhs, rhs }
                        if dst == cond && func.class(*lhs) == RegClass::Gpr =>
                    {
                        Some(Fused::Cmp(*cmp, *lhs, *rhs, at))
                    }
                    LirInst::TestTag { dst, src, test }
                        if dst == cond && func.class(*src) == RegClass::Gpr =>
                    {
                        Some(Fused::Test(*test, *src, at))
                    }
                    _ => None,
                };
            }
            for (k, inst) in data.insts.iter().enumerate() {
                self.g = alloc.index(b, k as u32);
                self.mark();
                self.sequence(alloc.moves_before(b, k as u32), &alloc.consts)?;
                if self.fused.is_some() && k + 1 == n {
                    continue;
                }
                self.inst(inst)?;
            }
            self.g = alloc.index(b, n as u32);
            self.mark();
            let term = data
                .term
                .as_ref()
                .ok_or_else(|| format!("{b} has no terminator"))?;
            self.sequence(alloc.moves_before(b, n as u32), &alloc.consts)?;
            self.term(term)?;
        }
        Ok(())
    }

    fn inst(&mut self, inst: &LirInst) -> Result<(), String> {
        let uses_fpr = inst
            .operands(self.func)
            .iter()
            .any(|op| self.func.class(op.vreg) == RegClass::Fpr);
        let second = self.both_spilled(inst);
        if uses_fpr {
            self.fpr_scope_begin(second);
        }
        let result = self.inst_body(inst);
        if uses_fpr {
            self.fpr_scope_end(second);
        }
        result
    }

    /// Whether `inst` is a float operation whose two operands are both
    /// spilled. It needs the second FPR temporary.
    fn both_spilled(&self, inst: &LirInst) -> bool {
        let (lhs, rhs) = match inst {
            LirInst::FloatArith { lhs, rhs, .. } | LirInst::FloatCmp { lhs, rhs, .. } => {
                (*lhs, *rhs)
            }
            _ => return false,
        };
        matches!(
            (self.loc(lhs, self.use_pos()), self.loc(rhs, self.use_pos())),
            (Ok(Loc::Slot(_)), Ok(Loc::Slot(_)))
        )
    }

    /// `acc = lhs op rhs` with both operands spilled. The right operand goes
    /// in a second FPR: `%xmm8` on x86-64, outside the pool and caller-saved,
    /// and `v6` on AArch64, outside the pool and caller-saved.
    fn spilled_float_arith(&mut self, op: FloatOp, acc: MReg, lhs: u32, rhs: u32) {
        let (ml, mr) = (self.slot_mem(lhs), self.slot_mem(rhs));
        #[cfg(target_arch = "x86_64")]
        {
            onlineasm!(*self.asm(); loadd {acc}, {ml}; loadd %xmm8, {mr};);
            match op {
                FloatOp::Add => onlineasm!(*self.asm(); addd {acc}, {acc}, %xmm8;),
                FloatOp::Sub => onlineasm!(*self.asm(); subd {acc}, {acc}, %xmm8;),
                FloatOp::Mul => onlineasm!(*self.asm(); muld {acc}, {acc}, %xmm8;),
                FloatOp::Div => onlineasm!(*self.asm(); divd {acc}, {acc}, %xmm8;),
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            let t2 = onlineasm::regs::v6;
            onlineasm!(*self.asm(); loadd {acc}, {ml}; loadd {t2}, {mr};);
            match op {
                FloatOp::Add => onlineasm!(*self.asm(); addd {acc}, {acc}, {t2};),
                FloatOp::Sub => onlineasm!(*self.asm(); subd {acc}, {acc}, {t2};),
                FloatOp::Mul => onlineasm!(*self.asm(); muld {acc}, {acc}, {t2};),
                FloatOp::Div => onlineasm!(*self.asm(); divd {acc}, {acc}, {t2};),
            }
        }
    }

    /// `d = lhs cmp rhs` with both operands spilled. See
    /// [`Emitter::spilled_float_arith`] for the second FPR.
    fn spilled_float_cmp(&mut self, cmp: Cmp, d: MReg, acc: MReg, lhs: u32, rhs: u32) {
        let (ml, mr) = (self.slot_mem(lhs), self.slot_mem(rhs));
        #[cfg(target_arch = "x86_64")]
        {
            onlineasm!(*self.asm(); loadd {acc}, {ml}; loadd %xmm8, {mr};);
            match cmp {
                Cmp::Eq => onlineasm!(*self.asm(); cdeq {d}, {acc}, %xmm8;),
                Cmp::Ne => onlineasm!(*self.asm(); cdnequn {d}, {acc}, %xmm8;),
                Cmp::Lt => onlineasm!(*self.asm(); cdlt {d}, {acc}, %xmm8;),
                Cmp::Le => onlineasm!(*self.asm(); cdlteq {d}, {acc}, %xmm8;),
                Cmp::Gt => onlineasm!(*self.asm(); cdgt {d}, {acc}, %xmm8;),
                Cmp::Ge => onlineasm!(*self.asm(); cdgteq {d}, {acc}, %xmm8;),
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            let t2 = onlineasm::regs::v6;
            onlineasm!(*self.asm(); loadd {acc}, {ml}; loadd {t2}, {mr};);
            match cmp {
                Cmp::Eq => onlineasm!(*self.asm(); cdeq {d}, {acc}, {t2};),
                Cmp::Ne => onlineasm!(*self.asm(); cdnequn {d}, {acc}, {t2};),
                Cmp::Lt => onlineasm!(*self.asm(); cdlt {d}, {acc}, {t2};),
                Cmp::Le => onlineasm!(*self.asm(); cdlteq {d}, {acc}, {t2};),
                Cmp::Gt => onlineasm!(*self.asm(); cdgt {d}, {acc}, {t2};),
                Cmp::Ge => onlineasm!(*self.asm(); cdgteq {d}, {acc}, {t2};),
            }
        }
    }

    fn inst_body(&mut self, inst: &LirInst) -> Result<(), String> {
        match inst {
            LirInst::Move { dst, src } => {
                let s = self.loc(*src, self.use_pos())?;
                let d = self.loc(*dst, self.def_pos())?;
                self.move_loc(s, d)?;
            }
            LirInst::Const { dst, value } => {
                let t = self.write(*dst)?;
                match *value {
                    Constant::Int32(i) => {
                        let v = i as i64;
                        onlineasm!(*self.asm(); move {t.reg}, {v};);
                    }
                    Constant::Int64(i) => {
                        // A word that needs more than one `movz`/`movabs`
                        // chunk is smaller as one PC-relative island load.
                        if i32::try_from(i).is_ok() {
                            onlineasm!(*self.asm(); move {t.reg}, {i};);
                        } else {
                            self.const_pool_load(t.reg, i as u64);
                        }
                    }
                    Constant::Bits(bits) => {
                        if i32::try_from(bits as i64).is_ok() {
                            let v = bits as i64;
                            onlineasm!(*self.asm(); move {t.reg}, {v};);
                        } else {
                            self.const_pool_load(t.reg, bits);
                        }
                    }
                    Constant::Double(bits) => {
                        // Raw IEEE bits through one island load, then move
                        // them to the float register.
                        self.const_pool_load(T0, bits);
                        onlineasm!(*self.asm(); fq2d {t.reg}, {T0};);
                    }
                    Constant::Pool { code, index } => {
                        if code != CodeRef::new(0) {
                            return Err("a constant of an inlined code block is not supported yet"
                                .to_string());
                        }
                        self.masm.load_import(
                            T0,
                            ImportKey::Block(0, Section::Bytecode, -8 * (3 + index as i32)),
                        );
                        let mem = self.masm.assembler().mem(T0, 0);
                        onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                    }
                    Constant::Block {
                        code,
                        section,
                        offset,
                    } => {
                        self.masm
                            .load_import(t.reg, ImportKey::Block(code.index() as u32, section, offset));
                    }
                    Constant::Table(index) => {
                        self.load_constant_addr(T0, index)?;
                        let mem = self.masm.assembler().mem(T0, 0);
                        onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                    }
                }
                self.commit(t);
            }
            LirInst::TagInt { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                self.masm.box_int32(t.reg, s);
                self.commit(t);
            }
            LirInst::UntagInt { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                self.masm.unbox_int32(t.reg, s);
                self.commit(t);
            }
            LirInst::BoolToValue { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                self.masm.bool_to_value(t.reg, s);
                self.commit(t);
            }
            LirInst::UntagChar { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                let d = t.reg;
                onlineasm!(*self.asm();
                    move {d}, {s};
                    urshiftq {d}, 16;
                    andq {d}, {d}, 0xffffffff;
                );
                self.commit(t);
            }
            LirInst::Guard { test, src, exit } => {
                let fail = self.exit_label(*exit);
                // Under CAPY_OPT_STRESS_DEOPT the guard jumps to its exit
                // unconditionally instead of testing.
                if !self.stress_guard(fail) {
                    match src {
                        Some(v) => {
                            let s = self.read(*v, T0)?;
                            self.branch_unless(*test, s, fail)?;
                        }
                        None => self.branch_invalid(fail),
                    }
                }
            }
            LirInst::GuardIndex {
                index,
                length,
                exit,
            } => {
                let fail = self.exit_label(*exit);
                if !self.stress_guard(fail) {
                    let i = self.read(*index, T0)?;
                    let n = self.read(*length, T1)?;
                    // Branch to the exit when `index >= length` unsigned. A
                    // negative fixnum payload is a huge unsigned word, so this
                    // one comparison also rejects it.
                    onlineasm!(*self.asm(); bqaeq {i}, {n}, {fail};);
                }
            }
            LirInst::GuardCmp {
                cmp,
                lhs,
                rhs,
                exit,
            } => {
                let fail = self.exit_label(*exit);
                if !self.stress_guard(fail) {
                    let l = self.read(*lhs, T0)?;
                    let r = self.read(*rhs, T1)?;
                    // Branch to the exit when the comparison is false.
                    match cmp {
                        Cmp::Eq => onlineasm!(*self.asm(); bqneq {l}, {r}, {fail};),
                        Cmp::Ne => onlineasm!(*self.asm(); bqeq {l}, {r}, {fail};),
                        Cmp::Lt => onlineasm!(*self.asm(); bqgteq {l}, {r}, {fail};),
                        Cmp::Le => onlineasm!(*self.asm(); bqgt {l}, {r}, {fail};),
                        Cmp::Gt => onlineasm!(*self.asm(); bqlteq {l}, {r}, {fail};),
                        Cmp::Ge => onlineasm!(*self.asm(); bqlt {l}, {r}, {fail};),
                    }
                }
            }
            LirInst::TestTag { dst, src, test } => {
                let s = self.read(*src, T0)?;
                let skip = self.masm.label();
                onlineasm!(*self.asm(); move {T3}, 0;);
                self.branch_unless(*test, s, skip)?;
                onlineasm!(*self.asm(); move {T3}, 1;);
                self.masm.bind(skip);
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {T3};);
                self.commit(t);
            }
            LirInst::IntArith {
                op,
                dst,
                lhs,
                rhs,
                exit,
            } => {
                // The op runs on a copy, so the operands survive an overflow
                // for the exit's frame.
                let l = self.read(*lhs, T0)?;
                let r = self.read(*rhs, T1)?;
                let fail = self.exit_label(*exit);
                onlineasm!(*self.asm(); move {T2}, {l};);
                match op {
                    IntOp::Add => onlineasm!(*self.asm(); baddio {T2}, {T2}, {r}, {fail};),
                    IntOp::Sub => onlineasm!(*self.asm(); bsubio {T2}, {T2}, {r}, {fail};),
                    IntOp::Mul => onlineasm!(*self.asm(); bmulio {T2}, {T2}, {r}, {fail};),
                    IntOp::Quot | IntOp::Rem => {
                        // The three-operand divide expands on both targets:
                        // x86-64 stages it through rax/rdx, which it saves and
                        // restores, and only `r11` (outside the pool) is
                        // scratch. A zero divisor deopts, and so does MIN / -1
                        // for a quotient, which would not fit an int32.
                        onlineasm!(*self.asm(); bqeq {r}, 0, {fail};);
                        if *op == IntOp::Quot {
                            let ok = self.masm.label();
                            onlineasm!(*self.asm();
                                bqneq {T2}, {i32::MIN as i64}, {ok};
                                bqeq {r}, {-1i64}, {fail};
                            );
                            self.masm.bind(ok);
                            onlineasm!(*self.asm(); divis {T2}, {T2}, {r};);
                        } else {
                            onlineasm!(*self.asm(); remis {T2}, {T2}, {r};);
                        }
                    }
                    IntOp::Mod => {
                        // The remainder takes the dividend's sign. A nonzero
                        // remainder with the other sign from the divisor
                        // moves by the divisor, which lands in the int32
                        // range.
                        let negative = self.masm.label();
                        let adjust = self.masm.label();
                        let done = self.masm.label();
                        onlineasm!(*self.asm(); bqeq {r}, 0, {fail};);
                        onlineasm!(*self.asm(); remis {T2}, {T2}, {r};);
                        onlineasm!(*self.asm(); sxi2q {T2}, {T2}; move {T0}, 0;);
                        onlineasm!(*self.asm();
                            bqeq {T2}, {T0}, {done};
                            bilt {T2}, {T0}, {negative};
                            bilt {r}, {T0}, {adjust};
                            jmp {done};
                        );
                        self.masm.bind(negative);
                        onlineasm!(*self.asm(); bilt {r}, {T0}, {done};);
                        self.masm.bind(adjust);
                        onlineasm!(*self.asm(); addi {T2}, {T2}, {r};);
                        self.masm.bind(done);
                    }
                }
                // Normalize the int32 result to a full sign-extended word.
                onlineasm!(*self.asm(); sxi2q {T2}, {T2};);
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {T2};);
                self.commit(t);
            }
            LirInst::Int64Wrapping { op, dst, lhs, rhs } => {
                let l = self.read(*lhs, T0)?;
                let r = self.read(*rhs, T1)?;
                let t = self.write(*dst)?;
                match op {
                    IntOp::Add => onlineasm!(*self.asm(); addq {t.reg}, {l}, {r};),
                    IntOp::Sub => onlineasm!(*self.asm(); subq {t.reg}, {l}, {r};),
                    // Only the total add, subtract and multiply use this.
                    _ => onlineasm!(*self.asm(); mulq {t.reg}, {l}, {r};),
                }
                self.commit(t);
            }
            LirInst::Int64Arith {
                op,
                dst,
                lhs,
                rhs,
                exit,
            } => {
                // The op runs on a copy, so the operands survive an overflow
                // for the exit's frame.
                let l = self.read(*lhs, T0)?;
                let r = self.read(*rhs, T1)?;
                let fail = self.exit_label(*exit);
                onlineasm!(*self.asm(); move {T2}, {l};);
                match op {
                    IntOp::Add => onlineasm!(*self.asm(); baddqo {T2}, {T2}, {r}, {fail};),
                    IntOp::Sub => onlineasm!(*self.asm(); bsubqo {T2}, {T2}, {r}, {fail};),
                    IntOp::Mul => onlineasm!(*self.asm(); bmulqo {T2}, {T2}, {r}, {fail};),
                    IntOp::Quot => {
                        // The divide expands on both targets (x86-64 saves
                        // rax/rdx around it), so no fixed registers are
                        // needed. A zero divisor deopts, and so does
                        // `MIN / -1`, which traps in hardware.
                        let ok = self.masm.label();
                        onlineasm!(*self.asm(); bqeq {r}, 0, {fail};);
                        onlineasm!(*self.asm(); move {T0}, {i64::MIN};);
                        onlineasm!(*self.asm();
                            bqneq {T2}, {T0}, {ok};
                            bqeq {r}, {-1i64}, {fail};
                        );
                        self.masm.bind(ok);
                        onlineasm!(*self.asm(); divqs {T2}, {T2}, {r};);
                    }
                    IntOp::Rem => {
                        onlineasm!(*self.asm(); bqeq {r}, 0, {fail};);
                        onlineasm!(*self.asm(); remqs {T2}, {T2}, {r};);
                    }
                    IntOp::Mod => {
                        // As the int32 modulo, but in 64 bits.
                        let negative = self.masm.label();
                        let adjust = self.masm.label();
                        let done = self.masm.label();
                        onlineasm!(*self.asm(); bqeq {r}, 0, {fail};);
                        onlineasm!(*self.asm(); remqs {T2}, {T2}, {r}; move {T0}, 0;);
                        onlineasm!(*self.asm();
                            bqeq {T2}, {T0}, {done};
                            bqlt {T2}, {T0}, {negative};
                            bqlt {r}, {T0}, {adjust};
                            jmp {done};
                        );
                        self.masm.bind(negative);
                        onlineasm!(*self.asm(); bqlt {r}, {T0}, {done};);
                        self.masm.bind(adjust);
                        onlineasm!(*self.asm(); addq {T2}, {T2}, {r};);
                        self.masm.bind(done);
                    }
                }
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {T2};);
                self.commit(t);
            }
            LirInst::BoxInt64 { dst, src } => {
                // Tags a fixnum when the value fits i32 in the hot path, and
                // branches to a cold block that allocates a one-limb
                // sign-magnitude BigInt (class 22, 8-byte header and a
                // 16-byte `{count, negative, pad, word}` payload) otherwise.
                let s = self.read(*src, T1)?;
                let src_loc = self.loc(*src, self.use_pos())?;
                let src = match src_loc {
                    Loc::Reg(p) => BoxSrc::Reg(self.gpr_of(p)?),
                    Loc::Slot(slot) => BoxSrc::Slot(slot),
                };
                let t = self.write(*dst)?;
                let wide = self.masm.label();
                let done = self.masm.label();
                // Fits i32 exactly when sign-extending the low 32 bits gives
                // the value back. That extension is also the untagged fixnum,
                // so the fast path only has to add the tag.
                onlineasm!(*self.asm(); sxi2q {T2}, {s}; bqneq {T2}, {s}, {wide};);
                onlineasm!(*self.asm(); orq {t.reg}, {T2}, {regs::NUMBER_TAG};);
                self.commit(t);
                self.masm.bind(done);
                self.deferred.push(Deferred::BoxBigInt {
                    wide,
                    done,
                    obj: t.reg,
                    slot: t.slot,
                    src,
                    bytes: sabi::OBJECT_HEADER_BYTES + 16,
                    header: header_word(builtin_class_ids::BIGINT),
                });
            }
            LirInst::UnboxBigInt64 { dst, obj, exit } => {
                // A fixnum's payload is already the value; sign-extend it.
                // Anything else must be a one-limb sign-magnitude BIGINT:
                // guard the class and the shape, and produce the signed
                // 64-bit value. Every failure deopts before the bytecode
                // instruction, so baseline runs it.
                let fail = self.exit_label(*exit);
                let o = self.read(*obj, T0)?;
                let t = self.write(*dst)?;
                let d = t.reg;
                if !self.stress_guard(fail) {
                    let bigint = self.masm.label();
                    let done = self.masm.label();
                    self.branch_unless(Test::Fixnum, o, bigint)?;
                    onlineasm!(*self.asm(); sxi2q {d}, {o};);
                    onlineasm!(*self.asm(); jmp {done};);
                    self.masm.bind(bigint);
                    self.branch_unless(Test::Class(ClassRef::BIGINT), o, fail)?;
                    // Pick scratch registers that are not the object (needed
                    // for the limb load) or the destination.
                    let temps = [T0, T1, T2, T3];
                    let w = *temps.iter().find(|&&r| r != o).expect("a scratch register");
                    let c = *temps
                        .iter()
                        .find(|&&r| r != o && r != w && r != d)
                        .or_else(|| temps.iter().find(|&&r| r != o && r != w))
                        .expect("a second scratch register");
                    let m = *temps
                        .iter()
                        .find(|&&r| r != w && r != c)
                        .expect("a third scratch register");

                    let word_at = self.masm.assembler().mem(o, 0);
                    onlineasm!(*self.asm(); loadq {w}, {word_at};);
                    // count lives in the low 32 bits of the payload word.
                    onlineasm!(*self.asm(); andq {c}, {w}, 0xffffffff;);
                    onlineasm!(*self.asm(); bqa {c}, 1, {fail};);
                    let zero = self.masm.label();
                    let have = self.masm.label();
                    onlineasm!(*self.asm(); bqeq {c}, 0, {zero};);
                    let limb_at = self.masm.assembler().mem(o, 8);
                    onlineasm!(*self.asm(); loadq {c}, {limb_at};);
                    onlineasm!(*self.asm(); jmp {have};);
                    self.masm.bind(zero);
                    onlineasm!(*self.asm(); move {c}, 0;);
                    self.masm.bind(have);
                    // sign = bit 32 of the payload word.
                    onlineasm!(*self.asm();
                        move {m}, {w};
                        urshiftq {m}, 32;
                        andq {m}, {m}, 1;
                        negq {m};
                        // value = (limb ^ mask) - mask.
                        xorq {c}, {c}, {m};
                        subq {c}, {c}, {m};
                    );
                    // A positive magnitude above i64::MAX, or a negative one
                    // above 2^63, wrapped the wrong way; reject it so
                    // `try_as_i64` semantics hold.
                    let nonnegative = self.masm.label();
                    let valid = self.masm.label();
                    onlineasm!(*self.asm(); bqeq {m}, 0, {nonnegative};);
                    onlineasm!(*self.asm(); bqgt {c}, 0, {fail}; jmp {valid};);
                    self.masm.bind(nonnegative);
                    onlineasm!(*self.asm(); bqlt {c}, 0, {fail};);
                    self.masm.bind(valid);
                    onlineasm!(*self.asm(); move {d}, {c};);
                    self.masm.bind(done);
                }
                self.commit(t);
            }
            LirInst::UntagInt64 { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); sxi2q {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::TruncateI64ToI32 { dst, src } => {
                // The low 32 bits are already the int32 value; a register
                // move keeps the same word and int32 ops ignore the top half.
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::Int64ToDouble { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                // `ci2d` is the signed 64-bit to f64 conversion (the raw
                // int32 form is sign-extended in its word, so it uses the
                // same instruction).
                onlineasm!(*self.asm(); ci2d {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::FloorF64 { dst, src } => {
                let s = self.read(*src, self.fpr_temp())?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); floord {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::CeilingF64 { dst, src } => {
                let s = self.read(*src, self.fpr_temp())?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); ceild {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::TruncateF64 { dst, src } => {
                let s = self.read(*src, self.fpr_temp())?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); truncated {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::SqrtF64 { dst, src } => {
                let s = self.read(*src, self.fpr_temp())?;
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); sqrtd {t.reg}, {s};);
                self.commit(t);
            }
            LirInst::IntCmp { cmp, dst, lhs, rhs } => {
                let l = self.read(*lhs, T0)?;
                let r = self.read(*rhs, T1)?;
                let t = self.write(*dst)?;
                let d = t.reg;
                match cmp {
                    Cmp::Eq => onlineasm!(*self.asm(); cqeq {d}, {l}, {r};),
                    Cmp::Ne => onlineasm!(*self.asm(); cqneq {d}, {l}, {r};),
                    Cmp::Lt => onlineasm!(*self.asm(); cqlt {d}, {l}, {r};),
                    Cmp::Le => onlineasm!(*self.asm(); cqlteq {d}, {l}, {r};),
                    Cmp::Gt => onlineasm!(*self.asm(); cqgt {d}, {l}, {r};),
                    Cmp::Ge => onlineasm!(*self.asm(); cqgteq {d}, {l}, {r};),
                }
                self.commit(t);
            }
            LirInst::FloatArith { op, dst, lhs, rhs } => {
                // The destination is tied to the left operand, so the op runs
                // in the left operand's register, or in the FPR temporary when
                // that operand is spilled.
                let tmp = self.fpr_temp();
                match (
                    self.loc(*lhs, self.use_pos())?,
                    self.loc(*rhs, self.use_pos())?,
                ) {
                    (Loc::Slot(l), Loc::Slot(r)) => self.spilled_float_arith(*op, tmp, l, r),
                    (lhs_loc, rhs_loc) => {
                        let (acc, from_slot) = match lhs_loc {
                            Loc::Reg(p) => (self.fpr_of(p)?, None),
                            Loc::Slot(s) => (tmp, Some(s)),
                        };
                        if let Some(s) = from_slot {
                            let mem = self.slot_mem(s);
                            onlineasm!(*self.asm(); loadd {acc}, {mem};);
                        }
                        let r = match rhs_loc {
                            Loc::Reg(p) => self.fpr_of(p)?,
                            Loc::Slot(s) => {
                                let mem = self.slot_mem(s);
                                onlineasm!(*self.asm(); loadd {tmp}, {mem};);
                                tmp
                            }
                        };
                        match op {
                            FloatOp::Add => onlineasm!(*self.asm(); addd {acc}, {acc}, {r};),
                            FloatOp::Sub => onlineasm!(*self.asm(); subd {acc}, {acc}, {r};),
                            FloatOp::Mul => onlineasm!(*self.asm(); muld {acc}, {acc}, {r};),
                            FloatOp::Div => onlineasm!(*self.asm(); divd {acc}, {acc}, {r};),
                        }
                        match self.loc(*dst, self.def_pos())? {
                            Loc::Reg(p) => {
                                let d = self.fpr_of(p)?;
                                if d != acc {
                                    onlineasm!(*self.asm(); moved {d}, {acc};);
                                }
                            }
                            Loc::Slot(s) => {
                                let mem = self.slot_mem(s);
                                onlineasm!(*self.asm(); stored {mem}, {acc};);
                            }
                        }
                    }
                }
            }
            LirInst::FloatCmp { cmp, dst, lhs, rhs } => {
                let tmp = self.fpr_temp();
                let t = self.write(*dst)?;
                let d = t.reg;
                match (
                    self.loc(*lhs, self.use_pos())?,
                    self.loc(*rhs, self.use_pos())?,
                ) {
                    (Loc::Slot(l), Loc::Slot(r)) => self.spilled_float_cmp(*cmp, d, tmp, l, r),
                    (lhs_loc, rhs_loc) => {
                        let l = match lhs_loc {
                            Loc::Reg(p) => self.fpr_of(p)?,
                            Loc::Slot(s) => {
                                let mem = self.slot_mem(s);
                                onlineasm!(*self.asm(); loadd {tmp}, {mem};);
                                tmp
                            }
                        };
                        let r = match rhs_loc {
                            Loc::Reg(p) => self.fpr_of(p)?,
                            Loc::Slot(s) => {
                                let mem = self.slot_mem(s);
                                onlineasm!(*self.asm(); loadd {tmp}, {mem};);
                                tmp
                            }
                        };
                        match cmp {
                            Cmp::Eq => onlineasm!(*self.asm(); cdeq {d}, {l}, {r};),
                            Cmp::Ne => onlineasm!(*self.asm(); cdnequn {d}, {l}, {r};),
                            Cmp::Lt => onlineasm!(*self.asm(); cdlt {d}, {l}, {r};),
                            Cmp::Le => onlineasm!(*self.asm(); cdlteq {d}, {l}, {r};),
                            Cmp::Gt => onlineasm!(*self.asm(); cdgt {d}, {l}, {r};),
                            Cmp::Ge => onlineasm!(*self.asm(); cdgteq {d}, {l}, {r};),
                        }
                    }
                }
                self.commit(t);
            }
            LirInst::LoadFlonum { dst, obj } => {
                let o = self.read(*obj, T0)?;
                let t = self.write(*dst)?;
                self.masm.unbox_double_fpr(t.reg, o, T1);
                self.commit(t);
            }
            LirInst::IntToDouble { dst, src } => {
                let s = self.read(*src, T0)?;
                let t = self.write(*dst)?;
                self.masm.int32_to_double_fpr(t.reg, s);
                self.commit(t);
            }
            LirInst::StoreFlonum { obj, src } => {
                let o = self.read(*obj, T0)?;
                let s = self.read(*src, self.fpr_temp())?;
                let mem = self.masm.assembler().mem(o, 0);
                onlineasm!(*self.asm(); stored {mem}, {s};);
            }
            LirInst::BoxDouble { dst, src } => {
                let s = self.read(*src, self.fpr_temp())?;
                let t = self.write(*dst)?;
                self.masm.box_double_fpr(t.reg, s);
                self.commit(t);
            }
            LirInst::Alloc { dst, layout } => self.alloc(*dst, *layout)?,
            LirInst::AllocVectorFill {
                dst,
                size,
                fill,
                exit,
            } => self.alloc_vector_fill(*dst, *size, *fill, *exit)?,
            LirInst::Load { dst, base, offset } => {
                let b = self.read(*base, T0)?;
                let t = self.write(*dst)?;
                let mem = self.masm.assembler().mem(b, *offset);
                onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::Store { base, offset, src } => {
                let b = self.read(*base, T0)?;
                let s = self.read(*src, T1)?;
                let mem = self.masm.assembler().mem(b, *offset);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            LirInst::LoadField { dst, obj, field } => {
                let o = self.read(*obj, T0)?;
                let t = self.write(*dst)?;
                let mem = self.masm.assembler().mem(o, 8 * *field as i32);
                onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::StoreField { obj, field, src } => {
                let o = self.read(*obj, T0)?;
                let s = self.read(*src, T1)?;
                let mem = self.masm.assembler().mem(o, 8 * *field as i32);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            LirInst::StoreNumberTagged {
                obj,
                offset,
                src,
                kind,
                exit,
            } => {
                // Writes one inline Number of a fresh rational. The tag word
                // is 0 with the sign-extended i32 payload for a fixnum, or 2
                // with the Gc pointer for a one-limb BigInt; anything else
                // deopts before the instruction. No barrier: the object is
                // fresh and no safepoint runs between the allocation and this
                // store. `kind` says what the lowering proved about `src`, so
                // a fixnum constant or `box.int32` needs no test and
                // `box.int64` needs no class test on its wide path.
                let o = self.read(*obj, T0)?;
                let s = self.read(*src, T3)?;
                let fail = self.exit_label(*exit);
                if self.stress_guard(fail) {
                    // The guard ordinal stays stable: a stress run deopts
                    // here even though the proved cases cannot fail.
                } else {
                    match kind {
                        NumberStoreKind::Fixnum => {
                            onlineasm!(*self.asm(); move {T1}, 0;);
                            let tag_at = self.masm.assembler().mem(o, *offset as i32);
                            onlineasm!(*self.asm(); storeq {tag_at}, {T1};);
                            onlineasm!(*self.asm(); sxi2q {T1}, {s};);
                            let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                            onlineasm!(*self.asm(); storeq {payload_at}, {T1};);
                        }
                        NumberStoreKind::Number => {
                            let wide = self.masm.label();
                            let done = self.masm.label();
                            self.masm.branch_if_not_fixnum(s, T1, wide);
                            onlineasm!(*self.asm(); move {T1}, 0;);
                            let tag_at = self.masm.assembler().mem(o, *offset as i32);
                            onlineasm!(*self.asm(); storeq {tag_at}, {T1};);
                            onlineasm!(*self.asm(); sxi2q {T1}, {s};);
                            let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                            onlineasm!(*self.asm(); storeq {payload_at}, {T1}; jmp {done};);
                            self.masm.bind(wide);
                            onlineasm!(*self.asm(); move {T1}, 2;);
                            let tag_at = self.masm.assembler().mem(o, *offset as i32);
                            onlineasm!(*self.asm(); storeq {tag_at}, {T1};);
                            let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                            onlineasm!(*self.asm(); storeq {payload_at}, {s};);
                            self.masm.bind(done);
                        }
                        NumberStoreKind::Any => {
                            let wide = self.masm.label();
                            let done = self.masm.label();
                            self.masm.branch_if_not_fixnum(s, T1, wide);
                            onlineasm!(*self.asm(); move {T1}, 0;);
                            let tag_at = self.masm.assembler().mem(o, *offset as i32);
                            onlineasm!(*self.asm(); storeq {tag_at}, {T1};);
                            onlineasm!(*self.asm(); sxi2q {T1}, {s};);
                            let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                            onlineasm!(*self.asm(); storeq {payload_at}, {T1}; jmp {done};);
                            self.masm.bind(wide);
                            self.branch_unless(Test::Class(ClassRef::BIGINT), s, fail)?;
                            onlineasm!(*self.asm(); move {T1}, 2;);
                            let tag_at = self.masm.assembler().mem(o, *offset as i32);
                            onlineasm!(*self.asm(); storeq {tag_at}, {T1};);
                            let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                            onlineasm!(*self.asm(); storeq {payload_at}, {s};);
                            self.masm.bind(done);
                        }
                    }
                }
            }
            LirInst::LoadNumberFixnum {
                dst,
                obj,
                offset,
                exit,
            } => {
                // A rational's component: guard the stable Number tag (0 for
                // a fixnum), then sign-extend the low 32 bits of the payload.
                let o = self.read(*obj, T0)?;
                let t = self.write(*dst)?;
                let fail = self.exit_label(*exit);
                if !self.stress_guard(fail) {
                    let tag_at = self.masm.assembler().mem(o, *offset as i32);
                    onlineasm!(*self.asm(); loadq {T1}, {tag_at}; bqneq {T1}, 0, {fail};);
                    let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                    onlineasm!(*self.asm(); loadq {T1}, {payload_at};);
                    onlineasm!(*self.asm(); sxi2q {t.reg}, {T1};);
                }
                self.commit(t);
            }
            LirInst::LoadNumberWide {
                dst,
                obj,
                offset,
                exit,
            } => {
                let o = self.read(*obj, T0)?;
                let t = self.write(*dst)?;
                let fail = self.exit_label(*exit);
                if !self.stress_guard(fail) {
                    let fixnum = self.masm.label();
                    let zero = self.masm.label();
                    let have = self.masm.label();
                    let nonnegative = self.masm.label();
                    let valid = self.masm.label();
                    let done = self.masm.label();
                    let tag_at = self.masm.assembler().mem(o, *offset as i32);
                    onlineasm!(*self.asm(); loadq {T1}, {tag_at}; bqeq {T1}, 0, {fixnum};);
                    onlineasm!(*self.asm(); bqneq {T1}, 2, {fail};);
                    let ptr_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                    onlineasm!(*self.asm(); loadq {T3}, {ptr_at};);
                    // `o` is dead now, so `T0` is free for the count and the
                    // limb even when the object arrived in it.
                    let word_at = self.masm.assembler().mem(T3, 0);
                    onlineasm!(*self.asm(); loadq {T1}, {word_at}; zxi2q {T0}, {T1};);
                    onlineasm!(*self.asm(); bqa {T0}, 1, {fail};);
                    onlineasm!(*self.asm(); bqeq {T0}, 0, {zero};);
                    let limb_at = self.masm.assembler().mem(T3, 8);
                    onlineasm!(*self.asm(); loadq {T0}, {limb_at}; jmp {have};);
                    self.masm.bind(zero);
                    onlineasm!(*self.asm(); move {T0}, 0;);
                    self.masm.bind(have);
                    // The sign bit is bit 32 of the payload word.
                    onlineasm!(*self.asm();
                        move {T3}, {T1};
                        urshiftq {T3}, 32;
                        andq {T3}, {T3}, 1;
                        negq {T3};
                        xorq {T0}, {T0}, {T3};
                        subq {T0}, {T0}, {T3};
                    );
                    onlineasm!(*self.asm(); btqz {T3}, {nonnegative}; bqgt {T0}, 0, {fail}; jmp {valid};);
                    self.masm.bind(nonnegative);
                    onlineasm!(*self.asm(); bqlt {T0}, 0, {fail};);
                    self.masm.bind(valid);
                    onlineasm!(*self.asm(); move {t.reg}, {T0}; jmp {done};);
                    self.masm.bind(fixnum);
                    let payload_at = self.masm.assembler().mem(o, *offset as i32 + 8);
                    onlineasm!(*self.asm(); loadq {T1}, {payload_at}; sxi2q {t.reg}, {T1};);
                    self.masm.bind(done);
                }
                self.commit(t);
            }
            LirInst::MulWide64 {
                dst_lo,
                dst_hi,
                lhs,
                rhs,
            } => {
                // Signed 64x64 -> 128 by magnitudes: negate each operand into
                // its unsigned magnitude, multiply the magnitudes with the
                // four 32-bit partial products (a 5-register algorithm), and
                // negate the two-limb result when exactly one operand was
                // negative. Every product of two magnitudes fits 126 bits.
                let (fl, fh) = self.wide_enter();
                let l = self.read(*lhs, T0)?;
                let r = self.read(*rhs, T1)?;
                onlineasm!(*self.asm();
                    move {T2}, {l};
                    move {T3}, {r};
                    move {fl}, {T2};
                    rshiftq {fl}, 63;
                    move {fh}, {T3};
                    rshiftq {fh}, 63;
                    xorq {T2}, {T2}, {fl};
                    subq {T2}, {T2}, {fl};
                    xorq {T3}, {T3}, {fh};
                    subq {T3}, {T3}, {fh};
                    xorq {fl}, {fl}, {fh};
                    // Unsigned 64x64 -> 128.
                    move {fh}, {T2};
                    urshiftq {fh}, 32;
                    move {T0}, {T3};
                    urshiftq {T0}, 32;
                    zxi2q {T2}, {T2};
                    zxi2q {T3}, {T3};
                    move {T1}, {T2};
                    mulq {T1}, {T1}, {T3};
                    mulq {T2}, {T2}, {T0};
                    mulq {T3}, {fh}, {T3};
                    mulq {fh}, {fh}, {T0};
                    addq {T2}, {T2}, {T3};
                    cqb {T0}, {T2}, {T3};
                    move {T3}, {T2};
                    urshiftq {T3}, 32;
                    addq {fh}, {fh}, {T3};
                    addq {fh}, {fh}, {T0};
                    lshiftq {T2}, 32;
                    // `addqs` sets the carry on every target; the plain
                    // `addq` does not set flags on AArch64, and `adcq`
                    // consumes the carry.
                    addqs {T1}, {T1}, {T2};
                    adcq {fh}, 0;
                );
                let positive = self.masm.label();
                onlineasm!(*self.asm(); btqz {fl}, {positive};);
                onlineasm!(*self.asm();
                    cqneq {T0}, {T1}, 0;
                    negq {T1};
                    negq {fh};
                    subq {fh}, {fh}, {T0};
                );
                self.masm.bind(positive);
                // Copy both results onto the plain temporaries before the
                // pair is restored (a destination may be one of its
                // registers).
                onlineasm!(*self.asm(); move {T0}, {T1}; move {T3}, {fh};);
                self.wide_leave();
                self.write_wide(*dst_lo, *dst_hi, T0, T3)?;
            }
            LirInst::WideAdd {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            }
            | LirInst::WideSub {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            } => {
                let is_add = matches!(inst, LirInst::WideAdd { .. });
                let fail = self.exit_label(*exit);
                if self.stress_guard(fail) {
                    return Ok(());
                }
                self.read_wide4(*lhs_lo, *lhs_hi, *rhs_lo, *rhs_hi)?;
                if is_add {
                    // OF after the carry-propagating high add is exactly the
                    // i128 signed overflow (the 129-bit sum).
                    onlineasm!(*self.asm();
                        addqs {T2}, {T0}, {T2};
                        adcq {T3}, {T1};
                        bo {fail};
                    );
                    self.write_wide(*dst_lo, *dst_hi, T2, T3)?;
                } else {
                    // The low subtraction leaves the borrow in CF, and the
                    // high one is the two-address form: `T1 - T3 - CF`.
                    onlineasm!(*self.asm();
                        subqs {T2}, {T0}, {T2};
                        sbcq {T1}, {T3};
                        bo {fail};
                    );
                    self.write_wide(*dst_lo, *dst_hi, T2, T1)?;
                }
            }
            LirInst::WideCmp {
                cmp,
                dst,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
            } => {
                self.read_wide4(*lhs_lo, *lhs_hi, *rhs_lo, *rhs_hi)?;
                let low = self.masm.label();
                let done = self.masm.label();
                // Different high limbs decide the comparison signed; equal
                // high limbs leave an unsigned comparison of the low limbs.
                onlineasm!(*self.asm(); bqeq {T1}, {T3}, {low};);
                wide_cmp_set(self.masm, *cmp, false, T1, T1, T3);
                onlineasm!(*self.asm(); jmp {done};);
                self.masm.bind(low);
                wide_cmp_set(self.masm, *cmp, true, T1, T0, T2);
                self.masm.bind(done);
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {T1};);
                self.commit(t);
            }
            LirInst::WideShift {
                op,
                dst_lo,
                dst_hi,
                src_lo,
                src_hi,
                amount,
            } => {
                let amount = *amount;
                let lo = self.read(*src_lo, T0)?;
                if lo != T0 {
                    onlineasm!(*self.asm(); move {T0}, {lo};);
                }
                let hi = self.read(*src_hi, T1)?;
                if hi != T1 {
                    onlineasm!(*self.asm(); move {T1}, {hi};);
                }
                self.emit_wide_shift(*op, amount);
                self.write_wide(*dst_lo, *dst_hi, T2, T3)?;
            }
            LirInst::WideExtend {
                dst_lo,
                dst_hi,
                src,
            } => {
                let s = self.read(*src, T0)?;
                if s != T0 {
                    onlineasm!(*self.asm(); move {T0}, {s};);
                }
                onlineasm!(*self.asm(); move {T1}, {T0}; rshiftq {T1}, 63;);
                self.write_wide(*dst_lo, *dst_hi, T0, T1)?;
            }
            LirInst::WideSplit {
                dst,
                src_lo,
                src_hi,
                high,
            } => {
                let src = if *high { *src_hi } else { *src_lo };
                let s = self.read(src, T0)?;
                let t = self.write(*dst)?;
                if t.reg != s {
                    onlineasm!(*self.asm(); move {t.reg}, {s};);
                }
                self.commit(t);
            }
            LirInst::BoxInt128 {
                dst,
                src_lo,
                src_hi,
            } => {
                // A fixnum when the value fits i32; otherwise a one-limb
                // bignum when it fits i64; otherwise a two-limb bignum with a
                // normalized high limb. Both allocations are deferred LAB
                // bumps whose slow path does not collect.
                let lo = self.read(*src_lo, T0)?;
                if lo != T0 {
                    onlineasm!(*self.asm(); move {T0}, {lo};);
                }
                let hi = self.read(*src_hi, T1)?;
                if hi != T1 {
                    onlineasm!(*self.asm(); move {T1}, {hi};);
                }
                let t = self.write(*dst)?;
                let wide_check = self.masm.label();
                let one = self.masm.label();
                let two = self.masm.label();
                let done = self.masm.label();
                // Sign-extending the low 32 bits gives the value back exactly
                // when it fits i32; the high limb must then be that sign.
                onlineasm!(*self.asm();
                    sxi2q {T3}, {T0};
                    bqneq {T3}, {T0}, {wide_check};
                    rshiftq {T3}, 63;
                    bqneq {T3}, {T1}, {wide_check};
                );
                self.masm.box_int32(t.reg, T0);
                self.commit(t);
                onlineasm!(*self.asm(); jmp {done};);
                self.masm.bind(wide_check);
                onlineasm!(*self.asm(); move {T3}, {T0}; rshiftq {T3}, 63; bqneq {T3}, {T1}, {two}; jmp {one};);
                self.deferred.push(Deferred::BoxBigInt {
                    wide: one,
                    done,
                    obj: t.reg,
                    slot: t.slot,
                    src: BoxSrc::Reg(T0),
                    bytes: sabi::OBJECT_HEADER_BYTES + 16,
                    header: header_word(builtin_class_ids::BIGINT),
                });
                self.deferred.push(Deferred::BoxInt128Two {
                    wide: two,
                    done,
                    obj: t.reg,
                    slot: t.slot,
                    lo: BoxSrc::Reg(T0),
                    hi: BoxSrc::Reg(T1),
                    bytes: sabi::OBJECT_HEADER_BYTES + 24,
                    header: header_word(builtin_class_ids::BIGINT),
                });
                self.masm.bind(done);
            }
            LirInst::LoadFree {
                dst,
                closure,
                index,
            } => {
                let c = self.read(*closure, T0)?;
                let t = self.write(*dst)?;
                let disp = sabi::CLOSURE_FREE as i32 + 8 * *index as i32;
                let mem = self.masm.assembler().mem(c, disp);
                onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::StoreFree {
                closure,
                index,
                src,
            } => {
                let c = self.read(*closure, T0)?;
                let s = self.read(*src, T1)?;
                let disp = sabi::CLOSURE_FREE as i32 + 8 * *index as i32;
                let mem = self.masm.assembler().mem(c, disp);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            LirInst::LoadReturned { dst, index, kind } => {
                // The call popped its frame, so the callee's values sit
                // above this frame's last word, at `fp[2 + index]` of it.
                let t = self.write(*dst)?;
                let disp = 8 * self.frame_words as i32 + 16 + 8 * *index as i32;
                match kind {
                    ReturnedKind::Value => {
                        let mem = self.masm.assembler().mem(SFP, disp);
                        onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                    }
                    ReturnedKind::Addr => {
                        let mem = self.masm.assembler().mem(SFP, disp);
                        onlineasm!(*self.asm(); leap {t.reg}, {mem};);
                    }
                    ReturnedKind::Count => {
                        let skip = i64::from(*index);
                        onlineasm!(*self.asm(); subp {t.reg}, {AC}, {skip};);
                    }
                }
                self.commit(t);
            }
            LirInst::LoadCell { dst, cell } => {
                let c = self.read(*cell, T0)?;
                let t = self.write(*dst)?;
                let mem = self.masm.assembler().mem(c, 0);
                onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::StoreCell { cell, src } => {
                let c = self.read(*cell, T0)?;
                let s = self.read(*src, T1)?;
                let mem = self.masm.assembler().mem(c, 0);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            LirInst::WriteBarrier { obj, value } => {
                let _ = value;
                let o = self.read(*obj, T0)?;
                let (slow, join) = (self.masm.label(), self.masm.label());
                self.gc.emit_unlogged_check(self.masm, o, T1, T2, slow);
                self.masm.bind(join);
                self.deferred.push(Deferred::Barrier {
                    slow,
                    join,
                    obj: o,
                    preserve: &[],
                });
            }
            LirInst::LoadElement { dst, vector, index } => {
                let v = self.read(*vector, T0)?;
                let i = self.read(*index, T1)?;
                let t = self.write(*dst)?;
                let mem = self.masm.assembler().mem_indexed(v, i, 8, 8);
                onlineasm!(*self.asm(); loadq {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::StoreElement { vector, index, src } => {
                let v = self.read(*vector, T0)?;
                let i = self.read(*index, T1)?;
                let s = self.read(*src, T2)?;
                let mem = self.masm.assembler().mem_indexed(v, i, 8, 8);
                onlineasm!(*self.asm(); storeq {mem}, {s};);
            }
            LirInst::StringRef {
                dst,
                string,
                index,
                exit,
            } => {
                let fail = self.exit_label(*exit);
                let obj = self.read(*string, T0)?;
                let idx = self.read(*index, T1)?;
                // Payload word 0 is the stringbuf; its header's class says
                // whether the data is one byte per character (narrow) or a
                // four-byte `char` (wide). Both are read inline; only an
                // unexpected class deopts.
                let sb_at = self.masm.assembler().mem(obj, 0);
                onlineasm!(*self.asm(); loadq {T2}, {sb_at};);
                let hdr_at = self.masm.assembler().mem(T2, -8);
                onlineasm!(*self.asm();
                    loadq {T3}, {hdr_at};
                    andq {T3}, {T3}, {sabi::CLASS_ID_MASK as i64};
                );
                let wide = self.masm.label();
                let ready = self.masm.label();
                onlineasm!(*self.asm();
                    bqeq {T3}, {builtin_class_ids::STRINGBUF_WIDE as i64}, {wide};
                    bqneq {T3}, {builtin_class_ids::STRINGBUF_NARROW as i64}, {fail};
                );
                // Narrow: data + start + index, one byte.
                let start_at = self.masm.assembler().mem(obj, 8);
                onlineasm!(*self.asm();
                    loadq {T0}, {start_at};
                    addp {T2}, {T2}, {sabi::STRINGBUF_DATA as i64};
                    addp {T2}, {T2}, {T0};
                    addp {T2}, {T2}, {idx};
                );
                let mem = self.masm.assembler().mem(T2, 0);
                onlineasm!(*self.asm();
                    loadb {T3}, {mem};
                    jmp {ready};
                );
                // Wide: four-byte `char`, scaled index.
                self.masm.bind(wide);
                onlineasm!(*self.asm();
                    loadq {T0}, {start_at};
                    move {T3}, {idx};
                    lshiftq {T3}, 2;
                    addp {T3}, {T3}, {T0};
                    addp {T2}, {T2}, {sabi::STRINGBUF_DATA as i64};
                    addp {T2}, {T2}, {T3};
                );
                let wide_mem = self.masm.assembler().mem(T2, 0);
                onlineasm!(*self.asm(); loadi {T3}, {wide_mem};);
                self.masm.bind(ready);
                onlineasm!(*self.asm();
                    lshiftq {T3}, 16;
                    orq {T3}, {T3}, {crate::runtime::objectmodel::Value::CHAR_TAG as i64};
                );
                let t = self.write(*dst)?;
                onlineasm!(*self.asm(); move {t.reg}, {T3};);
                self.commit(t);
            }
            LirInst::LoadByte { dst, base, index } => {
                let b = self.read(*base, T0)?;
                let i = self.read(*index, T1)?;
                let t = self.write(*dst)?;
                let mem = self.masm.assembler().mem_indexed(b, i, 1, 0);
                onlineasm!(*self.asm(); loadb {t.reg}, {mem};);
                self.commit(t);
            }
            LirInst::StoreByte { base, index, src } => {
                let b = self.read(*base, T0)?;
                let i = self.read(*index, T1)?;
                let s = self.read(*src, T2)?;
                let mem = self.masm.assembler().mem_indexed(b, i, 1, 0);
                onlineasm!(*self.asm(); storeb {mem}, {s};);
            }
            LirInst::SlotGet {
                dst,
                obj,
                meta,
                exit,
            } => self.slot_get(*dst, *obj, *meta, *exit)?,
            LirInst::SlotPut {
                obj,
                src,
                meta,
                exit,
            } => self.slot_put(*obj, *src, *meta, *exit)?,
            LirInst::Call {
                target,
                args,
                rv,
                safepoint,
                exit,
                values,
            } => self.call(*target, args, *rv, Some(*safepoint), *exit, *values)?,
            LirInst::Poll { safepoint } => self.poll(*safepoint)?,
        }
        Ok(())
    }

    /// The fixed GPR pair the two-limb multiply computes in: `rax:rdx` on
    /// x86-64 and `x0:x2` on AArch64 (both `t0:t2` in the portable table).
    /// Both are pool registers, so [`Emitter::wide_enter`] saves them on the
    /// native stack and [`Emitter::wide_leave`] restores them.
    fn wide_pair(&self) -> (MReg, MReg) {
        (onlineasm::regs::t0, onlineasm::regs::t2)
    }

    /// Pushes the fixed wide pair and returns it as `(low, high)`.
    fn wide_enter(&mut self) -> (MReg, MReg) {
        let (lo, hi) = self.wide_pair();
        onlineasm!(*self.asm(); push {lo}; push {hi};);
        (lo, hi)
    }

    /// Restores the fixed wide pair. The stack usage is balanced on every
    /// path that enters it: no wide operation that carries a deopt exit uses
    /// the pair (see [`LirInst::WideAdd`](super::lir::LirInst::WideAdd)).
    fn wide_leave(&mut self) {
        let (lo, hi) = self.wide_pair();
        onlineasm!(*self.asm(); pop {hi}; pop {lo};);
    }

    /// Reads four i64 words into `T0..T3`, in order. A spilled operand is
    /// loaded into its temporary; a register operand is copied there.
    fn read_wide4(&mut self, a_lo: VReg, a_hi: VReg, b_lo: VReg, b_hi: VReg) -> Result<(), String> {
        for (vreg, temp) in [(a_lo, T0), (a_hi, T1), (b_lo, T2), (b_hi, T3)] {
            let got = self.read(vreg, temp)?;
            if got != temp {
                onlineasm!(*self.asm(); move {temp}, {got};);
            }
        }
        Ok(())
    }

    /// Writes the two result words held in `lo`/`hi` into `dst_lo`/`dst_hi`.
    /// `write`'s temporary is `T2`, so the low word is committed before the
    /// high word is read; `hi` must not be `T2`.
    fn write_wide(&mut self, dst_lo: VReg, dst_hi: VReg, lo: MReg, hi: MReg) -> Result<(), String> {
        let t = self.write(dst_lo)?;
        onlineasm!(*self.asm(); move {t.reg}, {lo};);
        self.commit(t);
        let t = self.write(dst_hi)?;
        onlineasm!(*self.asm(); move {t.reg}, {hi};);
        self.commit(t);
        Ok(())
    }

    /// Branches to `fail` unless `test` holds for `s`. `s` is a GPR. It may be
    /// `T0`, and `T1`/`T2` are scratch.
    fn branch_unless(&mut self, test: Test, s: MReg, fail: Label) -> Result<(), String> {
        match test {
            Test::Fixnum => self.masm.branch_if_not_fixnum(s, T1, fail),
            Test::Flonum => self.masm.branch_if_not_double(s, T1, fail),
            Test::Pair => self
                .masm
                .branch_not_heap_class(s, ClassRef::PAIR.class_id(), T1, fail),
            Test::Class(class) => self
                .masm
                .branch_not_heap_class(s, class.class_id(), T1, fail),
            Test::Heap => self.masm.branch_if_not_cell(s, T1, fail),
            Test::Char => {
                let ok = self.masm.label();
                self.masm.branch_if_char(s, T1, T2, ok);
                onlineasm!(*self.asm(); jmp {fail};);
                self.masm.bind(ok);
            }
            Test::Boolean => {
                let ok = self.masm.label();
                onlineasm!(*self.asm(); bqeq {s}, {crate::runtime::objectmodel::Value::VALUE_TRUE}, {ok};);
                onlineasm!(*self.asm(); bqneq {s}, {crate::runtime::objectmodel::Value::VALUE_FALSE}, {fail};);
                self.masm.bind(ok);
            }
            Test::Number => {
                // The baseline's `number?` chain: an inline number always
                // matches; a heap object matches when its class is one of the
                // three heap number classes.
                let ok = self.masm.label();
                let try_rational = self.masm.label();
                let try_complex = self.masm.label();
                self.masm.branch_if_fixnum(s, T1, ok);
                self.masm.branch_if_double(s, T1, ok);
                self.masm
                    .branch_not_heap_class(s, builtin_class_ids::BIGINT, T1, try_rational);
                onlineasm!(*self.asm(); jmp {ok};);
                self.masm.bind(try_rational);
                self.masm
                    .branch_not_heap_class(s, builtin_class_ids::RATIONAL, T1, try_complex);
                onlineasm!(*self.asm(); jmp {ok};);
                self.masm.bind(try_complex);
                self.masm
                    .branch_not_heap_class(s, builtin_class_ids::COMPLEX, T1, fail);
                self.masm.bind(ok);
            }
            Test::Code(code) | Test::CodeOfClosure(code) => {
                // A closure first, so that its code-block field is a word
                // to compare. Then the block must be the one in the table.
                let slot = self.code_slot(code)?;
                if matches!(test, Test::Code(_)) {
                    self.masm
                        .branch_not_heap_class(s, ClassRef::CLOSURE.class_id(), T1, fail);
                }
                let block_at = self
                    .masm
                    .assembler()
                    .mem(s, sabi::CLOSURE_CODE_BLOCK as i32);
                onlineasm!(*self.asm(); loadq {T1}, {block_at};);
                self.load_constant_addr(T2, slot)?;
                let cell_at = self.masm.assembler().mem(T2, 0);
                onlineasm!(*self.asm(); loadq {T2}, {cell_at}; bqneq {T1}, {T2}, {fail};);
            }
            Test::NotInvalid => self.branch_invalid(fail),
        }
        Ok(())
    }

    /// Branches to `fail` when the unit has been invalidated.
    fn branch_invalid(&mut self, fail: Label) {
        self.masm.load_import(T0, ImportKey::InvalidWord);
        let mem = self.masm.assembler().mem(T0, 0);
        onlineasm!(*self.asm(); loadi {T0}, {mem}; bqneq {T0}, 0, {fail};);
    }

    fn alloc(&mut self, dst: VReg, layout: AllocLayout) -> Result<(), String> {
        // The header word, the payload size in words, and a payload word 0
        // written by the allocation (a length, or a packed stamp/length).
        let (header, payload_words, length_word): (u64, usize, Option<u64>) = match layout {
            AllocLayout::Pair => (header_word(ClassRef::PAIR.class_id()), 2, None),
            AllocLayout::Flonum => (header_word(builtin_class_ids::FLONUM), 1, None),
            AllocLayout::Closure { free, .. } => (
                header_word(ClassRef::CLOSURE.class_id()),
                sabi::CLOSURE_SIZE / 8 + free as usize,
                None,
            ),
            AllocLayout::Env { slots } => (
                header_word(ClassRef::ENV_RECORD.class_id()),
                1 + slots as usize,
                Some(slots as u64),
            ),
            AllocLayout::Vector { len } => (
                header_word(ClassRef::VECTOR.class_id()),
                1 + len as usize,
                Some(len as u64),
            ),
            AllocLayout::Tuple { len } => (
                header_word(ClassRef::TUPLE.class_id()),
                1 + len as usize,
                Some(len as u64),
            ),
            // A `SchemeInstance`: payload `[class descriptor, storage]`. The
            // lowering stores both after the allocation, before any
            // safepoint.
            AllocLayout::Instance { class, slots } => {
                if class == 0 || class > crate::heap::object::MAX_CLASS_ID {
                    return Err(format!("alloc.instance names class {class}"));
                }
                let _ = slots;
                (header_word(class), 2, None)
            }
            // The slot storage: the header names the registered `SlotStorage`
            // type class; word 0 packs the layout stamp (low 32 bits) and the
            // number of values (high 32 bits), which is what its GC hooks
            // read before tracing the values.
            AllocLayout::SlotStorage { header, stamp, len } => (
                header,
                1 + len as usize,
                Some((u64::from(len) << 32) | u64::from(stamp)),
            ),
            // Two inline `Number`s: 16 bytes each. There is no length word;
            // the class header alone tells the collector how to trace it.
            AllocLayout::Rational => (header_word(ClassRef::RATIONAL.class_id()), 4, None),
        };
        let bytes = sabi::OBJECT_HEADER_BYTES + 8 * payload_words;
        let t = self.write(dst)?;
        let obj = t.reg;
        let pending = self
            .gc
            .emit_alloc(self.masm, AllocSize::Bytes(bytes), header, obj, T0, T1);
        let (slow, join) = pending.labels();
        self.deferred.push(Deferred::Alloc {
            slow,
            join,
            obj,
            size: AllocDeferred::Bytes(bytes),
            header,
        });
        if let Some(n) = length_word {
            onlineasm!(*self.asm(); move {T0}, {n as i64};);
            let mem = self.masm.assembler().mem(obj, 0);
            onlineasm!(*self.asm(); storeq {mem}, {T0};);
        }
        if let AllocLayout::Closure { code, free } = layout {
            self.init_closure(obj, code, free)?;
        }
        self.commit(t);
        Ok(())
    }

    /// Writes the closure fields that the free variables do not cover: the
    /// machine entry, the code block, its metadata and the free count, as the
    /// baseline's `closure` template does. The `StoreFree`s that follow fill
    /// every free variable before any safepoint.
    fn init_closure(&mut self, obj: MReg, code: CodeRef, free: u32) -> Result<(), String> {
        let slot = self.code_slot(code)?;
        self.masm
            .load_import(T0, ImportKey::Runtime(RuntimeSym::InterpreterEnter));
        let code_at = self.masm.assembler().mem(obj, sabi::CLOSURE_CODE as i32);
        onlineasm!(*self.asm(); storeq {code_at}, {T0};);
        self.load_constant_addr(T1, slot)?;
        let cell_at = self.masm.assembler().mem(T1, 0);
        onlineasm!(*self.asm(); loadq {T1}, {cell_at};);
        let block_at = self
            .masm
            .assembler()
            .mem(obj, sabi::CLOSURE_CODE_BLOCK as i32);
        onlineasm!(*self.asm(); storeq {block_at}, {T1};);
        let meta = self
            .masm
            .assembler()
            .mem(T1, sabi::CODE_BLOCK_METADATA as i32);
        onlineasm!(*self.asm(); loadq {T0}, {meta};);
        let meta_at = self.masm.assembler().mem(obj, sabi::CLOSURE_META as i32);
        onlineasm!(*self.asm(); storeq {meta_at}, {T0};);
        onlineasm!(*self.asm(); move {T0}, {free as i64};);
        let nfree_at = self.masm.assembler().mem(obj, sabi::CLOSURE_NFREE as i32);
        onlineasm!(*self.asm(); storeq {nfree_at}, {T0};);
        Ok(())
    }

    /// A call. `Known` and `Generic` build a callee frame, a runtime helper
    /// takes its arguments in registers, and a primitive takes its arguments
    /// in registers and deopts on error. `safepoint` is `None` for a tail
    /// call, which only frame calls are.
    fn call(
        &mut self,
        target: CallTarget,
        args: &[VReg],
        rv: Option<VReg>,
        safepoint: Option<SafePoint>,
        exit: Option<DeoptExit>,
        values: ReturnValues,
    ) -> Result<(), String> {
        match target {
            CallTarget::Known(code) => self.frame_call_known(code, args, safepoint, values, exit),
            CallTarget::Generic => self.frame_call(args, safepoint, values, exit),
            CallTarget::Helper(_) | CallTarget::Prim(_) | CallTarget::PrimN(_)
                if safepoint.is_none() =>
            {
                Err("only a frame call can be a tail call".to_string())
            }
            CallTarget::Helper(helper) => self.helper_call(helper, args, rv, exit),
            CallTarget::Prim(prim) => self.prim_call(prim, args, exit),
            CallTarget::PrimN(prim) => self.prim_n_call(prim, args, rv, exit),
        }
    }

    /// The callee frame sits above this frame, at `fp[frame_words]`. It takes
    /// the arguments, the return point and the closure. With no `safepoint`
    /// the call is a tail call: it replaces this frame's arguments instead, so
    /// the callee runs in place.
    fn frame_call(
        &mut self,
        args: &[VReg],
        safepoint: Option<SafePoint>,
        values: ReturnValues,
        exit: Option<DeoptExit>,
    ) -> Result<(), String> {
        self.frame_call_common(None, args, safepoint, values, exit, false)
    }

    /// A tail call. `checked` marks a profile-named entry: the emitter
    /// compares the callee's code block and falls back to the generic tail
    /// path on a mismatch, because a tail-call frame cannot deopt.
    fn tail_call(
        &mut self,
        target: CallTarget,
        args: &[VReg],
        checked: bool,
    ) -> Result<(), String> {
        match target {
            CallTarget::Known(code) => {
                self.frame_call_common(Some(code), args, None, ReturnValues::Ignore, None, checked)
            }
            CallTarget::Generic => {
                self.frame_call_common(None, args, None, ReturnValues::Ignore, None, false)
            }
            _ => Err("only a frame call can be a tail call".to_string()),
        }
    }

    /// A call to the known code block `code` (IR `CallKnown`). The frame is
    /// built exactly as a generic call's; only the entry differs: the code
    /// block's compiled entry is loaded from the constant table instead of
    /// the closure's `code` field, which saves a dependent load and the
    /// baseline entry's optimized-entry redirect on every call.
    fn frame_call_known(
        &mut self,
        code: CodeRef,
        args: &[VReg],
        safepoint: Option<SafePoint>,
        values: ReturnValues,
        exit: Option<DeoptExit>,
    ) -> Result<(), String> {
        self.frame_call_common(Some(code), args, safepoint, values, exit, false)
    }

    fn frame_call_common(
        &mut self,
        known: Option<CodeRef>,
        args: &[VReg],
        safepoint: Option<SafePoint>,
        values: ReturnValues,
        exit: Option<DeoptExit>,
        checked: bool,
    ) -> Result<(), String> {
        let tail = safepoint.is_none();
        let callee = 8 * self.frame_words as i32;
        for (i, &arg) in args.iter().enumerate() {
            let r = self.read(arg, T0)?;
            let mem = self.masm.assembler().mem(SFP, callee + 8 * (1 + i as i32));
            onlineasm!(*self.asm(); storeq {mem}, {r};);
        }
        if tail {
            // A profile-named entry is only a hint: check the callee's code
            // block first, before the down-copy writes the callee closure
            // over slot 1. A mismatch takes the generic tail path below; no
            // deopt is possible here (a tail-call frame is never rebuilt).
            let fallback = if checked {
                let code = known.ok_or("a checked tail call needs a known target")?;
                let mismatch = self.masm.label();
                let callee_value = self.read(args[0], T0)?;
                self.masm.branch_not_heap_class(
                    callee_value,
                    builtin_class_ids::CLOSURE,
                    T1,
                    mismatch,
                );
                let slot = self.code_slot(code)?;
                self.load_constant_addr(T1, slot)?;
                let cell = self.masm.assembler().mem(T1, 0);
                onlineasm!(*self.asm(); loadq {T1}, {cell};);
                let block_at = self
                    .masm
                    .assembler()
                    .mem(callee_value, sabi::CLOSURE_CODE_BLOCK as i32);
                onlineasm!(*self.asm();
                    loadq {T2}, {block_at};
                    bqneq {T1}, {T2}, {mismatch};
                );
                Some(mismatch)
            } else {
                None
            };
            // Copy the new arguments down over this frame's own.
            for i in 0..args.len() as i32 {
                let from = self.masm.assembler().mem(SFP, callee + 8 * (1 + i));
                onlineasm!(*self.asm(); loadq {T0}, {from};);
                let to = self.masm.assembler().mem(SFP, 8 * (1 + i));
                onlineasm!(*self.asm(); storeq {to}, {T0};);
            }
            // The closure is argument 0, still where the call read it: avoid
            // the round trip through the slot just written.
            self.set_cp_from_closure_arg(args)?;
            let argc = args.len() as i64 - 1;
            onlineasm!(*self.asm(); move {AC}, {argc};);
            self.enter_frame_target(known)?;
            if let Some(mismatch) = fallback {
                self.masm.bind(mismatch);
                for i in 0..args.len() as i32 {
                    let from = self.masm.assembler().mem(SFP, callee + 8 * (1 + i));
                    onlineasm!(*self.asm(); loadq {T0}, {from};);
                    let to = self.masm.assembler().mem(SFP, 8 * (1 + i));
                    onlineasm!(*self.asm(); storeq {to}, {T0};);
                }
                self.set_cp_from_closure_arg(args)?;
                onlineasm!(*self.asm(); move {AC}, {argc};);
                self.enter_frame_target(None)?;
            }
            return Ok(());
        }
        let safepoint = safepoint.ok_or("a call without a safepoint")?;
        let ra = self.masm.label();
        let livemask = self.livemask(safepoint)?;
        self.label_addr(T1, ra);
        let ra_slot = self.masm.assembler().mem(SFP, callee);
        onlineasm!(*self.asm(); storeq {ra_slot}, {T1};);
        self.set_cp_from_closure_arg(args)?;
        let argc = args.len() as i64 - 1;
        onlineasm!(*self.asm(); move {AC}, {argc};);
        let callee_fp = self.masm.assembler().mem(SFP, callee);
        onlineasm!(*self.asm(); leap {SFP}, {callee_fp};);
        self.enter_frame_target(known)?;
        // The header sits between the jump and the return address, so only
        // the callee's return reaches it. A single-value return point takes a
        // callee that returns other than one value through the values-error
        // stub, as the baseline's does (`MV_ERROR`). Otherwise the callee's
        // several values reach `ra`, which takes the first.
        let mut take_labels = None;
        let target = match values {
            ReturnValues::Error => HeaderTarget::Runtime(RuntimeSym::ValuesError),
            ReturnValues::Ignore => HeaderTarget::Label(ra),
            // Several values reach the entry's exit, which pops the callee
            // frame and hands the count over (`Deferred::Returned`).
            ReturnValues::Forward => {
                let exit = exit.ok_or("a forwarded return point has no deopt exit")?;
                let label = self.masm.label();
                self.deferred.push(Deferred::Returned {
                    exit,
                    label,
                    callee,
                });
                HeaderTarget::Label(label)
            }
            // The callee's values reach `taken` (the single value of a
            // plain return falls into it from `ra`). It checks the count and
            // pops the frame, leaving the values for `LoadReturned`.
            ReturnValues::Take(_) | ReturnValues::TakeRest(_) => {
                let exit = exit.ok_or("a forwarded return point has no deopt exit")?;
                let wrong = self.masm.label();
                self.deferred.push(Deferred::Returned {
                    exit,
                    label: wrong,
                    callee,
                });
                let taken = self.masm.label();
                take_labels = Some((taken, wrong));
                HeaderTarget::Label(taken)
            }
        };
        self.rp_header(ra, target, livemask);
        if let (ReturnValues::Take(count) | ReturnValues::TakeRest(count), Some((taken, wrong))) =
            (values, take_labels)
        {
            // One value came back: it sits in `RV`, and the entry takes
            // another count. Leave it where a multiple-values return puts
            // its values, so the exit finds it in the callee frame.
            let slot2 = self.masm.assembler().mem(SFP, 16);
            onlineasm!(*self.asm();
                storeq {slot2}, {RV};
                move {AC}, 1;
            );
            self.masm.bind(taken);
            let count = count as i64;
            if matches!(values, ReturnValues::Take(_)) {
                onlineasm!(*self.asm(); bqneq {AC}, {count}, {wrong};);
            } else {
                onlineasm!(*self.asm(); bqb {AC}, {count}, {wrong};);
            }
        }
        // The return lands with SFP at the callee frame, so pop it, as the
        // baseline return point does (`pop_frame`).
        let back = self.masm.assembler().mem(SFP, -callee);
        onlineasm!(*self.asm(); leap {SFP}, {back};);
        Ok(())
    }

    /// Enter the callee: its known compiled entry when the IR named one,
    /// otherwise through the closure's `code` (the generic path).
    fn enter_frame_target(&mut self, known: Option<CodeRef>) -> Result<(), String> {
        match known {
            Some(code) => self.enter_known(code),
            None => {
                emit_enter_callee(self.masm);
                Ok(())
            }
        }
    }

    /// Jump to the compiled entry of the known code block. Its pointer sits
    /// in the unit's constant table, which the installer patches, so the call
    /// needs no closure load and no baseline-entry redirect. The entry word is
    /// read fresh on every call: an install or a retire can change it at any
    /// time. With no entry, `closure.code` is the interpreter entry, which
    /// heals the block when compiled code appears.
    fn enter_known(&mut self, code: CodeRef) -> Result<(), String> {
        // The root code block is the one being emitted: its entry is this
        // unit's prologue, so a self call jumps straight to it instead of
        // loading the block and its entry from the constant table.
        if code.index() == 0 {
            let entry = self.entry;
            onlineasm!(*self.asm(); jmp {entry};);
            return Ok(());
        }
        let slot = self.code_slot(code)?;
        self.load_constant_addr(T0, slot)?;
        let cell = self.masm.assembler().mem(T0, 0);
        onlineasm!(*self.asm(); loadq {T0}, {cell};);
        let entry = self.masm.assembler().mem(T0, sabi::CODE_BLOCK_ENTRY as i32);
        let interpreted = self.masm.label();
        onlineasm!(*self.asm();
            loadp {T1}, {entry};
            btpz {T1}, {interpreted};
            bpeq {T1}, -1, {interpreted};
            jmp {T1};
        );
        self.masm.bind(interpreted);
        emit_enter_callee(self.masm);
        Ok(())
    }

    /// A returning runtime helper. Its arguments are in the helper argument
    /// registers, and its result is in RV. The helper reports a status in
    /// [`ERROR_REG`]. A nonzero status jumps to `exit`, which deopts before the
    /// instruction, so baseline runs it.
    /// A variadic primitive call. The arguments go to consecutive words in
    /// the outgoing frame area, which the prologue checked
    /// (`deopt_extent`), and the helper reads them from there. A nonzero
    /// status exits to baseline, which runs the call from its own pc.
    fn prim_n_call(
        &mut self,
        prim: PrimRef,
        args: &[VReg],
        rv: Option<VReg>,
        exit: Option<DeoptExit>,
    ) -> Result<(), String> {
        let exit = exit.ok_or("a primitive call has no deopt exit")?;
        let fail = self.exit_label(exit);
        let area = 8 * (self.frame_words as i32 + 2);
        for (i, &arg) in args.iter().enumerate() {
            let r = self.read(arg, T0)?;
            let mem = self.masm.assembler().mem(SFP, area + 8 * i as i32);
            onlineasm!(*self.asm(); storeq {mem}, {r};);
        }
        let from = self.masm.assembler().mem(SFP, area);
        onlineasm!(*self.asm(); leap {T3}, {from};);
        let result = match rv {
            Some(_) => OpResult::Reg(self.gpr_of(self.regs.rv)?),
            None => OpResult::Reg(T0),
        };
        let call = OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::OptPrimN),
            args: smallvec::smallvec![
                OpArg::Imm(prim.index() as i64),
                OpArg::Reg(T3),
                OpArg::Imm(args.len() as i64)
            ],
            preserve: smallvec::SmallVec::new(),
            result,
            checks_error: true,
            cont: OpContinue::Next,
        };
        emit_operation_call(
            self.masm,
            &call,
            &[],
            &mut |_: &mut Masm, _: OpArg, _: MReg| {
                unreachable!("the packed call's arguments are immediates and a register")
            },
        );
        onlineasm!(*self.asm(); bqneq {ERROR_REG}, 0, {fail};);
        Ok(())
    }

    fn helper_call(
        &mut self,
        helper: HelperRef,
        args: &[VReg],
        rv: Option<VReg>,
        exit: Option<DeoptExit>,
    ) -> Result<(), String> {
        let runtime = helper_extern(helper)?;
        let exit = exit.ok_or("a helper call has no deopt exit")?;
        let fail = self.exit_label(exit);
        let mut operands: smallvec::SmallVec<[OpArg; 8]> = smallvec::SmallVec::new();
        for i in 0..args.len() {
            let reg = *self
                .regs
                .op_args
                .get(i)
                .ok_or("a helper argument has no register")?;
            operands.push(OpArg::Reg(self.gpr_of(reg)?));
        }
        let result = match rv {
            Some(_) => OpResult::Reg(self.gpr_of(self.regs.rv)?),
            None => OpResult::None,
        };
        let call = OperationCall {
            target: OpTarget::Runtime(runtime),
            args: operands,
            preserve: smallvec::SmallVec::new(),
            result,
            checks_error: true,
            cont: OpContinue::Next,
        };
        emit_operation_call(
            self.masm,
            &call,
            &[],
            &mut |_: &mut Masm, _: OpArg, _: MReg| unreachable!("helper arguments are registers"),
        );
        onlineasm!(*self.asm(); bqneq {ERROR_REG}, 0, {fail};);
        Ok(())
    }

    /// A primitive call, as the baseline's `prim_direct` makes it: the wrapper
    /// takes the context in the first argument register, the argument count
    /// in the second, and the arguments after them (`Constraint::Fixed` puts
    /// them there). It returns `(value, error)`.
    fn prim_call(
        &mut self,
        prim: PrimRef,
        args: &[VReg],
        exit: Option<DeoptExit>,
    ) -> Result<(), String> {
        let exit = exit.ok_or("a primitive call has no deopt exit")?;
        let fail = self.exit_label(exit);
        let (a0, a1, _, _, _) = stubs::arg_regs5();
        let id = prim.index() as u16;
        let ac_slot = self.masm.assembler().mem(regs::TC, sabi::TC_AC as i32);
        onlineasm!(*self.asm(); storeq {ac_slot}, {AC};);
        let argc = args.len() as i64;
        onlineasm!(*self.asm(); move {a0}, {regs::TC}; move {a1}, {argc};);
        let target = self.masm.declare_prim_fn(id);
        self.masm.call_rust(target);
        let (_, error) = stubs::ret_regs();
        onlineasm!(*self.asm(); bqneq {error}, 0, {fail};);
        Ok(())
    }

    /// The yieldpoint: a flag check, and on a pending request a call into the
    /// yield glue on an out-of-line block that returns to the join point.
    fn poll(&mut self, safepoint: SafePoint) -> Result<(), String> {
        let livemask = self.livemask(safepoint)?;
        let mut saves = Vec::new();
        for save in &self.alloc.polls[safepoint] {
            saves.push((self.gpr_of(save.reg)?, save.slot));
        }
        let (slow, join, ra) = (self.masm.label(), self.masm.label(), self.masm.label());
        self.masm.yieldpoint_check(T0, slow, false);
        self.masm.bind(join);
        self.deferred.push(Deferred::Poll {
            slow,
            join,
            ra,
            livemask,
            saves,
        });
        Ok(())
    }

    /// Emits a two-limb shift of the value in `T0`/`T1` by the constant
    /// `amount` (0..=127), leaving the result in `T2`/`T3`.
    fn emit_wide_shift(&mut self, op: WideShiftOp, amount: u32) {
        debug_assert!(amount < 128);
        let (k, complement) = (amount as i64, (64 - amount) as i64);
        match (op, amount) {
            (_, 0) => onlineasm!(*self.asm(); move {T2}, {T0}; move {T3}, {T1};),
            (WideShiftOp::Shl, 64) => onlineasm!(*self.asm(); move {T2}, 0; move {T3}, {T0};),
            (WideShiftOp::Shl, 1..=63) => onlineasm!(*self.asm();
                move {T2}, {T0};
                lshiftq {T2}, {k};
                move {T3}, {T1};
                lshiftq {T3}, {k};
                urshiftq {T0}, {complement};
                orq {T3}, {T3}, {T0};
            ),
            (WideShiftOp::Shl, _) => onlineasm!(*self.asm();
                move {T2}, 0;
                move {T3}, {T0};
                lshiftq {T3}, {k - 64};
            ),
            (WideShiftOp::Shr, 64) => onlineasm!(*self.asm(); move {T2}, {T1}; move {T3}, 0;),
            (WideShiftOp::Shr, 1..=63) => onlineasm!(*self.asm();
                move {T2}, {T0};
                urshiftq {T2}, {k};
                move {T3}, {T1};
                lshiftq {T3}, {complement};
                orq {T2}, {T2}, {T3};
                move {T3}, {T1};
                urshiftq {T3}, {k};
            ),
            (WideShiftOp::Shr, _) => onlineasm!(*self.asm();
                move {T2}, {T1};
                urshiftq {T2}, {k - 64};
                move {T3}, 0;
            ),
            (WideShiftOp::Ashr, 64) => onlineasm!(*self.asm();
                move {T2}, {T1};
                move {T3}, {T1};
                rshiftq {T3}, 63;
            ),
            (WideShiftOp::Ashr, 1..=63) => onlineasm!(*self.asm();
                move {T2}, {T0};
                urshiftq {T2}, {k};
                move {T3}, {T1};
                lshiftq {T3}, {complement};
                orq {T2}, {T2}, {T3};
                move {T3}, {T1};
                rshiftq {T3}, {k};
            ),
            (WideShiftOp::Ashr, _) => onlineasm!(*self.asm();
                move {T2}, {T1};
                rshiftq {T2}, {k - 64};
                move {T3}, {T1};
                rshiftq {T3}, 63;
            ),
        }
    }

    /// Branches to `label` when `l cmp r` holds.
    fn cmp_branch(&mut self, cmp: Cmp, l: MReg, r: MReg, label: Label) {
        match cmp {
            Cmp::Eq => onlineasm!(*self.asm(); bqeq {l}, {r}, {label};),
            Cmp::Ne => onlineasm!(*self.asm(); bqneq {l}, {r}, {label};),
            Cmp::Lt => onlineasm!(*self.asm(); bqlt {l}, {r}, {label};),
            Cmp::Le => onlineasm!(*self.asm(); bqlteq {l}, {r}, {label};),
            Cmp::Gt => onlineasm!(*self.asm(); bqgt {l}, {r}, {label};),
            Cmp::Ge => onlineasm!(*self.asm(); bqgteq {l}, {r}, {label};),
        }
    }

    /// A dense int switch: one signed range check against `min..min+span`,
    /// then a scaled load from a table of block addresses.
    ///
    /// `key - min` is computed in 64 bits from the sign-extended i32 key, so
    /// the unsigned range check is correct for `i32::MIN` and for negative
    /// keys below `min` (their wrapped difference is huge). Missing keys and
    /// duplicate targets need no special case: the builder fills holes with
    /// the default target and every table entry names its block.
    fn dense_switch(
        &mut self,
        value: VReg,
        cases: &[(i32, LEdge)],
        default: &LEdge,
        min: i32,
    ) -> Result<(), String> {
        debug_assert!(
            cases.windows(2).all(|pair| pair[0].0 < pair[1].0),
            "a dense switch's cases must be strictly increasing"
        );
        let fallback = self.block_label(default.block);
        let span = cases.len();
        if span == 0 {
            onlineasm!(*self.asm(); jmp {fallback};);
            return Ok(());
        }
        let v = self.read(value, T0)?;
        let miss = self.masm.label();
        let table = self.masm.label();
        let min = i64::from(min);
        let span = span as i64;
        // T1 is the scaled index; T0 becomes the table base.
        onlineasm!(*self.asm();
            move {T1}, {v};
            subq {T1}, {T1}, {min};
            bqaeq {T1}, {span}, {miss};
        );
        self.label_addr(T0, table);
        let slot = self.masm.assembler().mem_indexed(T0, T1, 8, 0);
        onlineasm!(*self.asm(); jmp {slot};);
        self.masm.bind(miss);
        onlineasm!(*self.asm(); jmp {fallback};);

        // Case `i` is key `min + i`; anything absent is a hole and takes the
        // default. The builder emits dense cases in key order, so the common
        // path is a direct index.
        let mut targets = Vec::with_capacity(cases.len());
        let mut next = 0;
        for index in 0..cases.len() {
            let key = min + index as i64;
            let mut target = fallback;
            while next < cases.len() && i64::from(cases[next].0) < key {
                next += 1;
            }
            if next < cases.len() && i64::from(cases[next].0) == key {
                target = self.block_label(cases[next].1.block);
            }
            targets.push(target);
        }
        self.deferred.push(Deferred::SwitchTable {
            label: table,
            targets,
        });
        Ok(())
    }

    /// A sparse int switch: a balanced binary search over the sorted cases.
    fn sparse_switch(
        &mut self,
        value: VReg,
        cases: &[(i32, LEdge)],
        default: &LEdge,
    ) -> Result<(), String> {
        let miss = self.block_label(default.block);
        if cases.is_empty() {
            onlineasm!(*self.asm(); jmp {miss};);
            return Ok(());
        }
        let v = self.read(value, T0)?;
        let mut sorted: Vec<(i32, Label)> = cases
            .iter()
            .map(|(key, edge)| (*key, self.block_label(edge.block)))
            .collect();
        sorted.sort_by_key(|&(key, _)| key);
        sorted.dedup_by_key(|&mut (key, _)| key);
        self.sparse_tree(v, &sorted, miss);
        Ok(())
    }

    /// Emits the search for `cases` (sorted) and leaves each miss at `miss`.
    fn sparse_tree(&mut self, v: MReg, cases: &[(i32, Label)], miss: Label) {
        match cases.len() {
            0 => onlineasm!(*self.asm(); jmp {miss};),
            1 => {
                let hit = cases[0].1;
                let key = i64::from(cases[0].0);
                onlineasm!(*self.asm();
                    bqeq {v}, {key}, {hit};
                    jmp {miss};
                );
            }
            _ => {
                let mid = cases.len() / 2;
                let key = i64::from(cases[mid].0);
                let (hit, left) = (cases[mid].1, self.masm.label());
                onlineasm!(*self.asm();
                    bqeq {v}, {key}, {hit};
                    bqlt {v}, {key}, {left};
                );
                // The right half is strictly greater than `key`.
                self.sparse_tree(v, &cases[mid + 1..], miss);
                self.masm.bind(left);
                self.sparse_tree(v, &cases[..mid], miss);
            }
        }
    }

    fn term(&mut self, term: &LirTerm) -> Result<(), String> {
        match term {
            LirTerm::Jump(edge) => {
                if self.next_block != Some(edge.block) {
                    let target = self.block_label(edge.block);
                    onlineasm!(*self.asm(); jmp {target};);
                }
            }
            LirTerm::Branch {
                cond,
                then,
                otherwise,
            } => {
                let then_label = self.block_label(then.block);
                let else_label = self.block_label(otherwise.block);
                let then_next = self.next_block == Some(then.block);
                let else_next = self.next_block == Some(otherwise.block);
                if let Some(fused) = self.fused {
                    let here = self.g;
                    match fused {
                        Fused::Cmp(cmp, lhs, rhs, at) => {
                            self.g = at;
                            let l = self.read(lhs, T0)?;
                            let r = self.read(rhs, T1)?;
                            self.g = here;
                            if then_next && !else_next {
                                self.cmp_branch(negate(cmp), l, r, else_label);
                            } else {
                                self.cmp_branch(cmp, l, r, then_label);
                                if !else_next {
                                    onlineasm!(*self.asm(); jmp {else_label};);
                                }
                            }
                        }
                        Fused::Test(test, src, at) => {
                            self.g = at;
                            let s = self.read(src, T0)?;
                            self.g = here;
                            self.branch_unless(test, s, else_label)?;
                            if !then_next {
                                onlineasm!(*self.asm(); jmp {then_label};);
                            }
                        }
                    }
                } else {
                    let c = self.read(*cond, T0)?;
                    if then_next && !else_next {
                        onlineasm!(*self.asm(); bqeq {c}, 0, {else_label};);
                    } else {
                        onlineasm!(*self.asm(); bqneq {c}, 0, {then_label};);
                        if !else_next {
                            onlineasm!(*self.asm(); jmp {else_label};);
                        }
                    }
                }
            }
            LirTerm::Switch {
                value,
                cases,
                default,
                density,
            } => match density {
                SwitchDensity::Dense { min } => self.dense_switch(*value, cases, default, *min)?,
                SwitchDensity::Sparse => self.sparse_switch(*value, cases, default)?,
            },
            LirTerm::Return { value } => {
                let v = self.read(*value, RV_TEMP)?;
                let slot2 = self.masm.assembler().mem(SFP, 16);
                onlineasm!(*self.asm(); storeq {slot2}, {v};);
                onlineasm!(*self.asm(); move {RV}, {v};);
                onlineasm!(*self.asm(); move {AC}, 1;);
                let ra = self.masm.assembler().mem(SFP, 0);
                onlineasm!(*self.asm(); jmp {ra};);
            }
            LirTerm::ReturnMany { values } => {
                // Find where the caller takes several values before anything
                // is written. Every return address has an mv target, the
                // interpreter's return glue (self-referential header) and
                // the runtime markers included, so this never leaves the
                // function.
                let ra_at = self.masm.assembler().mem(SFP, 0);
                onlineasm!(*self.asm(); loadq {T1}, {ra_at};);
                let values_error = ValuesError::Import;
                self.masm.emit_mv_target(T1, T2, T3, values_error);
                // `fp[2..]` is below every spill slot.
                for (i, &value) in values.iter().enumerate() {
                    let src = self.read(value, RV_TEMP)?;
                    let dst = self.masm.assembler().mem(SFP, 16 + 8 * i as i32);
                    onlineasm!(*self.asm(); storeq {dst}, {src};);
                }
                let count = values.len() as i64;
                onlineasm!(*self.asm();
                    move {AC}, {count};
                    jmp {T2};
                );
            }
            LirTerm::TailCall {
                target,
                args,
                checked,
            } => {
                self.tail_call(*target, args, *checked)?;
            }
            LirTerm::Deopt { exit } => {
                let label = self.exit_label(*exit);
                onlineasm!(*self.asm(); jmp {label};);
            }
            LirTerm::Unreachable => {
                onlineasm!(*self.asm(); break;);
            }
        }
        Ok(())
    }

    /// The out-of-line blocks, after all the code.
    fn deferred(&mut self) -> Result<(), String> {
        let deferred = std::mem::take(&mut self.deferred);
        for item in deferred {
            match item {
                Deferred::Exit { exit, label } => {
                    self.masm.bind(label);
                    let id = exit.index() as i64;
                    onlineasm!(*self.asm(); move {T3}, {id};);
                    let common = self.deopt_common;
                    onlineasm!(*self.asm(); jmp {common};);
                }
                Deferred::Returned {
                    exit,
                    label,
                    callee,
                } => {
                    self.masm.bind(label);
                    let back = self.masm.assembler().mem(SFP, -callee);
                    onlineasm!(*self.asm(); leap {SFP}, {back};);
                    let ac_slot = self.masm.assembler().mem(regs::TC, sabi::TC_AC as i32);
                    onlineasm!(*self.asm(); storeq {ac_slot}, {AC};);
                    let id = exit.index() as i64;
                    onlineasm!(*self.asm(); move {T3}, {id};);
                    let common = self.deopt_common;
                    onlineasm!(*self.asm(); jmp {common};);
                }
                Deferred::Alloc {
                    slow,
                    join,
                    obj,
                    size,
                    header,
                } => {
                    self.masm.bind(slow);
                    self.alloc_slow(obj, size, header, join);
                }
                Deferred::BoxBigInt {
                    wide,
                    done,
                    obj,
                    slot,
                    src,
                    bytes,
                    header,
                } => {
                    self.masm.bind(wide);
                    // The raw i64 is either in a pool register or in a spill
                    // slot. Spill it to the native stack before the LAB bump:
                    // the slow path calls the allocator, and the bump uses
                    // emitter temporaries. No safepoint runs before the
                    // payload is written.
                    let s = match src {
                        BoxSrc::Reg(reg) => reg,
                        BoxSrc::Slot(slot) => {
                            let mem = self.slot_mem(slot);
                            onlineasm!(*self.asm(); loadq {T1}, {mem};);
                            T1
                        }
                    };
                    let raw = self.masm.assembler().mem(onlineasm::regs::sp, 0);
                    onlineasm!(*self.asm(); subp sp, 16; storeq {raw}, {s};);

                    let pending =
                        self.gc
                            .emit_alloc(self.masm, AllocSize::Bytes(bytes), header, obj, T0, T3);
                    let (slow, join) = pending.labels();
                    // `emit_alloc` already bound `join` after its fast path.

                    let (a, b) = (T0, T1);
                    let saved = self.masm.assembler().mem(onlineasm::regs::sp, 0);
                    onlineasm!(*self.asm(); loadq {a}, {saved}; addp sp, 16;);
                    // mask = value >> 63 (arithmetic): 0 or all ones.
                    onlineasm!(*self.asm();
                        move {b}, {a};
                        rshiftq {b}, 63;
                        // limb = (value ^ mask) - mask, the magnitude.
                        xorq {a}, {a}, {b};
                        subq {a}, {a}, {b};
                    );
                    let limb_at = self.masm.assembler().mem(obj, 8);
                    onlineasm!(*self.asm(); storeq {limb_at}, {a};);
                    // payload word: count = 1 in the low 32 bits, `negative`
                    // at byte 4, pad zero, all in one 64-bit store.
                    onlineasm!(*self.asm();
                        andq {b}, {b}, 1;
                        lshiftq {b}, 32;
                        orq {b}, {b}, 1;
                    );
                    let count_at = self.masm.assembler().mem(obj, 0);
                    onlineasm!(*self.asm(); storeq {count_at}, {b};);
                    self.commit(Target {
                        reg: obj,
                        slot,
                        class: RegClass::Gpr,
                    });
                    onlineasm!(*self.asm(); jmp {done};);

                    // The non-collecting allocator slow path, then back to the
                    // payload stores at `join`.
                    self.masm.bind(slow);
                    self.alloc_slow(obj, AllocDeferred::Bytes(bytes), header, join);
                }
                Deferred::BoxInt128Two {
                    wide,
                    done,
                    obj,
                    slot,
                    lo,
                    hi,
                    bytes,
                    header,
                } => {
                    self.masm.bind(wide);
                    let l = match lo {
                        BoxSrc::Reg(reg) => reg,
                        BoxSrc::Slot(slot) => {
                            let mem = self.slot_mem(slot);
                            onlineasm!(*self.asm(); loadq {T0}, {mem};);
                            T0
                        }
                    };
                    let h = match hi {
                        BoxSrc::Reg(reg) => reg,
                        BoxSrc::Slot(slot) => {
                            let mem = self.slot_mem(slot);
                            onlineasm!(*self.asm(); loadq {T1}, {mem};);
                            T1
                        }
                    };
                    let raw_lo = self.masm.assembler().mem(onlineasm::regs::sp, 0);
                    let raw_hi = self.masm.assembler().mem(onlineasm::regs::sp, 8);
                    onlineasm!(*self.asm();
                        subp sp, 16;
                        storeq {raw_lo}, {l};
                        storeq {raw_hi}, {h};
                    );
                    let pending =
                        self.gc
                            .emit_alloc(self.masm, AllocSize::Bytes(bytes), header, obj, T0, T3);
                    let (slow, join) = pending.labels();
                    let saved_lo = self.masm.assembler().mem(onlineasm::regs::sp, 0);
                    let saved_hi = self.masm.assembler().mem(onlineasm::regs::sp, 8);
                    onlineasm!(*self.asm();
                        loadq {T0}, {saved_lo};
                        loadq {T1}, {saved_hi};
                        addp sp, 16;
                    );
                    // Sign mask and the magnitude of the two-limb value:
                    // `(hi:lo) ^ m - m` with the cross-word borrow.
                    onlineasm!(*self.asm();
                        move {T2}, {T1};
                        rshiftq {T2}, 63;
                        cqneq {T3}, {T0}, 0;
                        andq {T3}, {T3}, {T2};
                        xorq {T0}, {T0}, {T2};
                        subq {T0}, {T0}, {T2};
                        xorq {T1}, {T1}, {T2};
                        subq {T1}, {T1}, {T2};
                        subq {T1}, {T1}, {T3};
                    );
                    let word_at = self.masm.assembler().mem(obj, 0);
                    let limb0_at = self.masm.assembler().mem(obj, 8);
                    let limb1_at = self.masm.assembler().mem(obj, 16);
                    onlineasm!(*self.asm();
                        andq {T2}, {T2}, 1;
                        lshiftq {T2}, 32;
                        orq {T2}, {T2}, 2;
                        storeq {word_at}, {T2};
                        storeq {limb0_at}, {T0};
                        storeq {limb1_at}, {T1};
                    );
                    self.commit(Target {
                        reg: obj,
                        slot,
                        class: RegClass::Gpr,
                    });
                    onlineasm!(*self.asm(); jmp {done};);
                    self.masm.bind(slow);
                    self.alloc_slow(obj, AllocDeferred::Bytes(bytes), header, join);
                }
                Deferred::Barrier {
                    slow,
                    join,
                    obj,
                    preserve,
                } => {
                    self.masm.bind(slow);
                    self.barrier_slow(obj, preserve, join);
                }
                Deferred::Poll {
                    slow,
                    join,
                    ra,
                    livemask,
                    saves,
                } => {
                    self.masm.bind(slow);
                    // The yield may move objects, so the registers are kept in
                    // their slots, which the mask names for the tagged ones.
                    for &(r, s) in &saves {
                        let mem = self.slot_mem(s);
                        onlineasm!(*self.asm(); storeq {mem}, {r};);
                    }
                    let callee = 8 * self.frame_words as i32;
                    self.label_addr(T0, ra);
                    let ra_slot = self.masm.assembler().mem(SFP, callee);
                    onlineasm!(*self.asm(); storeq {ra_slot}, {T0};);
                    // The collector walks the parked callee frame as an entry
                    // frame (`fp+1..sp`), and `sp` is not kept up to date for
                    // compiled frames. Park it on the callee's `ra` for the
                    // duration of the yield: the caller's slots come from `ra`'s
                    // header. The old `sp` waits in the callee's closure slot,
                    // which the walk never reads, and goes back afterwards.
                    let next = self.masm.assembler().mem(SFP, callee + 8);
                    let sp_at = self
                        .masm
                        .assembler()
                        .mem(regs::TC, sabi::TC_STACK_SP as i32);
                    let rv_at = self.masm.assembler().mem(regs::TC, sabi::TC_RV as i32);
                    let undefined = crate::runtime::objectmodel::Value::undefined().bits() as i64;
                    onlineasm!(*self.asm();
                        loadp {T1}, {sp_at};
                        storep {next}, {T1};
                        leap {T1}, {next};
                        storep {sp_at}, {T1};
                        // `ThreadContext.rv` is a GC root and compiled code keeps its own
                        // live values in the save slots above, so never publish
                        // the scratch register to the collector.
                        move {T1}, {undefined};
                        storeq {rv_at}, {T1};
                    );
                    let callee_fp = self.masm.assembler().mem(SFP, callee);
                    onlineasm!(*self.asm(); leap {SFP}, {callee_fp};);
                    let (a0, _, _, _, _) = stubs::arg_regs5();
                    onlineasm!(*self.asm(); move {a0}, {regs::TC};);
                    let target = self.masm.declare_rust_fn(RuntimeExtern::OptYield);
                    let fprs: Vec<MReg> = self
                        .regs
                        .fprs
                        .iter()
                        .filter_map(|&p| arch::vreg(p))
                        .collect();
                    self.save_fprs(&fprs);
                    self.masm.call_rust(target);
                    self.restore_fprs(&fprs);
                    // `SFP` is the callee frame again here, so its closure slot
                    // is `fp[1]`.
                    let saved_sp = self.masm.assembler().mem(SFP, 8);
                    onlineasm!(*self.asm();
                        loadp {T1}, {saved_sp};
                        storep {sp_at}, {T1};
                    );
                    let back = self.masm.assembler().mem(SFP, -callee);
                    onlineasm!(*self.asm(); leap {SFP}, {back};);
                    for &(r, s) in &saves {
                        let mem = self.slot_mem(s);
                        onlineasm!(*self.asm(); loadq {r}, {mem};);
                    }
                    onlineasm!(*self.asm(); jmp {join};);
                    self.rp_header(ra, HeaderTarget::Label(ra), livemask);
                }
                Deferred::SwitchTable { label, targets } => {
                    // Data, not code: no path reaches it, since a switch
                    // ends in a jump. Align before binding so the table's
                    // offset is the label's.
                    self.masm.align(8);
                    self.masm.bind(label);
                    self.masm.reserve_words(targets.len() * 8);
                    self.data.push((label, targets.len() * 8));
                    for (index, &target) in targets.iter().enumerate() {
                        self.relocs.push(Reloc::TableLabel {
                            table: label,
                            index,
                            target,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// The allocation slow path. `operation.rs` spills what the helper
    /// clobbers, so this saves the GPRs it names and the FPRs around it.
    fn alloc_slow(&mut self, obj: MReg, size: AllocDeferred, header: u64, join: Label) {
        let fprs: Vec<MReg> = self
            .regs
            .slow_clobbers
            .iter()
            .filter(|p| p.0 >= arch::FPR_BASE)
            .filter_map(|&p| arch::vreg(p))
            .collect();
        let mut live: Vec<MReg> = self
            .regs
            .slow_clobbers
            .iter()
            .filter(|p| p.0 < arch::FPR_BASE)
            .filter_map(|&p| self.gpr_of(p).ok())
            .filter(|&r| r != obj)
            .collect();
        let bytes = match size {
            AllocDeferred::Bytes(bytes) => OpArg::Imm(bytes as i64),
            AllocDeferred::Words { count, base } => {
                if !live.contains(&count) {
                    live.push(count);
                }
                OpArg::Scaled {
                    reg: count,
                    shift: 3,
                    add: base as i64,
                }
            }
        };
        self.save_fprs(&fprs);
        let call = OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::Alloc),
            args: smallvec::smallvec![OpArg::Imm(header as i64), bytes],
            preserve: smallvec::smallvec![],
            result: OpResult::Reg(obj),
            checks_error: false,
            cont: OpContinue::Join(join),
        };
        emit_operation_call(
            self.masm,
            &call,
            &live,
            &mut |_: &mut Masm, _: OpArg, _: MReg| {
                unreachable!("allocation helper arguments are immediates")
            },
        );
        self.restore_fprs(&fprs);
        onlineasm!(*self.asm(); jmp {join};);
    }

    /// `make-vector` with a runtime size: guard the size, bump-allocate with
    /// a word count, store the length and fill the data. The allocator's
    /// slow path (a full LAB or a large object) is the deferred block
    /// `alloc_slow` emits; the count stays in a fixed temporary that the slow
    /// path saves around its call.
    fn alloc_vector_fill(
        &mut self,
        dst: VReg,
        size: VReg,
        fill: VReg,
        exit: DeoptExit,
    ) -> Result<(), String> {
        let fail = self.exit_label(exit);
        // The count lives in T3: `write` may hand a spilled destination T2,
        // and the allocation must keep the count across both paths.
        let count = self.read(size, T0)?;
        onlineasm!(*self.asm(); move {T3}, {count};);
        if !self.stress_guard(fail) {
            // A fixnum whose unsigned payload is below i32::MAX. A negative
            // size sign-extends to a huge word and fails here, exactly as the
            // baseline's unsigned compare sends it to the slow path.
            self.masm.branch_if_not_fixnum(T3, T0, fail);
            self.masm.unbox_int32(T3, T3);
            onlineasm!(*self.asm(); bqaeq {T3}, {i32::MAX as i64}, {fail};);
        }
        let t = self.write(dst)?;
        let obj = t.reg;
        let spilled = t.slot.is_some();
        let header = header_word(ClassRef::VECTOR.class_id());
        let base = sabi::OBJECT_HEADER_BYTES + sabi::VECTOR_SIZE;
        let pending = self.gc.emit_alloc(
            self.masm,
            AllocSize::Words { count: T3, base },
            header,
            obj,
            T0,
            T1,
        );
        let (slow, join) = pending.labels();
        self.deferred.push(Deferred::Alloc {
            slow,
            join,
            obj,
            size: AllocDeferred::Words { count: T3, base },
            header,
        });
        // The length word, then the fill. The fill consumes the count.
        onlineasm!(*self.asm();
            move {T0}, {T3};
            storeq [{obj}], {T0};
        );
        // `obj` is the temporary T2 when the destination is spilled, and the
        // data pointer below reuses T2. Keep the object in T0 so `commit`
        // stores the object itself, not the interior data pointer.
        if spilled {
            onlineasm!(*self.asm(); move {T0}, {obj};);
        }
        let value = self.read(fill, T1)?;
        onlineasm!(*self.asm(); addp {T2}, {obj}, {sabi::VECTOR_DATA as i64};);
        self.gc.emit_fill(self.masm, T2, T3, value);
        self.commit(if spilled {
            Target {
                reg: T0,
                slot: t.slot,
                class: t.class,
            }
        } else {
            t
        });
        Ok(())
    }

    /// The barrier slow path: `LogObject(obj)`, which returns to `join`.
    fn barrier_slow(&mut self, obj: MReg, preserve: &[MReg], join: Label) {
        let fprs: Vec<MReg> = self
            .regs
            .slow_clobbers
            .iter()
            .filter(|p| p.0 >= arch::FPR_BASE)
            .filter_map(|&p| arch::vreg(p))
            .collect();
        let live: Vec<MReg> = self
            .regs
            .slow_clobbers
            .iter()
            .filter(|p| p.0 < arch::FPR_BASE)
            .filter_map(|&p| self.gpr_of(p).ok())
            .collect();
        self.save_fprs(&fprs);
        let call = OperationCall {
            target: OpTarget::Runtime(RuntimeExtern::LogObject),
            args: smallvec::smallvec![OpArg::Reg(obj)],
            preserve: preserve.iter().copied().collect(),
            result: OpResult::None,
            checks_error: false,
            cont: OpContinue::Join(join),
        };
        emit_operation_call(
            self.masm,
            &call,
            &live,
            &mut |_: &mut Masm, _: OpArg, _: MReg| {
                unreachable!("barrier helper arguments are registers")
            },
        );
        self.restore_fprs(&fprs);
        onlineasm!(*self.asm(); jmp {join};);
    }

    /// Saves FPRs in a native frame below `sp`. `save_fprs` and
    /// `restore_fprs` pair with each other.
    fn save_fprs(&mut self, fprs: &[MReg]) {
        if fprs.is_empty() {
            return;
        }
        let bytes = round16(8 * fprs.len() as i64);
        onlineasm!(*self.asm(); subp sp, {bytes};);
        for (i, &f) in fprs.iter().enumerate() {
            let mem = self.masm.assembler().mem(onlineasm::regs::sp, 8 * i as i32);
            onlineasm!(*self.asm(); stored {mem}, {f};);
        }
    }

    fn restore_fprs(&mut self, fprs: &[MReg]) {
        if fprs.is_empty() {
            return;
        }
        let bytes = round16(8 * fprs.len() as i64);
        for (i, &f) in fprs.iter().enumerate() {
            let mem = self.masm.assembler().mem(onlineasm::regs::sp, 8 * i as i32);
            onlineasm!(*self.asm(); loadd {f}, {mem};);
        }
        onlineasm!(*self.asm(); addp sp, {bytes};);
    }

    /// `deopt_common`: saves the frame's state to a save area on the native
    /// stack, calls `capy_opt_deopt(th, exit, saved, owner)`, and resumes at
    /// the address it returns. The layout is the one `SaveArea` reads: word 0
    /// is `SFP`, word `1 + i` is the `i`-th pool GPR, the next `SAVE_TEMPS`
    /// words are temporaries, and the pool FPRs follow. `owner` is the image's
    /// owner word, which names the unit even after a recompile.
    fn deopt_common(&mut self) -> Result<(), String> {
        let label = self.deopt_common;
        self.masm.bind(label);
        let gprs: Vec<MReg> = self
            .regs
            .gprs
            .iter()
            .map(|&p| self.gpr_of(p))
            .collect::<Result<_, _>>()?;
        let fprs: Vec<MReg> = self
            .regs
            .fprs
            .iter()
            .map(|&p| arch::vreg(p).ok_or_else(|| format!("{p} has no onlineasm name")))
            .collect::<Result<_, _>>()?;
        // Temporaries that the runtime may read. The last word is reserved.
        let temps = [T0, T1, T2, T3, TC, regs::NUMBER_TAG, regs::NOT_CELL_MASK];
        let g = gprs.len();
        let words = 1 + g + SAVE_TEMPS + fprs.len();
        let bytes = round16(8 * words as i64);
        let sp = onlineasm::regs::sp;
        onlineasm!(*self.asm(); subp sp, {bytes};);
        let slot = |masm: &mut Masm, word: usize| masm.assembler().mem(sp, 8 * word as i32);
        let at_sfp = slot(self.masm, 0);
        onlineasm!(*self.asm(); storeq {at_sfp}, {regs::SFP};);
        for (i, &r) in gprs.iter().enumerate() {
            let at = slot(self.masm, 1 + i);
            onlineasm!(*self.asm(); storeq {at}, {r};);
        }
        for (i, &r) in temps.iter().enumerate() {
            let at = slot(self.masm, 1 + g + i);
            onlineasm!(*self.asm(); storeq {at}, {r};);
        }
        // The reserved temporary holds zero.
        let at = slot(self.masm, 1 + g + SAVE_TEMPS - 1);
        onlineasm!(*self.asm(); move {T2}, 0; storeq {at}, {T2};);
        for (j, &f) in fprs.iter().enumerate() {
            let at = slot(self.masm, 1 + g + SAVE_TEMPS + j);
            onlineasm!(*self.asm(); stored {at}, {f};);
        }
        // C arguments: th, the exit id in SITE_TMP, the save area, the owner word.
        let (a0, a1, a2, a3, _) = stubs::arg_regs5();
        let save_area = self.masm.assembler().mem(sp, 0);
        onlineasm!(*self.asm();
            leap {a2}, {save_area};
            move {a1}, {T3};
            move {a0}, {TC};
        );
        let owner = self.owner;
        self.label_addr(a3, owner);
        let target = self.masm.declare_rust_fn(RuntimeExtern::OptDeopt);
        self.masm.call_rust(target);
        // Resume at the returned address, as `emit_control_tail` does: park it,
        // then reload the transient registers from the thread context.
        let (fp_slot, ac_slot, rv_slot, cp_slot) = (
            self.masm.assembler().mem(TC, sabi::TC_STACK_FP as i32),
            self.masm.assembler().mem(TC, sabi::TC_AC as i32),
            self.masm.assembler().mem(TC, sabi::TC_RV as i32),
            self.masm.assembler().mem(TC, sabi::TC_CP as i32),
        );
        onlineasm!(*self.asm();
            addp sp, {bytes};
            move {T2}, {RV};
            loadq {SFP}, {fp_slot};
            loadq {AC}, {ac_slot};
            loadq {RV}, {rv_slot};
            loadq {CP}, {cp_slot};
            jmp {T2};
        );
        Ok(())
    }
}

/// The runtime helper of a [`HelperRef`].
fn helper_extern(helper: HelperRef) -> Result<RuntimeExtern, String> {
    HELPERS
        .get(helper.index())
        .copied()
        .ok_or_else(|| format!("{helper} is not in the helper table"))
}

/// `n` rounded up to a multiple of 16, which keeps a native frame aligned.
fn round16(n: i64) -> i64 {
    (n + 15) & !15
}

/// The register a `return` reads its value into. It is `RV` itself, so the
/// emitter needs no move.
const RV_TEMP: MReg = T0;

/// Sets `dst` to 0 or 1 from `a cmp b`. `unsigned` selects the low-limb
/// comparison, which is unsigned because the high limbs already decided the
/// sign; without it the comparison is signed, for the high limbs.
fn wide_cmp_set(masm: &mut Masm, cmp: Cmp, unsigned: bool, dst: MReg, a: MReg, b: MReg) {
    match (cmp, unsigned) {
        (Cmp::Eq, _) => onlineasm!(*masm.assembler(); cqeq {dst}, {a}, {b};),
        (Cmp::Ne, _) => onlineasm!(*masm.assembler(); cqneq {dst}, {a}, {b};),
        (Cmp::Lt, false) => onlineasm!(*masm.assembler(); cqlt {dst}, {a}, {b};),
        (Cmp::Le, false) => onlineasm!(*masm.assembler(); cqlteq {dst}, {a}, {b};),
        (Cmp::Gt, false) => onlineasm!(*masm.assembler(); cqgt {dst}, {a}, {b};),
        (Cmp::Ge, false) => onlineasm!(*masm.assembler(); cqgteq {dst}, {a}, {b};),
        (Cmp::Lt, true) => onlineasm!(*masm.assembler(); cqb {dst}, {a}, {b};),
        (Cmp::Le, true) => onlineasm!(*masm.assembler(); cqbeq {dst}, {a}, {b};),
        (Cmp::Gt, true) => onlineasm!(*masm.assembler(); cqa {dst}, {a}, {b};),
        (Cmp::Ge, true) => onlineasm!(*masm.assembler(); cqaeq {dst}, {a}, {b};),
    }
}

/// The comparison that holds exactly when `cmp` does not.
fn negate(cmp: Cmp) -> Cmp {
    match cmp {
        Cmp::Eq => Cmp::Ne,
        Cmp::Ne => Cmp::Eq,
        Cmp::Lt => Cmp::Ge,
        Cmp::Le => Cmp::Gt,
        Cmp::Gt => Cmp::Le,
        Cmp::Ge => Cmp::Lt,
    }
}
