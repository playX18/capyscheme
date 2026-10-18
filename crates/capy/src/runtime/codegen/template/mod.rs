mod control;
pub mod emit;
mod tables;
pub(crate) mod target;

pub(crate) use control::{SiteOperand, emit_control, emit_site_slow_call, emit_staged_control};
pub(crate) use tables::{
    metadata_entry_size, metadata_field_offset, metadata_id_operand, metadata_kind, op_len,
};
pub use tables::{opcode_name, parse_bail_filter, parse_bail_filter_str};
pub(crate) use target::{Placement, Target, Targets, emit_branch_to, emit_jump_to};

use onlineasm::{Extern, Label, VReg, onlineasm};

use super::masm::Masm;
pub use super::operation::{OpArg, OpContinue, OpResult, OpTarget, OperationCall};

pub mod tmp {
    use onlineasm::VReg;
    use onlineasm::regs::{t1, t5, t6, t7};

    pub const T0: VReg = t1;
    pub const T1: VReg = t6;
    pub const T2: VReg = t5;
    pub const T3: VReg = t7;
}

pub mod ftmp {
    use onlineasm::VReg;
    use onlineasm::regs::{ft0, ft1, ft2};

    pub const F0: VReg = ft0;
    pub const F1: VReg = ft1;
    pub const F2: VReg = ft2;
}

/// A decoded operand: a register (the interpreter) or an immediate (JIT)
#[derive(Clone, Copy)]
pub enum Operand {
    Register(VReg),
    Immediate(i64),
}

/// A prim id for [`Slow::prim_slow`]: a register holding the
/// id (the interpreter's cold fallthrough) or a static id (an inline
/// guard failure)
#[derive(Clone, Copy)]
pub enum PrimSlowId {
    Register(VReg),
    Immediate(u16),
}

/// A 64-bit comparison, or a 32-bit signed one over boxed fixnums.
#[derive(Clone, Copy)]
pub enum Condition {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Eq32,
    Ne32,
    Lt32,
    Le32,
    Gt32,
    Ge32,
}

/// Where a template leaves through [`Slow::control`].
#[derive(Clone, Copy)]
pub enum SlowPath {
    /// `capy_hot_enter` at an `enter`/`enter_rest` (`AC` holds `argc`).
    HotEnter,
    /// `capy_yield_loop` at a `loop_hint`.
    YieldLoop,
    /// `capy_hot_loop` at a `loop_hint`.
    HotLoop,
    /// `capy_overflow` at an `enter`/`enter_rest` (`AC` holds `argc`).
    Overflow,
    /// `capy_arity` at an `enter`/`enter_rest` (`AC` holds `argc`).
    Arity,
    /// `capy_call_slow` at a `call`/`tail-call` (`MD` is staged for the link).
    CallSlow,
    /// `capy_return_mv` at a `ret`/`return-values`/`return-point`/`return-point-values`/`return-point-values-rest`
    /// (`AC` holds the count, `RV` the single value): a runtime-owned return
    /// address, or a count the return point rejects.
    ReturnMv,
    /// `capy_raise` for a whole `raise`.
    Raise,
    /// `capy_raise_kind`: the value register holds the irritant, the byte
    /// the [`RaiseKind`](crate::runtime::vm::exceptions::RaiseKind) code.
    RaiseKind(VReg, u8),
    /// `capy_number_arith` for arithmetic and ordered comparisons.
    NumberArith,
    /// `capy_compare_jump` for the numeric jumps.
    CompareJump,
    /// `capy_vector_op` for `vector_ref`/`vector_set`.
    VectorOp,
    /// `capy_vector_length` for `vector_length`.
    VectorLength,
    /// `capy_prim_n` for a whole variadic `call_primitive_n`: the direct-call
    /// wrappers take at most three values, so only `primitive_n` still needs
    /// the decode-and-gather slow path. Fixed-arity cold prims go through
    /// [`Slow::prim_slow`] instead (never here).
    PrimN,
    /// `capy_prim_error` for a failed direct prim call: the kind code in
    /// the value register, the irritant in `state().exit_value`. Only the
    /// JIT emits it, from direct-call error blocks.
    PrimError(VReg),
    /// `capy_get_slot` / `capy_put_slot` for a whole slot access.
    GetSlot,
    PutSlot,
    /// `capy_runtime_proc` for a whole `runtime-procedure`.
    RuntimeProc,
    /// `capy_nop` for a `nop` that may be a native entry.
    Nop,
}

/// The linked slow-path helpers of one generated image.
#[derive(Clone, Copy)]
pub struct SlowPaths {
    pub hot_enter: Extern,
    pub yield_loop: Extern,
    pub hot_loop: Extern,
    pub overflow: Extern,
    pub arity: Extern,
    pub call_slow: Extern,
    pub return_mv: Extern,
    pub raise: Extern,
    pub raise_kind: Extern,
    pub number_arith: Extern,
    pub compare_jump: Extern,
    pub vector_op: Extern,
    pub vector_length: Extern,
    pub prim_n: Extern,
    pub prim_error: Extern,
    pub get_slot: Extern,
    pub put_slot: Extern,
    pub runtime_proc: Extern,
    pub nop: Extern,
}

/// Which arithmetic fast paths a template emits: the MathIC shape of one
/// site, selected from its `ArithProfile`
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ArithShape {
    /// Only the fixnum path: both sides observed fixnums alone.
    FixnumOnly,
    /// Only the flonum path: both sides observed flonums alone.
    FlonumOnly,
    /// Every path: mixed or otherwise unshaped observations.
    Full,
    /// No fast path: both sides observed non-numbers alone.
    Generic,
    /// No fast path: a side observed nothing yet.
    Cold,
}

impl ArithShape {
    /// Whether the shape emits the fixnum fast path.
    pub fn emit_fixnum(self) -> bool {
        matches!(self, ArithShape::FixnumOnly | ArithShape::Full)
    }

    /// Whether the shape emits the flonum fast paths.
    pub fn emit_flonum(self) -> bool {
        matches!(self, ArithShape::FlonumOnly | ArithShape::Full)
    }

    /// Whether the shape is a narrow MathIC specialization: one that a
    /// live site can outgrow (towards `Full`) when the other kind shows
    /// up. `Full` has nowhere to go, and `Generic`/`Cold` never inline.
    pub fn is_specialized(self) -> bool {
        matches!(self, ArithShape::FixnumOnly | ArithShape::FlonumOnly)
    }

    /// Whether the shape inlines nothing: the template leaves through a
    /// live slow edge, whose first miss specializes it.
    pub fn is_deferred(self) -> bool {
        matches!(self, ArithShape::Generic | ArithShape::Cold)
    }

    pub fn from_profile(word: u32) -> Self {
        use crate::runtime::interp::profile::*;
        let lhs = word & NUMBER_KIND_MASK;
        let rhs = (word >> ARITH_RHS_SHIFT) & NUMBER_KIND_MASK;
        if lhs == 0 || rhs == 0 {
            // Cold: a lone jump to a live site, which the first miss
            // specializes
            return ArithShape::Cold;
        }
        if lhs == NUMBER_FIXNUM && rhs == NUMBER_FIXNUM {
            return ArithShape::FixnumOnly;
        }
        if lhs == NUMBER_FLONUM && rhs == NUMBER_FLONUM {
            return ArithShape::FlonumOnly;
        }
        if lhs == NUMBER_NON_NUMBER && rhs == NUMBER_NON_NUMBER {
            return ArithShape::Generic;
        }
        ArithShape::Full
    }
}

/// How an arithmetic site's slow edges link to a stub site.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SiteLink {
    /// No site: slow blocks leave through the shared slow path. The
    /// template interpreter, and a terminal stub.
    None,
    /// A live site is created for this instruction when the shape can
    /// still narrow (JIT compile).
    Live,
    /// The stub being generated repatches the existing site at this
    /// address (a plain address, stable for the site's life).
    Resident(usize),
}

/// One arithmetic site as a template sees it: the shape to emit, the
/// profile cell it was read from (for the site's draft, re-read when the
/// site misses), and how slow edges link to a site. Returned by
/// [`Speculation::arith_site`] and passed explicitly to
/// [`Slow::slow_entry`] / [`Slow::slow_site`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ArithSpec {
    pub shape: ArithShape,
    pub profile_cell: Option<usize>,
    pub link: SiteLink,
}

impl ArithSpec {
    pub fn full() -> Self {
        ArithSpec {
            shape: ArithShape::Full,
            profile_cell: None,
            link: SiteLink::None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Known {
    Any,
    Fixnum,
    Flonum,
    Pair,
    Null,
    Bool,
    /// A heap object of this class id.
    HeapClass(u32),
}

impl Known {
    /// The least upper bound: equal elements join to themselves, anything
    /// else to `Any`.
    pub fn join(self, other: Known) -> Known {
        if self == other { self } else { Known::Any }
    }
}

/// Where operands come from: registers, constants, metadata, immediates.
pub trait Operands {
    /// `r[field]` -> `dst`.
    fn load_register(&mut self, masm: &mut Masm, field: usize, dst: VReg);
    fn store_register(&mut self, masm: &mut Masm, field: usize, src: VReg, tmp: VReg);
    /// The raw operand value: a register index, count, immediate or
    /// displacement. Interp decodes it into `tmp::T0`, so the result only
    /// survives until the next context call that touches `T0`.
    fn operand(&mut self, masm: &mut Masm, field: usize, signed: bool) -> Operand;
    /// Constant `field` -> `dst`.
    fn constant(&mut self, masm: &mut Masm, field: usize, dst: VReg);
    /// Address of metadata `field` of kind `kind` -> `dst`. Clobbers `tmp`.
    fn metadata(&mut self, masm: &mut Masm, kind: usize, field: usize, dst: VReg, tmp: VReg);
    /// The machine entry for a new closure (`interpreter_enter_cp`, past the `CP`
    /// reload every `closure.code` caller presets) -> `dst`.
    fn machine_entry(&mut self, masm: &mut Masm, dst: VReg);
}

/// How control continues: the next instruction, jumps, calls and returns.
pub trait Flow {
    /// Fall through to the next instruction.
    fn next(&mut self, masm: &mut Masm);
    /// Branch to the `JumpOffset` in `field`.
    fn jump(&mut self, masm: &mut Masm, field: usize);
    /// Compare and branch to `field`, else fall through.
    fn branch_if(&mut self, masm: &mut Masm, cond: Condition, a: Operand, b: Operand, field: usize);
    /// Write this call's return point for the frame in `frame`:
    /// `interpreter_return` into `frame[0]`, the next bytecode address into
    /// `frame[-1]`. Clobbers `tmp::T0`.
    fn call_return_point(&mut self, masm: &mut Masm, frame: VReg);
    /// Transfer to the closure in `CP`, whose frame `SFP` already points at:
    /// one indirect jump through its machine entry. An interpreted closure
    /// lands in `interpreter_enter_cp`, a healed closure of compiled code goes
    /// direct, a native lands in `subr_call`. Terminal.
    fn enter_callee(&mut self, masm: &mut Masm);
    /// Transfer to the running closure after a direct self call, whose frame
    /// `SFP` already points at. The default is `enter_callee`, only the
    /// interpreter overrides it, entering past the code reload when the JIT
    /// is off (`PC = CONSTANT_POINTER`, with `CONSTANT_POINTER`/`MD` already live), since nothing can
    /// have healed. Terminal.
    fn enter_self(&mut self, masm: &mut Masm) {
        self.enter_callee(masm);
    }
    /// Return a single value to an interpreted caller: `RV` holds the value
    /// and `AC` is 1. Anything else (a stub return address) goes to `slow`.
    /// Terminal. Clobbers `tmp::T0`/`tmp::T1`. The `slow` arm arrives with
    /// the return address in `tmp::T0`.
    fn resume_caller(&mut self, masm: &mut Masm, slow: Label);
    /// Return multiple values (`AC != 1`, values in `fp[2..]`) to an
    /// interpreted caller: resume at the bytecode return point in `fp[-1]`,
    /// whose template validates the count. A compiled return address jumps
    /// to its `%mv-jump` target instead, anything else (a runtime-owned
    /// marker, or a compact header with the values-error bit) goes to
    /// `slow`. Terminal. Clobbers `tmp::T0`/`tmp::T1`/`tmp::T2`.
    fn resume_caller_mv(&mut self, masm: &mut Masm, slow: Label) {
        let compiled = masm.label();
        self.resume_caller(masm, compiled);
        masm.bind(compiled);
        masm.emit_mv_jump(tmp::T0, tmp::T1, tmp::T2, slow);
    }
    /// Reload the code registers for the frame in `SFP` after a pop.
    /// Clobbers `tmp::T0`.
    fn reload_code(&mut self, masm: &mut Masm);
    /// Emit the native-entry check for a `nop`, branching to `slow` when
    /// the `nop` may be a native entry. Returns whether the slow block is
    /// reachable: the interpreter always wires it, the JIT never does
    /// (compiled code is never a native entry, and `PC`/`CONSTANT_POINTER` are not live
    /// there). Templates must not read `PC`/`CONSTANT_POINTER` themselves.
    fn nop_native_check(&mut self, masm: &mut Masm, slow: Label) -> bool;
    /// `switch-fixnum` (`is_char` false) or `switch_char`: type-test `src`,
    /// falling through to the next instruction on a mismatch, else dispatch
    /// through aux-pool table `table` (either int layout). Terminal.
    fn switch_int(&mut self, masm: &mut Masm, src: usize, table: usize, is_char: bool);
    /// `switch-symbol`: type-test `src` for a symbol, falling through on a
    /// mismatch, else dispatch through aux-pool table `table`. Terminal.
    fn switch_sym(&mut self, masm: &mut Masm, src: usize, table: usize);
}

/// Where slow blocks go and how they leave.
pub trait Slow {
    /// Leave through a control slow path. Terminal: emits a jump.
    fn control(&mut self, masm: &mut Masm, path: SlowPath);

    /// A slow block: bind `label` and leave through `path`. The
    /// interpreter emits it inline (exactly as `bind` + `control`),
    /// the JIT defers it to the end of the code, so fast paths fall
    /// through. Emits nothing inline in the JIT.
    fn slow_control(&mut self, masm: &mut Masm, label: Label, path: SlowPath);

    /// A taken block: bind `label` and branch to `field`. As
    /// [`Slow::slow_control`]: inline in the interpreter,
    /// deferred to the end of the code in the JIT.
    fn slow_jump(&mut self, masm: &mut Masm, label: Label, field: usize);

    /// An arithmetic instruction with no inline fast path: leave for `path`
    /// through the stub site `spec` names, so a later phase can still
    /// specialize it. The interpreter leaves directly (exactly as
    /// [`Slow::control`]), the JIT emits a lone jump to a live slow edge.
    /// Terminal: emits no `next`.
    fn slow_entry(&mut self, masm: &mut Masm, spec: &ArithSpec, path: SlowPath);

    /// A slow block of the arithmetic site `spec`: bind `label` and leave
    /// through `path`. A site that can still narrow (its [`ArithSpec::link`]
    /// asks for it) leaves through a repatching slow edge so its first miss
    /// can generate a stub, every other site behaves exactly as
    /// [`Slow::slow_control`]. Non-arithmetic paths (a fused site's vector
    /// half) go through `slow_control` instead.
    fn slow_site(&mut self, masm: &mut Masm, label: Label, spec: &ArithSpec, path: SlowPath);

    /// A slow block that calls an operation: bind `slow`, make `call`, and
    /// continue where it says. The interpreter emits it inline, so
    /// templates call this after their fast path, as with
    /// [`Slow::slow_control`]. The JIT defers it to the end of
    /// the code. Emits nothing inline in the JIT.
    fn call_operation(&mut self, masm: &mut Masm, slow: Label, call: OperationCall);

    /// Call prim `id` with the values in the `args` fields, storing the
    /// result to `dst`. The default runs the prim through
    /// [`Slow::prim_slow`], the JIT overrides it with a direct
    /// call to the prim's wrapper (staying in compiled code) plus a
    /// deferred [`SlowPath::PrimError`] block. The interpreter never calls
    /// this: its prim id is a register, so it dispatches to inlines or
    /// falls through to [`Slow::prim_slow`] directly.
    fn prim_direct(&mut self, masm: &mut Masm, id: i64, args: &[usize], dst: usize) {
        let slow = masm.label();
        self.prim_slow(masm, slow, PrimSlowId::Immediate(id as u16), args, dst);
    }
    /// A cold fixed-arity prim call: bind `slow` and call the prim's
    /// wrapper as an operation, storing the result to `dst` and continuing
    /// past the instruction, errors leaving through
    /// [`SlowPath::PrimError`]. `args`/`dst` are operand fields,
    /// `args.len() <= 3`, variadics keep [`SlowPath::PrimN`].
    fn prim_slow(
        &mut self,
        masm: &mut Masm,
        slow: Label,
        id: PrimSlowId,
        args: &[usize],
        dst: usize,
    ) {
        let target = match id {
            PrimSlowId::Immediate(id) => OpTarget::Prim(id),
            PrimSlowId::Register(id) => OpTarget::PrimIndexed(id),
        };
        let mut operands = smallvec::smallvec![OpArg::Imm(args.len() as i64)];
        operands.extend(args.iter().map(|field| OpArg::Slot(*field)));
        self.call_operation(
            masm,
            slow,
            OperationCall {
                target,
                args: operands,
                preserve: smallvec::SmallVec::new(),
                result: OpResult::Slot(dst),
                checks_error: true,
                cont: OpContinue::Next,
            },
        );
    }
}

/// Codegen-time decisions about speculation: arithmetic site shapes,
/// profiling, and link checks.
pub trait Speculation {
    /// The stub-site spec of the arithmetic site in metadata `field` of
    /// `kind`: which fast paths to emit, and how its slow edges link to a
    /// site. The default is [`ArithShape::Full`] with no link, as the
    /// template interpreter always emits, the JIT overrides it with
    /// profile-driven selection.
    fn arith_site(&mut self, kind: usize, field: usize) -> ArithSpec {
        let _ = (kind, field);
        ArithSpec::full()
    }
    /// Whether emitted code collects profiles and heat. False only in the
    /// template interpreter with the JIT off, where no tier-up consumer
    /// reads them. JIT code always profiles.
    fn profiling(&self) -> bool {
        true
    }
    /// Whether call templates emit the call-site link check. False only
    /// in the template interpreter with the JIT off: with no JIT no stub
    /// can exist (stubs form solely from compiled-code misses) and no
    /// tier-up consumer reads links, so interpreted calls enter directly
    /// and record nothing. With the JIT on the interpreter keeps the
    /// full check: Tier 0 stays a complete profiler, profiles, heat,
    /// and links, for every JIT tier above it, including a future
    /// Tier 2, which reads `CallLinkInfo` for inlining. Like
    /// [`Speculation::profiling`], a codegen-time decision fixed when
    /// the code is emitted.
    fn link_checks(&self) -> bool {
        true
    }

    /// What is statically known about the value in operand `field`.
    fn known(&self, field: usize) -> Known {
        let _ = field;
        Known::Any
    }

    /// Record that operand `field` is now known to be `known` (after a
    /// guard). No-op by default.
    fn refine(&mut self, field: usize, known: Known) {
        let _ = (field, known);
    }
}

/// A template context: [`Operands`] + [`Flow`] + [`Slow`] + [`Speculation`].
pub trait TemplateContext: Operands + Flow + Slow + Speculation {}

impl<T: Operands + Flow + Slow + Speculation> TemplateContext for T {}

/// [`Operands::operand`] materialized into `dst`, for arithmetic and address uses.
/// (`branch_if` takes the [`Operand`] directly.) The interpreter's operand
/// scratch is `tmp::T0`, so prefer [`operand_register`] unless another register
/// must hold the value.
pub fn operand_register_in<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    field: usize,
    signed: bool,
    dst: VReg,
) -> VReg {
    match context.operand(masm, field, signed) {
        Operand::Register(r) if r == dst => dst,
        Operand::Register(r) => {
            onlineasm!(*masm.assembler();
                move {dst}, {r};
            );
            dst
        }
        Operand::Immediate(i) => {
            onlineasm!(*masm.assembler();
                move {dst}, {i};
            );
            dst
        }
    }
}

/// [`operand_register_in`] into the interpreter's operand scratch.
pub fn operand_register<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    field: usize,
    signed: bool,
) -> VReg {
    operand_register_in(masm, context, field, signed, tmp::T0)
}

/// Reload the code registers for the frame in `SFP` after a pop:
/// `fp[1]` -> closure -> code block -> `CONSTANT_POINTER` (entrypoint), `MD` (`[CONSTANT_POINTER - 8]`).
/// Shared by both contexts (identical emission). Clobbers `tmp::T0`.
pub(crate) fn emit_reload_code(masm: &mut Masm) {
    use crate::runtime::sabi::{self, regs};
    let (sfp, constant_pointer, md) = (regs::SFP, regs::CONSTANT_POINTER, regs::MD);
    let clo_at = masm.assembler().mem(sfp, 8);
    let block_at = masm
        .assembler()
        .mem(tmp::T0, sabi::CLOSURE_CODE_BLOCK as i32);
    let entry_at = masm
        .assembler()
        .mem(tmp::T0, sabi::CODE_BLOCK_ENTRYPOINT as i32);
    let md_at = masm.assembler().mem(constant_pointer, -8);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {clo_at};
        loadq {tmp::T0}, {block_at};
        loadp {constant_pointer}, {entry_at};
        loadp {md}, {md_at};
    );
}

/// A register-register conditional branch to `taken`, shared by both
/// contexts' `branch_if` (identical emission).
pub(crate) fn emit_branch_register_register(
    masm: &mut Masm,
    cond: Condition,
    a: VReg,
    b: VReg,
    taken: Label,
) {
    match cond {
        Condition::Eq => onlineasm!(*masm.assembler(); bqeq {a}, {b}, {taken};),
        Condition::Ne => onlineasm!(*masm.assembler(); bqneq {a}, {b}, {taken};),
        Condition::Lt => onlineasm!(*masm.assembler(); bqlt {a}, {b}, {taken};),
        Condition::Le => onlineasm!(*masm.assembler(); bqlteq {a}, {b}, {taken};),
        Condition::Gt => onlineasm!(*masm.assembler(); bqgt {a}, {b}, {taken};),
        Condition::Ge => onlineasm!(*masm.assembler(); bqgteq {a}, {b}, {taken};),
        Condition::Eq32 => onlineasm!(*masm.assembler(); bieq {a}, {b}, {taken};),
        Condition::Ne32 => onlineasm!(*masm.assembler(); bineq {a}, {b}, {taken};),
        Condition::Lt32 => onlineasm!(*masm.assembler(); bilt {a}, {b}, {taken};),
        Condition::Le32 => onlineasm!(*masm.assembler(); bilteq {a}, {b}, {taken};),
        Condition::Gt32 => onlineasm!(*masm.assembler(); bigt {a}, {b}, {taken};),
        Condition::Ge32 => onlineasm!(*masm.assembler(); bigteq {a}, {b}, {taken};),
    }
}

/// Mirror a comparison whose constant sits on the left, so every
/// `branch_if` compares register-first.
pub(crate) fn mirror(cond: Condition) -> Condition {
    match cond {
        Condition::Lt => Condition::Gt,
        Condition::Le => Condition::Ge,
        Condition::Gt => Condition::Lt,
        Condition::Ge => Condition::Le,
        Condition::Lt32 => Condition::Gt32,
        Condition::Le32 => Condition::Ge32,
        Condition::Gt32 => Condition::Lt32,
        Condition::Ge32 => Condition::Le32,
        other => other,
    }
}

/// Fold a comparison of two constants: whether it holds.
pub(crate) fn holds(cond: Condition, a: i64, b: i64) -> bool {
    let (a32, b32) = (a as i32, b as i32);
    match cond {
        Condition::Eq => a == b,
        Condition::Ne => a != b,
        Condition::Lt => a < b,
        Condition::Le => a <= b,
        Condition::Gt => a > b,
        Condition::Ge => a >= b,
        Condition::Eq32 => a32 == b32,
        Condition::Ne32 => a32 != b32,
        Condition::Lt32 => a32 < b32,
        Condition::Le32 => a32 <= b32,
        Condition::Gt32 => a32 > b32,
        Condition::Ge32 => a32 >= b32,
    }
}

/// A register-immediate conditional branch to `taken`, shared by both
/// contexts' `branch_if` (identical emission).
pub(crate) fn emit_branch_register_immediate(
    masm: &mut Masm,
    cond: Condition,
    a: VReg,
    b: i64,
    taken: Label,
) {
    match cond {
        Condition::Eq => onlineasm!(*masm.assembler(); bqeq {a}, {b}, {taken};),
        Condition::Ne => onlineasm!(*masm.assembler(); bqneq {a}, {b}, {taken};),
        Condition::Lt => onlineasm!(*masm.assembler(); bqlt {a}, {b}, {taken};),
        Condition::Le => onlineasm!(*masm.assembler(); bqlteq {a}, {b}, {taken};),
        Condition::Gt => onlineasm!(*masm.assembler(); bqgt {a}, {b}, {taken};),
        Condition::Ge => onlineasm!(*masm.assembler(); bqgteq {a}, {b}, {taken};),
        Condition::Eq32 => onlineasm!(*masm.assembler(); bieq {a}, {b}, {taken};),
        Condition::Ne32 => onlineasm!(*masm.assembler(); bineq {a}, {b}, {taken};),
        Condition::Lt32 => onlineasm!(*masm.assembler(); bilt {a}, {b}, {taken};),
        Condition::Le32 => onlineasm!(*masm.assembler(); bilteq {a}, {b}, {taken};),
        Condition::Gt32 => onlineasm!(*masm.assembler(); bigt {a}, {b}, {taken};),
        Condition::Ge32 => onlineasm!(*masm.assembler(); bigteq {a}, {b}, {taken};),
    }
}
